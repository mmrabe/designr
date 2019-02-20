

# helper functions

find.rot.fun.in.extra <- function(facExtra, default) {
  rotation.function <- latin.square # default function
  if("assign" %in% names(facExtra)) { # check if there is a rotation function given for this factor
    if(is.function(facExtra$assign)) rotation.function <- facExtra$assign
    else if(!is.character(facExtra$assign)) stop("`assign` must be a function name or any of 'latin.square', 'permutations', or 'random'")
    else if(facExtra$assign %in% c("latin.square","lsq")) rotation.function <- latin.square
    else if(facExtra$assign %in% c("random","random.order")) rotation.function <- random.order
    else if(facExtra$assign %in% c("permute","permutations")) rotation.function <- permutations
    else {
      rotation.function <- eval(as.symbol(facExtra$assign))
      if(!is.function(rotation.function)) stop(sprintf("`%s` is not a function!", facExtra$assign))
    }
  }
  return(rotation.function)
}

replicate.factor <- function(fac, context=factor.design()) {
  df <- fac@levels
  if(is.random.factor(fac) && length(fac@name) == 1L) {
    df <- df[rep(seq_len(nrow(df)), each=fac@replications), , drop=F]

    for(fixed.nest in lapply(context[vapply(names(context), function(f) f %in% fac@groups && is.fixed.factor(context[[f]]), logical(1))], replicate.factor)) {
      df <- merge(df, fixed.nest)
    }

    for(random.fac in context[vapply(names(context), function(f) f %in% fac@groups && is.random.factor(context[[f]]), logical(1))]) {
      random.nest <- unique(context@design[,colnames(random.fac@levels),drop=F])
      df <- merge(df[,!colnames(df)%in%random.fac@name,drop=F], random.nest)
    }

    df[, fac@name] <- seq_len(nrow(df))
    rownames(df) <- NULL
    return(tibble::as.tibble(df))
  }else if(is.random.factor(fac) && length(fac@name) > 1L){
    id.factors <- unique(unlist(lapply(context[fac@name], function(f) colnames(f@levels)), use.names = F)) # these are the factors that define one level of this interaction
    ids <- unique(context@design[, id.factors]) # these are all the realizations of this interaction

    between.conditions <- setdiff(colnames(ids), fac@name) # group factor levels by main factor groups
    if(length(between.conditions) > 0L) {
      id.groups <- split(ids[, fac@name, drop=F], lapply(between.conditions, function(cond) ids[, cond]))
    }else{
      # if there are no groups, put all levels into the same bin
      id.groups <- list(ids[, fac@name, drop=F])
    }
    conditions <- fac@levels[, setdiff(colnames(fac@levels), colnames(ids)), drop=F] # conditions to which ids have to be assigned
    # within each id group, item assignment will be rotated according to chosen rotation method.
    # to ensure that there are enough realizations in each group, every constituting random factor must be replicated as many times as the realizations frame is long (rows)
    # i.e., n-times for latin.square and n!-times for permutations, where n is number of conditions

    rotation.function <- find.rot.fun.in.extra(fac@extra, latin.square)

    rotations <- rotation.function(nrow(conditions))


    multiplications <- do.call(join, lapply(fac@name, function(f) {
      x <- tibble::tibble(seq_len(nrow(rotations)))
      colnames(x) <- paste0('*', f)
      x
    }))

    map.multiplications.to.conditions <- vapply(seq_len(nrow(multiplications)), function(i) rotations[((i-1)%/%ncol(rotations)%%nrow(rotations))+1, (i-1)%%ncol(rotations)+1, drop=T], integer(1))
    multiplications <- cbind(multiplications, conditions[map.multiplications.to.conditions, , drop=F])

    assign.to.ids <- do.call(rbind, lapply(id.groups, join, multiplications)) # the *facname columns will help create new random factor levels
    assign.to.ids[,paste0('*', fac@name)] <- (assign.to.ids[, fac@name, drop=F] - 1L) * nrow(rotations) + assign.to.ids[, paste0('*', fac@name), drop=F]

    return(tibble::as.tibble(assign.to.ids))
  }else if(is.fixed.factor(fac) && length(fac@groups) == 0L){
    return(tibble::as.tibble(df[rep(seq_len(nrow(df)), each=fac@replications), , drop=F]))
  }else if(is.fixed.factor(fac) && length(fac@groups) >= 1L){
    if(!all(vapply(context[fac@groups], is.fixed.factor, logical(1)))) stop("Fixed factor may only be nested within other fixed factors or fixed factor interactions but not within levels of random factors!")
    
    conditions <- do.call(join, lapply(context[fac@groups], function(f) f@levels))
    conditions[,'*'] <- apply(conditions, 1L, paste, collapse=":")
    
    excluded.conditions <- setdiff(conditions[,'*',drop=T], df[,'*',drop=T])
    
    if(length(excluded.conditions)>0L) {
      warning(sprintf("`%1$s` does not specify levels for groups: %2$s. Note that the `%1$s` column will therefore be NA and could potentially lead to problems further on in the design! If this is intended, to suppress this warning, please specify %3$s in the fixed.factor level list.", fac@name, paste("'",excluded.conditions,"'",sep="",collapse = ", "), paste("`",excluded.conditions,"`=NA",sep="",collapse = ", ")))
      dummy.df <- do.call(data.frame, lapply(seq_len(ncol(df)), function(i) rep(NA, length(excluded.conditions))))
      colnames(dummy.df) <- colnames(df)
      dummy.df[, '*'] <- excluded.conditions
      df <- rbind(df, dummy.df)
    }
    
    ret <- join(as.data.frame(conditions), as.data.frame(df[rep(seq_len(nrow(df)), each=fac@replications), , drop=F]))
    ret[,'*'] <- NULL
    rownames(ret) <- NULL
    
    return(tibble::as.tibble(ret))
  }else{
    stop("Don't know how to expand this factor!")
  }
}

join.random.factors <- function(...) {
  elements <- list(...)
  if(length(elements) == 0L) stop("Argument list cannot be empty!")
  if(!all(vapply(elements, is.random.factor, logical(1)))) stop("Arguments to join must be random factors!")
  ret <- elements[[1]]
  for(el in elements[-1]) {
    ret@name <- c(ret@name, setdiff(el@name, ret@name))
    ret@levels <- join(ret@levels, el@levels)
    ret@extra <- c(ret@extra, el@extra)
    ret@replications <- ret@replications * el@replications
    ret@groups <- c(ret@groups, setdiff(el@groups, ret@groups))
  }
  return(ret)
}

parse.factor.language <- function(lang, ...) {
  extra = list(...)
  if(is.symbol(lang)) {
    return(random.factor(name=as.character(lang)))
  }
  if(!is.language(lang)) stop(sprintf("`lang` must be a language object! Unknown token: %s", as.character(lang)))
  tilde.pos <- find.in.list('~', lang)
  if(length(tilde.pos) > 0L) {
    if(length(lang) <= tilde.pos) {
      stop("If including the tilde operator (~), the factor formula is expected after it.")
    }
    return(parse.factor.language(lang[[tilde.pos+1]]))
  }
  if(is.symbol(lang[[1]])) {
    if(lang[[1]]=='+') {
      return(c(parse.factor.language(lang[[2]]), parse.factor.language(lang[[3]])))
    } else if(lang[[1]]=='[') {
      #this is a fixed factor definition
      args <- list()
      if(is.null(names(lang))) {
        levels <- lang[-c(1L,2L)]
      }else{
        levels <- lang[which(nchar(names(lang))==0L)[-c(1L,2L)]]
        for(i in which(nchar(names(lang))>0L)) {
          args[[names(lang)[i]]] <- eval(lang[[i]])
        }
      }
      args$levels <- unname(unlist(lapply(levels, function(level) {
        reps <- 1L
        if(is.language(level)&&!is.symbol(level)&&level[[1]]==':') {
          if(!is.numeric(level[[2]])||!is.numeric(level[[3]])) stop(": only allowed for numbers, such as 2:5")
          return(seq(level[[2]], level[[3]]))
        }else if(is.language(level)&&!is.symbol(level)&&level[[1]]=='*') {
          if(!is.numeric(level[[2]])) stop("* only allowed for level multiplication, such as 2*level")
          reps <- as.integer(level[[2]])
          level <- level[[3]]
        }else if(length(level)==1){
          level <- level
        }
        if(is.symbol(level)) return(rep(as.character(level), reps))
        else if(inherits(level, c("character","logical","integer","numeric"))) return(rep(level, reps))
        else stop("Unnamed arguments in level brackets must be levels or multiplied levels (2*level)!")
      })))
      args$name <- as.character(lang[[2]])

      return(do.call(fixed.factor, args))
    } else if(lang[[1]]=='(') {
      if(length(lang)==2) return(parse.factor.language(lang[[2]]))
      else return(lapply(lang[-1], parse.factor.language))
    } else if(lang[[1]]==':') {
      return(do.call(join.random.factors, lapply(lang[-1], parse.factor.language)))
    } else if(lang[[1]]=='*') {
      if(is.numeric(lang[[2]])) {
        repl <- as.integer(lang[[2]])
        fact <- parse.factor.language(lang[[3]])
      }else if(is.numeric(lang[[3]])){
        repl <- as.integer(lang[[3]])
        fact <- parse.factor.language(lang[[2]])
      }else{
        stop("Replication operator (*) must occur with a number and a design factor (or list of factors)!")
      }
      if(repl < 1L) stop("Replication multiplier must be an integer greater than or equal to 1!")
      if(is.design.factor(fact)) {
        fact@replications <- fact@replications * repl
        return(fact)
      }else if(is.list(fact)||is("design.factor", fact)) {
        return(lapply(fact, function(f) {
          f@replications <- f@replications*repl
          return(f)
        }))
      }
    }
  }
  # supplying additional arguments to factor
  fac <- parse.factor.language(lang[[1]])
  if(!is.design.factor(fac)) stop("Syntax error! Expected factor definition before parantheses.")
  if(is.null(names(lang))) {
    # argument list is completely unnamed -> all arguments are grouping factors
    group.args <- lang[-1L]
  }else{
    group.args <- lang[which(nchar(names(lang))==0L)[-1L]]
    for(i in which(nchar(names(lang))>0L)) {
      argname <- names(lang)[i]
      argval <- if(is.symbol(lang[[i]])) as.character(lang[[i]]) else eval(lang[[i]])
      fac@extra[[argname]] <- argval
    }
  }
  fac@groups <- vapply(group.args, as.character, character(1))
  return(fac)
}
