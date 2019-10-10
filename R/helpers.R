

# helper functions

find.rot.fun.in.extra <- function(facExtra, default) {
  rotation.function <- latin.square # default function
  if("assign" %in% names(facExtra)) { # check if there is a rotation function given for this factor
    if(is.function(facExtra$assign)) rotation.function <- facExtra$assign
    else if(!is.character(facExtra$assign)) stop("`assign` must be a function name or any of 'latin.square', 'permutations', or 'random'")
    else if(facExtra$assign %in% c("latin.square","lsq")) rotation.function <- latin.square
    else if(facExtra$assign %in% c("random","random.order")) rotation.function <- random.order
    else if(facExtra$assign %in% c("permute","permutations")) rotation.function <- permutations
    else if(facExtra$assign %in% c("williams")) rotation.function <- crossdes::williams
    else {
      rotation.function <- eval(as.symbol(facExtra$assign))
      if(!is.function(rotation.function)) stop(sprintf("`%s` is not a function!", facExtra$assign))
    }
  }
  return(rotation.function)
}

replicate.factor <- function(fac, context=factor.design()) {
  df <- fac@levels
  if(is.randomFactor(fac) && length(fac@name) == 1L) {
    df <- df[rep(seq_len(nrow(df)), each=fac@replications), , drop=FALSE]

    for(fixed.nest in lapply(context[vapply(names(context), function(f) f %in% fac@groups && is.fixedFactor(context[[f]]), logical(1))], replicate.factor)) {
      df <- merge(df, fixed.nest)
    }

    for(random.fac in context[vapply(names(context), function(f) f %in% fac@groups && is.randomFactor(context[[f]]), logical(1))]) {
      random.nest <- unique(context@design[,colnames(random.fac@levels),drop=FALSE])
      df <- merge(df[,!colnames(df)%in%random.fac@name,drop=FALSE], random.nest)
    }

    df[, fac@name] <- seq_len(nrow(df))
    rownames(df) <- NULL
    return(tibble::as.tibble(df))
  }else if(is.randomFactor(fac) && length(fac@name) > 1L){
    id.factors <- unique(unlist(lapply(context[fac@name], function(f) colnames(f@levels)), use.names = FALSE)) # these are the factors that define one level of this interaction
    ids <- unique(context@design[, id.factors]) # these are all the realizations of this interaction
    ids <- ids[rep(seq_len(nrow(ids)), fac@replications),,drop=FALSE]
    
    between.conditions <- setdiff(intersect(names(context), colnames(ids)), fac@name) # group factor levels by main factor groups
    if(length(between.conditions) > 0L) {
      id.groups <- split(ids[, fac@name, drop=FALSE], lapply(between.conditions, function(cond) ids[, cond]))
    }else{
      # if there are no groups, put all levels into the same bin
      id.groups <- list(ids[, fac@name, drop=FALSE])
    }
    conditions <- fac@levels[, setdiff(colnames(fac@levels), colnames(ids)), drop=FALSE] # conditions to which ids have to be assigned
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

    map.multiplications.to.conditions <- vapply(seq_len(nrow(multiplications)), function(i) rotations[((i-1)%/%ncol(rotations)%%nrow(rotations))+1, (i-1)%%ncol(rotations)+1, drop=TRUE], numeric(1))
    multiplications <- cbind(multiplications, conditions[map.multiplications.to.conditions, , drop=FALSE])

    assign.to.ids <- do.call(rbind, lapply(id.groups, join, multiplications)) # the *facname columns will help create new random factor levels
    assign.to.ids[,paste0('*', fac@name)] <- (assign.to.ids[, fac@name, drop=FALSE] - 1L) * nrow(rotations) + assign.to.ids[, paste0('*', fac@name), drop=FALSE]

    return(tibble::as.tibble(assign.to.ids))
  }else if(is.fixedFactor(fac) && length(fac@groups) == 0L){
    return(tibble::as.tibble(df[rep(seq_len(nrow(df)), each=fac@replications), , drop=FALSE]))
  }else if(is.fixedFactor(fac) && length(fac@groups) >= 1L){
    if(!all(vapply(context[fac@groups], is.fixedFactor, logical(1)))) stop("Fixed factor may only be nested within other fixed factors or fixed factor interactions but not within levels of random factors!")
    
    conditions <- do.call(join, lapply(context[fac@groups], function(f) f@levels))
    conditions[,'*'] <- apply(conditions, 1L, paste, collapse=":")
    
    excluded.conditions <- setdiff(conditions[,'*',drop=TRUE], df[,'*',drop=TRUE])
    
    if(length(excluded.conditions)>0L) {
      warning(sprintf("`%1$s` does not specify levels for groups: %2$s. Note that the `%1$s` column will therefore be NA and could potentially lead to problems further on in the design! If this is intended, to suppress this warning, please specify %3$s in the fixed.factor level list.", fac@name, paste("'",excluded.conditions,"'",sep="",collapse = ", "), paste("`",excluded.conditions,"`=NA",sep="",collapse = ", ")))
      dummy.df <- do.call(data.frame, lapply(seq_len(ncol(df)), function(i) rep(NA, length(excluded.conditions))))
      colnames(dummy.df) <- colnames(df)
      dummy.df[, '*'] <- excluded.conditions
      df <- rbind(df, dummy.df)
    }
    
    ret <- join(as.data.frame(conditions), as.data.frame(df[rep(seq_len(nrow(df)), each=fac@replications), , drop=FALSE]))
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
  if(!all(vapply(elements, is.randomFactor, logical(1)))) stop("Arguments to join must be random factors!")
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
  extra <- list(...)
  if(is.language(lang)) {
    if(is.name(lang)) {
      return(random.factor(as.character(lang)))
    } else if(is.expression(lang)) {
      return(factor.design(parse.factor.language(lang[[1]], ...)))
    } else if(is.call(lang)) {
      if(lang[[1]] == "~") {
        if(length(lang) > 2) {
          stop("If using a formula operator (~), the left-hand side must be empty!")
        } else {
          return(factor.design(parse.factor.language(lang[[2]], ...)))
        }
      } else if(lang[[1]] == "[") {
        fac <- parse.factor.language(lang[[2]])
        if(!is.designFactor(fac)) {
          stop(sprintf("Object of [] operation must be a designFactor (%s given)!", class(fac)))
        }
        fac@groups <- vapply(lang[c(-1,-2)], as.character, character(1))
        return(fac)
      } else if(lang[[1]] == "+") {
        fac1 <- parse.factor.language(lang[[2]])
        fac2 <- parse.factor.language(lang[[3]])
        if(!is(fac1, "factorContainer") || !is(fac2, "factorContainer")) {
          stop(sprintf("Objects combined with a + operator must evaluate to designFactors (%s and %s given)!", class(fac1), class(fac2)))
        }
        return(factor.design(fac1, fac2))
      } else if(lang[[1]] == "*") {
        if(!is.numeric(lang[[2]]) || lang[[2]] < 1) {
          stop("When using the multiplication sign (*), the left-hand side must be a number and the right-hand side must be factors!")
        }
        fac1 <- as.integer(lang[[2]])
        fac2 <- parse.factor.language(lang[[3]])
        fac2@replications <- fac2@replications * fac1
        return(fac2)
      } else if(lang[[1]] == ":") {
        fac1 <- lang[[2]]
        fac2 <- parse.factor.language(lang[[3]])
        if(!is.name(fac1) || !is(fac2, "designFactor")) {
          stop("When using the interaction operator (:), it must combine valid variable names!")
        }
        fac2@name <- c(as.character(fac1), fac2@name)
        return(fac2)
      } else if(is.name(lang[[1]])) {
        return(fixed.factor(as.character(lang[[1]]), levels = vapply(lang[-1], as.character, character(1))))
      } else {
        stop(sprintf("Unknown operator %s!", as.character(lang[[1]])))
      }
    }
  } else {
    stop(sprintf("`lang` must be a language object but is of class %s!", class(lang)))
  }
}
