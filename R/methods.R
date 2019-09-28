
# methods

#' @describeIn write.design Using default settings for writing CSV files
#' @export
write.design.csv <- function(..., quote=F, row.names=F) write.design(..., file.extension = ".csv", output.handler=utils::write.csv, quote=quote, row.names=row.names)
#' @describeIn write.design Using default settings for writing JSON files
#' @export
write.design.json <- function(..., dataframe="columns") write.design(..., file.extension = ".json", output.handler=jsonlite::write_json, dataframe=dataframe)

#' Subset factor designs
#'
#' @param x The factor design to subset
#' @param subset Criteria along which to filter in planned observations / design matrix rows.
#' @param select Names of columns to keep in the design matrix
#' @param ... (ignored)
#'
#' @return A factor design with updated design matrix.
#'
#' @export
subset.factorDesign <- function(x, subset, select = names(x), ...) {
  if(as.character(sys.calls()[[1]][[1]]) != "subset.factorDesign") fun.call <- match.call(call=sys.call(-1L))
  else fun.call <- match.call()
  if(any(!select %in% names(x))) stop("Undefined factor names in `select`!")
  x[setdiff(names(x), select)] <- NULL
  keep.columns <- unique(unname(unlist(lapply(x, function(f) colnames(f@levels)))))
  x@design <- subset(x@design, eval(fun.call$subset, x@design), keep.columns)
  return(x)
}

#' Write Design Files
#'
#' This function writes a design into a set of files. For each random factor, a unit list is created that contains a list of all levels (instances) of the random factor and the factor levels to which that level is assigned. Moreover, code files are created that contain a complete set of experimental codes.
#'
#' @param design The `factorDesign` to be written into files.
#' @param group_by Experimental codes are to be grouped by these factors. If `NULL`, all codes are written into one file. Also see [output.design()] for grouping design output.
#' @param run.files The pattern to be used for the file names of the run files (i.e., files containing the experimental codes). By default, file names are "run_Group1_Othergroup4.ext" ect.
#' @param order_by The experimental codes are to be ordered by these columns.  Also see [output.design()] for ordering design output.
#' @param randomize After ordering, lines in the same order rank are to be shuffled randomly if set to `TRUE`.
#' @param code.files Code files (files containing conditions for levels of random factors) are named after this pattern.
#' @param output.dir All files are written into this directory.
#' @param output.handler This is the function that is called to write the data frames. If using `write.design.csv`, this is utils::write.csv and if using `write.design.json`, this is `jsonlite::write_json`.
#' @param file.extension This is the file extension to be added after each file name. Use '' if no file extension is to be added. If `NULL`, the file extension is guessed from the output handler used.
#' @param ... Other parameters to be passed on to `write.design` and the underlying output handler.
#' @seealso [output.design()] for use of `order_by` and `group_by`.
#' @export
write.design <- function(design, group_by = NULL, order_by = NULL, randomize = F, run.files = paste0("run",ifelse(length(group_by)>0L,paste0("_",group_by,"-%",seq_along(group_by),"$s",collapse=""),"")), code.files = "codes_%s", output.dir = getwd(), output.handler, file.extension = NULL, ...) {
  clean.file.path <- function(paths) gsub("[^a-zA-Z0-9_.,]", "-", paths)
  output <- output.design(design=design, group_by=group_by, order_by=order_by, randomize=randomize)
  file_names <- file.path(output.dir, clean.file.path(paste0(do.call(sprintf, c(list(run.files), unname(as.list(output$groups)))), file.extension)))
  if(!is.function(output.handler)) stop("`output.handler` must be a function that accepts a data frame and a file path as arguments.")
  if(is.null(file.extension)) {
    if(identical(output.handler, utils::write.csv) || identical(output.handler, utils::write.csv2)) file.extension = ".csv"
    else if(identical(output.handler, base::write.dcf)) file.extension = ".dcf"
    else if(isNamespaceLoaded("jsonlite") && identical(output.handler, jsonlite::write_json)) file.extension = ".json"
    else {
      warning("Could not guess file extension from output handler. No file extension is used. To suppress this warning, set `file.extension` to an empty string (file.extension=\"\") or provide a file extension suitable for your output format!")
      file.extension <- ""
    }
  }
  if(!is.null(output$groups)) {
    for(i in seq_len(nrow(output$groups))) {
      do.call(output.handler, list(output$codes[[i]], file_names[i], ...))
    }
  }else{
    do.call(output.handler, list(output$codes, file_names[1], ...))
  }
  for(n in names(output$units)) {
    do.call(output.handler, list(output$units[[n]], file.path(output.dir, clean.file.path(paste0(sprintf(code.files, n), file.extension))), ...))
  }
}

#' Preparing Designs for Model Analysis
#'
#' Based on a given design, this function creates a model formula that can be used to analyze data with functions such as lm or lmer.
#'
#' @param design The `factorDesign` to be used.
#' @param contrasts The contrasts to override (NULL if none to override)
#' @param expand.contrasts If TRUE, factors with more than one contrast are replaced by so many contrasts, i.e. the result contains the names of the individual contrasts, not of the factors.
#' @param response The left-hand side of the equation. Typically, this is just the response/dependent variable.
#' @param intercepts Should an intercept be included?
#' @param env The environment in which to embed the formula
#' @param interactions Should fixed effects be additive or interactive?
#' @return A named list of `formula` objects, each name corresponding to the type of model it is suited for.
#' @export
design.formula <- function(design, contrasts = NULL, expand.contrasts = !is.null(contrasts), interactions=T, intercepts=T, response = "dv", env = parent.frame()) {
  add_them <- function(l, op) {
    if(length(l) < 1) return(NULL)
    ret <- l[[1L]]
    for(el in l[-1L]) {
      if(is.null(el)) next
      ret <- call(op, ret, el)
    }
    return(ret)
  }
  if(expand.contrasts) {
    fixed <- contrast.names(design, ranfac = NULL, contrasts = contrasts, interactions = interactions, intercept = F, as.symbols = T)
    random <- lapply(random.factors(design, include.interactions = F), function(fac) {
      c(list(add_them(lapply(fac@name, as.symbol), ':')), contrast.names(design, ranfac = fac@name, contrasts = contrasts, interactions = interactions, intercept = F, as.symbols = T))
    })
  }
  else {
    fixed <- lapply(names(fixed.factors(design)), as.symbol)
    random <- lapply(random.factors(design, include.interactions = F), function(fac) {
      c(list(add_them(lapply(fac@name, as.symbol), ':')), lapply(setdiff(fixed, colnames(fac@levels)), as.symbol))
    })
  }
  random_lmer <- lapply(random, function(el) call('(', call('|', add_them(c(if(intercepts) list(1) else list(), list(add_them(el[-1L], if(interactions&&!expand.contrasts) '*' else '+'))), '+'), el[[1L]])))
  random_aov <- lapply(random, function(el) call('Error', if(length(el)>1L) call('/', el[[1L]], add_them(el[-1],'*')) else el[[1L]]))
  list(
    lm = call('~',as.symbol(response),add_them(c(if(intercepts) list(1) else list(), list(add_them(fixed, if(interactions&&!expand.contrasts) '*' else '+'))), '+')),
    lmer = call('~',as.symbol(response),add_them(c(if(intercepts) list(1) else list(), list(add_them(fixed, if(interactions&&!expand.contrasts) '*' else '+')), random_lmer), '+')),
    aov = call('~',as.symbol(response),add_them(c(if(intercepts) list(1) else list(), list(add_them(fixed, if(interactions&&!expand.contrasts) '*' else '+')), random_aov), '+'))
  )
}
setMethod("formula", signature = "factorDesign", function(x, ...) design.formula(design=x, ...)$lmer )

#' Summary of Factor Designs
#'
#' This function creates a useful summary of a factor design, including the design matrix itself as well as other parameters and a list of random factors as experimental units.
#'
#' @param design The `factorDesign` object to summarize.
#' @param group_by If not `NULL`, the design matrix is grouped by these factors. Factors must be valid columns of the design matrix. If used, `$codes` will be a list matched to the entries in `$groups`.
#' @param order_by If not `NULL`, output within each output group is ordered by these columns.
#' @param randomize After ordering, remaining rows in the same order rank are randomly shuffled.
#' @return A list containing the output summary, including the following named entities:
#'
#'    *$table*: Either a tibble with all experimental codes or a list of tibbles of experimental codes. The list entries are matched to the rows of `$groups`.
#'
#'    *$groups*: If grouped, contains a tibble in which each row represents an output group, matched to the entries in $codes. If not grouped, this is `NULL`.
#'
#'    *$ordered*: If ordered, contains a vector of order criteria. If not ordered, this is `NULL`.
#'
#'    *$randomized*: Value of `randomized`.
#'
#'    *$units*: A list of random factors and their levels for this design as tibbles. Empty list if no random factors in the design.
#'
#'    *$formulas*: Example model formulae for use with functions such as lm() and lmer().
#'
#' @seealso [design.formula()] for more options generating model formulae other than the suggested default ones in the summary.
#'
#' @export

output.design <- function(design, group_by = NULL, order_by = NULL, randomize = F) {
  if(is.null(order_by)) order_by <- character(0)
  if(is.null(group_by)) group_by <- character(0)
  if(!is.factorDesign(design)) stop("`design` must be a factor design!")
  if(any(!group_by %in% colnames(design@design))) stop("Not all of the grouping variables are part of the design!")
  if(any(!order_by %in% colnames(design@design))) stop("Not all of the ordering variables are part of the design!")
  if(!is.logical(randomize)) stop("`randomize` must be logical (TRUE or FALSE)!")
  file_groups <- unique(design@design[,group_by,drop=F])
  if(randomize) data <- design@design[sample(nrow(design@design)),,drop=F]
  else data <- design@design
  if(length(order_by)>0L) data <- data[do.call(order, unname(as.list(data[, order_by, drop=F]))), , drop=F]
  rownames(data) <- NULL
  list(
    codes = if(length(group_by)>0L) lapply(seq_len(nrow(file_groups)), function(i) {
      df <- join(file_groups[i, , drop=F], data)
      rownames(df) <- NULL
      return(tibble::as.tibble(df))
    }) else tibble::as.tibble(data),
    groups = if(length(group_by)>0L) file_groups else NULL,
    ordered = if(length(order_by)>0L) order_by else NULL,
    randomized = randomize,
    units = sapply(random.factors(design), function(f) {
      df <- unique(data[,colnames(f@levels),drop=F])
      df <- df[do.call(order, as.list(df)), , drop=F]
      rownames(df) <- NULL
      return(df)
    }, simplify = F, USE.NAMES = T),
    formulas = design.formula(design)
  )
}

#' Output a design factor summary
#' 
#' @param object the factor container to display
#'
#' @export
#'
show.factorContainer <- function(object) {
  if(is(object, "factorDesign")) {
    cat(sprintf("Factor design with %d factor(s):\n", length(object)))
    print.listof(object)
    cat(sprintf("\nDesign matrix:\n"))
    show(object@design)
  } else if(is(object, "randomFactor")) {
    cat(sprintf("Random factor `%s` with %d group(s) and %d instance(s) (%d level(s) in total)", paste(object@name, collapse=":"), nrow(object@levels), object@replications, nrow(object@levels)*object@replications))
    if(length(object@groups)>0L) {
      cat(", grouped by ")
      cat(paste(object@groups, collapse=":"))
    }
    cat("\n")
  } else if(is(object, "fixedFactor")) {
    cat(sprintf("Fixed factor `%s` with %d level(s) (%s) and %d replication(s)", paste(object@name, collapse=":"), nrow(object@levels), paste(object@levels[,object@name], collapse=", "), object@replications))
    if(length(object@groups)>0L) {
      cat(", grouped by ")
      cat(paste(object@groups, collapse=":"))
    }
    cat("\n")
  } else {
    stop("Not a design factor or factor list!")
  }
}

#' Concatenate design factors and designs
#'
#' By adding factors and designs by "+", a new design is created that contains all of the components.
#'
#' @export
#'
`+.factorContainer` <- function(e1, e2) {
  if(is(e1, "factorDesign")) {
    if(is(e2, "factorDesign")) {
      if(length(e1) == 0) {
        return(e2)
      } else if(length(e2) == 0) {
        return(e1)
      }
      for(el in e2) {
        e1 <- sys.function()(e1, el)
      }
      return(e1)
    }else if(is(e2, "designFactor")) {
      new.id <- paste(e2@name, collapse = ":")
      old.ids <- vapply(e1, function(f) paste(f@name, collapse=":"), character(1))
      if(new.id %in% old.ids) stop(sprintf("Cannot add `%s` to factor list because it already exists.", new.id))
      if(any(!e2@groups %in% old.ids)) stop(sprintf("Cannot add `%s` to factor list because not all grouping factors exist yet.", new.id))
      if(length(e2@name)>1L && !all(e2@name %in% names(e1))) stop(sprintf("Cannot add `%s` to factor list because all main factors must be added first!", new.id))
      if(is.randomFactor(e2) && length(e2@groups) > 0L) {
        e2@levels <- join(e2@levels, do.call(join, lapply(e1[match(e2@groups, old.ids)], function(f) f@levels)))
      } else if(is.fixedFactor(e2) && length(e2@groups) > 0L) {
        if(!all(vapply(e1[e2@groups], is.fixedFactor, logical(1)))) stop(sprintf("Cannot add `%s` to factor list because all its grouping factors must be fixed.", new.id))
      }
      e1[[new.id]] <- e2
      add.to.design <- replicate.factor(e2, e1)
      if(nrow(e1@design) == 0L) e1@design <- add.to.design
      else if(nrow(add.to.design) > 0L) e1@design <- merge(e1@design, add.to.design)
      # some factors produce *fac columns. Those are to replace other columns (*fac replaces fac)
      replacement.columns <- which(substr(colnames(e1@design), 1, 1) == '*')
      e1@design[,substr(colnames(e1@design)[replacement.columns], 2, nchar(colnames(e1@design)[replacement.columns]))] <- e1@design[, replacement.columns, drop=F]
      e1@design[,replacement.columns] <- NULL
      return(e1)
    }else{
      stop("Second element must be a design factor or factor list!")
    }
  }else if(is(e1, "designFactor")){
    return(sys.function()(sys.function()(new("factorDesign"), e1), e2))
  }else{
    stop("First element must be a designFactor (fixed.factor or random.factor) or a factorDesign!")
  }
}

#' @describeIn is.designFactor Check if argument is a random factor.
#' @export
is.randomFactor <- function(fac) is(fac, "randomFactor")
#' @describeIn is.designFactor Check if argument is a fixed factor.
#' @export
is.fixedFactor <- function(fac) is(fac, "fixedFactor")
#' @describeIn is.designFactor Check if argument is a factor design.
#' @export
is.factorDesign <- function(fac) is(fac, "factorDesign")
#' Checking factor design data types
#'
#' Check if argument is a design factor (either random or fixed factor), specifically a random factor, a fixed factor or a factor design.
#'
#' @param fac Object to check.
#'
#' @return `TRUE` or `FALSE`
#' @export
is.designFactor <- function(fac) is(fac, "designFactor")

#' @describeIn fixed.factors Return fixed factors
#' @export
random.factors <- function(design, include.interactions = T) {
  if(!is.factorDesign(design)&&!is.list(design)) stop("`design` must be a design factor or list!")
  return(design[vapply(design, if(include.interactions) is.randomFactor else function(f) is.randomFactor(f) && length(f@name) == 1L, logical(1))])
}

#' Extract factors by type
#'
#' From a given design, extract contained random or fixed factors as a list.
#'
#' @param design The factor design to check.
#'
#' @return A list of factors that are either fixed or random.
#'
#' @export
fixed.factors <- function(design) {
  if(!is.factorDesign(design)&&!is.list(design)) stop("`design` must be a design factor or list!")
  return(design[vapply(design, is.fixedFactor, logical(1))])
}

#' Retrieve factors or design matrix from an factor design object
#' @usage
#' design[i, col]
#' design[i, NULL]
#' design[, col]
#' design[, NULL]
#' design[[fac]]
#' design[facs]
#' 
#' @param i The row number (number of the planned observation) to retrieve from the design matrix. Leave empty (but don't leave the comma out!) to select all rows.
#' @param col The column[s] (factor name[s]) to select from the matrix. Leave empty (but don't leave the comma out!) to select all columns.
#' @param fac,facs The factor name(s) to select from the factor list.
#' 
#' The design[i, col] syntax can be used to retrieve the design matrix (instead of the design object itself). You may leave names/indices for rows out in order to select all rows. You ALWAYS have to specify columns by names, indices or select all by using NULL or a single TRUE value.
#' 
#' If you use design[facs], you are selecting factor objects as a list from the design object, specified by names or indices in facs.
#' 
#' If you use design[[fac]], you are selecting a single factor object, specified by a name or index.
#' 
#' @export
`[.factorDesign` <- function(x, i, j) {
  if(missing(j)) {
    y <- as(x, "list")
    names(y) <- names(x)
    if(is.null(i)) {
      i <- T
    }
    return(y[i])
  }
  else return(x@design[i, j, drop=F])
}

#' Design matrix dimensions
#' @usage
#' nrow(design)
#' ncol(design)
#' dim(design)
#' nobs(design)
#' 
#' @param design A factor design object.
#' 
#' You can use nrow(design) or nobs(design) to retrieve the number of rows (planned observations) and ncol(design) to retrieve the number of columns of the design. dim(design) retrieves both as a 2-length integer vector.
#' 
#' @export
dim.factorDesign <- function(x) dim(x@design)

setMethod("[", c(x="factorDesign"), `[.factorDesign`)
setMethod("dim", "factorDesign", dim.factorDesign)
setMethod("nobs", "factorDesign", function(object) nrow(object))

setMethod("+", signature(e1="factorContainer", e2="factorContainer"), `+.factorContainer`)

setMethod("show", "factorContainer", `show.factorContainer`)

setMethod("subset", signature(x="factorDesign"), `subset.factorDesign`)

