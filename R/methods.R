
# methods


#' Write Design Files
#'
#' This function writes a design into a set of files. For each random factor, a unit list is created that contains a list of all levels (instances) of the random factor and the factor levels to which that level is assigned. Moreover, code_files are created that contain a complete set of experimental codes.
#'
#' @param design The \code{factorDesign} to be written into files.
#' @param group_by Experimental codes are to be grouped by these factors. If \code{NULL}, all codes are written into one file. Also see \code{\link[designr]{output.design}} for grouping design output.
#' @param run_files The pattern to be used for the file names of the run_files (i.e., files containing the experimental codes). By default, file names are \code{"run_Group1_Othergroup4.ext"} ect.
#' @param order_by The experimental codes are to be ordered by these columns. Also see \code{\link[designr]{output.design}} for ordering design output.
#' @param randomize After ordering, lines in the same order rank are to be shuffled randomly if set to \code{TRUE}.
#' @param code_files Code files (files containing conditions for levels of random factors) are named after this pattern.
#' @param output_dir All files are written into this directory.
#' @param output_handler This is the function that is called to write the data frames. If using \code{write.design.csv}, this is \code{utils::write.csv} and if using \code{write.design.json}, this is \code{jsonlite::write_json}.
#' @param file_extension This is the file_extension to be added after each file name. Use '' if no file_extension is to be added. If `NULL`, the file_extension is guessed from the output_handler used.
#' @param ... Other parameters to be passed on to \code{write.design} and the underlying output_handler.
#' @seealso \code{\link[designr]{output.design}} for use of \code{order_by} and \code{group_by}.
#' 
#' @examples
#' 
#' des <- fixed.factor("Factor1", c("1A","1B")) +
#'        fixed.factor("Factor2", c("2A","2B")) +
#'        random.factor("Subject", c("Factor1"))
#'     
#' # This writes a CSV file for each subject and a CSV list of subjects
#' write.design(des, group_by = "Subject", output_handler = write.csv, output_dir = tempdir())
#' 
#' # This writes a single CSV file for all subjects and a CSV list of subjects
#' write.design(des, output_handler = write.csv, output_dir = tempdir())
#' 
#' @export
write.design <- function(design, group_by = NULL, order_by = NULL, randomize = FALSE, run_files = paste0("run",ifelse(length(group_by)>0L,paste0("_",group_by,"-%",seq_along(group_by),"$s",collapse=""),"")), code_files = "codes_%s", output_dir, output_handler, file_extension = NULL, ...) {
  check_argument(design, "factorDesign")
  check_argument(run_files, "character", 1)
  check_argument(code_files, "character", 1)
  check_argument(output_dir, "character", 1)
  check_argument(output_handler, c("function", "character"), 1)
  check_argument(file_extension, c("character","NULL"), 1)
  clean.file.path <- function(paths) gsub("[^a-zA-Z0-9_.,]", "-", paths)
  output <- output.design(design=design, group_by=group_by, order_by=order_by, randomize=randomize)
  file_names <- file.path(output_dir, clean.file.path(paste0(do.call(sprintf, c(list(run_files), unname(as.list(output$groups)))), file_extension)))
  if(!is.function(output_handler)) stop("`output_handler` must be a function that accepts a data frame and a file path as arguments.")
  if(is.null(file_extension)) {
    if(identical(output_handler, utils::write.csv) || identical(output_handler, utils::write.csv2)) file_extension = ".csv"
    else if(identical(output_handler, base::write.dcf)) file_extension = ".dcf"
    else if(isNamespaceLoaded("jsonlite") && identical(output_handler, jsonlite::write_json)) file_extension = ".json"
    else {
      warning("Could not guess file_extension from output_handler. No file_extension is used. To suppress this warning, set `file_extension` to an empty string (file_extension=\"\") or provide a file_extension suitable for your output format!")
      file_extension <- ""
    }
  }
  if(!is.null(output$groups)) {
    for(i in seq_len(nrow(output$groups))) {
      do.call(output_handler, list(output$codes[[i]], file_names[i], ...))
    }
  }else{
    do.call(output_handler, list(output$codes, file_names[1], ...))
  }
  for(n in names(output$units)) {
    do.call(output_handler, list(output$units[[n]], file.path(output_dir, clean.file.path(paste0(sprintf(code_files, n), file_extension))), ...))
  }
}


#' @describeIn write.design Using default settings for writing CSV files
#' @param quote,row.names see \code{\link[utils:write.csv]{utils::write.csv()}}
#' @export
write.design.csv <- function(..., quote=FALSE, row.names=FALSE) write.design(..., file_extension = ".csv", output_handler=utils::write.csv, quote=quote, row.names=row.names)

#' @describeIn write.design Using default settings for writing XLSX files (using the \code{writexl} package)
#' @param format_headers see \code{\link[writexl:write_xlsx]{writexl::write_xlsx()}}, default is \code{FALSE}
#' @export
write.design.xlsx <- function(..., format_headers = FALSE) {
  if(!requireNamespace("writexl")) {
    stop("write.design.xlsx() requires the writexl package but it could not be loaded.")
  }
  write.design(..., file_extension = ".xlsx", output_handler=writexl::write_xlsx, format_headers = format_headers)
}


#' @describeIn write.design Using default settings for writing JSON files (using the \code{jsonlite} package)
#' @param dataframe see \code{\link[jsonlite:write_json]{jsonlite::write_json()}}
#' @export
write.design.json <- function(..., dataframe="columns") {
  if(!requireNamespace("jsonlite")) {
    stop("write.design.json() requires the jsonlite package but it could not be loaded.")
  }
  write.design(..., file_extension = ".json", output_handler=jsonlite::write_json, dataframe=dataframe)
}

#' Summary of Factor Designs
#'
#' These functions return useful summaries of a factor design, including the design matrix itself as well as other parameters and a list of random factors as experimental units.
#' 
#' The function \code{design.units} returns the experimental units of the design. Those are defined by random factors and their levels. See \code{units} return value below.
#' 
#' \code{design.codes} returns a dataframe or \code{tibble} of all planned observations including each observation's experimental codes, i.e. fixed and random factor levels. If you group the output, a list is returned. See \code{codes} return value below.
#' 
#' \code{design.formula} returns a list of formulas suitable for regression analysis. Currently, formulas for \code{lm} and \code{lme4} are returned. See \code{formulas} entry,
#' 
#' @rdname output.design
#' @param design The \code{factorDesign} object to summarize.
#' @param group_by If not \code{NULL}, the design matrix is grouped by these factors. Factors must be valid columns of the design matrix. If used, \code{$codes} will be a list matched to the entries in \code{$groups}.
#' @param order_by If not \code{NULL}, output within each output group is ordered by these columns.
#' @param randomize After ordering, remaining rows in the same order rank are randomly shuffled.
#' @param rename_random Should random factor levels be renamed? If \code{TRUE}, levels are renamed as strings composed of the factor name and factor level (e.g., Subj01, Subj02, ...). \code{FALSE} disables renaming of random factor levels. Alternatively, you may provide a function which should accept the vectorized ID (integer) as a first argument and the name (single character value) of the random factor as second argument or ignore it. Functions such as \code{as.double} or \code{as.integer} *are* possible because they ignore the second argument and only convert the ID.
#' @return
#' \code{output.design} returns a list containing all output summaries, including the following named entities:
#' \describe{
#'   \item{\code{codes}}{Either a \code{tibble} with all experimental codes or a list of \code{tibble}s of experimental codes. The list entries are matched to the rows of \code{$groups}.}
#'   \item{\code{groups}}{If grouped, contains a tibble in which each row represents an output group, matched to the entries in $codes. If not grouped, this is \code{NULL}.}
#'   \item{\code{ordered}}{If ordered, contains a vector of order criteria. If not ordered, this is \code{NULL}.}
#'   \item{\code{randomized}}{Value of \code{randomized}.}
#'   \item{\code{units}}{A list of random factors and their levels for this design as tibbles. Empty list if no random factors in the design.}
#'   \item{\code{formulas}}{A list of possible model formulas for use with functions such as \code{lm()} and \code{lmer()}.}
#' }
#' 
#' The functions \code{design.codes}, \code{design.formula} and \code{design.units} only return the values of the fields \code{codes} (a \code{tibble} or list or \code{tibble}s of experimental codes), \code{formulas} (a list of model formulas), and \code{units} (a list of random factors and their levels), respectively.
#' 
#' @examples 
#' 
#' des <- fixed.factor("Factor1", c("1A","1B")) +
#'        fixed.factor("Factor2", c("2A","2B")) +
#'        random.factor("Subject", c("Factor1"))
#'        
#' output.design(des)
#' design.codes(des)
#' design.units(des)
#' design.formula(des)
#'
#' @seealso \code{\link[designr]{design.formula}} for more options generating model formulae other than the suggested default ones in the summary.
#'
#' @export
output.design <- function(design, group_by = NULL, order_by = NULL, randomize = FALSE, rename_random = TRUE) {
  check_argument(design, "factorDesign")
  list(
    codes = design.codes(design, group_by, order_by, randomize, rename_random),
    groups = if(missing(group_by) || is.null(group_by)) NULL else unique(design@design[,group_by,drop=FALSE]),
    ordered = if(!missing(order_by) && length(order_by)>0L) order_by else NULL,
    randomized = !missing(randomize) && isTRUE(randomize),
    units = design.units(design),
    formulas = design.formula(design)
  )
}

#' @describeIn output.design Retrieve only the model formulas suitable for the design
#' 
#' @param contrasts The contrasts to override (\code{NULL} if none to override)
#' @param expand_contrasts If \code{TRUE}, factors with more than one contrast are replaced by so many contrasts, i.e. the result contains the names of the individual contrasts, not of the factors.
#' @param response The left-hand side of the equation. Typically, this is just the response/dependent variable.
#' @param intercepts Should an intercept be included?
#' @param env The environment in which to embed the formula
#' @param interactions Should fixed effects be additive or interactive?
#' 
#' @export
design.formula <- function(design, contrasts = NULL, expand_contrasts = !missing(contrasts), interactions=TRUE, intercepts=TRUE, response = "dv", env = parent.frame()) {
  add_them <- function(l, op) {
    if(length(l) < 1) return(NULL)
    ret <- l[[1L]]
    for(el in l[-1L]) {
      if(is.null(el)) next
      ret <- call(op, ret, el)
    }
    return(ret)
  }
  if(expand_contrasts) {
    fixed <- contrast.names(design, ranfac = NULL, contrasts = contrasts, interactions = interactions, intercept = FALSE, as_symbols = TRUE)
    random <- lapply(random.factors(design, include_interactions = FALSE), function(fac) {
      c(list(add_them(lapply(fac@name, as.symbol), ':')), contrast.names(design, ranfac = fac@name, contrasts = contrasts, interactions = interactions, intercept = FALSE, as_symbols = TRUE))
    })
  }
  else {
    fixed <- lapply(names(fixed.factors(design)), as.symbol)
    random <- lapply(random.factors(design, include_interactions = FALSE), function(fac) {
      c(list(add_them(lapply(fac@name, as.symbol), ':')), lapply(setdiff(fixed, colnames(fac@levels)), as.symbol))
    })
  }
  random_lmer <- lapply(random, function(el) call('(', call('|', add_them(c(if(intercepts) list(1) else list(), list(add_them(el[-1L], if(interactions&&!expand_contrasts) '*' else '+'))), '+'), el[[1L]])))
  random_aov <- lapply(random, function(el) call('Error', if(length(el)>1L) call('/', el[[1L]], add_them(el[-1],'*')) else el[[1L]]))
  list(
    lm = call('~',as.symbol(response),add_them(c(if(intercepts) list(1) else list(), list(add_them(fixed, if(interactions&&!expand_contrasts) '*' else '+'))), '+')),
    lmer = call('~',as.symbol(response),add_them(c(if(intercepts) list(1) else list(), list(add_them(fixed, if(interactions&&!expand_contrasts) '*' else '+')), random_lmer), '+')),
    aov = call('~',as.symbol(response),add_them(c(if(intercepts) list(1) else list(), list(add_them(fixed, if(interactions&&!expand_contrasts) '*' else '+')), random_aov), '+'))
  )
}

rename_random_default <- function(id, fac) factor(sprintf("%s%0*d", fac, nchar(as.character(max(id))), id))

#' @describeIn output.design Retrieve only the experimental units of a design
#' @param include_interactions Whether to include random factor interactions (i.e., counterbalancing factors) in the output
#' @export
design.units <- function(design, rename_random = TRUE, include_interactions = FALSE) {
  check_argument(design, "factorDesign")
  check_argument(rename_random, c("logical", "function"), 1)
  sapply(random.factors(design, include_interactions = include_interactions), function(f) {
    df <- unique(design@design[,colnames(f@levels),drop=FALSE])
    df <- df[do.call(order, as.list(df)), , drop=FALSE]
    rownames(df) <- NULL
    if(length(f@name) == 1) {
      if(isTRUE(rename_random)) {
        df[,f@name] <- rename_random_default(df[,f@name], f@name)
      } else if(is.function(rename_random)) {
        df[,f@name] <- rename_random(df[,f@name], f@name)
      } else {
        df[,f@name] <- as.integer(df[,f@name])
      }
    }
    return(df)
  }, simplify = FALSE, USE.NAMES = TRUE)
}

#' @describeIn output.design Retrieve only the codes of planned observations of an experimental design
#' @export
design.codes <- function(design, group_by = NULL, order_by = names(random.factors(design, include_interactions = FALSE)), randomize = FALSE, rename_random = TRUE) {
  check_argument(group_by, c("NULL", "character"))
  check_argument(order_by, c("NULL", "character"))
  check_argument(randomize, "logical", 1)
  check_argument(rename_random, c("logical","function"))
  if(is.null(order_by)) {
    order_by <- character(0)
  }
  if(is.null(group_by)) {
    group_by <- character(0)
  }
  if(any(!group_by %in% colnames(design@design))) {
    stop("Not all of the grouping variables are part of the design!")
  }
  if(any(!order_by %in% colnames(design@design))) {
    stop("Not all of the ordering variables are part of the design!")
  }
  if(!is.logical(randomize)) {
    stop("`randomize` must be logical (TRUE or FALSE)!")
  }
  file_groups <- if(length(group_by) > 0) unique(design@design[,group_by,drop=FALSE]) else NULL
  if(randomize) {
    data <- design@design[sample(nrow(design@design)),,drop=FALSE]
  } else {
    data <- design@design
  }
  if(length(order_by)>0L) {
    data <- data[do.call(order, unname(as.list(data[, order_by, drop=FALSE]))), , drop=FALSE]
  }
  rownames(data) <- NULL
  ranfac_names <- intersect(names(random.factors(design, include_interactions = FALSE)), colnames(data))
  fixfac_names <- intersect(names(fixed.factors(design)), colnames(data))
  data <- data[,c(ranfac_names, fixfac_names, setdiff(colnames(data), c(ranfac_names, fixfac_names)))]
  for(ranfac in names(random.factors(design, include_interactions = FALSE))) {
    if(isTRUE(rename_random)) {
      data[,ranfac] <- rename_random_default(data[,ranfac], ranfac)
      if(ranfac %in% group_by) {
        file_groups[,ranfac] <- rename_random_default(file_groups[,ranfac], ranfac)
      }
    } else if(is.function(rename_random)) {
      data[,ranfac] <- rename_random(data[,ranfac], ranfac)
      if(ranfac %in% group_by) {
        file_groups[,ranfac] <- rename_random(file_groups[,ranfac], ranfac)
      }
    } else {
      data[,ranfac] <- as.integer(data[,ranfac])
      if(ranfac %in% group_by) {
        file_groups[,ranfac] <- as.integer(file_groups[,ranfac])
      }
    }
  }
  if(length(group_by)>0L) lapply(seq_len(nrow(file_groups)), function(i) {
    df <- join(file_groups[i, , drop=FALSE], data)
    rownames(df) <- NULL
    return(tibble::as_tibble(df))
  }) else tibble::as_tibble(data)
}

#' Output a design factor summary
#' 
#' @param object the factor container to display
#'
#' @export
#'
show.factorContainer <- function(object) {
  check_argument(object, c("factorDesign","randomFactor","fixedFactor"))
  if(is(object, "factorDesign")) {
    cat(sprintf("Factor design with %d factor(s):\n", length(object)))
    for(fac in object) {
      cat(" - ")
      show(fac)
      cat("\n")
    }
    codes <- design.codes(object)
    cat(sprintf("\nDesign matrix with %d planned observations:\n", nrow(codes)))
    show(codes)
  } else if(is(object, "randomFactor")) {
    cat(sprintf("Random factor `%s` with %d group(s) and %d instance(s) (%d level(s) in total)", paste(object@name, collapse=":"), nrow(object@levels), object@replications, nrow(object@levels)*object@replications))
    if(length(object@groups)>0L) {
      cat(", grouped by ")
      cat(paste(object@groups, collapse=":"))
    }
  } else if(is(object, "fixedFactor")) {
    cat(sprintf("Fixed factor `%s` with %d level(s) (%s) and %d replication(s)", paste(object@name, collapse=":"), nrow(object@levels), paste(object@levels[,object@name], collapse=", "), object@replications))
    if(length(object@groups)>0L) {
      cat(", grouped by ")
      cat(paste(object@groups, collapse=":"))
    }
  } else {
    stop("Not a design factor or factor list!")
  }
}

`+.factorContainer` <- function(e1, e2) {
  check_argument(e1, c("factorDesign","designFactor"))
  check_argument(e2, c("factorDesign","designFactor"))
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
      e1@design[,substr(colnames(e1@design)[replacement.columns], 2, nchar(colnames(e1@design)[replacement.columns]))] <- e1@design[, replacement.columns, drop=FALSE]
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
#' @return \code{TRUE} or \code{FALSE}
#' 
#' @examples
#' 
#' x <- fixed.factor("factor", c("level1","level2"))
#' y <- random.factor("factor")
#' 
#' stopifnot(is.fixedFactor(x) && !is.randomFactor(x))
#' stopifnot(!is.fixedFactor(y) && is.randomFactor(y))
#' stopifnot(is.designFactor(x) && is.designFactor(y))
#' 
#' @export
is.designFactor <- function(fac) is(fac, "designFactor")

#' @describeIn fixed.factors Return fixed factors
#' @param include_interactions Should random factor interactions be included?
#' @export
random.factors <- function(design, include_interactions = TRUE) {
  check_argument(design, "factorDesign")
  check_argument(include_interactions, "logical", 1)
  if(!is.factorDesign(design)&&!is.list(design)) stop("`design` must be a design factor or list!")
  return(design[vapply(design, if(include_interactions) is.randomFactor else function(f) is.randomFactor(f) && length(f@name) == 1L, logical(1))])
}

#' Extract factors by type
#'
#' From a given design, extract contained random or fixed factors as a list.
#'
#' @param design The factor design to check.
#'
#' @return A list of factors that are either fixed or random.
#' 
#' @examples 
#' 
#' des <- fixed.factor("Factor1", c("1A","1B")) +
#'        fixed.factor("Factor2", c("2A","2B")) +
#'        random.factor("Subject", c("Factor1"))
#'        
#' random.factors(des)
#' fixed.factors(des)
#' 
#' stopifnot(setequal(names(random.factors(des)), c("Subject")))
#' stopifnot(setequal(names(fixed.factors(des)), c("Factor1","Factor2")))
#' 
#'
#' @export
fixed.factors <- function(design) {
  check_argument(design, "factorDesign")
  if(!is.factorDesign(design)&&!is.list(design)) stop("`design` must be a design factor or list!")
  return(design[vapply(design, is.fixedFactor, logical(1))])
}

#' Retrieve the number of observations
#' 
#' @param object A designFactor object
#' 
#' @return The number of observations
#' 
#' @examples 
#' des <- fixed.factor("Factor1", c("1A","1B")) +
#'        fixed.factor("Factor2", c("2A","2B")) +
#'        random.factor("Subject", c("Factor1"))
#'        
#' nobs(des)
#' 
#' stopifnot(nobs(des) == 4)
#' 
#' @export
setMethod("nobs", "factorDesign", function(object) nrow(object@design))

#' Concatenate design factors and designs
#'
#' By adding factors and designs by \code{+}, a new design is created that contains all of the components.
#' 
#' @param e1,e2 factor containers, such as factors or designs
#' @return A factorDesign object
#' 
#' @examples 
#' 
#' des <- fixed.factor("Factor1", c("1A","1B")) +
#'        fixed.factor("Factor2", c("2A","2B")) +
#'        random.factor("Subject", c("Factor1"))
#' 
#'
#' @export
#'
setMethod("+", signature(e1="factorContainer", e2="factorContainer"), `+.factorContainer`)

#' @describeIn factorContainer Display a factor container
#' @export
setMethod("show", "factorContainer", `show.factorContainer`)


subset.factorDesign <- function(x, subset, select = names(x), ...) {
  check_argument(x, "factorDesign")
  if(as.character(sys.calls()[[1]][[1]]) != "subset.factorDesign") fun.call <- match.call(call=sys.call(-1L))
  else fun.call <- match.call()
  if(!is.null(fun.call$select)) {
    if(any(!fun.call$select %in% names(x))) stop("Undefined factor names in `select`!")
    x[setdiff(names(x), fun.call$select)] <- NULL
  }
  keep.columns <- unique(unname(unlist(lapply(x, function(f) colnames(f@levels)))))
  x@design <- base::subset(x@design, if(is.null(fun.call$subset)) TRUE else eval(fun.call$subset, x@design), keep.columns)
  return(x)
}

#' Subset factor designs
#' 
#' @usage
#' subset(x, ...)
#' 
#' @aliases subset
#' @param x A factorDesign object
#' @param ... *subset*: Criteria along which to filter in planned observations / design matrix rows., *select*: Names of factors to keep in the design matrix
#' 
#' @return Returns a \code{factorDesign} object with a subsetted design matrix
#' 
#' @examples 
#' 
#' des <- fixed.factor("Factor1", c("1A","1B")) +
#'        fixed.factor("Factor2", c("2A","2B")) +
#'        random.factor("Subject", c("Factor1"))
#'        
#' subset(des, select = "Subject")
#' subset(des, Factor1 == "1A" | Factor2 == "2B", "Subject")
#' 
#' @export
setMethod("subset", c("factorDesign"), `subset.factorDesign`)


#' Retrieve contrast codes for a design
#' 
#' This function can be used to retrieve contrast codes based on experimental codes / planned observations.
#' 
#' @param design A design object
#' @param factors Which fixed factors to include in the output
#' @param contrasts Contrasts to use for each categorical factor. Should be a list named after the fixed effects and containing contrast matrices, such as the ones generated by the standard contrast functions. If NULL or the fixed factor is not found in the list, default contrasts are used (typically, treatment contrasts).
#' @param expand If TRUE, a design matrix is returned. If FALSE, all factors (and interactions) with their respective levels are returned.
#' @param rename_contrasts This is the pattern after which columns in the design matrix are named. By default, this is a direct concatenation of factor name and contrast name.
#' @param intercept If TRUE, an intercept is added to the matrix. Its value is 1 for all observations.
#' @param interactions If TRUE, interactions of fixed factors are included.
#' @param include_random_levels If TRUE, levels of random factors are included in the matrix.
#' @param ranfac Return random-effects contrast names for a given random factor. If NULL, return fixed-effects contrast names.
#' @param as_symbols Return contrast names as symbols rather than strings (character vectors).
#' @param ... Arguments to pass on to design.contrasts()
#' @return A design matrix (if expand==TRUE, default) or a list of factor levels (if expand==FALSE) for design.contrasts or contrast names for contrast.names.
#' 
#' @examples 
#' 
#' 
#' des <- fixed.factor("Factor1", c("1A","1B")) +
#'        fixed.factor("Factor2", c("2A","2B")) +
#'        random.factor("Subject", c("Factor1"))
#' 
#'               
#' design.contrasts(des)
#' 
#' contrast.names(des)
#' 
#' stopifnot(contrast.names(des) == c("Factor11B", "Factor22B"))
#' 
#' contrast.names(des, as_symbols = TRUE)
#' 
#' design.contrasts(des, contrasts = list(Factor2 = contr.sum))
#' 
#' contrast.names(des, contrasts = list(Factor2 = contr.sum))
#' 
#' @export
design.contrasts <- function(design, factors = names(fixed.factors(design)), contrasts = NULL, expand = TRUE, rename_contrasts = "%1$s%2$s", intercept = FALSE, interactions = FALSE, include_random_levels = FALSE) {
  check_argument(design, "factorDesign")
  check_argument(factors, "character")
  check_argument(contrasts, c("NULL","list"))
  check_argument(expand, "logical", 1)
  check_argument(rename_contrasts, "character", 1)
  check_argument(intercept, "logical", 1)
  check_argument(interactions, "logical", 1)
  check_argument(include_random_levels, "logical", 1)
  if(!is.null(contrasts)&&!is.list(contrasts)) stop("`contrasts` must be NULL or a named list!")
  main.contrasts <- lapply(design[factors], function(fac) {
    if(is.null(fac)||!is.fixedFactor(fac)) stop(sprintf("At least one factor name given does not refer to a fixed factor"))
    if(inherits(fac@levels[,fac@name], "factor") || !is.null(contrasts[[fac@name]])) {
      retfac <- fac@levels[,fac@name]
      if(!is.factor(retfac)) retfac <- factor(retfac)
      if(!is.null(contrasts)&&!is.null(contrasts[[fac@name]])) {
        if(is.character(contrasts[[fac@name]]) || is.function(contrasts[[fac@name]]))
          stats::contrasts(retfac) <- do.call(contrasts[[fac@name]], list(length(retfac), contrasts = TRUE, sparse = FALSE))
        else if(is.matrix(contrasts[[fac@name]]))
          stats::contrasts(retfac) <- contrasts[[fac@name]]
        else
          stop(sprintf("contrasts$`%s` is not a contrast matrix or contrast function!", fac@name))
      }
      retfac <- stats::contrasts(retfac)
      if(expand) {
        retfac <- retfac[design@design[,fac@name],,drop=FALSE]
        rownames(retfac) <- NULL
      }
      if(!is.null(rename_contrasts))
        colnames(retfac) <- sprintf(rename_contrasts, fac@name, if(is.null(colnames(retfac))) seq_len(ncol(retfac)) else colnames(retfac))
      return(retfac)
    }else if(inherits(fac@levels[,fac@name], c("numeric", "integer"))) {
      if(expand) {
        retfac <- design@design[,fac@name,drop=FALSE]
        rownames(retfac) <- NULL
      } else
        retfac <- fac@levels[,fac@name,drop=FALSE]
      if(!is.null(rename_contrasts))
        colnames(retfac) <- fac@name
      return(retfac)
    }else{
      stop(sprintf("Don't know how to expand factor `%s` of type %s!", fac@name, class(fac@levels[,fac@name])))
    }
  })
  if(interactions) {
    if(!expand) stop("`expand` cannot be FALSE if `interactions` is TRUE")
    ncontrasts <- list()
    for(i in seq_along(main.contrasts)) {
      ncontrasts <- c(ncontrasts, main.contrasts[i], lapply(seq_along(ncontrasts), function(j) {
        do.call(cbind, do.call(cbind, lapply(colnames(main.contrasts[[i]]), function(icol) {
          lapply(colnames(ncontrasts[[j]]), function(jcol) {
            ret <- main.contrasts[[i]][,icol,drop=FALSE] * ncontrasts[[j]][,jcol,drop=FALSE]
            colnames(ret) <- paste0(jcol,':',icol)
            rownames(ret) <- NULL
            return(ret)
          })
        })))
      }))
    }
    if(intercept) {
      ncontrasts <- c(list(data.frame(`1` = 1.0)), ncontrasts)
      names(ncontrasts[[1]])[1] = "(Intercept)"
    }
    if(include_random_levels) {
      for(ranfac in random.factors(design, include_interactions = FALSE)){
        if(expand) {
          main.contrasts <- c(main.contrasts, list(design@design[,ranfac@name,drop=FALSE]))
          names(main.contrasts)[length(main.contrasts)] <- ranfac@name
        }else {
          main.contrasts <- c(main.contrasts, list(sort(unique(design@design[,ranfac@name,drop=TRUE]))))
          names(main.contrasts)[length(main.contrasts)] <- ranfac@name
        }
      }
    }
    ret <- do.call(data.frame, ncontrasts)
    colnames(ret) <- unlist(sapply(ncontrasts, function(i) colnames(i)))
    return(ret)
  }else {
    if(intercept) {
      main.contrasts <- c(list(data.frame(`1` = 1.0)), main.contrasts)
      names(main.contrasts[[1]])[1] = "(Intercept)"
    }
    if(include_random_levels) {
      for(ranfac in random.factors(design, include_interactions = FALSE)){
        if(expand) {
          main.contrasts <- c(main.contrasts, list(design@design[,ranfac@name,drop=FALSE]))
          names(main.contrasts)[length(main.contrasts)] <- ranfac@name
        }else {
          main.contrasts <- c(main.contrasts, list(sort(unique(design@design[,ranfac@name,drop=TRUE]))))
          names(main.contrasts)[length(main.contrasts)] <- ranfac@name
        }
      }
    }
    if(expand){
      ret <- do.call(data.frame, main.contrasts)
      colnames(ret) <- unlist(sapply(main.contrasts, function(i) colnames(i)))
      return(ret)
    }else if(!expand) {
      return(main.contrasts)
    }
  }
}

#' Retrieve contrast names for a design
#' 
#' Use this function to retrieve the names for contrasts in an experimental design, such as used by lm, lmer and many other regression models.
#' 
#' @describeIn design.contrasts Retrieve contrast names for a design
#' @export
contrast.names <- function(design, ranfac = NULL, as_symbols = FALSE, ...) {
  check_argument(design, "factorDesign")
  check_argument(ranfac, c("NULL","character"))
  check_argument(as_symbols, "logical", 1)
  facnames <- names(fixed.factors(design))
  if(!is.null(ranfac)) {
    if(is.null(design[[ranfac]])||!is.randomFactor(design[[ranfac]])) stop(sprintf("Random factor `%s` does not exist", ranfac))
    facnames <- setdiff(facnames, design[[ranfac]]@groups)
  }
  cnames <- colnames(design.contrasts(design = design, factors = facnames, ...))
  if(as_symbols) {
    cnames <- lapply(strsplit(cnames, ':', TRUE), function(nm) {
      ret <- as.symbol(nm[1L])
      for(el in nm[-1L])
        ret <- call(':', ret, as.symbol(el))
      return(ret)
    })
  }
  return(cnames)
}
