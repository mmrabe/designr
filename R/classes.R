# class definitions

#' @importFrom methods as new is show
#' @importFrom stats contrasts
NULL

#' Design matrix S4 functions
#' 
#' @param object A \code{factorDesign} object.
#' 
#' @examples
#' des <- factor.design()
#' des <- fixed.factor("Factor1", c("1A","1B")) +
#'        fixed.factor("Factor2", c("2A","2B")) +
#'        random.factor("Subject", c("Factor1"))
#' 
#' @export
setClass("factorContainer")
setClass("designFactor", slots=c(name="character", levels="data.frame", extra="list", replications="integer", groups="character"), contains="factorContainer")
setClass("randomFactor", contains="designFactor")
setClass("fixedFactor", slots=c(blocked="logical"), contains="designFactor")

#' S4 Methods for designFactor
#' 
#' @param x a \code{factorDesign}
#' 
#' @examples 
#' x <- fixed.factor("Factor1", c("1A","1B"))
#' y <- random.factor("Subject", c("Factor1"))
#' 
#' @export
setClass("factorDesign", slots=c(design="data.frame"), contains=c("list", "factorContainer"))



# constructors


#' Random factors
#'
#' This function creates an instance of \code{randomFactor} to be used in a \code{factorDesign}. A random factor is typically related to an experimental unit such as Subject, Item, Experimenter, ect. and does not have preset levels.
#'
#' @param name Name of the random factor as a character vector. Typically, this should be a length-1 vector (i.e., a single string) but you may pass multiple names of random factors whose interaction is to be nested in groups (see *Assignment Constraints*).
#' @param instances Number of times (as a single integer value) each level (instantiation) is to be replicated.
#' @param groups Names of fixed and random factors that are to be used as grouping (nesting/between) levels.
#' @param ... Additional arguments to be stored as \code{extra} values.
#' @return An instance of the class \code{randomFactor}.
#'
#' @section Nesting Random Factors:
#' A typical case of nesting in a psychological experiment is to vary a factor between subjects. That means that each subject would only be assigned to one condition of the nesting fixed factor (such as type of instruction). All other fixed factors that are not listed under `groups` are considered to vary within the random factor. Note that nesting increases the number of replications of the random factor.
#'
#' Note that a random factor may be nested within fixed and/or other random factors but fixed factors may only be nested within levels of other fixed factors.
#'
#' @section Assignment Constraints:
#' A random interaction (a random factor instantiated with more than one name) governs the assignment of the co-occurence of the listed random factors.
#' That means that, for example, \code{random.factor(c("Subject","Item"), groups="correct")} ensures that the assignment of a Subject and Item to one another occurs in only *one* of the conditions of \code{correct}.
#' You may use \code{assign = '...'} to provide the method to counterbalance the assignment. By default, \code{'latin.square'} is used but you may also use \code{'random.order'} or \code{'permutations'}. Note that, depending on the assignment method you use, the constituting random factors (in this case \code{Subject} and \code{Item}) will be replicated n-times (\code{n} being the number of conditions of the nesting factors) for \code{'latin.square'} and \code{'random.order'} and n!-times for \code{'permutations'}.
#'
#'
#' @examples
#' # A random factor Subject that nests factors blockOrder and gender,
#' # i.e. blockOrder and gender are "between-subject"
#' 
#' random.factor("Subject", groups=c("blockOrder", "gender"))
#' 
#' # A random factor Item without any grouping
#' random.factor("Item")
#' 
#'
#' @seealso \code{\link[designr]{fixed.factor}}
#' @export
random.factor <- function(name, groups = character(0), instances = 1L, ...) {
  check_argument(name, "character")
  if(any(vapply(name, function(n) substr(n,1,1) == '*', logical(1)))) stop("Factor names must not start with an asterisk!")
  check_argument(instances, "numeric", function(x) !x%%1, 1, expression(x >= 1))
  check_argument(groups, "character")
  levels <- do.call(data.frame, sapply(name, function(n) NA_integer_, USE.NAMES = TRUE, simplify = FALSE))
  new("randomFactor", name = name, levels=levels, groups=groups, replications = as.integer(instances), extra = list(...))
}

#' Fixed factors
#'
#' This function creates an instance of \code{fixedFactor} to be used in a \code{factorDesign}. Fixed factors typically relate to (quasi-)experimental factors such as experimental conditions/manipulations, subject/item characteristics ect.
#'
#' @param name Name of the fixed factor.
#' @param levels If not grouped, a vector of factor levels. Any atomic data type (character, logical, numeric, integer) can be used. If grouped, this should be a named list with each entry being a vector (as described before) and its name being a value of the grouping factor(s). If grouped within several factors, i.e. an interaction, the values constituting the names should be concatenated by colons (:), e.g. \code{list(`f1l1:f2l1`=1:2, `f1l2:f2l1`=3:4, ...)}. If for any group there are no levels specified, a warning will be issued and \code{NA} will be assigned as the value for this factor. If this is intended and the warning should be suppressed, please explicitly assign \code{NA} as the value for that group, e.g. \code{list(`f1l1:f2l1`=1:2, `f1l2:f2l1`=NA, ...)}.
#' @param replications Either a single integer or an integer vector of the same length as \code{levels} that is used to determine how many times each factor level should be repeated.
#' @param blocked Set this to \code{TRUE} if the levels of this factor are blocked. In that case, a factor is created whose factor levels are different sequences of the levels specified in the function call.
#' @param assign If \code{blocked = TRUE}, you may specify a different method of rotating levels. The default if \code{'latin.square'} but \code{'permutations'}, \code{'williams'}, and \code{'random.order'} are also available.
#' @param character_as_factor If this is \code{TRUE}, character vectors passed in \code{levels} are automatically converted to a factor type.
#' @param block_name If \code{blocked = TRUE}, by default, there is not only a design matrix column created that contains the complete sequence of block levels but also a column for each position of the sequence with its assigned level. You may specify a different naming pattern using \code{\link[base]{sprintf}} naming conventions. The first argument passed is the factor name and the second argument is the sequence position (starting at 1). The default column names will be \code{factor.1}, \code{factor.2}, etc. If \code{NULL}, no additional block columns are created.
#' @param groups Names of fixed factors in which to nest this fixed factor (see *Nesting fixed factors*).
#' @param is_ordered Is this an ordered factor?
#' @param ... more data to save as attributes
#' @return An instance of \code{fixedFactor}.
#'
#' @section Nesting Fixed Factors:
#' If \code{groups} is used, the function will attempt to nest levels of the newly created factor within levels/interactions of the specified grouping factors. Note that nesting of fixed effects is only allowed within other fixed effects combinations but not within random effects. For each combination of the grouping factors, e.g. each group, you should specify an individual vector of levels (see above). If you fail to supply levels for any group, \code{NA}s will be assigned. This could result in unpredicted behavior when more factors are added. If you know what you are doing and would like to suppress the warning, please explicitly specify \code{NA} as the (only) value to assign to that group. At any rate, it is highly recommended to run sanity checks on the balancedness of the design if you are nesting fixed factors!
#'
#' @examples
#' fixed.factor("correct", levels=c(TRUE, FALSE))
#' fixed.factor("age", levels=c("child", "youth", "adult"))
#' fixed.factor("order", levels=c("task1", "task2", "task3"), blocked = TRUE, assign="latin.square")
#'
#' @seealso \code{\link[designr]{random.factor}}
#' @export
fixed.factor <- function(name, levels, blocked = FALSE, character_as_factor = TRUE, is_ordered = FALSE, block_name = "%1$s.%2$d", groups = character(0), replications = 1L, assign = "latin.square", ...) {
  check_argument(groups, "character")
  is.grouped <- length(groups) > 0L
  check_argument(name, "character", 1)
  if(substr(name,1,1) == '*') stop("Factor names must not start with an asterisk!")
  if(length(groups) == 0) check_argument(levels, c("character","numeric","logical"), expression(length(x) >= 1))
  else if(length(groups) > 0L && (!is.list(levels) || !all(vapply(levels, function(glevels) is.vector(glevels) && length(glevels) >= 1L, logical(1))))) stop("If `groups` are given (i.e., factor is nested), `levels` must be a list of vectors, each with a minimum length of 1!")
  check_argument(replications, "numeric", function(x) !x%%1, expression(x >= 1), expression(length(x) == length(levels) || length(x) == 1))
  check_argument(blocked, "logical", 1)
  check_argument(character_as_factor, "logical", 1)
  check_argument(is_ordered, "logical", 1)
  if(!is.list(levels) && is.vector(levels)) levels <- list(levels)
  if(length(unique(names(levels))) != length(names(levels))) stop("Names of levels list must be unique!")
  
  n.max.levels <- as.integer(max((vapply(levels, length, integer(1L)))))
  coerce.character <- any(vapply(levels, is.character, logical(1L)))
  
  glevels <- sapply(levels, function(levels) {
    if(blocked) {
      rotation.function <- find.rot.fun.in.extra(list(assign=assign), latin.square)
      levels <- rep(levels, each=replications)
      mat <- rotation.function(length(levels))
      lvmat <- do.call(data.frame, lapply(seq_len(n.max.levels), function(i) {
        if(i>ncol(mat)) return(NA)
        vals <- levels[mat[,i,drop=TRUE]]
        if(!is.character(vals) && coerce.character) vals <- as.character(vals)
        if(is.character(vals) && character_as_factor) factor(vals, levels=unique(levels), ordered = is_ordered)
        else vals
      }))
      if(!is.null(block_name)) colnames(lvmat) <- sprintf(block_name, name, seq_len(n.max.levels))
      lvmat[,name] <- factor(apply(lvmat[,seq_along(levels),drop=FALSE], 1, paste, collapse="-"))
      if(is.null(block_name)) lvmat[,-ncol(lvmat)] <- NULL
      levels <- lvmat
    }else {
      vals <- rep(levels, each=replications)
      if(!is.character(vals) && coerce.character) vals <- as.character(vals)
      if(is.character(vals) && character_as_factor) levels <- data.frame(factor(vals, levels=unique(levels), ordered = is_ordered))
      else levels <- data.frame(x=vals)
      colnames(levels) <- name
    }
    return(levels)
  }, simplify = FALSE)
  
  if(!is.null(names(glevels))) for(i in seq_along(glevels)) {
    glevels[[i]][,'*'] <- factor(names(glevels)[i], levels=names(glevels))
  }
  
  levels <- do.call(rbind, glevels)
  
  rownames(levels) <- NULL
  
  new("fixedFactor", name = name, blocked = blocked, replications = 1L, groups = groups, levels = levels, extra = list(assign = assign, ...))
}

#' Factorial Designs
#'
#' The main function of this package is to create factorial designs with this function.
#'
#' @param ... Factors to add to the design.
#' @return An instance of \code{factorDesign} with the complete factorial design and all fixed and random factors.
#' @seealso \code{\link[designr]{random.factor}} and \code{\link[designr]{fixed.factor}} for creating factors to add to the design. \code{\link[designr]{output.design}} and \code{\link[designr]{write.design}} for creating a useful summary and writing it into output files.
#' @examples
#' # To create an empty design:
#' design <- factor.design()
#'
#' # To create a design for a recognition memory experiment in
#' # which each participant only sees either picture or words:
#' design <- factor.design(
#'     fixed.factor("type",levels=c("pic","word")), 
#'     fixed.factor("status",levels=c("old","new")), 
#'     random.factor("subject", groups="type"), 
#'     random.factor("item", groups="type"), 
#'     random.factor(c("subject","item"), groups="status")
#' )
#'
#' # This is identical to:
#' design <- fixed.factor("type",levels=c("pic","word")) + 
#'           fixed.factor("status",levels=c("old","new")) + 
#'           random.factor("subject", groups="type") + 
#'           random.factor("item", groups="type") + 
#'           random.factor(c("subject","item"), groups="status")
#'
#' # Or:
#' design <- factor.design(
#'    ~type(pic,word)+status(old,new)+subject[type]+item[type]+subject:item[status]
#' )
#'
#' # You can also create a new design by adding more factors to an existing one:
#'
#' design1 <- factor.design(~type(pic,word)+status(old,new)+subject[type]+item[type])
#' design2 <- design1 + random.factor(c("subject","item"), groups="status")
#'
#' @export
factor.design <- function(...) {
  elements <- unname(list(...))
  ret <- new("factorDesign")
  for(element in elements) {
    if(is(element, "designFactor")) {
      ret <- `+.factorContainer`(ret, element)
    }else if(is(element, "factorDesign")) {
      ret <- `+.factorContainer`(ret, element)
    }else if(is.language(element)) {
      element <- parse.factor.language(element)
      ret <- `+.factorContainer`(ret, element)
    }else{
      stop("All arguments must be design factors (random or fixed), factor lists or factor formulas!")
    }
  }
  ret
}
