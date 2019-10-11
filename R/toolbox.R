

# general tools


check_argument <- function(val, ...) {
  val <- tryCatch(val, error = function(e) e)
  if(is(val, "error")) stop(val$message, call. = FALSE)
  argname <- as.character(as.expression(match.call()$val))
  if(length(argname) > 1) stop("Must be single character")
  for(tst in list(...)) {
    if(is.numeric(tst) && is.vector(val)) {
      if(length(val) != tst) {
        stop(sprintf("`%s` has a length of %d but must have a length of %d.", argname, length(val), tst), call. = FALSE)
      }
    } else if(is.character(tst)) {
      classValid <- FALSE
      for(cls in tst) {
        if(cls == "numeric" && is.numeric(val)) {
          classValid <- TRUE
        } else if(grepl("^list:", cls)) {
          classValid <- is.list(val) && all(vapply(val, function(x) is(x, substring(cls, 6)), logical(1)))
        } else {
          classValid <- is(val, cls)
        }
        if(classValid) break
      }
      if(!classValid) {
        stop(sprintf("`%s` must be of type %s but is %s.", argname, paste(tst, collapse=","), paste(class(val), collapse=",")), call. = FALSE)
      }
    } else if(is.function(tst)) {
      if(!all(tst(val))) {
        stop(sprintf("`%s` has an invalid value.", argname), call. = FALSE)
      }
    } else if(is.expression(tst)) {
      if(!isTRUE(all(eval(tst, list(x = val))))) {
        test_string <- if(tst[[1]][[1]] == "<" && tst[[1]][[2]] == "x") {
          sprintf("be smaller than %s", as.character(tst[[1]][[3]]))
        } else if(tst[[1]][[1]] == "<=" && tst[[1]][[2]] == "x") {
          sprintf("be smaller than or equal to %s", as.character(tst[[1]][[3]]))
        } else if(tst[[1]][[1]] == ">" && tst[[1]][[2]] == "x") {
          sprintf("be greater than %s", as.character(tst[[1]][[3]]))
        } else if(tst[[1]][[1]] == ">=" && tst[[1]][[2]] == "x") {
          sprintf("be greater than or equal to %s", as.character(tst[[1]][[3]]))
        } else if(tst[[1]][[1]] == "==" && tst[[1]][[2]] == "x") {
          sprintf("be equal to %s", as.character(tst[[1]][[3]]))
        } else {
          sprintf("satisfy %s", as.character(tst))
        }
        stop(sprintf("`%s` must %s!", argname, test_string), call. = FALSE)
      }
    }
  }
}


latin.square <- function(n) as.matrix(vapply(1:n, function(i) c(i:n, 1:i)[-(n+1)], integer(n)))
permutations <- function(n, lv = seq_len(n)) {
  if(!is.numeric(n)||length(n)!=1L||n<1L) stop("`n` must be a positive integer of length 1!")
  if(!is.integer(lv)||n!=length(lv)) stop("`lv` must be an integer vector of length `n`!")
  if(n==1L) return(matrix(lv[1L]))
  else if(n == 2L) return(matrix(lv[c(1L,2L,2L,1L)], ncol=2L))
  else{
    ret <- matrix(integer(factorial(n)*n), ncol=n)
    ret[,1L] <- rep(lv, each=factorial(n-1L))
    for(i in seq_len(n)) {
      smx <- sys.function()(n=n-1L, lv=lv[-i])
      ret[seq(1L+(i-1L)*nrow(smx), i*nrow(smx)), -1L] <- smx
    }
    return(ret)
  }
}
random.order <- function(n, m=n) {
  if(!is.numeric(n)||length(n)!=1L||n<1L) stop("`n` must be a positive integer of length 1!")
  if(!is.numeric(m)||length(m)!=1L||m<1L) stop("`m` must be a positive integer of length 1!")
  ret <- matrix(integer(n*m), ncol=n)
  for(i in seq_len(m)) ret[i,] <- sample(n)
  return(ret)
}

find.in.list <- function(what, where, all=TRUE) {
  ret <- integer(0)
  for(i in seq_along(where))
    if(where[[i]] == what) {
      if(!all) return(i)
      ret <- c(ret, i)
    }
  return(ret)
}

and <- function(...) {
  elements <- list(...)
  if(length(elements) == 0) return(logical(0))
  ret <- ifelse(is.na(elements[[1]]), TRUE, elements[[1]])
  for(element in elements[-1]) ret <- ret & ifelse(is.na(element), TRUE, element)
  ret
}

syms <- function(x) lapply(x, as.symbol)

na_join <- function(.data, b) {
  b <- dplyr::as_tibble(b)
  .data <- dplyr::as_tibble(.data)
  colmatches <- intersect(colnames(.data), colnames(b))
  if(length(colmatches) == 0) {
    cbind(.data[rep(seq_len(nrow(.data)), each=nrow(b)),,drop=FALSE], b)
  } else {
    where.na <- is.na(b[,colmatches,drop=FALSE])
    d2 <- do.call(dplyr::group_by, c(list(.data = as.data.frame(where.na)), syms(colmatches)))
    indices <- dplyr::group_rows(d2)
    b.uniq.cols <- setdiff(colnames(b), colmatches)
    for(ix in indices) {
      what.na <- where.na[ix[1],]
      if(!any(what.na)) {
        next
      }
      na.cols <- colmatches[what.na]
      vals.in.a <- unique(.data[,na.cols,drop=FALSE])
      b <- rbind(b[-ix,,drop=FALSE], cbind(b[rep(ix, each=nrow(vals.in.a)), b.uniq.cols, drop=FALSE], vals.in.a))
    }
    dplyr::inner_join(.data, b, by = colmatches)
  }
}

join <- function(...) {
  dfs <- list(...)
  if(!all(vapply(dfs, is.data.frame, FALSE))) stop("All arguments to `join` must be data frames!")
  if(length(dfs) == 0) return(tibble::tibble())
  ret <- dfs[[1]]
  for(el in dfs[-1]) {
    ret <- na_join(ret, el)
  }
  ret
}

sorto <- function(vec, order) {
  vec[order(match(vec, order))]
}
