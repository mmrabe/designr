

# general tools

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

find.in.list <- function(what, where, all=T) {
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
  #if(!all(vapply(elements, is.logical, logical(1)))) stop("All arguments must be logical!")
  #if(!all(vapply(elements[-1], function(vec) length(vec), integer(1))==length(elements[[1]]))) stop("All arguments must be same length!")
  if(length(elements) == 0) return(logical(0))
  ret <- ifelse(is.na(elements[[1]]), T, elements[[1]])
  for(element in elements[-1]) ret <- ret & ifelse(is.na(element), T, element)
  ret
}

join <- function(...) {
  elements <- list(...)
  if(!all(vapply(elements, is.data.frame, logical(1)))) stop("All arguments must be data.frames!")
  if(length(elements) == 0L) return(tibble::tibble())
  ret <- elements[[1]]
  rownames(ret) <- NULL
  for(element in elements[-1]) {
    if(nrow(element)==0) next
    rownames(element) <- NULL
    if(nrow(ret)==0) {
      ret <- element
      next
    }
    col.matches <- intersect(colnames(element), colnames(ret))
    new.col.ids <- setdiff(colnames(element), colnames(ret))
    old.col.ids <- setdiff(colnames(ret), colnames(element))
    matches.x <- integer(0)
    matches.y <- integer(0)
    for(row in seq_len(nrow(ret))) {
      if(length(col.matches)==0L) new.rows <- seq_len(nrow(element))
      else {
        new.rows <- seq_len(nrow(element))
        for(col in col.matches) {
          if(is.na(ret[row, col])) next
          new.rows <- new.rows[is.na(element[new.rows,col])|element[new.rows,col]==ret[row, col]]
        }
      }
      matches.x <- c(matches.x, rep(row, length(new.rows)))
      matches.y <- c(matches.y, new.rows)
    }
    ret <- cbind(ret[matches.x, , drop=F], element[matches.y, new.col.ids, drop=F])
    rownames(ret) <- NULL
  }
  tibble::as.tibble(ret)
}

sorto <- function(vec, order) {
  vec[order(match(vec, order))]
}
