gm2 = function (A, transform = FALSE, ...) 
{
  if (is.array(A)) {
    dims <- dim(A)
    if (length(dims) == 3) {
      if (any(is.na(A))) 
        stop("Data matrix contains missing values. Estimate these first (see 'estimate.missing').\n", 
             call. = FALSE)
      p <- dims[1]
      k <- dims[2]
      n <- dims[3]
    }
    else if (length(dims) == 2) {
      n <- dims[1]
      k <- NULL
      p <- dims[2]
    }
  }
  else {
    A <- try(as.matrix(A), silent = TRUE)
    if (inherits(A, "try-error")) 
      stop("Data not of a form coercible to matrix or array.\n", 
           call. = FALSE)
    dims <- dim(A)
    n <- dims[1]
    k <- NULL
    p <- dims[2]
  }
  ord.args <- list(...)
  if (!is.null(k)) {
    Y <- two.d.array(A)
    ord.args$Y <- Y
  }
  else ord.args$Y <- Y <- A
  if (is.null(ord.args$tol)) 
    ord.args$tol <- sqrt(.Machine$double.eps)
  ord.args$transform. = transform
  out <- do.call(ord2, ord.args)
  if (!is.null(k)) {
    out$A <- A
    out$shapes <- lapply(1:ncol(out$x), function(x) {
      shape.predictor(A, out$x[, x], min = min(out$x[, 
                                                     x]), max = max(out$x[, x]))
    })
    out$shapes <- lapply(out$shapes, function(x) {
      lapply(x, function(j) geomorph:::cs.scale(j))
    })
    names(out$shapes) <- paste("shapes.comp", 1:length(out$d), 
                               sep = "")
  }
  if (!is.null(rownames(Y))) 
    out$x <- out$x[rownames(as.matrix(Y)), ]
  class(out) <- c("gm.prcomp", class(out))
  out
}