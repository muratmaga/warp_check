ord2 = function (Y, transform. = TRUE, scale. = FALSE) 
{
  Ycheck <- try(as.matrix(Y), silent = TRUE)
  if (inherits(Ycheck, "try-error")) 
    stop("Y must be a matrix or data frame.\n", call. = FALSE)
  id <- if (is.vector(Y)) 
    names(Y)
  else rownames(Y)
  Y <- as.matrix(Y)
  dims <- dim(Y)
  n <- dims[1]
  p <- dims[2]
  if (is.null(id)) 
    id <- 1:n
  rownames(Y) <- id
  
  I <- Matrix(diag(n), sparse = TRUE)
  A <- I
  
  X <- matrix(1, n)
  rownames(X) <- id
  
  cen <- colMeans(Y)
  Z <- as.matrix(scale(Y, center = cen, scale = scale.))
  cen <- attr(Z, "scaled:center")
  sc <- attr(Z, "scaled:scale")
  if (any(sc == 0)) 
    stop("cannot rescale a constant/zero column to unit variance")
  
  k <- min(n, p)
  tf <- transform.
  
  if (tf) 
    Z <- as.matrix(Z)
  rownames(Z) <- id
  
  Saz <- crossprod(A, Z)
  Saz <- as.matrix(Saz)
  s <- svd(Saz, nu = 0, nv = k)
  
  j <- seq_len(k)
  s$v <- s$v[, j]
  s$d <- s$d[j]
  sy <- sum(svd(Z)$d^2)
  s$d <- s$d^2/sum(s$d^2) * sy/max(1, n - 1)
  s$sdev <- sqrt(s$d)
  
  s$v <- as.matrix(s$v)
  dimnames(s$v) <- list(colnames(Z), paste0("Comp", j))
  
  r <- list(d = s$d, sdev = s$sdev, rot = s$v, center = cen, 
            scale = if (is.null(sc)) FALSE else sc, transform = tf)
  r$x <- as.matrix(Z %*% s$v)
  rownames(r$x) <- id
  names(r$d) <- names(r$sdev) <- colnames(r$rot)
  
  class(r) <- "ordinate"
  r
}