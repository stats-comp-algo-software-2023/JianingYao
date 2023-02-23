mle_psudo_inverse <- function(design, outcome){
  # use Cholesky decomposition to perform pseudo-inverse
  a <- crossprod(design)
  b <- crossprod(design, outcome)
  upper <- chol(a)
  z <- backsolve(upper, b, transpose = TRUE)
  beta <- as.vector(backsolve(upper, z))
  return(beta)
}

