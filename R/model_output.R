#' @export
coef.hglm <- function(hglm_out){
  # warning("The function is yet to be implemented.")
  coef <- hglm_out$coef
  cat(paste0("The coefficients are", coef))
  return(coef)
}

#' @export
vcov.hglm <- function(hglm_out){
  warning("The function is yet to be implemented.")
  cat("The variance-covariance matrix is ..." )
}

#' @export
print.hglm <- function(hglm_out){
  warning("The function is yet to be implemented.")
  cat("The model is ...")
}
