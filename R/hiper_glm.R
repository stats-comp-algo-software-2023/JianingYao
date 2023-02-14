#' @export
hiper_glm <- function(design, outcome, model = 'linear') {
  supported_model <- c('linear', 'logit')
  if (! (model %in% supported_model)){
    stop(sprintf("The model %s is not supported.", model))
  }
  warning("The function is yet to be implemented.")
  # TODO: model fitting by MLE
  hglm_out <- list()
  class(hglm_out) <- 'hglm'
  return(hglm_out)
}


