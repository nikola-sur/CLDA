#' predict.CLDA
#' Make predictions on a new set of data from a CLDA object.
#'
#' @param mod CLDA object
#' @param x New data
#'
#' @return
#' @export
#'
#' @examples
predict.CLDA <- function(mod, x) {
  # Basic input checks ---
  stopifnot(is.matrix(x))
  stopifnot(ncol(x) == ncol(mod$params$x))
  
  
  # Make predictions ---
  # val0 <- t(mod$beta) %*% mod$Sigma_w %*% mod$beta - 2*log(mod$n_0/mod$n)
  preds <- predict_CLDA_cpp(mod, x)
  
  return(preds)
}