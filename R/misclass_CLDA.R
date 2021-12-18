#' Compute misclassification error rates
#'
#' @param preds Vector of predictions
#' @param y Vector of true values
#'
#' @return
#' @export
misclass.CLDA <- function(preds, y) {
  stopifnot(sum(preds %in% c(0, 1)) == length(preds))
  stopifnot(sum(y %in% c(0, 1)) == length(y))
  stopifnot(length(preds) == length(y))
  
  return(mean(preds != y))
}