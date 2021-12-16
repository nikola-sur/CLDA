#' CLDA
#'
#' @param x Numeric matrix of features.
#' @param y Binary-valued vector of class labels.
#' @param linear Boolean: Whether to use LDA (true) or QDA (false).
#' @param type Type of compression. One of "compressed", "projected", "FRF", "sub-sampled", or "full".
#' For QDA, "projected" and "FRF" are not implemented.
#' @param m Reduced sample size.
#' @param gamma Tuning parameter for numerical stability (see the paper). Defaults to 0.01.
#'
#' @return
#' @export
CLDA <- function(x, y, linear, type, m, gamma = 0.01) {
  
  return(x)
}