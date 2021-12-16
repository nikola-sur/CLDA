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
  # Basic input checks ---
  stopifnot(is.matrix(x))
  stopifnot(sum(y %in% c(0,1)) == length(y))
  stopifnot(is.logical(linear))
  stopifnot(type %in% c("compressed", "projected", "FRF", "sub-sampled", "full"))
  stopifnot(m <= nrow(x))
  stopifnot(is.double(gamma))
  
  # Collect inputs ---
  n <- nrow(x)
  p <- ncol(x)
  misclass_rate = 0.0
  exec_time = 0.0
  
  
  # Start LDA ---
  
  
  
  
  
  
  return(list(
    x             = x,
    y             = y,
    linear        = linear,
    type          = type,
    m             = m,
    gamma         = gamma,
    misclass_rate = misclass_rate,
    exec_time     = exec_time
  ))
}