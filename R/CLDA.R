#' CLDA
#' Various LDA/QDA functions and compression schemes.
#'
#' @param x Numeric matrix of features.
#' @param y Binary-valued vector of class labels.
#' @param linear Boolean: Whether to use LDA (true) or QDA (false).
#' @param type Type of compression. One of "compressed", "projected", "FRF", "sub-sampled", or "full".
#' For QDA, "projected" and "FRF" are not implemented.
#' @param m Reduced number of samples
#' @param gamma Tuning parameter for numerical stability (see the paper). Defaults to 0.01.
#'
#' @return
#' @export
CLDA <- function(x, y, linear, type, m, gamma = 0.01) {
  # Basic input checks ---
  stopifnot(is.matrix(x))
  stopifnot(sum(y %in% c(0,1)) == length(y))
  stopifnot(length(y) == nrow(x))
  stopifnot(is.logical(linear))
  stopifnot(type %in% c("compressed", "projected", "FRF", "sub-sampled", "full"))
  stopifnot(m <= length(x))
  stopifnot((m == nrow(x)) | (type != "full"))
  stopifnot((gamma == 0) | (type != "full"))
  stopifnot(is.double(gamma))
  
  
  # Collect inputs ---
  params <- list(
    x             = x,
    y             = y,
    linear        = linear,
    type          = type,
    m             = m,
    gamma         = gamma
  )
  
  n <- nrow(x)
  p <- ncol(x)
  exec_time = 0.0
  
  
  # Start calculations ---
  if (linear & (type == "full")) {
    x_0 <- x[y == 0, ]
    xbar_0 <- colMeans(x_0)
    y_0 <- y[y == 0]
    n_0 <- length(y_0)
    
    x_1 <- x[y == 1, ]
    xbar_1 <- colMeans(x_1)
    y_1 <- y[y == 1]
    n_1 <- length(y_1)
    
    d <- sqrt(n_0 * n_1)/n * (xbar_0 - xbar_1) # Class mean differences
    
    diffs_0 <- sweep(x_0, 2, xbar_0) # Subtract from each row
    diffs_1 <- sweep(x_1, 2, xbar_1)
    Sigma_w <- (t(diffs_0) %*% diffs_0 + t(diffs_1) %*% diffs_1)/n # Within-class covariance
    
    beta <- solve(Sigma_w, d)
  }
  
  
  # Prepare output ---
  mod <- list(
    beta      = beta,
    Sigma_w   = Sigma_w,
    xbar_0    = xbar_0,
    xbar_1    = xbar_1,
    n         = n,
    n_0       = n_0,
    n_1       = n_1,
    exec_time = exec_time,
    params    = params
  )
  class(mod) <- c("CLDA", class(mod))
  
  
  return(mod)
}