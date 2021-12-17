#' CLDA
#' Various LDA/QDA functions and compression schemes.
#'
#' @param x Numeric matrix of features.
#' @param y Binary-valued vector of class labels.
#' @param linear Boolean: Whether to use LDA (true) or QDA (false).
#' @param type Type of compression. One of "compressed", "projected", "FRF", "sub-sampled", or "full".
#' For QDA, "projected" and "FRF" are not implemented.
#' @param m Reduced number of samples.
#' @param s Sparsity parameter (see the paper). Defaults to 0.01.
#' @param gamma Tuning parameter for numerical stability (see the paper). Defaults to 1e-4.
#'
#' @return
#' @export
CLDA <- function(x, y, linear, type, m, s = 0.01, gamma = 1e-4) {
  # Basic input checks ---
  stopifnot(is.matrix(x))
  stopifnot(sum(y %in% c(0,1)) == length(y))
  stopifnot(length(y) == nrow(x))
  stopifnot(is.logical(linear))
  stopifnot(type %in% c("compressed", "projected", "FRF", "sub-sampled", "full"))
  stopifnot(m <= length(x))
  stopifnot((m == nrow(x)) | (type != "full"))
  stopifnot(is.double(gamma))
  stopifnot(gamma >= 0)
  
  
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
  
  
  # Prepare outputs ---
  
  
  
  # Start calculations ---
  if (linear) {
    x_0 <- x[y == 0, ]
    xbar_0 <- colMeans(x_0)
    y_0 <- y[y == 0]
    n_0 <- length(y_0)
    
    x_1 <- x[y == 1, ]
    xbar_1 <- colMeans(x_1)
    y_1 <- y[y == 1]
    n_1 <- length(y_1)
    
    d <- sqrt(n_0 * n_1)/n * (xbar_0 - xbar_1) # Class mean differences
    diffs_0 <- sweep(x = x_0, MARGIN = 2, STATS = xbar_0, FUN = "-") # Subtract from each row
    diffs_1 <- sweep(x = x_1, MARGIN = 2, STATS = xbar_1, FUN = "-")
    
    
    if (type == "full") {
      Sigma_w <- (t(diffs_0) %*% diffs_0 + t(diffs_1) %*% diffs_1)/n + diag(rep(gamma, p)) # Within-class covariance
      beta <- solve(Sigma_w, d)
    } else if (type == "compressed") {
      m_0 <- floor(m/n * n_0)
      m_1 <- m - m_0
      Q_0 <- CLDA::Rademacher(nrow = m_0, ncol = n_0, s=s)
      Q_1 <- CLDA::Rademacher(nrow = m_1, ncol = n_1, s=s)
      
      x_c_0 <- sweep(x = 1/sqrt(n_0 * s) * Q_0 %*% diffs_0, MARGIN = 2, STATS = xbar_0, FUN = "+") # Compressed data in group 0
      x_c_1 <- sweep(x = 1/sqrt(n_1 * s) * Q_1 %*% diffs_1, MARGIN = 2, STATS = xbar_1, FUN = "+")
      
      diffs_c_0 <- as.matrix(sweep(x = x_c_0, MARGIN = 2, STATS = xbar_0, FUN = "-"))
      diffs_c_1 <- as.matrix(sweep(x = x_c_1, MARGIN = 2, STATS = xbar_1, FUN = "-"))
      
      Sigma_w <- (t(diffs_c_0) %*% diffs_c_0 + t(diffs_c_1) %*% diffs_c_1)/m + diag(rep(gamma, p)) # Within-class covariance
      beta <- solve(Sigma_w, d)
    }
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