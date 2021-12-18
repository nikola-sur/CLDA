#' CLDA
#' Various LDA/QDA functions and compression schemes.
#'
#' @param x Numeric matrix of features.
#' @param y Binary-valued vector of class labels.
#' @param linear Boolean: Whether to use LDA (true) or QDA (false).
#' @param type Type of compression. One of "compressed", "projected", "FRF", "subsampled", or "full".
#' For QDA, "projected" and "FRF" are not implemented.
#' @param m Reduced number of samples.
#' @param s Sparsity parameter (see the paper). Defaults to 0.01.
#' @param gamma Tuning parameter for numerical stability (see the paper). Defaults to 1e-4.
#'
#' @return
#' @export
CLDA <- function(x, y, linear, type, m = nrow(x), s = 0.01, gamma = 1e-4) {
  # Basic input checks ---
  stopifnot(is.matrix(x))
  stopifnot(sum(y %in% c(0,1)) == length(y))
  stopifnot(length(y) == nrow(x))
  stopifnot(is.logical(linear))
  stopifnot(type %in% c("compressed", "projected", "FRF", "subsampled", "full"))
  stopifnot((type %in% c("compressed", "subsampled", "full")) | linear)
  stopifnot(floor(m) == m) # Check if 'm' is an integer
  stopifnot((m <= length(x)) & (m > 0))
  stopifnot((m == nrow(x)) | (type != "full"))
  stopifnot(is.double(gamma))
  stopifnot(gamma >= 0)
  
  
  
  # Update data if type == 'subsampled' ---
  if (type == "subsampled") {
    inds <- sample(1:nrow(x), size=m, replace=FALSE)
    x <- x[inds, ]
    y <- y[inds]
    m <- nrow(x)
  }
  
  
  
  # Collect inputs ---
  params <- list(
    x             = x,
    y             = y,
    linear        = linear,
    type          = type,
    m             = m,
    s             = s,
    gamma         = gamma
  )
  
  
  
  # Prepare outputs ---
  beta            <- NULL
  Sigma_w         <- NULL
  Sigma_w_0       <- NULL
  Sigma_w_1       <- NULL
  xbar            <- NULL
  xbar_0          <- NULL
  xbar_1          <- NULL
  x_c_0           <- NULL
  x_c_1           <- NULL
  x_0             <- NULL
  x_1             <- NULL
  n               <- NULL
  n_0             <- NULL
  n_1             <- NULL
  pi_0            <- NULL
  pi_1            <- NULL
  p               <- NULL
  inv_val         <- NULL
  exec_time       <- -1.0
  
  
  
  # Start calculations ---
  n <- nrow(x)
  p <- ncol(x)
  
  t1 <- system.time({
    x_0 <- x[y == 0, ]
    xbar_0 <- colMeans(x_0)
    y_0 <- y[y == 0]
    n_0 <- length(y_0)
    pi_0 <- n_0/n
    
    x_1 <- x[y == 1, ]
    xbar_1 <- colMeans(x_1)
    y_1 <- y[y == 1]
    n_1 <- length(y_1)
    pi_1 <- n_1/n
    
    d <- sqrt(n_0 * n_1)/n * (xbar_0 - xbar_1) # Class mean differences
    diffs_0 <- sweep(x = x_0, MARGIN = 2, STATS = xbar_0, FUN = "-") # Subtract from each row
    diffs_1 <- sweep(x = x_1, MARGIN = 2, STATS = xbar_1, FUN = "-")
    
    xbar <- colMeans(x)
    diffs <- sweep(x = x, MARGIN = 2, STATS = xbar, FUN = "-")
  })[3] # Elapsed time
  
  
  if (type %in% c("full", "subsampled")) {
    t2 <- system.time({
      Sigma_w_0_raw <- (t(diffs_0) %*% diffs_0)/n_0
      Sigma_w_0 <- Sigma_w_0_raw + diag(rep(gamma, p))
      
      Sigma_w_1_raw <- (t(diffs_1) %*% diffs_1)/n_1
      Sigma_w_1 <- Sigma_w_1_raw + diag(rep(gamma, p))
      
      if (linear) {
        Sigma_w <- (n_0 * Sigma_w_0_raw + n_1 * Sigma_w_1_raw)/n + diag(rep(gamma, p)) # Within-class covariance
        beta <- solve(Sigma_w, d)
      }
    })[3] 
    if ((type == "full") & linear) exec_time <- t1 + t2
  } else if (type %in% c("compressed", "projected", "FRF")) {
    t2 <- system.time({
      m_0 <- floor(m/n * n_0)
      m_1 <- m - m_0
      if (type %in% c("compressed", "projected")) {
        Q_0 <- CLDA::Rademacher(nrow = m_0, ncol = n_0, s=s)
        Q_1 <- CLDA::Rademacher(nrow = m_1, ncol = n_1, s=s)
        
        x_c_0 <- sweep(x = 1/sqrt(n_0 * s) * Q_0 %*% diffs_0, MARGIN = 2, STATS = xbar_0, FUN = "+") # Compressed data in group 0
        x_c_1 <- sweep(x = 1/sqrt(n_1 * s) * Q_1 %*% diffs_1, MARGIN = 2, STATS = xbar_1, FUN = "+")
        
        diffs_c_0 <- as.matrix(sweep(x = x_c_0, MARGIN = 2, STATS = xbar_0, FUN = "-"))
        diffs_c_1 <- as.matrix(sweep(x = x_c_1, MARGIN = 2, STATS = xbar_1, FUN = "-"))
        
        Sigma_w_0_raw <- (t(diffs_c_0) %*% diffs_c_0)/m_0
        Sigma_w_0 <- Sigma_w_0_raw + diag(rep(gamma, p))
        
        Sigma_w_1_raw <- (t(diffs_c_1) %*% diffs_c_1)/m_1
        Sigma_w_1 <- Sigma_w_1_raw + diag(rep(gamma, p))
        
        if (linear) {
          Sigma_w <- (m_0 * Sigma_w_0_raw + m_1 * Sigma_w_1_raw)/m + diag(rep(gamma, p)) # Within-class covariance
          beta <- solve(Sigma_w, d)
        }
      } else if (type == "FRF") {
        Q <- CLDA::Rademacher(nrow = m, ncol = n, s=s)
        x_c <- sweep(x = 1/sqrt(n * s) * Q %*% diffs, MARGIN = 2, STATS = xbar, FUN = "+")
        diffs_c <- as.matrix(sweep(x = x_c, MARGIN = 2, STATS = xbar, FUN = "-"))
        Sigma_w <- (t(diffs_c) %*% diffs_c)/m + diag(rep(gamma, p))
        beta <- solve(Sigma_w, d)
      }
      
    })[3]
    
    if (type == "compressed") {
      exec_time <- t1 + t2
    }
    
    if (type %in% c("projected", "FRF")) {
      Sigma_w <- NULL
      inv_val <- 1/((sum((diffs_0 %*% beta)^2) + sum((diffs_1 %*% beta)^2))/n)
    }
  }
  

  
  # Prepare output ---
  mod <- list(
    beta            = beta,
    Sigma_w         = Sigma_w,
    Sigma_w_0       = Sigma_w_0,
    Sigma_w_1       = Sigma_w_1,
    xbar            = xbar,
    xbar_0          = xbar_0,
    xbar_1          = xbar_1,
    x_c_0           = x_c_0,
    x_c_1           = x_c_1,
    x_0             = x_0,
    x_1             = x_1,
    n               = n,
    n_0             = n_0,
    n_1             = n_1,
    pi_0            = pi_0,
    pi_1            = pi_1,
    p               = p,
    inv_val         = inv_val,
    exec_time       = exec_time,
    params          = params
  )
  class(mod) <- c("CLDA", class(mod))
  
  
  return(mod)
}