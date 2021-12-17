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
  
  
  # Collect inputs ---
  n_test <- nrow(x)
  
  
  # Make predictions ---
  preds <- numeric(n_test)
  
  for (i in 1:n_test) { # Will be slow in R, but RcppArmadillo had some compilation issues...
    if (mod$params$linear) {
      if (mod$params$type %in% c("full", "compressed", "projected", "subsampled")) {
        obs <- x[i, ]
        diffs_0 <- obs - mod$xbar_0
        diffs_1 <- obs - mod$xbar_1
        beta_diffs_0 <- t(mod$beta) %*% diffs_0
        beta_diffs_1 <- t(mod$beta) %*% diffs_1
        if (mod$params$type == "projected") {
          inv_val <- mod$inv_val
        } else {
          inv_val <- 1.0 / (t(mod$beta) %*% mod$Sigma_w %*% mod$beta)
        }
        
        val0 <- beta_diffs_0 * inv_val * beta_diffs_0 - 2*log(mod$n_0/mod$n)
        val1 <- beta_diffs_1 * inv_val * beta_diffs_1 - 2*log(mod$n_1/mod$n)
      }
    }
    
    
    if (val0 < val1) {
      preds[i] <- 0
    } else {
      preds[i] <- 1
    }
  }
  
  return(preds)
}