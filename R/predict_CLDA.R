#' predict.CLDA
#' Make predictions on a new set of data from a CLDA object.
#'
#' @param mod CLDA object
#' @param x New data
#'
#' @return
#' @export
predict.CLDA <- function(mod, x) {
  # Basic input checks ---
  stopifnot(is.matrix(x))
  stopifnot(ncol(x) == ncol(mod$params$x))
  
  
  # Collect inputs ---
  n_test <- nrow(x)
  
  
  # Make predictions ---
  preds <- numeric(n_test)
  
  diffs_0_mat <- sweep(x = x, MARGIN = 2, STATS=mod$xbar_0, FUN = "-")
  diffs_1_mat <- sweep(x = x, MARGIN = 2, STATS=mod$xbar_1, FUN = "-")
  
  if (mod$params$linear) { # LDA
    if (mod$params$type == "projected") {
      inv_val <- mod$inv_val
    } else {
      inv_val <- 1.0 / (t(mod$beta) %*% mod$Sigma_w %*% mod$beta)
    }
  } else { # QDA
    Sigma_w_diffs_0 <- solve(mod$Sigma_w_0, t(diffs_0_mat))
    Sigma_w_diffs_1 <- solve(mod$Sigma_w_1, t(diffs_1_mat))
  }
  
  
  for (i in 1:n_test) { # Will be slow in R, but RcppArmadillo had some compilation issues...
    if (mod$params$type %in% c("full", "compressed", "projected", "subsampled")) {
      diffs_0 <- diffs_0_mat[i, ]
      diffs_1 <- diffs_1_mat[i, ]
      
      if (mod$params$linear) { # LDA
        beta_diffs_0 <- t(mod$beta) %*% diffs_0
        beta_diffs_1 <- t(mod$beta) %*% diffs_1
        val0 <- beta_diffs_0 * inv_val * beta_diffs_0 - 2*log(mod$n_0/mod$n)
        val1 <- beta_diffs_1 * inv_val * beta_diffs_1 - 2*log(mod$n_1/mod$n)
      } else { # QDA
        val0 <- t(diffs_0) %*% Sigma_w_diffs_0[, i] + 
          determinant(mod$Sigma_w_0, logarithm=TRUE)$modulus[1] - 2 * log(mod$pi_0)
        val1 <- t(diffs_1) %*% Sigma_w_diffs_1[, i] + 
          determinant(mod$Sigma_w_1, logarithm=TRUE)$modulus[1] - 2 * log(mod$pi_1)
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