#' Create a Rademacher matrix
#'
#' @param nrow Number of rows.
#' @param ncol Number of columns.
#' @param s Sparsity parameter in (0, 1). Probability of an entry being zero is 1-s.
#'
#' @return
#' @export
#'
#' @examples
Rademacher <- function(nrow, ncol, s) {
  rmultinom(n = nrow*ncol, size = 0)
  
  
  mat <- matrix(NA, nrow = nrow, ncol = ncol)
  
  return(mat)
}