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
  mat <- Matrix::Matrix(sample(x=c(1, -1, 0), size=nrow*ncol, replace=TRUE, prob=c(s/2, s/2, 1-s)), 
                        nrow = nrow, ncol = ncol, sparse = TRUE)
  
  return(mat)
}