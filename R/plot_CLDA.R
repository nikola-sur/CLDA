#' Plot results from a CLDA simulation study
#'
#' @param data Simulation data
#'
#' @return
#' @export
plot.CLDA <- function(data) {
  # LDA misclassification error rates ---
  p <- ggplot2::ggplot(subset(data, method %in% c("L_compressed", "L_projected", "L_FRF", "L_subsampled")), 
                       ggplot2::aes(x = m, y = error, col = method, group = interaction(method, m))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Reduced Sample Amount m") + 
    ggplot2::ylab("Misclassification Error Rates")
  plot(p)
  
  
  # LDA execution times ---
  
  # QDA misclassification error rates ---
}