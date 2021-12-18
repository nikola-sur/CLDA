#' Plot results from a CLDA simulation study
#'
#' @param data Simulation data
#'
#' @return
#' @export
plot.CLDA <- function(data) {
  # LDA misclassification error rates ---
  p1 <- ggplot2::ggplot(subset(data, method %in% c("L_compressed", "L_projected", "L_FRF", "L_subsampled")), 
                        ggplot2::aes(x = m, y = error, col = method, group = interaction(method, m))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Reduced Sample Amount m") + 
    ggplot2::ylab("Misclassification Error Rates")
  plot(p1)
  
  
  # LDA execution times ---
  p2 <- ggplot2::ggplot(subset(data, method %in% c("L_compressed", "L_full")), 
                        ggplot2::aes(x = m, y = error, col = method, group = interaction(method, m))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Reduced Sample Amount m") + 
    ggplot2::ylab("Time (s)")
  plot(p2)
  
  
  # QDA misclassification error rates ---
  p3 <- ggplot2::ggplot(subset(data, method %in% c("Q_compressed", "Q_subsampled")), 
                        ggplot2::aes(x = m, y = error, col = method, group = interaction(method, m))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Reduced Sample Amount m") + 
    ggplot2::ylab("Misclassification Error Rates")
  plot(p3)
}