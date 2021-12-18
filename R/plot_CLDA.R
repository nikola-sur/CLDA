#' Plot results from a CLDA simulation study
#'
#' @param data Simulation data.
#' @param light_theme Boolean: Whether to use the light theme or the default theme.
#'
#' @return
#' @export
plot.CLDA <- function(data, light_theme = TRUE) {
  # Transform data for plotting ---
  data$method <- factor(data$method, levels = c("L_compressed", "L_projected", "L_FRF", "L_subsampled", "L_full",
                                                "Q_compressed", "Q_subsampled", "Q_full"))
  m_vec <- unique(data$m)[order(unique(data$m))]
  data$m <- match(data$m, m_vec)
  
  
  # LDA misclassification error rates ---
  p1 <- ggplot2::ggplot(subset(data, method %in% c("L_compressed", "L_projected", "L_FRF", "L_subsampled")), 
                        ggplot2::aes(x = m, y = error, col = method, group = interaction(method, m))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Reduced Sample Amount m") + 
    ggplot2::ylab("Misclassification Error Rates") +
    ggplot2::scale_x_continuous(breaks = unique(data$m), minor_breaks = NULL,
                                labels = m_vec) +
    ggplot2::labs(color = "Method")
  if (light_theme) p1 <- p1 + ggplot2::theme_light()
  plot(p1)
  
  
  # LDA execution times ---
  p2 <- ggplot2::ggplot(subset(data, method %in% c("L_compressed", "L_full")), 
                        ggplot2::aes(x = m, y = exec_time, col = method, group = interaction(method, m))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Reduced Sample Amount m") + 
    ggplot2::ylab("Time (s)") +
    ggplot2::scale_x_continuous(breaks = unique(data$m), minor_breaks = NULL,
                                labels = m_vec) +
    ggplot2::labs(color = "Method")
  if (light_theme) p2 <- p2 + ggplot2::theme_light()
  plot(p2)
  
  
  # QDA misclassification error rates ---
  p3 <- ggplot2::ggplot(subset(data, method %in% c("Q_compressed", "Q_subsampled")), 
                        ggplot2::aes(x = m, y = error, col = method, group = interaction(method, m))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Reduced Sample Amount m") + 
    ggplot2::ylab("Misclassification Error Rates") +
    ggplot2::scale_x_continuous(breaks = unique(data$m), minor_breaks = NULL,
                                labels = m_vec) +
    ggplot2::labs(color = "Method")
  if (light_theme) p3 <- p3 + ggplot2::theme_light()
  plot(p3)
}