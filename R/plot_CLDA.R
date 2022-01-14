#' Plot results from a CLDA simulation study
#'
#' @param data Simulation data.
#' @param light_theme Boolean: Whether to use the light theme or the default theme.
#' @param save_plots Boolean: Whether to save the plots to disk as a PNG file.
#'
#' @return
#' @export
plot.CLDA <- function(data, light_theme = TRUE, save_plots = FALSE) {
  # Transform data for plotting ---
  data$method <- factor(data$method, levels = c("L_compressed", "L_projected", "L_FRF", "L_subsampled", "L_full",
                                                "Q_compressed", "Q_subsampled", "Q_full"),
                        labels = c("Compressed LDA", "Projected LDA", "Fast Random Fisher", "Sub-sampled LDA", "Full LDA",
                                   "Compressed QDA", "Sub-sampled QDA", "Full QDA"))
  m_old <- data$m
  m_vec <- unique(data$m)[order(unique(data$m))]
  data$m <- match(data$m, m_vec)
  
  
  # LDA misclassification error rates ---
  full_LDA_error <- mean(data$error[data$method == "Full LDA"])
  p1 <- ggplot2::ggplot(subset(data, method %in% c("Compressed LDA", "Projected LDA", "Fast Random Fisher", "Sub-sampled LDA")), 
                        ggplot2::aes(x = m, y = error, col = method, group = interaction(method, m))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Reduced Sample Amount m") + 
    ggplot2::ylab("Misclassification Error Rates") +
    ggplot2::scale_x_continuous(breaks = unique(data$m), minor_breaks = NULL,
                                labels = m_vec) +
    ggplot2::labs(color = "Method") +
    ggplot2::geom_hline(yintercept = full_LDA_error, linetype = "dashed", size = 1.0) +
    ggplot2::ylim(0, NA)
  if (light_theme) p1 <- p1 + ggplot2::theme_light()
  if (save_plots) ggplot2::ggsave(filename = "./LDA_misclass.png", plot = p1, device = "png",
                                  path = "./", width = 6.5, height = 4.25, units = "in", dpi = 1200)
  plot(p1)
  
  
  # QDA misclassification error rates ---
  full_QDA_error <- mean(data$error[data$method == "Full QDA"])
  p3 <- ggplot2::ggplot(subset(data, method %in% c("Compressed QDA", "Sub-sampled QDA")), 
                        ggplot2::aes(x = m, y = error, col = method, group = interaction(method, m))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Reduced Sample Amount m") + 
    ggplot2::ylab("Misclassification Error Rates") +
    ggplot2::scale_x_continuous(breaks = unique(data$m), minor_breaks = NULL,
                                labels = m_vec) +
    ggplot2::labs(color = "Method") +
    ggplot2::geom_hline(yintercept = full_QDA_error, linetype = "dashed", size = 1.0) +
    ggplot2::ylim(0, NA)
  if (light_theme) p3 <- p3 + ggplot2::theme_light()
  if (save_plots) ggplot2::ggsave(filename = "./QDA_misclass.png", plot = p3, device = "png",
                                  path = "./", width = 6.5, height = 4.25, units = "in", dpi = 1200)
  plot(p3)
  
  
  # LDA execution times ---
  data$m <- m_old
  data$m[data$method == "Full LDA"] <- 10^6
  m_vec <- unique(data$m)[order(unique(data$m))]
  data$m <- match(data$m, m_vec)
  p2 <- ggplot2::ggplot(subset(data, method %in% c("Compressed LDA", "Full LDA")), 
                        ggplot2::aes(x = m, y = exec_time, col = method, group = interaction(method, m))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Reduced Sample Amount m") + 
    ggplot2::ylab("Time (s)") +
    ggplot2::scale_x_continuous(breaks = unique(data$m), minor_breaks = NULL,
                                labels = c("250", "Full", "500", "1000", "2000")) +
    ggplot2::labs(color = "Method") +
    ggplot2::ylim(0, NA)
  if (light_theme) p2 <- p2 + ggplot2::theme_light()
  if (save_plots) ggplot2::ggsave(filename = "./LDA_time.png", plot = p2, device = "png",
                                  path = "./", width = 6.5, height = 4.25, units = "in", dpi = 1200)
  plot(p2)
}