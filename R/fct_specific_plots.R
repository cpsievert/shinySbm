#' specific_plots
#'
#' @description A fct that plot a the ILC curve and the position of two version
#'  of the same SBMmodel with or not differents K values (nb of groups)
#' @return no value
#'
#' @noRd
ILC_plot <- function(selected_sbm,comparison_sbm = selected_sbm){
  my_plot <- ggplot2::ggplot(selected_sbm$storedModels) +
    ggplot2::aes(x = nbBlocks, y = ICL, linetype = "ICL") +
    ggplot2::geom_line() +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(x = nbBlocks, y = loglik, linetype = "Log Likelihood")) +
    ggplot2::geom_point(ggplot2::aes(x = sum(selected_sbm$nbBlocks), y = selected_sbm$ICL, colour = "Selected Block Nb"), size = 4) +
    ggplot2::geom_point(ggplot2::aes(x = sum(comparison_sbm$nbBlocks), y = comparison_sbm$ICL, colour = "Best Block Nb"), size = 4, shape = 10) +
    ggplot2::labs(linetype = "Curves", colour = "Number of Blocks") +
    ggplot2::theme(
      legend.position = c(.40, .05),
      legend.justification = c("left", "bottom"),
      legend.box.just = "left",
      legend.margin = ggplot2::margin(6, 6, 6, 6)
    )
  plot(my_plot)
}


