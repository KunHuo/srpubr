gg_missing_pattern <- function(data, show.all = FALSE){

  plotdata <- identify_missing_pattern(data)

  plotdata <- plotdata[, -ncol(plotdata), drop = FALSE]
  plotdata <- plotdata[-nrow(plotdata), , drop = FALSE]

  pattern.count <- plotdata[[1]]

  plotdata <- plotdata[, -1, drop = FALSE]
  varnames <- names(plotdata)
  plotdata <- reshape_long(plotdata, add.id.col = TRUE)

  plotdata$.id    <- factor(plotdata$.id)
  plotdata$.name  <- factor(plotdata$.name,  levels = varnames)
  plotdata$.value <- factor(plotdata$.value, levels = c(0, 1), labels = c("Missing", "No missing"))

  ggplot2::ggplot(plotdata) +
    ggplot2::geom_tile(ggplot2::aes_string(x = ".name", y = ".id", fill = ".value"), color = "black", size = 0.1) +
    gg_theme_sci(legend.key.size = 0.8) +
    ggplot2::theme(axis.line  = ggplot2::element_blank(),
                   axis.ticks  = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(0.6, 0.4, 0.4, 0.4), "cm")) +
    gg_rotate_x_text() +
    gg_delete_x_title() +
    gg_delete_y_title() +
    gg_delete_legend_title() +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_y_discrete(label = pattern.count) +
    gg_legend_position("top")
}
