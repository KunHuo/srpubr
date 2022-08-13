#' Plot missing pattern with ggplot2
#'
#' @param data a data frame.
#' @param percent a logical, show percentage or frequency.
#' @param show.all a logical, indicate whether to show all variables, by default,
#' only show missing variables.
#' @param decreasing a logical. Should the sort order be increasing or decreasing?
#' if is NULL, not sorted.
#' @param add.var.miss  a logical, indicate whether to show missing count of each variable.
#' @param digits digits for missing percent, defualt 2.
#' @param language language, typically “en”, or "zh", default "en".
#' @param font.family font family.
#' @param font.size font size.
#' @param color fill color, must have two values, the first for missing, the second for observed.
#' @param ... further arguments pass to [gg_theme_sci] function.
#'
#' @return a ggplot.
#' @export
#'
#' @examples
#' # Basic example
#' gg_missing_pattern(lung)
#'
#' # show all variables
#' gg_missing_pattern(lung, show.all = TRUE)
#'
#' # Specify the color
#' gg_missing_pattern(lung, show.all = TRUE, color = c("#E85827", "#81D8D0"))
gg_missing_pattern <- function(data,
                               percent = FALSE,
                               show.all = FALSE,
                               decreasing = TRUE,
                               add.var.miss = TRUE,
                               digits = 1,
                               language  = c("en", "zh"),
                               font.family = "serif",
                               font.size = 12,
                               color = NULL,
                               ...){

  language <- match.arg(language)

  if(language == "zh"){
    sysfonts::font_add("simsun", "simsun.ttc")
    font.family <- "simsun"
  }else{
    font.family <- font.family
  }

  plotdata <- identify_missing_pattern(data, show.all = show.all, decreasing = decreasing)

  if(is.null(plotdata)){
    return(invisible(NULL))
  }

  plotdata <- plotdata[, -ncol(plotdata), drop = FALSE]

  miss.count.variable <- unlist(plotdata[nrow(plotdata), ])[-1]

  plotdata <- plotdata[-nrow(plotdata), , drop = FALSE]
  y.label <- plotdata[[1]]

  if(percent){
    y.label <- sprintf("%s%%", format_digits(as.numeric(y.label) / nrow(data) * 100, digits))
    miss.count.variable <- sprintf("%s%%", format_digits(as.numeric(miss.count.variable) / nrow(data) * 100, digits))

    y.title <- ifelse(language == "en", "Percent of missing pattern", "\u7f3a\u5931\u6a21\u5f0f\u767e\u5206\u6bd4")
  }else{
    y.title <- ifelse(language == "en", "Number of missing pattern", "\u7f3a\u5931\u6a21\u5f0f\u4f8b\u6570")
  }

  plotdata <- plotdata[, -1, drop = FALSE]
  varnames <- names(plotdata)

  if(add.var.miss){
    x.label <- sprintf("%s (%s)", varnames, miss.count.variable)
  }else{
    x.label <- varnames
  }

  plotdata <- reshape_long(plotdata, add.id.col = TRUE)

  label.missing  <- ifelse(language == "en", "Missing",  "\u7f3a\u5931")
  label.observed <- ifelse(language == "en", "Observed", "\u975e\u7f3a\u5931")

  plotdata$.id    <- factor(plotdata$.id)
  plotdata$.name  <- factor(plotdata$.name,  levels = varnames)
  plotdata$.value <- factor(plotdata$.value, levels = c(0, 1), labels = c(label.missing, label.observed))

  p <- ggplot2::ggplot(plotdata) +
    ggplot2::geom_tile(ggplot2::aes_string(x = ".name", y = ".id", fill = ".value"), color = "black", size = 0.1) +
    gg_theme_sci(legend.key.size = 0.7, font.family = font.family, font.size = font.size) +
    ggplot2::theme(axis.line   = ggplot2::element_blank(),
                   axis.ticks  = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(0.6, 0.4, 0.4, 0.4), "cm"), ...) +
    gg_rotate_x_text() +
    gg_delete_x_title() +
    gg_ylab(y.title) +
    gg_delete_legend_title() +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_y_discrete(label = y.label) +
    ggplot2::scale_x_discrete(label = x.label) +
    gg_legend_position("top")

  if(!is.null(color)){
    stopifnot(length(color) == 2L)
    p <- p +
      ggplot2::scale_fill_manual(values = color)
  }

  p
}
