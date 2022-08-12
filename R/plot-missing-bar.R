#' Plot a bar for missing with ggplot2
#'
#' @param data a data frame.
#' @param percent a logical, show percentage or frequency.
#' @param show.all a logical, indicate whether to show all variables, by default,
#' only show missing variables.
#' @param decreasing a logical. Should the sort order be increasing or decreasing?
#' if is NULL, not sorted.
#' @param detail.type detail type.
#' @param digits digits for missing percent, defualt 2.
#' @param language language, typically “en”, or "zh", default "en".
#' @param bar.color color for bar.
#' @param bar.width width for bar.
#' @param font.family font family.
#' @param font.size font size.
#' @param ... further arguments pass to [gg_theme_sci] function.
#'
#' @return a ggplot.
#' @export
#'
#' @examples
#' # Basic example
#' gg_missing_bar(lung)
#'
#' # Show all variable
#' gg_missing_bar(lung, show.all = TRUE)
#'
#' # Show missing count
#' gg_missing_bar(lung, show.all = TRUE, percent = FALSE)
gg_missing_bar <- function(data,
                       percent = TRUE,
                       show.all = FALSE,
                       decreasing = TRUE,
                       detail.type = TRUE,
                       digits = 1,
                       language = c("en", "zh"),
                       bar.color = NULL,
                       bar.width = 0.65,
                       font.family = "serif",
                       font.size = 12,
                       ...){

  language <- match.arg(language)

  data.miss <- identify_missing(data,
                                show.all = show.all,
                                decreasing = decreasing,
                                detail.type = detail.type,
                                digits = digits,
                                language = language)
  if(is.null(data.miss)){
    return(invisible(NULL))
  }

  names(data.miss) <- c("variable", "type", "total", "miss.count", "label.miss.percent")
  data.miss$miss.percent <- data.miss$miss.count / data.miss$total * 100
  data.miss[["variable"]] <- factor(data.miss[["variable"]], levels = data.miss[["variable"]])
  data.miss$label.miss.count <- as.character(data.miss$miss.count)

  data.miss$label.miss.percent[data.miss$miss.count == 0] <- ""
  data.miss$label.miss.count  [data.miss$miss.count == 0] <- ""

  if(percent){
    y.string <- "miss.percent"
    y.breaks <- pretty(c(0, max(data.miss[["miss.percent"]])), 5)
    y.title  <- paste(string_missing_percent(language), "(%)", sep = " ")
    label    <- "label.miss.percent"
  }else{
    y.string <- "miss.count"
    y.breaks <- pretty(c(0, max(data.miss[["miss.count"]])), 5)
    y.title  <- string_missing_count(language)
    label    <- "label.miss.count"
  }

  if(language == "zh"){
    sysfonts::font_add("simsun", "simsun.ttc")
    font.family <- "simsun"
  }else{
    font.family <- font.family
  }

  p <- ggplot2::ggplot(data = data.miss) +
    ggplot2::geom_col(ggplot2::aes_string(x  = "variable", y = y.string, fill  = "type"), width = bar.width) +
    ggplot2::geom_text(ggplot2::aes_string(x = "variable", y = y.string, label = label),
                       vjust = -0.75, family = font.family, size = font.size / 2.848) +
    gg_theme_sci(legend.key.size = 0.8, font.family = font.family, font.size = font.size, ...) +
    ggplot2::theme( plot.margin = ggplot2::unit(c(0.8, 0.4, 0.4, 0.4), "cm")) +
    gg_delete_x_title() +
    gg_rotate_x_text(45) +
    gg_ylab(y.title) +
    gg_legend_position("top") +
    gg_delete_legend_title() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_y_continuous(breaks = y.breaks, limits = c(0, max(y.breaks)), expand = c(0, 0))


  if(!is.null(bar.color)){
    p <- p +
      ggplot2::scale_fill_manual(values = bar.color)
  }

  if(length(unique(data.miss$type)) == 1L){
    p <- p +
      gg_delete_legend()
  }

  p
}
