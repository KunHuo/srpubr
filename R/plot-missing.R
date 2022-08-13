#' Plots for missing with ggplot2
#'
#' @param data a data frame.
#' @param percent.bar a logical for bar, show percentage or frequency.
#' @param percent.miss a logical for missing plot, show percentage or frequency.
#' @param show.all a logical, indicate whether to show all variables, by default,
#' only show missing variables.
#' @param show.miss.text a logical, indicate whether to show missing text for bar, default TRUE.
#' @param decreasing a logical. Should the sort order be increasing or decreasing?
#' if is NULL, not sorted.
#' @param detail.type detail type.
#' @param add.var.miss a logical, indicate whether to show missing count of each variable.
#' @param digits digits for missing percent, default 1.
#' @param language language, typically “en”, or "zh", default "en".
#' @param bar.color color for bar plot.
#' @param bar.width width for bar.
#' @param miss.color color for missing plot.
#' @param font.family font family, default 'serif'.
#' @param font.size font size, default 12.
#' @param tag.levels a character vector defining the enumeration format to use at
#' each level.  It can also be a list containing character vectors defining
#' arbitrary tag sequences. If any element in the list is a scalar and one of
#' 'a', 'A', '(a)', '(A)', '\[a\]', '\[A\]', '(1)'  or '\[1\]', this level will be
#' expanded to the expected sequence.
#' @param plot.width the relative widths and heights of each column and row in
#' the grid. Will get repeated to match the dimensions of the grid.
#' @param ... further arguments pass to [gg_theme_sci] function.
#'
#' @return a ggplot.
#' @export
#'
#' @examples
#' # Basic
#' gg_missing(lung)
#'
#' # set tag
#' gg_missing(lung, tag.levels = c("A: Missing bar", "B: Missing pattern"))
gg_missing <- function(data,
                       percent.bar = TRUE,
                       percent.miss = FALSE,
                       show.all = FALSE,
                       show.miss.text = TRUE,
                       decreasing = TRUE,
                       detail.type = TRUE,
                       add.var.miss = TRUE,
                       digits = 1,
                       language = NULL,
                       bar.color = NULL,
                       bar.width = 0.65,
                       miss.color = NULL,
                       font.family = NULL,
                       font.size = NULL,
                       tag.levels = "A",
                       plot.width  = c(0.5, 0.5),
                       ...){

  language    <- get_global_languange(language, default = "en")
  font.family <- get_global_family(font.family, default = "serif")
  font.size   <- get_global_fontsize(font.size, default = 12)

  bar.color   <- get_global_palette(bar.color)
  miss.color  <- get_global_palette(miss.color)

  A <- gg_missing_bar(data = data,
                      percent = percent.bar,
                      show.all = show.all,
                      show.miss.text = show.miss.text,
                      decreasing = decreasing,
                      detail.type = detail.type,
                      digits = digits,
                      language = language,
                      bar.color = bar.color,
                      bar.width = bar.width,
                      font.family = font.family,
                      font.size = font.size,
                      ...)

  if(is.null(A)){
    return(invisible(NULL))
  }

  B <- gg_missing_pattern(data = data,
                          percent = percent.miss,
                          show.all = show.all,
                          decreasing = decreasing,
                          add.var.miss = add.var.miss,
                          digits = digits,
                          language  = language,
                          font.family = font.family,
                          font.size = font.size,
                          color = miss.color,
                          ...)

  tag.levels <- tag_levels(tag.levels, n = 2)

  A <- A + gg_tags(tag.levels[1])
  B <- B + gg_tags(tag.levels[2])

  patchwork::wrap_plots(A, B, widths = plot.width)
}
