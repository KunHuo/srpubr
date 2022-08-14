#' Plot confusion matrix with ggplot2
#'
#' @param data a data frame.
#' @param outcome the outcome variable name contain in data, which can be a factor
#' or a numerical variable, but must be a binary variable.
#' @param exposure one or more exposure variable names are included in data.
#' @param positive in which  positive of outcome variable to make the comparison.
#'  By default, positive is automatically defined. If outcome is a factor variable,
#'  then positive is defined as the highest level. If outcome is a numerical
#'  variable, then positive is defined as the largest value.
#' @param combine a logical, indicating whether to use multiple exposure variables
#' for joint prediction using logistic regression.
#' @param combine.only a logical, indicating whether to show only the results of
#' joint predictions.
#' @param smooth a logical, indicating whether to smooth a ROC curve.
#' @param smooth.args further arguments for smooth, see [pROC::smooth] function.
#' @param threshold threshold, default "best", the optimal cut-off is the threshold
#' that calculating by youden index.
#' @param legend.collect A logical specifying how guides should be collected in the layout.
#' @param legend.scale.low.color legend scale low color, default 'white'.
#' @param legend.scale.high.color legend scale high color, default 'white'.
#' @param legend.scale.height legend scale height, default 0.75 cm.
#' @param legend.scale.breaks legend scale breaks, default auto. It is only
#' executed when legend.collect is equal to TRUE.
#' @param label.text.color label text color, default black.
#' @param label.text.size label text size, default 14.
#' @param label.axis labels for axis, length must be 2.
#' @param ncol the dimensions of the grid to create - if both are NULL it will
#' use the same logic as facet_wrap() to set the dimensions.
#' @param nrow 	the dimensions of the grid to create - if both are NULL it will
#' use the same logic as facet_wrap() to set the dimensions.
#' @param tag.levels a character vector defining the enumeration format to use at
#' each level.  It can also be a list containing character vectors defining
#' arbitrary tag sequences. If any element in the list is a scalar and one of
#' 'a', 'A', '(a)', '(A)', '\[a\]', '\[A\]', '(1)'  or '\[1\]', this level will be
#' expanded to the expected sequence.
#' @param language language, typically “en”, or "zh", default "en".
#' @param font.family font family, default 'serif'.
#' @param font.size font size, default 12.
#' @param ... further arguments pass to [gg_theme_sci] function.
#'
#' @return a ggplot.
#' @export
#'
#' @examples
#' # Basic
#' gg_confusion_matrix(aSAH, outcome = "outcome", exposure = "age")
#'
#' # Multiple variables
#' gg_confusion_matrix(aSAH, outcome = "outcome", exposure = "age:ndka")
#'
#' # Do not collect legends
#' gg_confusion_matrix(aSAH,
#'                     outcome = "outcome",
#'                     exposure = "age:ndka",
#'                     legend.collect = FALSE)
#'
#' # Set color
#' gg_confusion_matrix(aSAH,
#'                     outcome = "outcome",
#'                     exposure = "age:ndka",
#'                     legend.collect = TRUE,
#'                     legend.scale.low.color  = "#B4D6CB",
#'                     legend.scale.high.color = "#91BCE6")
#'
#' # Set scale
#' gg_confusion_matrix(aSAH,
#'                     outcome = "outcome",
#'                     exposure = "age:ndka",
#'                     legend.collect = TRUE,
#'                     legend.scale.low.color  = "#B4D6CB",
#'                     legend.scale.high.color = "#91BCE6",
#'                     legend.scale.breaks = seq(10, 60, 5),
#'                     legend.scale.height = 1.5)
gg_confusion_matrix <- function(data,
                                outcome,
                                exposure,
                                positive = NULL,
                                combine = FALSE,
                                combine.only = FALSE,
                                smooth = FALSE,
                                smooth.args = list(),
                                threshold = "best",
                                legend.collect = TRUE,
                                legend.scale.low.color = "white",
                                legend.scale.high.color = NULL,
                                legend.scale.height = 0.75,
                                legend.scale.breaks = NULL,
                                label.text.color = "black",
                                label.text.size = 14,
                                label.axis = NULL,
                                ncol = NULL,
                                nrow = NULL,
                                tag.levels = "A",
                                language  = NULL,
                                font.family = NULL,
                                font.size = NULL,
                                ...){

  language    <- get_global_languange(language, default = "en")
  font.family <- get_global_family(font.family, default = "serif")
  font.size   <- get_global_fontsize(font.size, default = 12)
  legend.scale.high.color  <- get_global_palette(legend.scale.high.color)

  if(is.null(legend.scale.high.color)){
    legend.scale.high.color <- pal_lancet_9(3)
  }else{
    legend.scale.high.color <- legend.scale.high.color[1]
  }

  if(language == "zh"){
    sysfonts::font_add("simsun", "simsun.ttc")
    font.family <- "simsun"
  }else{
    font.family <- font.family
  }

  exposure <- select_col_names(data, exposure)

  roclist <- .roc(data = data,
                  outcome = outcome,
                  exposure = exposure,
                  positive = positive,
                  combine = combine,
                  combine.only = combine.only,
                  smooth = smooth,
                  smooth.args = smooth.args)

  if("combine" %in% names(roclist)){
    names(roclist)[names(roclist) == "combine"] <- string_combine(language)
  }

  plotdata.list <- lapply(roclist, function(object){
    coords <- pROC::coords(object,
                           x = threshold,
                           ret = c("tp", "fp", "fn","tn"),
                           transpose = TRUE)
    tp <- coords[["tp"]]
    fp <- coords[["fp"]]
    fn <- coords[["fn"]]
    tn <- coords[["tn"]]

    plotdata <- data.frame(actual  = c(1, 1, 0, 0),
                           predict = c(1, 0, 1, 0),
                           freq    = c(tp, fn, fp, tn))

    if(is.null(label.axis)){
      label.axis <- object$levels
    }else{
      label.axis <- rev(label.axis)
    }

    plotdata$actual  <- factor(plotdata$actual,  levels = c("1", "0"), labels = rev(label.axis))
    plotdata$predict <- factor(plotdata$predict, levels = c("0", "1"), labels = label.axis)
    plotdata
  })

  if(is.null(legend.scale.breaks)){
    legend.scale.breaks <- list_rbind(plotdata.list)
    legend.scale.breaks <- pretty(legend.scale.breaks$freq, 4)
  }

  tags <- tag_levels(tag.levels, length(plotdata.list))
  tags <- sprintf("%s: %s", tags, names(plotdata.list))


  x.title <- ifelse(language == "en", "Actual classification",  "\u5b9e\u9645\u5206\u7c7b")
  y.title <- ifelse(language == "en", "Predictive classification", "\u9884\u6d4b\u5206\u7c7b")

  plots <- Map(function(plotdata, tag){
    p <- ggplot2::ggplot(plotdata, ggplot2::aes_string(y = "predict", x = "actual", fill = "freq")) +
      ggplot2::geom_tile(color = "black") +
      ggplot2::geom_text(ggplot2::aes_string(label = "freq"),
                         family = font.family,
                         size = label.text.size / 2.848,
                         color = label.text.color)

    if(legend.collect){
      p <- p +
        ggplot2::scale_fill_gradient(low    = legend.scale.low.color,
                                     high   = legend.scale.high.color,
                                     limits = c(min(legend.scale.breaks), max(legend.scale.breaks)),
                                     breaks = legend.scale.breaks)
    }else{
      p <- p +
        ggplot2::scale_fill_gradient(low  = legend.scale.low.color,
                                     high = legend.scale.high.color)
    }
    p <- p +
      ggplot2::labs(x = x.title,y = y.title)  +
      gg_theme_sci(aspect.ratio = 1, plot.margin = rep(0.4, 4), font.family = font.family, font.size = font.size, ...) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::theme(axis.line  = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     legend.key.height = ggplot2::unit(legend.scale.height, "cm")) +
      gg_delete_legend_title()

    if(length(tags) !=1L){
      p <- p + gg_tags(tag)
    }
    p
  }, plotdata.list, tags)

  if (legend.collect) {
    guides <- "collect"
  }else{
    guides <- NULL
  }

  patchwork::wrap_plots(plots, guides = guides, ncol = NULL, nrow = NULL)
}




