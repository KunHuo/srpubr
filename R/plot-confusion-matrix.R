gg_confusion_matrix <- function(data,
                                outcome,
                                exposure,
                                positive = NULL,
                                combine = FALSE,
                                combine.only = FALSE,
                                smooth = FALSE,
                                smooth.args = list(),
                                threshold = "best",
                                language  = NULL,
                                font.family = NULL,
                                font.size = NULL,
                                color = NULL,
                                ...){

  language    <- get_global_languange(language, default = "en")
  font.family <- get_global_family(font.family, default = "serif")
  font.size   <- get_global_fontsize(font.size, default = 12)
  color       <- get_global_palette(color)

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


  plots <- lapply(roclist, function(object){
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

    plotdata$actual  <- factor(plotdata$actual,  levels = c("1", "0"), labels = rev(object$levels))
    plotdata$predict <- factor(plotdata$predict, levels = c("0", "1"), labels = object$levels)

    ggplot2::ggplot(plotdata, ggplot2::aes_string(y = "predict", x = "actual", fill = "freq")) +
      ggplot2::geom_tile(color = "black") +
      ggplot2::geom_text(ggplot2::aes_string(label="freq"), family = "serif", size = (font.size + 2) / 2.848) +
      ggplot2::scale_fill_gradient(low="white", high = pal_lancet_9(3), limits = c(10, 60), breaks = seq(10, 60, 10)) +
      ggplot2::labs(x = "Reference",y = "Prediction")  +
      gg_theme_sci(panel.border = TRUE, aspect.ratio = 1, plot.margin = rep(0.4, 4)) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::theme(axis.line = ggplot2::element_blank()) +
      theme(legend.key.height = unit(0.8, "cm"))
  })

  patchwork::wrap_plots(plots, guides = "collect")
}




