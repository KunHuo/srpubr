gg_roc <- function(data, outcome, exposures, smooth = TRUE, smooth.args = list(),  ci.method = c("delong", "bootstrap")){

  # outcome   <- names(tidyselect::eval_select(rlang::enquo(outcome),   data = data))
  # exposures <- names(tidyselect::eval_select(rlang::enquo(exposures), data = data))

  ci.method <- match.arg(ci.method)

  g.list <- lapply(exposures, function(x){
    res <- suppressMessages(pROC::roc(response = data[[outcome]], predictor = data[[x]]))
    if(smooth){
      res <- do_call(pROC::smooth, roc = res, smooth.args)
    }
    res
  })

  names(g.list) <- exposures

  suppressMessages(pROC::ggroc(g.list, legacy.axes = TRUE) +
                     ggplot2::coord_cartesian(expand = FALSE) +
                     ggplot2::geom_abline(intercept = 0, color = "darkgrey", linetype = "dashed", size = 0.25) +
                     ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
                     ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1))) +
    theme_sci() +
    gg_legend_title(NULL) +
    gg_legend_position(c(1, 0))
}


sysfonts::font_add("simsun", "simsun.ttc")
