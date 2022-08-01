gg_roc <- function(data,
                   outcome,
                   exposure,
                   positive = NULL,
                   smooth = FALSE,
                   auc = TRUE,
                   auc.ci = TRUE,
                   auc.digits = 3,
                   smooth.args = list(),
                   ci.method = c("delong", "bootstrap")){

  ci.method <- match.arg(ci.method)

  if(!is.factor(data[[outcome]])){
    data[[outcome]] <- factor(data[[outcome]])
  }

  if(!is.null(positive)){
    positive <- as.character(positive)
    negative <- setdiff(unique(data[[outcome]]), positive)
    data[[outcome]] <-  factor(data[[outcome]],  levels = c(negative, positive))
  }

  g.list <- lapply(exposure, function(x){
    res <- pROC::roc(response  = data[[outcome]],
                     predictor = data[[x]],
                     direction = "<",
                     levels    = levels(data[[outcome]]))
    if(smooth){
      res <- do_call(pROC::smooth, roc = res, smooth.args)
    }
    res
  })

  legends <- exposure

  if(auc){
    auc.data <- sapply(g.list, function(x){
      format_digits(pROC::auc(x), auc.digits)
    })

    legends <- sprintf("%s (AUC = %s)", legends, auc.data)
  }

  names(g.list) <- legends

  suppressMessages(
    pROC::ggroc(g.list, legacy.axes = TRUE) +
      ggplot2::geom_abline(intercept = 0, color = "darkgrey", linetype = "dashed", size = 0.25) +
      gg_xbreaks_continuous(0, 1, by = 0.2) +
      gg_ybreaks_continuous(0, 1, by = 0.2) +
      theme_sci() +
      gg_legend_title(NULL)
    )
}

