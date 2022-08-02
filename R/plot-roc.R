gg_roc <- function(data,
                   outcome,
                   exposure,
                   positive = NULL,
                   combine = FALSE,
                   combine.only = FALSE,
                   auc = TRUE,
                   auc.ci = TRUE,
                   auc.digits = 2,
                   smooth = FALSE,
                   smooth.args = list(),
                   ci.method = c("delong", "bootstrap"),
                   language  = c("en", "zh"),
                   font.family = "serif"){

  ci.method <- match.arg(ci.method)
  language  <- match.arg(language)

  if(!is.factor(data[[outcome]])){
    data[[outcome]] <- factor(data[[outcome]])
  }

  if(!is.null(positive)){
    positive <- as.character(positive)
    negative <- setdiff(unique(data[[outcome]]), positive)
    data[[outcome]] <-  factor(data[[outcome]],  levels = c(negative, positive))
  }

  if(combine & length(exposure) != 1L){
    data$combine <- .pred_prob(data = data, outcome = outcome, exposure = exposure)
    if(combine.only){
      exposure <- "combine"
    }else{
      exposure <- c(exposure, "combine")
    }
  }

  data[exposure] <- lapply(data[exposure], function(x){
    if(is.factor(x)){
      as.numeric(x)
    }else if(is.character(x)){
      as.numeric(as.factor(x))
    }else{
      x
    }
  })

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

  if(language == "zh"){
    font.family <- "simsun"
  }else{
    font.family <- font.family
  }

  suppressMessages(
    pROC::ggroc(g.list, legacy.axes = TRUE) +
      ggplot2::geom_abline(intercept = 0, color = "darkgrey", linetype = "dashed", size = 0.25) +
      gg_xbreaks_continuous(0, 1, by = 0.2) +
      gg_ybreaks_continuous(0, 1, by = 0.2) +
      gg_xlab(sprintf("1 - %s", string_specificity(language))) +
      gg_ylab(string_sensitivity(language)) +
      gg_theme_sci(font.family = font.family) +
      gg_legend_title(NULL) +
      gg_legend_position(c(1, 0))
    )
}


string_combine <- function(language){
  switch(language,
         en = "Combine",
         zh = "\u8054\u5408")
}


string_sensitivity <- function(language){
  switch(language,
         en = "Sensitivity",
         zh = "\u654f\u611f\u5ea6")

}


string_specificity <- function(language){
  switch(language,
         en = "Specificity",
         zh = "\u7279\u5f02\u5ea6")
}


.pred_prob <- function(data, outcome, exposure, newdata = NULL){
  frm <- paste(exposure, collapse = " + ")
  frm <- paste(outcome, frm, sep = " ~ ")
  frm <- stats::as.formula(frm)

  fit <- stats::glm(formula = frm, data = data, family = stats::binomial(link = "logit"))

  if(is.null(newdata)){
    predict(fit, type = "response")
  }else{
    predict(fit, type = "response", newdata = newdata)
  }
}
