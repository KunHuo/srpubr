#' Plot a ROC curve with ggplot2
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
#' @param auc a logical, indicating whether to show auc.
#' @param auc.ci a logical, indicating whether to show the CI of auc.
#' @param auc.digits digits for auc and its CIs, default 2.
#' @param auc.ci.method the method to use, either “delong” or “bootstrap”. The
#' first letter is sufficient. If omitted, the appropriate method is selected as
#' explained in details.
#' @param line.size line size.
#' @param line.color line color.
#' @param line.type line type.
#' @param language language, typically “en”, or "zh", default "en".
#' @param font.family font family.
#' @param font.size font size.
#' @param progress the name of progress bar to display. Typically “none”, “win”,
#' “tk” or “text”.
#' @param boot.n the number of bootstrap replicates, default 1000.
#' @param seed seed, default 1234.
#' @param ... further arguments pass to [gg_theme_sci] function.
#'
#' @return a ggplot.
#' @export
gg_roc <- function(data,
                   outcome,
                   exposure,
                   positive = NULL,
                   combine = FALSE,
                   combine.only = FALSE,
                   smooth = FALSE,
                   smooth.args = list(),
                   auc = FALSE,
                   auc.ci = FALSE,
                   auc.digits = 2,
                   auc.ci.method = c("delong", "bootstrap"),
                   line.size = 0.5,
                   line.color = NULL,
                   line.type = NULL,
                   language  = c("en", "zh"),
                   font.family = "serif",
                   font.size = 12,
                   progress = "win",
                   boot.n = 1000,
                   seed = 1234,
                   ...){

  set.seed(seed)

  auc.ci.method <- match.arg(auc.ci.method)
  language      <- match.arg(language)

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

  if (auc) {
    auc.string <- sapply(roclist, function(x) {
      .auc_string(
        x,
        auc.ci = auc.ci,
        digits = auc.digits,
        method = auc.ci.method,
        boot.n = boot.n,
        seed = seed,
        progress = progress
      )
    })
    legends  <- sprintf("%s (%s)", names(roclist), auc.string)
    names(roclist) <- legends
  }

  if(language == "zh"){
    sysfonts::font_add("simsun", "simsun.ttc")
    font.family <- "simsun"
  }else{
    font.family <- font.family
  }

  if(is.null(line.type)){
    aes <- c("colour")
  }else{
    aes <- c("linetype", "colour")
  }

  p <- suppressMessages(
    pROC::ggroc(roclist, legacy.axes = TRUE, size = line.size, aes = aes) +
      ggplot2::geom_abline(intercept = 0, color = "darkgrey", linetype = "dashed", size = line.size) +
      gg_xbreaks_continuous(0, 1, by = 0.2) +
      gg_ybreaks_continuous(0, 1, by = 0.2) +
      gg_xlab(sprintf("1 - %s", string_specificity(language))) +
      gg_ylab(string_sensitivity(language)) +
      gg_theme_sci(font.family = font.family, font.size = font.size, ...) +
      gg_legend_title(NULL) +
      gg_legend_position(c(1, 0))
    )

  if(!is.null(line.color)){
    p <- p +
      ggplot2::scale_color_manual(values = line.color)
  }

  if(!is.null(line.type)){
    p <- p +
      ggplot2::scale_linetype_manual(values = line.type)
  }

  p
}


.pred_prob <- function(data, outcome, exposure, newdata = NULL){
  frm <- paste(exposure, collapse = " + ")
  frm <- paste(outcome, frm, sep  = " ~ ")
  frm <- stats::as.formula(frm)

  fit <- stats::glm(formula = frm, data = data, family = stats::binomial(link = "logit"))

  if(is.null(newdata)){
    stats::predict(fit, type = "response")
  }else{
    stats::predict(fit, type = "response", newdata = newdata)
  }
}


.auc_string <- function(x, auc.ci = TRUE, digits = 2, method = "delong", boot.n = 1000, seed = 1234, progress = "text"){
  set.seed(seed)
  if(auc.ci){
    res <- pROC::ci.auc(x, method = method, boot.n = boot.n, progress = progress)
    sprintf("AUC = %s, 95%% CI: %s-%s",
            format_digits(res[[2]], digits),
            format_digits(res[[1]], digits),
            format_digits(res[[3]], digits))
  }else{
    sprintf("AUC = %s", format_digits(pROC::auc(x), digits))
  }
}
