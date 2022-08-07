#' Build ROCs
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
#' @param ci a logical, indicating whether to show ci.
#' @param ci.method the method to use, either “delong” or “bootstrap”. The
#' first letter is sufficient. If omitted, the appropriate method is selected as
#' explained in details.
#' @param digits digits, default 2.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' roc(aSAH,
#'     outcome  = "outcome",
#'     exposure = c("age", "s100b"))
#'
#' # Combine with logistic regression.
#' roc(aSAH,
#'     outcome  = "outcome",
#'     exposure = c("age", "s100b"),
#'     combine  = TRUE)
roc <- function(data,
                outcome,
                exposure,
                positive = NULL,
                combine = FALSE,
                combine.only = FALSE,
                ci = TRUE,
                ci.method = c("delong", "bootstrap"),
                smooth = FALSE,
                smooth.args = list(),
                digits = 2){

  roclist <- .roc(data = data,
                  outcome  = outcome,
                  exposure = exposure,
                  positive = positive,
                  combine  = combine,
                  combine.only = combine.only,
                  smooth = smooth,
                  smooth.args = smooth.args)

  res <- lapply(roclist, function(x){
    .delong(x, ci = ci, digits = digits)
  })

  res <- list_rbind(res, varname = "Variable")

  res

}



.delong <- function(object, ci = TRUE, digits = 2){

  fmt  <- sprintf("%%.%df (%%.%df-%%.%df)", digits, digits, digits)
  fmt1 <- sprintf("%%.%df", digits)

  rets <- c("threshold", "accuracy", "specificity", "sensitivity", "ppv", "npv", "tp", "fp", "fn","tn")

  coords <- pROC::coords(object,
                         x = "best",
                         ret = rets,
                         transpose = TRUE)

  tp <- coords[["tp"]]
  fp <- coords[["fp"]]
  fn <- coords[["fn"]]
  tn <- coords[["tn"]]

  q <- stats::qnorm(0.025, lower.tail = FALSE)

  acc       <- coords[["accuracy"]]
  acc.lower <- acc - q * (acc * (1 - acc) / (tp + fp + fn + tn))
  acc.upper <- acc + q * (acc * (1 - acc) / (tp + fp + fn + tn))

  se       <- coords[["sensitivity"]]
  se.lower <- se - q * sqrt(se * (1 - se) / (tp + fn))
  se.upper <- se + q * sqrt(se * (1 - se) / (tp + fn))

  sp       <- coords[["specificity"]]
  sp.lower <- sp - q * sqrt(sp * (1 - sp) / (fp + tn))
  sp.upper <- sp + q * sqrt(sp * (1 - sp) / (fp + tn))

  PPV       <- coords[["ppv"]]
  PPV.lower <- PPV - q * sqrt(PPV * (1 - PPV) / (tp + fp))
  PPV.upper <- PPV + q * sqrt(PPV * (1 - PPV) / (tp + fp))

  NPV       <- coords[["npv"]]
  NPV.lower <- NPV - q * sqrt(NPV * (1 - NPV) / (tn + fn))
  NPV.upper <- NPV + q * sqrt(NPV * (1 - NPV) / (tn + fn))

  AUC <- pROC::ci.auc(object)

  if(ci){
    data.frame(Cutoff      = sprintf(fmt1, coords[["threshold"]]),
               AUC         = sprintf(fmt,  AUC[2], AUC[1],    AUC[3]),
               Accuracy    = sprintf(fmt,  acc,    acc.lower, acc.upper),
               Specificity = sprintf(fmt,  sp,     sp.lower,  sp.upper),
               Sensitivity = sprintf(fmt,  se,     se.lower,  se.upper),
               PPV         = sprintf(fmt,  PPV,    PPV.lower, PPV.upper),
               NPV         = sprintf(fmt,  NPV,    NPV.lower, NPV.upper),
               stringsAsFactors = FALSE)
  }else{
    data.frame(Cutoff      = sprintf(fmt1, coords[["threshold"]]),
               AUC         = sprintf(fmt1, AUC[2]),
               Accuracy    = sprintf(fmt1, acc),
               Specificity = sprintf(fmt1, sp),
               Sensitivity = sprintf(fmt1, se),
               PPV         = sprintf(fmt1, PPV),
               NPV         = sprintf(fmt1, NPV),
               stringsAsFactors = FALSE)
  }
}


.roc <- function(data,
                 outcome,
                 exposure,
                 positive = NULL,
                 combine = FALSE,
                 combine.only = FALSE,
                 smooth = FALSE,
                 smooth.args = list()){

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

  names(exposure) <- exposure

  lapply(exposure, function(x){
    res <- pROC::roc(response  = data[[outcome]],
                     predictor = data[[x]],
                     direction = "<",
                     levels    = levels(data[[outcome]]))
    if(smooth){
      res <- do_call(pROC::smooth, roc = res, smooth.args)
    }
    res
  })
}


roc_test <- function(object, ...){
  if(length(object) != 1L){
    comp <- utils::combn(names(object), 2)
    comp <- as.data.frame(comp, stringsAsFactors = FALSE)

    out <- lapply(comp, function(x){
      comparision <- paste(x[1], x[2], sep = " vs ")
      test <- pROC::roc.test(object[[x[1]]], object[[x[2]]])
      stat <- sprintf("%.3f", test$statistic)
      pvalue <- test$p.value
      pvalue <- ifelse(pvalue < 0.001, "<0.001", sprintf("%.3f", pvalue))
      data.frame(Comparision = comparision,
                 Statistic = stat,
                 P =  pvalue,
                 stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, out)
    row.names(out) <- NULL
    out
  }
}
