#' Identify univariate outliers
#'
#' @description  Detect outliers using boxplot methods. Boxplots are a popular and an
#' easy method for identifying outliers. There are two categories of outlier:
#' (1) outliers and (2) extreme points.
#'
#' Values above Q3 + 1.5xIQR or below Q1 - 1.5xIQR are considered as outliers.
#' Values above Q3 + 3xIQR or below Q1 - 3xIQR are considered as extreme points
#' (or extreme outliers).
#'
#' Q1 and Q3 are the first and third quartile, respectively. IQR is the
#' interquartile range (IQR = Q3 - Q1).
#'
#' Generally speaking, data points that are labelled outliers in boxplots are not
#' considered as troublesome as those considered extreme points and might even
#' be ignored. Note that, any NA and NaN are automatically removed before the
#' quantiles are computed.
#'
#' @param data a data frame.
#' @param varnames numeric variable names.
#' @param group group variable name.
#' @param language language, typically “en”, or "zh", default "en".
#' @param table.number table number.
#' @param labels a data frame contain the column of term, code, and label.
#' @param ... unused arguments.
#'
#' @return
#' identify_outliers() returns a data frame.
#'
#' is_outlier() and is_extreme() return logical vectors.
#' @export
identify_outliers <- function(data, group = NULL, varnames = NULL, language = NULL, table.number = NULL, labels = NULL, ...){

  language <- get_global_languange(language, default = "en")
  labels   <- get_global_labels(labels)

  varnames <- select_numeric(data, varnames, type = "name")
  varnames <- setdiff(varnames, group)

  if(length(varnames) == 0L){
    cat("\n No numeric variables to identify outliers. \n\n")
    return(invisible(NULL))
  }

  names(varnames) <- varnames

  data$.row.number <- 1:nrow(data)

  exec <- function(data, varname){
    data$outlier <- is_outlier(data[[varname]])
    data$extreme <- is_extreme(data[[varname]])
    data <- data[, c(".row.number", varname, "outlier", "extreme")]
    names(data)[2] <- "value"
    data
  }

  out <- lapply(varnames, function(varname){
    res <- group_exec(data, group = group, \(d){
      exec(data, varname)
    })
  })

  out <- list_rbind(out, collapse.names = FALSE)
  out <- out[!is.na(out$outlier) & out$outlier, ]

  if(nrow(out) < 1L){
    cat("\nNo outliers.\n\n")
    return(invisible(NULL))
  }

  out[[1]] <- delete_duplicate_values(out[[1]])
  out <- tibble::as_tibble(out)

  # Set labels
  if(!is.null(labels)){
    out <- add_lables(out, ldata = labels, col = 1)
    group.labels <- sapply(group, \(x){
      l <- find_labels(data = tidy_labels(labels), varname = x)
      ifelse(is_empty(l), x, l)
    })
    names(out)[2:(2+length(group) - 1)] <- group.labels
  }

  # set names
  names(out)[1] <- string_variable(language)
  names(out)[names(out) == ".row.number"] <- ifelse(language == "en", "Row number", "\u884c\u53f7")
  names(out)[names(out) == "value"]       <- ifelse(language == "en", "Value",      "\u6570\u503c")
  names(out)[names(out) == "outlier"]     <- ifelse(language == "en", "Outlier",    "\u662f\u5426\u5f02\u5e38\u503c")
  names(out)[names(out) == "extreme"]     <- ifelse(language == "en", "Extreme",    "\u662f\u5426\u6781\u7aef\u503c")

  attr(out, "title") <- string_title_outlier(language, table.number)
  attr(out, "note")  <- string_outlier_note(language)

  class(out) <- c("srp.outlier", class(out))
  out
}


#' Print class of 'srp.outlier'
#'
#' @param x an object.
#' @param ... passed to print.
#'
#' @keywords internal
#'
#' @export
print.srp.outlier <- function(x, ...){
  print_booktabs(x, adj = c("left"), ...)
}


string_title_outlier <- function(language, number = NULL){
  title <- switch(language,
                  en = "Identify univariate outliers using boxplot methods",
                  zh = "\u7bb1\u7ebf\u56fe\u8bc6\u522b\u5f02\u5e38\u503c")
  if(!is.null(number)){
    title <- switch(language,
                    en = paste(sprintf("Table %d:", number), title, sep = "  "),
                    zh = paste(sprintf("\u8868%d", number),  title, sep = "  "))
  }
  title
}


string_outlier_note <- function(language){
  english <- paste("Note: Values above Q3 + 1.5\u00D7IQR or below Q1 - 1.5\u00D7IQR",
                   "are considered as outliers. Values above Q3 + 3\u00D7IQR or",
                   "below Q1 - 3\u00D7IQR are considered as extreme points (or",
                   "extreme outliers). Q1 and Q3 are the first and third",
                   "quartile, respectively. IQR is the interquartile range",
                   "(IQR = Q3 - Q1).", sep = " ")
  chinese <-  ""

  switch(language,
         en = english,
         zh = chinese)
}


#' @rdname identify_outliers
#'
#' @param x a numeric vector.
#' @param coef coefficient specifying how far the outlier should be from the edge
#' of their box. Possible values are 1.5 (for outlier) and 3 (for extreme points
#' only). Default is 1.5
#'
#' @export
is_outlier <- function (x, coef = 1.5) {
  res  <- x
  Q1   <- stats::quantile(x, 0.25, na.rm = TRUE)
  Q3   <- stats::quantile(x, 0.75, na.rm = TRUE)
  .IQR <- stats::IQR(x, na.rm = TRUE)

  upper.limit <- Q3 + (coef * .IQR)
  lower.limit <- Q1 - (coef * .IQR)

  outlier <- ifelse(x < lower.limit | x > upper.limit, TRUE, FALSE)
  outlier
}


#' @rdname identify_outliers
#' @export
is_extreme <- function (x) {
  is_outlier(x, coef = 3)
}
