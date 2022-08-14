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
#' @param ... numeric variables.
#'
#' @return
#' identify_outliers() returns a data frame.
#'
#' is_outlier() and is_extreme() return logical vectors.
#' @export
identify_outliers <- function(data, ...){

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