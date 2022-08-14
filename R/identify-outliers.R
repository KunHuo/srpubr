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
#'
#' @return
#' identify_outliers() returns a data frame.
#'
#' is_outlier() and is_extreme() return logical vectors.
#' @export
identify_outliers <- function(data, group = NULL, varnames = NULL, language = NULL){

  language <- get_global_languange(language, default = "en")

  if(is.null(varnames)){
    varnames <- names(data)
  }else{
    varnames <- select_col_names(data, varnames)
  }

  varnames <- setdiff(varnames, group)
  varnames <- varnames[sapply(data[varnames], is.numeric)]

  if(length(varnames) == 0L){
    cat("\n No numeric variables to identify outliers. \n\n")
    return(invisible(NULL))
  }

  names(varnames) <- varnames

  data$.row.number <- 1:nrow(data)

  exec <- function(d, varname){
    x <- d[[varname]]
    row.number <- which(is_outlier(x))
    if(length(row.number) != 0L){
      res <- data[row.number, c(".row.number", varname), drop = FALSE]
      res$outlier <- "TRUE"
      names(res)[2] <- "value"
      extreme <- which(is_extreme(x))
      if(length(extreme) != 0L){
        extreme.number <- data[extreme, ".row.number", drop = FALSE]
        extreme.number$extreme <- "TRUE  *"
        res <- merge_left(res, extreme.number, by = ".row.number")
        res$extreme[is.na(res$extreme)] <- "FALSE"
        res
      }else{
        res$extreme <- "FALSE"
      }
      row.names(res) <- NULL
      res
    }else{
      NULL
    }
  }

  out <- lapply(varnames, function(varname){
    if(is.null(group)){
      exec(data, varname)
    }else{
       res <- NULL
       for(i in unique(data[[group]])){
         d <- data[data[[group]] == i, , drop = FALSE]
         r <- exec(d, varname)
         if(!is.null(r)){
           r <- append2(r, data.frame(group = i), after = 0)
         }
         res <- rbind(res, r)
       }
      res
    }
  })

  if(all(sapply(out, is.null))){
    cat("\n No outliers. \n\n")
    return(invisible(NULL))
  }

  out <- list_rbind(out, collapse.names = FALSE)
  out[[1]] <- delete_duplicate_values(out[[1]])

  out <- tibble::as_tibble(out)
  #names(out)[1] <- "Row number"

  attr(out, "title") <- "Identify univariate outliers using boxplot methods"
  attr(out, "note")  <- paste("Note: Values above Q3 + 1.5\u00D7IQR or below Q1 - 1.5\u00D7IQR",
                        "are considered as outliers. Values above Q3 + 3\u00D7IQR or",
                        "below Q1 - 3\u00D7IQR are considered as extreme points (or",
                        "extreme outliers). Q1 and Q3 are the first and third",
                        "quartile, respectively. IQR is the interquartile range",
                        "(IQR = Q3 - Q1).", sep = " ")

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
print.srp.outlier<- function(x, ...){
  print_booktabs(x, adj = c("left"), ...)
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
