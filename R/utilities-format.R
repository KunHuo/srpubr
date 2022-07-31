#' Format digits
#'
#' @param x typically numeric.
#' @param digits
#' how many significant digits are to be used for numeric
#'
#' @return a character string.
#' @seealso [format]
#' @export
#'
#' @examples
#' format_digits(43.25576, digits = 2)
#' format_digits(43, digits = 5)
format_digits <- function(x, digits){
  sapply(x, function(i){
    if(is.na(i)){
      i
    }else{
      fmt <- sprintf("%%.%df", digits)
      sprintf(fmt = fmt, i)
    }
  })
}


#' Format statistic values
#'
#' @param x numeric vector.
#' @param digits digits.
#'
#' @return character vector.
#' @export
#'
#' @examples
#' format_statistic(0.99999, digits = 3)
#' format_statistic(0.00001, digits = 3)
format_statistic <- function(x, digits) {
  fmt <- paste0("%.", digits, "f")
  pVec <- sapply(x, function(i) {
    if (is.na(i)) {
      NA
    } else {
      sprintf(fmt = fmt, i)
    }
  })

  small <- paste0("<0.", paste0(rep("0", digits - 1), collapse = ""), "1")
  pos.small <- grepl("^0\\.0*$", pVec)
  pVec[pos.small] <- small

  return(pVec)
}


#' Format P values
#'
#' @param x numeric vector.
#' @param digits digits.
#'
#' @return character vector.
#' @export
#'
#' @examples
#' format_pvalue(0.32555, digits = 3)
#' format_pvalue(0.00001, digits = 3)
#' format_pvalue(c(0.23, 0.2511, 0.0001, 0.01), digits = 3)
format_pvalue <- function(x, digits) {
  fmt  <- paste0("%.", digits, "f")

  pVec <- sapply(x, function(i){
    if(is.na(i)){
      NA
    }else{
      sprintf(fmt = fmt, i)
    }
  })
  smallPString <- paste0("<0.", paste0(rep("0", digits - 1), collapse = ""), "1")
  posAllZeros <- grepl("^0\\.0*$", pVec)

  pVec[posAllZeros]  <- smallPString
  return(pVec)
}


#' Format P values with signif start
#'
#' @param pvalues P values
#' @param digits digits.
#'
#' @return character vector.
#' @export
#'
#' @examples
#' format_signif_start(c(0.0001, 0.012, 0.02, 0.035, 0.24))
format_signif_start <- function(pvalues, digits = 3){
  if(is.numeric(pvalues)){
    sign <- ifelse(pvalues <= 0.001, "***",
                   ifelse(pvalues <= 0.01, "**",
                          ifelse(pvalues <= 0.05, "*",
                                 ifelse(pvalues <= 0.1, ".", ""))))
    sign <- ifelse(is.na(sign), "", format(sign, justify = "left"))

    pvalues <- format_pvalue(pvalues, digits)
    pvalues <- ifelse(is.na(pvalues), "", format(pvalues, justify = "right"))
    paste(pvalues, sign, sep = " ")
  }else{
    pvalues.numeric <- as.numeric(gsub(pattern = "<|>", replacement = "", x = pvalues))
    sign <- ifelse(pvalues.numeric <= 0.001, "***",
                   ifelse(pvalues.numeric <= 0.01, "**",
                          ifelse(pvalues.numeric <= 0.05, "*",
                                 ifelse(pvalues.numeric <= 0.1, ".", ""))))
    sign <- ifelse(is.na(sign), "", format(sign, justify = "left"))
    pvalues <- ifelse(is.na(pvalues), "", format(pvalues, justify = "right"))
    paste(pvalues, sign, sep = " ")
  }
}


#' Format variable names for output
#'
#' @param data a data frame.
#' @param varnames variable names.
#' @param fold whether fold variable.
#' @param space space.
#' @param sep sep.
#' @param add.first add before first.
#' @param add.last add after last.
#'
#' @return a data frame.
#' @export
format_variable <- function(data,
                            varnames = names(data),
                            fold = FALSE,
                            space = 4,
                            sep = " vs. ",
                            add.first = NULL,
                            add.last = NULL){
  space <- create_space(space)
  sep <- sprintf("%s", sep)
  execute <-  function(varname){
    label <- varname
    if(!is.null(data)){
      label <- attr(data[[varname]], "label")
      if(is.null(label)){
        label <- varname
      }
    }
    if(is.numeric(data[[varname]])){
      data.frame(term = varname, varname = varname, ref = FALSE, variable = label, stringsAsFactors = FALSE)
    }else if(is.factor(data[[varname]]) | is.character(data[[varname]])){
      levels <- extract_levels(data[[varname]])
      if(fold){
        style_factor1(varname, levels, label, space = space, sep = sep)
      }else{
        style_factor2(varname, levels, label, space = space)
      }
    }
  }

  res <- lapply(varnames, execute)
  res <- do.call(rbind, res)

  if(!is.null(add.first)){
    if(!is.data.frame(add.first)){
      first <- data.frame(term = add.first, varname = add.first, ref = FALSE,
                          variable = add.first, stringsAsFactors = FALSE)
      res <- rbind(first, res)
    }else{
      res <- rbind(add.first, res)
    }
  }

  if(!is.null(add.last)){
    if(!is.data.frame(add.last)){
      last <- data.frame(term = add.last, varname = add.last, ref = FALSE,
                         variable = add.last, stringsAsFactors = FALSE)
      res <- rbind(res, last)
    }else{
      res <- rbind(res, add.last)
    }
  }
  res
}


create_space <- function(n){
  strrep(" ", n)
}


extract_levels <- function(x){
  if(is.character(x)){
    x <- as.factor(x)
  }
  levels(x)
}


style_factor1 <- function(varname, levels, label, space, sep) {
  if (length(levels) <= 2L) {
    variable <- sprintf("%s (%s%s%s)", label, levels[2], sep, levels[1])
    term <- paste0(varname, levels[2])
    data.frame(term = term, varname = varname, ref = FALSE, variable = variable, stringsAsFactors = FALSE)
  } else{
    term <- paste0(varname, levels[-1])
    term <- c(varname, term)
    variable <- paste(levels[-1], levels[1], sep = sep)
    variable <- paste0(space, variable)
    variable <- c(label, variable)
    data.frame(term = term, varname = varname, ref = FALSE, variable = variable, stringsAsFactors = FALSE)
  }
}


style_factor2 <- function(varname, levels, label, space) {
  term <- paste0(varname, levels)
  term <- c(varname, term)
  variable <- paste0(space, levels)
  variable <- c(label, variable)
  ref <- c(FALSE, TRUE, rep(FALSE, length(levels) - 1))
  data.frame(term = term, varname = varname, ref = ref,  variable = variable, stringsAsFactors = FALSE)
}



