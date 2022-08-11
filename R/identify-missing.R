#' Missing value analysis
#'
#' @param data a data frame.
#' @param show.all a logical, indicate whether to show all variables, by default,
#' only show missing variables.
#' @param digits digits for missing percent, defualt 2.
#' @param language language, typically “en”, or "zh", default "en".
#' @param table.number table number.
#' @param ... unused arguments.
#'
#' @return a data frame.
#' @export
identify_missing <- function(data, show.all = FALSE, digits = 2, language = c("en", "zh"), table.number = NULL, ...){

  language <- match.arg(language)

  exec_missing <- function(x){
    f <- class(x)
    n <- length(x)
    m <- sum(is.na(x))
    p <- sprintf("%s%%", format_digits(m / n * 100, digits = digits))

    data.frame(type = f, n = length(x), m = m, p = p)
  }

  res.miss <- lapply(data, function(x){
    if(show.all){
      exec_missing(x)
    }else{
      if(any(is.na(x))){
        exec_missing(x)
      }
    }
  })

  if(all(sapply(res.miss, is.null))){
    cat("\n", string_no_missing(language),"\n\n")
    invisible(NULL)

  }else{
    res <- list_rbind(res.miss)

    names(res)[names(res) == "variable"] <- string_variable(language)
    names(res)[names(res) == "type"]     <- string_missing_type(language)
    names(res)[names(res) == "n"]        <- string_missing_total(language)
    names(res)[names(res) == "m"]        <- string_missing_count(language)
    names(res)[names(res) == "p"]        <- string_missing_percent(language)

    n.miss.variable <- sum(sapply(data, function(x){ any(is.na(x)) }))

    title1 <- string_title_missing(language, number = table.number)
    title2 <- string_missing(language, n.miss.variable, ncol(data))
    title <-  paste(title1, title2, sep = "")

    res <-  add_title(res, title)
    res <- tibble::as_tibble(res)

    class(res) <- c("srp.miss", class(res))

    res
  }
}



#' Print class of 'srp.miss'
#'
#' @param x an object.
#' @param ... passed to print.
#'
#' @keywords internal
#'
#' @export
print.srp.miss <- function(x, ...){
  x[[ncol(x)]] <- str_align(x[[ncol(x)]], sep = ".")
  print_booktabs(x, adj = c("left", "left", "center"))
}



string_missing <- function(language, n.miss.variable, n.total.variable){
  switch(language,
         en = sprintf(", %d missing in %d variables", n.miss.variable, n.total.variable),
         zh = sprintf("\uff0c%d\u4e2a\u53d8\u91cf\u4e2d%d\u4e2a\u6709\u7f3a\u5931\u503c",
                      n.total.variable, n.miss.variable))
}

string_no_missing <- function(language){
  switch(language,
         en = "No missing values.",
         zh = "\u6ca1\u6709\u7f3a\u5931\u503c\u3002")
}


string_missing_type <- function(language){
  switch(language,
         en = "Type",
         zh = "\u6570\u636e\u7c7b\u578b")
}


string_missing_total <- function(language){
  switch(language,
         en = "n",
         zh = "\u603b\u4f8b\u6570")
}


string_missing_count <- function(language){
  switch(language,
         en = "Missing count",
         zh = "\u7f3a\u5931\u4f8b\u6570")
}


string_missing_percent <- function(language){
  switch(language,
         en = "Missing percent",
         zh = "\u7f3a\u5931\u6bd4\u4f8b")
}


string_title_missing <- function(language, number = NULL){
  title <- switch(language,
                  en = "Missing analysis",
                  zh = "\u7f3a\u5931\u503c\u5206\u6790")
  if(!is.null(number)){
    title <- switch(language,
                    en = paste(sprintf("Table %d:", number), title, sep = " "),
                    zh = paste(sprintf("\u8868%d", number),  title, sep = " "))
  }
  title
}


gg_missing <- function(data, language = 'en'){
  data.miss <- identify_missing(data, language = language)

  names(data.miss) <- c("variable", "type", "total", "miss.count", "miss.percent")

  ggplot2::ggplot(data = data.miss) +
    ggplot2::geom_col(ggplot2::aes_string(x = "variable", y = "miss.count", fill = "variable"), width = 0.65, show.legend = FALSE) +
    gg_theme_sci() +
    gg_delete_x_title() +
    gg_rotate_x_text(45) +
    gg_ybreaks_continuous(0, 100, 20)
}

