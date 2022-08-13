#' Identify missing pattern
#'
#' @param data a data frame.
#' @param show.all a logical, indicate whether to show all variables, by default,
#' only show missing variables.
#' @param decreasing a logical. Should the sort order be increasing or decreasing?
#' if is NULL, not sorted.
#' @param language language, typically “en”, or "zh", default "en".
#' @param table.number table number.
#' @param ... unused arguments.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' # Basic example
#' identify_missing_pattern(lung)
#'
#' # Show all variables
#' identify_missing_pattern(lung, show.all = TRUE)
identify_missing_pattern <- function (data,
                                      show.all = FALSE,
                                      decreasing = TRUE,
                                      language = NULL,
                                      table.number = NULL,
                                      ...) {

  language <- get_global_languange(language, default = "en")

  if (!(is.matrix(data) || is.data.frame(data))) {
    stop("Data should be a matrix or dataframe")
  }
  if (ncol(data) < 2) {
    stop("Data should have at least two columns")
  }
  R <- is.na(data)
  nmis <- colSums(R)
  R <- matrix(R[, order(nmis)], dim(data))
  pat <- apply(R, 1, function(data) paste(as.numeric(data), collapse = ""))
  sortR <- matrix(R[order(pat), ], dim(data))
  if (nrow(data) == 1) {
    mpat <- is.na(data)
  }
  else {
    mpat <- sortR[!duplicated(sortR), ]
  }
  if (all(!is.na(data))) {
    cat(string_no_missing(language), "\n")
    return(invisible(NULL))
  }
  else {
    if (is.null(dim(mpat))) {
      mpat <- t(as.matrix(mpat))
    }
    rownames(mpat) <- table(pat)
  }
  r <- cbind(abs(mpat - 1), rowSums(mpat))
  r <- rbind(r, c(nmis[order(nmis)], sum(nmis)))

  np <- attr(r, "dimnames")[[1]]
  np[length(np)] <- ifelse(language == "en", "Var missing", "\u53d8\u91cf\u7f3a\u5931\u4f8b\u6570")
  r <- as.data.frame(r)
  r <- append2(r, data.frame(np = np), after = 0)

  names(r)[1]       <- ifelse(language == "en", "Pattern count", "\u7f3a\u5931\u6a21\u5f0f\u4f8b\u6570")
  names(r)[ncol(r)] <- ifelse(language == "en", "Missing count", "\u7f3a\u5931\u4f8b\u6570")

  if(!show.all){
    col.index <- sapply(r, function(x){
      if(is.numeric(x)){
        ifelse(x[length(x)] == 0, FALSE, TRUE)
      }else{
        TRUE
      }
    })
    r <- r[col.index]
  }

  varnames <- identify_missing(data, show.all = show.all, decreasing = decreasing)[[1]]
  varnames <- c(names(r)[1], varnames, names(r)[ncol(r)])

  n.miss.variable <- sum(sapply(data, function(x){ any(is.na(x)) }))

  title1 <- string_title_missing_pattern(language, number = table.number)
  title2 <- string_missing(language, n.miss.variable, ncol(data))
  title  <-  paste(title1, title2, sep = "")

  r <- r[varnames]
  r <- add_title(r, title)
  r <- add_note(r, string_note_missing_pattern(language))
  r <- tibble::as_tibble(r)

  class(r) <- c("srp.mp", class(r))
  r
}


#' Print class of 'srp.mp'
#'
#' @param x an object.
#' @param ... passed to print.
#'
#' @keywords internal
#'
#' @export
print.srp.mp <- function(x, ...){
  print_booktabs(x, adj = c("center"))
}


string_title_missing_pattern <- function(language, number = NULL){
  title <- switch(language,
                  en = "Data missing pattern",
                  zh = "\u6570\u636e\u7f3a\u5931\u6a21\u578b")
  if(!is.null(number)){
    title <- switch(language,
                    en = paste(sprintf("Table %d:", number), title, sep = "  "),
                    zh = paste(sprintf("\u8868%d", number),  title, sep = "  "))
  }
  title
}

string_note_missing_pattern <- function(language){
  switch(language,
         en = "Note: Each row corresponds to a missing data pattern (1=observed, 0=missing), the last column and row contain row and column counts, respectively.",
         zh = "\u6ce8\uff1a\u6bcf\u4e00\u884c\u5bf9\u5e94\u4e00\u4e2a\u7f3a\u5931\u7684\u6570\u636e\u6a21\u5f0f\uff08\u0031\u003d\u975e\u7f3a\u5931\uff0c\u0030\u003d\u7f3a\u5931\uff09\uff0c\u0020\u6700\u540e\u4e00\u5217\u4e3a\u6bcf\u4e2a\u7f3a\u5931\u6a21\u5f0f\u4e2d\u7f3a\u5931\u7684\u4f8b\u6570\uff0c\u6700\u540e\u4e00\u884c\u4e3a\u6bcf\u4e2a\u53d8\u91cf\u4e2d\u7f3a\u5931\u7684\u4f8b\u6570\u3002")
}
