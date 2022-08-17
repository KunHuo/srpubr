#' Overview a data frame
#'
#' @param data a data frame.
#' @param digits digits, default 2.
#' @param ... unused.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' overview(aSAH)
#'
#' overview(lung)
overview <- function(data, digits = 2, language = NULL, labels = NULL, ...){

  language <- get_global_languange(language, default = "en")
  labels   <- get_global_labels(labels)

  out <- lapply(names(data), function(x){
    dv <- data[[x]]

    type   <- class(dv)[1]
    unique <- length(unique(na.omit(dv)))
    miss   <- sum(is.na(dv))
    valid  <- length(dv) - miss

    if(is.factor(dv)){
      all <- data.frame(col = which(names(data) == x),
                        variable = x,
                        type = type,
                        valid = valid,
                        miss = miss,
                        unique = unique)
      levels <- split.data.frame(data, f = dv)
      levels <- lapply(levels, function(ld){
        data.frame(type = "    level",
                   unique = length(unique(ld[[x]])),
                   valid = length(ld[[x]]),
                   miss = sum(is.na(ld[[x]])))
      })
      levels <- list_rbind(levels)
      levels$variable <- paste0("    ", levels$variable)
      levels <- append2(levels, data.frame(col = NA), after = 0)
      rbind(all, levels)
    }else{
      data.frame(col = which(names(data) == x),
                 variable = x,
                 type = type,
                 unique = unique,
                 valid = valid,
                 miss = miss)
    }
  })

  out <- do.call(rbind, out)
  out <- tibble::as_tibble(out)

  # Set labels
  if(!is.null(labels)){
    out <- add_lables(out, ldata = labels, col = 2)
  }

  names(out)[names(out) == "variable"] <- string_variable(language)
  names(out)[names(out) == "col"]      <- ifelse(language == "en", "Col",     "\u5217\u7d22\u5f15")
  names(out)[names(out) == "type"]     <- ifelse(language == "en", "Type",    "\u6570\u636e\u7c7b\u578b")
  names(out)[names(out) == "valid"]    <- ifelse(language == "en", "Valid",   "\u6709\u6548\u4f8b\u6570")
  names(out)[names(out) == "unique"]   <- ifelse(language == "en", "Unique",  "\u552f\u4e00\u503c")
  names(out)[names(out) == "miss"]     <- ifelse(language == "en", "Missing", "\u7f3a\u5931\u4f8b\u6570")

  attr(out, "title") <- sprintf("Overview the data [%dx%d]", nrow(data), ncol(data))
  class(out) <- c("srp.overview", class(out))
  out
}


#' Print class of 'srp.overview'
#'
#' @param x an object.
#' @param ... passed to print.
#'
#' @keywords internal
#'
#' @export
print.srp.overview <- function(x, ...){
  print_booktabs(x, adj = c("l", "l", "l", "r"), ...)
}
