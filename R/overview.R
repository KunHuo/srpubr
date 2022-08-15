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
overview <- function(data, digits = 2, ...){

  out <- lapply(names(data), function(x){
    dv <- data[[x]]

    type   <- class(dv)[1]
    unique <- length(unique(dv))
    miss   <- sum(is.na(dv))
    valid  <- length(dv) - miss

    if(is.numeric(dv)){
      min    <- min(dv, na.rm = TRUE)
      max    <- max(dv, na.rm = TRUE)
      median <- format_digits(stats::median(dv, na.rm = TRUE), digits)
      mean   <- format_digits(mean(dv, na.rm = TRUE), digits)
      data.frame(col = which(names(data) == x),
                 variable = x,
                 type = type,
                 valid = valid,
                 unique = unique,
                 miss = miss,
                 min = min,
                 mean = mean,
                 median = median,
                 max = max)
    }else if(is.factor(dv)){
      all <- data.frame(col = which(names(data) == x),
                        variable = x,
                        type = type,
                        valid = valid,
                        unique = unique,
                        miss = miss)
      levels <- split.data.frame(data, f = dv)
      levels <- lapply(levels, function(ld){
        data.frame(type = "  level",
                   valid = length(ld[[x]]),
                   unique = length(unique(ld[[x]])),
                   miss = sum(is.na(ld[[x]])))
      })
      levels <- list_rbind(levels)
      levels$variable <- paste0("  ", levels$variable)
      levels <- append2(levels, data.frame(col = NA), after = 0)
      rbind(all, levels)
    }else{
      data.frame(col = which(names(data) == x),
                 variable = x,
                 type = type,
                 valid = valid,
                 unique = unique,
                 miss = miss)
    }
  })

  out <- lapply(out, \(d){
    if(ncol(d) == 6L){
      d$min <- NA
      d$mean <- NA
      d$median <- NA
      d$max <- NA
    }
    d
  })

  out <- do.call(rbind, out)
  out <- tibble::as_tibble(out)
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
  print_booktabs(x, adj = c("c", "l", "l", "r"), ...)
}
