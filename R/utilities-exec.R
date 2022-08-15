#' bind a list by rows
#'
#' @param data a data frame.
#' @param names.as.column names as a column, default TRUE..
#' @param collapse.names collapse names, default FALSE.
#' @param collapse.one.row collapse one row, default FALSE.
#' @param varname variable name.
#'
#' @return a data frame.
#' @export
list_rbind <- function(data,
                       names.as.column = TRUE,
                       collapse.names = FALSE,
                       collapse.one.row = FALSE,
                       varname = "variable"){
  if(class(data) != "list"){
    stop("Data must be a list.", call. = FALSE)
  }

  data <- data[!sapply(data, is.null)]

  NAMES <- names(data)

  if(is.null(NAMES)){
    NAMES <- sprintf("%d", 1:length(data))
  }

  collapse <- function(d, nm){
    d[[1]] <- paste0("    ", d[[1]])
    rbind(c(nm, rep(NA, ncol(d) - 1)), d)
  }

  collapse_column <- function(d, nm){
    if(collapse.one.row){
      collapse(d, nm)
    }else{
      if(nrow(d) == 1L){
        tmpname <- names(d)
        d <- cbind(data.frame(nm), d[, -1, drop = FALSE])
        names(d) <- tmpname
        d
      }else{
        collapse(d, nm)
      }
    }
  }

  out <- Map(function(d, nm){
    if(names.as.column){
      if(collapse.names){
        collapse_column(d, nm)
      }else{
        cbind(data.frame(variable = nm), d)
      }
    }else{
      d
    }
  }, data, NAMES)

  out <- do.call(rbind, out)
  row.names(out) <- NULL
  if(names.as.column){
    names(out)[1] <- varname
  }
  out
}


group_exec <- function(data, group = NULL, func = NULL, ...){
  group <- select_variable(data, group)
  if(length(group) == 0L){
    sdat <- list(overall = data)
  }else{
    sdat <- split.data.frame(data, f = data[group], drop = TRUE, sep = "#")
  }
  out <- lapply(sdat, \(d){
    tryCatch(do_call(func, d, ...), error = function(e) NULL)
  })

  list_rbind(out)
}
