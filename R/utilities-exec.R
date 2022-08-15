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


group_exec <- function(data, group, func, ...){
  INDICES <- lapply(group, function(x) {
    data[[x]]
  })
  names(INDICES) <- group
  res <- by(data = data, INDICES = INDICES, FUN = func, ..., simplify = FALSE)
  dn <- dimnames(res)
  d <- dim(res)
  by.res <- lapply(X = seq_along(res), FUN = function(i, res) {
    ii <- i - 1L
    out <- NULL
    for (j in seq_along(dn)) {
      iii <- ii%%d[j] + 1L
      ii <- ii%/%d[j]
      out <- c(out, dn[[j]][iii])
    }
    out
  }, res)


  by.res <- do.call(rbind, by.res)
  by.res <- as.data.frame(by.res)

  by.res <- lapply(by.res, function(x){
    rep(x, each = nrow(res[[1]]))
  })

  by.res <- do.call(cbind, by.res)
  by.res <- as.data.frame(by.res)
  colnames(by.res) <- group

  res <- do.call(rbind, res)
  res <- as.data.frame(res)

  res <- cbind(by.res, res)
  rownames(res) <- NULL
  res <- res[order(match(res[[1]], unique(data[[group[1]]]))), ]
  res
}


group_vars <- function(data, groups = NULL){
  if(is.null(groups)){
    if("grouped_df" %in% class(data)){
      tmp <- attr(data, "groups")
      groups <- setdiff(names(tmp), ".rows")
    }
  }
  groups
}


map_dfr <- function(x, func, group = NULL, ...,
                    collapse.names = NULL,
                    collapse.one.row = FALSE,
                    varname = "variable", transpose = TRUE){

  if(is.null(collapse.names)){
    if(is.null(group)){
      collapse.names <- FALSE
    }else{
      collapse.names <- TRUE
    }
  }

  groups <- group_vars(data = x, groups = group)

  nms <- names(x)
  nms <- setdiff(nms, groups)
  names(nms) <- nms

  out <- lapply(nms, \(nm){
    if(is.null(groups)){
      tryCatch({
        do.call(func, args = list(x[[nm]], ...))
      }, error = function(e) {
        print(e)
        NULL })
    }else{
      group_exec(x, group = groups, \(d){
        do.call(func, args = list(d[[nm]], ...))
      })
    }
  })

  # if(transpose){
  #   out <- lapply(out, function(x){
  #     print(x)
  #     transpose(x)
  #   })
  # }


  out <- list_rbind(out,
                    collapse.names = collapse.names,
                    collapse.one.row = collapse.one.row,
                    varname = varname)

  # tibble::as_tibble(out)
}



map_dfc <- function(x, func, group = NULL, ...){
  groups <- group_vars(data = x, groups = group)

  nms <- names(x)
  nms <- setdiff(nms, groups)
  names(nms) <- nms

  out <- lapply(nms, \(nm){
    if(is.null(groups)){
      tryCatch({
        do.call(func, args = list(x[[nm]], ...))
      }, error = function(e) {
        print(e)
        NULL })
    }else{
      group_exec(x, group = groups, \(d){
        do.call(func, args = list(d[[nm]], ...))
      })
    }
  })

  out
}
