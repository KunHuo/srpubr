identify_missing_pattern <- function (data) {
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
    cat("This data set is completely observed.\n")
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
  np[length(np)] <- "Var missing"
  r <- as.data.frame(r)
  r <- append2(r, data.frame(np = np), after = 0)

  names(r)[1]       <- "Pattern count"
  names(r)[ncol(r)] <- "Missing count"
  r
}


gg_missing_pattern <- function(data, show.all = FALSE){

  plotdata <- identify_missing_pattern(data)

  if(!show.all){
    col.index <- sapply(plotdata, function(x){
      if(is.numeric(x)){
        ifelse(x[length(x)] == 0, FALSE, TRUE)
      }else{
        TRUE
      }
    })
    plotdata <- plotdata[col.index]
  }

  plotdata <- plotdata[, -ncol(plotdata), drop = FALSE]
  plotdata <- plotdata[-nrow(plotdata), , drop = FALSE]

  pattern.count <- plotdata[[1]]

  plotdata <- plotdata[, -1, drop = FALSE]
  varnames <- names(plotdata)
  plotdata <- reshape_long(plotdata)

  plotdata$.id    <- factor(plotdata$.id)
  plotdata$.name  <- factor(plotdata$.name,  levels = varnames)
  plotdata$.value <- factor(plotdata$.value, levels = c(0, 1), labels = c("Missing", "No missing"))

  ggplot2::ggplot(plotdata) +
    ggplot2::geom_tile(ggplot2::aes_string(x = ".name", y = ".id", fill = ".value"), color = "black", size = 0.1) +
    gg_theme_sci() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank()) +
    gg_rotate_x_text() +
    gg_delete_x_title() +
    gg_delete_y_title() +
    gg_delete_legend_title() +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_y_discrete(label = pattern.count)
}
