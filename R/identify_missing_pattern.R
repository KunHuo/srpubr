identify_missing_pattern <- function (data, show.all = FALSE) {
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

  r
}
