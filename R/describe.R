desc_numeric <- function(data, group = NULL, varnames = NULL, show.overall = FALSE, methods = c("mean"), digits = 1){

  excute <- function(varname){
    if(is.null(group)){
      res <- excute_method(data[[varname]], methods = methods, digits = digits)
    }else{
      res <- excute_method(data[[varname]], g = data[[group]], methods = methods, digits = digits)
      if(show.overall){
        overall <- excute_method(data[[varname]], methods = methods, digits = digits)
        res <- cbind(overall, res[-1])
      }
    }

    if(nrow(res) == 1L){
      res$term <- varname
    }else{
      res$term <- paste0(varname, res$term)
    }
    res
  }
  output <- lapply(varnames, excute)
  output <- do.call(rbind, output)
  output
}


excute_method <- function(x, g = NULL, methods = "mean", digits = 1){
  if(is.null(g)){
    g <- rep("overall", length(x))
  }

  excute <- function(method){
    if(method %in% c("mean", "sd", "median", "IQR", "min", "max")){
      res <- tapply(x, g, method, na.rm = TRUE)
    }else{
      args <- methods::formalArgs(method)
      if("digits" %in% args){
        res <- tapply(x, g, method, digits = digits)
      }else{
        res <- tapply(x, g, method)
      }
    }
    if(is.numeric(res)){
      names <- names(res)
      fmt <- sprintf("%%.%df", digits)
      res <- sprintf(fmt, res)
      names(res) <- names
      res
    }
    res <- as.matrix(res)
    res <- t(res)
    res <- as.data.frame(res, stringsAsFactors = FALSE)
    cbind(data.frame(term = method, stringsAsFactors = FALSE), res)
  }

  if (any(is.na(x))) {
    methods <- c(methods, "miss")
  }

  output <- lapply(methods, excute)
  output <- do.call(rbind, output)
  output
}


skew <- function (x) {
  x <- x[!is.na(x)]
  n <- length(x)
  (sum((x - mean(x)) ^ 3) / n) / (sum((x - mean(x)) ^ 2) / n) ^ (3 / 2)
}


kurt <- function (x) {
  x <- x[!is.na(x)]
  n <- length(x)
  n * sum((x - mean(x)) ^ 4) / (sum((x - mean(x)) ^ 2) ^ 2)
}


n <- function(x) {
  as.character(length(x))
}


miss  <- function(x) {
  as.character(sum(is.na(x)))
}


se <- function(x) {
  x <- x[!is.na(x)]
  stats::sd(x) / sqrt(length(x))
}


mean_ci <- function(x, conf = 0.95) {
  se <- stats::sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * stats::qnorm(c(alpha / 2, 1 - alpha / 2))
}


mean_sd <- function(x, digits = 2) {
  m <- format_digits(mean(x, na.rm = TRUE), digits)
  s <- format_digits(stats::sd(x, na.rm = TRUE), digits)
  sprintf("%s\u00b1%s",m, s)
}


mean_sd2 <- function(x, digits = 2) {
  m <- format_digits(mean(x, na.rm = TRUE), digits)
  s <- format_digits(stats::sd(x, na.rm = TRUE), digits)
  sprintf("%s (%s)",m, s)
}


mean_sd3 <- function(x, digits = 2) {
  m <- format_digits(mean(x, na.rm = TRUE), digits)
  s <- format_digits(stats::sd(x, na.rm = TRUE), digits)
  sprintf("%s (\u00b1%s)",m, s)
}


median_IQR <- function(x, digits = 2){
  m <- format_digits(stats::median(x, na.rm = TRUE), digits)
  p25 <- format_digits(stats::quantile(x, probs = 0.25, na.rm = TRUE), digits)
  p75 <- format_digits(stats::quantile(x, probs = 0.75, na.rm = TRUE), digits)
  sprintf("%s (%s, %s)", m, p25, p75)
}


median_IQR2 <- function(x, digits = 2){
  m <- format_digits(stats::median(x, na.rm = TRUE), digits)
  p25 <- format_digits(stats::quantile(x, probs = 0.25, na.rm = TRUE), digits)
  p75 <- format_digits(stats::quantile(x, probs = 0.75, na.rm = TRUE), digits)
  sprintf("%s (%s to %s)", m, p25, p75)
}


median_IQR3 <- function(x, digits = 2){
  m <- format_digits(stats::median(x, na.rm = TRUE), digits)
  p25 <- format_digits(stats::quantile(x, probs = 0.25, na.rm = TRUE), digits)
  p75 <- format_digits(stats::quantile(x, probs = 0.75, na.rm = TRUE), digits)
  sprintf("%s (%s - %s)", m, p25, p75)
}


median_IQR4 <- function(x, digits = 2){
  m <- format_digits(stats::median(x, na.rm = TRUE), digits)
  IQR <- format_digits(stats::IQR(x, na.rm = TRUE), digits)
  sprintf("%s (%s)", m, IQR)
}


median_range <- function(x, digits = 2){
  m <- format_digits(stats::median(x, na.rm = TRUE), digits)
  min <- format_digits(min(x, na.rm = TRUE), digits)
  max <- format_digits(max(x, na.rm = TRUE), digits)
  sprintf("%s (%s, %s)", m, min, max)
}


median_range2 <- function(x, digits = 2){
  m <- format_digits(stats::median(x, na.rm = TRUE), digits)
  min <- format_digits(min(x, na.rm = TRUE), digits)
  max <- format_digits(max(x, na.rm = TRUE), digits)
  sprintf("%s (%s to %s)", m, min, max)
}


median_range3 <- function(x, digits = 2){
  m <- format_digits(stats::median(x, na.rm = TRUE), digits)
  min <- format_digits(min(x, na.rm = TRUE), digits)
  max <- format_digits(max(x, na.rm = TRUE), digits)
  sprintf("%s (%s - %s)", m, min, max)
}



desc_category <- function(data, group = NULL, varnames = NULL, show.overall = FALSE, method = "nprop", digits = 1) {
  excute <- function(varname){
    if(is.null(group)){
      res <- do.call(method, args = list(x = data[[varname]], digits = digits))
    }else{
      res <- do.call(method, args = list(x = data[[varname]], g = data[[group]], digits = digits))
      if(show.overall){
        overall <- do.call(method, args = list(x = data[[varname]], digits = digits))
        res <- cbind(overall, res[-1])
      }
    }
    res$term <- paste0(varname, res$term)
    res
  }

  output <- lapply(varnames, excute)
  output <- do.call(rbind, output)
  output
}


prop <- function(x, g = NULL, type = 1, digits = 1){
  if(is.null(g)){
    g <- rep("overall", length(x))
  }
  freqs <- table(x, g)
  freqs <- prop.table(freqs, 2) * 100
  ncol  <- ncol(freqs)
  rname <- row.names(freqs)
  cname <- colnames(freqs)
  freqs <- as.vector(freqs)

  if(!is.null(digits)){
    fmt <- ifelse(type == 1,
                  sprintf("%%.%df%%%%", digits),
                  sprintf("%%.%df", digits))
    freqs <- sprintf(fmt, freqs)
  }
  freqs <- matrix(freqs, ncol = ncol)
  colnames(freqs)  <- cname
  freqs <- as.data.frame(freqs, stringsAsFactors = FALSE)
  cbind(data.frame(term = rname, stringsAsFactors = FALSE), freqs)
}


prop2 <- function(x, g = NULL, digits = 1){
  prop(x, g, type = 2, digits = digits)
}


ratio <- function(x, g = NULL, type = 1, digits = 1){
  if(is.null(g)){
    g <- rep("overall", length(x))
  }
  freqs <- table(x, g)
  freqs <- prop.table(freqs, 1) * 100
  ncol  <- ncol(freqs)
  rname <- row.names(freqs)
  cname <- colnames(freqs)
  freqs <- as.vector(freqs)

  if(!is.null(digits)){
    fmt <- ifelse(type == 1,
                  sprintf("%%.%df%%%%", digits),
                  sprintf("%%.%df", digits))
    freqs <- sprintf(fmt, freqs)
  }
  freqs <- matrix(freqs, ncol = ncol)
  colnames(freqs)  <- cname
  freqs <- as.data.frame(freqs, stringsAsFactors = FALSE)
  cbind(data.frame(term = rname, stringsAsFactors = FALSE), freqs)
}


ratio2 <- function(x, g = NULL, digits = 1){
  ratio(x, g, type = 2, digits = digits)
}


freq <- function(x, g = NULL, type = 1, digits = NULL){
  if(is.null(g)){
    g <- rep("overall", length(x))
  }
  freqs <- table(x, g)
  ncol  <- ncol(freqs)
  rname <- row.names(freqs)
  cname <- colnames(freqs)
  freqs <- as.vector(freqs)
  if(is.null(digits)){
    freqs <- sprintf("%d", freqs)
  }
  freqs <- matrix(freqs, ncol = ncol)
  colnames(freqs)  <- cname
  freqs <- as.data.frame(freqs, stringsAsFactors = FALSE)
  cbind(data.frame(term = rname, stringsAsFactors = FALSE), freqs)
}


nprop <- function(x, g = NULL, type = 1, digits = 1){
  if(is.null(g)){
    g <- rep("overall", length(x))
  }
  freqs <- table(x, g)
  freqs <- table(x, g)
  ncol  <- ncol(freqs)
  rname <- row.names(freqs)
  cname <- colnames(freqs)
  props <- prop.table(freqs, 2) * 100

  fmt <- paste0("%d (%.", digits, "f%%)")
  if(type != 1L){
    fmt <- paste0("%d (%.", digits, "f)")
  }
  output <- sprintf(fmt, as.vector(freqs), props)
  output <- matrix(output, ncol = ncol)
  colnames(output)  <- cname
  output <- as.data.frame(output, stringsAsFactors = FALSE)
  cbind(data.frame(term = rname, stringsAsFactors = FALSE), output)
}


nprop2 <- function(x, g = NULL, digits = 1){
  nprop(x, g, type = 2, digits = digits)
}


nratio <- function(x, g = NULL, type = 1, digits = 1){
  if(is.null(g)){
    g <- rep("overall", length(x))
  }
  freqs <- table(x, g)
  freqs <- table(x, g)
  ncol  <- ncol(freqs)
  rname <- row.names(freqs)
  cname <- colnames(freqs)
  props <- prop.table(freqs, 1) * 100

  fmt <- paste0("%d (%.", digits, "f%%)")
  if(type != 1L){
    fmt <- paste0("%d (%.", digits, "f)")
  }
  output <- sprintf(fmt, as.vector(freqs), props)
  output <- matrix(output, ncol = ncol)
  colnames(output)  <- cname
  output <- as.data.frame(output, stringsAsFactors = FALSE)
  cbind(data.frame(term = rname, stringsAsFactors = FALSE), output)
}


nratio2 <- function(x, g = NULL, digits = 1){
  nratio(x, g, type = 2, digits = digits)
}


describe_variable <- function(df,
                              group = NULL,
                              varnames.normal = NULL,
                              varnames.nonnormal = NULL,
                              varnames.category = NULL,
                              method.normal = "mean_sd",
                              method.nonnormal = "median_IQR",
                              method.category = "nprop",
                              digits.numeric = 2,
                              digits.category = 1,
                              show.overall = FALSE){
  out <- NULL

  if(!is.null(varnames.normal)){
    out <- desc_numeric(df,
                        group = group,
                        varnames = varnames.normal,
                        show.overall = show.overall,
                        methods = method.normal,
                        digits = digits.numeric)
  }

  if(!is.null(varnames.nonnormal)){
    res <- desc_numeric(df,
                        group = group,
                        varnames = varnames.nonnormal,
                        show.overall = show.overall,
                        methods = method.nonnormal,
                        digits = digits.numeric)
    out <- rbind(out, res)
  }

  if(!is.null(varnames.category)){
    res <- desc_category(df,
                         group = group,
                         varnames = varnames.category,
                         show.overall = show.overall,
                         method = method.category,
                         digits = digits.category)
    out <- rbind(out, res)
  }
  out
}


