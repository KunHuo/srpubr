#' Normality test method
#'
#' @param data a data frame.
#' @param varnames numeric variable names.
#' @param group group variable name.
#' @param language language language, typically “en”, or "zh", default "en".
#' @param labels a data frame contain the column of term, code, and label.
#' @param table.number table number.
#' @param digits digits for p value.
#' @param ... unused.
#'
#' @return a data frame with class of 'srp.norm'
#' @export
shapiro_wilk <- function(data, group = NULL, varnames = NULL, language = NULL, labels = NULL, table.number = NULL, digits = 3, ...){
  language <- get_global_languange(language, default = "en")
  out <- normality_impl(data,
              group = group,
              varnames = varnames,
              func = shapiro_wilk_impl,
              digits = digits,
              language = language,
              labels = labels,
              key.stat = "norm.stat.sw")
  title <- switch_string(language = language,
                         english = "Shapiro-Wilk normality test",
                         chinese = "Shapiro-Wilk\u6b63\u6001\u6027\u68c0\u9a8c",
                         number  = table.number)
  wrap_output(out,
              class = c("srp.norm", "srp"),
              title = title,
              note  = NULL,
              short = "Shapiro-Wilk",
              empty = "No suitable numeric variable for calculation.")
}


shapiro_wilk_impl <- function(x, name, ...){

  x <- x[stats::complete.cases(x)]

  tryCatch(
    expr = {
      res <- stats::shapiro.test(x)
      data.frame(statistic = res$statistic[[1]], p.value = res$p.value[[1]])
    }, error = function(e){
      cat(name, ": ")
      print(e)
      data.frame(statistic = -1, p.value = -1)
    }
  )
}


normality_impl <- function(data, group = NULL, varnames = NULL, func = NULL, digits = 3, language = NULL, labels = NULL, key.stat, ...){

  group    <- select_variable(data, group)
  varnames <- select_numeric(data, varnames)
  varnames <- setdiff(varnames, group)

  language <- get_global_languange(language, default = "en")
  labels   <- get_global_labels(labels)

  if(length(varnames) == 0L){
    return(NULL)
  }

  names(varnames) <- varnames

  out <- lapply(varnames, \(x){
    group_exec(data, group = group, \(d){
      do_call(func, d[[x]], name = x, ...)
    }, labels = labels)
  })

  if(all(sapply(out, is_empty))){
    return(NULL)
  }

  collapse.names <- ifelse(length(group) == 1L, TRUE, FALSE)
  dup.var <- !collapse.names
  out <- list_rbind(out, collapse.names = collapse.names, labels = labels, dup.var = dup.var)

  out$statistic <- format_statistic(out$statistic, digits)
  out$p.value   <- format_pvalue(out$p.value,      digits)

  names(out)[which(names(out) == "variable")]  <- dictionary(key = "variable", language)
  names(out)[which(names(out) == "p.value")]   <- dictionary(key = "p.value",  language)
  names(out)[which(names(out) == "statistic")] <- dictionary(key = key.stat,   language)

  attr(out, "args") <- list(data = data,
                            group = group,
                            varnames = varnames)

  out
}


#' @rdname shapiro_wilk
#' @export
ks_test <- function(data, group = NULL, varnames = NULL, language = NULL, labels = NULL, table.number = NULL, digits = 3, ...){

  language <- get_global_languange(language, default = "en")

  out <- normality_impl(data,
                        group = group,
                        varnames = varnames,
                        func = ks_impl,
                        digits = digits,
                        language = language,
                        labels = labels,
                        key.stat = "norm.stat.sw")
  title <- switch_string(language = language,
                         english = "Kolmogorov-Smirnov normality test",
                         chinese = "Kolmogorov-Smirnov\u6b63\u6001\u6027\u68c0\u9a8c",
                         number  = table.number)
  wrap_output(out,
              class = c("srp.norm", "srp"),
              title = title,
              note  = NULL,
              short = "Kolmogorov-Smirnov",
              empty = "No suitable numeric variable for calculation.")
}


#' @rdname shapiro_wilk
#' @export
kolmogorov_smirnov <- ks_test



ks_impl <- function(x, name, ...){
  x <- x[stats::complete.cases(x)]
  tryCatch(
    expr = {
      res <- execute_ks(x)
      data.frame(statistic = res$statistic[[1]], p.value = res$p.value[[1]])
    }, error = function(e){
      cat(name, ": ")
      print(e)
      data.frame(statistic = -1, p.value = -1)
    }
  )
}

execute_ks <- function (x){
  x <- sort(x[stats::complete.cases(x)])
  n <- length(x)
  if (n < 5)
    stop("sample size must be greater than 4")
  p <- stats::pnorm((x - mean(x)) / stats::sd(x))
  Dplus <- max(seq(1:n) / n - p)
  Dminus <- max(p - (seq(1:n) - 1) / n)
  K <- max(Dplus, Dminus)
  if (n <= 100) {
    Kd <- K
    nd <- n
  }
  else {
    Kd <- K * ((n / 100) ^ 0.49)
    nd <- 100
  }
  pvalue <- exp(
    -7.01256 * Kd ^ 2 * (nd + 2.78019) + 2.99587 *
      Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598 / sqrt(nd) +
      1.67997 / nd
  )
  if (pvalue > 0.1) {
    KK <- (sqrt(n) - 0.01 + 0.85 / sqrt(n)) * K
    if (KK <= 0.302) {
      pvalue <- 1
    }
    else if (KK <= 0.5) {
      pvalue <- 2.76773 - 19.828315 * KK + 80.709644 *
        KK ^ 2 - 138.55152 * KK ^ 3 + 81.218052 * KK ^ 4
    }
    else if (KK <= 0.9) {
      pvalue <- -4.901232 + 40.662806 * KK - 97.490286 *
        KK ^ 2 + 94.029866 * KK ^ 3 - 32.355711 * KK ^ 4
    }
    else if (KK <= 1.31) {
      pvalue <- 6.198765 - 19.558097 * KK + 23.186922 *
        KK ^ 2 - 12.234627 * KK ^ 3 + 2.423045 * KK ^ 4
    }
    else {
      pvalue <- 0
    }
  }
  data.frame(statistic = K, p.value = pvalue, stringsAsFactors = FALSE)
}


#' @rdname shapiro_wilk
#' @export
shapiro_francia <- function(data, group = NULL, varnames = NULL, language = NULL, labels = NULL, table.number = NULL, digits = 3, ...){

  language <- get_global_languange(language, default = "en")

  out <- normality_impl(data,
                        group = group,
                        varnames = varnames,
                        func = sf_impl,
                        digits = digits,
                        language = language,
                        labels = labels,
                        key.stat = "norm.stat.sw")
  title <- switch_string(language = language,
                         english = "Shapiro-Francia normality test",
                         chinese = "Shapiro-Francia\u6b63\u6001\u6027\u68c0\u9a8c",
                         number  = table.number)
  wrap_output(out,
              class = c("srp.norm", "srp"),
              title = title,
              note  = NULL,
              short = "Shapiro-Francia",
              empty = "No suitable numeric variable for calculation.")
}


sf_impl <- function(x, name, ...){
  x <- x[stats::complete.cases(x)]
  tryCatch(
    expr = {
      res <- execute_sf(x)
      data.frame(statistic = res$statistic[[1]], p.value = res$p.value[[1]])
    }, error = function(e){
      cat(name, ": ")
      print(e)
      data.frame(statistic = -1, p.value = -1)
    }
  )
}


execute_sf <- function (x, n.classes = ceiling(2 * (n^(2/5))), adjust = TRUE) {
  x <- sort(x[stats::complete.cases(x)])
  n <- length(x)
  if ((n < 5 || n > 5000))
    stop("sample size must be between 5 and 5000")
  y <- stats::qnorm(stats::ppoints(n, a = 3/8))
  W <- stats::cor(x, y)^2
  u <- log(n)
  v <- log(u)
  mu <- -1.2725 + 1.0521 * (v - u)
  sig <- 1.0308 - 0.26758 * (v + 2/u)
  z <- (log(1 - W) - mu)/sig
  pval <- stats::pnorm(z, lower.tail = FALSE)

  data.frame(statistic = W, p.value = pval, stringsAsFactors = FALSE)
}
