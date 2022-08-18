shapiro_wilk <- function(data, group = NULL, varnames = NULL, language = NULL, labels = NULL, table.number = NULL, digits = 3, ...){
  out <- normality_impl(data,
              group = group,
              varnames = varnames,
              func = shapiro_wilk_impl,
              digits = digits,
              language = language,
              labels = labels,
              key.stat = "norm.stat.sw")

  wrap_output(out,
              class = c("srp.norm", "srp"),
              title = "Shapiro-Wilk normality test",
              note  = "note",
              path  = "Shapiro wilk test",
              empty = "empty")
}


shapiro_wilk_impl <- function(x){
  x <- x[stats::complete.cases(x)]
  res <- stats::shapiro.test(x)
  data.frame(statistic = res$statistic[[1]],
             p.value = res$p.value[[1]])
}


normality_impl <- function(data, group = NULL, varnames = NULL, func = NULL, digits = 3, language = NULL, labels = NULL, key.stat, ...){

  group    <- select_variable(data, group)
  varnames <- select_numeric(data, varnames)
  varnames <- setdiff(varnames, group)

  language <- get_global_languange(language, default = "en")
  labels   <- get_global_labels(labels)

  if(length(varnames) == 0L){
    return(invisible(NULL))
  }

  names(varnames) <- varnames

  out <- lapply(varnames, \(x){
    group_exec(data, group = group, \(d){
      do_call(func, d[[x]], ...)
    }, na.error = TRUE)
  })

  out <- list_rbind(out, collapse.names = FALSE)

  out$statistic <- format_statistic(out$statistic, digits)
  out$p.value   <- format_pvalue(out$p.value,      digits)


  # Set labels
  if(!is.null(labels)){
    out <- add_lables(out, ldata = labels, col = 1)
    if(!is_empty(group)){
      group.labels <- sapply(group, \(x){
        l <- find_labels(data = tidy_labels(labels), varname = x)
        ifelse(is_empty(l), x, l)
      })
      names(out)[2:(2+length(group) - 1)] <- group.labels
    }
  }

  names(out)[which(names(out) == "variable")]  <- dictionary(key = "variable", language)
  names(out)[which(names(out) == "p.value")]   <- dictionary(key = "p.value",  language)
  names(out)[which(names(out) == "statistic")] <- dictionary(key = key.stat,   language)

  attr(out, "args") <- list(data = data, group = group, varnames = varnames)

  out
}
