shapiro_wilk <- function(data, group = NULL, varnames = NULL, digits = 3, ...){
  normality_impl(data,
              group = group,
              varnames = varnames,
              func = shapiro_wilk_impl,
              digits = digits)
}


shapiro_wilk_impl <- function(x){
  x <- x[stats::complete.cases(x)]
  res <- stats::shapiro.test(x)
  data.frame(n = length(x),
             statistic = res$statistic[[1]],
             p.value = res$p.value[[1]],
             stringsAsFactors = FALSE)
}


normality_impl <- function(data, group = NULL, varnames = NULL, func = NULL, digits = 3, ...){

  group    <- select_variable(data, group)
  varnames <- select_numeric(data, varnames)
  varnames <- setdiff(varnames, group)

  if(length(varnames) == 0L){
    return(invisible(NULL))
  }

  names(varnames) <- varnames

  out <- lapply(varnames, \(x){
    group_exec(data, group = group, \(d){
      do_call(func, d[[x]], ...)
    })
  })

  out <- list_rbind(out, collapse.names = TRUE)

  out$statistic <- format_statistic(out$statistic, digits)
  out$p.value   <- format_pvalue(out$p.value,      digits)

  out
}
