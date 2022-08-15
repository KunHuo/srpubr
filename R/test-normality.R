shapiro_wilk <- function(data, group = NULL, varnames = NULL, digits = 3, ...){
  normal_impl(df,
              group = group,
              varnames = varnames,
              func = execute_sw,
              digits = digits,
              label.stat = "W value",
              table.title = "Shapiro-Wilk normality test",
              table.note = "")
}


execute_sw <- function(x){
  x <- x[stats::complete.cases(x)]
  res <- stats::shapiro.test(x)
  data.frame(n = length(x),
             statistic = res$statistic[[1]],
             p.value = res$p.value[[1]],
             stringsAsFactors = FALSE)
}

.normality_impl <- function(data, group = NULL, varnames = NULL, func = NULL, digits = 3, ...){

  group    <- select_variable(data, group)
  varnames <- select_numeric(data, varnames)
  varnames <- setdiff(varnames, group)

  if(length(varnames) == 0L){
    return(invisible(NULL))
  }

  names(varnames) <- varnames

  lapply(varnames, \(x){
    group_exec(data, group = group, \(d){
      do_call(func, d[[x]], ...)
    })
  })
}

