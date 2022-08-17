dictionary  <- function(key, language = NULL, defalut = NULL){

  dic <- list(
    min      = list(en = "Min",       zh = "\u6700\u5c0f\u503c"),
    variable = list(en = "Variable",  zh = "\u53d8\u91cf"),
    p.value  = list(en = "P value",   zh = "P\u503c")
  )

  value <- dic[[key]][[language]]

  if(is_empty(value)){
    defalut
  }else{
    value
  }
}
