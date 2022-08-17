dictionary  <- function(key, language = NULL, defalut = NULL){

  dic <- list(
    min = list(en = "min", zh = "min2")
  )

  value <- dic[[key]][[language]]

  if(is_empty(value)){
    defalut
  }else{
    value
  }
}
