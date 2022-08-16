#' Set global languange
#'
#' @param language lauage
#'
#' @export
global_option_languange <- function(language = NULL){
  options(language = language)
}


get_global_languange <- function(language = NULL, default = "en"){
  if(is.null(language)){
    language <-  getOption("language", default = default)
  }
  language
}


#' Set global family
#'
#' @param family family
#'
#' @export
global_option_family <- function(family = NULL){
  options(family = family)
}


get_global_family <- function(family = NULL, default = "serif"){
  if(is.null(family)){
    family <-  getOption("family", default = default)
  }
  family
}

#' Set global font size
#'
#' @param font.size font.size
#'
#' @export
global_option_fontsize <- function(font.size = NULL){
  options(font.size = font.size)
}


get_global_fontsize <- function(font.size = NULL, default = 12){
  if(is.null(font.size)){
    font.size <- getOption("font.size", default = default)
  }
  font.size
}


#' Set global palette
#'
#' @param palette palette
#'
#' @export
global_option_palette <- function(palette = NULL){
  options(palette = palette)
}


#' Set global color
#'
#' @param color color
#'
#' @export
global_option_color <- function(color = NULL){
  options(palette = color)
}


get_global_palette <- function(palette = NULL, default = NULL){
  if(is.null(palette)){
    palette <-  getOption("palette", default = NULL)
  }
  palette
}



#' Set global labels
#'
#' @param labels a data frame contain the column of term, code, and label.
#'
#' @export
global_option_labels <- function(labels = NULL){
  options(labels = labels)
}


get_global_labels <- function(labels = NULL, default = NULL){
  if(is.null(labels)){
    labels <-  getOption("labels", default = NULL)
  }
  labels
}


#' Reset global options
#'
#' @export
global_option_reset <- function(){
  global_option_languange()
  global_option_fontsize()
  global_option_family()
  global_option_palette()
  global_option_labels()
}


#' @rdname global_option_reset
#'
#' @export
reset_global_option <- function(){
  global_option_languange()
  global_option_fontsize()
  global_option_family()
  global_option_palette()
  global_option_labels()
}
