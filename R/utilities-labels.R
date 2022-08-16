extract_terms <- function(x, which = 1){
  if(is.character(which)){
    which(names(x) == which)
  }
  variables <- x[[which]]
  term <- vector(length = length(variables))
  varname <- vector(length = length(variables))
  for(i in seq_along(variables)){
    if(regex_detect(variables[i], pattern = "^\\s")){
      tmp <- regex_extract(rev(variables[1:i]), pattern = "^\\S*")
      tmp <- tmp[tmp != ""][1]
      term[i] <-  paste0(tmp, trimws(variables[i]))
      varname[i] <- tmp
    }else{
      term[i] <- variables[i]
      varname[i] <- variables[i]
    }
  }
  data.frame(term = term, varname = varname)
}


add_terms_column <- function(x, which = 1){
  terms <- extract_terms(x, which = which)
  terms <- terms[, 1, drop = FALSE]
  x.class <- class(x)
  x.title <- attr(x, "title")
  x.note <- attr(x, "note")
  x <- cbind(terms, x)
  class(x) <- x.class
  attr(x, "title") <- x.title
  attr(x, "note") <- x.note
  x
}


add_varnames_column <- function(x, which = 1){
  terms <- extract_terms(x, which = which)
  terms <- terms[, 2, drop = FALSE]
  x.class <- class(x)
  x.title <- attr(x, "title")
  x.note <- attr(x, "note")
  x <- cbind(terms, x)
  class(x) <- x.class
  attr(x, "title") <- x.title
  attr(x, "note") <- x.note
  x
}


#' Add title attribute to a data frame
#'
#' @param x a data frame.
#' @param value a character string.
#'
#' @return a data frame.
#' @export
add_title <- function(x, value = NULL){
  attr(x, "title") <- value
  x
}



#' Add note attribute to a data frame
#'
#' @param x a data frame.
#' @param value a character string.
#' @param append a logical.
#'
#' @return a data frame.
#' @export
add_note <- function(x, value = NULL, append = TRUE){
  if(is_empty(value)){
    attr(x, "note") <- NULL
  }else{
    if(append){
      note <- attr(x, "note")
      if(is_empty(note)){
        attr(x, "note") <- value
      }else{
        attr(x, "note") <- paste(note, value, sep = "\n")
      }
    }else{
      attr(x, "note") <- value
    }
  }
  x
}


#' Add labels to  the first column of a data frame
#'
#' @param x a data frame.
#' @param value a data frame contain the column of term, code, and label.
#'
#' @return a data frame.
#' @export
add_lables <- function(x, value){
  value[, 1] <- lapply(value[, 1], function(v) {
    for (i in seq_along(v)) {
      if (i != 1) {
        if (is.na(v[i])) {
          v[i] <- v[i - 1]
        }
      }
    }
    v
  })

  terms <- extract_terms(x)
  for(i in 1:nrow(x)){
    term <- terms[i, 1]
    if(term %in% value[[1]]){
      index <- which(term == value[[1]])
      code  <- value[index, 2, drop = TRUE]
      label <- value[index, 3, drop = TRUE]

      if(!is_empty(label)){
        if(is_empty(code)){
          x[i, 1] <- label
        }else{
          x[i, 1] <- regex_replace(string = x[i, 1, drop = TRUE],
                                   pattern = code,
                                   replacement = label)
        }
      }
    }
  }
  x
}
