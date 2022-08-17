extract_terms <- function(x, which = 1){
  if(is.character(which)){
    which(names(x) == which)
  }
  variables <- x[[which]]
  term <- vector(length = length(variables))
  varname <- vector(length = length(variables))
  code <- rep(NA, length(variables))
  for(i in seq_along(variables)){
    if(regex_detect(variables[i], pattern = "^\\s")){
      tmp <- regex_extract(rev(variables[1:i]), pattern = "^\\S*")
      tmp <- tmp[tmp != ""][1]
      term[i] <- paste0(tmp, trimws(variables[i]))
      code[i] <- trimws(variables[i])
      varname[i] <- tmp
    }else{
      term[i] <- variables[i]
      varname[i] <- variables[i]
    }
  }
  data.frame(term = term, code = code,  varname = varname)
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
  terms <- terms[, 3, drop = FALSE]
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
#' @param data a data frame.
#' @param ldata a data frame contain the column of term, code, and label.
#' @param col col index.
#'
#' @return a data frame.
#' @export
add_lables <- function(data, ldata, col = 1){
  ldata <- tidy_labels(ldata)
  tdata <- extract_terms(data, which = col)
  for(i in 1:nrow(data)){
    label <- find_labels(ldata, varname = tdata$term[i])
    if(!is_empty(label)){
      print(data[[col]][i])
      print("-------------------------------------------------")
      print(label)
      data[[col]][i] <- regex_replace(string = data[[col]][i],
                                      pattern = trimws(data[[col]][i]),
                                      replacement = label)
    }
  }
  data
}

tidy_labels <- function(data){
  data[, 1] <- lapply(data[, 1], function(v) {
    for (i in seq_along(v)) {
      if (i != 1) {
        if (is.na(v[i])) {
          v[i] <- v[i - 1]
        }
      }
    }
    v
  })
  names(data) <- c("term", "code", "label")
  data$term <- ifelse(is.na(data$code),
                            data$term,
                            paste(data$term, data$code, sep = ""))
  data
}


find_labels <- function(data, varname, code = NA){
  if(is_empty(code)){
    x <- varname
  }else{
    x <- paste(varname, code, sep = "")
  }
  data$label[which(data$term == x)]
}
