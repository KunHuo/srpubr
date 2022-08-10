#' Append elements to objects
#'
#'
#' Append elements to a number of various objects as vectors, matrices, data.frames
#' and lists. In a matrix either rows or columns can be inserted at any position.
#' In data frames any vectors can be inserted. values will be recycled to the
#' necessary length.
#'
#'
#' @param x
#' object for the elements to be inserted.
#' @param values
#' the elements to be inserted.
#' @param after
#' a subscript, after which the values are to be appended. If it's missing the
#' values will be appended after the last element (or column/row).
#' @param ...
#' further arguments (not used here).
#'
#' @details
#' The vector x will be recycled to a length of the next multiple of the number
#' of rows (or columns) of the matrix m and will be inserted such that the first
#' inserted row (column) has the index i. If the dimnames are given, they will
#' be used no matter if the matrix m has already dimnames defined or not.
#'
#' @return
#' An object containing the values in x with the elements of values appended
#' after the specified element of x.
#' @seealso [rbind], [cbind], [append]
#' @export
#'
#' @examples
#' # the same as append
#' append2(1:5, 0:1, after = 3)
#'
#' # Insert columns and rows
#' x <- matrix(runif(25), 5)
#'
#' append2(x, values=1:10, after=2, names = c("X","Y"))
#' append2(x, values=1:10, after=2)
#'
#' # append to a data.frame
#' d.frm <- data.frame("id"   = c(1,2,3),
#'                     "code" = c("AAA", "BBB", "CCC"),
#'                     "val"  = c(111, 222, 333))
#' z <- c(10, 20, 30)
#'
#' append2(d.frm, z, after=2, names="ZZZ")
append2 <- function(x, values, after = NULL, ...){
  UseMethod("append2")
}


#' @rdname append2
#' @export
append2.default <- function(x, values, after = NULL, ...){
  if(is.character(after)){
    after <- which(names(x) == after)
  }
  if (is.null(after)) {
    after <- length(x)
  }
  append(x, values, after)
}

#' @rdname append2
#'
#' @param rows
#' logical, defining if vector should be added as row or as column. Default is
#' column (rows=FALSE).
#' @param names
#' the dimension names for the inserted elements(s).
#'
#' @export
append2.matrix <- function (x, values, after = NULL, rows = FALSE, names = NULL, ...) {
  if(is.character(after)){
    after <- which(names(x) == after)
  }
  if (rows) {
    nr <- dim(x)[1]
    if (is.null(after))
      after <- nr
    values <- matrix(values, ncol = ncol(x))
    if (!is.null(names)) {
      err <- try(row.names(x) <- names, silent = TRUE)
      if (class(err) == "try-error")
        warning("Could not set rownames.")
    }
    if (!after)
      res <- rbind(values, x)
    else if (after >= nr)
      res <- rbind(x, values)
    else res <- rbind(x[1L:after, , drop = FALSE], values,
                      x[(after + 1L):nr, , drop = FALSE])
    colnames(res) <- colnames(x)
  }
  else {
    nc <- dim(x)[2]
    if (missing(after))
      after <- nc
    values <- matrix(values, nrow = nrow(x))
    if (!is.null(names))
      colnames(values) <- names
    if (!after)
      res <- cbind(values, x)
    else if (after >= nc)
      res <- cbind(x, values)
    else res <- cbind(x[, 1L:after, drop = FALSE], values,
                      x[, (after + 1L):nc, drop = FALSE])
    rownames(res) <- rownames(x)
  }
  return(res)
}


#' @rdname append2
#' @export
append2.data.frame <- function (x, values, after = NULL, rows = FALSE, names = NULL, ...) {

  if(is.character(after)){
    after <- which(names(x) == after)
  }

  .InsertRow <- function(x, val, after = nrow(x)) {
    x[seq(after + 1, nrow(x) + 1), ] <- x[seq(after, nrow(x)), ]
    x[after, ] <- val
    x
  }
  if (rows) {
    .InsertRow(x, values, after = after)
  }else {
    as.data.frame(append(x, set_names(list(values), names = names), after = after))
  }
}


#' Dummy codes for a factor
#'
#' Generate a matrix of dummy codes (class indicators) for a given factor.
#'
#' @param x
#' factor or vector of classes for cases.
#' @param method
#' efines the method of the contrasts being formed. Can be one out of "treatment",
#' "sum", "helmert", "poly", "full", whereas "treatment" is the default one.
#' Abbreviations are accepted. The option "full" returns a full set of class
#' indicators, say a dummy factor for each level of x. Note that this would be
#' redundant for lm() and friends.
#' @param base
#' an integer specifying which group is considered the baseline group.
#' @param levels
#' an optional vector of the values (as character strings) that x might have taken.
#' The default is the unique set of values taken by as.character(x), sorted into
#' increasing order of x. This is directly passed on to factor. Not used when the
#' x is a data frame.
#' @param ... further arguments, no used here.
#'
#' @return
#' a matrix with the dummy codes. The number of rows correspond to the number of
#' elements in x and the number of columns to the number of its levels - 1,
#' respectively to the number of levels given as argument -1. When method = "full"
#' is chosen the number of columns will correspond to the number of levels.
#'
#' @references
#' Venables, W N and Ripley, B D (2002): Modern Applied Statistics with S. Fourth edition. Springer.
#' @seealso [model.frame], [contrasts]
#' @export
#'
#' @examples
#' x <- c("red","blue","green","blue","green","red","red","blue")
#' dummy(x)
#' dummy(x, base=2)
#'
#' dummy(x, method="sum")
#'
#'
#' y <- c("Max","Max","Max","Max","Max","Bill","Bill","Bill")
#'
#' dummy(y)
#' dummy(y, base="Max")
#'
#' dummy(y, base="Max", method="full")
#'
#'
#' # "Undummy" (revert the dummy coding)
#' m <- dummy(y, method="full")
#' m
#' z <- apply(m, 1, function(x) colnames(m)[x==1])
#' z
#' identical(y, as.vector(z))
#'
#' m <- dummy(y)
#' m
#' z <- apply(m, 1, function(x) ifelse(sum(x)==0, attr(m,"base"), colnames(m)[x==1]))
#' z
dummy <- function(x,
                  method = c("treatment", "sum", "helmert", "poly", "full"),
                  base = 1,
                  levels = NULL,
                  ...){
  UseMethod("dummy")
}


#' @rdname dummy
#' @export
dummy.default <- function (x,
                           method = c("treatment", "sum", "helmert", "poly", "full"),
                           base = 1,
                           levels = NULL,
                           ...) {

  if (is.null(levels))
    x <- factor(x)
  else
    x <- factor(x, levels = levels)
  if (!is.numeric(base))
    base <- match(base, levels(x))
  method <- match.arg(arg = method,
                      choices = c("treatment", "sum", "helmert", "poly", "full"))
  switch(
    method,
    treatment = {
      res <- stats::contr.treatment(n = nlevels(x), base = base)[x,, drop = FALSE]
    },
    sum = {
      res <- stats::contr.sum(n = nlevels(x))[x, , drop = FALSE]
    },
    helmert = {
      res <- stats::contr.helmert(n = nlevels(x))[x, , drop = FALSE]
    },
    poly = {
      res <- stats::contr.poly(n = nlevels(x))[x, , drop = FALSE]
    },
    full = {
      res <- diag(nlevels(x))[x, , drop = FALSE]
    }
  )
  res <- as.matrix(res)
  if (method == "full") {
    dimnames(res) <-
      list(if (is.null(names(x)))
        1L:length(x)
        else
          names(x),
        levels(x))
    attr(res, "base") <- NA
  }
  else {
    dimnames(res) <-
      list(if (is.null(names(x)))
        1L:length(x)
        else
          names(x),
        levels(x)[-base])
    attr(res, "base") <- levels(x)[base]
  }
  return(res)
}


#' @rdname dummy
#' @param varnames
#' Specify the name of the variable that needs to be processed
#' with dummy variables, if not specified, all factor variables and character
#' variables will be processed. Only used when the x is a data frame.
#' @param keep
#' Whether to keep the original column, the default is FALSE. Only used when the
#' x is a data frame.
#' @param sep a character string to separate the terms.
#'
#' @export
#'
#' @examples
#' dummy(iris)
#' dummy(iris, keep = TRUE)
#'
#' dummy(mtcars, varnames = c("vs", "am"))
#' dummy(mtcars, varnames = c("vs", "am"), sep = ".")
dummy.data.frame <- function(x,
                             method = c("treatment", "sum", "helmert", "poly", "full"),
                             base = 1,
                             levels = NULL,
                             varnames = NULL,
                             keep = FALSE, sep = "_", ...) {

  method <- match.arg(method)

  if(is.null(varnames)){
    varnames <- names(x)[sapply(x, function(i) is.factor(i) | is.character(i))]
  }else{
    check_name(x, varnames)
  }

  if(length(varnames) == 0L){
    return(x)
  }

  for(i in seq_along(varnames)){
    res <- dummy(x[[varnames[i]]], method = method, base = base)
    res <- as.data.frame(res)
    names(res) <- paste(varnames[i], names(res), sep = sep)
    x <- append2(x, res, after = varnames[i])
    if(!keep){
      x[which(names(x) == varnames[i])] <- NULL
    }
  }
  x
}

#' Set the names in an object
#'
#'
#' This is a convenience function that sets the names of an object and returns
#' it including the new names. It is most useful at the end of a function
#' definition where one is creating the object to be returned and would prefer
#' not to store it under a name just that the names can be assigned. In addition
#' to the function setNames in base R the user can decide, whether rownames,
#' colnames or simply the names are to be set. Names are recyled.
#'
#'
#' @param x
#' an object for which a names attribute will be meaningful
#' @param ...
#' the names to be assigned to the object. This should be a character vector of
#' names named dimnames, rownames, colnames or names. Setting rownames=NULL would
#' remove existing rownames. All kind of names can be changed at the same time.
#' Default would be names. Abbreviations are supported.
#'
#' @return
#' An object of the same sort as object with the new names assigned.
#' @seealso [setNames]
#' @export
#'
#' @examples
#' set_names(1:5, names=letters[1:5])
#'
#' # the default, if no argument names are provided, is "names"
#'
#' set_names(1:5, letters[1:5])
set_names <- function (x, ...) {
  args <- list(...)
  if (is.null(names(args)))
    names(args) <- "names"
  names(args) <- lapply(names(args), match.arg, c("names",
                                                  "rownames", "colnames", "dimnames"))
  if ("dimnames" %in% names(args)) {
    if (is.null(args[["dimnames"]]))
      dimnames(x) <- NULL
    else dimnames(x) <- args[["dimnames"]]
  }
  if ("rownames" %in% names(args)) {
    if (is.null(args[["rownames"]]))
      rownames(x) <- NULL
    else rownames(x) <- rep_len(args[["rownames"]],
                                dim(x)[1])
  }
  if ("colnames" %in% names(args)) {
    if (is.null(args[["colnames"]]))
      colnames(x) <- NULL
    else colnames(x) <- rep_len(args[["colnames"]],
                                dim(x)[2])
  }
  if ("names" %in% names(args)) {
    if (is.null(args[["names"]]))
      names(x) <- NULL
    else names(x) <- rep_len(args[["names"]], length(x))
  }
  x
}


#' Change names of a named object
#'
#' Rename changes the names of a named object.
#'
#' @param x
#' Any named object
#' @param ...
#' A sequence of named arguments, all of type character.
#' @param gsub
#' a logical value; if TRUE, [gsub] is used to change the row and column labels of
#' the resulting table. That is, instead of substituting whole names, substrings
#' of the names of the object can changed.
#' @param fixed
#' a logical value, passed to [gsub]. If TRUE, substitutions are by fixed strings
#' and not by regular expressions.
#' @param warn
#' a logical value; should a warning be issued if those names to change are not found?
#'
#' @details
#' This function changes the names of x according to the remaining arguments.
#' If gsub is FALSE, argument tags are the old names, the values are the new names.
#' If gsub is TRUE, arguments are substrings of the names that are substituted
#' by the argument values.
#'
#' @return
#' The object x with new names defined by the ... arguments.
#' @export
#'
#' @examples
#' x <- c(a=1, b=2)
#' rename(x, a="A", b="B")
#'
#' str(rename( iris,
#'             Sepal.Length="Sepal_Length",
#'             Sepal.Width ="Sepal_Width",
#'             Petal.Length="Petal_Length",
#'             Petal.Width ="Petal_Width"
#' ))
#'
#' str(rename(iris, .="_", gsub=TRUE))
rename <- function (x, ..., gsub = FALSE, fixed = TRUE, warn = TRUE) {
  subst <- c(...)
  if (is.null(names(subst)))
    names(x)[1:length(subst)] <- subst
  if (gsub) {
    names.x <- names(x)
    for (i in 1:length(subst)) {
      names.x <- gsub(names(subst[i]), subst[i], names.x, fixed = fixed)
    }
    names(x) <- names.x
  }
  else {
    i <- match(names(subst), names(x))
    if (any(is.na(i))) {
      if (warn)
        warning("unused name(s) selected")
      if (any(!is.na(i)))
        subst <- subst[!is.na(i)]
      i <- i[!is.na(i)]
    }
    if (length(i))
      names(x)[i] <- subst
  }
  return(x)
}


#' Convert rownames to column
#'
#' @param data 	a data frame.
#' @param varname name of column to use for rownames
#'
#' @return a data frame.
#' @export
rownames_to_column <- function(data, varname = "term"){
  if(!is.data.frame(data)){
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  data <- cbind(data.frame(term = row.names(data)), data)
  row.names(data) <- NULL
  names(data)[1] <- varname
  data
}


#' Flatten a nested list to a one-level list
#'
#' Flatten a nested list to a one-level list.
#'
#' @param x a list.
#'
#' @return a one-level list.
#' @export
#'
#' @examples
#' dlist <- list("a", mtcars[1:2,], list(5, iris[2:3, ]))
#' dlist
#' flatten_list(dlist)
flatten_list <- function(x) {
  morelists <- sapply(x, function(xprime)
    class(xprime)[1] == "list")
  out <- c(x[!morelists], unlist(x[morelists], recursive = FALSE))
  if (sum(morelists)) {
    Recall(out)
  } else{
    return(out)
  }
}


#' Format and interpolate a string
#'
#' @param ... Unnamed arguments are taken to be expression string(s) to format.
#' Multiple inputs are concatenated together before formatting.
#' @param sep Separator used to separate elements.
#'
#' @return a character string.
#' @export
#'
#' @examples
#' name <- "Fred"
#' age <- 50
#'
#' str_v("My name is {name}, ", "and age is {age}.")
str_v <- function(..., sep = ""){

  text <- c(...)

  out <- sapply(text, function(tex){
    rr <- " "
    pos_1 <- which(strsplit(tex, "")[[1]]=="{")
    pos_2 <- which(strsplit(tex, "")[[1]]=="}")
    end_pos <- nchar(tex)
    varname <- substr(tex, pos_1+1, pos_2-1)
    t <- get(eval(varname))
    t1 <- substr(tex, 1, pos_1-1)
    t2 <- substr(tex, pos_2+1, end_pos)
    t1 <- paste0(t1, t, t2)
    t1
  })

  out <- paste0(out, collapse = sep)
  class(out) <- c("strv", "character")
  out
}


#' Print a 'strv' object
#'
#' @param x a 'strv' object
#' @param ... unused here.
#'
#' @keywords internal
#' @export
print.strv <- function(x, ...){
  cat(x)
}


check_index <- function(data, index){
  tmp <- index >=1 & index <= ncol(data)
  if(!all(tmp)){
    meaasge <- sprintf("Index must be between 1 and %d.", ncol(data))
    stop(meaasge, call. = FALSE)
  }
}


#' Check name
#'
#' Check whether the variable is included in the data frame.
#'
#' @param data  a data frame.
#' @param varnames variable names.
#'
#' @return NULL.
#' @export
check_name <- function(data, varnames){
  tmp <- varnames %in% names(data)
  if(!all(tmp)){
    tmpname <- varnames[!tmp]
    tmpname <- paste(tmpname, collapse = ", ")
    message <- sprintf("%s are (is) not included in the data frame.", tmpname)
    stop(message, call. = FALSE)
  }
}


check_installed <- function (pkg, message = FALSE) {
  res <- all(sapply(pkg, function(x) isTRUE(requireNamespace(x, quietly = TRUE))))
  if(res){
    return(res)
  }else{
    if(message){
      stop(sprintf("The '%s' package needs to be installed. Run the following
                   code to install: install.packages('%s').", pkg, pkg), call. = FALSE)
    }else{
      return(res)
    }
  }
}


#' Change column order
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy data frame.
#' @param variables Columns to move. It can be a variable name or an index in the data frame.
#' @param before Destination of columns selected by variables. Supplying neither
#' will move columns to the left-hand side.
#' @param after Destination of columns selected by variables. Supplying neither
#' will move columns to the right-hand side.
#'
#' @return a data frame.
#' @export
relocate <- function(data, variables, before = NULL, after = NULL) {
  if (is.numeric(variables)) {
    check_index(data, variables)
    to_move <- variables
  } else{
    check_name(data, variables)
    to_move <- sapply(variables, function(x) { which(names(data) == x) })
    names(to_move) <- NULL
  }

  if (!is.null(before) && !is.null(after)) {
    stop("Must supply only one of `.before` and `.after`.")
  } else if (!is.null(before)) {
    if (is.numeric(before)) {
      check_index(data, before)
      where <- before
    } else{
      check_name(data, before)
      where <- which(names(data) == before)
    }
    if (!where %in% to_move) {
      to_move <- c(to_move, where)
    }
  } else if (!is.null(after)) {
    if (is.numeric(after)) {
      check_index(data, after)
      where <- after
    } else{
      check_name(data, after)
      where <- which(names(data) == after)
    }
    if (!where %in% to_move) {
      to_move <- c(where, to_move)
    }
  } else {
    where <- 1L
    if (!where %in% to_move) {
      to_move <- c(to_move, where)
    }
  }

  lhs <- setdiff(seq2(1, where - 1), to_move)
  rhs <- setdiff(seq2(where + 1, ncol(data)), to_move)

  pos <- unique(c(lhs, to_move, rhs))
  out <- data[pos]
  out
}


#' Increasing sequence of integers in an interval
#'
#'
#' These helpers take two endpoints and return the sequence of all integers within
#' that interval. Unlike base::seq(), they return an empty vector if the starting
#' point is a larger integer than the end point.
#'
#' @param from The starting point of the sequence.
#' @param to The end point.
#'
#' @return An integer vector containing a strictly increasing sequence.
#' @export
#'
#' @examples
#' seq2(2, 10)
#' seq2(10, 2)
#' seq(10, 2)
seq2 <- function(from, to) {
  if (length(from) != 1) {
    stop("`from` must be length one")
  }
  if (length(to) != 1) {
    stop("`to` must be length one")
  }
  if (from > to) {
    integer()
  }
  else {
    seq.int(from, to)
  }
}


is_empty <- function(x){
  if(is.null(x)){
    return(TRUE)
  }else{
    if(is.na(x)){
      return(TRUE)
    }else{
      if(length(x) == 0L){
        return(TRUE)
      }else{
        if(trimws(x) == ""){
          return(TRUE)
        }else{
          return(FALSE)
        }
      }
    }
  }
}


#' Merge two data frames by left
#'
#' @param x a data frame.
#' @param y a data frame.
#' @param by specifications of the columns used for merging.
#'
#' @return a data frame.
#' @export
merge_left <- function(x, y, by){
  x$.id <- 1:nrow(x)
  res <- merge(x, y, sort = FALSE, by = by, all.x = TRUE)
  res <- res[order(res$.id), ]
  res[, -which(names(res) == ".id")]
}


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


#' Merge two tables  by the first column
#'
#' @param x a data frame.
#' @param y a data frame.
#' @param name.x name of x.
#' @param name.y name of y.
#' @param name.x.index Start naming from which column of x.
#' @param name.y.index Start naming from which column of y.
#' @param sep a character string to separate the terms.
#'
#' @return a data frame.
#' @export
merge_table <- function(x, y, name.x = NULL, name.y = NULL, name.x.index = 2, name.y.index = 2, sep = "__"){
  x.class <- class(x)
  x.title <- attr(x, "title")
  x.note <- attr(x, "note")

  if(!is.null(name.x)){
    names(x) <- c(names(x)[1:(name.x.index - 1)],
                  paste(name.x, names(x)[c(name.x.index:length(names(x)))], sep = sep))
  }

  if(!is.null(name.y)){
    names(y) <- c(names(y)[1:(name.y.index - 1)],
                  paste(name.y, names(y)[c(name.y.index:length(names(y)))], sep = sep))
  }

  x <- add_terms_column(x, which = 1)
  y <- add_terms_column(y, which = 1)
  y <- y[, -2, drop = FALSE]
  out <- merge_left(x, y, by = "term")
  out <- out[, -1, drop = FALSE]

  class(out) <- x.class
  attr(out, "title") <- x.title
  attr(out, "note") <- x.note
  out
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


#' Execute a function
#'
#' @param what either a function or a non-empty character string naming the
#' function to be called.
#' @param ... arguments for what.
#' @param envir an environment within which to evaluate the call. This will be
#' most useful if what is a character string and the arguments are symbols or quoted expressions.
#'
#' @return The result of the (evaluated) function call.
#' @export
do_call <- function(what, ..., envir = parent.frame()){
  args <- list(...)
  args <- flatten_list(args)
  do.call(what, args = args, quote = FALSE, envir = envir)
}


#' Data frame Transpose
#'
#' @param x a data frame.
#' @param row.names.col specifies which column is the transposed row name,
#' default 1. If row.names.col=0, only row and column names are transposed.
#' @param varname variable name.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' transpose(mtcars, row.names.col = 0)
#'
#' roc.res <- roc(aSAH,
#'               outcome = "outcome",
#'               exposure = c("age", "s100b"))
#' roc.res
#'
#' transpose(roc.res)
transpose <- function(x, row.names.col = 1, varname = NULL){

  title <- attr(x, "title")
  note  <- attr(x, "note")
  args  <- attr(x, "args")

  if(row.names.col == 0){
    x <- rownames_to_column(x, varname = "variable")
    row.names.col <- 1
  }

  if(is.null(varname)){
    varname <- names(x)[row.names.col]
  }

  o.class <- class(x)
  row.names <- x[[row.names.col]]
  x <- x[-row.names.col]
  col.names <- names(x)
  x <- t(x)
  x <- as.data.frame(x)
  names(x) <- row.names
  x <- append2(x, col.names, after = 0)
  names(x)[1] <- varname

  class(x) <- o.class
  attr(x, "title") <- title
  attr(x, "note")  <- note
  attr(x, "args")  <- args
  x
}



fmt_ci_3 <- function(sep = NULL, digits = 2, bracket = c("(", "[")){
  bracket <- match.arg(bracket)
  if(bracket == "("){
    bracket <- c("(", ")")
  }else{
    bracket <- c("[", "]")
  }
  if(is.null(sep)){
    sep <- "\u2013"
  }
  sprintf("%%.%df %s%%.%df%s%%.%df%s", digits, bracket[1], digits, sep, digits, bracket[2])
}

