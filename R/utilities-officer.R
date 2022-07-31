format_flex <- function(data, headers = NULL, font.size = 11){
  if(length(headers) == 0L){
    ft <- flextable::flextable(data)
  }else{
    newnames <- headers[[1]]
    newnames <- newnames[!grepl(x = newnames, pattern = "^blank\\d+$")]
    names(data) <- newnames

    ft <- flextable::flextable(data, col_keys = headers$col_keys)
    ft <- flextable::set_header_df(ft, mapping = headers, key = "col_keys")
  }
  border <- officer::fp_border(color = "black")

  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::padding(ft, padding = 3, part = "all")
  ft <- flextable::fontsize(ft, size = font.size, part = "all")
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::merge_v(ft, part = "header")
  ft <- flextable::merge_h(ft, part = "header")
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::autofit(ft)
  ft <- flextable::hline(ft, part = "header", border = border)
  ft <- flextable::hline_bottom(ft, border = border, part = "body")
  ft <- flextable::hline_top(ft, border = border, part = "header")
  ft <- flextable::fix_border_issues(ft)
  ft
}


make_headers <- function(data, headerlist = NULL, sep = "__"){
  if(is.null(sep)){
    return(NULL)
  }

  if(length(headerlist) != 0L){
    for (i in seq_along(headerlist)) {
      index <- headerlist[i][[1]]
      names(data)[index] <- paste(names(headerlist)[i], names(data)[index], sep = sep)
    }
  }

  if(!any(grepl(x = names(data), pattern = sep, fixed = TRUE))){
    return(NULL)
  }

  col_keys <- names(data)
  lines <- strsplit(names(data), split = sep, fixed = TRUE)
  lines <- do.call(cbind, lines)
  line2 <- lines[1, ]
  line3 <- lines[2, ]

  index <- sapply(unique(line2), function(x){
    if(length(which(x == line2)) > 1 ){
      x
    }else{
      NA
    }
  })
  index <- index[!is.na(index)]

  for(i in seq_along(index)){
    tmp <- which(index[i] == line2)
    col_keys[tmp] <- sprintf("%d_%d", seq_along(tmp), i)
  }

  out <- data.frame(col_keys = col_keys, line2 = line2, line3 = line3)
  out <- split(out, f = line2)
  out <- out[unique(line2)]

  for (i in seq_along(out)){
    if(i < length(out)){
      if((names(out)[i] %in% index) & (names(out)[i + 1] %in% index)){
        blank <- sprintf("blank%s", substr(out[[i]][1, 1], start = 3, stop = nchar(out[[i]][1, 1])))
        out[[i]] <- rbind(out[[i]], c(blank, " ", " "))
      }
    }
  }
  do.call(rbind, out)
}


get_template <- function(template) {
  template <- paste("templates", template, sep = "/")
  # package  <- methods::getPackageName()
  template <- file.path(system.file(package = "srpubr"), template)
  regression <- regexpr(paste("(\\.(?i)(docx))$", sep = ""), template)

  if (regression < 1) {
    stop("invalid template name, it must have extension .docx", call. = FALSE)
  }
  template <- R.utils::getAbsolutePath(template, expandTilde = TRUE)

  if (!file.exists(template)) {
    stop(template , " can not be found.")
  }
  return(template)
}


#' flextable creation
#'
#' @param data data
#' @param headerlist header list
#' @param sep sep
#' @param font.size font size
#' @param ... unused
#'
#' @keywords internal
#' @export
flex_table <- function(data, headerlist = NULL, sep = "__", font.size = 11, ...){
  headers <- make_headers(data = data, sep = sep, headerlist = headerlist)
  format_flex(data, headers, font.size = font.size)
}


#' Create a 'Word' document object
#'
#' read and import a docx file as an R object representing the document. When no
#' file is specified, it uses a default empty file.Use then this object to add
#' content to it and create Word files from R.
#'
#' @return an object of class rdocx.
#' @keywords internal
#' @export
get_docx <- function(path = NULL){
  if(is.null(path)){
    path <- get_template("template.docx")
  }
  officer::read_docx(path = path)
}


file_ext <- function(path){
  regmatches(path,
             regexpr(pattern = "(?<=\\.)[^\\.]+$",
                     path,
                     ignore.case = FALSE,
                     perl = TRUE,
                     fixed = FALSE,
                     useBytes = FALSE))
}


file_path <- function(path, ext = "docx"){
  tmp <- file_ext(path = path)
  if(length(tmp) == 0L){
    path <- paste(path, ext, sep = ".")
  }else{
    if(tolower(tmp) != ext){
      path <- paste(path, ext, sep = ".")
    }
  }
  path
}


dir_create <- function(path){
  path.name <- dirname(path)
  if(path.name != "."){
    if(!dir.exists(path.name)){
      dir.create(path.name, recursive = TRUE)
    }
  }
}


#' Add paragraphs of text in a 'Word' document
#'
#' @param x a docx device.
#' @param value a character
#' @param style paragraph style name
#' @param pos where to add the new element relative to the cursor, one of "after", "before", "on"
#'
#' @keywords internal
#' @export
body_add_par2 <- function(x, value, style = NULL, pos = "after"){
  value <- paste(value, collapse = "\n")
  values <- strsplit(value, split = "\n", fixed = TRUE)
  values <- values[[1]]

  for(i in seq_along(values)){
    x <- officer::body_add_par(x, value = values[i], style = style, pos = "after")
  }
  x
}


body_add_tablenote <- function(x, value) {
  value <- paste(value, collapse = "\n")
  values <- strsplit(value, split = "\n", fixed = TRUE)
  values <- values[[1]]

  for(i in seq_along(values)){
    if (i == 1L){
      padding.top <- 5
    }else{
      padding.top <- 0
    }
    paragraph <- officer::fpar( officer::ftext(values[i]),
      fp_p = officer::fp_par(text.align = "left", padding.top = padding.top, line_spacing = 1.5))
    x <- officer::body_add_fpar(x, value = paragraph)
  }
  x
}


#' Add heading of text in a 'Word' document
#'
#' @param x a docx device.
#' @param value a character
#'
#' @keywords internal
#' @export
body_add_heading1 <- function(x, value){
  body_add_par2(x, value = value, style = "heading 1")
}


#' @rdname body_add_heading1
#' @export
body_add_heading2 <- function(x, value){
  body_add_par2(x, value = value, style = "heading 2")
}


#' @rdname body_add_heading1
#' @export
body_add_heading3 <- function(x, value){
  body_add_par2(x, value = value, style = "heading 3")
}


#' add data frame into a Word document
#'
#' @param x an rdocx object.
#' @param value a data frame.
#' @param ... arguments passed to flex_table.
#'
#' @keywords internal
#' @export
body_add_dataframe <- function(x, value, ...){
  if(!("flextable" %in% class(x))){
    value <- flex_table(data = value, ...)
  }
  flextable::body_add_flextable(x = x,
                                value = value,
                                align = "left",
                                split = TRUE,
                                keepnext = FALSE)
}


#' Heading
#'
#' @param x a character.
#' @keywords internal
#' @export
heading1 <- function(x){
  out <- list(text = as.character(x), style = "heading 1")
  class(out) <- c("style")
  out
}


#' @rdname heading1
#' @export
heading2 <- function(x){
  out <- list(text = as.character(x), style = "heading 2")
  class(out) <- c("style")
  out
}


#' @rdname heading1
#' @export
heading3 <- function(x){
  out <- list(text = as.character(x), style = "heading 3")
  class(out) <- c("style")
  out
}


write_rdocx <- function(x, path, ...){
  # x <- officer::body_end_section_landscape(x)

  if(is.null(path)){
    stop("Path can not be empty.", call. = FALSE)
  }else{
    path <- trimws(path)
  }
  if(length(path) == 0L | path == ""){
    stop("Path can not be empty.", call. = FALSE)
  }
  path <- file_path(path)
  dir_create(path = path)
  print(x, path)
}


#' Write a object to MS-Word
#'
#' Send objects like data.frame, list, or just simple texts to a MS-Word document
#'
#' @param x a string, data frame or list.
#' @param path file path.
#' @param ... unused.
#'
#' @export
write_docx <- function(x, path = "", ...){
  UseMethod("write_docx")
}


#' @rdname write_docx
#' @export
write_docx.default <- function(x, path = "", ...){
  get_docx() |>
    body_add_par2(value = as.character(x)) |>
    write_rdocx(path)
}


#' @rdname write_docx
#' @export
write_docx.data.frame <- function(x, path = "", ...){
  doc <- get_docx()
  title <- attr(x, "title")
  note <- attr(x, "note")
  if(length(title) != 0L){
    doc <- body_add_par2(doc, value = title, style = "table title")
  }
  doc <- body_add_dataframe(doc, value = x, ...)
  if(length(note) != 0L){
    doc <- body_add_tablenote(doc, value = note)
  }
  write_rdocx(doc, path = path)
}


#' @rdname write_docx
#' @export
write_docx.list <- function(x, path = "", ...){
  doc <- get_docx()
  for(i in 1:length(x)){
    if(is.character(x[[i]]) | is.numeric(x)){
      doc <- body_add_par2(doc, value = x[[i]], style = "Normal")
    }else if(is.data.frame(x[[i]])){
      title <- attr(x[[i]], "title")
      note <- attr(x[[i]], "note")
      if(length(title) != 0L){
        doc <- body_add_par2(doc, value = title, style = "table title")
      }
      if(i != 1L){
        if(is.data.frame(x[[i-1]])){
          doc <- body_add_par2(doc, value = " ")
        }
      }
      doc <- body_add_dataframe(doc, value = x[[i]])
      if(length(note) != 0L){
        doc <- body_add_tablenote(doc, value = note)
      }
    }else if(class(x[[i]]) == "spubr"){

    }else if(class(x[[i]]) == "flextable"){
      doc <- body_add_dataframe(doc, value = x[[i]])
    }else if(class(x[[i]]) == "style"){
      value <- x[[i]]$text
      style <- x[[i]]$style
      doc <- body_add_par2(doc, value = value, style = style)
    }else{
      doc <- body_add_par2(doc, value = as.character(x[[i]]), style = "Normal")
    }
  }
  write_rdocx(doc, path = path)
}


#' @rdname write_docx
#' @export
write_docx.spubr <- function(x, path = "", ...){
  get_docx() |>
    body_add_dataframe(value = x, ...) |>
    write_rdocx(path = path)
}


#' @rdname write_docx
#' @export
write_docx.rdocx <- function(x, path = "", ...){
  write_rdocx(x, path = path)
}
