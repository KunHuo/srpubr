gg_bar_error <- function(data,
                         x = NULL,
                         y = "..count",
                         by = NULL,
                         func = "mean_sd",
                         bar.width = 0.6,
                         bar.color = NULL,
                         bar.label = FALSE,
                         bar.label.data = NULL,
                         bar.label.vjust = -0.5,
                         bar.label.hjust = NA,
                         line.size = 0.25,
                         error.color = bar.color,
                         error.size = line.size,
                         error.width = 0.2,
                         language = NULL,
                         font.family = NULL,
                         font.size = NULL,
                         ...
                         ){

  language    <- get_global_languange(language, default = "en")
  font.family <- get_global_family(font.family, default = "serif")
  font.size   <- get_global_fontsize(font.size, default = 12)
  bar.color   <- get_global_palette(bar.color)
  error.color <- get_global_palette(error.color)

  if(language == "zh"){
    sysfonts::font_add("simsun", "simsun.ttc")
    font.family <- "simsun"
  }else{
    font.family <- font.family
  }

  x  <- select_variable(data, x)
  by <- select_variable(data, by)

  data[[x]] <- factor(data[[x]], exclude = NULL)
  if(!is.null(by)){
    data[[by]] <- factor(data[[by]], exclude = NULL)
  }

  if(!is.null(y)){
    if(y %in% c("..count")){
      func <- "count"
    }else{
      y  <- select_variable(data, y)
    }
  }

  plotdata <- .plotdata(data = data, x = x, y = y, by = by, func = func)

  if(is.null(bar.label.data)){
    plotdata$label <- format_digits(plotdata$y, 1)
  }else{
    plotdata$label <- bar.label.data
    bar.label <- TRUE
  }

  p <- ggplot2::ggplot(data = plotdata,
                       ggplot2::aes_string(x = x,
                                           y = "y",
                                           fill  = ifelse(is.null(by), x, by),
                                           color = ifelse(is.null(by), x, by)))


  if("ymin" %in% names(plotdata)){
    p <- p +ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "ymin", ymax = "ymax"),
                                   width = error.width,
                                   position = ggplot2::position_dodge(width = bar.width),
                                   size = error.size)
    ybresks <- pretty(c(0, max(plotdata[["ymax"]], na.rm = TRUE)), n = 5)
  }else{
    ybresks <- pretty(c(0, max(plotdata[["y"]], na.rm = TRUE)), n = 5)
  }


  if(y == "..count"){
    ylab <- "Count"
  }else{
    ylab <- y
  }

  p <- p +  ggplot2::geom_bar(stat = "identity",
                      width = bar.width,
                      position = ggplot2::position_dodge(width = bar.width),
                      size = line.size)

 if(bar.label){
   if("ymin" %in% names(plotdata)){
     p <- p +
       ggplot2::geom_text(ggplot2::aes_string(label = "label", y = "ymax"),
                          vjust = bar.label.vjust,
                          hjust = bar.label.hjust,
                          color = "black",
                          position = ggplot2::position_dodge(width = bar.width),
                          family = font.family)
   }else{
     p <- p +
       ggplot2::geom_text(ggplot2::aes_string(label = "label"),
                          vjust = bar.label.vjust,
                          hjust = bar.label.hjust,
                          color = "black",
                          position = ggplot2::position_dodge(width = bar.width),
                          family = font.family)
   }
 }


  p <- p + gg_theme_sci(legend.key.size = 0.8,
                 axis.line.size = line.size,
                 font.family = font.family,
                 font.size = font.size, ...) +
    ggplot2::coord_cartesian(clip = "off") +
    gg_ylab(ylab) +
    gg_delete_x_title() +
    ggplot2::scale_y_continuous(breaks = ybresks, limits = c(min(ybresks), max(ybresks)), expand = c(0, 0))


  if(is.null(by)){
    p <- p + gg_delete_legend()
  }

  if(!is.null(bar.color)){
    p <- p +
      ggplot2::scale_fill_manual(values = bar.color)
  }

  if(!is.null(error.color)){
    p <- p +
      ggplot2::scale_color_manual(values = error.color)
  }

  p
}


.plotdata <- function(data, x = NULL, y = NULL, by = NULL, func = NULL){

  mean_sd <- function(x, ...){
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    data.frame(y = m, ymin = m - s, ymax = m + s)
  }

  mean_se <- function(x, ...){
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x) / sqrt(length(x))
    data.frame(y = m, ymin = m - s, ymax = m + s)
  }

  if(func == "count"){
    out  <- group_exec(data, group = c(x, by), \(d){
      data.frame(y = length(d[[x]]))
    })
  }else if(func %in% c("min", "max", "mean", "median", "IQR", "sum")){
    out <- group_exec(data, group = c(x, by), \(d){
      res <- do_call(func, d[[y]], na.rm = TRUE)
      data.frame(y = res)
    })
  }else{
    out <- group_exec(data, group = c(x, by), \(d){
      do_call(func, d[[y]], na.rm = TRUE)
    })
  }

  out[[x]] <- factor(out[[x]], levels = levels(data[[x]]))

  if(!is.null(by)){
    out[[by]] <- factor(out[[by]], levels = levels(data[[by]]))
  }

  out
}
