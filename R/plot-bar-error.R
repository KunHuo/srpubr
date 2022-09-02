gg_bar_error <- function(data,
                         x = NULL,
                         y = NULL,
                         by = NULL,
                         bar.width = 0.6,
                         bar.color = NULL,
                         line.size = 0.25,
                         error.color = bar.color,
                         error.size = 0.25,
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
  y  <- select_variable(data, y)
  by <- select_variable(data, by)

  plotdata <- group_exec(data, group = c(x, by), \(d){
    m <- mean(d[[y]])
    s <- sd(d[[y]])
    lower <- m - s
    upper <- m + s
    data.frame(value = m, lower = lower, upper = upper)
  })


  by.legend <- ifelse(is.null(by), x, by)


  pos <- ggplot2::position_dodge(width = bar.width)

  p <- ggplot2::ggplot(data = plotdata, ggplot2::aes_string(x = x, y = "value", fill = by.legend, color = by.legend)) +
    ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "lower", ymax = "upper"),
                           width = error.width,
                           position =  pos,
                           size = error.size) +
    ggplot2::geom_bar(stat = "identity",
                      width = bar.width,
                      position = pos,
                      size = line.size) +
    gg_theme_sci(legend.key.size = 0.8,
                 axis.line.size = line.size,
                 font.family = font.family,
                 font.size = font.size, ...) +
    ggplot2::coord_cartesian(clip = "off") +
    gg_ylab(y) +
    gg_delete_x_title()

  ybresks <- pretty(c(0, max(plotdata[["upper"]])), n = 5)

  p <- p +
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

