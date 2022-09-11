gg_boxplot <- function(data, x, y, by = NULL, box.width = 0.5,
                       na.include = TRUE,
                       x.order = NULL,
                       language = NULL,
                       font.family = NULL,
                       font.size = NULL){

  language    <- get_global_languange(language, default = "en")
  font.family <- get_global_family(font.family, default = "serif")
  font.size   <- get_global_fontsize(font.size, default = 12)

  if(language == "zh"){
    sysfonts::font_add("simsun", "simsun.ttc")
    font.family <- "simsun"
  }else{
    font.family <- font.family
  }

  x  <- select_variable(data, x)
  y  <- select_variable(data, y)
  by <- select_variable(data, by)

  if(na.include){
    exclude.na <- NULL
  }else{
    exclude.na <- NA
  }

  data[[x]] <- factor(data[[x]], exclude = exclude.na)
  if(!is.null(by)){
    data[[by]] <- factor(data[[by]], exclude = exclude.na)
  }

  if(!is.null(x.order)){
    data <- fct_reorder(data, x, x.order, exclude = exclude.na)
  }

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_boxplot(ggplot2::aes_string(x = x, y = y, fill = ifelse(is.null(by), x, by)),
                          width = box.width,
                          color = "black",
                          size = 0.25) +
    gg_theme_sci()

  gbdata <-  ggplot2::ggplot_build(p)$data[[1]]
  ybreaks <- pretty(c(min(gbdata[["ymin_final"]]), max(gbdata[["ymax_final"]])), 5)

  p <- p +
    ggplot2::scale_y_continuous(breaks = ybreaks, limits = c(min(ybreaks), max(ybreaks)), expand = c(0, 0))

  if(is.null(by)){
    p <- p + gg_delete_legend()
  }

  p
}

