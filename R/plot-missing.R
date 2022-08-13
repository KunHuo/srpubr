gg_missing <- function(data,
                       percent.bar = TRUE,
                       percent.miss = FALSE,
                       show.all = FALSE,
                       decreasing = TRUE,
                       detail.type = TRUE,
                       add.var.miss = TRUE,
                       digits = 1,
                       language = c("en", "zh"),
                       bar.color = NULL,
                       bar.width = 0.65,
                       miss.color = NULL,
                       font.family = "serif",
                       font.size = 12,
                       tag.levels = "A",
                       ...){

  A <- gg_missing_bar(data = data,
                      percent = percent.bar,
                      show.all = show.all,
                      decreasing = decreasing,
                      detail.type = detail.type,
                      digits = digits,
                      language = language,
                      bar.color = bar.color,
                      bar.width = bar.width,
                      font.family = font.family,
                      font.size = font.size,
                      ...)

  if(is.null(A)){
    return(invisible(NULL))
  }

  B <- gg_missing_pattern(data = data,
                          percent = percent.miss,
                          show.all = show.all,
                          decreasing = decreasing,
                          add.var.miss = add.var.miss,
                          digits = digits,
                          language  = language,
                          font.family = font.family,
                          font.size = font.size,
                          color = miss.color,
                          ...)

  tag.levels <- tag_levels(tag.levels, n = 2)

  A <- A + gg_tags(tag.levels[1])
  B <- B + gg_tags(tag.levels[2])

  patchwork::wrap_plots(A, B)
}
