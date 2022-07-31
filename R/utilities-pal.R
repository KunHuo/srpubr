#' Color palettes of AAAS journal
#'
#' @param n Choose the color from the 10 colors.
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @export
pal_aaas_10 <- function(n = 1, alpha = 1){
  ggsci::pal_aaas(alpha = alpha)(10)[n]
}


#' Color palettes of JAMA journal
#'
#' @param n Choose the color from the 7 colors.
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @export
pal_jama_7 <- function(n = 1, alpha = 1){
  ggsci::pal_jama(alpha = alpha)(7)[n]
}


#' Color palettes of JCO journal
#'
#' @param n Choose the color from the 10 colors.
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @export
pal_jco_10 <- function(n = 1, alpha = 1){
  ggsci::pal_jco(alpha = alpha)(10)[n]
}


#' Color palettes of NEJM journal
#'
#' @param n Choose the color from the 8 colors.
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @export
pal_nejm_8 <- function(n = 1, alpha = 1){
  ggsci::pal_nejm(alpha = alpha)(8)[n]
}


#' Color palettes of lancet journal
#'
#' @param n Choose the color from the 9 colors.
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @export
pal_lancet_9 <- function(n = 1, alpha = 1){
  ggsci::pal_lancet(alpha = alpha)(9)[n]
}
