#
#' @title TODO
#'
#'
#' @description TODO
#'
#'
#' @param base.theme TODO
#' @param axis.text.size TODO
#' @param axis.title.size TODO
#' @param bg.transparent TODO
#'
#'
#' @return TODO
#'
#'
#' @examples
#' TODO
#' TODO
#'
#'
#' @export
#'
#'
#' @importFrom ggplot2 %+replace%
plot_theme_NoY_ticks <- function(base.theme = theme_gray(),axis.text.size,axis.title.size,bg.transparent = FALSE){
  ## theme part
  theme_01 <- theme(axis.ticks.length = unit(-6,"pt"),
        axis.text.x = element_text(margin = margin(10,8,6,8,unit = "pt"),size = axis.text.size),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_text(margin = margin(2,12,2,6,unit = "pt"),size = axis.title.size),
        axis.title.x = element_text(margin = margin(2,6,2,6,unit = "pt"),size = axis.title.size),
        panel.border = element_rect(color = "black",fill = NA))
  ## theme:
  if (isTRUE(bg.transparent)){
    base.theme %+replace%
      theme_01 +
      theme(plot.background = element_rect(fill = "transparent"))
  } else{
    base.theme %+replace%
      theme_01
  }
}
