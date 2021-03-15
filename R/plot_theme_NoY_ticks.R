#
#' @title TODO
#'
#'
#' @description TODO
#'
#'
#' @param basic.theme TODO
#' @param axis.text.size TODO
#' @param axis.title.size TODO
#' @param grid TODO
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
plot_theme_NoY_ticks <- function(basic.theme = theme_gray(),
                                 axis.text.size = 15,
                                 axis.title.size = 17,
                                 grid = TRUE,
                                 bg.transparent = FALSE){
  ## theme parts:
  theme_bas <- theme(axis.ticks.length = unit(-6,"pt"),
        axis.text.x = element_text(margin = margin(10,8,6,8,unit = "pt"),size = axis.text.size),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_text(margin = margin(2,12,2,6,unit = "pt"),size = axis.title.size),
        axis.title.x = element_text(margin = margin(2,6,2,6,unit = "pt"),size = axis.title.size),
        panel.border = element_rect(color = "black",fill = NA))
  theme_Nogrid <- theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  ## theme:
  if (isTRUE(bg.transparent)){
    if (isTRUE(grid)){
      base.theme %+replace%
        theme_bas +
        theme(plot.background = element_rect(fill = "transparent"))
    } else{
      base.theme %+replace%
        theme_bas +
        theme_Nogrid +
        theme(plot.background = element_rect(fill = "transparent"))
    }
  } else{
    if (isTRUE(grid)){
      base.theme %+replace%
        theme_bas
    } else{
      base.theme %+replace%
        theme_bas +
        theme_Nogrid
    }
  }
}
