#
#' Custom \code{ggplot2} Theme with Axis Ticks Oriented Inside the Panel
#'
#'
#' @description
#' tbc
#'
#'
#' @param axis.text.size Numeric, text size (in \code{pt}) for the axes units/descriptions,
#'   \strong{default}: \code{axis.text.size = 14}
#' @param axis.title.size Numeric, text size (in \code{pt}) for the axes title,
#'   \strong{default}: \code{axis.title.size = 15}
#' @param grid Boolean, whether to dislay the \code{grid} within the plot/graph, \strong{default}: \code{grid = TRUE}
#' @param bg.transparent Boolean, whether the \code{entire plot background} (NOT the \code{panel}=\code{own graph})
#'   should be transparent, \strong{default}: \code{bg.transparent = FALSE}, i.e. no transparent background
#'
#'
#' @return Custom \pkg{ggplot2} \code{theme} with axis \code{ticks pointing inside} the graph panel,
#'   to show opposite axis ticks use: \code{scale_..._continuous(sec.axis = dup_axis(name = "",labels = NULL))}
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#'
#' @export
#'
#'
plot_theme_In_ticks <- function(axis.text.size = 14,
                                axis.title.size = 15,
                                grid = TRUE,
                                bg.transparent = FALSE){
  ## theme parts:
  theme_bas <- theme(axis.ticks.length = unit(-6,"pt"),
                     axis.text.x = element_text(margin = margin(8,8,6,8,unit = "pt"),size = axis.text.size),
                     axis.text.y = element_text(margin = margin(8,10,8,0,unit = "pt"),size = axis.text.size),
                     axis.title.y = element_text(margin = margin(2,12,2,6,unit = "pt"),size = axis.title.size),
                     axis.title.x = element_text(margin = margin(2,6,2,6,unit = "pt"),size = axis.title.size),
                     panel.border = element_rect(color = "black",fill = NA))
  theme_Nogrid <- theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  #
  ## theme:
  if (isTRUE(bg.transparent)){
    if (isTRUE(grid)){
        thm <- theme_bas +
          theme(plot.background = element_rect(fill = "transparent"))
    } else{
        thm <- theme_bas +
          theme_Nogrid +
          theme(plot.background = element_rect(fill = "transparent"))
    }
  } else{
    if (isTRUE(grid)){
        thm <- theme_bas
    } else{
        thm <- theme_bas +
          theme_Nogrid
    }
  }
  #
  return(thm)
  #
}
