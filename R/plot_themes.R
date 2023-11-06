#'
#' Custom \code{ggplot2} Theme with Axis Ticks Oriented Inside the Panel
#'
#'
#' @family Visualizations and Graphics
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
#' @param border.line.color description...tbc...
#' @param border.line.type description ...tbc...
#' @param border.line.width description ...tbc...
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
                                border.line.color = "black",
                                border.line.type = "solid",
                                border.line.width = 0.5,
                                bg.transparent = FALSE) {
  ## theme parts:
  theme_bas <- theme(
    axis.ticks.length = unit(-6, "pt"),
    axis.text.x = element_text(margin = margin(6, 6, 4, 6, unit = "pt"), size = axis.text.size),
    axis.text.y = element_text(margin = margin(4, 6, 6, 0, unit = "pt"), size = axis.text.size),
    axis.title.y = element_text(margin = margin(2, 8, 2, 6, unit = "pt"), size = axis.title.size),
    axis.title.x = element_text(margin = margin(2, 6, 2, 6, unit = "pt"), size = axis.title.size),
    panel.border = element_rect(color = border.line.color,
                                linetype = border.line.type,
                                linewidth = border.line.width,
                                fill = NA)
  )
  theme_Nogrid <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  #
  ## theme:
  if (isTRUE(bg.transparent)) {
    if (isTRUE(grid)) {
      thm <- theme_bas +
        theme(plot.background = element_rect(fill = "transparent"))
    } else {
      thm <- theme_bas +
        theme_Nogrid +
        theme(plot.background = element_rect(fill = "transparent"))
    }
  } else {
    if (isTRUE(grid)) {
      thm <- theme_bas
    } else {
      thm <- theme_bas +
        theme_Nogrid
    }
  }
  #
  return(thm)
  #
}
#
#
#
#
#' Custom \code{ggplot2} Theme with NO \code{Y} Axis Ticks
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#' tbc
#'
#'
#' @param axis.text.size  Numeric, text size (in \code{pt}) for the axes units/descriptions,
#'   \strong{default}: \code{axis.text.size = 14}
#' @param axis.title.size Numeric, text size (in \code{pt}) for the axes title,
#'   \strong{default}: \code{axis.title.size = 15}
#' @param grid Boolean, whether to dislay the \code{grid} within the plot/graph, \strong{default}: \code{grid = TRUE}
#' @param border.line.color description...tbc...
#' @param border.line.type description ...tbc...
#' @param border.line.width description ...tbc...
#' @param bg.transparent Boolean, whether the \code{entire plot background} (NOT the \code{panel}=\code{own graph})
#'   should be transparent, \strong{default}: \code{bg.transparent = FALSE}, i.e. no transparent background
#'
#'
#' @return Custom \pkg{ggplot2} \code{theme} \code{without Y axis ticks}, to show opposite X axis ticks
#'   use: \code{scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL))}
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
plot_theme_NoY_ticks <- function(axis.text.size = 14,
                                 axis.title.size = 15,
                                 grid = TRUE,
                                 border.line.color = "black",
                                 border.line.type = "solid",
                                 border.line.width = 0.5,
                                 bg.transparent = FALSE) {
  ## theme parts:
  theme_bas <- theme(
    axis.ticks.length = unit(-6, "pt"),
    axis.text.x = element_text(margin = margin(6, 6, 4, 6, unit = "pt"), size = axis.text.size),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    axis.title.y = element_text(margin = margin(2, 8, 2, 6, unit = "pt"), size = axis.title.size),
    axis.title.x = element_text(margin = margin(2, 6, 2, 6, unit = "pt"), size = axis.title.size),
    panel.border = element_rect(color = border.line.color,
                                linetype = border.line.type,
                                linewidth = border.line.width,
                                fill = NA)
  )
  theme_Nogrid <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  #
  ## theme:
  if (isTRUE(bg.transparent)) {
    if (isTRUE(grid)) {
      thm <- theme_bas +
        theme(plot.background = element_rect(fill = "transparent"))
    } else {
      thm <- theme_bas +
        theme_Nogrid +
        theme(plot.background = element_rect(fill = "transparent"))
    }
  } else {
    if (isTRUE(grid)) {
      thm <- theme_bas
    } else {
      thm <- theme_bas +
        theme_Nogrid
    }
  }
  #
  return(thm)
  #
}
#
#
#
#
#' Custom \code{ggplot2} Theme with Axis Ticks Oriented Outside of the Panel
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description TODO
#'
#'
#' @param axis.text.size Numeric, text size (in \code{pt}) for the axes units/descriptions,
#'   \strong{default}: \code{axis.text.size = 14}
#' @param axis.title.size Numeric, text size (in \code{pt}) for the axes title,
#'   \strong{default}: \code{axis.title.size = 15}
#' @param grid Boolean, whether to dislay the \code{grid} within the plot/graph, \strong{default}: \code{grid = TRUE}
#' @param border.line.color description...tbc...
#' @param border.line.type description ...tbc...
#' @param border.line.width description ...tbc...
#' @param bg.transparent Boolean, whether the \code{entire plot background} (NOT the \code{panel}=\code{own graph})
#'   should be transparent, \strong{default}: \code{bg.transparent = FALSE}, i.e. no transparent background
#'
#'
#' @return Custom \pkg{ggplot2} \code{theme} with axis \code{ticks pointing outside} the graph panel
#'
#'
#' @examples
#' \dontrun{
#' TODO
#' TODO
#' }
#'
#'
#' @export
#'
#'
plot_theme_Out_ticks <- function(axis.text.size = 14,
                                 axis.title.size = 15,
                                 grid = TRUE,
                                 border.line.color = "black",
                                 border.line.type = "solid",
                                 border.line.width = 0.5,
                                 bg.transparent = FALSE) {
  ## theme parts:
  theme_bas <- theme(
    axis.ticks.length = unit(6, "pt"),
    axis.text.x = element_text(margin = margin(6, 6, 4, 6, unit = "pt"), size = axis.text.size),
    axis.text.y = element_text(margin = margin(4, 6, 6, 0, unit = "pt"), size = axis.text.size),
    axis.title.y = element_text(margin = margin(2, 8, 2, 6, unit = "pt"), size = axis.title.size),
    axis.title.x = element_text(margin = margin(2, 6, 2, 6, unit = "pt"), size = axis.title.size),
    panel.border = element_rect(color = border.line.color,
                                linetype = border.line.type,
                                linewidth = border.line.width,
                                fill = NA)
  )
  theme_Nogrid <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  #
  ## theme:
  if (isTRUE(bg.transparent)) {
    if (isTRUE(grid)) {
      thm <- theme_bas +
        theme(plot.background = element_rect(fill = "transparent"))
    } else {
      thm <- theme_bas +
        theme_Nogrid +
        theme(plot.background = element_rect(fill = "transparent"))
    }
  } else {
    if (isTRUE(grid)) {
      thm <- theme_bas
    } else {
      thm <- theme_bas +
        theme_Nogrid
    }
  }
  #
  return(thm)
  #
}
