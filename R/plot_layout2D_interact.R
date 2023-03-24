#
#' Additional Layout for Interactive Plots Similar to Layout from Plotly Package
#'
#'
#' @description
#' tbc
#'
#'
#' @param p tbc
#' @param data tbc
#' @param xlab tbc
#' @param ylab tbc
#' @param axis.title.x.family tbc
#' @param axis.title.x.color tbc
#' @param axis.title.x.size tbc
#' @param axis.text.x.family tbc
#' @param axis.text.x.color tbc
#' @param axis.text.x.size tbc
#' @param axis.title.y.family tbc
#' @param axis.title.y.color tbc
#' @param axis.title.y.size tbc
#' @param axis.text.y.family tbc
#' @param axis.text.y.color tbc
#' @param axis.text.y.size tbc
#' @param legend.title tbc
#' @param legend.title.family tbc
#' @param legend.title.color tbc
#' @param legend.title.size tbc
#' @param bg.color tbc
#' @param grid.color tbc
#' @param border.line.width tbc
#' @param border.line.color tbc
#'
#'
#' @return
#' tbc
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
#' @importFrom plotly layout toRGB
plot_layout2D_interact <- function(p,
                                   data = NULL,
                                   xlab = "<i>B</i> (mT)",
                                   ylab = "d <i>I</i><sub>EPR</sub> / d <i>B</i>  (p.d.u.)",
                                   axis.title.x.family = "Arial",
                                   axis.title.x.color = "black",
                                   axis.title.x.size = 15,
                                   axis.text.x.family = "Arial",
                                   axis.text.x.color = "black",
                                   axis.text.x.size = 14,
                                   axis.title.y.family = "Arial",
                                   axis.title.y.color = "black",
                                   axis.title.y.size = 15,
                                   axis.text.y.family = "Arial",
                                   axis.text.y.color = "black",
                                   axis.text.y.size = 14,
                                   legend.title = NULL,
                                   legend.title.family = "Arial",
                                   legend.title.color = "black",
                                   legend.title.size = 13,
                                   bg.color = "#e5ecf6",
                                   grid.color = "#ffff",
                                   border.line.width = 1.2,
                                   border.line.color = "black") {
  #
  Plotly_layout_0 <- plotly::layout(
    p,
    data,
    plot_bgcolor = bg.color,
    xaxis = list(
      title = list(
        text = xlab,
        font = list(
          family = axis.title.x.family,
          color = axis.title.x.color,
          size = axis.title.x.size
        )
      ),
      tickfont = list(
        family = axis.text.x.family,
        color = axis.text.x.color,
        size = axis.text.x.size
      ),
      gridcolor = grid.color,
      linecolor = plotly::toRGB(border.line.color),
      linewidth = border.line.width, showline = T, mirror = T
    ),
    yaxis = list(
      title = list(
        text = ylab,
        font = list(
          family = axis.title.y.family,
          color = axis.title.y.color,
          size = axis.title.y.size
        )
      ),
      tickfont = list(
        family = axis.text.y.family,
        color = axis.text.y.color,
        size = axis.text.y.size
      ),
      gridcolor = grid.color,
      linecolor = plotly::toRGB(border.line.color),
      linewidth = border.line.width, showline = T, mirror = T
    )
  )
  if (is.null(legend.title)) {
    Plotly_layout <- Plotly_layout_0
  } else {
    Plotly_layout <- Plotly_layout_0 %>%
      plotly::layout(legend = list(title = list(
        text = legend.title,
        font = list(
          family = legend.title.family,
          color = legend.title.color,
          size = legend.title.size
        )
      )
    ))
  }
  #
  return(Plotly_layout)
  #
}
