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
#' @param x.label tbc
#' @param y.label tbc
#' @param x.axis.title.family tbc
#' @param x.axis.title.color tbc
#' @param x.axis.title.size tbc
#' @param x.axis.tick.family tbc
#' @param x.axis.tick.color tbc
#' @param x.axis.tick.size tbc
#' @param y.axis.title.family tbc
#' @param y.axis.title.color tbc
#' @param y.axis.title.size tbc
#' @param y.axis.tick.family tbc
#' @param y.axis.tick.color tbc
#' @param y.axis.tick.size tbc
#' @param legend.label tbc
#' @param legend.label.family tbc
#' @param legend.label.color tbc
#' @param legend.label.size tbc
#' @param bg.color tbc
#' @param grid.color tbc
#' @param border.line.width tbc
#' @param border.line.color tbc
#'
#'
#' @return
#' tbc
#'
#' @examples
#' tbc
#' tbc
#'
#' @export
#'
#' @importFrom plotly layout toRGB
plot_layout2D_interact <- function(p,
                                 data = NULL,
                                 x.label = "<i>B</i> (mT)",
                                 y.label = "d <i>I</i><sub>EPR</sub> / d <i>B</i>  (p.d.u.)",
                                 x.axis.title.family = "Arial",
                                 x.axis.title.color = "black",
                                 x.axis.title.size = 15,
                                 x.axis.tick.family = "Arial",
                                 x.axis.tick.color = "black",
                                 x.axis.tick.size = 14,
                                 y.axis.title.family = "Arial",
                                 y.axis.title.color = "black",
                                 y.axis.title.size = 15,
                                 y.axis.tick.family = "Arial",
                                 y.axis.tick.color = "black",
                                 y.axis.tick.size = 14,
                                 legend.label = NULL,
                                 legend.label.family = "Arial",
                                 legend.label.color = "black",
                                 legend.label.size = 13,
                                 bg.color = "#e5ecf6",
                                 grid.color = "#ffff",
                                 border.line.width = 1.2,
                                 border.line.color = "black"
                                 ){
#
Plotly_layout <- plotly::layout(
    p,
    data,
    plot_bgcolor = bg.color,
    xaxis = list(title = list(text = x.label,
                              font = list(family = x.axis.title.family,
                                          color = x.axis.title.color,
                                          size = x.axis.title.size)),
                 tickfont = list(family = x.axis.tick.family,
                                 color = x.axis.tick.color,
                                 size = x.axis.tick.size),
                 gridcolor = grid.color,
                 linecolor = plotly::toRGB(border.line.color),
                 linewidth = border.line.width,showline = T,mirror = T),
    yaxis = list(title = list(text = y.label,
                              font = list(family = y.axis.title.family,
                                          color = y.axis.title.color,
                                          size = y.axis.title.size)),
                 tickfont = list(family = y.axis.tick.family,
                                 color = y.axis.tick.color,
                                 size = y.axis.tick.size),
                 gridcolor = grid.color,
                 linecolor = plotly::toRGB(border.line.color),
                 linewidth = border.line.width,showline = T,mirror = T),
    legend = list(title = list(text = legend.label,
                               font = list(family = legend.label.family,
                                           color = legend.label.color,
                                           size = legend.label.size)))
  )
  #
  return(Plotly_layout)
  #
}
