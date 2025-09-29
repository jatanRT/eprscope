#
#' Additional Layout for the Interactive 2D Plots
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#'    Additional customization of interactive 2D plots (see also \code{\link{plot_EPR_Specs2D_interact}}).
#'    The main goal is to simplify setting up process of the \code{\link[plotly]{layout}} via several nested \code{lists}.
#'    Mostly, it nicely works (using the \code{\link[magrittr]{\%>\%}}) either with the general \code{\link[plotly]{plot_ly}}
#'    or the above-mentioned \code{\link{plot_EPR_Specs2D_interact}} (see also \code{Examples}).
#'
#'
#' @inheritParams plot_EPR_Specs2D_interact
#' @param p \code{Plotly} object, corresponding to previous/actual graph/plot.
#' @param data Data frame object in order to associate with the layout (optional). If not provided, arguments are evaluated
#'   using the data frame in \code{\link[plotly]{plot_ly}} function of the previous/actual plot. Therefore,
#'   \strong{default}: \code{data = NULL}.
#' @param xlab Character string \eqn{\equiv} title of the \eqn{x}-axis. Either simple, like
#'   \code{xlab = "B (mT)"} can be applied or if additional formatting is required,
#'   the \href{https://www.w3schools.com/html/html_formatting.asp}{\code{html} markup language} is used,
#'   such as \code{xlab = "<i>B</i> (mT)"} (\strong{default}). If a \eqn{\LaTeX} typesetting
#'   is required for the title, please refer to e.g. \href{https://plotly.com/r/LaTeX/}{LaTeX Plotly Tepesetting}.
#' @param ylab Character string \eqn{\equiv} title of the \eqn{y}-axis (see also \code{xlab}), \strong{default}:
#'   \code{ylab = "d <i>I</i> <sub>EPR</sub> / d <i>B</i>  (p.d.u.)"}.
#' @param axis.title.x.family Character string, setting the font of the \eqn{x}-axis title. Following \code{html} fonts
#'   are available: "Arial" (\strong{default}), "Balto", "Courier New", "Droid Sans", "Droid Serif", "Droid Sans Mono",
#'   "Gravitas One", "Old Standard TT", "Open Sans", "Overpass", "PT Sans Narrow", "Raleway" and "Times New Roman".
#' @param axis.title.x.color Character string, pointing to color of the \eqn{x}-axis title font, \strong{default}:
#'   \code{axis.title.x.color = "black"}. Additional available colors are listed
#'   at the \href{https://www.w3.org/TR/css-color-3/}{CSS Color Module Homepage}.
#' @param axis.title.x.size Numeric, setting up the size of the \eqn{x}-axis title font in \code{px}, \strong{default}:
#'   \code{axis.title.x.size = 15}.
#' @param axis.text.x.family Character string, setting the font of the \eqn{x}-axis text. Following \code{html} fonts
#'   are available: "Arial" (\strong{default}), "Balto", "Courier New", "Droid Sans", "Droid Serif", "Droid Sans Mono",
#'   "Gravitas One", "Old Standard TT", "Open Sans", "Overpass", "PT Sans Narrow", "Raleway" and "Times New Roman".
#' @param axis.text.x.color Character string, pointing to color of the \eqn{x}-axis text font, \strong{default}:
#'   \code{axis.title.x.color = "black"}. Additional available colors are listed
#'   at the \href{https://www.w3.org/TR/css-color-3/}{CSS Color Module Homepage}.
#' @param axis.text.x.size Numeric, setting up the size of the \eqn{x}-axis text font in \code{px}, \strong{default}:
#'   \code{axis.title.x.size = 14}.
#' @param axis.title.y.family Character string, setting the font of the \eqn{y}-axis title (see also \code{axis.title.x.family}).
#'   \strong{Default}: \code{axis.title.y.family = "Arial"}.
#' @param axis.title.y.color Character string, pointing to color of the \eqn{y}-axis title font (see also \code{axis.title.x.color}).
#'   \strong{Default}: \code{axis.title.y.color = "black"}.
#' @param axis.title.y.size Numeric, setting up the size of the \eqn{y}-axis title font in \code{px}, \strong{default}:
#'   \code{axis.title.x.size = 15}.
#' @param axis.text.y.family Character string, setting the font of the \eqn{y}-axis text (see also \code{axis.text.x.family}).
#'   \strong{Default}: \code{axis.text.y.family = "Arial"}.
#' @param axis.text.y.color Character string, pointing to color of the \eqn{y}-axis text font (see also \code{axis.text.x.color}).
#'   \strong{Default}: \code{axis.text.y.color = "black"}.
#' @param axis.text.y.size Numeric, setting up the size of the \eqn{y}-axis text font in \code{px}, \strong{default}:
#'   \code{axis.text.x.size = 14}.
#' @param legend.title.family Character string, setting the font of the legend title. Following \code{html} fonts
#'   are available: "Arial" (\strong{default}), "Balto", "Courier New", "Droid Sans", "Droid Serif", "Droid Sans Mono",
#'   "Gravitas One", "Old Standard TT", "Open Sans", "Overpass", "PT Sans Narrow", "Raleway" and "Times New Roman".
#' @param legend.title.color Character string pointing to color of the legend title font, \strong{default}:
#'   \code{legend.title.x.color = "black"}. Additional available colors are listed
#'   at the \href{https://www.w3.org/TR/css-color-3/}{CSS Color Module Homepage}.
#' @param grid.x Logical, whether to show (\code{grid.x = TRUE}, \strong{default}) or hide the grid \eqn{x}-axis lines.
#' @param grid.y Logical, whether to show (\code{grid.y = TRUE}, \strong{default}) or hide the grid \eqn{y}-axis lines.
#' @param grid.line.width Numeric, width (in \code{px}) of the graph/plot panel grid lines, \strong{default}:
#'   \code{grid.line.width = 1.4}.
#' @param grid.line.type Character string, corresponding to type of the grid line. In addition to \code{grid.line.type = "solid"}
#'   (\strong{default}), "dot", "dash", "longdash", "dashdot", "longdashdot" can be used, as well.
#'
#'
#' @return List of the interactive plot object properties/parts, which can be used in the combination with
#'   \code{\link{plot_EPR_Specs2D_interact}} or \code{\link[plotly]{plot_ly}}.
#'
#'
#'
#' @examples
#' ## example from `plot_EPR_Specs2D_interact()`
#' ## loading the built-in CW ENDOR spectrum
#' ## of perinaphthenyl (PNT)
#' pnt.file.path <- load_data_example("PNT_ENDOR_a.txt")
#' ## read the PNT CW ENDOR data without intensity
#' ## normalization
#' pnt.endor.data <-
#'   readEPR_Exp_Specs(pnt.file.path,
#'                     col.names = c("index",
#'                                   "RF_MHz",
#'                                   "dIepr_over_dB"),
#'                     x.unit = "MHz")
#' ## plotting ENDOR spectrum
#' plot_EPR_Specs2D_interact(data.spectra = pnt.endor.data,
#'   x = "RF_MHz",
#'   x.unit = "MHz",
#'   line.colors = "darkgreen",
#'   bg.color = "cornsilk",
#'   grid.color = "darkgrey") %>%
#'     plot_layout2D_interact(xlab =
#'                      "<i>&#957;</i><sub>RF</sub> (MHz)",
#'       axis.title.x.family = "Times New Roman",
#'       axis.title.y.family = "Times New Roman",
#'       border.line.color = "blue",
#'       grid.y = FALSE
#'       )
#'
#'
#' @export
#'
#'
plot_layout2D_interact <- function(p,
                                   data = NULL,
                                   xlab = "<i>B</i> (mT)",
                                   ylab = "d <i>I</i> <sub>EPR</sub> / d <i>B</i>  (p.d.u.)",
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
                                   grid.x = TRUE,
                                   grid.y = TRUE,
                                   grid.color = "white",
                                   grid.line.width = 1,
                                   grid.line.type = "solid",
                                   border.line.width = 1.4,
                                   border.line.color = "black") {
  #
  Plotly_layout_0 <- layout(
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
      showgrid = grid.x,
      gridcolor = grid.color,
      gridwidth = grid.line.width,
      griddash = grid.line.type,
      linecolor = toRGB(border.line.color),
      linewidth = border.line.width, showline = TRUE, mirror = TRUE
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
      showgrid = grid.y,
      gridcolor = grid.color,
      gridwidth = grid.line.width,
      griddash = grid.line.type,
      linecolor = toRGB(border.line.color),
      linewidth = border.line.width, showline = TRUE, mirror = TRUE
    )
  )
  if (is.null(legend.title)) {
    Plotly_layout <- Plotly_layout_0
  } else {
    Plotly_layout <- Plotly_layout_0 %>%
      layout(legend = list(title = list(
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
