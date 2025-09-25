#
#' Present/Save Interactive Plot for Publication in \code{.html},\code{.pdf} or \code{.docx}
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#'   Add the interactive plots (see \code{\link{plot_EPR_Specs2D_interact}} or \code{\link{plot_EPR_Specs3D_interact}})
#'   to various document formats like \code{.html}, \code{.pdf} or \code{.docx}. Function is based on the
#'   \code{\link[htmlwidgets]{saveWidget}} as well as on the \code{\link[webshot2]{webshot}} with the help
#'   of \href{https://github.com/yihui/knitr}{knitr} package functions. Depending on the output format, plot is saved
#'   in working/actual directory either as \code{.html} or as \code{.png}.
#'   The file name inherits the name of the object/variable (see argument \code{p} and example). Afterwards, during
#'   the \href{https://rmarkdown.rstudio.com/docs/}{R markdown} or \href{https://quarto.org/docs/guide/}{Quarto}
#'   processing, the image is automatically attached to the document in the above-described format. Therefore, this function is quite
#'   handy in interactive notebooks (such as \code{.Rmd} or \code{.qmd}, please also consult
#'   the \code{\link{create_qmdReport_proj}} function).
#'
#'
#' @param p Plot object/variable name.
#' @param size.width Numeric, \code{width} of the image/object window in \code{in},
#'   \strong{default}: \code{size.width = 7}.
#' @param size.height Numeric, \code{height} of the image/object window in \code{in},
#'   \strong{default}: \code{size.height = 5}.
#' @param res.ppi Numeric, \code{resolution} in \code{ppi}, \strong{default}: \code{res.ppi = 200}.
#'
#' @return Interactive plot format corresponding to that of the output document. If the desired document
#'   format \eqn{\equiv} \code{.html}, interactive plotly graph is saved in working/actual directory
#'   in the same format. Otherwise, for \code{.pdf} and \code{.docx}
#'   it is saved as \code{.png} bitmap with the resolution of \code{size.with}\eqn{\cdot}\code{res.ppi} x
#'   \code{size.height}\eqn{\cdot}\code{res.ppi}.
#'
#'
#' @examples
#' \dontrun{
#' ## plotting EPR spectrum of verdazyl radical (`verdazyl.rad.data`),
#' ## where the actual plot is stored under
#' ## the `verdazyl.epr.plot.interact` variable name
#' verdazyl.epr.plot.interact <-
#'   plot_EPR_Specs2D_interact(data.spectra = verdazyl.rad.data)
#' #
#' ## afterwards, it is automatically transformed and attached
#' ## to document with the desired format by `knitr`
#' plot_EPR_present_interact(verdazyl.epr.plot.interact)
#' #
#' ## remaining image files are stored in the working directory
#' }
#'
#'
#' @export
#'
#'
plot_EPR_present_interact <- function(p,
                                      size.width = 7,
                                      size.height = 5,
                                      res.ppi = 200) {
  #
  ## widget and webshot step-by-step
  widget <- htmlwidgets::saveWidget(
    widget = p,
    file = paste0(deparse(substitute(p)), ".html"), ## to save the widget
    selfcontained = FALSE
  )
  webshot <- webshot2::webshot(
    url = paste0(deparse(substitute(p)), ".html"), ## read it from widget
    file = paste0(deparse(substitute(p)), ".png"),
    delay = 2, ## Time to wait before taking screenshot, in seconds
    ## Sometimes a longer delay is needed for all assets to display properly.
    vwidth = size.width * res.ppi,
    vheight = size.height * res.ppi,
    zoom = 1
  )
  #
  ## Conditions/document format
  if (knitr::is_latex_output() || knitr::pandoc_to(fmt = "docx")) {
    widget
    webshot ## as png
  } else {
    p ## as html
  }
}
