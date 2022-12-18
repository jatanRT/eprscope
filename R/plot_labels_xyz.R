#
#' Labels for Various Plots (Spectroscopy incl. EPR, Voltammetry,...etc)
#'
#'
#' @description TODO To write greek symbols, subscript and superscript, see
#'   \href{https://www.r-bloggers.com/math-notation-for-r-plot-titles-expression-and-bquote/}{R-Bloggers:Math Notation}
#'   and \href{https://www.r-bloggers.com/r-plotmath-functions-combined-with-variable-values/}{R-Bloggers:Plotmath}
#'   or \code{\link[grDevices]{plotmath}} (in console write \code{?plotmath})
#'
#'
#' @param quantity Variable String (without quotation however, sometimes part of it can be quoted, see examples below),
#'   physical quantity, which should be displayed as the axis title like \eqn{B},
#'   d\eqn{I_{EPR}}/d\eqn{B}, \eqn{time}, \eqn{\Delta B_{pp}}, \emph{Double Integral} etc.
#' @param unit Variable String (without quotation, sometimes part of it can be quoted, see examples below),
#'   physical quanity corresponding unit, which should be displayed like \code{mT}, \code{s}, \code{p.d.u.}, etc.
#' @param user.defined Boolean, in order to bring more flexibility to display more complicated quantities and units
#'   based on users requirements (see examples bellow)
#'
#'
#' @return Axis labels for different plots
#'
#'
#' @examples
#' plot_labels_xyz(B,mT)
#' plot_labels_xyz("d"~italic(I)[EPR]~"/"~"d"~italic(B),"("~p.d.u.~")",user.defined = TRUE)
#' plot_labels_xyz(quantity = Delta*B[pp],unit = mT,user.defined = FALSE)
#' plot_labels_xyz(E,"("~V~")"~~~italic(vs)~~~italic(Ref.~Electrode),user.defined = TRUE)
#' plot_labels_xyz(c,mmol*dm^-3)
#' plot_labels_xyz(Double~~Integral,p.d.u.)
#'
#'
#' @export
#'
#'
#' @importFrom rlang enquo
plot_labels_xyz <- function(quantity,unit,user.defined = FALSE){
  #
  ## in 'bquote' bang-bang ('!!') operator does not work
  ## use '.()' instead
  quantity <- enquo(quantity)
  unit <- enquo(unit)
  if (isFALSE(user.defined)){
    l <- bquote(italic(.(quantity))~~"("~.(unit)~")")
  } else{
    l <- bquote(.(quantity)~~.(unit))
  }
  #
  return(l)
  #
}
