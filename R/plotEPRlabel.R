#
#' @title Labels for Various EPR Spectroscopy Plots
#'
#'
#' @description TODO To write greek symbols, subscript and superscript, see
#'   \href{https://www.r-bloggers.com/math-notation-for-r-plot-titles-expression-and-bquote/}{R-Bloggers:Math Notation}
#'   and \href{https://www.r-bloggers.com/r-plotmath-functions-combined-with-variable-values/}{R-Bloggers:Plotmath}
#'   or \code{\link[grDevices]{plotmath}} (in console write \code{?plotmath})
#'
#'
#' @param quantity Variable String (without quotation, sometimes part of it can be quoted, see examples below),
#'   physical quantity, which should be displayed as the axis title like \eqn{B},
#'   d\eqn{I}_EPR/d\eqn{B}, \eqn{time}, \eqn{\Delta B_pp}, \code{Double Integral} etc.
#' @param unit Variable String (without quotation, sometimes part of it can be quoted, see examples below),
#'   physical quanity corresponding unit, which should be displayed like \code{mT}, \code{s}, \code{p.d.u.}, etc.
#' @param user.defined Boolean, in order to bring more flexibility to display more complicated quantities and units
#'   based on users requirements (sse examples bellow)
#'
#'
#' @return TODO
#'
#'
#' @examples
#' plotEPRlabel(B,mT)
#' plotEPRlabel("d"~italic(I)[EPR]~"/"~"d"~italic(B),"("~p.d.u.~")",user.defined = TRUE)
#' plotEPRlabel(quantity = Delta*B[pp],unit = mT,user.defined = FALSE)
#' plotEPRlabel(E,"("~V~")"~~~italic(vs)~~~italic(Ref.~Electrode),user.defined = TRUE)
#' plotEPRlabel(c,mmol*dm^-3)
#'
#'
#' @export
#'
#'
#' @importFrom rlang enquo
plotEPRlabel <- function(quantity,unit,user.defined = FALSE){
  ## in 'bquote' bang-bang ('!!') operator does not work
  ## use '.()' instead
  quantity <- enquo(quantity)
  unit <- enquo(unit)
  if (isFALSE(user.defined)){
    l <- bquote(italic(.(quantity))~~"("~.(unit)~")")
  } else{
    l <- bquote(.(quantity)~~.(unit))
  }
  return(l)
}
