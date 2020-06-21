#
#' @title Labels for Various EPR Spectroscopy Plots
#'
#'
#' @description TODO
#'
#'
#' @param axis String, which corresponds to axis label, labels
#'   are defined for the following quantities and their units:
#'   \itemize{
#'   \item \code{"B_mT"} => magnetic flux density in mT (mostly for x-axis) \eqn{B} (mT)
#'   \item \code{"Deriv_Intensity"} => derivative intensity d\eqn{I_EPR}/d\eqn{B} (p.d.u.),
#'   "_" corresponds to subscript and p.d.u. is abbreviation of "procedure defined unit" according to
#'   \href{http://www.iupac.org/divisions/VII/VII.C.1/C-NPU_Uppsala_081023_25_minutes_confirmed.pdf}{IUPAC}
#'   \item \code{"Time_s"} => time \eqn{Time} (s)
#'   \item \code{"Integral"} => integral of the derivative intensity EPR spectrum \eqn{Integral} (p.d.u.)
#'   \item \code{"Double_Integral"} => double integral of the derivative intensity EPR spectrum
#'   \eqn{Double Integral (p.d.u.)}
#'   \item \code{"Potential_V"} => potential (for spectroelectrochemical plots) in V \eqn{E} (V)
#'   \item \code{"Concentration_mM"} => concentration (of the radicals or paramagnet. species in analyte)
#'   in mM (common concentration order used in EPR) \eqn{c} (mmol dm^-3) "^" corresponds to superscript
#'   \item \code{"DeltaBpp_mT"} => Peak-to-Peak spectral linewidth in mT \eqn{\Delta B_pp} (mT)
#'   }
#'
#'
#' @return TODO
#'
#'
#' @examples
#' plotEPRlabel(axis = "B_mT")
#' plotEPRlabel(axis = "Deriv_Intensity")
#' plotEPRlabel(axis = "DeltaBpp_mT")
#'
#' @export
#'
#'
plotEPRlabel <- function(axis){
  if (axis == "B_mT"){
    l <- bquote(italic(B)~"("~mT~")")
  } else if (axis == "Deriv_Intensity"){
    l <- bquote("d"~italic(I)[EPR]~"/"~"d"~italic(B)~~"("~p.d.u.~")")
  } else if (axis == "Time_s"){
    l <- bquote(italic(Time)~"("~s~")")
  } else if (axis == "Integral"){
    l <- bquote(italic(Integral)~"("~p.d.u.~")")
  } else if (axis == "Double_Integral"){
    l <- bquote(italic(Double)~~italic(Integral)~"("~p.d.u.~")")
  } else if (axis == "Potential_V"){
    l <- bquote(italic(E)~"("~V~")")
  } else if (axis == "Concentration_mM"){
    l <- bquote(italic(c)~"("~mmol~dm^-3~")")
  } else if (axis == "DeltaBpp_mT"){
    l <- bquote(Delta~italic(B)[pp]~"("~mT~")")
  }
  return(l)
}
