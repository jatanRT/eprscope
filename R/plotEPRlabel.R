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
#'   \item \code{"B_mT"} => magnetic flux density in mT (mostly for x-axis) \code{\eqn{B} (mT)}
#'   \item \code{"Deriv_Intensity"} => derivative intensity \code{d\eqn{I_{EPR}}/d\eqn{B} (p.d.u.)},
#'   "_" corresponds to subscript and p.d.u. is abbreviation of "procedure defined unit" according to
#'   \href{http://www.iupac.org/divisions/VII/VII.C.1/C-NPU_Uppsala_081023_25_minutes_confirmed.pdf}{IUPAC}
#'   \item \code{"Time_s"} => time \code{\eqn{Time} (s)}
#'   \item \code{"Integral"} => integral of the derivative intensity EPR spectrum \code{\eqn{Integral} (p.d.u.)}
#'   \item \code{"Double_Integral"} => double integral of the derivative intensity EPR spectrum
#'   \code{\eqn{Double Integral} (p.d.u.)}
#'   \item \code{"Potential_V"} => potential (for spectroelectrochemical plots) in V \code{\eqn{E} (V)}
#'   \item \code{"Concentration_mM"} => concentration (of the radicals or paramagnet. species in analyte)
#'   in mM (common concentration order used in EPR) \code{\eqn{c} (mmol dm^{-3})} "^" corresponds to superscript
#'   \item \code{"DeltaBpp_mT"} => Peak-to-Peak spectral linewidth in mT \code{\eqn{Delta B_{pp}} (mT)}
#'   \item \code{"Own"} => user defined TODO
#'   }
#' @param quantity TODO
#' @param unit TODO
#'
#'
#' @return
#'
#'
#' @examples
#' plotEPRlabel(axis = "B_mT")
#' plotEPRlabel(axis = "Deriv_Intensity")
#' plotEPRlabel(axis = "Own",quantity = m,unit = mg)
#' plotEPRlabel(axis = "Own",quantity = g) ## If the quantity is unitless
#' plotEPRlabel(axis = "DeltaBpp_mT")
#'
#' @export
#'
#'
#' @importFrom rlang enquo
plotEPRlabel <- function(axis,quantity = NULL,unit = NULL){
  quant <- enquo(quantity)
  un <- enquo(unit)
  if (axis == "B_mT"){
    return(bquote(italic(B)~"("~mT~")"))
  } else if (axis == "Deriv_Intensity"){
    return(bquote("d"~italic(I)[EPR]~"/"~"d"~italic(B)~~"("~p.d.u.~")"))
  } else if (axis == "Time_s"){
    return(bquote(italic(Time)~"("~s~")"))
  } else if (axis == "Integral"){
    return(bquote(italic(Integral)~"("~p.d.u.~")"))
  } else if (axis == "Double_Integral"){
    return(bquote(italic(Double)~~italic(Integral)~"("~p.d.u.~")"))
  } else if (axis == "Potential_V"){
    return(bquote(italic(E)~"("~V~")"))
  } else if (axis == "Own"){
    return(bquote(italic(!!quant)~"("~!!un~")"))
    ## In case, the "Own" quantity is unitless:
    if (is.null(unit)){
      return(bquote(italic(!!quant)))
    }
  } else if (axis == "Concentration_mM"){
    return(bquote(italic(c)~"("~mmol~dm^-3~")"))
  } else if (axis == "DeltaBpp_mT"){
    return(bquote(Delta~italic(B)[pp]~"("~mT~")"))
  }
}
