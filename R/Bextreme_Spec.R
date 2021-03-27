#
#' @title Find Extreme (\strong{Min}imum or \strong{Max}imum) within the EPR Spectrum (or its Integrated Form)
#'
#' @description TODO
#'
#' @param spectrum.data Spectrum data frame/table with magnetic flux density (in \code{mT} or \code{G}) column
#'   must be labeled as \code{B_mT} (or \code{B_G}) and that of the derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column may be included as well, integrated/simulated spectra (incl. other \code{Intensity}
#'   and \code{B} columns) can be read as well
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{spectrum.data} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} (\strong{default})
#'   or \code{B = "B_G"} or \code{B = "B_G_Sim"} to include simulated EPR spectra as well
#' @param Intensity Character/String pointing to \code{intensity column} if other than \code{dIepr_over_dB}
#'   name/label is used (e.g. for simulated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param B.reg.start Numeric, magnetic flux density in \code{mT} corresponding to \code{starting} border
#'   of the \code{selected B region} (therefore abbreviation \code{.reg.})
#' @param B.reg.end Numeric, magnetic flux density in \code{mT} corresponding to \code{ending} border
#'   of the \code{selected B region} (therefore abbreviation \code{.reg.})
#' @param extreme Character/String with only a two values allowed: \code{"min"} or \code{"max"}
#'
#' @return TODO
#'
#' @examples
#' TODO
#' TODO
#'
#'
#' @export
#'
## COMMENT Find B-extremes (magnetic flux density) in EPR spectra:
## Function to find B ('B_mT' or 'B_G') corresponding to max. or min.
## intensity ('dIepr_over_dB' or other name/label). To select a region / span / interval
## (limits are 'B.reg.start','B.reg.end')
## of single spectrum in which the extrem ("max" or "min") has to be found
Bextreme_Spec  <-  function(spectrum.data,
                                   B = "B_mT",
                                   Intensity = "dIepr_over_dB",
                                   B.reg.start,
                                   B.reg.end,
                                   extreme){
  if (extreme == "min"){
    B.min  <-  spectrum.data  %>%
      filter(between(.data[[B]],B.reg.start,B.reg.end))  %>%
      filter(.data[[Intensity]] == min(.data[[Intensity]]))  %>%
      pull(.data[[B]])
    return(B.min)
  }
  if (extreme == "max"){
    B.max  <-  spectrum.data  %>%
      filter(between(.data[[B]],B.reg.start,B.reg.end))  %>%
      filter(.data[[Intensity]] == max(.data[[Intensity]]))  %>%
      pull(.data[[B]])
    return(B.max)
  }
}
