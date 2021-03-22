#
#' @title Find Extreme (\strong{Min}imum or \strong{Max}imum) within the EPR Spectrum (or its Integrated Form)
#'
#' @description TODO
#'
#' @param spectrum.data TODO
#' @param B TODO
#' @param Intensity TODO
#' @param B.reg.start TODO
#' @param B.reg.end TODO
#' @param extreme TODO
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
Bextreme_fromSpectrum  <-  function(spectrum.data,
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
