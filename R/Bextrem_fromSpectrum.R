#
#' @title TODO
#'
#' @description TODO
#'
#' @param spectrum.data TODO
#' @param B TODO
#' @param B.reg.start TODO
#' @param B.reg.end TODO
#' @param extrem TODO
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
## intensity ('dIepr_over_dB'). To select a region / span / interval
## (limits are 'B.reg.start','B.reg.end')
## of single spectrum in which the extrem ("max" or "min") has to be found
Bextrem_fromSpectrum  <-  function(spectrum.data,B = "B_mT",B.reg.start,B.reg.end,extrem){
  if (extrem == "min"){
    B.min  <-  spectrum.data  %>%
      filter(between(.data[[B]],B.reg.start,B.reg.end))  %>%
      filter(.data$dIepr_over_dB == min(.data$dIepr_over_dB))  %>%
      pull(.data[[B]])
    return(B.min)
  }
  if (extrem == "max"){
    B.max  <-  spectrum.data  %>%
      filter(between(.data[[B]],B.reg.start,B.reg.end))  %>%
      filter(.data$dIepr_over_dB == max(.data$dIepr_over_dB))  %>%
      pull(.data[[B]])
    return(B.max)
  }
}
