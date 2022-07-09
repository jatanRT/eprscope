#
#' Find Intensity Extremes within the EPR Spectrum (or its Integrated Form).
#'
#'
#' @description Finding positions (\emph{B} in \code{mT} or \code{G}) of intensity minimum and/or maximum
#' within selected region of the EPR spectrum
#'
#'
#' @param spectrum.data EPR spectrum data frame/table with magnetic flux density (in \code{mT} or \code{G}) column
#'   and that of the derivative \code{dIepr_over_dB} or integrated e.g. \code{Intensity}.
#'   \code{Index} column may be included as well
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{spectrum.data} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} (\strong{default})
#'   or \code{B = "B_G"} or \code{B = "B_G_Sim"} to include simulated EPR spectra as well
#' @param Intensity Character/String pointing to \code{intensity column} if other than \code{dIepr_over_dB}
#'   name/label is used (e.g. for simulated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \emph{B} region, e.g. like `Blim = c(3495.4,3595.4)`
#' @param extreme Character/String with only a two values allowed: \code{"min"} or \code{"max"}
#'
#' @returns Numeric value of magnetic flux density \emph{B} corresponding to \code{minimal} or \code{maximal}
#'   \code{intensity} within the EPR spectra (or their integrated form)
#'
#'
#' @examples
#' \dontrun{
#' Bextreme_Spec(spectrum.data,Blim = c(349.54,359.54),extreme = 'maximum')
#' Bextreme_Spec(data.frame,"B_G",Intensity = "Integrated_Intensity",c(3495.4,3595.4),'minimum')
#' }
#'
#'
#' @export
#'
#'
## COMMENT Find B-extremes (magnetic flux density) in EPR spectra:
## Function to find B ('B_mT' or 'B_G') corresponding to max. or min.
## intensity ('dIepr_over_dB' or other name/label). To select a region / span / interval
## (limits are `Blim`, `Blim[1]` = 'start',`Blim[2]` = 'end')
## of single spectrum in which the extreme ("max" or "min") needs to be found
Bextreme_Spec  <-  function(spectrum.data,
                            B = "B_mT",
                            Intensity = "dIepr_over_dB",
                            Blim,
                            extreme){
  if (extreme == "min"){
    B.min  <-  spectrum.data  %>%
      filter(between(.data[[B]],Blim[1],Blim[2]))  %>%
      filter(.data[[Intensity]] == min(.data[[Intensity]]))  %>%
      pull(.data[[B]])
    #
    return(round(B.min,digits = 2))
    #
  }
  if (extreme == "max"){
    B.max  <-  spectrum.data  %>%
      filter(between(.data[[B]],Blim[1],Blim[2]))  %>%
      filter(.data[[Intensity]] == max(.data[[Intensity]]))  %>%
      pull(.data[[B]])
    #
    return(round(B.max,digits = 2))
    #
  }
}
