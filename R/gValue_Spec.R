#
#' @title Calculation of \eqn{g}-factor ("Position") from the EPR Spectrum/Data
#'
#' @description Calculation of g-value according to fundamental formula (\code{\link{gValue}}).
#'   \eqn{g}-related magnetic flux density (like \eqn{B_{iso}} or \eqn{B_{center}}) is directly taken
#'   from the EPR spectrum. If positive and negative derivative intensities of the spectral line are similar
#'   and their distance from the middle of the spectrum equals, the \eqn{B_{iso}} should be be considered,
#'   otherwise the \eqn{B_{center}} must be taken into account.
#'
#'
#' @param data.spectrum Spectrum data frame/table where the magnetic flux density (in \code{mT} or \code{G}) column
#'   must be labeled as \code{B_mT} (or \code{B_G}) and that of the derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column can be included as well
#' @param nu.GHz Numeric, microwave frequency in \code{GHz}
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{data.spectrum} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} (\strong{default})
#'   or \code{B = "B_G"} or \code{B = "B_G_Sim"} to include simulated EPR spectra as well
#' @param Intensity Character/String pointing to \code{intensity column} if other than \code{dIepr_over_dB}
#'   name/label is used (e.g. for simulated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `Blim = c(3495.4,3595.4)`
#' @param iso Boolean, whether to calculate the \eqn{g}-factor from the \eqn{B} value corresponding to
#'   that between the \code{min.} and \code{max.} derivative intensities (\code{dIepr_over_dB}, that is \eqn{g_{iso}}
#'   (this is the \strong{default}: \code{iso = TRUE}), or by finding the the \eqn{B} value corresponding
#'   to \code{dIepr_over_dB = 0} (close/near zero, which is \code{iso = FALSE})
#'
#' @return Numeric \eqn{g_{iso}}-value ('iso' = 'isotropic') according to \eqn{(\nu h)/(\mu_{B} B)}
#'
#'
#' @examples
#' \dontrun{
#' gValue_Spec(data.spectrum,9.82451,"B_mT",Intensity = "dIepr_over_dB_Sim",c(349.8841,351.112))
#' gValue_Spec(data.spectrum,nu.GHz = 9.82451,B = "B_G",Blim = c(3498.841,3511.12),iso = FALSE)
#' gValue_Spec(data.spectrum,9.91024,B = "B_G_Sim",c(3499,3501))
#' }
#'
#'
#' @export
#'
#'
#' @importFrom dplyr filter select mutate pull between near
gValue_Spec <- function(data.spectrum,
                                nu.GHz,
                                B = "B_mT",
                                Intensity = "dIepr_over_dB",
                                Blim,
                                iso = TRUE){
  ## 'Temporary' processing variables
  AbsIntens <- NULL
  #
  ## B minimum & maximum
  B.min <- data.spectrum %>%
    filter(between(.data[[B]],Blim[1],Blim[2])) %>%
    filter(.data[[Intensity]] == min(.data[[Intensity]])) %>%
    pull(.data[[B]])
  #
  B.max <- data.spectrum %>%
    filter(between(.data[[B]],Blim[1],Blim[2])) %>%
    filter(.data[[Intensity]] == max(.data[[Intensity]])) %>%
    pull(.data[[B]])
  ## B between minimum and maximum of dIepr_over_dB:
  if (isTRUE(iso)){
    B.center <- (B.min + B.max)/2
    ## B at dIepr_over_dB = 0 (near 0, see next comment):
  } else{
    ## Find the value B, corresponding to Intensity very close to 0 (tolerance max(Intensity)/100)
    B.center <- data.spectrum %>%
      filter(between(.data[[B]],B.max,B.min)) %>%
      mutate(AbsIntens = abs(.data[[Intensity]])) %>%
      filter(near(AbsIntens,0,tol = max(.data[[Intensity]])/100)) %>%
      filter(AbsIntens == min(AbsIntens)) %>%
      pull(.data[[B]])
  }
  ## g -value calculation:
  Planck.const <- constants::syms$h
  Bohr.magnet <- constants::syms$mub
  g.precurs <- (Planck.const*nu.GHz*1e+9)/(Bohr.magnet*B.center)
  #
  ## Conditions for B column, the name should contain ("B", "mT" or "G"):
  if (sjmisc::str_contains(B,c("B","mT"),logic = "and",ignore.case = F)){
    g <- g.precurs/1e-3
  }
  if (sjmisc::str_contains(B,c("B","G"),logic = "and",ignore.case = F)){
    g <- g.precurs/1e-4
  }
  #
  return(round(g,digits = 5))
  #
}
