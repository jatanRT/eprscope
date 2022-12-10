#
#' Polynomial Baseline Correction for single-Integrated EPR Spectra + Double Integral Calculation/Presentation
#'
#'
#' @description
#' tbc
#'
#'
#' @param data.spec.integ Spectrum data frame/table with magnetic flux density (in \code{mT} or \code{G}) column
#'   must be labeled as \code{B_mT} (or \code{B_G}) and that of the derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column may be included as well, \code{sinlge integrated EPR} \strong{spectrum must be included}
#'   in column named by \code{"single|sinteg|s_integ|single_|singleinteg|sintegral|sInteg_"} ("|" == "or" operator),
#'   this can be obtained by \code{\link[pracma:trapz]{pracma::cumtrapz}} function from \emph{B} and \emph{dIepr_over_dB}
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{spectrum.data} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"}
#'   or \code{B = "B_G"} (\strong{default})
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `Blim = c(3495.4,3595.4)`
#' @param BpeaKlim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `BpeaKlim = c(3535.4,3555.4)`
#' @param poly.degree Numeric, degree of polynomial function used to fit the baseline under the single integrated
#'   curve of \emph{dIepr_over_dB}
#' @param double.integ Boolean, whether to present (column in data frame) the double integral of \emph{dIepr_over_dB},
#'   which is required for quantitative analysis, \strong{default}: \code{double.integ = FALSE}
#'
#'
#' @return Data frame/table including the EPR spectral data (\emph{dIepr_over_dB} vs \eqn{B}) as well as its
#'   corresponding \code{single} (column \code{sIntegCorr}) and/or \code{double} (column \code{dIntegCorr})
#'   integral corrected against the baseline fit
#'
#'
#'
#' @examples
#' tbc
#'
#'
#' @export
#'
#'
integ_correct_EPRspecs <- function(data.spec.integ,
                                   B = "B_G",
                                   Blim,
                                   BpeaKlim,
                                   poly.degree,
                                   double.integ = FALSE){
  ## 'Temporary' processing variables
  sIntegBaseLinFit <- NULL
  sIntegCorr <- NULL
  #
  ## Intensity column from spe.integ.data
  integ.string <- str_subset(colnames(data.spec.integ),
                             regex("single|sinteg|s_integ|single_|singleinteg|sintegral|sInteg_",
                                   ignore_case = T))
  #
  ## select a region / range / interval of a integrated spectrum
  ## in which the second integral will be performed
  ## (limits are 'Blim[1]'<=> 'start','Blim[2]' <=> 'end'):
  data.slct <- data.spec.integ %>%
    filter(between(.data[[B]],Blim[1],Blim[2]))
  #
  ## select region / range / interval of the peak, which will be not
  ## considered ("!") for the baseline correction / fit
  ## (limits are 'BpeaKlim[1]'<=> 'start','BpeaKlim[2]' <=> 'end'):
  data.NoPeak <- data.slct %>%
    filter(!between(.data[[B]],BpeaKlim[1],BpeaKlim[2]))
  #
  ## Polynomial baseline fit:
  integ.baseline.fit <- stats::lm(.data[[integ.string]] ~ stats::poly(.data[[B]],degree = poly.degree),
                           data = data.NoPeak)
  #
  ## apply fit to data.slct, remove the .resid colum (which is not required),
  ## rename column with fit, subtract the baseline,
  ## then shift the integral baseline up having all the values > 0 (subtract its minimum)
  ## and finally calculate the double integral
  data.slct <- broom::augment(integ.baseline.fit,newdata = data.slct) %>%
    dplyr::select(-.data[[".resid"]]) %>%
    dplyr::rename(sIntegBaseLinFit = .data[[".fitted"]]) %>%
    dplyr::mutate(sIntegCorr = .data[[integ.string]] - .data$sIntegBaseLinFit) %>%
    dplyr::select(-sIntegBaseLinFit) %>%
    dplyr::mutate(sIntegCorr = sIntegCorr - min(.data$sIntegCorr))
  #
  ## double integral calculation:
  if (isTRUE(double.integ)){
    result.integ.data <- data.slct %>%
      dplyr::mutate(dIntegCorr = pracma::cumtrapz(.data[[B]],sIntegCorr))
  } else{
    result.integ.data <- data.slct
  }
  #
  return(result.integ.data)
  #
}
