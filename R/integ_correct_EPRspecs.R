#
#' @title Polynomial Baseline Correction for single-Integrated EPR Spectra + Double Integral Calculation/Presentation
#'
#'
#' @description TODO
#'
#'
#' @param spec.integ.data Spectrum data frame/table with magnetic flux density (in \code{mT} or \code{G}) column
#'   must be labeled as \code{B_mT} (or \code{B_G}) and that of the derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column may be included as well, \code{sinlge integrated EPR} \strong{spectrum must be included}
#'   in column named by \code{"single|sinteg|s_integ|single_|singleinteg|sintegral|sInteg_"} ("|" == "or" operator),
#'   this can be obtained by \code{\link[pracma:trapz]{pracma::cumtrapz}} function from \emph{B} and \emph{dIepr_over_dB}
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{spectrum.data} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"}
#'   or \code{B = "B_G"} (\strong{default})
#' @param B.reg.start TODO
#' @param B.reg.end TODO
#' @param B.peak.start TODO
#' @param B.peak.end TODO
#' @param poly.degree TODO
#' @param double.integ TODO
#'
#' @return data frame TODO
#'
#'
#'
#' @examples
#'
#'
#' @export
#'
#'
integ_correct_EPRspecs <- function(spec.integ.data,
                                   B = "B_G",
                                   B.reg.start,
                                   B.reg.end,
                                   B.peak.start,
                                   B.peak.end,
                                   poly.degree,
                                   double.integ = FALSE){
  #
  ## Intensity column from spe.integ.data
  integ.string <- str_subset(colnames(spec.integ.data),
                             regex("single|sinteg|s_integ|single_|singleinteg|sintegral|sInteg_",
                                   ignore_case = T))
  #
  ## select a region / range / interval of a integrated spectrum
  ## in which the second integral will be performed
  ## (limits are 'B.reg.start','B.reg.end'):
  data.slct <- spec.integ.data %>%
    filter(between(.data[[B]],B.reg.start,B.reg.end))
  #
  ## select region / range / interval of the peak, which will be not
  ## considered ("!") for the baseline correction / fit
  ## (limits are 'B.peak.start','B.peak.end'):
  data.NoPeak <- data.slct %>%
    filter(!between(.data[[B]],B.peak.start,B.peak.end))
  #
  ## Polynomial baseline fit:
  integ.baseline.fit <- lm(.data[[integ.string]] ~ stats::poly(.data[[B]],degree = poly.degree),
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
    dplyr::select(-sIntegBaselinFit) %>%
    dplyr::mutate(sIntegCorr - min(.data$sIntegCorr))
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
