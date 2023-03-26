#
#' Polynomial Baseline Correction for single-Integrated EPR Spectra
#'
#'
#' @description
#'  Single-integrated spectrum (from original derivative form) can be obtained
#'  by \code{\link[pracma:trapz]{pracma::cumtrapz}} function
#'  taken into account \emph{B} and \emph{dIepr_over_dB}. Double Integral Calculation/Presentation
#'  may be also provided by this function, if \code{double.integ = T}
#'
#'
#' @param data.spec.integ Spectrum data frame/table with magnetic flux density (in \code{mT} or \code{G}) column
#'   must be labeled as \code{B_mT} (or \code{B_G}) and that of the derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column may be included as well, \code{sinlge integrated EPR} \strong{spectrum must be included}
#'   as column.
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{spectrum.data} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"}
#'   or \code{B = "B_G"} (\strong{default})
#' @param integ.single Character/String pointing to single integral \code{column} of EPR spectrum data frame
#'   \code{spectrum.data} (\strong{default}: \code{integ.single = "single_integ"})
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `Blim = c(3495.4,3595.4)`. \strong{Default}: \code{Blim = NULL} (corresponding
#'   to entire `B` range).
#' @param BpeaKlim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `BpeaKlim = c(3535.4,3555.4)`
#' @param poly.degree Numeric, degree of polynomial function used to fit the baseline under the single integrated
#'   curve of \emph{dIepr_over_dB}
#' @param double.integ Boolean, whether to present (column in data frame) the double integral of \emph{dIepr_over_dB},
#'   which is required for quantitative analysis, \strong{default}: \code{double.integ = FALSE}
#' @param output.vector Boolean, whether to "vectorize" the result in order to use it (in a form of additional
#'   data frame column) for quantitative analysis of a EPR spectral series (for each individual spectrum).
#'   \strong{Default}: \code{output.vector = FALSE}
#'
#'
#' @return Data frame/table  (or vector/column) including the EPR spectral data (\emph{dIepr_over_dB} vs \eqn{B}) as well as its
#'   corresponding \code{single} (column \code{sIntegCorr}) and/or \code{double} (column \code{dIntegCorr})
#'   integral corrected against the baseline fit.
#'
#'
#'
#' @examples
#' \dontrun{
#' correct_integ_EPR_Specs(EPR_spectral_data_table,
#'                         B = "B_mT",
#'                         Blim = c(348.2,351.1),
#'                         BpeaKlim = c(349,350),
#'                         poly.degree = 3,
#'                         output.vector = TRUE)
#' }
#'
#'
#' @export
#'
#'
#' @importFrom pracma cumtrapz
correct_integ_EPR_Specs <- function(data.spec.integ,
                                    B = "B_G",
                                    integ.single = "single_integ",
                                    Blim = NULL,
                                    BpeaKlim,
                                    poly.degree,
                                    double.integ = FALSE,
                                    output.vector = FALSE) {
  ## 'Temporary' processing variables
  sIntegBaseLinFit <- NULL
  sIntegCorr <- NULL
  #
  ## Define limits
  if (is.null(Blim)) {
    ## the entire data region
    Blim <- c(min(data.spec.integ[[B]]), max(data.spec.integ[[B]]))
  } else {
    ## otherwise use predefined vector
    Blim <- Blim
  }
  #
  ## select a region / range / interval of a integrated spectrum
  ## in which the second integral will be performed
  ## (limits are 'Blim[1]'<=> 'start','Blim[2]' <=> 'end'):
  data.slct <- data.spec.integ %>%
    filter(between(.data[[B]], Blim[1], Blim[2]))
  #
  ## select region / range / interval of the peak, which will be not
  ## considered ("!") for the baseline correction / fit
  ## (limits are 'BpeaKlim[1]'<=> 'start','BpeaKlim[2]' <=> 'end'):
  data.NoPeak <- data.slct %>%
    filter(!between(.data[[B]], BpeaKlim[1], BpeaKlim[2]))
  #
  ## Polynomial baseline fit:
  integ.baseline.fit <- stats::lm(.data[[integ.single]] ~ stats::poly(.data[[B]], degree = poly.degree),
    data = data.NoPeak
  )
  #
  ## apply fit to data.slct, remove the .resid colum (which is not required),
  ## rename column with fit, subtract the baseline,
  ## then shift the integral baseline up having all the values > 0 (subtract its minimum)
  ## and finally calculate the double integral
  data.slct <- broom::augment(integ.baseline.fit, newdata = data.slct) %>%
    dplyr::select(-.data[[".resid"]]) %>%
    dplyr::rename(sIntegBaseLinFit = .data[[".fitted"]]) %>%
    dplyr::mutate(sIntegCorr = .data[[integ.single]] - .data$sIntegBaseLinFit) %>%
    dplyr::select(-sIntegBaseLinFit) %>%
    dplyr::mutate(sIntegCorr = sIntegCorr - min(.data$sIntegCorr))
  #
  ## double integral calculation:
  if (isTRUE(double.integ)) {
    if (isTRUE(output.vector)) {
      result.integ.data <- pracma::cumtrapz(data.slct[[B]], data.slct$sIntegCorr)[, 1]
    } else {
      result.integ.data <- data.slct %>%
        dplyr::mutate(dIntegCorr = pracma::cumtrapz(.data[[B]], sIntegCorr)[, 1])
    }
  } else {
    if (isTRUE(output.vector)) {
      result.integ.data <- data.slct$sIntegCorr
    } else {
      result.integ.data <- data.slct
    }
  }
  #
  return(result.integ.data)
  #
}
