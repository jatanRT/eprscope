#
#' Normalization Constant Calculation for Quantitative EPR Analysis
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description Normalization constant used by \code{\link{quantify_EPR_abs}}. This constant
#'   can be evaluated from the following expression =>
#'   \deqn{N_{\text{norm}} = t_{\text{C}}\,N_{\text{Scans}}\,20\,10^{(G_{\text{R}}/20)}}
#'   where \eqn{t_{\text{C}}} depicts the conversion time in \eqn{\text{ms}}; \eqn{N_{\text{Scans}}}
#'   corresponds to number of scans and \eqn{G_{\text{R}}} is the receiver gain in \eqn{\text{dB}}.
#'   One can gather all three parameters by \code{\link{readEPR_param_slct}} function from the
#'   corresponding `.DSC` or `.par` file. \strong{If during the recording of EPR spectra the option}
#'   `Normalize Acquisition` (in Spectrometer Configuration/Acquisition Options) \strong{is activated
#'   the intensity is already normalized and it doesn't require any additional normalization.}.
#'   See also \code{\link{quantify_EPR_abs}}.
#'
#'
#'
#' @param conv.time.ms Numeric, conversion time in milliseconds.
#' @param Nscans Numeric, number of scans.
#' @param rg.dB Numeric, receiver gain in dB.
#'
#'
#' @return Numeric value of normalization constant for quantitative EPR.
#'
#'
#' @examples
#' quantify_EPR_Norm_const(conv.time.ms = 6.4,
#'                         Nscans = 6,
#'                         rg.dB = 30)
#' #
#' quantify_EPR_Norm_const(conv.time.ms = 13.52,
#'                         Nscans = 100,
#'                         rg.dB = 24)
#'
#' @export
#'
#'
quantify_EPR_Norm_const <- function(conv.time.ms,
                                    Nscans,
                                    rg.dB) {
  #
  ## Receiver Gain devided by '20'
  rg.dB.20 <- rg.dB / 20
  #
  ## Constant Calculation
  Const <- conv.time.ms * Nscans * 20 * 10^rg.dB.20
  #
  return(round(Const))
  #
}
