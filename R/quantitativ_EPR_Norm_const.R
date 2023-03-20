#
#' Normalization Constant Calculation for Quantitative EPR Analysis
#'
#'
#' @description Normalization Constant used by \code{\link{quantitativ_EPR_abs}}
#'
#'
#' @param conv.time.ms Numeric, \strong{Conversion time in milliseconds}, can be obtained
#'   from the spectrometer parameters file using \code{\link{readEPR_param_slct}}.
#' @param Nscans Numeric, \strong{Number of scans}, can be obtained from the spectrometer
#'   parameters file using \code{\link{readEPR_param_slct}}.
#' @param rg.dB Numeric, \strong{Receiver gain in dB}, can be obtained from the spectrometer
#'   parameters file using \code{\link{readEPR_param_slct}}.
#'
#'
#' @return Numeric value of normalization constant for quantitative EPR
#'
#'
#' @examples
#' quantitativ_EPR_Norm_const(conv.time.ms = 6.4,
#'                           Nscans = 6,
#'                           rg.dB = 30)
#' quantitativ_EPR_Norm_const(conv.time.ms = 13.52,
#'                           Nscans = 100,
#'                           rg.dB = 24)
#'
#' @export
#'
#'
quantitativ_EPR_Norm_const <- function(conv.time.ms,
                                       Nscans,
                                       rg.dB){
  #
  ## Receiver Gain devided by '20'
  rg.dB.20 <- rg.dB/20
  #
  ## Constant Calculation
  Const <- conv.time.ms*Nscans*20*10^rg.dB.20
  #
  return(round(Const))
  #
}
