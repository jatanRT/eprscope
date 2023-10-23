#
#' Normalization Constant Calculation for Quantitative EPR Analysis
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description Normalization constant used by \code{\link{quantify_EPR_abs}} or to normalize
#'   the EPR spectrum intensity. The calculation depends on the acquisition/processing software
#'   characterized by the \code{origin} argument.
#'
#'
#' @details
#'   The normalization constant is defined by expression used in "xenon" processing/acquisition
#'   software =>
#'   \deqn{N_{\text{norm}} = t_{\text{C}}(\text{ms})\,N_{\text{Scans}}\,(20)\,
#'   10^{(G_{\text{R}}(\text{dB})/20)}}
#'   where \eqn{t_{\text{C}}(\text{ms})} depicts the conversion time in \eqn{\text{ms}};
#'   \eqn{N_{\text{Scans}}} corresponds to number of scans and \eqn{G_{\text{R}}(\text{dB})}
#'   is the receiver gain in \eqn{\text{dB}}. Within the "winepr" sofware, the normalization constant
#'   is defined by following relation =>
#'   \deqn{N_{\text{norm}} = t_{\text{C}}(\text{ms})\,G_{\text{R}}\,
#'   (N_{\text{Scans}} - 1)\,/\,B_{\text{SW}}(\text{G})}
#'   where \eqn{G_{\text{R}}} and \eqn{B_{\text{SW}}(\text{G})} correspond to unitless receiver gain
#'   and sweep width in Gauss, respectively. One can gather all parameters
#'   by \code{\link{readEPR_param_slct}} or by \code{\link{readEPR_params_tabs}} functions from
#'   the corresponding `.DSC` or `.par` file. \strong{If during the recording of EPR spectra
#'   the option} `Normalize Acquisition` (in Spectrometer Configuration/Acquisition Options)
#'   \strong{is activated, THE INTENSITY is ALREADY NORMALIZED and DOESN'T REQUIRED ANY ADDITIONAL
#'   NORMALIZATION !}. See also \code{\link{quantify_EPR_abs}}.
#'
#'
#'
#' @inheritParams readEPR_param_slct
#' @param conv.time.ms Numeric, conversion time in milliseconds.
#' @param Nscans Numeric, number of scans.
#' @param rg Numeric, receiver gain in dB (in case if \code{origin = "xenone"}) or unitless
#'   (in case if \code{origin = "winepr"}).
#' @param sw Numeric, experimental sweep width (magnetic flux density recording region,
#'   \eqn{B_{\text{SW}}}) in "G". \strong{Default}: \code{sw = NULL}.
#'
#'
#' @return Numeric value of normalization constant for quantitative EPR and intensity normalization.
#'
#'
#' @examples
#' quantify_EPR_Norm_const(conv.time.ms = 8.2,
#'                         Nscans = 10,
#'                         rg = 32,
#'                         origin = "xenon")
#' #
#' quantify_EPR_Norm_const(conv.time.ms = 4.1,
#'                         sw = 180,
#'                         Nscans = 10,
#'                         rg = 3.2e+4,
#'                         origin = "winepr")
#'
#'
#' @export
#'
#'
quantify_EPR_Norm_const <- function(conv.time.ms,
                                    Nscans,
                                    sw = NULL,
                                    rg,
                                    origin = "xenon") {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## Definition of the sweep width
  sw <- sw %>% `if`(is.null(sw),1,.)
  #
  ## Calculation depending on the origin
  if (origin == "xenon"){
    ## Receiver Gain devided by '20'
    rg.dB.20 <- rg / 20 ## rg in `dB`
    #
    ## Constant Calculation
    Const <- conv.time.ms * Nscans * 20 * 10^rg.dB.20
  }
  if (origin == "winepr"){
    Const <- (conv.time.ms * rg * (Nscans - 1)) / sw
  }
  #
  return(round(Const))
  #
}
