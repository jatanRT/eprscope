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
#'   This constant
#'   can be evaluated from the following expression =>
#'   \deqn{N_{\text{norm}} = t_{\text{C}}\,N_{\text{Scans}}\,(20)\,10^{(G_{\text{R}}/20)}}
#'   where \eqn{t_{\text{C}}} depicts the conversion time in \eqn{\text{ms}}; \eqn{N_{\text{Scans}}}
#'   corresponds to number of scans and \eqn{G_{\text{R}}} is the receiver gain in \eqn{\text{dB}}.
#'   One can gather all three parameters by \code{\link{readEPR_param_slct}} function from the
#'   corresponding `.DSC` or `.par` file. \strong{If during the recording of EPR spectra the option}
#'   `Normalize Acquisition` (in Spectrometer Configuration/Acquisition Options) \strong{is activated,
#'   THE INTENSITY is ALREADY NORMALIZED and DOESN'T REQUIRED ANY ADDITIONAL NORMALIZATION !}.
#'   See also \code{\link{quantify_EPR_abs}}...define also for "winepr" system
#'
#'
#'
#'
#' @param conv.time.ms Numeric, conversion time in milliseconds.
#' @param Nscans Numeric, number of scans.
#' @param rg Numeric, receiver gain in dB or unitless ("winepr")
#' @param sweep.width Numeric..either in "G" or "mT".tbc...\strong{Default}: NULL
#' @param origin Character string ...tbc...
#'
#'
#' @return Numeric value of normalization constant for quantitative EPR.
#'
#'
#' @examples
#' quantify_EPR_Norm_const(conv.time.ms = 6.4,
#'                         Nscans = 6,
#'                         rg = 30,
#'                         origin = "xenon")
#' #
#' quantify_EPR_Norm_const(conv.time.ms = 13.52,
#'                         sweep.width = 100,
#'                         Nscans = 100,
#'                         rg = 2.4e+4,
#'                         origin = "winepr")
#'
#' @export
#'
#'
quantify_EPR_Norm_const <- function(conv.time.ms,
                                    Nscans,
                                    sweep.width = NULL,
                                    rg,
                                    origin = "xenon") {
  #
  ## Definition of the sweep width
  sweep.width <- sweep.width %>% `if`(is.null(sweep.width),1,.)
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
    Const <- (conv.time.ms * rg * (Nscans - 1)) / sweep.width
  }
  #
  return(round(Const))
  #
}
