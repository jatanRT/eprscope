#
#' Normalization Constant Calculation for Quantitative EPR Analysis
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description Normalization constant used by \code{\link{quantify_EPR_Abs}} or to normalize
#'   the EPR spectrum intensity. Calculation depends on the receiver gain expression.
#'
#'
#' @details
#'   For the receiver gain expressed in \eqn{\text{dB}} units the normalization constant is defined
#'   by the following relation  =>
#'   \deqn{N_{\text{norm}} = t_{\text{C}}(\text{ms})\,N_{\text{Scans}}\,(20)\,
#'   10^{(G_{\text{R}}(\text{dB})/20)}}
#'   where \eqn{t_{\text{C}}(\text{ms})} depicts the conversion time in \eqn{\text{ms}};
#'   \eqn{N_{\text{Scans}}} corresponds to number of scans and \eqn{G_{\text{R}}(\text{dB})}
#'   is the receiver gain in \eqn{\text{dB}}. In the case that the receiver gain is unitless, the normalization
#'   constant is defined by =>
#'   \deqn{N_{\text{norm}} = t_{\text{C}}(\text{ms})\,G_{\text{R}}\,
#'   (N_{\text{points}} - 1)\,N_{\text{scans}}\,/\,B_{\text{SW}}(\text{G})}
#'   where \eqn{G_{\text{R}}} and \eqn{B_{\text{SW}}(\text{G})} correspond to unitless receiver gain
#'   and sweep width in Gauss, respectively. \eqn{N_{\text{points}}} equals to the number of points (resolution of
#'   an individual sweep). One can gather all parameters
#'   by the \code{\link{readEPR_param_slct}} or by the \code{\link{readEPR_params_tabs}} function from
#'   the corresponding \code{.DSC}/\code{.dsc} or \code{.par} file. \strong{If during recording of EPR spectra
#'   the option} \code{Normalize Acquisition} (in Spectrometer Configuration/Acquisition Options)
#'   \strong{is activated, THE INTENSITY is ALREADY NORMALIZED and DOESN'T REQUIRE ANY ADDITIONAL
#'   NORMALIZATION !}. Please, refer also to the \code{\link{quantify_EPR_Abs}} function.
#'
#'
#' @references
#'  Weber RT (2011). \emph{Xenon User's Guide}. Bruker BioSpin Manual Version 1.3, Software Version 1.1b50.
#'
#'  Bruker Biospin (2007). \emph{WIN-EPR User's Manual}.
#'
#'
#' @param conv.time.ms Numeric, conversion time in milliseconds.
#' @param Nscans Numeric, number of scans.
#' @param rg Numeric, receiver gain value.
#' @param rg.unit Character string corresponding to unit of the receiver gain.
#'   Either \code{rg.unit = "db"} (\code{rg.unit = "dB"}, \strong{default})
#'   or \code{rg.unit = "unitless"} (\code{rg.unit = "Unitless"}).
#' @param Npoints Numeric, number of points (resolution) corresponding to individual sweep,
#'   if \code{rg.unit = "unitless"} (\code{rg.unit = "Unitless"}). \strong{Default}: \code{Npoints = NULL}.
#' @param Bsw Numeric, experimental sweep width (magnetic flux density recording region,
#'   \eqn{B_{\text{SW}}}) in "G" if \code{rg.unit = "unitless"} (\code{rg.unit = "Unitless"}).
#'   \strong{Default}: \code{Bsw = NULL}.
#'
#'
#' @return Numeric value of the normalization constant for quantitative EPR and intensity normalization.
#'
#'
#' @examples
#' quantify_EPR_Norm_const(conv.time.ms = 8.2,
#'                         Nscans = 10,
#'                         rg = 32)
#' #
#' quantify_EPR_Norm_const(conv.time.ms = 13.1,
#'                         Bsw = 180,
#'                         Nscans = 10,
#'                         Npoints  = 1024,
#'                         rg = 3.2e+4,
#'                         rg.unit = "Unitless")
#'
#'
#' @export
#'
#'
quantify_EPR_Norm_const <- function(conv.time.ms,
                                    Nscans,
                                    Npoints = NULL,
                                    Bsw = NULL,
                                    rg,
                                    rg.unit = "dB") {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## Definition of the sweep width
  Bsw <- Bsw %>% `if`(is.null(Bsw),1,.)
  #
  ## Calculation depending on the origin
  if (rg.unit == "db" || rg.unit == "dB"){
    ## Receiver Gain devided by '20'
    rg.dB.20 <- rg / 20 ## rg in `dB`
    #
    ## Constant Calculation
    Const <- conv.time.ms * Nscans * 20 * 10^rg.dB.20
  }
  if (rg.unit == "unitless" || rg.unit == "Unitless"){
    if (is.null(Npoints) || is.null(Bsw)){
      stop(" Please provide number of points `Npoints`\n
           and magnetic flux density sweep width `Bsw` in Gauss ! ")
    } else{
      Const <- (conv.time.ms * rg * (Npoints - 1) * Nscans) / Bsw
    }
  }
  #
  return(round(Const))
  #
}
