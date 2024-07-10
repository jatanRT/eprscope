#
#'
#' Calculation of Spectral Linewidth ( e.g. \eqn{\Delta B_{pp}}) of the EPR/ENDOR Spectrum
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   Calculating the \code{peak-to-peak} (distance between of the \emph{x}-axis
#'   projection of "min" and "max" derivative intensities) linewidth of an EPR/ENDOR spectrum.
#'
#'
#' @param data.spectr EPR/ENDOR spectrum data frame object with magnetic flux density \emph{B} (in \code{mT} or \code{G})
#'   or \emph{g}-Value or \emph{RF} (in \code{MHz}) column/variable and that of the derivative \code{dIepr_over_dB}
#'   \code{Intensity}. \code{Index} column may be included as well.
#' @param x Character string pointing to name of the \code{x}-axis/column/variable (in the original \code{data.spectr})
#'   like magnetic flux density \eqn{B}, \eqn{g}-Value or \eqn{RF} (radio frequency), \strong{default}: \code{x = "B_mT"}.
#' @param Intensity Character string pointing to name of the \code{intensity column/variable}, in the original \code{data.spectr},
#'   if other than \code{dIepr_over_dB} name/label is used (e.g. for simulated spectra).
#'   \strong{Default}: \code{Intesity = "dIepr_over_dB"}.
#' @param xlim Numeric vector corresponding to border limits of the selected \emph{x} region,
#'   e.g. like \code{xlim = c(3495.4,3595.4)} (\emph{B} in \code{G}) or \code{xlim = c(12.5,21.2)} (\emph{RF} in \code{MHz})
#'   or \code{xlim = c(2.004,2.001)} (\emph{g} dimensionless). \strong{Default}: \code{xlim = NULL} (corresponding
#'   to the entire \code{x} range).
#'
#'
#' @return Numeric value difference of the \code{x}-axis quantity like \emph{B},\emph{g},\emph{RF} (the absolute value)
#'   corresponding to \code{minimum} and \code{maximum} of the derivative intensity (\code{dIepr_over_dB})
#'   in EPR/ENDOR spectrum.
#'
#'
#' @examples
#' ## loading the aminoxyl radical CW EPR spectrum
#' aminoxyl.data.path <-
#' load_data_example(file = "Aminoxyl_radical_a.txt")
#' aminoxyl.data <-
#' readEPR_Exp_Specs(aminoxyl.data.path,qValue = 2100)
#' #
#' ## evaluation of the central linewidth (\Delta Bpp in `mT`):
#' eval_DeltaXpp_Spec(aminoxyl.data,xlim = c(348.345,350.450))
#' #
#' ## plot interactive spectrum
#' plot_EPR_Specs2D_interact(aminoxyl.data)
#' #
#' ## the linewidth \Delta Bpp (in `mT`) may be directly
#' ## checked from the interactive spectrum above
#' ## as a difference between the minimum and maximum
#' ## of the `dIepr_over_dB` central line intensity:
#' 349.648-349.107
#' #
#' ## loading the perinaphthenyl (PNT) CW ENDOR spectrum
#' pnt.endor.data.path <-
#' load_data_example(file = "PNT_ENDOR_a.txt")
#' pnt.endor.data <-
#' readEPR_Exp_Specs(pnt.endor.data.path,
#'                   col.names = c("index",
#'                                 "RF_MHz",
#'                                 "dIepr_over_dB"),
#'                  x.id = 2,
#'                  x.unit = "MHz",
#'                  Intensity.id = 3
#'                  )
#' #
#' ## evaluation of the fourth linewidth (\Delta freq.(pp)
#' ## in `MHz`) in the ENDOR spectrum:
#' eval_DeltaXpp_Spec(pnt.endor.data,
#'                   x = "RF_MHz",
#'                   xlim = c(22.38,24.54)
#'                   )
#' #
#' ## plot interactive ENDOR spectrum
#' plot_EPR_Specs2D_interact(pnt.endor.data,
#'                           x = "RF_MHz",
#'                           x.unit = "MHz")
#' #
#' ## the linewidth (\Delta freq.(pp) in `MHz`) may be
#' ## directly checked from the previous interactive
#' ## CW ENDOR spectrum as a difference between
#' ## the minimum and maximum of the `dIepr_over_dB`
#' ## fourth line intensity:
#' 23.42-23.30
#'
#'
#' @export
#'
#'
eval_DeltaXpp_Spec <- function(data.spectr,
                               x = "B_mT",
                               Intensity = "dIepr_over_dB",
                               xlim = NULL) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## Define limits if `xlim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.x.region <- c(min(data.spectr[[x]]), max(data.spectr[[x]]))
  xlim <- xlim %>% `if`(is.null(xlim), data.x.region, .)
  #
  ## x corresponding to minimum and maximum derivative intensities
  ## in the selected x region:
  x.min <- data.spectr %>%
    filter(between(.data[[x]], xlim[1], xlim[2])) %>%
    filter(.data[[Intensity]] == min(.data[[Intensity]])) %>%
    pull(.data[[x]])
  #
  x.max <- data.spectr %>%
    filter(between(.data[[x]], xlim[1], xlim[2])) %>%
    filter(.data[[Intensity]] == max(.data[[Intensity]])) %>%
    pull(.data[[x]])
  #
  ## Delta_B calculation:
  DeltaX_pp <- abs(x.min - x.max)
  #
  return(round(DeltaX_pp, digits = 3))
  #
}
