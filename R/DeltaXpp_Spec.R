#
#'
#' Calculation of Spectral Linewidth ( e.g. \eqn{\Delta B_{pp}}) of the EPR/ENDOR Spectrum
#'
#'
#' @description
#' The function calculates the \code{peak-to-peak} (distance between of the \eqn{x}-axis
#' projection of "min" and "max" derivative intensities) linewidth
#' of an EPR/ENDOR spectrum.
#'
#'
#' @param data.spectrum EPR/ENDOR spectrum data frame/table with magnetic flux density \eqn{B} (in \code{mT} or \code{G})
#'   or \eqn{g}-Value or \eqn{RF} (in \code{MHz}) column and that of the derivative \code{dIepr_over_dB}
#'   \code{Intensity}. \code{Index} column may be included as well.
#' @param x Character/String pointing to \code{x}-axis/column (in the original \code{data.spectrum}) quantity
#'   like magnetic flux density \eqn{B}, \eqn{g}-Value or \eqn{RF} (radio frequency), \strong{default}: \code{x = "B_mT"}.
#' @param Intensity Character/String pointing to \code{intensity column} ((in the original \code{data.spectrum}))
#'   if other than \code{dIepr_over_dB} name/label is used (e.g. for simulated spectra) please, define.
#'   \strong{Default}: \code{Intesity = "dIepr_over_dB"}.
#' @param xlim Numeric vector corresponding to border limits of the selected \eqn{x} region,
#'   e.g. like `xlim = c(3495.4,3595.4)` (\eqn{B} in \code{G}) or `xlim = c(12.5,21.2)` (\eqn{RF} in \code{MHz})
#'   or `xlim = c(2.004,2.001)` (\eqn{g} dimensionless). \strong{Default}: \code{xlim = NULL} (corresponding
#'   to entire `x` range)
#'
#'
#' @return Numeric value of difference of \code{x}-axis quantity like \eqn{B},\eqn{g},\eqn{RF} (the absolute value)
#'   corresponding to \code{minimum} and \code{maximum} of the derivative intensity (\code{dIepr_over_dB})
#'   in EPR/ENDOR spectrum
#'
#'
#' @examples
#' \dontrun{
#' DeltaXpp_Spec(data.spectrum,
#'               c(320.221,328.331))
#' DeltaXpp_Spec(data.spectrum,
#'               B = "B_G",
#'               Intensity = "dIepr_over_dB",
#'               c(3202.11,3283.31))
#' DeltaXpp_Spec(data.spectrum,
#'               "RF_MHz",
#'               xlim = c(10,42))
#' DeltaXpp_Spec(data.spectrum,
#'               "B_mT_Sim",
#'               c(320.221,328.331))
#' }
#'
#' @export
#'
#'
DeltaXpp_Spec <- function(data.spectrum,
                          x = "B_mT",
                          Intensity = "dIepr_over_dB",
                          xlim = NULL) {
  ## Define limits
  if (is.null(xlim)) {
    ## the entire data region
    xlim <- c(min(data.spectrum[[x]]), max(data.spectrum[[x]]))
  } else {
    ## otherwise use predefined vector
    xlim <- xlim
  }
  #
  ## x corresponding to minimum and maximum derivative intensities
  ## in the selected x region:
  x.min <- data.spectrum %>%
    filter(between(.data[[x]], xlim[1], xlim[2])) %>%
    filter(.data[[Intensity]] == min(.data[[Intensity]])) %>%
    pull(.data[[x]])
  #
  x.max <- data.spectrum %>%
    filter(between(.data[[x]], xlim[1], xlim[2])) %>%
    filter(.data[[Intensity]] == max(.data[[Intensity]])) %>%
    pull(.data[[x]])
  #
  ## Delta_B calculation:
  DeltaX_pp <- abs(x.min - x.max)
  #
  return(round(DeltaX_pp, digits = 2))
  #
}
