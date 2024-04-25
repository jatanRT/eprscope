#
#' Find Intensity Extremes within the EPR/ENDOR Spectrum (incl. Integrated Form).
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   Finding the \code{x} positions like \emph{B} (in \code{mT} or \code{G})) or \emph{g} (unitless)
#'   or \emph{RF} (in \code{MHz}) of intensity minimum or maximum within the selected region
#'   of an EPR/ENDOR spectrum.
#'
#'
#' @param data.spectr EPR/ENDOR spectrum data frame object with magnetic flux density \eqn{B} (in \code{mT} or \code{G})
#'   or \eqn{g}-Value or \emph{RF} (in \code{MHz}) column/variable and that of the derivative \code{dIepr_over_dB}
#'   or integrated \code{Intensity}. \code{Index} column/variable may be included as well.
#' @param x Character string pointing to name of the \code{x}-axis/column/variable (in the original \code{data.spectr})
#'   like magnetic flux density \emph{B}, \emph{g}-Value or \emph{RF} (radio frequency), \strong{default}: \code{x = "B_mT"}.
#' @param Intensity Character string pointing to name of the \code{intensity column/variable}
#'   (in the original \code{data.spectr}) if other than \code{dIepr_over_dB} name/label
#'   is used (e.g. for simulated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}.
#' @param xlim Numeric vector corresponding to border limits of the selected \emph{x} region,
#'   e.g. like \code{xlim = c(3495.4,3595.4)} (\emph{B} in \code{G}) or \code{xlim = c(12.5,21.2)} (\emph{RF} in \code{MHz})
#'   or \code{xlim = c(2.004,2.001)} (\emph{g} dimensionless). \strong{Default}: \code{xlim = NULL} (corresponding
#'   to entire \code{x} range).
#' @param extreme Character string with only a two values allowed: \code{"min"} or \code{"max"} (\strong{default}).
#'
#'
#' @returns Numeric value of \code{x}-axis variable like \emph{B},\emph{g},\emph{RF} corresponding
#'  to \code{minimal} or \code{maximal} \code{intensity} within the EPR/ENDOR spectrum or its integrated form.
#'
#'
#' @examples
#' \dontrun{
#' eval_extremeX_Spec(data.spectr,
#'                    xlim = c(349.54,359.54),
#'                    extreme = 'min')
#' #
#' eval_extremeX_Spec(data.frame,
#'                    "g_Value",
#'                    Intensity = "single_Integ",
#'                    c(2.007,2.000))
#' }
#'
#'
#' @export
#'
#'
## COMMENT Find extremes in EPR spectra:
## Function to find B,g or RF corresponding to max. or min.
## intensity ('dIepr_over_dB' or other name/label). To select a region / span / interval
## (limits are `xlim`, `xlim[1]` <=> 'start',`xlim[2]` <=> 'end')
## of single spectrum in which the extreme ("max" or "min") needs to be found
eval_extremeX_Spec <- function(data.spectr,
                               x = "B_mT",
                               Intensity = "dIepr_over_dB",
                               xlim = NULL,
                               extreme = "max") {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## Define limits if `xlim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.x.region <- c(min(data.spectr[[x]]), max(data.spectr[[x]]))
  xlim <- xlim %>% `if`(is.null(xlim), data.x.region, .)
  #
  if (extreme == "min") {
    x.min <- data.spectr %>%
      filter(between(.data[[x]], xlim[1], xlim[2])) %>%
      filter(.data[[Intensity]] == min(.data[[Intensity]])) %>%
      pull(.data[[x]])
    #
    return(round(x.min, digits = 2))
    #
  }
  if (extreme == "max") {
    x.max <- data.spectr %>%
      filter(between(.data[[x]], xlim[1], xlim[2])) %>%
      filter(.data[[Intensity]] == max(.data[[Intensity]])) %>%
      pull(.data[[x]])
    #
    return(round(x.max, digits = 2))
    #
  }
}
