#
#' Find Intensity Extremes within the EPR/ENDOR Spectrum
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   Finding the \code{x} positions like \emph{B} (in \code{mT}, \code{G} or \code{T}), \emph{g} (unitless)
#'   or \emph{RF} (in \code{MHz}) of the intensity minimum or maximum within the selected region
#'   of an EPR/ENDOR spectrum. In terms of extremes, if one wants to perform peak picking of an EPR/ENDOR spectrum,
#'   the \code{\link{eval_peakPick_Spec}} function can be applied as well.
#'
#'
#' @inheritParams eval_DeltaXpp_Spec
#' @param extreme Character string with only two values allowed: \code{"min"} or \code{"max"} (\strong{default}).
#'
#'
#' @returns Numeric value of \code{x}-axis variable like \emph{B},\emph{g},\emph{RF}, corresponding
#'  to \code{Intensity} \code{minimum} or its \code{maximum}, within the EPR/ENDOR spectrum or its integrated form.
#'
#'
#' @examples
#' ## loading TMPD built-in example file:
#' tmpd.data.file.path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.asc")
#' ## reading data:
#' tmpd.data.file <-
#'   readEPR_Exp_Specs(path_to_ASC = tmpd.data.file.path,
#'                     col.names = c("B_G",
#'                                   "dIepr_over_dB"),
#'                     qValue = 3500,
#'                     norm.vec.add = 20,
#'                     origin = "winepr")
#' #
#' ## finding maximum and minimum `B` within the entire
#' ## spectral (`B`) range:
#' eval_extremeX_Spec(data.spectr = tmpd.data.file)
#' #
#' eval_extremeX_Spec(data.spectr = tmpd.data.file,
#'                    extreme = "min")
#' #
#' ## both values can be checked by the interactive
#' ## spectrum:
#' plot_EPR_Specs2D_interact(tmpd.data.file)
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
  if (extreme == "min" || extreme == "Min" || extreme == "MIN") {
    x.min <- data.spectr %>%
      filter(between(.data[[x]], xlim[1], xlim[2])) %>%
      filter(.data[[Intensity]] == min(.data[[Intensity]])) %>%
      pull(.data[[x]])
    #
    return(round(x.min, digits = 3))
    #
  }
  if (extreme == "max" || extreme == "Max" || extreme == "MAX") {
    x.max <- data.spectr %>%
      filter(between(.data[[x]], xlim[1], xlim[2])) %>%
      filter(.data[[Intensity]] == max(.data[[Intensity]])) %>%
      pull(.data[[x]])
    #
    return(round(x.max, digits = 3))
    #
  }
}
