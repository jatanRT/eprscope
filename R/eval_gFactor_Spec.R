#
#' @title Calculation of \eqn{g}-factor ("Position") from the EPR Spectrum/Data
#'
#' @description Calculation of g-value according to fundamental formula (\code{\link{eval_gFactor}}).
#'   \eqn{g}-related magnetic flux density (like \eqn{B_{iso}} or \eqn{B_{center}}) is directly taken
#'   from the EPR spectrum. If positive and negative derivative intensities of the spectral line are similar
#'   and their distance from the middle of the spectrum equals, the \eqn{B_{iso}} should be be considered,
#'   otherwise the \eqn{B_{center}} must be taken into account. In case of integrated EPR spectrum/data
#'   the \eqn{B_{max}} is used for the \eqn{g}-value evaluation.
#'
#'
#' @param data.spectrum Spectrum data frame/table where the magnetic flux density (in \code{mT} or \code{G}) column
#'   can be labeled as \code{Field} or \code{B_G} and that of the derivative intensity as \code{dIepr_over_dB}
#'   or single integrated intensity like \code{Integrated_Intensity}, \code{index} column can be included as well.
#' @param nu.GHz Numeric, microwave frequency in \code{GHz}
#' @param B.unit description tbc
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{data.spectrum} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} (\strong{default})
#'   or \code{B = "B_G"} or \code{B = "B_G_Sim"} to include simulated EPR spectra as well
#' @param Intensity Character/String pointing to \code{intensity column} if other than \code{dIepr_over_dB}
#'   name/label is used (e.g. for simulated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `Blim = c(3495.4,3595.4)`. \strong{Default}: \code{Blim = NULL} (corresponding
#'   to entire `B` range).
#' @param iso Logical, whether to calculate the \eqn{g}-factor from the \eqn{B} value corresponding to
#'   that between the \code{min.} and \code{max.} derivative intensities (\code{dIepr_over_dB}, that is \eqn{g_{iso}}
#'   (this is the \strong{default}: \code{iso = TRUE}), or by finding the the \eqn{B} value corresponding
#'   to \code{dIepr_over_dB = 0} (close/near zero, which is \code{iso = FALSE})
#'
#' @return Numeric \eqn{g_{iso}}-value ('iso' = 'isotropic') according to \eqn{(\nu h)/(\mu_{B} B)}
#'
#'
#' @examples
#' \dontrun{
#' eval_gFactor_Spec(data.spectrum,
#'                   9.82451,
#'                   B.unit = "mT",
#'                   "Field_mT",
#'                   Intensity = "dIepr_over_dB_Sim",
#'                   c(349.8841,351.112))
#' eval_gFactor_Spec(data.spectrum,
#'                   nu.GHz = 9.82451,
#'                   B.unit = "G",
#'                   B = "B_G",
#'                   Blim = c(3498.841,3511.12),
#'                   iso = FALSE)
#' eval_gFactor_Spec(data.spectrum,
#'                   9.91024,
#'                   B.unit = "G",
#'                   B = "B_G_Sim",
#'                   Intensity = "Integral_Intensity",
#'                   c(3499,3501))
#' }
#'
#'
#' @export
#'
#'
#' @importFrom dplyr filter select mutate pull between near
eval_gFactor_Spec <- function(data.spectrum,
                              nu.GHz,
                              B.unit = "G",
                              B = "B_G",
                              Intensity = "dIepr_over_dB",
                              Blim = NULL,
                              iso = TRUE) {
  ## 'Temporary' processing variables
  AbsIntens <- NULL
  . <- NULL
  #
  ## Define limits if `Blim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.B.region <- c(min(data.spectrum[[B]]), max(data.spectrum[[B]]))
  Blim <- Blim %>% `if`(is.null(Blim), data.B.region, .)
  #
  ## First of all define vectors with intensity column names =>
  ## in order to defferentiate between derivative and integrated
  ## EPR spectra
  slct.vec.deriv.EPR.intens <- c(
    "dB", "_dB", "intens", "deriv", "Intens",
    "Deriv", "dIepr", "dIepr_over_dB", "dIepr_dB",
    "MW_Absorp", "MW_intens", "MW_Intens"
  )
  ## &
  slct.vec.integ.EPR.intens <- c(
    "single", "Single", "SInteg", "sinteg", "s_integ",
    "single_", "singleinteg", "sintegral", "integral_Single",
    "Integral_single", "sInteg_", "sInteg", "singleI",
    "Sinteg", "Single_", "integral_single", "SingleI",
    "SingleInteg", "Isingle", "iSingle", "singleInteg", "ISingle",
    "IntegralSingl", "intergralSingl", "IntegSingl",
    "integSingl", "IntegSingl", "integSingl"
  )
  #
  ## B minimum & maximum
  B.min <- data.spectrum %>%
    dplyr::filter(dplyr::between(.data[[B]], Blim[1], Blim[2])) %>%
    dplyr::filter(.data[[Intensity]] == min(.data[[Intensity]])) %>%
    dplyr::pull(.data[[B]])
  #
  B.max <- data.spectrum %>%
    dplyr::filter(dplyr::between(.data[[B]], Blim[1], Blim[2])) %>%
    dplyr::filter(.data[[Intensity]] == max(.data[[Intensity]])) %>%
    dplyr::pull(.data[[B]])
  ## B between minimum and maximum of dIepr_over_dB:
  if (isTRUE(iso)) {
    ## `sjmisc::str_contains` can be replaced by `any(grepl())` and `regex` `|` `or` sign
    if (any(grepl(paste(slct.vec.deriv.EPR.intens, collapse = "|"), Intensity))) {
      B.center <- (B.min + B.max) / 2
      ## B at dIepr_over_dB = 0 (near 0, see next comment on `B.center`):
    }
    if (any(grepl(paste(slct.vec.integ.EPR.intens, collapse = "|"), Intensity))) {
      B.center <- B.max
    }
  } else {
    if (any(grepl(paste(slct.vec.deriv.EPR.intens, collapse = "|"), Intensity))) {
      ## Find the value B, corresponding to Intensity very close to 0 (tolerance max(Intensity)/100)
      B.center <- data.spectrum %>%
        dplyr::filter(dplyr::between(.data[[B]], B.max, B.min)) %>%
        dplyr::mutate(AbsIntens = abs(.data[[Intensity]])) %>%
        dplyr::filter(dplyr::near(AbsIntens, 0, tol = max(.data[[Intensity]]) / 100)) %>%
        dplyr::filter(AbsIntens == min(AbsIntens)) %>%
        dplyr::pull(.data[[B]])
    }
    if (any(grepl(paste(slct.vec.integ.EPR.intens, collapse = "|"),Intensity))) {
      B.center <- B.max
    }
  }
  ## g -value calculation:
  Planck.const <- constants::syms$h
  Bohr.magnet <- constants::syms$mub
  g.precurs <- (Planck.const * nu.GHz * 1e+9) / (Bohr.magnet * B.center)
  #
  ## Conditions for B column, the name should contain ("B", "mT" or "G"):
  if (B.unit == "mT") {
    g <- g.precurs / 1e-3
  }
  if (B.unit == "G") {
    g <- g.precurs / 1e-4
  }
  #
  return(round(g, digits = 5))
  #
}
