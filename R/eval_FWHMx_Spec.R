#'
#' Evaluating Full Width at Half-Maximum (FWHM) from integrated EPR Spectra
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   Finding the full width at half-maximum (FWHM) height of the EPR integrated spectrum/intensity.
#'   For such purpose the EPR spectrum has to be in single integrated form (common absorption-like spectrum).
#'   If this is not the case, the derivative EPR spectrum (with the intensity \code{dIepr_over_dB})
#'   can be integrated by \code{\link{eval_integ_EPR_Spec}}. The FWHM is evaluated as a difference
#'   between the points (\eqn{x > x_{\text{max}}} and \eqn{x < x_{\text{max}}}) having the intensity
#'   closest to the maximal intensity/2 corresponding to one individual EPR line/peak defined
#'   by the \code{xlim} argument.
#'
#'
#'
#' @param data.spectr.integ Data frame object containing \code{x}-column/variable like magnetic flux density,
#'   \emph{B} (in in \code{mT} or \code{G}) or \emph{g}-factor/value (unitless) and integrated intensity
#'   (common absorption-like spectrum) column/variable.
#' @param x Character string pointing to name of the \code{x}-axis/column/variable (in the original
#'   \code{data.spectr.integ}) like magnetic flux density \emph{B} (in \code{mT} or \code{G}) or \emph{g}-Value
#'   (unitless), \strong{default}: \code{x = "B_G"}.
#' @param Intensity Character string pointing to name of the \code{intensity column/variable}
#'   (in the original \code{data.spectr.integ}) if other than \code{single_Integ} (\strong{default}) name/label
#'   is used (such as "Integral_Intensity" or "integral").
#' @param xlim Numeric vector corresponding to lower and upper limit of the selected \emph{x}-region,
#'   e.g. \code{xlim = c(3495.4,3595.4)} (\emph{B} in \code{G}) or \code{xlim = c(2.004,2.001)}
#'   (\emph{g} dimensionless). \strong{Default}: \code{xlim = NULL} (corresponding to the entire \emph{x}-range).
#'
#'
#' @return Numeric value of the FWHM directly from EPR spectrum, depending on the \code{x} variable =>
#'   either in \code{mT}/\code{G} or unitless in case if \emph{g}-factor is presented on abscissa.
#'
#'
#' @examples
#' ## simulation of phenalenyl/perinaphthenyl (PNT) radical
#' ## in integrated form:
#' pnt.sim.integ.iso <-
#'   eval_sim_EPR_iso(g = 2.0027,
#'     instrum.params = c(Bcf = 3500, # central field
#'                        Bsw = 100, # sweep width
#'                        Npoints = 4096,
#'                        mwGHz = 9.8), # MW Freq. in GHz
#'     B.unit = "G",
#'     nuclear.system = list(
#'       list("1H",3,5.09), # 3 x A(1H) = 5.09 MHz
#'       list("1H",6,17.67) # 6 x A(1H) = 17.67 MHz
#'      ),
#'     lineSpecs.form = "integrated",
#'     lineGL.DeltaB = list(0.54,NULL), # Gauss. FWHM in G
#'     Intensity.sim = "single_Integ"
#'   )
#' #
#' ## FWHM of one of the central
#' ## lines/peaks (`xlim = c(3494,3496.5)`)
#' ## from the simulated spectral data:
#' eval_FWHMx_Spec(pnt.sim.integ.iso$df,
#'                 x = "Bsim_G",
#'                 Intensity = "single_Integ",
#'                 xlim = c(3494,3496.5)
#'                 )
#' #
#' ## interactive plot of the above-simulated
#' ## EPR spectrum in order to check the values:
#' plot_EPR_Specs2D_interact(pnt.sim.integ.iso$df,
#'   x = "Bsim_G",
#'   x.unit = "G",
#'   Intensity = "single_Integ",
#'   lineSpecs.form = "integrated"
#'  )
#'
#'
#' @export
#'
#'
eval_FWHMx_Spec <- function(data.spectr.integ,
                            x = "B_G",
                            Intensity = "single_Integ",
                            xlim = NULL) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## Define limits if `xlim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.x.region <- c(min(data.spectr.integ[[x]]), max(data.spectr.integ[[x]]))
  xlim <- xlim %>% `if`(is.null(xlim), data.x.region, .)
  #
  # ===== This is not required, however it's better to select a narrow region at the beginning =====
  #
  ## see also `which.min` function below
  ## Selecting `x region`
  ## variable set as `xs.init` (data frame) +
  ## Condition to find x values for near(max(`Intensity`)/2)
  xs.init <- data.spectr.integ %>%
    dplyr::filter(dplyr::between(
      .data[[x]],
      xlim[1], xlim[2]
    )) %>%
    dplyr::filter(dplyr::near(.data[[Intensity]],
      max(.data[[Intensity]]) / 2,
      tol = max(.data[[Intensity]]) / 4
    ))
  #
  # ==================================================================================
  #
  ## calculate `x.max` corresponding to max(Intensity)
  x.max <- data.spectr.integ %>%
    dplyr::filter(dplyr::between(.data[[x]], xlim[1], xlim[2])) %>%
    dplyr::filter(.data[[Intensity]] == max(.data[[Intensity]])) %>%
    dplyr::pull(.data[[x]])
  #
  ## set the condition for the closest values individually
  ## for x < `x.max` & x > `x.max`
  ## however individual values must be compared => create a loop
  ## for all `x` of the `xs.init`
  for (p in seq(xs.init[[x]])) {
    if (xs.init[[x]][p] < x.max) {
      ## filter x values
      x.init.low <- xs.init %>%
        dplyr::filter(.data[[x]] < x.max)
      ## intensity condition by `which.min` and results in indices (res. one line df)
      ## => it is just like dplyr filtering, therefore
      Intens.cond.left <- which.min(abs(x.init.low[[Intensity]] - (max(data.spectr.integ[[Intensity]]) / 2)))
      Intens.cond.left <- x.init.low[Intens.cond.left,] %>% dplyr::pull(.data[[Intensity]])
      ## finding x
      x.init.low <- x.init.low %>%
        dplyr::filter(.data[[Intensity]] == Intens.cond.left) %>%
        dplyr::pull(.data[[x]])
    }
    if (xs.init[[x]][p] > x.max) {
      ## filter x values
      x.init.high <- xs.init %>%
        dplyr::filter(.data[[x]] > x.max)
      ## intensity condition by `which.min` and results in indices (res. one line df)
      ## => it is just like dplyr filtering, therefore
      Intens.cond.right <- which.min(abs(x.init.high[[Intensity]] - (max(data.spectr.integ[[Intensity]]) / 2)))
      Intens.cond.right <- x.init.high[Intens.cond.right,] %>% dplyr::pull(.data[[Intensity]])
      ## finding x
      x.init.high <- x.init.high %>%
        dplyr::filter(.data[[Intensity]] == Intens.cond.right) %>%
        dplyr::pull(.data[[x]])
    }
    #
  }
  #
  FWHM <- round(abs(x.init.low - x.init.high), digits = 3)
  #
  return(FWHM)
  #
}
