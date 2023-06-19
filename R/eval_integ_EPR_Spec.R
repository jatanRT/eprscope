#
#' Integration of EPR Spectral Data for Quantitative Analysis
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'  Evaluates integrals of EPR spectra depending on input data (corresponding to either derivative or single integrated
#'  EPR signal form) with option to correct the single integral baseline by the polynomial of \code{poly.degree}
#'  level. Integration is done by \code{\link[pracma:trapz]{pracma::cumtrapz}} function. For the purpose
#'  of quantitative analysis the integrals are evaluated using the \code{B.units = "G"} (see below).
#'  Therefore, depending on \eqn{B} unit (either \code{"G"} or \code{"mT"}) each resulting integral data/column
#'  have to be optionally (in case of \code{"mT"}) multiplied by factor of \code{10} because
#'  \eqn{1 \text{mT}\equiv 10 \text{G}}. Such correction is already included in the `R` function/script.
#'  Instead of `double integral/integ.` the term `sigmoid integral/integ.` is used. `Double integral`
#'  \strong{in the case of originally single integrated EPR spectrum} (see \code{data.spectrum} and \code{Intensity})
#'  \strong{is confusing. In such case the EPR spectrum is integrated only once.}
#'
#'
#' @param data.spectrum Spectrum data frame/table with magnetic flux density (in \code{mT}
#'   or \code{G})) and that of the derivative or already single integrated intensity.
#'   \code{Index} column may be already present as well.
#' @param B Character/String pointing to magnetic flux density \code{column} (in the original
#'   \code{data.spectrum}) either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"}
#'   or \code{B = "Field"}...etc or \code{B = "B_G"} (\strong{default}).
#' @param Intensity Character/String pointing to \code{column} of either derivative
#'   (e.g. \code{Intensity = "dIepr_over_dB"}, \strong{default}) or single integrated EPR
#'   spectrum (e.g. \code{Intensity = "single_Integrated"}) within the actual data frame \code{data.spectrum}.
#' @param B.unit Character/String pointing to unit of magnetic flux density (coming from original data) which
#'   is to be presented on \eqn{B} abscissa of the EPR spectrum,
#'   like \code{"G"} (`Gauss`) or \code{"mT"} (`millitesla`), \strong{default}: \code{B.unit = "mT"}.
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `Blim = c(3495.4,3595.4)`. \strong{Default}: \code{Blim = NULL}
#'   (corresponding to entire `B` range).
#' @param correct.integ Logical, whether to correct the integral by baseline model fit.
#'   \strong{Default}: \code{correct.integ = FALSE}.
#' @param BpeaKlim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `BpeaKlim = c(3535.4,3555.4)`.
#' @param poly.degree Numeric, degree of polynomial function used to fit the baseline under the single integrated
#'   curve of the original EPR spectrum.
#' @param sigmoid.integ Logical, whether to present (column in data frame) the double integral or single
#'   integral (if the \code{data.spectrum} and \code{Intesity} are already in single integrated form),
#'   in sigmoidal shape, which is required for quantitative analysis,
#'   \strong{default}: \code{sigmoid.integ = FALSE}.
#' @param output.vecs Logical, whether the `integral` \code{columns} are presented within the entire
#'   data frame (\code{output.vecs = FALSE}, \strong{default}) or called as a vectors or list for
#'   additional processing of spectral data series by \pkg{dplyr}.
#'
#'
#' @return The integration results may be divided into following types depending on the above-described
#'   parameters/arguments. Generally, they are either data frames incl. the original data and the integrals
#'   (\code{output.vecs = FALSE}) or vectors corresponding to individual baseline corrected/uncorrected
#'   integrals (\code{output.vecs = TRUE}). This is especially useful for spectral (time) series EPR data,
#'   which can be handily processed by \code{\link[dplyr]{group_by}} using the
#'   `pipe` operators (\code{\link[magrittr]{\%>\%}}).
#'   \enumerate{
#'   \item Data frame/table including the EPR spectral data (general \emph{Intensity}
#'   (integrated or derivative) \emph{vs} \eqn{B}) as well as its corresponding \code{single}
#'   (\strong{column} \code{single_Integ}) integral. This is the case if only a single uncorrected integral
#'   is required.
#'
#'   \item Data frame/table with single integral/intensity already corrected by a certain degree
#'   of polynomial baseline (fitted to experimental baseline without peak). Single integrals are referred either
#'   to derivative or already integrated EPR spectra where corrected integral \code{column}
#'   id denoted as \code{single_Integ_correct}. This is the case for \code{correct.integ = TRUE}
#'   and \code{sigmoid.integ = FALSE} + \code{output.vecs = FALSE}.
#'
#'   \item Data frame with \code{single} and \code{double/sigmoid} integral \strong{column}
#'   (\code{sigmoid_Integ}) essential for quantitative analysis. This is the case for \code{output.vecs = FALSE}
#'   and \code{correct.integ = FALSE}.
#'
#'   \item Data frame in case of \code{correct.integ = TRUE}, \code{sigmoid.integ = TRUE}
#'   and \code{output.vecs = FALSE}. It contains the original data + corrected
#'   single integral (\code{single_Integ_correct}) and double/sigmoid integral (\code{sigmoid_Integ}) which
#'   is evaluated from the baseline corrected single one. Therefore such double/sigmoid integral
#'   is suitable for the accurate determination of radical (paramagnetic centers) amount.
#'
#'   \item Numeric vector corresponding to baseline uncorrected/corrected single integral
#'   in case of \code{sigmoid.integ = FALSE} + \code{output.vecs = TRUE}.
#'
#'   \item List of numeric vectors corresponding to  \strong{`single`} (corrected or uncorrected depending
#'   on \code{correct.integ} parameter/argument) and \strong{`sigmoid`} integrals (\code{sigmoid.integ = TRUE}).
#'   In such case integrals are called by \code{list} names like \code{...[["single"]]}
#'   (\code{...$single}) or by \code{...[["sigmoid"]]} (\code{...$sigmoid}) corresponding to double integrated
#'   (or single integrated EPR spectra originally presented already in single integrals) EPR spectral data.
#'   }
#'
#'
#' @examples
#' \dontrun{
#' ## Single integration with default arguments
#' eval_integ_EPR_Spec(data.spectrum)
#' #
#' ## Integration gathering the double/sigmoid integral
#' ## without baseline correction
#' eval_integ_EPR_Spec(data.spectrum,
#'                     sigmoid.integ = T)
#' #
#' ## baseline correction (by the polynomial of the 3rd level) for
#' ## the single integral as well as evaluating the sigmoid integral,
#' ## single integral peak is located in the region of c(3430,3560) G
#' eval_integ_EPR_Spec(data.spectrum,
#'                     correct.integ = T,
#'                     BpeaKlim = c(3430,3560),
#'                     poly.degree = 3,
#'                     sigmoid.integ = T)
#' #
#' ## vectorized output of the uncorrected `sigmoid integral`
#' eval_integ_EPR_Spec(data.spectrum,sigmoid.integ = T,output.vecs = T)[["sigmoid"]]
#' #
#' ## incorporation of vectorized integration into data "pipe" ("%>%")
#' ## `dplyr` processing of EPR spectral time series, creating column
#' ## with `sigmoid` integral
#' ## where its corresponding single integral (intensity) has undergone
#' ## a baseline correction, finally the max. value of the all sigmoid
#' ## integrals along the time is summarized in data frame
#' ## for quantitative or kinetic analysis
#' data.integrals <- data.spectra %>%
#'   dplyr::group_by(time_s) %>%
#'   dplyr::mutate(sigmoid_Integ = eval_integ_EPR_Spec(dplyr::pick(B_G,dIepr_over_dB),
#'                                                     correct.integ = T,
#'                                                     BpeaKlim = c(3430,3560),
#'                                                     poly.degree = 3,
#'                                                     sigmoid.integ = T,
#'                                                     output.vecs = T)$sigmoid) %>%
#'  dplyr::summarize(Area = max(sigmoid_Integ))
#' }
#'
#'
#' @export
#'
#'
#' @importFrom pracma cumtrapz
eval_integ_EPR_Spec <- function(data.spectrum,
                                B = "B_G",
                                B.unit = "G",
                                Intensity = "dIepr_over_dB",
                                Blim = NULL,
                                correct.integ = FALSE,
                                BpeaKlim = NULL,
                                poly.degree = NULL,
                                sigmoid.integ = FALSE,
                                output.vecs = FALSE) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  single_Integ <- NULL
  sigmoid_Integ <- NULL
  baseline_Integ_fit <- NULL
  baseline_Intens_fit <- NULL
  single_Integ_correct <- NULL
  #
  ## Define limits if `Blim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.B.region <- c(min(data.spectrum[[B]]), max(data.spectrum[[B]]))
  Blim <- Blim %>% `if`(is.null(Blim), data.B.region, .)
  #
  ## evaluating primary integral based on `Intensity`
  ## and `B` (`B.unit` has to be in "G") parameter
  ## otherwise each integration has to be multiplied by 10,
  ## because 1 mT = 10 G
  ## First of all define vectors with intensity column names =>
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
  ## primary data for integration
  data.spectrum <- data.spectrum %>%
    dplyr::filter(dplyr::between(.data[[B]], Blim[1], Blim[2]))
  #
  if (any(grepl(paste(slct.vec.deriv.EPR.intens,collapse = "|"), Intensity))) {
    #
    ## integration depending on `B` unit
    if (B.unit == "G") {
      data.spectrum <- data.spectrum %>%
        dplyr::mutate(single_Integ = pracma::cumtrapz(
          .data[[B]],
          .data[[Intensity]]
        )[, 1])
      if (isFALSE(sigmoid.integ)) {
        data.spectrum <- data.spectrum
      } else {
        data.spectrum$sigmoid_Integ <- pracma::cumtrapz(
          data.spectrum[[B]],
          data.spectrum$single_Integ
        )[, 1]
        #
        ## rescale the `sigmoid_Integ` => from 0 to max
        data.spectrum <- data.spectrum %>%
          dplyr::mutate(sigmoid_Integ = abs(min(sigmoid_Integ)-sigmoid_Integ))
      }
    }
    if (B.unit == "mT") {
      data.spectrum <- data.spectrum %>%
        dplyr::mutate(single_Integ = pracma::cumtrapz(
          .data[[B]],
          .data[[Intensity]]
        )[, 1] * 10)
      if (isFALSE(sigmoid.integ)) {
        data.spectrum <- data.spectrum
      } else {
        data.spectrum$sigmoid_Integ <- pracma::cumtrapz(
          data.spectrum[[B]],
          data.spectrum$single_Integ
        )[, 1] * 10
        #
        ## rescale the `sigmoid_Integ` => from 0 to max
        data.spectrum <- data.spectrum %>%
          dplyr::mutate(sigmoid_Integ = abs(min(sigmoid_Integ)-sigmoid_Integ))
      }
    }
  }
  if (any(grepl(paste(slct.vec.integ.EPR.intens,collapse = "|"), Intensity))) {
    #
    ## integration depending on `B` unit
    if (B.unit == "G") {
      if (isFALSE(sigmoid.integ)) {
        data.spectrum <- data.spectrum
      } else {
        data.spectrum$sigmoid_Integ <- pracma::cumtrapz(
          data.spectrum[[B]],
          data.spectrum[[Intensity]]
        )[, 1]
        #
        ## rescale the `sigmoid_Integ` => from 0 to max
        data.spectrum <- data.spectrum %>%
          dplyr::mutate(sigmoid_Integ = abs(min(sigmoid_Integ)-sigmoid_Integ))
      }
    }
    if (B.unit == "mT") {
      if (isFALSE(sigmoid.integ)) {
        data.spectrum <- data.spectrum
      } else {
        data.spectrum$sigmoid_Integ <- pracma::cumtrapz(
          data.spectrum[[B]],
          data.spectrum[[Intensity]]
        )[, 1] * 10
        #
        ## rescale the `sigmoid_Integ` => from 0 to max
        data.spectrum <- data.spectrum %>%
          dplyr::mutate(sigmoid_Integ = abs(min(sigmoid_Integ)-sigmoid_Integ))
      }
    }
  }
  #
  ## Integral baseline correction
  if (isTRUE(correct.integ)) {
    # select region / range / interval of the peak, which won't be
    ## considered ("!") for the baseline correction / fit
    ## (limits are 'BpeaKlim[1]'<=> 'start','BpeaKlim[2]' <=> 'end'):
    if (is.null(BpeaKlim)) {
      stop(" The `B`-range for the peak baseline correction is not defined. Please, specify the range ! ")
    } else {
      data.NoPeak <- data.spectrum %>%
        filter(!between(.data[[B]], BpeaKlim[1], BpeaKlim[2]))
      if (is.null(poly.degree)) {
        stop(" The degree of a polynomial to model the baseline is not defined. Please, specify ! ")
      } else {
        ## Polynomial baseline and integrate fit incl. derivative intensities =>
        if (any(grepl(paste(slct.vec.deriv.EPR.intens,collapse = "|"), Intensity))) {
          ## Polynomial baseline fit:
          #
          ## convert B to variable in formula by `get(B)`/`eval(parse(text = B))` or `eval(str2lang(B))`
          integ.baseline.fit <- stats::lm(single_Integ ~ stats::poly(get(B), degree = poly.degree),
            data = data.NoPeak
          )
          #
          ## apply fit to data.spectrum
          data.spectrum <- broom::augment(integ.baseline.fit, newdata = data.spectrum) %>%
            ## remove the .resid colum (which is not required),
            dplyr::select(-.data[[".resid"]]) %>%
            ## rename column with fit
            dplyr::rename(baseline_Integ_fit = .data[[".fitted"]]) %>%
            ## subtract the baseline
            dplyr::mutate(single_Integ_correct = .data$single_Integ - .data$baseline_Integ_fit)
            ##  keep `baselin_integ_fit`
            ## & shift the integral baseline up having all the values > 0 (subtract its minimum)
          data.spectrum <- data.spectrum %>%
            dplyr::mutate(single_Integ_correct = abs(min(.data$single_Integ_correct) - single_Integ_correct))
          #
          ## integration depending on `B` unit
          if (B.unit == "G") {
            if (isFALSE(sigmoid.integ)) {
              data.spectrum <- data.spectrum %>%
                ## remove previous double integral (if present)
                `if`(
                  any(grepl("sigmoid_Integ", colnames(data.spectrum))),
                  dplyr::select(-sigmoid_Integ), .
                )
            } else {
              ## uncorrected double integral is `overwritten`
              data.spectrum$sigmoid_Integ <- pracma::cumtrapz(
                data.spectrum[[B]],
                data.spectrum$single_Integ_correct
              )[, 1]
              #
              ## rescale the `sigmoid_Integ` => from 0 to max
              data.spectrum <- data.spectrum %>%
                dplyr::mutate(sigmoid_Integ = abs(min(sigmoid_Integ)-sigmoid_Integ))
            }
          }
          if (B.unit == "mT") {
            if (isFALSE(sigmoid.integ)) {
              data.spectrum <- data.spectrum %>%
                ## remove previous double integral (if present)
                `if`(
                  any(grepl("sigmoid_Integ", colnames(data.spectrum))),
                  dplyr::select(-sigmoid_Integ), .
                )
            } else {
              ## uncorrected double integral is `overwritten`
              data.spectrum$sigmoid_Integ <- pracma::cumtrapz(
                data.spectrum[[B]],
                data.spectrum$single_Integ_correct
              )[, 1] * 10
              #
              ## rescale the `sigmoid_Integ` => from 0 to max
              data.spectrum <- data.spectrum %>%
                dplyr::mutate(sigmoid_Integ = abs(min(sigmoid_Integ)-sigmoid_Integ))
            }
          }
        }
        ## Polynomial baseline fit integrate incl. already single integrated intensities =>
        if (any(grepl(paste(slct.vec.integ.EPR.intens,collapse = "|"), Intensity))) {
          ## Polynomial baseline fit:
          integ.baseline.fit <- stats::lm(get(Intensity) ~ stats::poly(get(B), degree = poly.degree),
            data = data.NoPeak
          )
          #
          ## apply fit to data.spectrum
          data.spectrum <- broom::augment(integ.baseline.fit, newdata = data.spectrum) %>%
            ## remove the .resid colum (which is not required),
            dplyr::select(-.data[[".resid"]]) %>%
            ## rename column with fit
            dplyr::rename(baseline_Intens_fit = .data[[".fitted"]]) %>%
            ## subtract the baseline
            dplyr::mutate(single_Integ_correct = .data[[Intensity]] - .data$baseline_Intens_fit)
            ##  keep `baseline_Intens_fit`
            ## & shift the integral baseline up having all the values > 0 (subtract its minimum)
          data.spectrum <- data.spectrum %>%
            dplyr::mutate(single_Integ_correct = abs(min(.data$single_Integ_correct) - single_Integ_correct))
          #
          ## integration depending on `B` unit
          if (B.unit == "G") {
            if (isFALSE(sigmoid.integ)) {
              data.spectrum <- data.spectrum %>%
                ## remove previous sigmoid integral (if present)
                `if`(
                  any(grepl("sigmoid_Integ", colnames(data.spectrum))),
                  dplyr::select(-sigmoid_Integ), .
                )
            } else {
              data.spectrum$sigmoid_Integ <- pracma::cumtrapz(
                data.spectrum[[B]],
                data.spectrum$single_Integ_correct
              )[, 1]
              #
              ## rescale the `sigmoid_Integ` => from 0 to max
              data.spectrum <- data.spectrum %>%
                dplyr::mutate(sigmoid_Integ = abs(min(sigmoid_Integ)-sigmoid_Integ))
            }
          }
          if (B.unit == "mT") {
            if (isFALSE(sigmoid.integ)) {
              data.spectrum <- data.spectrum %>%
                ## remove previous sigmoid integral (if present)
                `if`(
                  any(grepl("sigmoid_Integ", colnames(data.spectrum))),
                  dplyr::select(-sigmoid_Integ), .
                )
            } else {
              data.spectrum$sigmoid_Integ <- pracma::cumtrapz(
                data.spectrum[[B]],
                data.spectrum$single_Integ_correct
              )[, 1] * 10
              #
              ## rescale the `sigmoid_Integ` => from 0 to max
              data.spectrum <- data.spectrum %>%
                dplyr::mutate(sigmoid_Integ = abs(min(sigmoid_Integ)-sigmoid_Integ))
            }
          }
        }
      }
    }
  }
  #
  ## Data Frame or Vectorized output for the EPR spectral series
  if (isFALSE(output.vecs)) {
    ## delete `index` column which is not necessary anymore
    if (any(grepl("index", colnames(data.spectrum)))) {
      data.spectrum$index <- NULL
    }
    #
    ## reorder columns in data frame
    ## `sigmoiod_integ` as last
    data.spectrum <- data.spectrum %>%
      dplyr::select(-sigmoid_Integ,sigmoid_Integ)
    #
    integrate.results <- data.spectrum
    #
  } else {
    if (isFALSE(sigmoid.integ)) {
      ## bacause `ifelse()` does not work for vectors
      ## it hast be replaced by `switch()` function, like =>
      ## integrated <- FALSE
      ## a <- "Derivative Intensity", b <- "Integrated Intensity"
      ## switch(2-isFALSE(integrated),a,b) =>
      integrate.results <- switch(2 - isFALSE(correct.integ),
        data.spectrum$single_Integ,
        data.spectrum$single_Integ_correct
      )
    } else {
      if (any(grepl(paste(slct.vec.deriv.EPR.intens,collapse = "|"), Intensity))) {
        integrate.results <- list(
          single = switch(2 - isFALSE(correct.integ),
            data.spectrum$single_Integ,
            data.spectrum$single_Integ_correct
          ),
          sigmoid = data.spectrum$sigmoid_Integ
        )
      }
      if (any(grepl(paste(slct.vec.integ.EPR.intens,collapse = "|"), Intensity))) {
        integrate.results <- list(
          single = switch(2 - isFALSE(correct.integ),
            data.spectrum$Intensity,
            data.spectrum$single_Integ_correct
          ),
          sigmoid = data.spectrum$sigmoid_Integ
        )
      }
    }
  }
  #
  return(integrate.results)
  #
}
