#
#' Integration of EPR Spectral Data for Quantitative Analysis
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'  Evaluates integrals of EPR spectra depending on input data => corresponding either to derivative or single integrated
#'  EPR signal form with the option to correct the single integral baseline by the polynomial of \code{poly.degree}
#'  level.
#'
#'
#' @details
#'  Integration is done by \code{\link[pracma:trapz]{pracma::cumtrapz}} function. For the purpose
#'  of quantitative analysis the integrals are evaluated using the \code{B.units = "G"} (see below).
#'  Therefore, depending on \emph{B} unit (either `G` or `mT`) each resulting integral data/column
#'  have to be optionally (in case of `mT`) multiplied by factor of \code{10} because
#'  \eqn{1 \text{mT}\equiv 10 \text{G}}. Such correction is already included in the function/script.
#'  Instead of `double integral/integ.` the term `sigmoid integral/integ.` is used. `Double integral`
#'  \strong{in the case of originally single integrated EPR spectrum} (see \code{data.spectr} and \code{Intensity})
#'  \strong{is confusing. In such case the EPR spectrum is integrated only once.}
#'
#'
#'
#' @param data.spectr Spectrum data frame/table with magnetic flux density (in `mT`
#'   or `G`) and that of the derivative or already single integrated intensity.
#'   \code{Index} column may be already present as well.
#' @param B Character/String pointing to magnetic flux density \code{column} (in the original
#'   \code{data.spectr}) either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"}
#'   or \code{B = "Field"}...etc or \code{B = "B_G"} (\strong{default}).
#' @param Intensity Character/String pointing to \code{column} of either derivative
#'   (e.g. \code{Intensity = "dIepr_over_dB"}, \strong{default}) or single integrated EPR
#'   spectrum (e.g. \code{Intensity = "single_Integrated"}) within the actual data frame \code{data.spectr}.
#' @param lineSpecs.form Character string describing either \code{"derivative"} (\strong{default}) or \code{"integrated"}
#'   (i.e. \code{"absorption"} which can be used as well) line form of the analyzed EPR spectrum/data.
#' @param B.unit Character/String pointing to unit of magnetic flux density (coming from original data) which
#'   is to be presented on \eqn{B} abscissa of the EPR spectrum,
#'   like \code{"G"} (`Gauss`) or \code{"mT"} (`millitesla`), \strong{default}: \code{B.unit = "mT"}.
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \emph{B} region, e.g. like \code{Blim = c(3495.4,3595.4)}. \strong{Default}: \code{Blim = NULL}
#'   (corresponding to entire \emph{B} range).
#' @param correct.integ Logical, whether to correct the integral by baseline model fit.
#'   \strong{Default}: \code{correct.integ = FALSE}.
#' @param BpeaKlim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like \code{BpeaKlim = c(3535.4,3555.4)}.
#' @param poly.degree Numeric, degree of polynomial function used to fit the baseline under the single integrated
#'   curve of the original EPR spectrum.
#' @param sigmoid.integ Logical, whether to present (column in data frame) the double integral or single
#'   integral (if the \code{data.spectr} and \code{Intesity} are already in single integrated form),
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
#'   integrals (\code{output.vecs = TRUE}). This is especially useful for the spectral (time) series EPR data,
#'   which can be handily processed by \code{\link[dplyr]{group_by}} using the
#'   `pipe` operators (\code{\link[magrittr]{\%>\%}}).
#'   \enumerate{
#'   \item Data frame/table including the EPR spectral data (general \emph{Intensity}
#'   (integrated or derivative) \emph{vs} \eqn{B}) as well as its corresponding \code{single}
#'   (\strong{column} \code{single_Integ}) integral. This is the case if only a single
#'   uncorrected integral is required.
#'
#'   \item Data frame/table with single integral/intensity already corrected by a certain degree
#'   of polynomial baseline (fitted to experimental baseline without peak). Single integrals
#'   are referred either to derivative or already integrated EPR spectra where corrected
#'   integral \code{column} id denoted as \code{single_Integ_correct}. This is the case
#'   for \code{correct.integ = TRUE} and \code{sigmoid.integ = FALSE} + \code{output.vecs = FALSE}.
#'
#'   \item Data frame with \code{single} and \code{double/sigmoid} integral \strong{column}
#'   (\code{sigmoid_Integ}) essential for quantitative analysis. This is the case
#'   for \code{output.vecs = FALSE} and \code{correct.integ = FALSE}.
#'
#'   \item Data frame in case of \code{correct.integ = TRUE}, \code{sigmoid.integ = TRUE}
#'   and \code{output.vecs = FALSE}. It contains the original data + corrected
#'   single integral (\code{single_Integ_correct}) and double/sigmoid integral
#'   (\code{sigmoid_Integ}) which is evaluated from the baseline corrected single one.
#'   Therefore, such double/sigmoid integral is suitable for the accurate determination
#'   of radical (paramagnetic centers) amount.
#'
#'   \item Numeric vector corresponding to baseline uncorrected/corrected single integral
#'   in case of \code{sigmoid.integ = FALSE} + \code{output.vecs = TRUE}.
#'
#'   \item List of numeric vectors corresponding to:
#'     \describe{
#'     \item{single}{Corrected or uncorrected single integral (in case of derivative form)
#'     depending on \code{correct.integ} parameter/argument.}
#'     \item{sigmoid}{Double integral (in case of derivative form) or single integral
#'     (in case of integrated spectral form) for quantitative analysis.}
#'     }
#'   }
#'
#'
#' @examples
#' \dontrun{
#' ## Single integration of derivative spectrum with default arguments
#' ## returns data frame with additional `single_Integ` column
#' eval_integ_EPR_Spec(data.spectr)
#' #
#' ## Integration gathering the double/sigmoid integral
#' ## without baseline correction returns data frame with
#' ## two additional columns `single_Integ` + `sigmoid_Integ`
#' eval_integ_EPR_Spec(data.spectr,
#'                     sigmoid.integ = T)
#' #
#' ## Baseline correction (by the polynomial of the 3rd level) for
#' ## the single integrated spec. as well as evaluating the sigmoid integral,
#' ## single integral peak is located in the region of c(3430,3560) G,
#' ## the result is data frame with the following additional columns:
#' ## `single_Integ`, `baseline_Integ_fit`, `single_Integ_correct`, `sigmoid_Integ`
#' eval_integ_EPR_Spec(data.spectr,
#'                     lineSpec.form = "absorption",
#'                     correct.integ = T,
#'                     BpeaKlim = c(3430,3560),
#'                     poly.degree = 3,
#'                     sigmoid.integ = T)
#' #
#' ## Vectorized output of the uncorrected `sigmoid integral`
#' eval_integ_EPR_Spec(data.spectr,sigmoid.integ = T,output.vecs = T)[["sigmoid"]]
#' #
#' ## Incorporation of vectorized integration into data "pipe" ("%>%")
#' ## `dplyr` processing of EPR spectral time series, creating column
#' ## with `sigmoid` integral where its corresponding single integral (intensity)
#' ## has undergone a baseline correction, finally the max. value
#' ## of the all sigmoid integrals along the time is summarized in data frame
#' ## for quantitative or kinetic analysis
#' data.integrals <- data.spectra %>%
#'   dplyr::group_by(time_s) %>%
#'   dplyr::filter(dplyr::between(B_G,3390,3600)) %>%
#'   dplyr::mutate(sigmoid_Integ = eval_integ_EPR_Spec(dplyr::pick(B_G,dIepr_over_dB),
#'                                                     correct.integ = T,
#'                                                     BpeaKlim = c(3430,3560),
#'                                                     poly.degree = 3,
#'                                                     sigmoid.integ = T,
#'                                                     output.vecs = T)$sigmoid) %>%
#'  dplyr::summarize(Area = max(sigmoid_Integ))
#' ## in such case `Blim` range is not defined by `eval_integ_EPR_Spec`,
#' ## it must be `Blim = NULL`, however by `dplyr::between()` !!!
#' #
#' ## Similar to previous data processing, creating both: corrected
#' ## single integral + sigmoid integral for each time within the spectral
#' ## series. Sigmoid integral was evalutated from the single one by
#' ## `cumtrapz()` function from `pracma` package and finally rescaled.
#' data.integrals <- data.spectra %>%
#'   dplyr::group_by(time_s) %>%
#'   eval_integ_EPR_Spec(correct.integ = T,
#'                       Blim = c(3380,3610),
#'                       BpeaKlim = c(3430,3560),
#'                       poly.degree = 3) %>%
#'  dplyr::group_by(time_s) %>%
#'  dplyr::mutate(sigmoid_Integ = pracma::cumtrapz(B_G,single_Integ_correct)[,1]) %>%
#'  dplyr::mutate(sigmoid_Integ_correct = abs(min(sigmoid_Integ) - sigmoid_Integ))
#' }
#'
#'
#' @export
#'
#'
#' @importFrom pracma cumtrapz
eval_integ_EPR_Spec <- function(data.spectr,
                                B = "B_G",
                                B.unit = "G",
                                Intensity = "dIepr_over_dB",
                                lineSpecs.form = "derivative",
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
  data.B.region <- c(min(data.spectr[[B]]), max(data.spectr[[B]]))
  Blim <- Blim %>% `if`(is.null(Blim), data.B.region, .)
  #
  ## evaluating primary integral based on `Intensity`
  ## and `B` (`B.unit` has to be in "G") parameter
  ## otherwise each integration has to be multiplied by 10,
  ## because 1 mT = 10 G
  #
  ## primary data for integration
  data.spectr <- data.spectr %>%
    dplyr::filter(dplyr::between(.data[[B]], Blim[1], Blim[2]))
  #
  if (lineSpecs.form == "derivative") {
    #
    ## integration depending on `B` unit
    if (B.unit == "G") {
      data.spectr <- data.spectr %>%
        dplyr::mutate(single_Integ = pracma::cumtrapz(
          .data[[B]],
          .data[[Intensity]]
        )[, 1])
      if (isFALSE(sigmoid.integ)) {
        data.spectr <- data.spectr
      } else {
        data.spectr$sigmoid_Integ <- pracma::cumtrapz(
          data.spectr[[B]],
          data.spectr$single_Integ
        )[, 1]
        #
        ## rescale the `sigmoid_Integ` => from 0 to max
        data.spectr <- data.spectr %>%
          dplyr::mutate(sigmoid_Integ = abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
      }
    }
    if (B.unit == "mT") {
      data.spectr <- data.spectr %>%
        dplyr::mutate(single_Integ = pracma::cumtrapz(
          .data[[B]],
          .data[[Intensity]]
        )[, 1] * 10)
      if (isFALSE(sigmoid.integ)) {
        data.spectr <- data.spectr
      } else {
        data.spectr$sigmoid_Integ <- pracma::cumtrapz(
          data.spectr[[B]],
          data.spectr$single_Integ
        )[, 1] * 10
        #
        ## rescale the `sigmoid_Integ` => from 0 to max
        data.spectr <- data.spectr %>%
          dplyr::mutate(sigmoid_Integ = abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
      }
    }
  }
  if (lineSpecs.form == "integrated" || lineSpecs.form == "absorption") {
    #
    ## integration depending on `B` unit
    if (B.unit == "G") {
      if (isFALSE(sigmoid.integ)) {
        data.spectr <- data.spectr
      } else {
        data.spectr$sigmoid_Integ <- pracma::cumtrapz(
          data.spectr[[B]],
          data.spectr[[Intensity]]
        )[, 1]
        #
        ## rescale the `sigmoid_Integ` => from 0 to max
        data.spectr <- data.spectr %>%
          dplyr::mutate(sigmoid_Integ = abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
      }
    }
    if (B.unit == "mT") {
      if (isFALSE(sigmoid.integ)) {
        data.spectr <- data.spectr
      } else {
        data.spectr$sigmoid_Integ <- pracma::cumtrapz(
          data.spectr[[B]],
          data.spectr[[Intensity]]
        )[, 1] * 10
        #
        ## rescale the `sigmoid_Integ` => from 0 to max
        data.spectr <- data.spectr %>%
          dplyr::mutate(sigmoid_Integ = abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
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
      data.NoPeak <- data.spectr %>%
        filter(!between(.data[[B]], BpeaKlim[1], BpeaKlim[2]))
      if (is.null(poly.degree)) {
        stop(" The degree of a polynomial to model the baseline is not defined. Please, specify ! ")
      } else {
        ## Polynomial baseline and integrate fit incl. derivative intensities =>
        if (lineSpecs.form == "derivative") {
          ## Polynomial baseline fit:
          #
          ## convert B to variable in formula by `get(B)`/`eval(parse(text = B))` or `eval(str2lang(B))`
          integ.baseline.fit <- stats::lm(single_Integ ~ stats::poly(get(B), degree = poly.degree),
            data = data.NoPeak
          )
          #
          ## apply fit to data.spectr
          data.spectr <- broom::augment(integ.baseline.fit, newdata = data.spectr) %>%
            ## remove the .resid colum (which is not required),
            dplyr::select(!dplyr::all_of(c(".resid"))) %>%
            ## rename column with fit
            dplyr::rename(baseline_Integ_fit = .data[[".fitted"]]) %>%
            ## subtract the baseline
            dplyr::mutate(single_Integ_correct = .data$single_Integ - .data$baseline_Integ_fit)
            ##  keep `baselin_integ_fit`
            ## & shift the integral baseline up having all the values > 0 (subtract its minimum)
          data.spectr <- data.spectr %>%
            dplyr::mutate(single_Integ_correct =
                            abs(min(.data$single_Integ_correct) - .data$single_Integ_correct))
          #
          ## integration depending on `B` unit
          if (B.unit == "G") {
            if (isFALSE(sigmoid.integ)) {
              data.spectr <- data.spectr %>%
                ## remove previous double integral (if present)
                `if`(
                  any(grepl("sigmoid_Integ", colnames(data.spectr))),
                  dplyr::select(!dplyr::all_of(c("sigmoid_Integ"))), .
                )
            } else {
              ## uncorrected double integral is `overwritten`
              data.spectr$sigmoid_Integ <- pracma::cumtrapz(
                data.spectr[[B]],
                data.spectr$single_Integ_correct
              )[, 1]
              #
              ## rescale the `sigmoid_Integ` => from 0 to max
              data.spectr <- data.spectr %>%
                dplyr::mutate(sigmoid_Integ =
                                abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
            }
          }
          if (B.unit == "mT") {
            if (isFALSE(sigmoid.integ)) {
              data.spectr <- data.spectr %>%
                ## remove previous double integral (if present)
                `if`(
                  any(grepl("sigmoid_Integ", colnames(data.spectr))),
                  dplyr::select(!dplyr::all_of(c("sigmoid_Integ"))), .
                )
            } else {
              ## uncorrected double integral is `overwritten`
              data.spectr$sigmoid_Integ <- pracma::cumtrapz(
                data.spectr[[B]],
                data.spectr$single_Integ_correct
              )[, 1] * 10
              #
              ## rescale the `sigmoid_Integ` => from 0 to max
              data.spectr <- data.spectr %>%
                dplyr::mutate(sigmoid_Integ = abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
            }
          }
        }
        ## Polynomial baseline fit integrate incl. already single integrated intensities =>
        if (lineSpecs.form == "integrated" || lineSpecs.form == "absorption") {
          ## Polynomial baseline fit:
          integ.baseline.fit <- stats::lm(get(Intensity) ~ stats::poly(get(B), degree = poly.degree),
            data = data.NoPeak
          )
          #
          ## apply fit to data.spectr
          data.spectr <- broom::augment(integ.baseline.fit, newdata = data.spectr) %>%
            ## remove the .resid colum (which is not required),
            dplyr::select(!dplyr::all_of(c(".resid"))) %>%
            ## rename column with fit
            dplyr::rename(baseline_Intens_fit = .data[[".fitted"]]) %>%
            ## subtract the baseline
            dplyr::mutate(single_Integ_correct = .data[[Intensity]] - .data$baseline_Intens_fit)
            ##  keep `baseline_Intens_fit`
            ## & shift the integral baseline up having all the values > 0 (subtract its minimum)
          data.spectr <- data.spectr %>%
            dplyr::mutate(single_Integ_correct = abs(min(.data$single_Integ_correct) -
                                                       .data$single_Integ_correct))
          #
          ## integration depending on `B` unit
          if (B.unit == "G") {
            if (isFALSE(sigmoid.integ)) {
              data.spectr <- data.spectr %>%
                ## remove previous sigmoid integral (if present)
                `if`(
                  any(grepl("sigmoid_Integ", colnames(data.spectr))),
                  dplyr::select(!dplyr::all_of(c("sigmoid_Integ"))), .
                )
            } else {
              data.spectr$sigmoid_Integ <- pracma::cumtrapz(
                data.spectr[[B]],
                data.spectr$single_Integ_correct
              )[, 1]
              #
              ## rescale the `sigmoid_Integ` => from 0 to max
              data.spectr <- data.spectr %>%
                dplyr::mutate(sigmoid_Integ = abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
            }
          }
          if (B.unit == "mT") {
            if (isFALSE(sigmoid.integ)) {
              data.spectr <- data.spectr %>%
                ## remove previous sigmoid integral (if present)
                `if`(
                  any(grepl("sigmoid_Integ", colnames(data.spectr))),
                  dplyr::select(!dplyr::all_of(c("sigmoid_Integ"))), .
                )
            } else {
              data.spectr$sigmoid_Integ <- pracma::cumtrapz(
                data.spectr[[B]],
                data.spectr$single_Integ_correct
              )[, 1] * 10
              #
              ## rescale the `sigmoid_Integ` => from 0 to max
              data.spectr <- data.spectr %>%
                dplyr::mutate(sigmoid_Integ = abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
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
    if (any(grepl("index", colnames(data.spectr)))) {
      data.spectr$index <- NULL
    }
    #
    if (isFALSE(sigmoid.integ)){
      data.spectr <- data.spectr
    } else {
      ## reorder columns in data frame
      ## `sigmoiod_integ` as last
      data.spectr <- data.spectr %>%
        dplyr::select(-sigmoid_Integ,sigmoid_Integ)
      #
      data.spectr <- data.spectr %>%
        dplyr::mutate(sigmoid_Integ = abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
    }
    #
    integrate.results <- data.spectr
    #
  } else {
    if (isFALSE(sigmoid.integ)) {
      ## bacause `ifelse()` does not work for vectors
      ## it hast be replaced by `switch()` function, like =>
      ## integrated <- FALSE
      ## a <- "Derivative Intensity", b <- "Integrated Intensity"
      ## switch(2-isFALSE(integrated),a,b) =>
      integrate.results <- switch(2 - isFALSE(correct.integ),
        data.spectr$single_Integ,
        data.spectr$single_Integ_correct
      )
    } else {
      if (lineSpecs.form == "derivative") {
        integrate.results <- list(
          single = switch(2 - isFALSE(correct.integ),
            data.spectr$single_Integ,
            data.spectr$single_Integ_correct
          ),
          sigmoid = data.spectr$sigmoid_Integ
        )
      }
      if (lineSpecs.form == "integrated" || lineSpecs.form == "absorption") {
        integrate.results <- list(
          single = switch(2 - isFALSE(correct.integ),
            data.spectr[[Intensity]],
            data.spectr$single_Integ_correct
          ),
          sigmoid = data.spectr$sigmoid_Integ
        )
      }
    }
  }
  #
  return(integrate.results)
  #
}
