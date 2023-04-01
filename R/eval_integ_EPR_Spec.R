#
#' Integration of EPR Spectral Data for Quantitative Analysis
#'
#'
#' @description
#'  Evaluates integrals of EPR spectra depending on input data (corresponding to either derivative or single integrated
#'  EPR signal form) with option to correct the single integral baseline by the polynomial of \code{poly.degree}
#'  level. Integration is done by \code{\link[pracma:trapz]{pracma::cumtrapz}} function. For the purpose
#'  of quantitative analysis the integrals are evaluated using the \code{B.units = "G"} (see below).
#'  Therefore, depending on \eqn{B} unit (either \code{"G"} or \code{"mT"}) the resulting integral data
#'  have to be optionally (in case of \code{"mT"}) multiplied by factor of \code{10} because
#'  \eqn{1 \text{mT}\equiv 10 \text{G}}. Such correction is already included in the `R` function/script.
#'
#'
#' @param data.spectrum Spectrum data frame/table with magnetic flux density (in \code{mT}
#'   or \code{G})) and that of the derivative or already single integrated intensity.
#'   \code{Index} column may be already present as well.
#' @param B Character/String pointing to magnetic flux density \code{column} (in the original
#'   \code{data.spectrum}) either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"}
#'   or \code{B = "BField"}...etc or \code{B = "B_G"} (\strong{default}).
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
#' @param double.integ Logical, whether to present (column in data frame) the double integral of \emph{dIepr_over_dB},
#'   which is required for quantitative analysis, \strong{default}: \code{double.integ = FALSE}.
#' @param output.vecs Logical, whether the `integral` \code{columns} are presented within the entire
#'   data frame (\code{output.vecs = FALSE}, \strong{default}) or called as a vectors or list for
#'   additional processing by \pkg{dplyr}.
#'
#'
#' @return Data frame/table including the EPR spectral data (general \emph{Intensity}
#'   (integrated or derivative) \emph{vs} \eqn{B}) as well as its corresponding \code{single}
#'   (\strong{column} \code{single_Integ}) and/or \code{double} (\strong{column} \code{double_Integ} required
#'   for quantitative analysis) integrals. Single integrals (referred either to derivative or already
#'   single integrated EPR spectra) can be optionally corrected by the polynomial baseline
#'   fit (\strong{column} \code{single_Integ_correct}). If \code{output.vecs = TRUE} the integrals
#'   are either simple vectors (in case of \code{correct.integ = FALSE}) or can be called from a \strong{list}
#'   by \code{...[["single"]]} (or \code{...$single}), corresponding to single integral (or integrated form of
#'   EPR spectrum), or by \code{...[["double"]]} (or \code{...$double}) corresponding to doubly integrated (
#'   or single integrated EPR spectra originally presented already in single integrals) EPR spectral data.
#'   This is especially useful for spectral (time) series EPR data, which can be handily processed
#'   \code{\link[dplyr]{group_by}} using `pipe` operators (\code{\link[magrittr]{\%>\%}}).
#'
#'
#'
#' @examples
#' \dontrun{
#' ## Evaluation of single corrected (by polynomial degree = 3) as well as double
#' ## integrals corresponding to original derivative EPR spectrum.
#' ## Integrals are presented as a columns within the en tire data frame
#' eval_integ_EPR_Spec(EPR_spectral_data_table,
#'                     B = "FieldB",
#'                     B.unit = "mT"
#'                     Blim = c(348.2,351.1),
#'                     correct.integ = TRUE,
#'                     BpeaKlim = c(349,350),
#'                     poly.degree = 3,
#'                     double.integ = TRUE)
#'
#' ## Double integral evaluation corresponding to original non-corrected
#' ## integral/intensity (non-derivative EPR signal). Double Integral
#' ## (which is actually single integral of the original form, however
#' ## possesses the sigmoidal curve form) is exported into vectors list
#' eval_integ_EPR_Spec(EPR_spectral_data_table,
#'                     B = "B_G",
#'                     B.unit = "G",
#'                     double.integ = TRUE,
#'                     output.vecs = TRUE)
#'
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
                                double.integ = FALSE,
                                output.vecs = FALSE) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  single_Integ <- NULL
  double_Integ <- NULL
  baseline_Integ_fit <- NULL
  baseline_Intens_fit <- NULL
  single_Integ_correct <- NULL
  #
  ## Define limits if `Blim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.B.region <- c(min(data.spectrum[[B]]),max(data.spectrum[[B]]))
  Blim <- Blim %>% `if`(is.null(Blim),data.B.region, .)
  #
  ## evaluating primary integral based on `Intensity`
  ## and `B` (`B.unit` has to be in "G") parameter
  ## otherwise each integration has to be multiplied by 10,
  ## because 1 mT = 10 G
  ## First of all define vectors with intensity column names =>
  slct.vec.deriv.EPR.intens <- c("dB","_dB","intens","deriv","Intens",
                             "Deriv","dIepr","dIepr_over_dB","dIepr_dB",
                             "MW_Absorp","MW_intens","MW_Intens")
  ## &
  slct.vec.integ.EPR.intens <- c("single","Single","SInteg","sinteg","s_integ",
                             "single_","singleinteg","sintegral","integral",
                             "Integral","sInteg_","sInteg","singleI","integ","Integ")
  #
  ## primary data for integration
  data.spectrum <- data.spectrum %>%
    dplyr::filter(dplyr::between(.data[[B]],Blim[1],Blim[2]))
  #
  if (sjmisc::str_contains(Intensity,slct.vec.deriv.EPR.intens,logic = "or")){
    #
    ## integration depending on `B` unit
    if (B.unit == "G"){
      data.spectrum <- data.spectrum %>%
        dplyr::mutate(single_Integ = pracma::cumtrapz(.data[[B]],
                                                      .data[[Intensity]])[,1])
      if (isFALSE(double.integ)){
        data.spectrum <- data.spectrum
      } else{
        data.spectrum$double_Integ <- pracma::cumtrapz(data.spectrum[[B]],
                                                       data.spectrum$single_Integ)[,1]
      }
    }
    if (B.unit == "mT"){
      data.spectrum <- data.spectrum %>%
        dplyr::mutate(single_Integ = pracma::cumtrapz(.data[[B]],
                                                      .data[[Intensity]])[,1]*10)
      if (isFALSE(double.integ)){
        data.spectrum <- data.spectrum
      } else{
        data.spectrum$double_Integ <- pracma::cumtrapz(data.spectrum[[B]],
                                                       data.spectrum$single_Integ)[,1]*10
      }
    }
  }
  if (sjmisc::str_contains(Intensity,slct.vec.integ.EPR.intens, logic = "or")){
    #
    ## integration depending on `B` unit
    if (B.unit == "G"){
      if (isFALSE(double.integ)){
        data.spectrum <- data.spectrum
      } else {
        data.spectrum$double_Integ <- pracma::cumtrapz(data.spectrum[[B]],
                                                       data.spectrum[[Intensity]])[,1]
      }
    }
    if (B.unit == "mT"){
      if (isFALSE(double.integ)){
        data.spectrum <- data.spectrum
      } else{
        data.spectrum$double_Integ <- pracma::cumtrapz(data.spectrum[[B]],
                                                       data.spectrum[[Intensity]])[,1]*10
      }
    }
  }
  #
  ## Integral baseline correction
  if (isTRUE(correct.integ)){
    # select region / range / interval of the peak, which won't be
    ## considered ("!") for the baseline correction / fit
    ## (limits are 'BpeaKlim[1]'<=> 'start','BpeaKlim[2]' <=> 'end'):
    if (is.null(BpeaKlim)){
      stop(" The `B`-range for the peak baseline correction is not defined. Please, specify the range ! ")
    } else{
      data.NoPeak <- data.spectrum %>%
        filter(!between(.data[[B]], BpeaKlim[1], BpeaKlim[2]))
      if (is.null(poly.degree)){
        stop( " The degree of a polynomial to model the baseline is not defined. Please, specify ! " )
      } else{
        ## Polynomial baseline and integrate fit incl. derivative intensities =>
        if (sjmisc::str_contains(Intensity,slct.vec.deriv.EPR.intens,logic = "or")){
          ## Polynomial baseline fit:
          #
          ## convert B to variable in formula by `get(B)`/`eval(parse(text = B))` or `eval(str2lang(B))`
          integ.baseline.fit <- stats::lm(single_Integ ~ stats::poly(get(B),degree = poly.degree),
                                          data = data.NoPeak)
          #
          ## apply fit to data.spectrum
          data.spectrum <- broom::augment(integ.baseline.fit, newdata = data.spectrum) %>%
            ## remove the .resid colum (which is not required),
            dplyr::select(-.data[[".resid"]]) %>%
            ## rename column with fit
            dplyr::rename(baseline_Integ_fit = .data[[".fitted"]]) %>%
            ## subtract the baseline
            dplyr::mutate(single_Integ_correct = .data$single_Integ - .data$baseline_Integ_fit) %>%
            ##  keep `baselin_integ_fit`
            ## & shift the integral baseline up having all the values > 0 (subtract its minimum)
            dplyr::mutate(single_Integ_correct = single_Integ_correct - min(.data$single_Integ_correct))
          #
          ## integration depending on `B` unit
          if (B.unit == "G"){
            if (isFALSE(double.integ)){
              data.spectrum <- data.spectrum %>%
                # remove index and previous double integral (if present)
                dplyr::select(-.data$index) %>%
                `if`(any(grepl("double_Integ",colnames(data.spectrum))),
                     dplyr::select(-double_Integ), .)

            } else{
              ## uncorrected double integral is `overwritten`
              data.spectrum$double_Integ <- pracma::cumtrapz(data.spectrum[[B]],
                                                             data.spectrum$single_Integ_correct)[,1]
            }
          }
          if (B.unit == "mT"){
            if (isFALSE(double.integ)){
              data.spectrum <- data.spectrum %>%
                # remove index and previous double integral (if present)
                dplyr::select(-.data$index) %>%
                `if`(any(grepl("double_Integ",colnames(data.spectrum))),
                     dplyr::select(-double_Integ), .)
            } else{
              ## uncorrected double integral is `overwritten`
              data.spectrum$double_Integ <- pracma::cumtrapz(data.spectrum[[B]],
                                                             data.spectrum$single_Integ_correct)[,1]*10
           }
          }
        }
        ## Polynomial baseline fit integrate incl. already single integrated intensities =>
        if (sjmisc::str_contains(Intensity,slct.vec.integ.EPR.intens, logic = "or")){
          ## Polynomial baseline fit:
          integ.baseline.fit <- stats::lm(get(Intensity) ~ stats::poly(get(B),degree = poly.degree),
                                          data = data.NoPeak)
          #
          ## apply fit to data.spectrum
          data.spectrum <- broom::augment(integ.baseline.fit, newdata = data.spectrum) %>%
            ## remove the .resid colum (which is not required),
            dplyr::select(-.data[[".resid"]]) %>%
            ## rename column with fit
            dplyr::rename(baseline_Intens_fit = .data[[".fitted"]]) %>%
            ## subtract the baseline
            dplyr::mutate(single_Integ_correct = .data[[Intensity]] - .data$baseline_Intens_fit) %>%
            ##  keep `baseline_Intens_fit`
            ## & shift the integral baseline up having all the values > 0 (subtract its minimum)
            dplyr::mutate(single_Integ_correct = single_Integ_correct - min(.data$single_Integ_correct))
          #
          ## integration depending on `B` unit
          if (B.unit == "G"){
            if (isFALSE(double.integ)){
              data.spectrum <- data.spectrum %>%
                # remove index and previous double integral (if present)
                dplyr::select(-.data$index) %>%
                `if`(any(grepl("double_Integ",colnames(data.spectrum))),
                     dplyr::select(-double_Integ), .)
            } else{
              data.spectrum$double_Integ <- pracma::cumtrapz(data.spectrum[[B]],
                                                             data.spectrum$single_Integ_correct)[,1]
            }
          }
          if (B.unit == "mT"){
            if (isFALSE(double.integ)){
              data.spectrum <- data.spectrum %>%
                # remove index and previous double integral (if present)
                dplyr::select(-.data$index) %>%
                `if`(any(grepl("double_Integ",colnames(data.spectrum))),
                     dplyr::select(-double_Integ), .)
            } else{
              data.spectrum$double_Integ <- pracma::cumtrapz(data.spectrum[[B]],
                                                             data.spectrum$single_Integ_correct)[,1]*10
            }
          }
        }
      }
    }
  }
  #
  ## Vectorized output for the EPR spectral series
  if (isFALSE(output.vecs)){
    integrate.results <- data.spectrum
  } else{
    if (isFALSE(double.integ)){
      ## bacause `ifelse()` does not work for vectors
      ## it hast be replaced by `switch()` function, like =>
      ## integrated <- FALSE
      ## a <- "Derivative Intensity", b <- "Integrated Intensity"
      ## switch(2-isFALSE(integrated),a,b) =>
      integrate.results <- switch(2-isFALSE(correct.integ),
                                  data.spectrum$single_Integ,
                                  data.spectrum$single_Integ_correct)
    } else{
      if (sjmisc::str_contains(Intensity,slct.vec.deriv.EPR.intens,logic = "or")){
        integrate.results <- list(single = switch(2-isFALSE(correct.integ),
                                                  data.spectrum$single_Integ,
                                                  data.spectrum$single_Integ_correct),
                                  double = data.spectrum$double_Integ)
      }
      if (sjmisc::str_contains(Intensity,slct.vec.integ.EPR.intens, logic = "or")){
        integrate.results <- list(single = switch(2-isFALSE(correct.integ),
                                                  data.spectrum$Intensity,
                                                  data.spectrum$single_Integ_correct),
                                  double = data.spectrum$double_Integ)
      }
    }
  }
  #
  return(integrate.results)
  #
}
