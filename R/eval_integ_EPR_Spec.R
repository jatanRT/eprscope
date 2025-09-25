#
#' Integration of EPR Spectrum/Data for Quantitative Analysis
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'  Evaluates integrals of EPR spectra (based on the \code{\link[pracma:trapz]{pracma::cumtrapz}} function)
#'  depending on input data => either corresponding to \code{derivative}
#'  or single \code{integrated} EPR signal form, with the option to correct the single integral baseline
#'  by the polynomial fit of the \code{poly.degree} level. \strong{For EPR time/temperature/...etc spectral series},
#'  (data frame must be available in \href{https://r4ds.had.co.nz/tidy-data.html}{tidy/long table format}),
#'  there is an \strong{option to integrate all EPR spectra literally in one step} (see also \code{Examples}),
#'  similarly to function available in acquisition/processing software at EPR spectrometers.
#'
#'
#' @details
#'  The relative error of the cumulative trapezoidal (\code{cumtrapz}) function is minimal,
#'  usually falling into the range of \eqn{\langle 1,5\rangle\,\%} or even lower, depending on the spectral
#'  data resolution (see Epperson JF (2013) and Seeburger P (2023) in the \code{References}).
#'  Therefore, the better the resolution, the more accurate the integral. If the initial EPR spectrum displays low
#'  signal-to-noise ratio, the integral often looses its sigmoid-shape
#'  and thus, the EPR spectrum has to be either simulated (see also \code{vignette("functionality")})
#'  or smoothed by the \code{\link{smooth_EPR_Spec_by_npreg}}, prior to integration. Afterwards,
#'  integrals are evaluated from the simulated or smoothed EPR spectra.
#'  For the purpose of quantitative analysis, the integrals are evaluated using the \code{B.units = "G"}
#'  (see Arguments). Hence, depending on \eqn{B}-unit (\code{G} or \code{mT} or \code{T}) each resulting integral
#'  column have to be optionally (in case of \code{mT} or \code{T}) multiplied by the factor of \code{10} or \code{10000},
#'  respectively, because \eqn{1\,\text{mT}\equiv 10\,\text{G}} and \eqn{1\,\text{T}\equiv 10^4\,\text{G}}.
#'  Such corrections are already included in the function/script.
#'  Instead of "double integral/integ." the term "sigmoid integral/integ." is used. "Double integral"
#'  \strong{in the case of originally single integrated EPR spectrum} (see \code{data.spectr}
#'  and \code{Intensity}) \strong{is confusing. In such case, the EPR spectrum is integrated just once.}
#'
#'
#' @references
#'  Weber RT (2011). \emph{Xenon User's Guide}. Bruker BioSpin Manual Version 1.3, Software Version 1.1b50.
#'
#'  Hans W. Borchers (2023). \emph{pracma: Practical Numerical Math Functions}. R package version 2.4.4,
#'  \url{https://cran.r-project.org/web/packages/pracma/index.html}.
#'
#'  Seeburger P (2023). “Numerical Integration - Midpoint, Trapezoid, Simpson's rule”,
#'  \url{http://tinyurl.com/trapz-integral}.
#'
#'  Svirin A (2023). “Calculus, Integration of Functions-Trapezoidal Rule”,
#'  \url{https://math24.net/trapezoidal-rule.html}.
#'
#'  Epperson JF (2013). \emph{An Introduction to Numerical Methods and Analysis}. Wiley and Sons,
#'  ISBN 978-1-118-36759-9, \url{https://books.google.cz/books?id=310lAgAAQBAJ}.
#'
#'
#' @param data.spectr Spectrum data frame/table object with magnetic flux density (in \code{mT}
#'   or \code{G} or \code{T}) and that of the derivative or already single integrated intensity.
#'   \code{index} column may be already present as well.
#' @param B Character string, pointing to magnetic flux density column header (in the original
#'   \code{data.spectr}) either in \code{millitesla}/\code{tesla} or in \code{Gauss}, such as \code{B = "B_mT"}
#'   or \code{B = "B_G"} (\strong{default}) or \code{B = "Field"}...etc.
#' @param Intensity Character string, pointing to column name of either derivative
#'   (e.g. \code{Intensity = "dIepr_over_dB"}, \strong{default}) or single integrated EPR
#'   spectrum (e.g. \code{Intensity = "single_Integrated"}) within the actual \code{data.spectr}.
#' @param lineSpecs.form Character string, describing either \code{"derivative"} (\strong{default})
#'   or \code{"integrated"} (i.e. \code{"absorption"}, which can be used as well) line form of the analyzed
#'   EPR spectrum/data.
#' @param B.unit Character string pointing to unit of magnetic flux density, which
#'   is to be presented on \eqn{x(B)}-axis of the EPR spectrum,
#'   like \code{"G"} ("Gauss"), \code{"mT"} ("millitesla") or \code{"T"} ("Tesla").
#'   \strong{Default}: \code{B.unit = "G"}.
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G}/\code{T} corresponding to lower and upper
#'   limit of the selected \eqn{B}-region, e.g. \code{Blim = c(3495.4,3595.4)}.
#'   \strong{Default}: \code{Blim = NULL} (corresponding to the entire spectral \eqn{B}-range).
#' @param correct.integ Logical, whether to correct the integral by baseline polynomial model fit.
#'   \strong{Default}: \code{correct.integ = FALSE}.
#' @param BpeaKlim Numeric vector, magnetic flux density in \code{mT}/\code{G}/\code{T} corresponding to lower
#'   and upper limit of the SELECTED \eqn{B}-PEAK REGION, e.g. \code{BpeaKlim = c(3535.4,3555.4)}.
#'   This is the region (without the peak), which is actually not considered for the baseline fit.
#' @param poly.degree Numeric, degree of the polynomial function used to fit baseline under the single
#'   integrated curve of the original EPR spectrum (see also \code{BpeaKlim}).
#' @param sigmoid.integ Logical, whether to involve (column in data frame) double integral or single
#'   integral (if the \code{data.spectr} and \code{Intesity} are already in single integrated form),
#'   in sigmoid shape, which is required for the quantitative analysis,
#'   \strong{default}: \code{sigmoid.integ = FALSE}.
#' @param output.vecs Logical, whether the "integral" columns are presented within the original \code{data.spectr}
#'   data frame (\code{output.vecs = FALSE}, \strong{default}) or called as a vectors or list for
#'   additional processing of spectral data series by \href{https://dplyr.tidyverse.org/}{dplyr}
#'   (see \code{Value} and \code{Examples}).
#'
#'
#' @return The integration results may be divided into following types, depending on the above-described
#'   arguments. Generally, they are either data frames, including the original data and the integrals
#'   (\code{output.vecs = FALSE}) or vectors/vectors list, corresponding to individual baseline
#'   corrected/uncorrected integrals (\code{output.vecs = TRUE}). This is especially useful
#'   for spectral (time) series EPR data, which can be handily processed
#'   by the \code{\link[dplyr]{group_by}} using the
#'   \code{pipe} operators (\code{\link[magrittr]{\%>\%}}).
#'   \enumerate{
#'   \item Data frame/table, including EPR spectral data (general \code{Intensity}
#'   (integrated or derivative) \emph{vs} \eqn{B}) as well as its corresponding \code{single}
#'   (column \code{single_Integ}) integral. This is the case if only a single
#'   uncorrected integral is required.
#'
#'   \item Data frame/table with single integral/intensity already corrected by a certain degree
#'   of polynomial baseline (fitted to experimental baseline without peak). Single integrals
#'   are related either to derivative or already integrated EPR spectra where corrected
#'   integral column header is denoted as \code{single_Integ_correct}. This is the case
#'   if \code{correct.integ = TRUE} and \code{sigmoid.integ = FALSE} + \code{output.vecs = FALSE}.
#'
#'   \item Data frame with \code{single} and \code{double/sigmoid} integral column/variable
#'   (\code{sigmoid_Integ}), essential for the quantitative analysis. For such case it applies:
#'   \code{output.vecs = FALSE} and \code{correct.integ = FALSE}.
#'
#'   \item Data frame in case of \code{correct.integ = TRUE}, \code{sigmoid.integ = TRUE}
#'   and \code{output.vecs = FALSE}. It contains the original data frame columns + corrected
#'   single integral (\code{single_Integ_correct}) and double/sigmoid integral
#'   (\code{sigmoid_Integ}) which is evaluated from the baseline corrected single one.
#'   Therefore, such double/sigmoid integral is suitable for the accurate determination
#'   of radical (paramagnetic centers) amount.
#'
#'   \item Numeric vector, corresponding to baseline uncorrected/corrected single integral
#'   in case of \code{sigmoid.integ = FALSE} + \code{output.vecs = TRUE}.
#'
#'   \item List of numeric vectors (if \code{output.vecs = TRUE}) corresponding to:
#'     \describe{
#'     \item{single}{Corrected or uncorrected single integral (in case of derivative form),
#'     depending on the \code{correct.integ} argument.}
#'     \item{sigmoid}{Double integral (in case of derivative form) or single integral
#'     (in case of integrated spectral form) for quantitative analysis.}
#'     }
#'   }
#'
#'
#' @examples
#' ## loading the built-in package example
#' ## time series EPR spectra:
#' triarylamine.decay.series.dsc.path <-
#' load_data_example(file =
#'         "Triarylamine_radCat_decay_series.DSC")
#' triarylamine.decay.series.asc.path <-
#' load_data_example(file =
#'         "Triarylamine_radCat_decay_series.zip")
#' unzip(triarylamine.decay.series.asc.path,
#'       exdir = tempdir()
#'       )
#' ## loading the kinetics:
#' triarylamine.decay.series.data <-
#'   readEPR_Exp_Specs_kin(name.root =
#'     "Triarylamine_radCat_decay_series",
#'     dir_ASC = tempdir(),
#'     dir_dsc_par =
#'       system.file("extdata",
#'                   package = "eprscope")
#'    )
#' #
#' ## select the first spectrum
#' triarylamine.decay.series.data1st <-
#'    triarylamine.decay.series.data$df %>%
#'      dplyr::filter(time_s ==
#'        triarylamine.decay.series.data$time[1])
#' #
#' ## integrate the first spectrum with default arguments
#' triarylamine.decay.data1st.integ01 <-
#'   eval_integ_EPR_Spec(triarylamine.decay.series.data1st)
#' #
#' ## data frame preview
#' head(triarylamine.decay.data1st.integ01)
#' #
#' ## integration (including baseline correction)
#' ## of the 1st spectrum from the series
#' triarylamine.decay.data1st.integ02 <-
#'   eval_integ_EPR_Spec(triarylamine.decay.series.data1st,
#'     ## limits obtained from interactive spectrum:
#'     BpeaKlim = c(3471.5,3512.5),
#'     Blim = c(3425,3550),
#'     correct.integ = TRUE,
#'     poly.degree = 3,
#'     sigmoid.integ = TRUE
#'     )
#' #
#' ## data frame preview
#' head(triarylamine.decay.data1st.integ02)
#' #
#' ## plot the single integrated EPR spectrum,
#' ## including baseline correction
#' plot_EPR_Specs(triarylamine.decay.data1st.integ02,
#'                x = "B_G",
#'                x.unit = "G",
#'                Intensity = "single_Integ_correct",
#'                lineSpecs.form = "integrated"
#'              )
#' #
#' ## plot, corresponding to double/sigmoid integral,
#' ## which is related to corrected single integral
#' plot_EPR_Specs(triarylamine.decay.data1st.integ02,
#'                x = "B_G",
#'                x.unit = "G",
#'                Intensity = "sigmoid_Integ",
#'                lineSpecs.form = "integrated"
#'              )
#' #
#' ## vectorized output of the uncorrected `sigmoid_integral`
#' triarylamine.decay.data1st.integ03 <-
#'   eval_integ_EPR_Spec(triarylamine.decay.series.data1st,
#'                       sigmoid.integ = TRUE,
#'                       output.vecs = TRUE)[["sigmoid"]]
#' #
#' ## preview of the first 6 values
#' triarylamine.decay.data1st.integ03[1:6]
#' #
#' ## incorporation of vectorized integration into
#' ## data "pipe" ("%>%") `dplyr` processing of EPR spectral
#' ## time series, creating column with `sigmoid` integral
#' ## where its corresponding single integral (intensity)
#' ## has undergone a baseline correction, finally the max. value
#' ## of all sigmoid integrals along with the time is
#' ## summarized in data frame for quantitative kinetic analysis
#' triarylamine.decay.data.integs <-
#'   triarylamine.decay.series.data$df %>%
#'   dplyr::group_by(time_s) %>%
#'   dplyr::filter(dplyr::between(B_G,3425,3550)) %>%
#'   dplyr::mutate(sigmoid_Integ =
#'     eval_integ_EPR_Spec(dplyr::pick(B_G,dIepr_over_dB),
#'                         correct.integ = TRUE,
#'                         BpeaKlim = c(3471.5,3512.5),
#'                         poly.degree = 3,
#'                         sigmoid.integ = TRUE,
#'                         output.vecs = TRUE)$sigmoid
#'                        ) %>%
#'   dplyr::summarize(Area = max(sigmoid_Integ))
#' ## in such case `Blim` range is not defined by `eval_integ_EPR_Spec`,
#' ## the `Blim = NULL` and `dplyr::between()` must be set !!!
#' #
#' ## preview of the final data frame
#' head(triarylamine.decay.data.integs)
#' #
#' ## preview of the simple plot
#' ggplot2::ggplot(triarylamine.decay.data.integs) +
#'   ggplot2::geom_point(ggplot2::aes(x = time_s,y = Area))
#' #
#' ## this does not correspond to example
#' ## in `eval_kinR_EPR_modelFit`, `eval_kin_EPR_ODE_model`
#' ## or in `plot_theme_NoY_ticks` based on the same input data,
#' ## as those `Area` vs `time` relations were evaluated using
#' ## the simulated EPR spectra (see also `vignette("datasets")`)
#' #
#' \dontrun{
#' ## Similar to previous data processing, creating both: corrected
#' ## single integral + sigmoid integral for each time within the spectral
#' ## series. Sigmoid integral was evalutated from the single one by
#' ## `cumtrapz()` function from `pracma` package and finally re-scaled.
#' triarylamine.decay.data.integs <-
#'   triarylamine.decay.series.data$df %>%
#'   dplyr::group_by(time_s) %>%
#'   eval_integ_EPR_Spec(correct.integ = TRUE,
#'                       Blim = c(3425,3550),
#'                       BpeaKlim = c(3472.417,3505.5),
#'                       poly.degree = 3) %>%
#'  dplyr::group_by(time_s) %>%
#'  dplyr::mutate(sigmoid_Integ =
#'    pracma::cumtrapz(B_G,single_Integ_correct)[,1]) %>%
#'  dplyr::mutate(sigmoid_Integ_correct =
#'    abs(min(sigmoid_Integ) - sigmoid_Integ))
#' }
#'
#'
#' @export
#'
#'
#' @importFrom pracma cumtrapz
#' @importFrom broom augment
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
  ## because 1 mT = 10 G or by 1e4, if `B.unit = "T"`,
  ## therefore function to distinguish between units =>
  fn_units <- function(unit){
    if (unit == "G"){
      return(0)
    }
    if (unit == "mT"){
      return(1)
    }
    if (unit == "T"){
      return(2)
    }
  }
  #
  ## function to switch/select integrals based on `B.unit`
  fn_switch_integ <- function(u = B.unit,B,I){
    result <-
      switch(3 - fn_units(unit = u),
             cumtrapz(x = B, y = I)[, 1] * 1e+4,
             cumtrapz(x = B, y = I)[, 1] * 10,
             cumtrapz(x = B, y = I)[, 1]
      )
    return(result)
  }
  #
  ## primary data for integration
  data.spectr <- data.spectr %>%
    dplyr::filter(dplyr::between(.data[[B]], Blim[1], Blim[2]))
  #
  if (grepl("deriv|Deriv",lineSpecs.form)) {
    #
    ## integration depending on `B` unit
    data.spectr <- data.spectr %>%
      dplyr::mutate(single_Integ = fn_switch_integ(B = .data[[B]],I = .data[[Intensity]]))
    # integral with proper scaling (from `0`)
    data.spectr <- data.spectr %>%
      dplyr::mutate(single_Integ = abs(min(.data$single_Integ) - .data$single_Integ))
    #
    if (isFALSE(sigmoid.integ)) {
      data.spectr <- data.spectr
    } else {
      data.spectr$sigmoid_Integ <-
        fn_switch_integ(B = data.spectr[[B]],I = data.spectr$single_Integ)
      # scaling integral
      data.spectr <- data.spectr %>%
        dplyr::mutate(sigmoid_Integ = abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
    }
  }
  if (grepl("integ|Integ|absorpt|Absorpt",lineSpecs.form)) {
    #
    ## integration depending on `B` unit
    if (isFALSE(sigmoid.integ)) {
      # scale integral/intensity simultaneously
      data.spectr <- data.spectr %>%
        dplyr::mutate(!!rlang::quo_name(Intensity) :=
                        abs(min(.data[[Intensity]]) - .data[[Intensity]]))
    } else {
      data.spectr$sigmoid_Integ <-
        fn_switch_integ(B = data.spectr[[B]],I = data.spectr[[Intensity]])
      # scaling integral
      data.spectr <- data.spectr %>%
        dplyr::mutate(sigmoid_Integ = abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
    }
  }
  #
  ## Integral baseline correction
  if (isTRUE(correct.integ)) {
    # select region / range / interval of the peak, which won't be
    ## considered ("!") for the baseline correction / fit
    ## (limits are 'BpeaKlim[1]'<=> 'start','BpeaKlim[2]' <=> 'end'):
    if (is.null(BpeaKlim)) {
      stop(" The `B`-range for the peak baseline correction is not specified.\n
           Please, define the range ! ")
    } else {
      data.NoPeak <- data.spectr %>%
        filter(!between(.data[[B]], BpeaKlim[1], BpeaKlim[2]))
      if (is.null(poly.degree)) {
        stop("Degree of a polynomial to fit the baseline is not specified.\n
             Please, define the `poly.degree` ! ")
      } else {
        ## Polynomial baseline and integrate fit incl. derivative intensities =>
        if (grepl("deriv|Deriv",lineSpecs.form)) {
          ## Polynomial baseline fit:
          #
          ## convert B to variable in formula
          ## by `get(B)`/`eval(parse(text = B))` or `eval(str2lang(B))`
          integ.baseline.fit <-
            stats::lm(
              single_Integ ~ stats::poly(get(B), degree = poly.degree,raw = TRUE),
              data = data.NoPeak
          )
          #
          ## apply fit to data.spectr
          data.spectr <- augment(integ.baseline.fit, newdata = data.spectr) %>%
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
          if (isFALSE(sigmoid.integ)) {
            data.spectr <- data.spectr %>%
              ## remove previous double integral (if present)
              `if`(
                any(grepl("sigmoid_Integ", colnames(data.spectr))),
                dplyr::select(!dplyr::all_of(c("sigmoid_Integ"))), .
              )
          } else {
            ## uncorrected double integral is `overwritten`
            data.spectr$sigmoid_Integ <-
              fn_switch_integ(B = data.spectr[[B]],I = data.spectr$single_Integ_correct)
          }
        }
        ## Polynomial baseline fit integrate incl. already single integrated intensities =>
        if (grepl("integ|Integ|absorpt|Absorpt",lineSpecs.form)) {
          ## Polynomial baseline fit:
          integ.baseline.fit <-
            stats::lm(
              get(Intensity) ~ stats::poly(get(B), degree = poly.degree,raw = TRUE),
              data = data.NoPeak
          )
          #
          ## apply fit to data.spectr
          data.spectr <- augment(integ.baseline.fit, newdata = data.spectr) %>%
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
          if (isFALSE(sigmoid.integ)) {
            data.spectr <- data.spectr %>%
              ## remove previous sigmoid integral (if present)
              `if`(
                any(grepl("sigmoid_Integ", colnames(data.spectr))),
                dplyr::select(!dplyr::all_of(c("sigmoid_Integ"))), .
              )
          } else {
            data.spectr$sigmoid_Integ <-
              fn_switch_integ(B = data.spectr[[B]],I = data.spectr$single_Integ_correct)
          }
        }
        ## ----------- finally re-scale the integrals => from 0 to max -----------
        if (isTRUE(sigmoid.integ)){
          data.spectr <- data.spectr %>%
            dplyr::mutate(sigmoid_Integ = abs(min(.data$sigmoid_Integ) - .data$sigmoid_Integ))
        } else {
          data.spectr <- data.spectr
        }
        #
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
      if (grepl("deriv|Deriv",lineSpecs.form)) {
        integrate.results <- list(
          single = switch(2 - isFALSE(correct.integ),
            data.spectr$single_Integ,
            data.spectr$single_Integ_correct
          ),
          sigmoid = data.spectr$sigmoid_Integ
        )
      }
      if (grepl("integ|Integ|absorpt|Absorpt",lineSpecs.form)) {
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
