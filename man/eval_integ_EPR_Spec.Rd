% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval_integ_EPR_Spec.R
\name{eval_integ_EPR_Spec}
\alias{eval_integ_EPR_Spec}
\title{Integration of EPR Spectrum/Data for Quantitative Analysis}
\usage{
eval_integ_EPR_Spec(
  data.spectr,
  B = "B_G",
  B.unit = "G",
  Intensity = "dIepr_over_dB",
  lineSpecs.form = "derivative",
  Blim = NULL,
  correct.integ = FALSE,
  BpeaKlim = NULL,
  poly.degree = NULL,
  sigmoid.integ = FALSE,
  output.vecs = FALSE
)
}
\arguments{
\item{data.spectr}{Spectrum data frame/table object with magnetic flux density (in \code{mT}
or \code{G} or \code{T}) and that of the derivative or already single integrated intensity.
\code{index} column may be already present as well.}

\item{B}{Character string, pointing to magnetic flux density column header (in the original
\code{data.spectr}) either in \code{millitesla}/\code{tesla} or in \code{Gauss}, such as \code{B = "B_mT"}
or \code{B = "B_G"} (\strong{default}) or \code{B = "Field"}...etc.}

\item{B.unit}{Character string pointing to unit of magnetic flux density, which
is to be presented on \eqn{x(B)}-axis of the EPR spectrum,
like \code{"G"} ("Gauss"), \code{"mT"} ("millitesla") or \code{"T"} ("Tesla").
\strong{Default}: \code{B.unit = "G"}.}

\item{Intensity}{Character string, pointing to column name of either derivative
(e.g. \code{Intensity = "dIepr_over_dB"}, \strong{default}) or single integrated EPR
spectrum (e.g. \code{Intensity = "single_Integrated"}) within the actual \code{data.spectr}.}

\item{lineSpecs.form}{Character string, describing either \code{"derivative"} (\strong{default})
or \code{"integrated"} (i.e. \code{"absorption"}, which can be used as well) line form of the analyzed
EPR spectrum/data.}

\item{Blim}{Numeric vector, magnetic flux density in \code{mT}/\code{G}/\code{T} corresponding to lower and upper
limit of the selected \eqn{B}-region, e.g. \code{Blim = c(3495.4,3595.4)}.
\strong{Default}: \code{Blim = NULL} (corresponding to the entire spectral \eqn{B}-range).}

\item{correct.integ}{Logical, whether to correct the integral by baseline polynomial model fit.
\strong{Default}: \code{correct.integ = FALSE}.}

\item{BpeaKlim}{Numeric vector, magnetic flux density in \code{mT}/\code{G}/\code{T} corresponding to lower
and upper limit of the SELECTED \eqn{B}-PEAK REGION, e.g. \code{BpeaKlim = c(3535.4,3555.4)}.
This is the region (without the peak), which is actually not considered for the baseline fit.}

\item{poly.degree}{Numeric, degree of the polynomial function used to fit baseline under the single
integrated curve of the original EPR spectrum (see also \code{BpeaKlim}).}

\item{sigmoid.integ}{Logical, whether to involve (column in data frame) double integral or single
integral (if the \code{data.spectr} and \code{Intesity} are already in single integrated form),
in sigmoid shape, which is required for the quantitative analysis,
\strong{default}: \code{sigmoid.integ = FALSE}.}

\item{output.vecs}{Logical, whether the "integral" columns are presented within the original \code{data.spectr}
data frame (\code{output.vecs = FALSE}, \strong{default}) or called as a vectors or list for
additional processing of spectral data series by \href{https://dplyr.tidyverse.org/}{dplyr}
(see \code{Value} and \code{Examples}).}
}
\value{
The integration results may be divided into following types, depending on the above-described
  arguments. Generally, they are either data frames, including the original data and the integrals
  (\code{output.vecs = FALSE}) or vectors/vectors list, corresponding to individual baseline
  corrected/uncorrected integrals (\code{output.vecs = TRUE}). This is especially useful
  for spectral (time) series EPR data, which can be handily processed
  by the \code{\link[dplyr]{group_by}} using the
  \code{pipe} operators (\code{\link[magrittr]{\%>\%}}).
  \enumerate{
  \item Data frame/table, including EPR spectral data (general \code{Intensity}
  (integrated or derivative) \emph{vs} \eqn{B}) as well as its corresponding \code{single}
  (column \code{single_Integ}) integral. This is the case if only a single
  uncorrected integral is required.

  \item Data frame/table with single integral/intensity already corrected by a certain degree
  of polynomial baseline (fitted to experimental baseline without peak). Single integrals
  are related either to derivative or already integrated EPR spectra where corrected
  integral column header is denoted as \code{single_Integ_correct}. This is the case
  if \code{correct.integ = TRUE} and \code{sigmoid.integ = FALSE} + \code{output.vecs = FALSE}.

  \item Data frame with \code{single} and \code{double/sigmoid} integral column/variable
  (\code{sigmoid_Integ}), essential for the quantitative analysis. For such case it applies:
  \code{output.vecs = FALSE} and \code{correct.integ = FALSE}.

  \item Data frame in case of \code{correct.integ = TRUE}, \code{sigmoid.integ = TRUE}
  and \code{output.vecs = FALSE}. It contains the original data frame columns + corrected
  single integral (\code{single_Integ_correct}) and double/sigmoid integral
  (\code{sigmoid_Integ}) which is evaluated from the baseline corrected single one.
  Therefore, such double/sigmoid integral is suitable for the accurate determination
  of radical (paramagnetic centers) amount.

  \item Numeric vector, corresponding to baseline uncorrected/corrected single integral
  in case of \code{sigmoid.integ = FALSE} + \code{output.vecs = TRUE}.

  \item List of numeric vectors (if \code{output.vecs = TRUE}) corresponding to:
    \describe{
    \item{single}{Corrected or uncorrected single integral (in case of derivative form),
    depending on the \code{correct.integ} argument.}
    \item{sigmoid}{Double integral (in case of derivative form) or single integral
    (in case of integrated spectral form) for quantitative analysis.}
    }
  }
}
\description{
Evaluates integrals of EPR spectra (based on the \code{\link[pracma:trapz]{pracma::cumtrapz}} function)
 depending on input data => either corresponding to \code{derivative}
 or single \code{integrated} EPR signal form, with the option to correct the single integral baseline
 by the polynomial fit of the \code{poly.degree} level. \strong{For EPR time/temperature/...etc spectral series},
 (data frame must be available in \href{https://r4ds.had.co.nz/tidy-data.html}{tidy/long table format}),
 there is an \strong{option to integrate all EPR spectra literally in one step} (see also \code{Examples}),
 similarly to function available in acquisition/processing software at EPR spectrometers.
}
\details{
The relative error of the cumulative trapezoidal (\code{cumtrapz}) function is minimal,
 usually falling into the range of \eqn{\langle 1,5\rangle\,\%} or even lower, depending on the spectral
 data resolution (see Epperson JF (2013) and Seeburger P (2023) in the \code{References}).
 Therefore, the better the resolution, the more accurate the integral. If the initial EPR spectrum displays low
 signal-to-noise ratio, the integral often looses its sigmoid-shape
 and thus, the EPR spectrum has to be either simulated (see also \code{vignette("functionality")})
 or smoothed by the \code{\link{smooth_EPR_Spec_by_npreg}}, prior to integration. Afterwards,
 integrals are evaluated from the simulated or smoothed EPR spectra.
 For the purpose of quantitative analysis, the integrals are evaluated using the \code{B.units = "G"}
 (see Arguments). Hence, depending on \eqn{B}-unit (\code{G} or \code{mT} or \code{T}) each resulting integral
 column have to be optionally (in case of \code{mT} or \code{T}) multiplied by the factor of \code{10} or \code{10000},
 respectively, because \eqn{1\,\text{mT}\equiv 10\,\text{G}} and \eqn{1\,\text{T}\equiv 10^4\,\text{G}}.
 Such corrections are already included in the function/script.
 Instead of "double integral/integ." the term "sigmoid integral/integ." is used. "Double integral"
 \strong{in the case of originally single integrated EPR spectrum} (see \code{data.spectr}
 and \code{Intensity}) \strong{is confusing. In such case, the EPR spectrum is integrated just once.}
}
\examples{
## loading the built-in package example
## time series EPR spectra:
triarylamine.decay.series.dsc.path <-
load_data_example(file =
        "Triarylamine_radCat_decay_series.DSC")
triarylamine.decay.series.asc.path <-
load_data_example(file =
        "Triarylamine_radCat_decay_series.zip")
unzip(triarylamine.decay.series.asc.path,
      exdir = tempdir()
      )
## loading the kinetics:
triarylamine.decay.series.data <-
  readEPR_Exp_Specs_kin(name.root =
    "Triarylamine_radCat_decay_series",
    dir_ASC = tempdir(),
    dir_dsc_par =
      system.file("extdata",
                  package = "eprscope")
   )
#
## select the first spectrum
triarylamine.decay.series.data1st <-
   triarylamine.decay.series.data$df \%>\%
     dplyr::filter(time_s ==
       triarylamine.decay.series.data$time[1])
#
## integrate the first spectrum with default arguments
triarylamine.decay.data1st.integ01 <-
  eval_integ_EPR_Spec(triarylamine.decay.series.data1st)
#
## data frame preview
head(triarylamine.decay.data1st.integ01)
#
## integration (including baseline correction)
## of the 1st spectrum from the series
triarylamine.decay.data1st.integ02 <-
  eval_integ_EPR_Spec(triarylamine.decay.series.data1st,
    ## limits obtained from interactive spectrum:
    BpeaKlim = c(3471.5,3512.5),
    Blim = c(3425,3550),
    correct.integ = TRUE,
    poly.degree = 3,
    sigmoid.integ = TRUE
    )
#
## data frame preview
head(triarylamine.decay.data1st.integ02)
#
## plot the single integrated EPR spectrum,
## including baseline correction
plot_EPR_Specs(triarylamine.decay.data1st.integ02,
               x = "B_G",
               x.unit = "G",
               Intensity = "single_Integ_correct",
               lineSpecs.form = "integrated"
             )
#
## plot, corresponding to double/sigmoid integral,
## which is related to corrected single integral
plot_EPR_Specs(triarylamine.decay.data1st.integ02,
               x = "B_G",
               x.unit = "G",
               Intensity = "sigmoid_Integ",
               lineSpecs.form = "integrated"
             )
#
## vectorized output of the uncorrected `sigmoid_integral`
triarylamine.decay.data1st.integ03 <-
  eval_integ_EPR_Spec(triarylamine.decay.series.data1st,
                      sigmoid.integ = TRUE,
                      output.vecs = TRUE)[["sigmoid"]]
#
## preview of the first 6 values
triarylamine.decay.data1st.integ03[1:6]
#
## incorporation of vectorized integration into
## data "pipe" ("\%>\%") `dplyr` processing of EPR spectral
## time series, creating column with `sigmoid` integral
## where its corresponding single integral (intensity)
## has undergone a baseline correction, finally the max. value
## of all sigmoid integrals along with the time is
## summarized in data frame for quantitative kinetic analysis
triarylamine.decay.data.integs <-
  triarylamine.decay.series.data$df \%>\%
  dplyr::group_by(time_s) \%>\%
  dplyr::filter(dplyr::between(B_G,3425,3550)) \%>\%
  dplyr::mutate(sigmoid_Integ =
    eval_integ_EPR_Spec(dplyr::pick(B_G,dIepr_over_dB),
                        correct.integ = TRUE,
                        BpeaKlim = c(3471.5,3512.5),
                        poly.degree = 3,
                        sigmoid.integ = TRUE,
                        output.vecs = TRUE)$sigmoid
                       ) \%>\%
  dplyr::summarize(Area = max(sigmoid_Integ))
## in such case `Blim` range is not defined by `eval_integ_EPR_Spec`,
## the `Blim = NULL` and `dplyr::between()` must be set !!!
#
## preview of the final data frame
head(triarylamine.decay.data.integs)
#
## preview of the simple plot
ggplot2::ggplot(triarylamine.decay.data.integs) +
  ggplot2::geom_point(ggplot2::aes(x = time_s,y = Area))
#
## this does not correspond to example
## in `eval_kinR_EPR_modelFit`, `eval_kin_EPR_ODE_model`
## or in `plot_theme_NoY_ticks` based on the same input data,
## as those `Area` vs `time` relations were evaluated using
## the simulated EPR spectra (see also `vignette("datasets")`)
#
\dontrun{
## Similar to previous data processing, creating both: corrected
## single integral + sigmoid integral for each time within the spectral
## series. Sigmoid integral was evalutated from the single one by
## `cumtrapz()` function from `pracma` package and finally re-scaled.
triarylamine.decay.data.integs <-
  triarylamine.decay.series.data$df \%>\%
  dplyr::group_by(time_s) \%>\%
  eval_integ_EPR_Spec(correct.integ = TRUE,
                      Blim = c(3425,3550),
                      BpeaKlim = c(3472.417,3505.5),
                      poly.degree = 3) \%>\%
 dplyr::group_by(time_s) \%>\%
 dplyr::mutate(sigmoid_Integ =
   pracma::cumtrapz(B_G,single_Integ_correct)[,1]) \%>\%
 dplyr::mutate(sigmoid_Integ_correct =
   abs(min(sigmoid_Integ) - sigmoid_Integ))
}


}
\references{
Weber RT (2011). \emph{Xenon User's Guide}. Bruker BioSpin Manual Version 1.3, Software Version 1.1b50.

 Hans W. Borchers (2023). \emph{pracma: Practical Numerical Math Functions}. R package version 2.4.4,
 \url{https://cran.r-project.org/web/packages/pracma/index.html}.

 Seeburger P (2023). “Numerical Integration - Midpoint, Trapezoid, Simpson's rule”,
 \url{http://tinyurl.com/trapz-integral}.

 Svirin A (2023). “Calculus, Integration of Functions-Trapezoidal Rule”,
 \url{https://math24.net/trapezoidal-rule.html}.

 Epperson JF (2013). \emph{An Introduction to Numerical Methods and Analysis}. Wiley and Sons,
 ISBN 978-1-118-36759-9, \url{https://books.google.cz/books?id=310lAgAAQBAJ}.
}
\seealso{
Other Evaluations and Quantification: 
\code{\link{eval_kinR_EPR_modelFit}()},
\code{\link{eval_kinR_ODE_model}()},
\code{\link{quantify_EPR_Abs}()},
\code{\link{quantify_EPR_Norm_const}()}
}
\concept{Evaluations and Quantification}
