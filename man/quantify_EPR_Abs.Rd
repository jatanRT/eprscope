% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantify_EPR_Abs.R
\name{quantify_EPR_Abs}
\alias{quantify_EPR_Abs}
\title{Absolute Quantification of Radicals/Spins}
\usage{
quantify_EPR_Abs(
  integ.sigmoid.max,
  instrum.params = NULL,
  path_to_dsc_par,
  origin = "xenon",
  qValue = NULL,
  tube.sample.id.mm,
  point.sample.factor = 8.51e-09,
  fill.sample.h.mm,
  eff.cavity.h.mm = 23,
  fn.B1.Bm.fit.y = c(1.00179, -0.00307086, -0.0265409, 0.000297603, 0.000223277,
    -4.53833e-06, -4.1451e-07, 1.89417e-08, -1.48241e-09),
  fn.B1.Bm.fit.y.max = 0.28,
  Norm.const = NULL,
  Temp.K = NULL,
  S = 0.5
)
}
\arguments{
\item{integ.sigmoid.max}{Numeric value or vector of the entire EPR spectrum sigmoid integral.}

\item{instrum.params}{Named numeric vector, containing instrumental parameters required
for the quantification =>
\tabular{ll}{
\code{PmW} \tab power of the MW source in mW \cr
\code{BmmT} \tab modulation amplitude (magnetic flux density modulation,
\eqn{B_{\text{m}}}) in mT \cr
\code{TK} \tab temperature in K \cr
\code{mwGHz} \tab applied microwave frequency in \code{GHz} to record the continuous wave (CW)
EPR spectrum \cr
}
\strong{Default}: \code{instrum.params = NULL} because they are primarily extracted
from the \code{path_to_dsc_par} based on the \code{origin}.}

\item{path_to_dsc_par}{Character string, path (can be also acquired by the \code{\link[base]{file.path}})
to \code{.DSC/.dsc} or \code{.par} (depending on the OS, see the \code{origin} argument)
\code{text} files including all instrumental parameters from the EPR machine.
If the \code{instrum.params} is already defined, the \code{path_to_dsc_par = NULL}. Otherwise,
BOTH the \code{path_to_dsc_par} AS WELL AS the \code{origin} MUST BE SPECIFIED !}

\item{origin}{Character string, corresponding to software which was used to obtain the EPR spectra
on spectrometers, because the files are slightly different, whether they
were recorded by the "WinEpr" (\code{origin = "winepr"}) or by the "Xenon" (\strong{default}).
If \code{origin = NULL} as well as \code{path_to_dsc_par = NULL}, the \code{instrum.params} have to be set.}

\item{qValue}{Numeric value of the sensitivity \code{Q} factor. For the processed EPR spectra by
the \code{{eprscope}} package the \code{integ.sigmoid.max} is usually normalized by the \code{Q} value.
Therefore, \strong{default}: \code{qValue = NULL} (corresponding to \code{1}).}

\item{tube.sample.id.mm}{Numeric value, which equals to internal diameter (in \code{mm}) of the tube/cell used
for the quantitative EPR experiment.}

\item{point.sample.factor}{Numeric value, corresponding to point sample correction factor, provided by the
cavity/probehead manufacturer. Value for the standard Bruker rectangular cavity is set as \strong{default}.}

\item{fill.sample.h.mm}{Numeric value, referring to sample height (in \code{mm}) within the tube/cell.}

\item{eff.cavity.h.mm}{Numeric value, which equals to effective cavity/probehead height/length,
usually provided by the probehead manufacturer.}

\item{fn.B1.Bm.fit.y}{Numeric vector (coefficients) of the polynomial degree from 5 to 12.
Coefficients for the standard Bruker rectangular cavity are set as \strong{default}.}

\item{fn.B1.Bm.fit.y.max}{Numeric value, corresponding to maximum value of the polynomial fit.
Value for the standard Bruker rectangular cavity is set as \strong{default}.}

\item{Norm.const}{Numeric value, corresponding to normalization constant (see
\code{\link{quantify_EPR_Norm_const}}). \strong{Default}: \code{Norm.const = NULL} in case
if the EPR spectrum was normalized by such constant either during the measurement or processing.
Otherwise it must be provided by the \code{\link{quantify_EPR_Norm_const}}.}

\item{Temp.K}{Numeric value, temperature value in \code{K}. Because the \code{instrum.params} also contains temperature
input one may choose which definition (\code{Temp.K} or \code{TK}) is taken for the calculation.
Either \code{Temp.K} or \code{TK} CAN BE ALSO \code{NULL} but NOT BOTH !! In the latter case, \strong{default value}
\code{298 K} is considered.}

\item{S}{Numeric value, total spin sample quantum number. For radicals \code{S = 0.5}
(\strong{default}).}
}
\value{
List of the following quantities:
  \describe{
  \item{N.cm}{Number of spins per effective centimeter. It is defined
    as the cm around the maximum, \eqn{\pm 5\,\text{mm}}, of the intensity
    distribution curve/polynomial fit within the cavity \eqn{f(B_1,B_{\text{m}})} from
    the equation shown in \code{Details}.}
   \item{N.cm3}{Number of spins per \eqn{\text{cm}^3}.}
   \item{c.M}{Concentration of spins/radicals in \eqn{\text{mol}\,\text{dm}^{-3}}.}
  }
}
\description{
Estimate the number (or concentration) of "spins"/radicals/paramagnetic species using the absolute
  quantitative method by sigmoid integral as well as by the instrumental parameters without the need
  for a standard sample with known concentration/amount of radicals/"spins". \strong{The calculation assumes
  that the sample height middle point, within an EPR tube, matches the cavity/resonator center}.
}
\details{
There are two approaches how to quantify the number of paramagnetic species/radicals/spins.
  The \strong{relative} one needs a standard sample with a known spin number and can be evaluated
  by the sigmoid integral ratio of the sample under study and that of the standard.
  While the \strong{absolute} method do not need the reference sample, it requires
  a precise cavity signal calibration as well as standardized cell geometry. Both are provided
  by the EPR instrument and lab-glass manufacturers (see e.g. Hirschmann-Laborgeräte (2023), \code{References}).
  In case of absolute quantitative EPR analysis the sigmoid integral (its maximum value),
  \eqn{I_{\text{sigmoid}}},can be used to calculate the number of "spins"/radicals/paramagnetic species,
  \eqn{N_{\text{Spins}}} (see also \code{References}) =>
  \deqn{N_{\text{Spins}} = I_{\text{sigmoid}}\,/\,[(c/f(B_1,B_{\text{m}}))\,(G_{\text{R}}\,t_{\text{C}}
  \,N_{\text{Scans}})\,(\sqrt{P_{\text{MW}}}\,B_{\text{m}}\,Q\,n_{\text{B}}\,S(S+1))]}
  where the quantity notations possess the following meaning (parentheses denote whether it is an instrumental
  or sample dependent parameter):
  \tabular{ll}{
  \strong{Quantity Symbol} \tab \strong{Meaning/Short Desription} \cr
  \eqn{c}\tab Point sample calibration factor (instrumental). \cr
  \eqn{f(B_1,B_\text{m})} \tab Spatial distribution of the microwave \eqn{B_1} and modulation
  amplitude within the cavity/probehead/resonator (instrumental). \cr
  \eqn{G_{\text{R}}} \tab Receiver gain (commonly in \eqn{\text{dB}} units (instrumental)). \cr
  \eqn{t_{\text{C}}} \tab Conversion time (commonly in \eqn{\text{ms}}) which is an analogy
  to integration time in other spectroscopies (instrumental). \cr
  \eqn{N_{\text{Scans}}} \tab Number of scans/accumulations during the experiment (instrumental). \cr
  \eqn{P_{\text{MW}}} \tab Microwave power (instrumental). \cr
  \eqn{B_{\text{m}}} \tab Modulation amplitude (instrumental). \cr
  \eqn{Q} \tab \emph{Q}-Value or \emph{Q}-Factor characterizing the resonator/cavity/probehead
  sensitivity (unitless and instrumental). \cr
  \eqn{n_{\text{B}}} \tab Boltzmann factor for temperature dependence (instrumental-sample). \cr
  \eqn{S} \tab Total electronic spin quantum number (sample). Commonly, for radicals \eqn{S = 1/2}. \cr
  }
  Almost all summarized quantities are instrument-dependent. Most of them correspond to the essential
  parameters for the experiment and can be easily acquired from the \code{.DSC}/\code{.dsc}/\code{.par} file(s).
  The Boltzmann factor describes the population of spin states
  by \eqn{\exp{(\Delta \varepsilon)\,/\,(k_{\text{B}}\,T)}}, where \eqn{\Delta \varepsilon} denotes
  the energy difference between the basic spin states, \eqn{k_{\text{B}}} is the Boltzmann constant
  (available from \code{\link[constants]{syms}}) and the \eqn{T} represents the temperature in \eqn{\text{K}}.
  For temperatures \eqn{\geq 4\,\text{K}} and continuous wave experiments where
  the \eqn{\Delta \varepsilon = h\,\nu_{\text{MW}}^{}} is constant, this factor may be very well estimated
  by the following formula:
  \deqn{n_{\text{B}} = h\,\nu_{\text{MW}}^{}\,/\,(2\,k_{\text{B}}\,T)}
  The term \eqn{(G_{\text{R}}\,t_{\text{C}}\,N_{\text{Scans}})} actually corresponds to normalization constant
  which is available from \code{\link{quantify_EPR_Norm_const}}.
  Besides the above-described parameters which can be easily estimated, there are however characteristics
  that requires precise calibration and usually are provided by the spectrometer manufacturers.
  The spatial distribution of the microwave, \eqn{B_1}, and modulation
  amplitude, \eqn{B_\text{m}}, influences the intensity of the sample predominantly along the \eqn{y}-axis direction
  (i.e. when moving the sample tube up or down within the cavity). Such intensity distribution,
  depending on the cavity/probehead for different sample length and positions, can be approximated by a polynomial
  (see the \code{fn.B1.Bm.fit.y} argument) that is supplied by the manufacturer as well (the coefficients
  of the polynomial can be sometimes found in \code{.DSC}/\code{.dsc}/\code{.par} file(s)). For quantitative
  purposes, such polynomial is integrated over the length of the sample.
}
\examples{
\dontrun{
## quantitative analysis (determining the
## radical concentration `c.M`) of a sample measured
## at different temperatures
## all data summarized in `data.tidy.integ`
data.quant <- mapply(function(x,y)
  {quantify_EPR_Abs(x,
    instrum.params = c(PmW = 2.518,
                       BmmT = 5.4e-02,
                       TK = NULL, ## 298 K
                       mwGHz = 9.530265),
    path_to_dsc_par = NULL,
    origin = NULL,
    tube.sample.id.mm = 2.86,
    fill.sample.h.mm = 23,
    Temp.K = y)$c.M
    },
  data.tidy.integ$Area,
  data.tidy.integ$Temperature_K
  )
}


}
\references{
Eaton GR, Eaton SS, Barr DP, Weber RT (2010). \emph{Quantitative EPR}. Springer Science and Business Media.
  ISBN 978-3-211-92947-6, \url{https://link.springer.com/book/10.1007/978-3-211-92948-3}.

  Weber RT (2011). \emph{Xenon User's Guide}. Bruker BioSpin Manual Version 1.3, Software Version 1.1b50.

  Hirschmann-Laborgeräte (2023). “Ringcaps.” \url{https://hirschmannlab.de/en/produkt/ringcaps/}.

  Mazúr M, Valko M, Morris H (2000). “Analysis of the Radial and Longitudinal Effect in a Double TE104
  and a Single TE102 Rectangular Cavity.” \emph{J. Magn. Reson.}, \strong{142}(1), 37–56. ISSN 1090-7807,
  \url{https://doi.org/10.1006/jmre.1999.1915}.

  Portis AM (1953). “Electronic Structure ofF Centers: Saturation of the Electron Spin Resonance.”
  \emph{Phys. Rev.}, \strong{91}(5), 1071–1078, \url{https://doi.org/10.1103/PhysRev.91.1071}.

  Mailer C, Sarna T, Swartz HM, Hyde JS (1977). “Quantitative Studies of Free Radicals in Biology:
  Corrections to ESR Saturation Data.” \emph{J. Magn. Reson.} (1969), \strong{25}(1), 205–210, ISSN 0022-2364,
  \url{https://doi.org/10.1016/0022-2364(77)90133-0}.
}
\seealso{
Other Evaluations and Quantification: 
\code{\link{eval_integ_EPR_Spec}()},
\code{\link{eval_kinR_EPR_modelFit}()},
\code{\link{eval_kinR_ODE_model}()},
\code{\link{quantify_EPR_Norm_const}()}
}
\concept{Evaluations and Quantification}
