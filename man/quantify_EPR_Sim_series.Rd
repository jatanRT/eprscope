% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantify_EPR_Sim_series.R
\name{quantify_EPR_Sim_series}
\alias{quantify_EPR_Sim_series}
\title{Quantify (Components) Areas of Series of Simulated EPR Spectra}
\usage{
quantify_EPR_Sim_series(
  data.spectra.series,
  dir_ASC_sim,
  name.pattern.sim,
  sim.origin = "easyspin",
  var2nd.series = "time_s",
  B.unit = "G",
  Intensity.expr = "dIepr_over_dB",
  Intensity.sim = "dIeprSim_over_dB",
  optim.method = "sbplx",
  optim.params.init,
  optim.params.lower = NULL,
  optim.params.upper = NULL,
  Nmax.evals = 1024,
  tol.step = 5e-07,
  pswarm.size = NULL,
  pswarm.diameter = NULL,
  single.integ = "single_IntegSim",
  double.integ = "double_IntegSim",
  output.area.stat = TRUE,
  ...
)
}
\arguments{
\item{data.spectra.series}{Spectrum data frame/table object containing magnetic flux density
as \code{x} variable. They can be labeled as \code{Field}, \code{B_mT}
in mT (or \code{B_G} in gauss). The \code{y/Intensity} variable
can be labeled as \code{dIepr_over_dB}, in case of derivative intensity, or if
integrated spectral intensities are present, they can be labeled accordingly.
See also \code{Intensity.expr} parameter/argument. A second independent variable
\code{var2nd.series} column (e.g. \code{var2nd.series = "time_s"}) must be available. In such case,
the entire \code{data.spectra} must be present in the form of
\href{https://r4ds.had.co.nz/tidy-data.html}{tidy/long table format}
(see also parameter/argument \code{var2nd.series}). Such data frame can be created, e.g.
by the \code{\link{readEPR_Exp_Specs_kin}} or the \code{\link{readEPR_Exp_Specs_multif}} function.}

\item{dir_ASC_sim}{Character string, pointing to folder where the simulated EPR spectra of all
components are stored. Path can be alternatively specified by the \code{\link[base]{file.path}} function.}

\item{name.pattern.sim}{Character string pattern from file names related to simulated EPR spectral data
like \code{name.pattern.sim = "DHMB0_1st_04_SimA"}
or \code{name.pattern.sim = "DHMB0_1st_04_Sim[[:upper:]]"} (for the file names \code{..._SimA},
\code{..._SimB},...etc). It assumes, those file must have similar names and this pattern appears
at the beginning of the file name. One may also consult
how to \href{https://r4ds.hadley.nz/regexps}{use regular expressions in R}.}

\item{sim.origin}{Character string referring to "origin" of the simulated ASCII data.
There are four possibilities \eqn{\Rightarrow} \code{sim.orimgin = "easyspin"} (\strong{default}),
\code{"xenon"}, \code{"simfonia"} as well as universal \code{"csv"}.}

\item{var2nd.series}{Character string referred to name of the second independent variable/quantity
column in the original \code{data.spectra.series} (such as time, temperature, electrochemical potential,
Microwave Power) altered upon individual experiments as a second variable.
Data must be available in tidy/long table format.
\strong{Default}: \code{var2nd.series = "time_s"}.}

\item{B.unit}{Character string pointing to unit of magnetic flux density
like \code{"G"} (Gauss) or \code{"mT"} (millitesla), \strong{default}: \code{B.unit = "G"}.
THE UNIT MUST BE SHARED ACROSS ALL RELEVANT B-ARGUMENTS/DATAFRAMES.}

\item{Intensity.expr}{Character string pointing to column name of the experimental EPR intensity within
the original \code{data.spectra.series}. \strong{Default}: \code{dIepr_over_dB}.}

\item{Intensity.sim}{Character string pointing to column name of the simulated EPR intensity within the related
data frames (check the simulated spectral data for all components).
\strong{Default}: \code{Intensity.sim = "dIeprSim_over_dB"}.}

\item{optim.method}{Character string, pointing to applied optimization method/algorithm.
One may choose one from those listed in \code{\link{optim_for_EPR_fitness}}, \strong{default}:
\code{method = "sbplx"}, setting up
the \href{https://nlopt.readthedocs.io/en/latest/NLopt_Algorithms/#sbplx-based-on-subplex}{"Subplex" method}.}

\item{optim.params.init}{Numeric vector with the elements: baseline constant/intercept
followed by simulated intensity multiplication coefficient for each EPR spectral component.
Therefore, the length of this vector is equal to: number of components + 1.}

\item{optim.params.lower}{Numeric vector with the length of \code{optim.params.init} and the lower
bound constraints. \strong{Default}: \code{optim.params.init = NULL}, actually corresponding
to vector with all \code{0} value elements.}

\item{optim.params.upper}{Numeric vector with the length of \code{optim.params.init}) and the upper
bound constraints. \strong{Default}: \code{optim.params.init = NULL}, actually corresponding
to vector with all \code{0.9} value elements.}

\item{Nmax.evals}{Numeric value, maximum number of function evaluations and/or iterations.
The only one method, limited by this argument, is \code{\link[minpack.lm]{nls.lm}}, where
\code{Nmax.evals = 1024} (\strong{default}). Higher \code{Nmax.evals} may extend the optimization
time.}

\item{tol.step}{Numeric, the smallest optimization step (relative change) between
2 iterations to stop the optimization procedure. For the \code{optim.method = "pswarm"}
(particle swarm optimization procedure) it actually corresponds to tolerance for restarting.
Once the maximum distance between the "best" particle and all the others is less
than \code{tol.step} * \code{pswarm.diameter}) the algorithm restarts.
See also \code{\link[pso]{psoptim}}. \strong{Default}: \code{tol.step = 5e-7}.}

\item{pswarm.size}{Numeric value equal to particle swarm size (i.e. number of particles),
if \code{optim.method = "pswarm"}. The \strong{default} value (\code{pswarm.size = NULL}) actually
corresponds to \code{floor(10+2*sqrt(length(x.0)))}.}

\item{pswarm.diameter}{Numeric value corresponding to diameter of the particle swarm search space
(in case \code{optim.method = "pswarm"}). The \strong{default} value (\code{pswarm.diameter = NULL})
refers to the Euclidean distance, defined as:
\deqn{\sqrt{\sum_k\,(\text{optim.params.upper}[k] - \text{optim.params.lower}[k])^2}}}

\item{single.integ}{Character string, setting up the column/variable name related to single-integrated spectrum
within the output data frame, \strong{default}: \code{single.integ = "single_IntegSim"}.}

\item{double.integ}{Character string, setting up the column/variable name related to double-integrated spectrum
within the output data frame, \strong{default}: \code{double.integ = "single_IntegSim"}.
If \code{double.integ = NULL} only single integrals are calculated/returned (e.g. in the case of
single integrated spectral data).}

\item{output.area.stat}{Logical, whether to summarize all fitted EPR spectral components, in columns,
for each time/temperature/...etc. point in row. Additional optimization measures are presented as well
(see \code{Details}).\strong{Default}: \code{output.area.stat = TRUE}.}

\item{...}{additional arguments specified (see also \code{\link{optim_for_EPR_fitness}}).}
}
\value{
Function provides data frame object, depending on the \code{output.area.stat} argument,
  as listed below:
  \enumerate{
  \item If \code{output.area.stat = TRUE} (\strong{default}), the resulting data frame consists
  of columns/variables like integrals/areas for each simulated and fitted EPR spectrum, where
  the components are denoted by uppercase letters (\code{Area_Sim_A}, \code{Area_Sim_B},...etc.);
  best fitted/optimized coefficients to multiply the intensities (\code{Optim_CoeffInt_Sim_A},
  \code{Optim_CoeffInt_Sim_B},...etc); best fitted/optimized intercept (or baseline constant,
  \code{Optim_intercept}); minimum sum of the residual squares (\code{minLSQ_sum}); number
  of evaluations/iterations (\code{N_evals}) and finally convergence information/number (\code{N_converg},
  like already described in \code{\link{optim_for_EPR_fitness}}). These variables are presented for each
  \code{var2nd.series} (e.g. time) point like example for one EPR spectral component:
  \tabular{lcccccc}{
  \strong{time_s} \tab \strong{Area_Sim_A} \tab \strong{Optim_CoeffInt_Sim_A} \tab \strong{Optim_intercept} \tab
  \strong{minLSQ_sum} \tab \strong{N_evals} \tab \strong{N_converg} \cr
  6 \tab 0.020624473 \tab 0.052843937 \tab 5.508809e-10 \tab 2.289953e-07 \tab 198 \tab 4 \cr
  21 \tab 0.020217930\tab 0.051802287\tab 5.401823e-10 \tab 2.438172e-07 \tab 177 \tab 4 \cr
  36 \tab 0.018836579 \tab 0.048263010 \tab 5.029705e-10 \tab 2.662651e-07 \tab 201 \tab 4 \cr
  }

  \item Tidy/long table format of the original \code{data.spectra.series} with additional
  columns/variables (best fitted simulated intensities) for all spectral components: A, B, C, ...etc.
  }
}
\description{
Evaluating the linear combination of spectral intensities of components (loaded as ASCII text files corresponding
  to simulated spectra). The related intensity multiplication coefficients
  (please, refer to the \code{optim.params.init} argument) are optimized by methods gathered in the
  \code{\link{optim_for_EPR_fitness}}. The goal is to fit the sum of the simulated components
  onto each experimental spectrum within the series. So far, the maximum number of components is set to 6.
}
\details{
Analyzed EPR spectra may consists of several components (see also the \code{\link{eval_sim_EPR_iso_combo}}
  function), like the overlapped EPR spectra of several radicals. In order to follow the concentration/amount
  variations of each individual radical during the kinetic/temperature/...series, one must figure out
  how those individual spectral components are actually changed. In the first approximation, it means
  to follow the corresponding EPR intensities/integrals, whereas the component positions (\eqn{g}-values)
  are assumed to be fixed (or those changes can be neglected). Therefore, the actual function takes the linear
  combination of the spectral intensities of components (simulated spectra) and optimizes
  the related multiplication coefficients. Additional analysis, where the positions of spectral
  components (simulated spectra) are not fixed and can be optimized as well is under development.
  That will be based on the combination of the actual function with the \code{\link{eval_sim_EPR_iso_combo}}.
}
\examples{
\dontrun{
## example with default arguments corresponding
## to one simulated spectral component,
## `optim.params.init` has the length
## of => number of components + 1,
## because of included intercept/baseline constant
quant.data.sim.test.a <-
  quantify_EPR_Sim_series(data.spectra.series,
     dir_ASC_sim = "./",
     optim.method = "pswarm",
     name.pattern.sim = "DHMB0_1st_04_SimA",
     optim.params.init = c(0,0.8),
     output.area.stat = TRUE)
#
## similar example with two components
## (simulated spectra) and tidy data frame
## output (not the summarized one)
quant.data.sim.test.b <-
  quantify_EPR_Sim_series(data.spectra.series,
     dir_ASC_sim = "./",
     optim.method = "sbplx",
     name.pattern.sim = "DHMB0_1st_04_Sim[[:upper:]]",
     optim.params.init = c(0,0.8,0.2),
     output.area.stat = FALSE)
#
}


}
\seealso{
Other Simulations and Optimization: 
\code{\link{eval_sim_EPR_iso}()},
\code{\link{eval_sim_EPR_isoFit}()},
\code{\link{eval_sim_EPR_iso_combo}()},
\code{\link{optim_for_EPR_fitness}()},
\code{\link{smooth_EPR_Spec_by_npreg}()}
}
\concept{Simulations and Optimization}
