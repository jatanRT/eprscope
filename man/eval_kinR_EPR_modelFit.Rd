% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval_kinR_EPR_modelFit.R
\name{eval_kinR_EPR_modelFit}
\alias{eval_kinR_EPR_modelFit}
\title{Radical Kinetic Models Fitted onto Experimental Data}
\usage{
eval_kinR_EPR_modelFit(
  data.qt.expr,
  time.unit = "s",
  time = "time_s",
  qvarR = "Area",
  model.react = "(r=1)R --> [k1] B",
  elementary.react = TRUE,
  params.guess = c(qvar0R = 0.001, k1 = 0.001),
  params.guess.lower = NULL,
  params.guess.upper = NULL,
  fit.kin.method = "diff-levenmarq",
  solve.ode.method = "lsoda",
  time.frame.model = 2,
  time.correct = FALSE,
  path_to_dsc_par = NULL,
  origin = NULL,
  ...
)
}
\arguments{
\item{data.qt.expr}{A data frame object, containing the concentrations/integral intensities/areas under
the EPR spectra calculated using the experimental data as well as time column. These two essential
columns are described by character strings like those below (see arguments \code{time} and \code{qvarR}).}

\item{time.unit}{Character string, corresponding to time unit like \code{"s"} (\strong{default}),
\code{"min"} or \code{"h"}.}

\item{time}{Character string, pointing to \code{time} column/variable name in the original
\code{data.qt.expr} data frame. \strong{Default}: \code{time = "time_s"}.}

\item{qvarR}{Character string, pointing to \code{qvarR} (quantitative variable related to radical) column/variable
name in the original \code{data.qt.expr}. \strong{Default}: \code{qvarR = "Area"}.}

\item{model.react}{Character string, denoting a specific radical (\code{"R"}) reaction related to
changes in integral intensities (or any other \strong{q}uantitative \strong{var}iable) in EPR spectral
time series. Arrow shows direction of the reaction (\code{"-->", forward} or \code{"<==>", forward + reverse}).
Rate constants are indicated by square brackets after the arrows. Following examples of the reaction schemes
are predefined and commonly used to describe the integral intensity and/or radical concentration/amount
changes during the EPR time series experiment (the \code{r,a,b} stoichiometric coefficients
may vary, see below).
\tabular{ll}{
\strong{Reaction Scheme} \tab \strong{model.react} \cr
\eqn{(r=1)\text{R} \xrightarrow{k_1} \text{B}} \tab \code{"(r=1)R --> [k1] B"} \cr
 \eqn{(a=2)\text{A} \xrightarrow{k_1} (r=2)\text{R}} \tab \code{"(a=2)A --> [k1] (r=2)R"} \cr
 \eqn{(a=2)\text{A} \xrightleftharpoons[k_4]{k_1} (r=2)\text{R}
 \xrightleftharpoons[k_3]{k_2} (b=1)\text{B}} \tab
 \code{"(a=2)A <==> [k1] [k4] (r=2)R <==> [k2] [k3] (b=1)B"} \cr
 \eqn{(r=1)\text{R} \xrightleftharpoons[k_2]{k_1} (b=1)\text{B}} \tab
 \code{"(r=1)R <==> [k1] [k2] (b=1)B"} \cr
 \eqn{(a=2)\text{A} \xrightleftharpoons[k_2]{k_1} (r=2)\text{R}} \tab
 \code{"(a=2)A <==> [k1] [k2] (r=2)R"} \cr
 \eqn{(a=1)\text{A} + (b=1)\text{B} \xrightarrow{k_1} (r=1)\text{R}} \tab
 \code{"(a=1)A + (b=1)B --> [k1] (r=1)R"} \cr
 \eqn{(a=1)\text{A} + (r=1)\text{R} \xrightarrow{k_1} \text{B}} \tab
 \code{"(a=1)A + (r=1)R --> [k1] B"} \cr
}
Couple of examples are also given in \code{Details}. The function is relatively flexible and enables
later addition of any other reaction schemes describing the EPR time series experiments
(YOU MAY ASK DEVELOPER(S) via forum/help-channels). The stoichiometric coefficient (such as \code{(r=1)}
or \code{(a=1)}) can be varied within the \code{model.react} character string.
Defined/Allowed values are integers, e.g. 1,2,3...etc. The space character within the \code{model.react}
string is not fixed and can be skipped for the sake of simplicity.
If \code{elementary.react = FALSE} (the model reaction is not considered as an elementary one),
a possible non-integer partial coefficients (e.g. \code{alpha},\code{beta} or \code{gamma})
must be included in \code{kin.params} (see also \code{kin.params} description). For the consecutive model reaction
presented above, it applies only to one part/step of the mechanism.}

\item{elementary.react}{Logical, if the model reaction should be considered as elementary one,
i.e. the stoichiometric coefficients equal to the partial reaction orders. Such reaction proceeds without
identifiable intermediate species forming. \strong{Default}: \code{elementary.react = TRUE}.
If \code{elementary.react = FALSE}, i.e. the \code{model.react} cannot be considered like an elementary one,
one must include the parameterized reaction orders \eqn{\alpha}, \eqn{\beta} or \eqn{\gamma} in
the \code{kin.params}, e.g \code{kin.params = c(k1 = 0.01, qvar0A = 0.05, alpha = 1.5)}. For the consecutive
model reaction presented above, it applies only to one part/step of the mechanism.}

\item{params.guess}{Named vector, initial values of \code{kin.params} (see \code{\link{eval_kinR_ODE_model}})
ready for optimization/fitting. The \code{k1}-unit is eventually expressed
in terms of \eqn{s^{-1}} as well as in units of the applied \code{qvar}
(e.g. \code{c}, concentration) and depends on the partial reaction order(s), which power(s) the \code{qvar(s)}.
For example, the \code{k1}-unit of elementary radical recombination, evaluated by double integrals,
like \code{model.react = "(r=2)R --> [k1] B"}, reads: \eqn{\text{s}^{-1}\,(\text{p.d.u.})^{-1}}.}

\item{params.guess.lower}{Numeric vector of lower bounds on each parameter in \code{params.guess}.
If not given, the \strong{default} (\code{params.guess.lower = NULL}) lower bound
corresponds to \code{-Inf} of each \code{params.guess} component.}

\item{params.guess.upper}{Numeric vector of upper bounds on each parameter in \code{params.guess}.
If not given, the \strong{default} (\code{params.guess.upper = NULL}) upper bound
corresponds to \code{+Inf} of each \code{params.guess} component.}

\item{fit.kin.method}{Character string, pointing to optimization/fitting method. So far,
the default one (\code{fit.kin.method = "diff-levenmarq"}) is exclusively used (additional methods
are planned). It corresponds to differential Levenberg-Marquardt (see also \code{\link[minpack.lm]{nls.lm}})
because it is based on the numeric solution of the ordinary differential equations
and not on the common integration of rate equations.}

\item{solve.ode.method}{Character string, setting up the integrator (the \code{method} argument in \code{\link[deSolve]{ode}}),
applied to find the numeric solution of ODE. \strong{Default}: \code{solve.ode.method = "lsoda"}
(\code{\link[deSolve]{lsoda}}, additional methods, see the \code{ode} link above).}

\item{time.frame.model}{Numeric value, corresponding to interval time resolution, i.e. the smallest time difference
between two consecutive points. The number of points is thus defined by the \code{time.interval.model} argument:
\deqn{((Interval[2] - Interval[1])\,/\,Frame) + 1}
This argument is required to numerically solve the kinetic differential equations by the \code{\link[deSolve]{ode}}.
For the default interval mentioned above, the \strong{default} value reads \code{time.frame.model = 2} (in seconds).}

\item{time.correct}{Logical, if time of recorded series of the EPR spectra needs to be corrected.
\strong{Default}: \code{time.correc = FALSE}, which actually assumes that time correction was done
(either by \code{\link{correct_time_Exp_Specs}} or by \code{\link{readEPR_Exp_Specs_kin}} with
a subsequent integration), prior to fitting procedure. If \code{time.correct = TRUE},
the \code{path} to file with EPR instrumental parameters (like \code{.DSC}/\code{.dsc} or \code{par})
must be defined (see the \code{path_to_dsc_par}).}

\item{path_to_dsc_par}{Character string, path (also provided by the \code{\link[base]{file.path}})
to \code{.DSC/.dsc} or \code{.par} (depending on \code{origin} parameter)
\code{text} files including instrumental parameters and provided by the EPR machine.
\strong{Default}: \code{path_to_dsc_par = NULL}.}

\item{origin}{Character string, corresponding to software which was used to acquire the EPR spectra,
essential to load the parameters by the \code{path_to_dsc_par} (see also the \code{\link{readEPR_params_slct_kin}}).
Two origins are available: \code{origin = "winepr"} or \code{origin = "xenon"}.}

\item{...}{additional arguments for \code{\link[minpack.lm]{nls.lm}}, e.g. defined
by the \code{control = minpack.lm::nls.lm.control()}.}
}
\value{
List with the following components is available:
  \describe{
  \item{df}{Data frame object with the variables/columns such as \code{time},
  experimental quantitative variable like \code{sigmoid_Integ} (sigmoid integral) or \code{Area},
  concentration \code{c_M} or number of radicals of the relevant EPR spectrum; the corresponding
  quantitative variable \code{fitted} vector values as well as residual vector (experiment - kinetic model)
  related to the \code{qvarR} argument.}
  \item{plot}{Plot object \emph{Quantitative variable} \emph{vs} \emph{Time} with the experimental
  data and the corresponding fit.}
  \item{ra}{Simple residual analysis - a list consisting of 4 elements: diagnostic plots
  \code{plot.rqq}, \code{plot.histDens}; original data frame (\code{df}) with residuals and their corresponding
  standard deviation (\code{sd}). For details, please refer to the \code{\link{plot_eval_RA_forFit}}.}
  \item{df.coeffs}{Data frame object containing the optimized (best fit) parameter values (\code{Estimates}),
  their corresponding \code{standard errors}, \code{t-} as well as \code{p-values}.}
  \item{cov.coeffs}{Covariance \code{matrix}, consisting of fitted/optimized kinetic parameters/coefficients
  (see also \code{df.coeffs} above). The corresponding variances (diagonal elements) should be small,
  indicating that the estimates possess a lower uncertainties.
  The off-diagonal elements show how the two coefficient estimates change together. For a decent model they should be
  as close to \code{0} as possible. Large values indicate
  \href{https://www.geeksforgeeks.org/machine-learning/how-to-test-for-multicollinearity-in-r/}{multicollinearity}
  with positive sign suggesting the coefficient are overestimated, and with a negative one, indicating that one coefficient
  is overestimated, while the other one is underestimated.}
  \item{cor.coeffs}{Correlation \code{matrix} of fitted/optimized kinetic parameters/coefficients
  (see also \code{df.coeffs} above). Such matrix can be additionally nicely visualized
  by a correlation \code{plot} created by the \code{\link[corrplot]{corrplot}} function.
  The off-diagonal elements should be as small as possible
  (ideally close to \code{0}) in order to exclude the multicollinearity (see the \code{cov.coeffs}) and trust the optimized
  kinetic parameters.}
  \item{N.evals}{Total number of evaluations/iterations before the best fit is found.}
  \item{min.rss}{Minimum sum of residual squares after \code{N.evals}.}
  \item{abic}{A list consisting of Akaike and Bayesian information criteria (AIC & BIC) vector (\code{abic.vec})
  and \code{message}, denoting the probability distribution of residuals/errors, applied to evaluate
  those criteria. To be used when comparing different kinetic models. The lower the (negative) values,
  the better the fit. Please, refer to the \code{\link{eval_ABIC_forFit}}.}
  \item{cov.df}{Covariance \code{matrix} of a data frame, consisting of \code{qvarR} (e.g. double integral/Area - experiment),
  \code{fitted} (kinetic model fit) and the corresponding residuals as columns/variables. Covariance between
  the experiment and kinetic model should be positive and strong for a decent fit. Contrary, the \code{cov} between
  the kinetic model fit and residuals should be ideally close to \code{0}, indicating no systematic relationship.
  However, the covariance is scale-depended and must be "normalized". Therefore, for such a purpose, the correlation
  is defined as shown below.}
  \item{cor.df}{Correlation \code{matrix} of a data frame, consisting of \code{qvarR} (e.g. double integral/Area - experiment),
  \code{fitted} (kinetic model fit) and the corresponding residuals as columns/variables.
  Such matrix can be additionally nicely visualized by a correlation \code{plot} created
  by the \code{\link[corrplot]{corrplot}} function. A higher positive correlation
  (between the integrals and the kinetic model fit), with the value close to \code{1}, indicates that the kinetic model
  fit nicely follows the integral(s) \emph{vs} time relation. Contrary, no clear correlation between the residuals
  and the experiment and/or kinetic model must be visible. Therefore, such correlation should be ideally close to \code{0}.}
  \item{N.converg}{Vector, corresponding to residual sum of squares at each iteration/evaluation.}
  }
}
\description{
Fitting of the integrals/areas/concentration/...etc. \emph{vs} time relation (either from experiment
   or from integration of the EPR spectral time series) in order to find the kinetic parameters
   (like rate constant, \eqn{k} as well as (partial) reaction order(s)) of proposed radical reaction.
   Reaction model is taken from the \code{\link{eval_kinR_ODE_model}}, while the optimization/fitting
   is provided by the differential Levenberg-Marquardt optimization method, \code{\link[minpack.lm]{nls.lm}}.
   Because the radical concentration is directly proportional to the EPR spectrum (double)
   integral (see the \code{\link{quantify_EPR_Abs}}), for a quick evaluation and/or comparison of different
   kinetic data, it is possible to obtain the rate constants \eqn{k} by the integrals/areas \emph{vs} time fit.
   Therefore, the unit of \eqn{k} might be expressed in terms of \eqn{\text{s}^{-1}} as well as in units of integrals/areas,
   e.g. \code{procedure defined unit} (see \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6803776/}{p.d.u.}),
   depending on the order of reaction (see the \code{params.guess} argument).
}
\examples{
## loading example data (incl. `Area` and `time` variables)
## from Xenon: decay of a triarylamine radical cation
## after its generation by electrochemical oxidation
triaryl_radCat_path <-
  load_data_example(file = "Triarylamine_radCat_decay_a.txt")
## corresponding data (double integrated
## EPR spectrum = `Area` vs `time`)
triaryl_radCat_data <-
  readEPR_Exp_Specs(triaryl_radCat_path,
                    header = TRUE,
                    fill = TRUE,
                    select = c(3,7),
                    col.names = c("time_s","Area"),
                    x.unit = "s",
                    x.id = 1,
                    Intensity.id = 2,
                    qValue = 1700,
                    data.structure = "others") \%>\%
  na.omit()
## data preview
head(triaryl_radCat_data)
#
## loading the `.DSC` file
triaryl_radCat_dsc_path <-
  load_data_example(file = "Triarylamine_radCat_decay_a.DSC")
#
## fit previous data by second order kinetics,
## where the `model.react` is considered as an elementary
## step (`time.correct` of the CW-sweeps is included (`TRUE`))
triaryl_model_kin_fit_01 <-
  eval_kinR_EPR_modelFit(data.qt.expr = triaryl_radCat_data,
                         model.react = "(r=2)R --> [k1] B",
                         elementary.react = TRUE,
                         params.guess = c(qvar0R = 0.019,
                                          k1 = 0.04
                                         ),
                         time.correct = TRUE,
                         path_to_dsc_par = triaryl_radCat_dsc_path,
                         origin = "xenon")
## data frame preview
head(triaryl_model_kin_fit_01$df)
#
## plot preview
triaryl_model_kin_fit_01$plot
#
## coefficients/parameters table preview
triaryl_model_kin_fit_01$df.coeffs
#
## convergence preview
triaryl_model_kin_fit_01$N.converg
#
## simple residual analysis plots
## showing the random pattern, which indicates that
## kinetic model provides a decent fit to the data +
## normal quantile (Q-Q) plot, indicating that residuals
## are normally distributed; third plot demonstrates
## the probability density with the histogram of residuals
triaryl_model_kin_fit_01$ra$plot.rqq
triaryl_model_kin_fit_01$ra$plot.histDens
#
## standard deviation of residuals
triaryl_model_kin_fit_01$ra$sd
#
## Akaike and Bayesian Criteria (AIC & BIC)
## information about the residuals +
## + probability distribution
triaryl_model_kin_fit_01$abic
#
## take the same experimental data and perform fit
## by first order kinetics where the `model.react`
## is considered as an elementary step
## (`time.correct` of the CW-sweeps is included (`TRUE`))
triaryl_model_kin_fit_02 <-
  eval_kinR_EPR_modelFit(data.qt.expr = triaryl_radCat_data,
    model.react = "(r=1)R --> [k1] B",
    elementary.react = TRUE,
    params.guess = c(qvar0R = 0.019,
                     k1 = 0.0002
                     ),
    time.correct = TRUE,
    path_to_dsc_par = triaryl_radCat_dsc_path,
    origin = "xenon")
## plot preview
triaryl_model_kin_fit_02$plot
#
## coefficients/parameters table preview
triaryl_model_kin_fit_02$df.coeffs
#
## simple residual analysis, indicating
## the 1st order kinetics is less convenient
## model than that of the 2nd order (based on
## the decrease of EPR intensity/integral)
triaryl_model_kin_fit_02$ra$plot.rqq
#
## standard deviation of residuals
triaryl_model_kin_fit_02$ra$sd
#
## Akaike and Bayesian Criteria (AIC & BIC) +
## + information about the residuals
## probability distribution
triaryl_model_kin_fit_02$abic


}
\references{
Mullen KM, Elzhov TV, Spiess A, Bolker B (2023). “minpack.lm.” \url{https://github.com/cran/minpack.lm}.

 Gavin HP (2024). “The Levenberg-Marquardt algorithm for nonlinear least squares curve-fitting problems.”
 \emph{Department of civil and environmental engineering, Duke University},
 \url{https://people.duke.edu/~hpgavin/ce281/lm.pdf}.
}
\seealso{
Other Evaluations and Quantification: 
\code{\link{eval_integ_EPR_Spec}()},
\code{\link{eval_kinR_ODE_model}()},
\code{\link{quantify_EPR_Abs}()},
\code{\link{quantify_EPR_Norm_const}()}
}
\concept{Evaluations and Quantification}
