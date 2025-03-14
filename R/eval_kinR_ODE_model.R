#
#' Quantitative EPR Kinetic Model Profiles by Numeric Solution of the ODE.
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'   Theoretical quantitative kinetic profiles (such as concentration/amount/integral intensity)
#'   as well as comparison with the experimental data for various predefined model reactions involving
#'   radical(s) (labeled as "R"). Profiles are evaluated by the numeric solution of rate equations
#'   by the \strong{O}rdinary \strong{D}ifferential \strong{E}quations (ODE from
#'   \href{https://desolve.r-forge.r-project.org/index.html}{desolve R package}).
#'   This function is inspired by the
#'   \href{https://www.r-bloggers.com/2013/06/learning-r-parameter-fitting-for-models-involving-differential-equations/}{R-bloggers
#'   article}.
#'
#'
#' @details
#'   According to IUPAC (2019), see als the \code{References}, the rate of a chemical reaction
#'   with the radicals (\eqn{\text{R}}) involved (refer to the example in \code{model.react} argument)
#'   \deqn{a\text{A} + r\text{R} \xrightarrow\, b\text{B}}
#'   is expressed via the time change of extent of the reaction (\eqn{\text{d}\xi/\text{d}t}):
#'   \deqn{-(1/r)\,(\text{d}n_{\text{R}}/\text{d}t) = -(1/a)\,(\text{d}n_{\text{A}}/\text{d}t) =
#'   (1/b)\,(\text{d}n_{\text{B}}/\text{d}t)}
#'   where \eqn{a,r,b} are the stoichiometric coefficients. At constant volume (\eqn{V}) conditions
#'   (or if volume changes are negligible) the amount (\eqn{n} in mole) of reactant/product
#'   can be replaced by its corresponding concentration (\eqn{c = n/V}). Such reaction rate
#'   (expressed in moles per unit volume and per second) is function of temperature (\eqn{T}),
#'   pressure (\eqn{p}) as well as that of concentration of reactants/products.
#'   For the reaction example shown above it applies (for radical \eqn{\text{R}}):
#'   \deqn{\text{d}c_{\text{R}}/\text{d}t = - r\,k(T,p)\,c_{\text{A}}^{\alpha}\,c_{\text{R}}^{\beta}}
#'   This is called rate law, where \eqn{k} is the rate constant and its pressure dependence is usually
#'   small and therefore can be ignored, in the first approach. Coefficients \eqn{\alpha} and \eqn{\beta},
#'   in general, correspond to fitting parameters, coming from experimental relation of the reaction
#'   rate and the concentration of reactants/products. These coefficients are called \strong{partial reaction orders}
#'   or \code{PROs} and their \strong{sum} represents \strong{total order of the reaction}. If the kinetic equation,
#'   for the reaction, corresponds to its stoichiometry, the reaction is described as the elementary
#'   one. In EPR Spectroscopy the number of radicals is directly proportional to (double) integral
#'   of the radical EPR spectrum (see also \code{\link{quantify_EPR_Abs}}). Therefore, for a quick evaluation
#'   or and/or comparison of different kinetic data, one can also obtain the rate constant
#'   from the area/integral \emph{vs} time fit onto the experimental EPR spectral time series outputs
#'   (see also the \code{\link{eval_kinR_EPR_modelFit}}).
#'   Accordingly, the "R" concentration (or number of radicals/V) can be replaced by the corresponding integral.
#'   For such a purpose a more general \strong{q}uantitative \strong{var}iable (\eqn{qvar}) is defined.
#'   However, in such case the unit of \eqn{k} must be expressed accordingly (see the \code{kin.params} argument).
#'   Quantitative kinetic profiles (such as that \eqn{\text{d}c_{\text{R}}/\text{d}t}
#'   or \eqn{\text{d}(qvar)_{\text{R}}/\text{d}t} described above) are not evaluated
#'   by common integration of the kinetic equations/rate laws, however by numeric solution of the Ordinary
#'   Differential Equations, \href{https://desolve.r-forge.r-project.org/index.html}{ODE in \code{{desolve}} R package}.
#'   Therefore, higher number of models might be available than for integrated
#'   differential equations, because for complex mechanisms it's quite often highly demanding to obtain
#'   the analytical solution by common integration. \strong{Several kinetic models for radical reactions} in EPR spectroscopy
#'   \strong{are predefined and summarized} below (see also the \code{model.react} function argument).
#'   \tabular{ll}{
#'   \strong{model.react} \tab \strong{Short Description} \cr
#'   \code{"(r=1)R --> [k1] B"} \tab Basic forward reaction,
#'   e.g. irrev. dimerization (if \code{(r=2)}). \cr
#'   \code{"(a=1)A --> [k1] (r=1)R"} \tab Basic forward radical formation \cr
#'   \code{"(a=1)A <==> [k1] [k4] (r=1)R <==> [k2] [k3] (b=1)B"} \tab Consecutive reactions,
#'   e.g. considering comproportionation (for \code{(a=2)} and \code{(r=2)}) + follow-up reversible
#'   dimerization (\code{(b=1)}). \cr
#'   \code{"(r=1)R <==> [k1] [k2] (b=1)B"} \tab Basic reversible radical quenching,
#'   e.g. rev. \eqn{\pi-\pi} dimerization for \code{(r=2)} and \code{(b=1)}. \cr
#'   \code{"(a=1)A <==> [k1] [k2] (r=1)R"} \tab Basic reversible radical formation,
#'   e.g. from rev. comproportionation of conjugated thiophene oligomers
#'   (\eqn{\text{A}^{++} + \text{A}^0 \xrightleftharpoons ~ 2\text{R}^{.+}}, for \code{(a=2)} and \code{(r=2)}). \cr
#'   \code{"(a=1)A + (b=1)B --> [k1] (r=1)R"} \tab Radical formation by chemical reaction like oxidation,
#'   reduction or spin trapping (if \code{A} refers to transient radical, which is not visible within
#'   the common EPR time scale). \cr
#'   \code{"(a=1)A + (r=1)R --> [k1] B"} \tab General radical quenching by chemical reaction. \cr
#'   }
#'
#'
#' @references
#'  International Union of Pure and Applied Chemistry (IUPAC) (2019). “Rate of Reaction”,
#'  \url{https://goldbook.iupac.org/terms/view/R05156}.
#'
#'  Quisenberry KT, Tellinghuisen J (2006). “Textbook Deficiencies: Ambiguities in Chemical Kinetics Rates
#'  and Rate Constants.” \emph{J. Chem. Educ.}, \strong{83}(3), 510, \url{https://doi.org/10.1021/ed083p510}.
#'
#'  Levine IN (2009). \emph{Physical Chemistry}, 6th edition. McGraw-Hill, ISBN 978-0-072-53862-5,
#'  \url{https://books.google.cz/books/about/Physical_Chemistry.html?id=L5juAAAAMAAJ&redir_esc=y}.
#'
#'  rdabbler (2013). “Learning R: Parameter Fitting for Models Involving Differential Equations”,
#'  \url{https://www.r-bloggers.com/2013/06/learning-r-parameter-fitting-for-models-involving-differential-equations/}.
#'
#'
#' @param model.react Character string, denoting a specific radical (\code{"R"}) reaction related to
#'   changes in integral intensities (or any other \strong{q}uantitative \strong{var}iable) in EPR spectral
#'   time series. Arrow shows direction of the reaction (\code{"-->", forward} or \code{"<==>", forward + reverse}).
#'   Rate constants are indicated by square brackets after the arrows. Following examples of the reaction schemes
#'   are predefined and commonly used to describe the integral intensity and/or radical concentration/amount
#'   changes during the EPR time series experiment (the \code{r,a,b} stoichiometric coefficients
#'   may vary, see below).
#'   \tabular{ll}{
#'   \strong{Reaction Scheme} \tab \strong{model.react} \cr
#'   \eqn{(r=1)\text{R} \xrightarrow{k_1} \text{B}} \tab \code{"(r=1)R --> [k1] B"} \cr
#'    \eqn{(a=2)\text{A} \xrightarrow{k_1} (r=2)\text{R}} \tab \code{"(a=2)A --> [k1] (r=2)R"} \cr
#'    \eqn{(a=2)\text{A} \xrightleftharpoons[k_4]{k_1} (r=2)\text{R}
#'    \xrightleftharpoons[k_3]{k_2} (b=1)\text{B}} \tab
#'    \code{"(a=2)A <==> [k1] [k4] (r=2)R <==> [k2] [k3] (b=1)B"} \cr
#'    \eqn{(r=1)\text{R} \xrightleftharpoons[k_2]{k_1} (b=1)\text{B}} \tab
#'    \code{"(r=1)R <==> [k1] [k2] (b=1)B"} \cr
#'    \eqn{(a=2)\text{A} \xrightleftharpoons[k_2]{k_1} (r=2)\text{R}} \tab
#'    \code{"(a=2)A <==> [k1] [k2] (r=2)R"} \cr
#'    \eqn{(a=1)\text{A} + (b=1)\text{B} \xrightarrow{k_1} (r=1)\text{R}} \tab
#'    \code{"(a=1)A + (b=1)B --> [k1] (r=1)R"} \cr
#'    \eqn{(a=1)\text{A} + (r=1)\text{R} \xrightarrow{k_1} \text{B}} \tab
#'    \code{"(a=1)A + (r=1)R --> [k1] B"} \cr
#'   }
#'   Couple of examples are also given in \code{Details}. The function is relatively flexible and enables
#'   later addition of any other reaction schemes describing the EPR time series experiments
#'   (YOU MAY ASK DEVELOPER(S) via forum/help-channels). The stoichiometric coefficient (such as \code{(r=1)}
#'   or \code{(a=1)}) can be varied within the \code{model.react} character string.
#'   Defined/Allowed values are integers, e.g. 1,2,3...etc. The space character within the \code{model.react}
#'   string is not fixed and can be skipped for the sake of simplicity.
#'   If \code{elementary.react = FALSE} (the model reaction is not considered as an elementary one),
#'   a possible non-integer partial coefficients (e.g. \code{alpha},\code{beta} or \code{gamma})
#'   must be included in \code{kin.params} (see also \code{kin.params} description). For the consecutive model reaction
#'   presented above, it applies only to one part/step of the mechanism.
#' @param model.expr.diff Logical, difference between the integral intensities/areas under the EPR spectra calculated
#'   using the experimental data and those generated by the model. By \strong{default} the argument
#'   is \strong{FALSE} and it is ONLY ACTIVATED (\code{model.expr.diff = TRUE}) IN THE CASE WHEN THE KINETIC MODEL
#'   FITTING PROCEDURE (see also \code{\link{eval_kinR_EPR_modelFit}} or examples below) IS PERFORMED.
#' @param elementary.react Logical, if the model reaction should be considered as the elementary one,
#'   i.e. the stoichiometric coefficients equal to the partial reaction orders. Such reaction proceeds without
#'   identifiable intermediate species forming. \strong{Default}: \code{elementary.react = TRUE}.
#'   If \code{elementary.react = FALSE}, i.e. the \code{model.react} cannot be considered like an elementary one,
#'   one must include the parameterized reaction orders \eqn{\alpha}, \eqn{\beta} or \eqn{\gamma} in
#'   the \code{kin.params}, e.g \code{kin.params = c(k1 = 0.01, qvar0A = 0.05, alpha = 1.5)}. For the consecutive
#'   model reaction presented above, it applies only to one part/step of the mechanism.
#' @param kin.params Named numeric vector, containing rate constants as well as initial radical
#'   or other reactant/product concentration/integral intensities/areas...etc. Therefore, a general
#'   \code{qvar} (\strong{q}uantitative \strong{var}iable) is defined which may actually reflect
#'   all above-mentioned quantities. \strong{Default}: \code{kin.params = c(k1 = 0.001,qvar0R = 0.02)}.
#'   The initial values are denoted as \code{qvar0X} (e.g. qvar0R for radical or qvar0A for the reactant \code{A}).
#'   The components of \code{kin.params} depend on \code{model.react} as well as on the \code{elementary.react}.
#'   If \code{elementary.react = FALSE} additional parameters like partial reaction orders (\code{alpha}
#'   and/or \code{beta} and/or \code{gamma}) must be defined within the \code{kin.params}, like summarized
#'   in the following table:
#'   \tabular{ll}{
#'   \strong{model.react} \tab \strong{Essential kin.params components} \cr
#'   \code{"(r=1)R --> [k1] B"} \tab \code{k1}, \code{qvar0R}, (\code{alpha}) \cr
#'   \code{"(a=1)A --> [k1] (r=1)R"} \tab \code{k1}, \code{qvar0A}, \code{qvar0R}, (\code{alpha}) \cr
#'   \code{"(a=1)A <==> [k1] [k4] (r=1)R <==> [k2] [k3] (b=1)B"} \tab \code{k1}, \code{k2},
#'   \code{k3}, \code{k4}, \code{qvar0A}, \code{qvar0R}, \code{qvar0B}, (\code{alpha},
#'   \code{beta}, \code{gamma}) \cr
#'   \code{"(r=1)R <==> [k1] [k2] (b=1)B"} \tab \code{k1}, \code{k2}, \code{qvar0R},
#'   \code{qvar0B}, (\code{alpha}, \code{beta}) \cr
#'   \code{"(a=1)A <==> [k1] [k2] (r=1)R"} \tab \code{k1}, \code{k2}, \code{qvar0A},
#'   \code{qvar0R}, (\code{alpha}, \code{beta}) \cr
#'   \code{"(a=1)A + (b=1)B --> [k1] (r=1)R"} \tab \code{k1}, \code{qvar0A}, \code{qvar0B},
#'   \code{qvar0R}, (\code{alpha},\code{beta}) \cr
#'   \code{"(a=1)A + (r=1)R --> [k1] B"} \tab \code{k1}, \code{qvar0A},
#'   \code{qvar0R}, (\code{alpha},\code{beta}) \cr
#'   }
#'   The \code{k1}-unit is eventually expressed in terms of \eqn{s^{-1}} as well as in units of the applied \code{qvar}
#'   (e.g. \code{c}, concentration) and depends on the partial reaction order(s) (PRO, see above),
#'   which power(s) the \code{qvar(s)}. For example, the \code{k1}-unit of elementary radical recombination,
#'   evaluated by double integrals, like \code{model.react = "(r=2)R --> [k1] B"},
#'   reads: \eqn{\text{s}^{-1}\,(\text{p.d.u.})^{-1}},
#'   where \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6803776/}{p.d.u.} stands for the procedure defined units.
#' @param time.unit Character string, corresponding to time unit like \code{"s"} (\strong{default}),
#'   \code{"min"} or \code{"h"}.
#' @param time.interval.model Numeric vector, including two values: starting and final time/termination
#'   of the model reaction (e.g. \code{c(0,1800)} in seconds, \strong{default}).
#' @param time.frame.model Numeric value, corresponding to interval time resolution, i.e. the smallest time difference
#'   between two consecutive points. The number of points is thus defined by the \code{time.interval.model} argument:
#'   \deqn{((Interval[2] - Interval[1])\,/\,Frame) + 1}
#'   This argument is required to numerically solve the kinetic differential equations by the \code{\link[deSolve]{ode}}.
#'   For the default interval mentioned above, the \strong{default} value reads \code{time.frame.model = 2} (in seconds).
#' @param solve.ode.method Character string, setting up the integrator (the \code{method} argument in \code{\link[deSolve]{ode}}),
#'   applied to find the numeric solution of ODE. \strong{Default}: \code{solve.ode.method = "lsoda"}
#'   (\code{\link[deSolve]{lsoda}}, additional methods, see the \code{ode} link above).
#' @param data.qt.expr A data frame object, containing the concentrations/integral intensities/areas under
#'   the EPR spectra calculated using the \strong{experimental data} as well as time column. These two essential
#'   column headers are described by the character strings like those below \code{time.expr} and \code{qvar.expr}.
#'   The \code{data.qt.expr} MUST BE USED ONLY IN SUCH CASE WHEN THE EXPERIMENTAL TIME HAS TO BE INCLUDED
#'   IN THE KINETIC MODEL (e.g. also for THE FITTING of EXPERIMENTAL DATA BY THE KINETIC MODEL).
#'   \strong{Default}: \code{data.qt.expr = NULL}.
#' @param time.expr Character string, pointing to \code{time} column/variable name in the original
#'   \code{data.qt.expr} data frame. \strong{Default}: \code{time.expr = NULL} (when the experimental
#'   data aren't taken into account). If the time has to be corrected (e.g. in the case of double integrals),
#'   please use \code{\link{correct_time_Exp_Specs}} function prior to kinetic evaluation.
#' @param qvar.expr Character string, pointing to \code{qvar} column/variable name in the original
#'   \code{data.qt.expr} data frame. \strong{Default}: \code{qvar.expr = NULL} (when the experimental
#'   data aren't taken into account).
#' @param ... additional arguments passed to the ODE (see also \code{\link[deSolve]{ode}}).
#'
#'
#' @return If the function \strong{is not used for fitting} of the experimental and processed data,
#'   the result is \code{list} consisting of:
#'   \describe{
#'   \item{df}{Data frame, containing \code{time} column and \code{qvar}, quantitative variable,
#'   columns corresponding to quantities of different relevant species
#'   denoted as \code{"R"}, \code{"A"}, \code{"B"} + if \code{data.qt.expr} is NOT NULL
#'   additional experimental quantitative variable is present.}
#'   \item{plot}{Plot object, containing \code{time} as \eqn{x}-axis and \code{qvar}
#'   (see \code{df} above) as \eqn{y}-axis + if \code{data.qt.expr} is NOT NULL the experimental
#'   quantitative variable is presented as well.}
#'   }
#'   Applying function \strong{for the fitting} procedure
#'   requires \code{model.expr.diff = TRUE} and therefore the result is represented by the difference between
#'   the integral intensities/areas, calculated using the experimental data
#'   and those generated by the model.
#'
#'
#' @examples
#' ## irreversible dimerization quantitative kinetic profile,
#' ## table (df) with first 10 observations/rows and application
#' ## of the "euler" integrator (to solve ODE) method
#' ##
#' kin.test.01 <-
#'   eval_kinR_ODE_model(model.react = "(r=2)R --> [k1] B",
#'                       kin.params = c(k1 = 0.012,
#'                                      qvar0R = 0.08),
#'                       solve.ode.method = "euler")
#' ## preview
#' head(kin.test.01$df,n = 10)
#' #
#' ## consecutive reactions and the corresponding plot
#' ## (`model.react` character string without spaces)
#' kin.test.02 <-
#'  eval_kinR_ODE_model(
#'    model.react = "(a=2)A<==>[k1][k4](r=2)R<==>[k2][k3](b=1)B",
#'    kin.params = c(k1 = 0.1,
#'                   k2 = 0.1,
#'                   k3 = 2e-4,
#'                   k4 = 2e-5,
#'                   qvar0A = 0.02,
#'                   qvar0R = 0.002,
#'                   qvar0B = 0)
#'  )
#' ## plot preview
#' kin.test.02$plot
#' #
#' ## data frame/table preview
#' head(kin.test.02$df)
#' #
#' ## loading example data (incl. `Area` and `time` variables)
#' ## from Xenon software: decay of a triarylamine radical cation
#' ## after its generation by electrochemical oxidation
#' triaryl_radCat_path <-
#'   load_data_example(file = "Triarylamine_radCat_decay_a.txt")
#' ## corresponding data (double integrated
#' ## EPR spectrum = `Area` vs `time`)
#' triaryl_radCat_data <-
#'   readEPR_Exp_Specs(triaryl_radCat_path,
#'                     header = TRUE,
#'                     fill = TRUE,
#'                     select = c(3,7),
#'                     col.names = c("time_s","Area"),
#'                     x.unit = "s",
#'                     x.id = 1,
#'                     Intensity.id = 2,
#'                     qValue = 1700,
#'                     data.structure = "others") %>%
#'   na.omit()
#' ## data preview
#' head(triaryl_radCat_data)
#' #
#' ## comparison of the kinetic model with the experimental
#' ## data `triaryl_radCat_data`, kinetic parameters were estimated
#' ## to be as close as possible to the latter.
#' compar_model_expr_data_01 <-
#'   eval_kinR_ODE_model(model.react = "(r=2)R --> [k1] B",
#'                       kin.params = c(qvar0R = 0.019,
#'                                      k1 = 0.04),
#'                       time.interval.model = c(0,1500),
#'                       data.qt.expr = triaryl_radCat_data,
#'                       qvar.expr = "Area",
#'                       time.expr = "time_s")
#' ## plot preview
#' compar_model_expr_data_01$plot
#' #
#' ## previous kinetic model with partial reaction
#' ## order ("alpha") corresponding to "R" (radical species).
#' ## In such case REACTION is NOT CONSIDERED
#' ## as an ELEMENTARY one !
#' compar_model_expr_data_02 <-
#'   eval_kinR_ODE_model(model.react = "(r=2)R --> [k1] B",
#'                       elementary.react = FALSE,
#'                       kin.params = c(qvar0R = 0.019,
#'                                      k1 = 0.04,
#'                                      alpha = 1.9
#'                                     ),
#'                       time.interval.model = c(0,1500),
#'                       data.qt.expr = triaryl_radCat_data,
#'                       qvar.expr = "Area",
#'                       time.expr = "time_s")
#' ## plot preview
#' compar_model_expr_data_02$plot
#'
#'
#' @export
#'
#'
#' @importFrom deSolve ode
#' @importFrom ggplot2 geom_point
eval_kinR_ODE_model <- function(model.react = "(r=1)R --> [k1] B", ## e.g. r = 1 or 2
                                model.expr.diff = FALSE, ## must TRUE for fit, otherwise `FALSE`
                                elementary.react = TRUE, ## can be `FALSE` or `TRUE`
                                kin.params = c(
                                  k1 = 0.001,
                                  qvar0R = 0.02
                                ),  ## add. "alpha", "beta", "gamma" for general partial react. orders
                                time.unit = "s", ## also "min" and "h" can be defined
                                time.interval.model = c(0,1800), ## also provided for expr.
                                time.frame.model = 2, # time resolution in s
                                solve.ode.method = "lsoda", # numeric integrator method
                                data.qt.expr = NULL,
                                time.expr = NULL,
                                qvar.expr = NULL,
                                ...) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  time <- NULL
  #
  ## Condition for `elementary.react` and `kin.params`
  if (isTRUE(elementary.react)){
    ## looking for parameterized partial reaction orders
    if (any(grepl("alpha|beta|gamma",names(kin.params)))){
      stop(" The model reaction is considered as an elementary one.\n
           Parameterized partial reaction orders ('alpha','beta',...)\n
           must not be included in `kin.params` ! ")
    }
  } else {
    if (!any(grepl("alpha|beta|gamma",names(kin.params)))){
      stop(" The model reaction is not elementary !\n
           Please, define parametrized partial reaction\n
           order (such as 'alpha = 1.5') in `kin.params` ! ")
    }
  }
  #
  ## Data definition
  if (!is.null(data.qt.expr)) {
    if (is.null(time.expr) || is.null(qvar.expr)) {
      stop(" Time of the experimental data series (`time.expr`)\n
           or the corresponding quantitative variable (`qvar.expr`)\n
           is not specified. Please, define ! ")
    } else {
      time.expr.vec <- data.qt.expr[[time.expr]]
      start.time <- min(time.expr.vec)
      final.time <- max(time.expr.vec)
    }
  }
  #
  ## time definition for the spectral series
  if (is.null(time.interval.model)){
    stop(" Please define the hypothetical time interval for the model reaction ! ")
  } else{
    if (!is.null(data.qt.expr)){
      start.time <- min(time.expr.vec)
      final.time <- max(time.expr.vec)
    } else{
      start.time <- time.interval.model[1]
      final.time <- time.interval.model[2]
    }
    ## time resolution for different spans
    ## due point limitations of `ODE` solution
    t <- seq(start.time, final.time, by = time.frame.model)
    #
    if (final.time > 259200){
      if (time.unit == "s"){
        stop(" Hypothetical time interval for the model reaction > 3 days.\n
               Please, define the `time.unit` in minutes or in hours ! ")
      }
    }
  }
  if (is.null(data.qt.expr)) {
    t <- t
  } else {
    if (is.null(time.expr)){
      stop(" Time vector/column in the original `data.qt.expr`,\n
           to compare the experiment with the kinetic model, must be defined ! ")
    } else{
      ## time to combine  model + experiment for better "resolution"
      t <- c(t, time.expr.vec)
      ## order time in ascending mode + remove duplicate values :
      t <- sort(unique(t))
    }
  }
  #
  ## function to extract stoichiometric coefficient from `model.react`
  ## expression => extracting the `(r=...)` or `(a=...)` string from `model.react`
  stoichiom_coeff <- function(expression, coeff = "r") {
    if (coeff == "r") {
      rabc.string <- stringr::str_extract(expression, pattern = "\\(r=[[:digit:]]\\)")
    }
    if (coeff == "a") {
      rabc.string <- stringr::str_extract(expression, pattern = "\\(a=[[:digit:]]\\)")
    }
    if (coeff == "b"){
      rabc.string <- stringr::str_extract(expression, pattern = "\\(b=[[:digit:]]\\)")
    }
    model.react.rabc <- stringr::str_extract(rabc.string, pattern = "[[:digit:]]")
    model.react.rabc <- as.numeric(model.react.rabc)
    #
    return(model.react.rabc)
    #
  }
  #
  ## elementary reaction (partitial reaction order,pro) condition =>
  pro.cond <- ifelse(isFALSE(elementary.react),TRUE,FALSE)
  #
  ## ======================= INDIVIDUAL KINETIC MODELS ===============================
  #
  ## ------- (1) ------- "(r=1)R --> [k1] B" (r = 1,2,...integer) -------------------
  #
  ## the `model.react` can be also detected regardless of "spaces" which
  ## in R regular expression can be expressed as `(.*|?!\\s)` meaning
  ## that there is either arb.character (incl. space) or no space :-) =>
  #
  if (grepl("^\\(r=.*R(.*|?!\\s)-->(.*|?!\\s)\\[k1\\](.*|?!\\s)B$",
            model.react)) {
    #
    ## functions for derivative solution of kinetic equation
    react_rates_diff_01 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    no.pro = elementary.react) {
      ## t <- time vector
      ## qvar <- concentration, double EPR integral,
      ## (`qvar` \equiv "quantitative varialble")
      ## number of radicals...etc. vector
      ## kin.params <- rate constant and coeffs.
      ## ( if `elementary.react = FALSE`)
      ## `stoichio.coeff` stoichiometric coefficients
      ## `no.pro` no partial reaction order, if `TRUE` => stoichiometric
      ## coeff. == exponent in rate definition
      k1 <- kin.params$k1
      if (isFALSE(no.pro)) {
        alpha <- kin.params$alpha
      }
      ## initial conditions
      rate <- rep(0, times = 1)
      ## differential equations
      c_r <- stoichio.coeff[1]
      if (isTRUE(no.pro)) {
        rate[1] <- -k1 * c_r * (qvar["R"])^(c_r) ## - (1/c_r) * (dc(R)/dt) = k * c^(c_r)(R)
      } else {
        rate[1] <- -k1 * c_r * (qvar["R"])^alpha ## - (1/c_r) * (dc(R)/dt) = k * c^(alpha)(R)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. concentration)
    qvar0 <- c(R = unname(kin.params["qvar0R"]))
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    if (pro.cond) {alpha <- kin.params["alpha"]}
    #
    ## solving `ordinary diff. equation(s)`
    result <- deSolve::ode(
      y = qvar0,
      times = t,
      stoichio.coeff = stoichiom_coeff(model.react),
      func = react_rates_diff_01,
      parms = switch(2-pro.cond,
                     list(k1 = k1,alpha = alpha),
                     list(k1 = k1)),
      method = solve.ode.method,
      ...
    )
    #
  }
  #
  ## ----- (2) ----- "(a=1)A --> [k1] (r=1)R" (a,r = 1,2,...integer) ---------------
  #
  ## the `model.react` can be also detected regardless of "spaces" which
  ## in R regular expression can be expressed as `(.*|?!\\s)` meaning
  ## that there is either arb.character (incl. space) or no space :-) =>
  #
  if (grepl("^\\(a=.*A(.*|?!\\s)-->(.*|?!\\s)\\[k1\\](.*|?!\\s)\\(r=.*R$",
            model.react)) {
    ## functions for derivative solution of kinetic equations
    react_rates_diff_02 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    no.pro = elementary.react) {
      k1 <- kin.params$k1
      if (isFALSE(no.pro)) {
        alpha <- kin.params$alpha
      }
      ## initial conditions
      rate <- rep(0, times = 2)
      ## differential equations
      c_a <- stoichio.coeff[1]
      c_r <- stoichio.coeff[2]
      if (isTRUE(no.pro)) {
        rate[1] <- -k1 * c_a * (qvar["A"])^(c_a)
        rate[2] <- k1 * c_r * (qvar["A"])^(c_a)
      } else{
        rate[1] <- -k1 * c_a * (qvar["A"])^alpha
        rate[2] <- k1 * c_r * (qvar["A"])^alpha
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. concentration)
    qvar0 <- c(
      A = unname(kin.params["qvar0A"]),
      R = unname(kin.params["qvar0R"])
    )
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    if (pro.cond) {alpha <- kin.params["alpha"]}
    #
    ## solving `ordinary diff. equation(s)
    result <- deSolve::ode(
      y = qvar0,
      times = t,
      stoichio.coeff = c(stoichiom_coeff(model.react,coeff = "a"),
                         stoichiom_coeff(model.react)),
      func = react_rates_diff_02,
      parms = switch(2-pro.cond,
                     list(k1 = k1,alpha = alpha),
                     list(k1 = k1)),
      method = solve.ode.method,
      ...
    )
    #
  }
  #
  ## --- (3) --- "(a=1)A <==> [k1] [k4] (r=1)R <==> [k2] [k3] (b=1)B" (a,r,b = 1,2,...integer) ------
  #
  ## ...the same `(.*|?!\\s)` arb.character(incl. space)/no space like before
  #
  ## With several `(.*|?!\\s)` the `grepl()` argument would be quite long =>
  ## therefore condition is changed:
  if (grepl("A.*R.*B",model.react) &
      grepl("<==>.*<==>",model.react) &
      grepl("k1.*k4.*k2.*k3",model.react)){
    ## functions for derivative solution of kinetic equations
    react_rates_diff_03 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    no.pro = elementary.react) {
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      k3 <- kin.params$k3
      k4 <- kin.params$k4
      if (isFALSE(no.pro)) {
        alpha <- kin.params$alpha
        beta <- kin.params$beta
        gamma <- kin.params$gamma
      }
      ## initial conditions
      rate <- rep(0, times = 3)
      ## differential equations
      c_a <- stoichio.coeff[1]
      c_r <- stoichio.coeff[2]
      c_b <- stoichio.coeff[3]
      if (isTRUE(no.pro)) {
        rate[1] <- -(k1 * c_a * (qvar["A"])^(c_a)) + (k4 * c_a * (qvar["R"])^(c_r))
        rate[2] <- (k1 * c_r * (qvar["A"])^(c_a)) - (k2 * c_r * (qvar["R"])^(c_r)) +
          (k3 * c_r * (qvar["B"])^(c_b)) - (k4 * c_r * (qvar["R"])^(c_r))
        rate[3] <- (k2 * c_b * (qvar["R"])^(c_r)) - (k3 * c_b * (qvar["B"])^(c_b))
      } else {
        rate[1] <- -(k1 * c_a * (qvar["A"])^alpha) + (k4 * c_a * (qvar["R"])^beta)
        rate[2] <- (k1 * c_r * (qvar["A"])^alpha) - (k2 * c_r * (qvar["R"])^beta) +
          (k3 * c_r * (qvar["B"])^gamma) - (k4 * c_r * (qvar["R"])^beta)
        rate[3] <- (k2 * c_b * (qvar["R"])^beta) - (k3 * c_b * (qvar["B"])^gamma)
      }
      ## derivative as a list
      return(list(rate))
    }
    ## initial (`0`) qvar (e.g. concentration)
    qvar0 <- c(
      A = unname(kin.params["qvar0A"]),
      R = unname(kin.params["qvar0R"]),
      B = unname(kin.params["qvar0B"])
    )
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    k2 <- kin.params["k2"]
    k3 <- kin.params["k3"]
    k4 <- kin.params["k4"]
    if (pro.cond) {
      alpha <- kin.params["alpha"]
      beta <- kin.params["beta"]
      gamma <- kin.params["gamma"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    result <- deSolve::ode(
      y = qvar0,
      times = t,
      stoichio.coeff = c(stoichiom_coeff(model.react,coeff = "a"),
                         stoichiom_coeff(model.react,coeff = "r"),
                         stoichiom_coeff(model.react,coeff = "b")),
      func = react_rates_diff_03,
      parms = switch(2-pro.cond,
                     list(k1 = k1,k2 = k2,k3 = k3,k4 = k4,
                          alpha = alpha,beta = beta,gamma = gamma),
                     list(k1 = k1,k2 = k2,k3 = k3,k4 = k4)),
      method = solve.ode.method,
      ...
    )
    #
  }
  #
  ## ---- (4) ---- "(r=1)R <==> [k1] [k2] (b=1)B" (r,b = 1,2,...integer) -------------
  #
  ## ...the same `(.*|?!\\s)` arb.character(incl. space)/no space like before
  #
  if (grepl("^\\(r=.*R(.*|?!\\s)<==>(.*|?!\\s)\\[k1\\](.*|?!\\s)\\[k2\\](.*|?!\\s)\\(b=.*B$",
            model.react)) {
    ## functions for derivative solution of kinetic equations
    react_rates_diff_04 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    no.pro = elementary.react) {
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (isFALSE(no.pro)) {
        alpha <- kin.params$alpha
        beta <- kin.params$beta
      }
      ## initial conditions
      rate <- rep(0, times = 2)
      ## differential equations
      c_r <- stoichio.coeff[1]
      c_b <- stoichio.coeff[2]
      if (isTRUE(no.pro)) {
        rate[1] <- -(k1 * c_r * (qvar["R"])^(c_r)) + (k2 * c_r * (qvar["B"])^(c_b))
        rate[2] <- (k1 * c_b * (qvar["R"])^(c_r)) - (k2 * c_b * (qvar["B"])^(c_b))
      } else{
        rate[1] <- -(k1 * c_r * (qvar["R"])^alpha) + (k2 * c_r * (qvar["B"])^beta)
        rate[2] <- (k1 * c_b * (qvar["R"])^alpha) - (k2 * c_b * (qvar["B"])^beta)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. concentration)
    qvar0 <- c(
      R = unname(kin.params["qvar0R"]),
      B = unname(kin.params["qvar0B"])
    )
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    k2 <- kin.params["k2"]
    if (pro.cond) {
      alpha <- kin.params["alpha"]
      beta <- kin.params["beta"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    result <- deSolve::ode(
      y = qvar0,
      times = t,
      stoichio.coeff = c(stoichiom_coeff(model.react),
                         stoichiom_coeff(model.react,coeff = "b")),
      func = react_rates_diff_04,
      parms = switch(2-pro.cond,
                     list(k1 = k1,k2 = k2,alpha = alpha,beta = beta),
                     list(k1 = k1,k2 = k2)),
      method = solve.ode.method,
      ...
    )
    #
  }
  #
  ## --- (5) --- "(a=1)A <==> [k1] [k2] (r=1)R" (a,r = 1,2,...integer) ------------
  #
  ## ...the same `(.*|?!\\s)` arb.character(incl. space)/no space like before
  #
  if (grepl("^\\(a=.*A(.*|?!\\s)<==>(.*|?!\\s)\\[k1\\](.*|?!\\s)\\[k2\\](.*|?!\\s)\\(r=.*R$",
            model.react)) {
    ## functions for derivative solution of kinetic equations
    react_rates_diff_05 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    no.pro = elementary.react) {
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (isFALSE(no.pro)) {
        alpha <- kin.params$alpha
        beta <- kin.params$beta
      }
      ## initial conditions
      rate <- rep(0, times = 2)
      ## differential equations
      c_a <- stoichio.coeff[1]
      c_r <- stoichio.coeff[2]
      if (isTRUE(no.pro)) {
        rate[1] <- -(k1 * c_a * (qvar["A"])^(c_a)) + (k2 * c_a * (qvar["R"])^(c_r))
        rate[2] <- (k1 * c_r * (qvar["A"])^(c_a)) - (k2 * c_r * (qvar["R"])^(c_r))
      } else {
        rate[1] <- -(k1 * c_a * (qvar["A"])^alpha) + (k2 * c_a * (qvar["R"])^beta)
        rate[2] <- (k1 * c_r * (qvar["A"])^alpha) - (k2 * c_r * (qvar["R"])^beta)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. concentration)
    qvar0 <- c(
      A = unname(kin.params["qvar0A"]),
      R = unname(kin.params["qvar0R"])
    )
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    k2 <- kin.params["k2"]
    if (pro.cond) {
      alpha <- kin.params["alpha"]
      beta <- kin.params["beta"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    result <- deSolve::ode(
      y = qvar0,
      times = t,
      stoichio.coeff = c(stoichiom_coeff(model.react,coeff = "a"),
                         stoichiom_coeff(model.react,coeff = "r")),
      func = react_rates_diff_05,
      parms = switch(2-pro.cond,
                     list(k1 = k1,k2 = k2,alpha = alpha,beta = beta),
                     list(k1 = k1,k2 = k2)),
      method = solve.ode.method,
      ...
    )
    #
  }
  #
  ## --- (6) --- "(a=1)A + (b=1)B --> [k1] (r=1)R", (a,b,r = 1,2,...integer) ----------
  #
  ## ...the same `(.*|?!\\s)` arb.character(incl. space)/no space like before
  #
  if (grepl("^\\(a=.*A(.*|?!\\s)\\+(.*|?!\\s)\\(b=.*B(.*|?!\\s)-->(.*|?!\\s)\\[k1\\](.*|?!\\s)\\(r=.*R$",
            model.react)) {
    ## functions for derivative solution of kinetic equations
    react_rates_diff_06 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    no.pro = elementary.react) {
      k1 <- kin.params$k1
      if (isFALSE(no.pro)) {
        alpha <- kin.params$alpha
        beta <- kin.params$beta
      }
      ## initial conditions
      rate <- rep(0, times = 3)
      ## differential equations
      c_a <- stoichio.coeff[1]
      c_b <- stoichio.coeff[2]
      c_r <- stoichio.coeff[3]
      if (isTRUE(no.pro)) {
        rate[1] <- -k1 * c_a * ((qvar["A"])^(c_a)) * ((qvar["B"])^(c_b))
        rate[2] <- -k1 * c_b * ((qvar["B"])^(c_b)) * ((qvar["A"])^(c_a))
        rate[3] <- k1 * c_r * ((qvar["A"])^(c_a)) * ((qvar["B"])^(c_b))
      } else {
        rate[1] <- -k1 * c_a * ((qvar["A"])^alpha) * ((qvar["B"])^beta)
        rate[2] <- -k1 * c_b * ((qvar["B"])^beta) * ((qvar["A"])^alpha)
        rate[3] <- k1 * c_r * ((qvar["A"])^alpha) * ((qvar["B"])^beta)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. concentration)
    qvar0 <- c(
      A = unname(kin.params["qvar0A"]),
      B = unname(kin.params["qvar0B"]),
      R = unname(kin.params["qvar0R"])
    )
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    if (isFALSE(elementary.react)) {
      alpha <- kin.params["alpha"]
      beta <- kin.params["beta"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    result <- deSolve::ode(
      y = qvar0,
      times = t,
      stoichio.coeff = c(stoichiom_coeff(model.react,coeff = "a"),
                         stoichiom_coeff(model.react,coeff = "b"),
                         stoichiom_coeff(model.react)),
      func = react_rates_diff_06,
      parms = switch(2-pro.cond,
                     list(k1 = k1,alpha = alpha,beta = beta),
                     list(k1 = k1)),
      method = solve.ode.method,
      ...
    )
    #
  }
  #
  ## --- (7) --- "(a=1)A + (r=1)R --> [k1] B", (a,r = 1,2,...integer) -----------
  #
  ## ...the same `(.*|?!\\s)` arb.character(incl. space)/no space like before
  #
  if (grepl("^\\(a=.*A(.*|?!\\s)\\+(.*|?!\\s)\\(r=.*R(.*|?!\\s)-->(.*|?!\\s)\\[k1\\](.*|?!\\s)B$",
            model.react)) {
    ## functions for derivative solution of kinetic equations
    react_rates_diff_07 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    no.pro = elementary.react) {
      k1 <- kin.params$k1
      if (isFALSE(no.pro)) {
        alpha <- kin.params$alpha
        beta <- kin.params$beta
      }
      ## initial conditions
      rate <- rep(0, times = 2)
      ## differential equations
      c_a <- stoichio.coeff[1]
      c_r <- stoichio.coeff[2]
      if (isTRUE(no.pro)) {
        rate[1] <- -k1 * c_a * ((qvar["A"])^(c_a)) * ((qvar["R"])^(c_r))
        rate[2] <- -k1 * c_r * ((qvar["R"])^(c_r)) * ((qvar["A"])^(c_a))
      } else {
        rate[1] <- -k1 * c_a * ((qvar["A"])^alpha) * ((qvar["R"])^beta)
        rate[2] <- -k1 * c_r * ((qvar["R"])^beta) * ((qvar["A"])^alpha)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. concentration)
    qvar0 <- c(
      A = unname(kin.params["qvar0A"]),
      R = unname(kin.params["qvar0R"])
    )
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    if (pro.cond) {
      alpha <- kin.params["alpha"]
      beta <- kin.params["beta"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    result <- deSolve::ode(
      y = qvar0,
      times = t,
      stoichio.coeff = c(stoichiom_coeff(model.react,coeff = "a"),
                         stoichiom_coeff(model.react,coeff = "r")),
      func = react_rates_diff_07,
      parms = switch(2-pro.cond,
                     list(k1 = k1,alpha = alpha,beta = beta),
                     list(k1 = k1)),
      method = solve.ode.method,
      ...
    )
    #
  }
  #
  ## ======================== DATA AND KINETIC PLOTS ============================
  #
  ## DATA processing
  ## conversion into data frame
  result.df <- data.frame(result)
  if (!is.null(data.qt.expr)) {
    ## following operation required for the difference
    ## the same number of points for the model as well as for the expr.
    result.df <- result.df %>%
      dplyr::filter(.data$time %in% data.qt.expr[[time.expr]]) %>%
      ## add `qvar.expr`
      dplyr::mutate(!!rlang::quo_name(paste0(qvar.expr,"_expr")) := data.qt.expr[[qvar.expr]])
  }
  if (!is.null(data.qt.expr) & isTRUE(model.expr.diff)) {
    ## difference
    diff.model.expr <- result.df[[paste0(qvar.expr,"_expr")]] - result.df[["R"]]
  }
  #
  if (grepl("^\\(r=.*R(.*|?!\\s)-->(.*|?!\\s)\\[k1\\](.*|?!\\s)B$", model.react)) {
    #
    ## the first col. is `time` and 2nd has to be renamed
    # names(result.df)[2] <- "R"
    #
    ## data frame for plotting
    if (isFALSE(model.expr.diff)) {
      result.df.plot <- result.df
      #
      ## BASE PLOT
      plot.base <- ggplot(result.df.plot) +
        geom_point(
          aes(
            x = .data[["time"]],
            y = .data[["R"]]
          ),
          size = 2.0,
          shape = 18,
          color = "darkviolet"
        )
    }
    #
  } else {
    #
    if (isFALSE(model.expr.diff)) {
      ## data frame for plotting
      result.df.plot <- result.df %>%
        tidyr::pivot_longer(!time, names_to = "Species", values_to = "qvar") %>%
        dplyr::arrange(time)
      #
      ## BASE PLOT
      plot.base <- ggplot(result.df.plot) +
        geom_point(
          aes(
            x = .data[["time"]],
            y = .data[["qvar"]],
            color = .data[["Species"]]
          ),
          size = 2.0,
          shape = 18
        ) +
        theme(
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 13)
        )
    }
  }
  #
  ## ============================= ENTIRE PLOTS & RESULTS ==============================
  #
  if (isFALSE(model.expr.diff)) {
  ## Caption character vector
  caption.char.vec <- mapply(function(i, j) paste0(i, " = ", j), names(kin.params), kin.params)
  caption.char.vec <- paste(unname(caption.char.vec), collapse = ", ")
  ## the entire plot (incl. scheme and parameters)
  plotR <- plot.base +
    labs(
      title = model.react,
      caption = caption.char.vec,
      x = bquote(italic(Time) ~ ~"(" ~ .(time.unit) ~ ")"),
      y = bquote(italic(Quantitative ~ ~Variable) ~ ~ ~ ~ bolditalic(qvar))
    ) +
    plot_theme_In_ticks() +
    theme(plot.title = element_text(hjust = 0.5))
  #
  ## RESULTS
    if (!is.null(data.qt.expr)){
      ## plot with experimental data + model
      plot.compar <- ggplot(result.df) +
        geom_point(aes(x = .data[["time"]],
                       y = .data[[paste0(qvar.expr,"_expr")]],
                       color = "Experiment"),
                   size = 2.6) +
        geom_point(aes(x = .data[["time"]],
                       y = .data[["R"]],
                       color = "\nKinetic\nModel"),
                   size = 1.5) +
        scale_color_manual(values = c("darkcyan","magenta"),
                           breaks = c("Experiment","\nKinetic\nModel")) +
        labs(title = paste0(model.react,"    Model + Experiment"),
             color = NULL,
             caption = caption.char.vec,
             x = bquote(italic(Time) ~ ~"(" ~ .(time.unit) ~ ")"),
             y = bquote(italic(Quantitative ~ ~Variable) ~ ~ ~ ~ bolditalic(qvar))) +
        plot_theme_In_ticks() +
        theme(plot.title = element_text(hjust = 0.5),
              legend.text = element_text(size = 13),
              legend.text.align = 0.5)
      #
      return(list(df = result.df, plot = plot.compar))
      #
    } else{
      #
      return(list(df = result.df, plot = plotR))
      #
    }
  } else {
    #
    return(diff.model.expr)
    #
  }
  #
}
