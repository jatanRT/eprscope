#
#' Quantitative Kinetic Model Profiles by Numeric Solution of the ODE.
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'   Theoretical quantitative kinetic profiles (such as concentration/amount/integral intensity)
#'   as well as fitting of the experimental data for various predefined model reactions with
#'   the radical(s) (labeled as "R"). Profiles are provided by the numeric solution of rate equations
#'   by the \strong{O}rdinary \strong{D}ifferential \strong{E}quations (ODE). This function is inspired by:
#'   \href{https://www.r-bloggers.com/2013/06/learning-r-parameter-fitting-for-models-involving-differential-equations/}{R-bloggers
#'   article}.
#'
#'
#' @details
#'   According to IUPAC \insertCite{IUPACkinetR2019}{eprscope} the rate of a chemical reaction
#'   with the radicals (\eqn{\text{R}}) involved (example see argument \code{model.react})
#'   \deqn{r\text{R} + b\text{B} \xrightarrow\,\, c\text{C}}
#'   is expressed via time change of the reaction extend (\eqn{\text{d}\xi/\text{d}t}):
#'   \deqn{-(1/r)\,(\text{d}n_{\text{R}}/\text{d}t) = -(1/b)\,(\text{d}n_{\text{B}}/\text{d}t) =
#'   (1/c)\,(\text{d}n_{\text{C}}/\text{d}t)}
#'   At constant volume (\eqn{V}) conditions (or if volume changes are negligible) the amount
#'   (\eqn{n} in mole) of reactant/product can be replaced by its corresponding concentration
#'   (\eqn{c = n/V}). Such reaction rate (expressed in moles per unit volume) is function of temperature,
#'   pressure as well as of concentration of reactants and products. For the reaction example shown above
#'   it applies:
#'   equation...TBC
#'   In EPR Spectroscopy the number of radicals
#'   is directly proportional to (double) integral of the radical EPR spectrum
#'   (see also \code{\link{quantify_EPR_Abs}}).
#'   Secondary purpose is to fit the experimental EPR spectral time series outputs, e.g. integral
#'   intensity (area under the spectral curve) \emph{vs} time in order to gather the rate constants
#'   (\eqn{k}) of radical ("R") formation and/or decay. Quantitative kinetic profiles
#'   are not evaluated by integration of the kinetic equations, however by numeric solution of the Ordinary
#'   Differential Equations (\href{http://desolve.r-forge.r-project.org/index.html}{ODE}, see also
#'   \pkg{deSolve} package). Therefore, higher number of models is available than for integrated
#'   differential equations because for complex mechanisms it's quite often highly demanding.
#'   Several kinetic models for radical reactions in EPR spectroscopy are predefined and summarized below
#'   (see also \code{model.react} function argument).
#'   \tabular{ll}{
#'   \strong{model.react} \tab \strong{Short Description} \cr
#'   \code{"(r=1)R --> [k1] B"} \tab Basic irreversible forward reaction,
#'   e.g. irrev. dimerization (if \code{(r=2)}). \cr
#'   \code{"(a=1)A --> [k1] (r=1)R"} \tab Basic irreversible radical formation,
#'   e.g. irrev. \eqn{\text{e}^-} transfer. \cr
#'   \code{"(a=1)A --> [k1] (r=1)R <==> [k2] [k3] (c=1)C"} \tab Consecutive reactions,
#'   e.g. like comproportionation (for \code{(a=2)} and \code{(r=2)}) + follow-up reversible
#'   dimerization (\code{(c=1)}). \cr
#'   \code{"(r=1)R <==> [k1] [k2] (b=1)B"} \tab Basic reversible radical quenching,
#'   e.g. rev. \eqn{\pi-\pi} dimerization for \code{(r=2)} and \code{(b=1)}. \cr
#'   \code{"(a=1)A <==> [k1] [k2] (r=1)R"} \tab Basic reversible radical formation,
#'   e.g. from rev. comproportionation (\eqn{\text{A}^{++} + \text{A}^0 \xrightleftharpoons ~ 2\text{R}^{.+}},
#'   for \code{(a=2)} and \code{(r=2)}). \cr
#'   \code{"(a=1)A + (b=1)B --> [k1] (r=1)R"} \tab Radical formation by chemical reaction like oxidation,
#'   reduction or spin trapping (transient radical is not visible within the common EPR time scale). \cr
#'   \code{"(r=1)R + (b=1)B --> [k1] (c=1)C"} \tab General radical quenching by chemical reaction. \cr
#'   }
#'
#'
#' @references
#'  \insertRef{IUPACkinetR2019}{eprscope}
#'
#'  \insertRef{KinetRJCE2006}{eprscope}
#'
#'  \insertRef{LevinePCbook2009}{eprscope}
#'
#'  \insertRef{RbloggKinet2013}{eprscope}
#'
#'
#' @param model.react Character string denoting a specific radical (\code{"R"}) reaction related to
#'   changes in integral intensities (or any other \strong{q}uantitative \strong{var}iable) in EPR spectral
#'   time series. Arrow shows direction of the reaction (\code{"-->", irreversible} or \code{"<==>", reversible}).
#'   Rate constants are indicated by square brackets after the arrows. Following examples of the reaction schemes
#'   are predefined and commonly used to describe the integral intensity and/or radical concentration/amount
#'   changes during the EPR time series experiment (the \code{r,a,b,c} stoichiometric coefficients
#'   may be varied, see below).
#'   \tabular{ll}{
#'   \strong{Reaction Scheme} \tab \strong{model.react} \cr
#'   \eqn{(r=1)\text{R} \xrightarrow{k_1} \text{B}} \tab \code{"(r=1)R --> [k1] B"} \cr
#'    \eqn{(a=2)\text{A} \xrightarrow{k_1} (r=2)\text{R}} \tab \code{"(a=2)A --> [k1] (r=2)R"} \cr
#'    \eqn{(a=2)\text{A} \xrightarrow{k_1} (r=2)\text{R} \xrightleftharpoons[k_3]{k_2} (c=1)\text{C}} \tab
#'    \code{"(a=2)A --> [k1] (r=2)R <==> [k2] [k3] (c=1)C"} \cr
#'    \eqn{(r=1)\text{R} \xrightleftharpoons[k_2]{k_1} (b=1)\text{B}} \tab
#'    \code{"(r=1)R <==> [k1] [k2] (b=1)B"} \cr
#'    \eqn{(a=2)\text{A} \xrightleftharpoons[k_2]{k_1} (r=2)\text{R}} \tab
#'    \code{"(a=2)A <==> [k1] [k2] (r=2)R"} \cr
#'    \eqn{(a=1)\text{A} + (b=1)\text{B} \xrightarrow{k_1} (r=1)\text{R}} \tab
#'    \code{"(a=1)A + (b=1)B --> [k1] (r=1)R"} \cr
#'    \eqn{(r=1)\text{R} + (b=1)\text{B} \xrightarrow{k_1} (c=1)\text{C}} \tab
#'    \code{"(r=1)R + (b=1)B --> [k1] (c=1)C"} \cr
#'   }
#'   Couple of examples are also given in details. The function is relatively flexible and enables
#'   later addition of any other reaction schemes describing the EPR time series experiments
#'   (YOU MAY ASK DEVELOPER(S) via forum/help-channels). The stoichiometric coefficient (such as \code{(r=1)}
#'   or \code{(a=1)}) can be varied within the \code{model.react} character string.
#'   Defined/Allowed values are integers e.g. 1,2,3...etc. The space character within the \code{model.react}
#'   string is not fixed and can be skipped for the sake of flexibility.
#'   If \code{elementary.react = FALSE} (the model reaction is not considered as an elementary one),
#'   a possible non-integer partial stoichiometric coefficients (e.g. \code{alpha},\code{beta} or \code{gamma})
#'   must be included in \code{kin.params} (see also \code{kin.params} description).
#' @param model.expr.diff Logical, difference between the integral intensities/areas under the EPR spectra calculated
#'   using the experimental data and those generated by the model. By \strong{default} the argument
#'   is \strong{FALSE} and it is ONLY ACTIVATED (\code{model.expr.diff = TRUE}) IN THE CASE WHEN THE KINETIC MODEL
#'   FITTING PROCEDURE (see also \code{\link{eval_kinR_EPR_modelFit}} or examples below) IS PERFORMED.
#' @param elementary.react Logical, if the model reaction should be considered as elementary one,
#'   i.e. the stoichiometric coefficients equal to the partial reaction orders. Such reaction proceeds without
#'   identifiable intermediate species forming. \strong{Default}: \code{elementary.react = TRUE}.
#'   If \code{elementary.react = FALSE}, i.e. the \code{model.react} cannot be considered like an elementary one,
#'   one must include the parameterized reaction orders \eqn{\alpha}, \eqn{\beta} or \eqn{\gamma} into
#'   the \code{kin.params}, e.g like \code{kin.params = c(k1 = 0.01, qvar0A = 0.05, alpha = 1.5)}.
#' @param kin.params Named numeric vector containing rate constants as well as initial radical
#'   or other reactant/product concentration/integral intensities/areas...etc. Therefore, a general
#'   \code{qvar} (\strong{q}uantitative \strong{var}iable) was defined which may actually reflect
#'   all above-mentioned quantities. \strong{Default}: \code{kin.params = c(k1 = 0.001,qvar0R = 0.02)}.
#'   The initial values are denoted as \code{qvar0X} (e.g. qvar0R for radical or qvar0A for the reactant \code{A}).
#'   The components of \code{kin.params} depend on \code{model.react} as well as on the \code{elementary.react}.
#'   If \code{elementary.react = FALSE} additional parameters like partial reaction orders (\code{alpha}
#'   and/or \code{beta} and/or \code{gamma}) must be defined within \code{kin.params}, like summarized
#'   in the following table:
#'   \tabular{ll}{
#'   \strong{model.react} \tab \strong{Essential kin.params components} \cr
#'   \code{"(r=1)R --> [k1] B"} \tab \code{k1}, \code{qvar0R}, (\code{alpha}) \cr
#'   \code{"(a=1)A --> [k1] (r=1)R"} \tab \code{k1}, \code{qvar0A}, \code{qvar0R}, (\code{alpha}) \cr
#'   \code{"(a=1)A --> [k1] (r=1)R <==> [k2] [k3] (c=1)C"} \tab \code{k1}, \code{k2}, \code{k3} \code{qvar0A},
#'   \code{qvar0R}, \code{qvar0C}, (\code{alpha}, \code{beta}, \code{gamma}) \cr
#'   \code{"(r=1)R <==> [k1] [k2] (b=1)B"} \tab \code{k1}, \code{k2}, \code{qvar0R},
#'   \code{qvar0B}, (\code{alpha}, \code{beta}) \cr
#'   \code{"(a=1)A <==> [k1] [k2] (r=1)R"} \tab \code{k1}, \code{k2}, \code{qvar0A},
#'   \code{qvar0R}, (\code{alpha}, \code{beta}) \cr
#'   \code{"(a=1)A + (b=1)B --> [k1] (r=1)R"} \tab \code{k1}, \code{qvar0A}, \code{qvar0B},
#'   \code{qvar0R}, (\code{alpha},\code{beta}) \cr
#'   \code{"(r=1)R + (b=1)B --> [k1] (c=1)C"} \tab \code{k1}, \code{qvar0R}, \code{qvar0B},
#'   \code{qvar0C}, (\code{alpha},\code{beta}) \cr
#'   }
#' @param time.unit Character string corresponding to time unit like \code{"s"} (\strong{default}),
#'   \code{"min"} or \code{"h"}.
#' @param timeLim.model Numeric vector incl. two values corresponding to starting and final time/termination
#'   of the model reaction.
#' @param data.qt.expr A data frame object containing the concentrations/integral intensities/areas under
#'   the EPR spectra calculated using the \strong{experimental data} as well as time column. These two essential
#'   columns are described by the character strings like those below \code{time.expr} and \code{qvar.expr}.
#'   The \code{data.qt.expr} MUST BE USED ONLY IN SUCH CASE WHEN THE EXPERIMENTAL TIME HAS TO BE INCLUDED
#'   IN THE KINETIC MODEL (e.g. also for THE FITTING of EXPERIMENTAL DATA BY THE KINETIC MODEL).
#'   \strong{Default}: \code{data.qt.expr = NULL}.
#' @param time.expr Character string pointing to \code{time} column/variable name in the original
#'   \code{data.qt.expr} data frame. \strong{Default}: \code{time.expr = NULL} (when the experimental
#'   data aren't taken into account). If the time has to be corrected (e.g. in the case of double integrals),
#'   please use \code{\link{correct_time_Exp_Specs}} function prior to kinetic evaluation.
#' @param qvar.expr Character string pointing to \code{qvar} column/variable name in the original
#'   \code{data.qt.expr} data frame. \strong{Default}: \code{qvar.expr = NULL} (when the experimental
#'   data aren't taken into account).
#' @param ... additional arguments passed to the ODE (see also \code{\link[deSolve]{ode}}).
#'
#'
#' @return If the function \strong{is not used for fitting} of the experimental and processed data,
#'   the result is \code{list} consisting of:
#'   \describe{
#'   \item{df}{Data frame containing \code{time} column and \code{qvar}, quantitative variable,
#'   columns corresponding to quantities of different relevant species
#'   denoted as \code{"R"}, \code{"A"}, \code{"B"}, ... etc. + if \code{data.qt.expr} is NOT NULL
#'   additional experimental quantitative variable is present.}
#'   \item{plot}{Plot object containing \code{time} as abscissa and \code{qvar}
#'   (see \code{df} above) as \eqn{y}-axis. + if \code{data.qt.expr} is NOT NULL the experimental
#'   quantitative variable is presented as well.}
#'   }
#'   Applying function \strong{for the fitting} procedure
#'   requires \code{model.expr.diff = TRUE} and therefore the result is difference between
#'   the integral intensities/areas under the curves calculated using the experimental data
#'   and those generated by the model.
#'
#'
#' @examples
#' ## irreversible dimerization quantitative kinetic profile
#' ## table (df) with first 10 observations/rows
#' kin.test.01 <-
#'   eval_kinR_ODE_model(model.react = "(r=2)R --> [k1] B",
#'                       kin.params = c(k1 = 0.012,
#'                                      qvar0R = 0.08))
#' ## preview
#' head(kin.test.01$df,n = 10)
#' #
#' ## consecutive reactions and the corresponding plot
#' ## (`model.react` character string without spaces)
#' kin.test.02 <-
#'  eval_kinR_ODE_model(
#'    model.react = "(a=2)A-->[k1](r=2)R<==>[k2][k3](c=1)C",
#'    kin.params = c(k1 = 0.1,
#'                   k2 = 0.1,
#'                   k3 = 0.0002,
#'                   qvar0A = 0.02,
#'                   qvar0R = 0.002,
#'                   qvar0C = 0)
#'  )
#' ## plot preview
#' kin.test.02$plot
#' #
#' ## data frame/table preview
#' head(kin.test.02$df)
#' #
#' ## loading example data (incl. `Area` and `time` variables)
#' ## from Xenon: decay of a triarylamine radical cation
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
#'                       timeLim.model = c(0,1500),
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
#'                       timeLim.model = c(0,1500),
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
                                timeLim.model = c(0,1800), ## also provided for expr.
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
           have not to be incl. in `kin.params` ! ")
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
  if (is.null(timeLim.model)){
    stop(" Please define the hypothetical time\n
           span for the model reaction ! ")
  } else{
    if (!is.null(data.qt.expr)){
      start.time <- min(time.expr.vec)
      final.time <- max(time.expr.vec)
    } else{
      start.time <- timeLim.model[1]
      final.time <- timeLim.model[2]
    }
    ## time resolution for different spans
    ## due point limitations of `ODE` solution
    if (final.time < 0.1 & final.time >= 0.001){
      t <- seq(start.time, final.time, by = 0.0001)
    }
    if (final.time < 6 & final.time >= 0.1){
      t <- seq(start.time, final.time, by = 0.005)
    }
    if (final.time < 100 & final.time >= 6){
      t <- seq(start.time, final.time, by = 0.1)
    }
    if (final.time < 600 & final.time >= 100){
      t <- seq(start.time, final.time, by = 0.5)
    }
    if (final.time < 1000 & final.time >= 600){
      t <- seq(start.time, final.time, by = 1)
    }
    if (final.time < 2000 & final.time >= 1000){
      t <- seq(start.time, final.time, by = 2)
    }
    if (final.time < 3600 & final.time >= 2000){
      t <- seq(start.time, final.time, by = 4)
    }
    if (final.time < 86400 & final.time >= 3600){
      t <- seq(start.time, final.time, by = 60)
    }
    if (final.time < 259200 & final.time >= 86400){
      t <- seq(start.time, final.time, by = 300)
    }
    if (final.time > 259200){
      if (time.unit == "s"){
        stop(" Hypothetical time span for the model reaction > 3 days.\n
               Please, define the `time.unit` in minutes or in hours ! ")
      }
    }
  }
  if (is.null(data.qt.expr)) {
    t <- t
  } else {
    if (is.null(time.expr)){
      stop(" Time vector/column to compare the experiment with\n
           the kinetic model must be defined ! ")
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
    if (coeff == "c"){
      rabc.string <- stringr::str_extract(expression, pattern = "\\(c=[[:digit:]]\\)")
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
        rate[1] <- -k1 * c_r * (qvar)^(c_r) ## - (1/c_r) * (dc(R)/dt) = k * c^(c_r)(R)
      } else {
        rate[1] <- -k1 * c_r * (qvar)^alpha ## - (1/c_r) * (dc(R)/dt) = k * c^(alpha)(R)
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
      ...
    )
    #
  }
  #
  ## --- (3) --- "(a=1)A --> [k1] (r=1)R <==> [k2] [k3] (c=1)C" (a,r,c = 1,2,...integer) ------
  #
  ## ...the same `(.*|?!\\s)` arb.character(incl. space)/no space like before
  #
  ## With several `(.*|?!\\s)` the `grepl()` argument would be quite long =>
  ## therefore condition is changed:
  if (grepl("A.*R.*C",model.react) &
      grepl("-->.*<==>",model.react) &
      grepl("k1.*k2.*k3",model.react)){
    ## functions for derivative solution of kinetic equations
    react_rates_diff_03 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    no.pro = elementary.react) {
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      k3 <- kin.params$k3
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
      c_c <- stoichio.coeff[3]
      if (isTRUE(no.pro)) {
        rate[1] <- -k1 * c_a * (qvar["A"])^(c_a)
        rate[2] <- (k1 * c_r * (qvar["A"])^(c_a)) - (k2 * c_r * (qvar["R"])^(c_r)) +
          (k3 * c_r * (qvar["C"])^(c_c))
        rate[3] <- (k2 * c_c * (qvar["R"])^(c_r)) - (k3 * c_c * (qvar["C"])^(c_c))
      } else {
        rate[1] <- -k1 * c_a * (qvar["A"])^alpha
        rate[2] <- (k1 * c_r * (qvar["A"])^alpha) - (k2 * c_r * (qvar["R"])^beta) +
          (k3 * c_r * (qvar["C"])^gamma)
        rate[3] <- (k2 * c_c * (qvar["R"])^beta) - (k3 * c_c * (qvar["C"])^gamma)
      }
      ## derivative as a list
      return(list(rate))
    }
    ## initial (`0`) qvar (e.g. concentration)
    qvar0 <- c(
      A = unname(kin.params["qvar0A"]),
      R = unname(kin.params["qvar0R"]),
      C = unname(kin.params["qvar0C"])
    )
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    k2 <- kin.params["k2"]
    k3 <- kin.params["k3"]
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
                         stoichiom_coeff(model.react,coeff = "c")),
      func = react_rates_diff_03,
      parms = switch(2-pro.cond,
                     list(k1 = k1,k2 = k2,k3 = k3,
                          alpha = alpha,beta = beta,gamma = gamma),
                     list(k1 = k1,k2 = k2,k3 = k3)),
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
        rate[1] <- (-k1 * c_r * (qvar["R"])^(c_r)) + (k2 * c_r * (qvar["B"])^(c_b))
        rate[2] <- (k1 * c_b * (qvar["R"])^(c_r)) - (k2 * c_b * (qvar["B"])^(c_b))
      } else{
        rate[1] <- (-k1 * c_r * (qvar["R"])^alpha) + (k2 * c_r * (qvar["B"])^beta)
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
        rate[1] <- (-k1 * c_a * (qvar["A"])^(c_a)) + (k2 * c_a * (qvar["R"])^(c_r))
        rate[2] <- (k1 * c_r * (qvar["A"])^(c_a)) - (k2 * c_r * (qvar["R"])^(c_r))
      } else {
        rate[1] <- (-k1 * c_a * (qvar["A"])^alpha) + (k2 * c_a * (qvar["R"])^beta)
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
      ...
    )
    #
  }
  #
  ## --- (7) --- "(r=1)R + (b=1)B --> [k1] (c=1)C", (r,b,c = 1,2,...integer) -----------
  #
  ## ...the same `(.*|?!\\s)` arb.character(incl. space)/no space like before
  #
  if (grepl("^\\(r=.*R(.*|?!\\s)\\+(.*|?!\\s)\\(b=.*B(.*|?!\\s)-->(.*|?!\\s)\\[k1\\](.*|?!\\s)\\(c=.*C$",
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
      rate <- rep(0, times = 3)
      ## differential equations
      c_r <- stoichio.coeff[1]
      c_b <- stoichio.coeff[2]
      c_c <- stoichio.coeff[3]
      if (isTRUE(no.pro)) {
        rate[1] <- -k1 * c_r * ((qvar["R"])^(c_r)) * ((qvar["B"])^(c_b))
        rate[2] <- -k1 * c_b * ((qvar["B"])^(c_b)) * ((qvar["R"])^(c_r))
        rate[3] <- k1 * c_c * ((qvar["R"])^(c_r)) * ((qvar["B"])^(c_b))
      } else {
        rate[1] <- -k1 * c_r * ((qvar["R"])^alpha) * ((qvar["B"])^beta)
        rate[2] <- -k1 * c_b * ((qvar["B"])^beta) * ((qvar["R"])^alpha)
        rate[3] <- k1 * c_c * ((qvar["R"])^alpha) * ((qvar["B"])^beta)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. concentration)
    qvar0 <- c(
      A = unname(kin.params["qvar0R"]),
      B = unname(kin.params["qvar0B"]),
      R = unname(kin.params["qvar0C"])
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
      stoichio.coeff = c(stoichiom_coeff(model.react),
                         stoichiom_coeff(model.react,coeff = "b"),
                         stoichiom_coeff(model.react,coeff = "c")),
      func = react_rates_diff_07,
      parms = switch(2-pro.cond,
                     list(k1 = k1,alpha = alpha,beta = beta),
                     list(k1 = k1)),
      ...
    )
    #
  }
  #
  ## ======================== DATA AND KINETIC PLOTS ============================
  #
  if (grepl("^\\(r=.*R(.*|?!\\s)-->(.*|?!\\s)\\[k1\\](.*|?!\\s)B$", model.react)) {
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
    ## the first col. is `time` and 2nd has to be renamed
    names(result.df)[2] <- "R"
    #
    if (!is.null(data.qt.expr) & isTRUE(model.expr.diff)) {
      ## difference
      diff.model.expr <- data.qt.expr[[qvar.expr]] - result.df[["R"]]
    }
    #
    ## data frame for plotting
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
  } else {
    #
    ## DATA processing
    ## conversion into data frame
    result.df <- data.frame(result)
    if (!is.null(data.qt.expr)) {
      result.df <- result.df %>%
        dplyr::filter(.data$time %in% data.qt.expr[[time.expr]]) %>%
        ## add `qvar.expr`
        dplyr::mutate(!!rlang::quo_name(paste0(qvar.expr,"_expr")) := data.qt.expr[[qvar.expr]])
    }
    #
    if (!is.null(data.qt.expr) & isTRUE(model.expr.diff)) {
      ## difference
      diff.model.expr <- data.qt.expr[[qvar.expr]] - result.df[["R"]]
    }
    #
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
  #
  ## ============================= ENTIRE PLOTS & RESULTS ==============================
  #
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
  if (isFALSE(model.expr.diff)) {
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
