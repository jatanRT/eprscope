#
#' Quantitative Kinetic Model Profiles by Numeric Solution of the ODE.
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'   Theoretical quantitative kinetic profiles (e.g. like concentration/amount/integral intensity)
#'   as well as fitting of the experimental data for various model reactions where
#'   the radical(s) ("R") are involved in the processes. Profiles are provided by the numeric solution
#'   of the \strong{O}rdinary \strong{D}ifferential \strong{E}quations.
#'
#'
#' @details
#'   Secondary purpose is to fit the experimental EPR spectral time series outputs, e.g. like integral
#'   intensity (area under the spectral curve) \emph{vs} time in order to gather the rate constants
#'   (\eqn{k}) of radical ("R") formation and/or decay. Quantitative kinetic profiles
#'   are not evaluated by integration of the kinetic equations, however by numeric solution of the Ordinary
#'   Differential Equations (\href{http://desolve.r-forge.r-project.org/index.html}{ODE}, see also
#'   \pkg{deSolve} package). Therefore, higher number of models is available than for integrated
#'   differential equations because their solution is quite often highly demanding.
#'   The function is inspired by
#'   \href{https://www.r-bloggers.com/2013/06/learning-r-parameter-fitting-for-models-involving-differential-equations/}{R-bloggers
#'   article}.
#'   Several kinetic models for radical reactions in EPR spectroscopy are summarized below
#'   (see also \code{model.react} function argument).
#'   \tabular{ll}{
#'   ------------------------------- \tab --------------------------------------------------------- \cr
#'   \strong{`model.react`} \tab \strong{Short Description} \cr
#'   ------------------------------- \tab --------------------------------------------------------- \cr
#'   \code{"(n=1)R --> [k1] B"} \tab Basic irreversible forward reaction,
#'   e.g. like irrev. dimerization (if \code{"(n=2)"}). \cr
#'   \code{"(n=1)A --> [k1] (m=1)R"} \tab Basic irreversible radical formation,
#'   e.g. like irrev. homogeneous \eqn{\text{e}^-} transfer (if \code{"(n=2)"} and \code{"(m=2)"}). \cr
#'   \code{"(n=1)A --> [k1] (m=1)R <==> [k2] [k3] (l=1)C"} \tab Consecutive reactions,
#'   e.g. like comproportionation (or homogeneous
#'   \eqn{\text{e}^-} transfer, for \code{"(n=2)"} and \code{"(m=2)"}) + follow-up reversible
#'   dimerization (\code{"(l=1)"}). \cr
#'   \code{"(n=1)R <==> [k1] [k2] (m=1)B"} \tab Basic reversible radical quenching,
#'   e.g. like rev. \eqn{\pi-\pi} dimerization for \code{"(n=2)"} and \code{"(m=1)"}. \cr
#'   \code{"(n=1)A <==> [k1] [k2] (m=1)R"} \tab Basic reversible radical formation,
#'   e.g. from rev.
#'   comproportionation/disproportionation (\eqn{\text{A}^{++} + \text{A}^0 \xrightleftharpoons ~ 2\text{R}^{.+}},
#'   for \code{"(n=2)"} and \code{"(m=2)"}). \cr
#'   \code{"(n=1)A + (m=1)B --> [k1] (l=1)R"} \tab Radical formation by chemical reaction like oxidation,
#'   reduction or spin trapping (transient radical is not visible within the common EPR time scale). \cr
#'   \code{"(n=1)R + (m=1)B --> [k1] (l=1)C"} \tab General radical quenching by chemical reaction. \cr
#'   ------------------------------- \tab --------------------------------------------------------- \cr
#'   }
#'
#'
#' @param model.react Character string denoting a specific radical (\code{"R"}) reaction related to
#'   changes in integral intensities in EPR spectral time series. Arrow shows direction of the reaction
#'   (\code{"-->"}, irreversible or \code{"<==>", reversible}). Rate constants are indicated by square
#'   brackets after the arrows. Such notation inherited from
#'   \href{https://www.ctan.org/pkg/mhchem?lang=en}{\eqn{\LaTeX} `mhchem` package}.
#'   Following examples of the reaction schemes are commonly used to describe the integral intensity
#'   and/or radical concentration/amount changes during the EPR time series experiment
#'   (the \code{m,n,l} stoichiometric coefficients may be varied, see below).
#'   \tabular{ll}{
#'   ------------------------------ \tab ------------------------------------------------------ \cr
#'   \strong{Reaction Scheme} \tab \strong{`model.react`} \cr
#'   ------------------------------ \tab ------------------------------------------------------ \cr
#'   \eqn{(n=1)\text{R} \xrightarrow{k_1} \text{B}} \tab \code{"(n=1)R --> [k1] B"} \cr
#'    \eqn{(n=2)\text{A} \xrightarrow{k_1} (m=2)\text{R}} \tab \code{"(n=2)A --> [k1] (m=2)R"} \cr
#'    \eqn{(n=2)\text{A} \xrightarrow{k_1} (m=2)\text{R} \xrightleftharpoons[k_3]{k_2} (l=1)\text{C}} \tab
#'    \code{"(n=2)A --> [k1] (m=2)R <==> [k2] [k3] (l=1)C"} \cr
#'    \eqn{(n=1)\text{R} \xrightleftharpoons[k_2]{k_1} (m=1)\text{B}} \tab
#'    \code{"(n=1)R <==> [k1] [k2] (m=1)B"} \cr
#'    \eqn{(n=2)\text{A} \xrightleftharpoons[k_2]{k_1} (m=2)\text{R}} \tab
#'    \code{"(n=2)A <==> [k1] [k2] (m=2)R"} \cr
#'    \eqn{(n=1)\text{A} + (m=1)\text{B} \xrightarrow{k_1} (l=1)\text{R}} \tab
#'    \code{"(n=1)A + (m=1)B --> [k1] (l=1)R"} \cr
#'    \eqn{(n=1)\text{R} + (m=1)\text{B} \xrightarrow{k_1} (l=1)\text{C}} \tab
#'    \code{"(n=1)R + (m=1)B --> [k1] (l=1)C"} \cr
#'    ----------------------------- \tab ------------------------------------------------------ \cr
#'   }
#'   Couple of examples are also given in details. The function is relatively flexible and enables
#'   later addition of any other reaction schemes describing the EPR time series experiments
#'   (YOU MAY ASK DEVELOPER(S) via `github issue`). The stoichiometric coefficient (e.g. like \code{"(n=1)"}
#'   or \code{"(m=1)"}) can be varied within the \code{model.react} character string.
#'   Defined/Allowed values are e.g. 1,2,... or non-integer values like 1.5 or 2.1.
#'   If \code{elementary.react = FALSE} (the model reaction is not considered as the elemental one) therefore,
#'   a possible non-integer partial stoichiometric coefficients (like e.g. `alpha`,`beta` or `gamma`)
#'   must be incl. in \code{kin.params} (see also \code{kin.params} below).
#' @param model.expr.diff Logical, difference between the integral intensities/areas under the curves calculated
#'   using the experimental data and those generated by the model. By \strong{default} the argument
#'   is \strong{FALSE} and it is ONLY ACTIVATED (\code{model.expr.diff = TRUE}) IN CASE KINETIC MODEL
#'   FITTING PROCEDURE (see also \code{\link{eval_kinR_EPR_modelFit}} or examples below).
#' @param elementary.react Logical, description...TBC...
#' @param kin.params Named numeric vector containing rate constants as well as initial radical
#'   or other reactant/product concentration/integral intensities/areas...etc. Therefore, a general
#'   \code{qvar} (\strong{q}uantitative \strong{var}iable) was defined which may actually reflect
#'   all mentioned quantities. \strong{Default}: \code{kin.params = c(k1 = 0.001,qvar0R = 0.02)}.
#'   The initial values are denoted as \code{qvar0X} (e.g. qvar0R for radical or qvar0A for reactant).
#'   The components of \code{kin.params} named numeric vector depend on \code{model.react}
#'   as well as on the \code{elementary.react}. If \code{elementary.react = FALSE} additional parameterized partial
#'   reaction orders (`alpha` and/or `beta` and/or `gamma`) must be defined within \code{kin.params}
#'   like summarized in the following table =>
#'   \tabular{ll}{
#'   ----------------------------- \tab ----------------------------------- \cr
#'   \strong{`model.react`} \tab \strong{Essential `kin.params` components} \cr
#'   ----------------------------- \tab ----------------------------------- \cr
#'   \code{"(n=1)R --> [k1] B"} \tab \code{k1}, \code{qvar0R}, (\code{alpha}) \cr
#'   \code{"(n=1)A --> [k1] (m=1)R"} \tab \code{k1}, \code{qvar0A}, \code{qvar0R}, (\code{alpha}) \cr
#'   \code{"(n=1)A --> [k1] (m=1)R <==> [k2] [k3] (l=1)C"} \tab \code{k1}, \code{k2}, \code{k3} \code{qvar0A},
#'   \code{qvar0R}, \code{qvar0C}, (\code{alpha}, \code{beta}, \code{gamma}) \cr
#'   \code{"(n=1)R <==> [k1] [k2] (m=1)B"} \tab \code{k1}, \code{k2}, \code{qvar0R},
#'   \code{qvar0B}, (\code{alpha}, \code{beta}) \cr
#'   \code{"(n=1)A <==> [k1] [k2] (m=1)R"} \tab \code{k1}, \code{k2}, \code{qvar0A},
#'   \code{qvar0R}, (\code{alpha}, \code{beta}) \cr
#'   \code{"(n=1)A + (m=1)B --> [k1] (l=1)R"} \tab \code{k1}, \code{qvar0A}, \code{qvar0B},
#'   \code{qvar0R}, (\code{alpha},\code{beta}) \cr
#'   \code{"(n=1)R + (m=1)B --> [k1] (l=1)C"} \tab \code{k1}, \code{qvar0R}, \code{qvar0B},
#'   \code{qvar0C}, (\code{alpha},\code{beta}) \cr
#'   ----------------------------- \tab ----------------------------------- \cr
#'   }
#' @param time.unit Character string ...description...TBC.
#' @param timeLim.model Numeric vector incl. two values corresponding to starting and final time
#'   for the model reaction.
#' @param data.expr R data frame containing the integral intensities/areas under the curves calculated
#'   using the \strong{experimental data} as well as time column. These two essential columns
#'   are described by character strings like bellow (see the last two arguments).
#'   The \code{data.expr} MUST BE USED ONLY IN CASE IF THE EXPERIMENTAL TIME HAS TO BE INCLUDED
#'   IN THE KINETIC MODEL (e.g. also for THE FITTING of EXPR. DATA BY THE KINETIC MODEL).
#'   \strong{Default}: \code{data.expr = NULL}.
#' @param time.expr Character string pointing to `time` \strong{column name} in the original
#'   \code{data.expr} data frame. \strong{Default}: \code{time.expr = NULL} (in case of experimental
#'   data aren't taken into account).
#' @param qvar.expr Character string pointing to `qvar` \strong{column name} in the original
#'   \code{data.expr} data frame. \strong{Default}: \code{qvar.expr = NULL} (in case of experimental
#'   data aren't taken into account).
#' @param ... additional arguments passed to the integrator or to the methods of the ODE
#'   (see also \code{\link[deSolve]{ode}}).
#'
#'
#' @return If the function is not used for fitting of the experimental and processed data, the result
#'   is \code{list} consisting of:
#'   \describe{
#'   \item{df}{Data frame containing \code{time} column and \code{qvar}, quantitative variable,
#'   columns corresponding to quantities of different relevant species
#'   denoted as \code{"R"}, \code{"A"}, \code{"B"}, ... etc.}
#'   \item{plot}{Plot object containing \code{time} as abscissa and \code{qvar}
#'   (see \code{df} above) as \eqn{y}-axis.}
#'   }
#'   Applying function for the fitting procedure
#'   requires \code{model.expr.diff = TRUE} and therefore the result is difference between
#'   the integral intensities/areas under the curves calculated using the experimental data
#'   and those generated by the model.
#'
#'
#' @examples
#' ## irreversible dimerization quantitative kinetic profile
#' ## table (df) with first 10 observations/rows
#' kin.test.01 <-
#'  eval_kinR_ODE_model(model.react = "(n=2)R --> [k1] B",
#'                      kin.params = c(k1 = 0.012,
#'                                     qvar0R = 0.08))
#' head(kin.test.01$df,n = 10)
#' #
#' ## consecutive reactions and preesenting plot
#' kin.test.02 <-
#'  eval_kinR_ODE_model(model.react = "(n=2)A --> [k1] (m=2)R <==> [k2] [k3] (l=1)C",
#'                      kin.params = c(k1 = 0.01,
#'                                     k2 = 0.015,
#'                                     k3 = 0.0075,
#'                                     qvar0A = 0.02,
#'                                     qvar0R = 0.002,
#'                                     qvar0C = 0))
#' kin.test.02$plot
#' #
#' \dontrun{
#' ## taking into account the experimental time for the spectral series
#' eval_kinR_ODE_model(model.react = "(n=2)A --> [k1] (m=2)R",
#'                     model.expr.diff = FALSE,
#'                     kin.params = c(k1 = 0.005,
#'                                    qvar0A = 0.05,
#'                                    qvar0R = 0),
#'                     data.expr = data.integs,
#'                     time.expr = "time_s")
#' #
#' ## using `eval_kinR_ODE_model()` function for fitting
#' ## of the experimental data in the previous case +
#' ## + parameterized partial order for `A` (alpha)
#' minpack.lm::nls.lm(par = c(k1 = 0.005,
#'                            qvar0A = 0.05,
#'                            qvar0R = 0,
#'                            alpha = 1.5),
#'                    elementary.react = FALSE,
#'                    fn = eval_kinR_ODE_model,
#'                    model.react = "(n=2)A --> [k1] (m=2)R",
#'                    model.expr.diff = TRUE,
#'                    data.expr = data.integs,
#'                    time.expr = "time_s",
#'                    qvar.expr = "Area")
#' }
#'
#'
#' @export
#'
#'
#' @importFrom deSolve ode
#' @importFrom ggplot2 geom_point
eval_kinR_ODE_model <- function(model.react = "(n=1)R --> [k1] B", ## e.g. n = 1,2
                                model.expr.diff = FALSE, ## must TRUE for fit, otherwise `FALSE`
                                elementary.react = TRUE, ## can be `FALSE` or `TRUE`
                                kin.params = c(
                                  k1 = 0.001,
                                  qvar0R = 0.02
                                ),  ## "alpha", "beta", "gamma" for general partial react. orders
                                time.unit = "s", ## also "min" and "h" can be defined
                                timeLim.model = c(0,1800), ## also provided for expr.
                                data.expr = NULL,
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
      stop(" The model reaction is considered as elementary.\n
           No parameterized partial reaction orders\n
           ('alpha','beta',...) must be incl. in `kin.params` ! ")
    }
  } else {
    if (!any(grepl("alpha|beta|gamma",names(kin.params)))){
      stop(" The model reaction is not elementary !\n
           Please, define parametrized partial reaction\n
           order values (e.g. like 'alpha = 1.5') in `kin.params` ! ")
    }
  }
  #
  ## Data definition
  if (!is.null(data.expr)) {
    if (is.null(time.expr) || is.null(qvar.expr)) {
      stop(" Time of the experimental data series (`time.expr`)\n
           or the corresponding quantitative variable (`qvar.expr`)\n
           is not specified. Please, define ! ")
    } else {
      time.expr.vec <- data.expr[[time.expr]]
      start.time <- min(time.expr.vec)
      final.time <- max(time.expr.vec)
    }
  }
  #
  ## time definition for the spectral series
  if (is.null(timeLim.model) || is.na(timeLim.model)){
    stop(" Please define hypothetical time\n
           span for the model reaction ! ")
  } else{
    if (!is.null(data.expr)){
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
      if (time.unit = "s"){
        stop(" Hypothetical time span for the model reaction > 3 days.\n
               Please, define time `time.unit` in minutes or in hours ! ")
      }
    }
  }
  if (is.null(data.expr)) {
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
  ## expression => extracting the `(n=...)` string from `model.react`
  stoichiom_coeff <- function(expression, coeff = "n") {
    if (coeff == "n") {
      nm.string <- stringr::str_extract(expression, pattern = "\\(n=[[:digit:]]\\)")
    }
    if (coeff == "m") {
      nm.string <- stringr::str_extract(expression, pattern = "\\(m=[[:digit:]]\\)")
    }
    model.react.nm <- stringr::str_extract(nm.string, pattern = "[[:digit:]]")
    model.react.nm <- as.numeric(model.react.nm)
    #
    return(model.react.nm)
    #
  }
  #
  ## ======================= INDIVIDUAL KINETIC MODELS ===============================
  #
  ## ------- (1) ------- "(n=1)R --> [k1] B" (n = 1,2,non-integer) -------------------
  #
  if (grepl("^\\(n=.*R --> \\[k1\\] B$", model.react)) {
    #
    ## functions for derivative solution of kinetic equation
    react_rates_diff_01 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    pro = elementary.react) {
      ## t <- time vector
      ## qvar <- concentration, double EPR integral,
      ## (`qvar` \equiv "quantitative varialble")
      ## number of radicals...etc. vector
      ## kin.params <- rate constant and coeffs.
      ## ( if `elementary.react = FALSE`)
      ## `stoichio.coeff` stoichiometric coefficient
      ## `pro` partial reaction order, if `TRUE` => stoichiometric
      ## coeff. == exponent in rate definition
      k1 <- kin.params$k1
      if (isFALSE(pro)) {
        alpha <- kin.params$alpha
      }
      ## initial conditions
      rate <- rep(0, times = 1)
      ## differential equations
      c_n <- stoichio.coeff[1]
      if (isTRUE(pro)) {
        rate[1] <- -k1 * c_n * (qvar)^(c_n) ## - (1/c_n) * (dc(R)/dt) = k * c^(c_n)(R)
      } else {
        rate[1] <- -k1 * c_n * (qvar)^alpha ## - (1/c_n) * (dc(R)/dt) = k * c^(alpha)(R)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. like concentration)
    qvar0 <- c(R = unname(kin.params["qvar0R"]))
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    if (isFALSE(elementary.react)) {
      alpha <- kin.params["alpha"]
      #
      ## solving `ordinary diff. equation(s)`
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = stoichiom_coeff(model.react),
        func = react_rates_diff_01,
        parms = list(k1 = k1,
                     alpha = alpha),
        ...
      )
    } else {
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = stoichiom_coeff(model.react),
        func = react_rates_diff_01,
        parms = list(k1 = k1),
        ...
      )
    }
    #
  }
  #
  ## ----- (2) ----- "(n=1)A --> [k1] (m=1)R" (n,m = 1,2,non-integer) ---------------
  #
  if (grepl("^\\(n=.*A --> \\[k1\\] \\(m=.*R$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_02 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    pro = elementary.react) {
      k1 <- kin.params$k1
      if (isFALSE(pro)) {
        alpha <- kin.params$alpha
      }
      ## initial conditions
      rate <- rep(0, times = 2)
      ## differential equations
      c_n <- stoichio.coeff[1]
      c_m <- stoichio.coeff[2]
      if (isTRUE(pro)) {
        rate[1] <- -k1 * c_n * (qvar["A"])^(c_n)
        rate[2] <- k1 * c_m * (qvar["A"])^(c_n)
      } else{
        rate[1] <- -k1 * c_n * (qvar["A"])^alpha
        rate[2] <- k1 * c_m * (qvar["A"])^alpha
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. like concentration)
    qvar0 <- c(
      A = unname(kin.params["qvar0A"]),
      R = unname(kin.params["qvar0R"])
    )
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    if (isFALSE(elementary.react)) {
      alpha <- kin.params["alpha"]
      #
      ## solving `ordinary diff. equation(s)
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m")),
        func = react_rates_diff_02,
        parms = list(k1 = k1,
                     alpha = alpha),
        ...
      )
    } else {
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m")),
        func = react_rates_diff_02,
        parms = list(k1 = k1),
        ...
      )
    }
    #
  }
  #
  ## --- (3) --- "(n=1)A --> [k1] (m=1)R <==> [k2] [k3] (l=1)C" (n,m,l = 1,2,non-integer) ------
  #
  if (grepl("^\\(n=.*A --> \\[k1\\] \\(m=.*R <==> \\[k2\\] \\[k3\\] \\(l=.*C$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_03 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    pro = elementary.react) {
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      k3 <- kin.params$k3
      if (isFALSE(pro)) {
        alpha <- kin.params$alpha
        beta <- kin.params$beta
        gamma <- kin.params$gamma
      }
      ## initial conditions
      rate <- rep(0, times = 3)
      ## differential equations
      c_n <- stoichio.coeff[1]
      c_m <- stoichio.coeff[2]
      c_l <- stoichio.coeff[3]
      if (isTRUE(pro)) {
        rate[1] <- -k1 * c_n * (qvar["A"])^(c_n)
        rate[2] <- (k1 * c_m * (qvar["A"])^(c_n)) - (k2 * c_m * (qvar["R"])^(c_m)) +
          (k3 * c_m * (qvar["C"])^(c_l))
        rate[3] <- (k2 * c_l * (qvar["R"])^(c_m)) - (k3 * c_l * (qvar["C"])^(c_l))
      } else {
        rate[1] <- -k1 * c_n * (qvar["A"])^alpha
        rate[2] <- (k1 * c_m * (qvar["A"])^alpha) - (k2 * c_m * (qvar["R"])^beta) +
          (k3 * c_m * (qvar["C"])^gamma)
        rate[3] <- (k2 * c_l * (qvar["R"])^beta) - (k3 * c_l * (qvar["C"])^gamma)
      }
      ## derivative as a list
      return(list(rate))
    }
    ## initial (`0`) qvar (e.g. like concentration)
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
    if (isFALSE(elementary.react)) {
      alpha <- kin.params["alpha"]
      beta <- kin.params["beta"]
      gamma <- kin.params["gamma"]
      #
      ## solving `ordinary diff. equation(s)`
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m"),
                           stoichiom_coeff(model.react,coeff = "l")),
        func = react_rates_diff_03,
        parms = list(k1 = k1,
                     k2 = k2,
                     k3 = k3,
                     alpha = alpha,
                     beta = beta,
                     gamma = gamma),
        ...
      )
    } else {
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m"),
                           stoichiom_coeff(model.react,coeff = "l")),
        func = react_rates_diff_03,
        parms = list(k1 = k1,
                     k2 = k2,
                     k3 = k3),
        ...
      )
    }
    #
  }
  #
  ## ---- (4) ---- "(n=1)R <==> [k1] [k2] (m=1)B" (n,m = 1,2,non-integer) -------------
  #
  if (grepl("^\\(n=.*R <==> \\[k1\\] \\[k2\\] \\(m=.*B$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_04 <- function(t,
                                    qvar,
                                    kin.params,
                                    stiochio.coeff,
                                    pro = elementary.react) {
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (isFALSE(pro)) {
        alpha <- kin.params$alpha
        beta <- kin.params$beta
      }
      ## initial conditions
      rate <- rep(0, times = 2)
      ## differential equations
      c_n <- stoichio.coeff[1]
      c_m <- stoichio.coeff[2]
      if (isTRUE(pro)) {
        rate[1] <- (-k1 * c_n * (qvar["R"])^(c_n)) + (k2 * c_n * (qvar["B"])^(c_m))
        rate[2] <- (k1 * c_m * (qvar["R"])^(c_n)) - (k2 * c_m * (qvar["B"])^(c_m))
      } else{
        rate[1] <- (-k1 * c_n * (qvar["R"])^alpha) + (k2 * c_n * (qvar["B"])^beta)
        rate[2] <- (k1 * c_m * (qvar["R"])^alpha) - (k2 * c_m * (qvar["B"])^beta)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. like concentration)
    qvar0 <- c(
      R = unname(kin.params["qvar0R"]),
      B = unname(kin.params["qvar0B"])
    )
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    k2 <- kin.params["k2"]
    if (isFALSE(elementary.react)) {
      alpha <- kin.params["alpha"]
      beta <- kin.params["beta"]
      #
      ## solving `ordinary diff. equation(s)`
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m")),
        func = react_rates_diff_04,
        parms = list(k1 = k1,
                     k2 = k2,
                     alpha = alpha,
                     beta = beta),
        ...
      )
    } else {
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m")),
        func = react_rates_diff_04,
        parms = list(k1 = k1,
                     k2 = k2),
        ...
      )
    }
    #
  }
  #
  ## --- (5) --- "(n=1)A <==> [k1] [k2] (m=1)R" (n,m = 1,2,non-integer) ------------
  #
  if (grepl("^\\(n=.*A <==> \\[k1\\] \\[k2\\] \\(m=.*R$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_05 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    pro = elementary.react) {
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (isFALSE(pro)) {
        alpha <- kin.params$alpha
        beta <- kin.params$beta
      }
      ## initial conditions
      rate <- rep(0, times = 2)
      ## differential equations
      c_n <- stoichio.coeff[1]
      c_m <- stoichio.coeff[2]
      if (isTRUE(pro)) {
        rate[1] <- (-k1 * c_n * (qvar["A"])^(c_n)) + (k2 * c_n * (qvar["R"])^(c_m))
        rate[2] <- (k1 * c_m * (qvar["A"])^(c_n)) - (k2 * c_m * (qvar["R"])^(c_m))
      } else {
        rate[1] <- (-k1 * c_n * (qvar["A"])^alpha) + (k2 * c_n * (qvar["R"])^beta)
        rate[2] <- (k1 * c_m * (qvar["A"])^alpha) - (k2 * c_m * (qvar["R"])^beta)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. like concentration)
    qvar0 <- c(
      A = unname(kin.params["qvar0A"]),
      R = unname(kin.params["qvar0R"])
    )
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    k2 <- kin.params["k2"]
    if (isFALSE(elementary.react)) {
      alpha <- kin.params["alpha"]
      beta <- kin.params["beta"]
      #
      ## solving `ordinary diff. equation(s)`
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m")),
        func = react_rates_diff_05,
        parms = list(k1 = k1,
                     k2 = k2,
                     alpha = alpha,
                     beta = beta),
        ...
      )
    } else {
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m")),
        func = react_rates_diff_05,
        parms = list(k1 = k1,
                     k2 = k2),
        ...
      )
    }
    #
  }
  #
  ## --- (6) --- "(n=1)A + (m=1)B --> [k1] (l=1)R", (n,m,l = 1,2,non-integer) ----------
  #
  if (grepl("^\\(n=.*A \\+ \\(m=.*B --> \\[k1\\] \\(l=.*R$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_06 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    pro = elementary.react) {
      k1 <- kin.params$k1
      if (isFALSE(pro)) {
        alpha <- kin.params$alpha
        beta <- kin.params$beta
      }
      ## initial conditions
      rate <- rep(0, times = 3)
      ## differential equations
      c_n <- stoichio.coeff[1]
      c_m <- stoichio.coeff[2]
      c_l <- stoichio.coeff[3]
      if (isTRUE(pro)) {
        rate[1] <- -k1 * c_n * ((qvar["A"])^(c_n)) * ((qvar["B"])^(c_m))
        rate[2] <- -k1 * c_m * ((qvar["B"])^(c_m)) * ((qvar["A"])^(c_n))
        rate[3] <- k1 * c_l * ((qvar["A"])^(c_n)) * ((qvar["B"])^(c_m))
      } else {
        rate[1] <- -k1 * c_n * ((qvar["A"])^alpha) * ((qvar["B"])^beta)
        rate[2] <- -k1 * c_m * ((qvar["B"])^beta) * ((qvar["A"])^alpha)
        rate[3] <- k1 * c_l * ((qvar["A"])^alpha) * ((qvar["B"])^beta)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. like concentration)
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
      #
      ## solving `ordinary diff. equation(s)`
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m"),
                           stoichiom_coeff(model.react,coeff = "l")),
        func = react_rates_diff_06,
        parms = list(k1 = k1,
                     alpha = alpha,
                     beta = beta),
        ...
      )
    } else {
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m"),
                           stoichiom_coeff(model.react,coeff = "l")),
        func = react_rates_diff_06,
        parms = list(k1 = k1),
        ...
      )
    }
    #
  }
  #
  ## --- (7) --- "(n=1)R + (m=1)B --> [k1] (l=1)C", (n,m,l = 1,2,non-integer) -----------
  #
  if (grepl("^\\(n=.*R \\+ \\(m=.*B --> \\[k1\\] \\(l=.*C$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_07 <- function(t,
                                    qvar,
                                    kin.params,
                                    stoichio.coeff,
                                    pro = elementary.react) {
      k1 <- kin.params$k1
      if (isFALSE(pro)) {
        alpha <- kin.params$alpha
        beta <- kin.params$beta
      }
      ## initial conditions
      rate <- rep(0, times = 3)
      ## differential equations
      c_n <- stoichio.coeff[1]
      c_m <- stoichio.coeff[2]
      c_l <- stoichio.coeff[3]
      if (isTRUE(pro)) {
        rate[1] <- -k1 * c_n * ((qvar["R"])^(c_n)) * ((qvar["B"])^(c_m))
        rate[2] <- -k1 * c_m * ((qvar["B"])^(c_m)) * ((qvar["R"])^(c_n))
        rate[3] <- k1 * c_l * ((qvar["R"])^(c_n)) * ((qvar["B"])^(c_m))
      } else {
        rate[1] <- -k1 * c_n * ((qvar["R"])^alpha) * ((qvar["B"])^beta)
        rate[2] <- -k1 * c_m * ((qvar["B"])^beta) * ((qvar["R"])^alpha)
        rate[3] <- k1 * c_l * ((qvar["R"])^alpha) * ((qvar["B"])^beta)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. like concentration)
    qvar0 <- c(
      A = unname(kin.params["qvar0R"]),
      B = unname(kin.params["qvar0B"]),
      R = unname(kin.params["qvar0C"])
    )
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    if (isFALSE(elementary.react)) {
      alpha <- kin.params["alpha"]
      beta <- kin.params["beta"]
      #
      ## solving `ordinary diff. equation(s)`
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m"),
                           stoichiom_coeff(model.react,coeff = "l")),
        func = react_rates_diff_07,
        parms = list(k1 = k1,
                     alpha = alpha,
                     beta = beta),
        ...
      )
    } else {
      result <- deSolve::ode(
        y = qvar0,
        times = t,
        stoichio.coeff = c(stoichiom_coeff(model.react),
                           stoichiom_coeff(model.react,coeff = "m"),
                           stoichiom_coeff(model.react,coeff = "l")),
        func = react_rates_diff_07,
        parms = list(k1 = k1),
        ...
      )
    }
    #
  }
  #
  ## ======================== DATA AND KINETIC PLOTS ============================
  #
  if (grepl("^\\(n=.*R --> \\[k1\\] B$", model.react)) {
    #
    ## DATA processing
    ## conversion into data frame
    result.df <- data.frame(result)
    if (!is.null(data.expr)) {
      ## following operation required for the difference
      ## the same number of points for the model as well as for the expr.
      result.df <- result.df %>%
        dplyr::filter(.data$time %in% data.expr[[time.expr]])
    }
    ## the first col. is `time` and 2nd has to be renamed
    names(result.df)[2] <- "R"
    #
    if (!is.null(data.expr) & isTRUE(model.expr.diff)) {
      ## difference
      diff.model.expr <- data.expr[[qvar.expr]] - result.df[["R"]]
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
    if (!is.null(data.expr)) {
      result.df <- result.df %>%
        dplyr::filter(.data$time %in% data.expr[[time.expr]])
    }
    #
    if (!is.null(data.expr) & isTRUE(model.expr.diff)) {
      ## difference
      diff.model.expr <- data.expr[[qvar.expr]] - result.df[["R"]]
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
  ## ============================= ENTIRE PLOT & RESULTS ==============================
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
    scale_x_continuous(sec.axis = dup_axis(name = "", labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = "", labels = NULL)) +
    theme(plot.title = element_text(hjust = 0.5))
  #
  ## RESULTS
  if (isFALSE(model.expr.diff)) {
    return(list(df = result.df, plot = plotR))
  } else {
    return(diff.model.expr)
  }
  #
}
