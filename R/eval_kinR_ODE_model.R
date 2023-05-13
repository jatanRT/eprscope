#
#' Quantitative Kinetic Model Profiles by Numeric Solution of ODE for Visualization
#' and Fitting of the Radical Reactions.
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'   The primary aim of the function is to get an idea of theoretical quantitative (e.g. like
#'   concentration/amount/integral intensity) kinetic profile for various model reactions where
#'   the radical(s) ("R") are involved in the processes.
#'   Secondary purpose is to fit the experimental EPR spectral time series outputs, e.g. like integral
#'   intensity (area under the spectral curve) \emph{vs} time in order to gather the rate constants
#'   (\eqn{k}) of radical ("R") formation and/or decomposition. Quantitative kinetic profiles
#'   are not evaluated by integration of the kinetic equations, however by numeric solution of the Ordinary
#'   Differential Equations (\href{http://desolve.r-forge.r-project.org/index.html}{ODE}, see also
#'   \pkg{deSolve} package). Therefore, higher number of models is available than for integrated
#'   differential equations because their solution is quite often highly demanding.
#'   The function is inspired by
#'   \href{https://www.r-bloggers.com/2013/06/learning-r-parameter-fitting-for-models-involving-differential-equations/}{R-bloggers
#'   article}.
#'   The most applied kinetic models for radical reactions in EPR spectroscopy are summarized below
#'   (see also \code{model.react} function argument).
#'   \tabular{ccl}{
#'   -------------------- \tab | \tab -------------------- \cr
#'   \strong{`model.react`} \tab | \tab \strong{Description} \cr
#'   -------------------- \tab | \tab -------------------- \cr
#'   \code{"(x=1)R --> [k1] B"} \tab | \tab Basic irreversible forward reaction,
#'   e.g. like irrev. dimerization (if \code{"(x=2)"}). \cr
#'   \code{"(x=1)A --> [k1] R"} \tab | \tab Basic irreversible radical formation,
#'   e.g. like irrev. \eqn{\text{e}^-} transfer. \cr
#'   \code{"(x=1)A --> [k1] (x=1)R --> [k2] C"} \tab | \tab Consecutive reactions,
#'   e.g. like comproportionation (or homogeneous
#'   \eqn{\text{e}^-} transfer, for \code{"(x=2)"}) + follow up irrev. dimerization. \cr
#'   \code{"(x=1)R <==> [k1] [k2] B"} \tab | \tab Basic reversible radical quenching,
#'   e.g. like rev. (\eqn{\pi-\pi} dimerization). \cr
#'   \code{"(x=1)A <==> [k1] [k2] (x=1)R"} \tab | \tab Basic reversible radical formation,
#'   e.g. from rev.
#'   comproportionation/disproportionation (\eqn{\text{A}^{++} + \text{A}^0 \xrightleftharpoons ~ 2\text{R}^{.+}},
#'   for \code{"(x=2)"}). \cr
#'   \code{"A [k1] <-- (x=1)R --> [k2] B"} \tab | \tab Basic parallel reactions,
#'   e.g. like radical decomposition and isomerization or two radical diffusion forming different product
#'   than without diffusion like recombination (\code{"(x=2)"}). \cr
#'   \code{"(x=1)A + (y=1)B --> [k1] R"} \tab | \tab Radical formation by chemical reaction like oxidation,
#'   reduction or spin trapping (transient radical is not visible within the common EPR time scale). \cr
#'   -------------------- \tab | \tab -------------------- \cr
#'   }
#'
#'
#' @param model.react Character string denoting a specific radical (\code{"R"}) reaction related to
#'   changes in integral intensities in EPR spectral time series. Arrow shows direction of the reaction
#'   (\code{"-->"}, irreversible or \code{"<==>", reversible}). Rate constants are indicated by square
#'   brackets after the arrows. Such notation inherited from
#'   \href{https://www.ctan.org/pkg/mhchem?lang=en}{\eqn{\LaTeX} `mhchem` package}.
#'   Following reaction schemes are the most common used to describe the integral intensity and/or radical
#'   concentration/amount changes during the EPR time series experiment.
#'   \tabular{lcl}{
#'   ------------------------ \tab | \tab -------------------- \cr
#'   \strong{Reaction Scheme} \tab | \tab \strong{`model.react`} \cr
#'   ------------------------ \tab | \tab -------------------- \cr
#'   \eqn{(x=1)\text{R} \xrightarrow{k_1} \text{B}} \tab | \tab \code{"(x=1)R --> [k1] B"} \cr
#'    \eqn{(x=1)\text{A} \xrightarrow{k_1} \text{R}} \tab | \tab \code{"(x=1)A --> [k1] R"} \cr
#'    \eqn{(x=1)\text{A} \xrightarrow{k_1} (x=1)\text{R} \xrightarrow{k_2} \text{C}} \tab | \tab
#'    \code{"(x=1)A --> [k1] (x=1)R --> [k2] C"} \cr
#'    \eqn{(x=1)\text{R} \xrightleftharpoons[k_2]{k_1} \text{B}} \tab | \tab
#'    \code{"(x=1)R <==> [k1] [k2] B"} \cr
#'    \eqn{(x=1)\text{A} \xrightleftharpoons[k_2]{k_1} (x=1)\text{R}} \tab | \tab
#'    \code{"(x=1)A <==> [k1] [k2] (x=1)R"} \cr
#'    \eqn{\text{A} \xleftarrow{k_1} (x=1)\text{R} \xrightarrow{k_2} \text{B}} \tab | \tab
#'    \code{"A [k1] <-- (x=1)R --> [k2] B"} \cr
#'    \eqn{(x=1)\text{A} + (y=1)\text{B} \xrightarrow{k_1} \text{R}} \tab | \tab
#'    \code{"(x=1)A + (y=1)B --> [k1] R"} \cr
#'    ------------------------ \tab | \tab -------------------- \cr
#'   }
#'   Couple of examples are also given in the description. The function is relatively flexible and enables
#'   later addition of any other reaction schemes describing the EPR time series experiments
#'   (YOU MAY ASK DEVELOPER(S) via `github issue`). The stoichiometric coefficient (e.g. like \code{"(x=1)"}
#'   or \code{"(y=1)"}) can be varied within the \code{model.react} character string.
#'   Defined/Allowed values are \code{"(x=1)"},  \code{"(x=2)"} and \code{"(x=n)"}.
#'   \strong{The latest NOTATION (\code{"(x=n)"}) is USED ONLY FOR FITTING},
#'   see \code{\link{eval_kinR_EPR_modelFit}} or the examples below. The reason is to take into account
#'   a possible non-integer stoichiometric coefficients. If equal coefficients are given for both sides
#'   of (partial) reaction they have to be changed equally (e.g. like \code{"(x=2)A <==> [k1] [k2] (x=2)R"}).
#'   Otherwise, the stoichiometric coefficients may be varied independently
#'   (e.g. like in \code{"(x=1)A + (y=2)B --> [k1] R"}). \strong{Default}: \code{model.react = "(x=1)R --> [k1] B"}.
#' @param model.expr.diff Logical, difference between the integral intensities/areas under the curves calculated
#'   using the experimental data and those generated by the model. By \strong{default} the argument
#'   is \strong{FALSE} and it is ONLY ACTIVATED (\code{model.expr.diff = TRUE}) IN CASE KINETIC MODEL
#'   FITTING PROCEDURE (see also \code{\link{eval_kinR_EPR_modelFit}} or examples below).
#' @param kin.params Named numeric vector containing rate constants as well as initial radical
#'   or other reactant/product concentration/integral intensities/areas...etc. Therefore, a general
#'   \code{qvar} (\strong{q}uantitative \strong{var}iable) was defined which may actually reflect
#'   all the mentioned quantities. \strong{Default}: \code{kin.params = c(k1 = 0.001,qvar0R = 0.02)}.
#'   The initial values are denoted as \code{qvar0X} (e.g. qvar0R for radical or qvar0A for reactant).
#'   The components of \code{kin.params} numeric vector depend on \code{model.react} like summarized in the
#'   following table =>
#'   \tabular{ccl}{
#'   -------------------- \tab | \tab ------------------- \cr
#'   \strong{`model.react`} \tab | \tab \strong{Essential `kin.params` components} \cr
#'   -------------------- \tab | \tab ------------------- \cr
#'   \code{"(x=1)R --> [k1] B"} \tab | \tab \code{k1}, \code{qvar0R}, (\code{n}) \cr
#'   \code{"(x=1)A --> [k1] R"} \tab | \tab \code{k1}, \code{qvar0A}, \code{qvar0R}, (\code{n}) \cr
#'   \code{"(x=1)A --> [k1] (x=1)R --> [k2] C"} \tab | \tab \code{k1}, \code{k2}, \code{qvar0A},
#'   \code{qvar0R}, \code{qvar0C}, (\code{n}) \cr
#'   \code{"(x=1)R <==> [k1] [k2] B"} \tab | \tab \code{k1}, \code{k2}, \code{qvar0R},
#'   \code{qvar0B}, (\code{n}) \cr
#'   \code{"(x=1)A <==> [k1] [k2] (x=1)R"} \tab | \tab \code{k1}, \code{k2}, \code{qvar0A},
#'   \code{qvar0R}, (\code{n}) \cr
#'   \code{"A [k1] <-- (x=1)R --> [k2] B"} \tab | \tab \code{k1}, \code{k2}, \code{qvar0R}, (\code{n}) \cr
#'   \code{"(x=1)A + (y=1)B --> [k1] R"} \tab | \tab \code{k1}, \code{qvar0A}, \code{qvar0B},
#'   \code{qvar0R}, (\code{n}) \cr
#'   -------------------- \tab | \tab ------------------- \cr
#'   }
#' @param data.expr R data frame containing the integral intensities/areas under the curves calculated
#'   using the \strong{experimental data} as well as time column. These two essential columns
#'   are described by character strings like bellow (see the last two arguments).
#'   The \code{data.expr} MUST BE USED ONLY IN CASE IF THE EXPERIMENTAL TIME HAS TO BE INCLUDED
#'   IN THE KINETIC MODEL (e.g. also for THE FITTING of EXPR. DATA BY THE KINETIC MODEL).
#'   \strong{Default}: \code{data.expr = NULL}.
#' @param time.expr.series Character string pointing to `time` \strong{column name} in the original
#'   \code{data.expr} data frame. \strong{Default}: \code{time.expr.series = NULL} (in case of experimental
#'   data aren't taken into account).
#' @param qvar.expr Character string pointing to `qvar` \strong{column name} in the original
#'   \code{data.expr} data frame. \strong{Default}: \code{qvar.expr = NULL} (in case of experimental
#'   data aren't taken into account).
#'
#'
#' @return If the function is not used for fitting of the experimental and processed data, the result
#'   is \code{list}. It consists of data frame (\code{df}) and \code{plot} containing \code{time}
#'   (within the plot presented as abscissa) column and `qvar`, quantitative variable, columns
#'   (or points in the plot => \eqn{y}-axis) corresponding to quantities of different relevant species
#'   denoted as \code{"R"}, \code{"A"}, \code{"B"}, ... etc. Applying function for the fitting procedure
#'   requires \code{model.expr.diff = TRUE} and therefore the result is difference between
#'   the integral intensities/areas under the curves calculated using the experimental data
#'   and those generated by the model.
#'
#'
#' @examples
#' ## irreversible dimerization quantitative kinetic profile
#' ## table (df) with first 10 observations/rows
#' kin.test.01 <-
#'  eval_kinR_ODE_model(model.react = "(x=2)R --> [k1] B",
#'                      kin.params = c(k1 = 0.012,
#'                                     qvar0R = 0.08))
#' head(kin.test.01$df,n = 10)
#' #
#' ## consecutive reactions and preesenting plot
#' kin.test.02 <-
#'  eval_kinR_ODE_model(model.react = "(x=1)A --> [k1] (x=1)R --> [k2] C",
#'                      kin.params = c(k1 = 0.01,
#'                                     k2 = 0.015,
#'                                     qvar0A = 0.02,
#'                                     qvar0R = 0.002,
#'                                     qvar0C = 0))
#' kin.test.02$plot
#' #
#' \dontrun{
#' ## taking onto account the experimental time series
#' eval_kinR_ODE_model(model.react = "(x=2)A --> [k1] R",
#'                     model.expr.diff = FALSE,
#'                     kin.params = c(k1 = 0.005,
#'                                    qvar0A = 0.05,
#'                                    qvar0R = 0),
#'                     data.expr = data.spectra.integ,
#'                     time.expr.series = "time_s")
#' #
#' ## using `eval_kinR_ODE_model` function for fitting
#' ## of the experimental data in the previous case
#' minpack.lm::nls.lm(par = c(k1 = 0.005,
#'                            qvar0A = 0.05,
#'                            qvar0R = 0),
#'                    fn = eval_kinR_ODE_model,
#'                    model.react = "(x=2)A --> [k1] R",
#'                    model.expr.diff = TRUE,
#'                    data.expr = data.spectra.integ,
#'                    time.expr.series = "time_s",
#'                    qvar.expr = "Area")
#' }
#'
#'
#' @export
#'
#'
#' @importFrom deSolve ode
#' @importFrom ggplot2 geom_point
eval_kinR_ODE_model <- function(model.react = "(x=1)R --> [k1] B", ## for x = 1,2 or n
                                model.expr.diff = FALSE,
                                kin.params = c(
                                  k1 = 0.001,
                                  qvar0R = 0.02
                                ),
                                data.expr = NULL,
                                time.expr.series = NULL,
                                qvar.expr = NULL) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  time <- NULL
  #
  ## Data definition
  if (!is.null(data.expr)) {
    if (is.null(time.expr.series) || is.null(qvar.expr)) {
      stop(" The time series of the experimental data (`time.expr.series`)\n
           or the corresponding quantitative variable (`qvar.expr`)\n
           is not specified. Please, define ! ")
    } else {
      time.expr <- data.expr[[time.expr.series]]
      final.time <- max(data.expr[[time.expr.series]])
    }
  }
  #
  ## Time.series definition
  if (is.null(data.expr)) {
    final.time <- 1600 ## theoretical final time in seconds
    t <- seq(0, final.time, 2)
  } else {
    t <- c(seq(0, final.time, 2), time.expr)
    t <- sort(unique(t))
  }
  #
  ## function to extract stoichiometric coefficient from `model.react`
  ## expression => extracting the `(x=...)` string from `model.react`
  stoichiom_coeff <- function(expression, coeff = "x") {
    if (coeff == "x") {
      xy.string <- stringr::str_extract(expression, pattern = "\\(x=[[:digit:]]\\)|\\(x=n\\)")
    }
    if (coeff == "y") {
      xy.string <- stringr::str_extract(expression, pattern = "\\(y=[[:digit:]]\\)|\\(y=m\\)")
    }
    xy.string.cond <- grepl("[[:digit:]]", xy.string)
    if (xy.string.cond) {
      model.react.xy <- stringr::str_extract(xy.string, pattern = "[[:digit:]]")
      model.react.xy <- as.numeric(model.react.xy)
    } else {
      model.react.xy <- stringr::str_extract(xy.string, pattern = "n|m")
    }
    #
    return(model.react.xy)
    #
  }
  #
  if (grepl("^\\(x=.*R --> \\[k1\\] B$", model.react)) {
    #
    ## functions for derivative solution of kinetic equation
    react_rates_diff_01 <- function(t, qvar, kin.params, c_x) {
      ## t <- time vector
      ## qvar <- concentration, double EPR integral, (`qvar` \equiv "quantitative varialble")
      ## number of radicals...etc. vector
      ## kin.rate.params <- rate constant and coeffs.
      k1 <- kin.params$k1
      if (c_x == "n") {
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0, times = 1)
      ## differential equations
      if (inherits(c_x, "numeric")) {
        rate[1] <- -k1 * (qvar)^(c_x)
      }
      if (c_x == "n") {
        rate[1] <- -k1 * (qvar)^n
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
    if (stoichiom_coeff(model.react) == "n") {
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n") {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_01,
        parms = list(k1 = k1, n = n)
      )
    }
    if (inherits(stoichiom_coeff(model.react), "numeric")) {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_01,
        parms = list(k1 = k1)
      )
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result)
    if (!is.null(data.expr)) {
      result.df <- result.df %>%
        dplyr::filter(.data$time %in% data.expr[[time.expr.series]])
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
  }
  if (grepl("^\\(x=.*A --> \\[k1\\] R$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_02 <- function(t, qvar, kin.params, c_x) {
      k1 <- kin.params$k1
      if (c_x == "n") {
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0, times = 2)
      ## differential equations
      if (inherits(c_x, "numeric")) {
        rate[1] <- -k1 * (qvar["A"])^(c_x)
        rate[2] <- k1 * (qvar["A"])^(c_x)
      }
      if (c_x == "n") {
        rate[1] <- -k1 * (qvar["A"])^n
        rate[2] <- k1 * (qvar["A"])^n
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
    if (stoichiom_coeff(model.react) == "n") {
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n") {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_02,
        parms = list(k1 = k1, n = n)
      )
    }
    if (inherits(stoichiom_coeff(model.react), "numeric")) {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_02,
        parms = list(k1 = k1)
      )
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result)
    if (!is.null(data.expr)) {
      result.df <- result.df %>%
        dplyr::filter(.data$time %in% data.expr[[time.expr.series]])
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
  }
  if (grepl("^\\(x=.*A --> \\[k1\\] \\(x=.*R --> \\[k2\\] C$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_03 <- function(t, qvar, kin.params, c_x) {
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (c_x == "n") {
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0, times = 3)
      ## differential equations
      if (inherits(c_x, "numeric")) {
        rate[1] <- -k1 * (qvar["A"])^(c_x)
        rate[2] <- k1 * (qvar["A"])^(c_x) - k2 * (qvar["R"])^(c_x)
        rate[3] <- k2 * (qvar["R"])^(c_x)
      }
      if (c_x == "n") {
        rate[1] <- -k1 * (qvar["A"])^n
        rate[2] <- k1 * (qvar["A"])^n - k2 * (qvar["R"])^n
        rate[3] <- k2 * (qvar["R"])^n
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
    if (stoichiom_coeff(model.react) == "n") {
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n") {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_03,
        parms = list(k1 = k1, k2 = k2, n = n)
      )
    }
    if (inherits(stoichiom_coeff(model.react), "numeric")) {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_03,
        parms = list(k1 = k1, k2 = k2)
      )
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result)
    if (!is.null(data.expr)) {
      result.df <- result.df %>%
        dplyr::filter(.data$time %in% data.expr[[time.expr.series]])
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
  }
  if (grepl("^\\(x=.*R <==> \\[k1\\] \\[k2\\] B$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_04 <- function(t, qvar, kin.params, c_x) {
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (c_x == "n") {
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0, times = 2)
      ## differential equations
      if (inherits(c_x, "numeric")) {
        rate[1] <- -k1 * (qvar["R"])^(c_x) + k2 * qvar["B"]
        rate[2] <- k1 * (qvar["R"])^(c_x) - k2 * qvar["B"]
      }
      if (c_x == "n") {
        rate[1] <- -k1 * (qvar["R"])^n + k2 * qvar["B"]
        rate[2] <- k1 * (qvar["R"])^n - k2 * qvar["B"]
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
    if (stoichiom_coeff(model.react) == "n") {
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n") {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_04,
        parms = list(k1 = k1, k2 = k2, n = n)
      )
    }
    if (inherits(stoichiom_coeff(model.react), "numeric")) {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_04,
        parms = list(k1 = k1, k2 = k2)
      )
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result)
    if (!is.null(data.expr)) {
      result.df <- result.df %>%
        dplyr::filter(.data$time %in% data.expr[[time.expr.series]])
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
  }
  if (grepl("^\\(x=.*A <==> \\[k1\\] \\[k2\\] \\(x=.*R$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_05 <- function(t, qvar, kin.params, c_x) {
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (c_x == "n") {
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0, times = 2)
      ## differential equations
      if (inherits(c_x, "numeric")) {
        rate[1] <- -k1 * (qvar["A"])^(c_x) + k2 * (qvar["R"])^(c_x)
        rate[2] <- k1 * (qvar["A"])^(c_x) - k2 * (qvar["R"])^(c_x)
      }
      if (c_x == "n") {
        rate[1] <- -k1 * (qvar["A"])^n + k2 * (qvar["R"])^n
        rate[2] <- k1 * (qvar["A"])^n - k2 * (qvar["R"])^n
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
    if (stoichiom_coeff(model.react) == "n") {
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n") {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_05,
        parms = list(k1 = k1, k2 = k2, n = n)
      )
    }
    if (inherits(stoichiom_coeff(model.react), "numeric")) {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_05,
        parms = list(k1 = k1, k2 = k2)
      )
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result)
    if (!is.null(data.expr)) {
      result.df <- result.df %>%
        dplyr::filter(.data$time %in% data.expr[[time.expr.series]])
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
  }
  if (grepl("^A \\[k1\\] <-- \\(x=.*R --> \\[k2\\] B$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_06 <- function(t, qvar, kin.params, c_x) {
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (c_x == "n") {
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0, times = 1)
      ## differential equations
      if (inherits(c_x, "numeric")) {
        rate[1] <- -k1 * (qvar)^(c_x) - k2 * (qvar)^(c_x)
      }
      if (c_x == "n") {
        rate[1] <- -k1 * (qvar)^n - k2 * (qvar)^n
      }
      ## derivative as a list
      return(list(rate))
    }
    # initial (`0`) qvar (e.g. like concentration)
    qvar0 <- c(R = unname(kin.params["qvar0R"]))
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    k2 <- kin.params["k2"]
    if (stoichiom_coeff(model.react) == "n") {
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n") {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_06,
        parms = list(k1 = k1, k2 = k2, n = n)
      )
    }
    if (inherits(stoichiom_coeff(model.react), "numeric")) {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        func = react_rates_diff_06,
        parms = list(k1 = k1, k2 = k2)
      )
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result)
    if (!is.null(data.expr)) {
      result.df <- result.df %>%
        dplyr::filter(.data$time %in% data.expr[[time.expr.series]])
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
  }
  if (grepl("^\\(x=.*A \\+ \\(y=.*B --> \\[k1\\] R$", model.react)) {
    ## functions for derivative solution of kinetic equation
    react_rates_diff_07 <- function(t, qvar, kin.params, c_x, c_y) {
      k1 <- kin.params$k1
      if (c_x == "n" & inherits(c_y, "numeric")) {
        n <- kin.params$n
      }
      if (inherits(c_x, "numeric") & c_y == "m") {
        m <- kin.params$m
      }
      if (c_x == "n" & c_y == "m") {
        n <- kin.params$n
        m <- kin.params$m
      }
      ## initial conditions
      rate <- rep(0, times = 3)
      ## differential equations
      if (inherits(c_x, "numeric") & inherits(c_y, "numeric")) {
        rate[1] <- -k1 * ((qvar["A"])^(c_x)) * ((qvar["B"])^(c_y))
        rate[2] <- -k1 * ((qvar["B"])^(c_y)) * ((qvar["A"])^(c_x))
        rate[3] <- k1 * ((qvar["A"])^(c_x)) * ((qvar["B"])^(c_y))
      }
      if (c_x == "n" & c_y == "m") {
        rate[1] <- -k1 * ((qvar["A"])^n) * ((qvar["B"])^m)
        rate[2] <- -k1 * ((qvar["B"])^m) * ((qvar["A"])^n)
        rate[3] <- k1 * ((qvar["A"])^n) * ((qvar["B"])^m)
      }
      if (c_x == "n" & inherits(c_y, "numeric")) {
        rate[1] <- -k1 * ((qvar["A"])^n) * ((qvar["B"])^(c_y))
        rate[2] <- -k1 * ((qvar["B"])^(c_y)) * ((qvar["A"])^n)
        rate[3] <- k1 * ((qvar["A"])^n) * ((qvar["B"])^(c_y))
      }
      if (inherits(c_x, "numeric") & c_y == "m") {
        rate[1] <- -k1 * ((qvar["A"])^(c_x)) * ((qvar["B"])^m)
        rate[2] <- -k1 * ((qvar["B"])^m) * ((qvar["A"])^(c_x))
        rate[3] <- k1 * ((qvar["A"])^(c_x)) * ((qvar["B"])^m)
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
    if (stoichiom_coeff(model.react) == "n" &
      inherits(stoichiom_coeff(model.react, coeff = "y"), "numeric")) {
      n <- kin.params["n"]
    }
    if (inherits(stoichiom_coeff(model.react), "numeric") &
      stoichiom_coeff(model.react, coeff = "y") == "m") {
      m <- kin.params["m"]
    }
    if (stoichiom_coeff(model.react) == "n" &
      stoichiom_coeff(model.react, coeff = "y") == "m") {
      n <- kin.params["n"]
      m <- kin.params["m"]
    }
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n" &
      inherits(stoichiom_coeff(model.react, coeff = "y"), "numeric")) {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        c_y = stoichiom_coeff(model.react, coeff = "y"),
        func = react_rates_diff_07,
        parms = list(k1 = k1, n = n)
      )
    }
    if (inherits(stoichiom_coeff(model.react), "numeric") &
      stoichiom_coeff(model.react, coeff = "y") == "m") {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        c_y = stoichiom_coeff(model.react, coeff = "y"),
        func = react_rates_diff_07,
        parms = list(k1 = k1, m = m)
      )
    }
    if (stoichiom_coeff(model.react) == "n" &
      stoichiom_coeff(model.react, coeff = "y") == "m") {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        c_y = stoichiom_coeff(model.react, coeff = "y"),
        func = react_rates_diff_07,
        parms = list(k1 = k1, n = n, m = m)
      )
    }
    if (inherits(stoichiom_coeff(model.react), "numeric") &
      inherits(stoichiom_coeff(model.react, coeff = "y"), "numeric")) {
      result <- deSolve::ode(
        y = qvar0, times = t,
        c_x = stoichiom_coeff(model.react),
        c_y = stoichiom_coeff(model.react, coeff = "y"),
        func = react_rates_diff_07,
        parms = list(k1 = k1)
      )
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result)
    if (!is.null(data.expr)) {
      result.df <- result.df %>%
        dplyr::filter(.data$time %in% data.expr[[time.expr.series]])
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
  }
  #
  ## kinetics-behavior plots
  if (grepl("^\\(x=.*R --> \\[k1\\] B$", model.react) ||
    grepl("^A \\[k1\\] <-- \\(x=.*R --> \\[k2\\] B$", model.react)) {
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
  ## Caption character vector
  caption.char.vec <- mapply(function(i, j) paste0(i, " = ", j), names(kin.params), kin.params)
  caption.char.vec <- paste(unname(caption.char.vec), collapse = ", ")
  ## the entire plot (incl. scheme and parameters)
  plotR <- plot.base +
    labs(
      title = model.react,
      caption = caption.char.vec,
      x = bquote(italic(Time) ~ ~"(" ~ s ~ ")"),
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
