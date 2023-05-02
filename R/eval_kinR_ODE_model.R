#
#' Radical Kinetic Models Solved by Ordinary Differential Equations (ODE) for Visualization and Fitting
#'
#'
#' @description
#'   A short description ...
#'   \tabular{lcl}{
#'   \strong{Reaction Scheme} \tab | \tab \strong{model.react} \cr
#'   ------------------------ \tab | \tab -------------------- \cr
#'   \eqn{(x=1)\text{R} \xrightarrow{k_1} \text{B}} \tab | \tab \code{"(x=1)R --> [k1] B"} \cr
#'    \eqn{(x=1)\text{A} \xrightarrow{k_1} \text{R}} \tab | \tab \code{"(x=1)A --> [k1] R"} \cr
#'    \eqn{(x=1)\text{A} \xrightarrow{k_1} (x=1)\text{R} \xrightarrow{k_2} \text{C}} \tab | \tab
#'    \code{"(x=1)A --> [k1] (x=1)R --> [k2] C"} \cr
#'    \eqn{(x=1)\text{R} \xrightleftharpoons[k_2]{k_1} \text{B}} \tab | \tab \code{"(x=1)R <==> [k1] [k2] B"} \cr
#'   }
#'
#'   Additional description ...
#'   \tabular{ccl}{
#'   \strong{model.react} \tab | \tab \strong{Description} \cr
#'   -------------------- \tab | \tab -------------------- \cr
#'   \code{"(x=1)R --> [k1] B"} \tab | \tab Blah Blah Blah \cr
#'   \code{"(x=1)A --> [k1] R"} \tab | \tab Blah Blah Blah \cr
#'   \code{"(x=1)A --> [k1] (x=1)R --> [k2] C"} \tab | \tab Blah Blah Blah \cr
#'   \code{"(x=1)R <==> [k1] [k2] B"} \tab | \tab Blah Blah Blah \cr
#'   }
#'
#'   More additional description ...
#'   \tabular{ccl}{
#'   \strong{model.react} \tab | \tab \strong{kin.params} \cr
#'   -------------------- \tab | \tab ------------------- \cr
#'   \code{"(x=1)R --> [k1] B"} \tab | \tab \code{k1}, \code{qvar0R}, (\code{n}) \cr
#'   \code{"(x=1)A --> [k1] R"} \tab | \tab \code{k1}, \code{qvar0A}, \code{qvar0R}, (\code{n}) \cr
#'   \code{"(x=1)A --> [k1] (x=1)R --> [k2] C"} \tab | \tab \code{k1}, \code{k2}, \code{qvar0A}, \code{qvar0R}, \code{qvar0C},
#'   (\code{n}) \cr
#'   \code{"(x=1)R <==> [k1] [k2] B"} \tab | \tab \code{k1}, \code{k2}, \code{qvar0R}, \code{qvar0B}, (\code{n}) \cr
#'   }
#'
#'
#' @param model.react parameter/argument tbc
#' @param model.expr.diff parmeter/argument tbc
#' @param kin.params parameter/argument tbc
#' @param data.expr parameter/argument tbc
#' @param time.expr.series parameter/argument tbc
#' @param qvar.expr parameter/argument tbc
#'
#'
#' @return tbc
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
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
                                kin.params = c(k1 = 0.001,
                                               qvar0R = 0.02),
                                data.expr = NULL,
                                time.expr.series = NULL,
                                qvar.expr = NULL){
  #
  ## 'Temporary' processing variables
  . <- NULL
  time <- NULL
  #
  ## Data definition
  if (!is.null(data.expr)){
    if (is.null(time.expr.series) || is.null(qvar.expr)){
      stop(" The time series of the experimental data (`time.expr.series`)\n
           or the corresponding quantitative variable (`qvar.expr`)\n
           is not specified. Please, define ! ")
    } else{
      time.expr <- data.expr[[time.expr.series]]
      final.time <- max(data.expr[[time.expr.series]])
    }
  }
  #
  ## Time.series definition
  if (is.null(data.expr)){
    final.time <- 1600 ## theoretical final time in seconds
    t <- seq(0,final.time,2)
  } else{
    t <- c(seq(0,final.time,2),time.expr)
    t <- sort(unique(t))
  }
  #
  ## function to extract stoichiometric coefficient from `model.react`
  ## expression => extracting the `(x=...)` string from `model.react`
  stoichiom_coeff <- function(expression,coeff = "x"){
    if (coeff == "x"){
      xy.string <- stringr::str_extract(expression,pattern = "\\(x=[[:digit:]]\\)|\\(x=n\\)")
    }
    if (coeff == "y"){
      xy.string <- stringr::str_extract(expression,pattern = "\\(y=[[:digit:]]\\)|\\(y=m\\)")
    }
    xy.string.cond <- grepl("[[:digit:]]",xy.string)
    if (xy.string.cond){
      model.react.xy <- stringr::str_extract(xy.string,pattern = "[[:digit:]]")
      model.react.xy <- as.numeric(model.react.xy)
    } else{
      model.react.xy <- stringr::str_extract(xy.string,pattern = "n|m")
    }
    #
    return(model.react.xy)
    #
  }
  #
  if (model.react == "(x=1)R --> [k1] B"){
    #
    ## functions for derivative solution of kinetic equation
    react_rates_diff_01 <- function(t,qvar,kin.params,c_x){
      ## t <- time vector
      ## qvar <- concentration, double EPR integral, (`qvar` \equiv "quantitative varialble")
      ## number of radicals...etc. vector
      ## kin.rate.params <- rate constant and coeffs.
      k1 <- kin.params$k1
      if (c_x == "n"){
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0,times = 1)
      ## differential equations
      if (inherits(c_x,"numeric")){
        rate[1] <- - k1*(qvar)^(c_x)
      }
      if (c_x == "n"){
        rate[1] <- - k1*(qvar)^n
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
    if (stoichiom_coeff(model.react) == "n"){
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n"){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_01,
                             parms = list(k1 = k1,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_01,
                             parms = list(k1 = k1))
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result) %>%
      `if`(!is.null(data.expr),
           dplyr::filter(time %in% data.expr[[time.expr.series]]), .)
    ## the first col. is `time` and 2nd has to be renamed
    names(result.df)[2] <- "R"
    #
    if (!is.null(data.expr) & isTRUE(model.expr.diff)){
      ## difference
      diff.model.expr <- data.expr[[qvar.expr]] - result.df[["R"]]
    }
    #
    ## data frame for plotting
    result.df.plot <- result.df
  }
  if (model.react == "(x=1)A --> [k1] R"){
    ## functions for derivative solution of kinetic equation
    react_rates_diff_02 <- function(t,qvar,kin.params,c_x){
      k1 <- kin.params$k1
      if (c_x == "n"){
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0,times = 2)
      ## differential equations
      if (inherits(c_x,"numeric")){
        rate[1] <- - k1*(qvar["A"])^(c_x)
        rate[2] <- k1*(qvar["A"])^(c_x)
      }
      if (c_x == "n"){
        rate[1] <- - k1*(qvar["A"])^n
        rate[2] <- k1*(qvar["A"])^n
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. like concentration)
    qvar0 <- c(A = unname(kin.params["qvar0A"]),
               R = unname(kin.params["qvar0R"]))
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    if (stoichiom_coeff(model.react) == "n"){
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n"){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_02,
                             parms = list(k1 = k1,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_02,
                             parms = list(k1 = k1))
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result) %>%
      `if`(!is.null(data.expr),
           dplyr::filter(time %in% data.expr[[time.expr.series]]), .)
    #
    if (!is.null(data.expr) & isTRUE(model.expr.diff)){
      ## difference
      diff.model.expr <- data.expr[[qvar.expr]] - result.df[["R"]]
    }
    #
    ## data frame for plotting
    result.df.plot <- result.df %>%
      tidyr::pivot_longer(!time,names_to = "Species",values_to = "qvar") %>%
      dplyr::arrange(time)
  }
  if (model.react == "(x=1)A --> [k1] (x=1)R --> [k2] C"){
    ## functions for derivative solution of kinetic equation
    react_rates_diff_03 <- function(t,qvar,kin.params,c_x){
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (c_x == "n"){
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0,times = 3)
      ## differential equations
      if (inherits(c_x,"numeric")){
        rate[1] <- - k1*(qvar["A"])^(c_x)
        rate[2] <- k1*(qvar["A"])^(c_x) - k2*(qvar["R"])^(c_x)
        rate[3] <- k2*(qvar["R"])^(c_x)
      }
      if (c_x == "n"){
        rate[1] <- - k1*(qvar["A"])^n
        rate[2] <- k1*(qvar["A"])^n - k2*(qvar["R"])^n
        rate[3] <- k2*(qvar["R"])^n
      }
      ## derivative as a list
      return(list(rate))
    }
    ## initial (`0`) qvar (e.g. like concentration)
    qvar0 <- c(A = unname(kin.params["qvar0A"]),
               R = unname(kin.params["qvar0R"]),
               C = unname(kin.params["qvar0C"]))
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    k2 <- kin.params["k2"]
    if (stoichiom_coeff(model.react) == "n"){
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n"){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_03,
                             parms = list(k1 = k1,k2 = k2,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_03,
                             parms = list(k1 = k1,k2 = k2))
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result) %>%
      `if`(!is.null(data.expr),
           dplyr::filter(time %in% data.expr[[time.expr.series]]), .)
    #
    if (!is.null(data.expr) & isTRUE(model.expr.diff)){
      ## difference
      diff.model.expr <- data.expr[[qvar.expr]] - result.df[["R"]]
    }
    #
    ## data frame for plotting
    result.df.plot <- result.df %>%
      tidyr::pivot_longer(!time,names_to = "Species",values_to = "qvar") %>%
      dplyr::arrange(time)
  }
  if (model.react == "(x=1)R <==> [k1] [k2] B"){
    ## functions for derivative solution of kinetic equation
    react_rates_diff_04 <- function(t,qvar,kin.params,c_x){
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (c_x == "n"){
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0,times = 2)
      ## differential equations
      if (inherits(c_x,"numeric")){
        rate[1] <- - k1*(qvar["R"])^(c_x) + k2*qvar["B"]
        rate[2] <- k1*(qvar["R"])^(c_x) - k2*qvar["B"]
      }
      if (c_x == "n"){
        rate[1] <- - k1*(qvar["R"])^n + k2*qvar["B"]
        rate[2] <- k1*(qvar["R"])^n - k2*qvar["B"]
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. like concentration)
    qvar0 <- c(R = unname(kin.params["qvar0R"]),
               B = unname(kin.params["qvar0B"]))
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    k2 <- kin.params["k2"]
    if (stoichiom_coeff(model.react) == "n"){
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n"){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_04,
                             parms = list(k1 = k1,k2 = k2,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_04,
                             parms = list(k1 = k1,k2 = k2))
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result) %>%
      `if`(!is.null(data.expr),
           dplyr::filter(time %in% data.expr[[time.expr.series]]), .)
    #
    if (!is.null(data.expr) & isTRUE(model.expr.diff)){
      ## difference
      diff.model.expr <- data.expr[[qvar.expr]] - result.df[["R"]]
    }
    #
    ## data frame for plotting
    result.df.plot <- result.df %>%
      tidyr::pivot_longer(!time,names_to = "Species",values_to = "qvar") %>%
      dplyr::arrange(time)
  }
  if (model.react == "(x=1)A <==> [k1] [k2] (x=1)R"){
    ## functions for derivative solution of kinetic equation
    react_rates_diff_05 <- function(t,qvar,kin.params,c_x){
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (c_x == "n"){
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0,times = 2)
      ## differential equations
      if (inherits(c_x,"numeric")){
        rate[1] <- - k1*(qvar["A"])^(c_x) + k2*(qvar["R"])^(c_x)
        rate[2] <- k1*(qvar["A"])^(c_x) - k2*(qvar["R"])^(c_x)
      }
      if (c_x == "n"){
        rate[1] <- - k1*(qvar["A"])^n + k2*(qvar["R"])^n
        rate[2] <- k1*(qvar["A"])^n - k2*(qvar["R"])^n
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. like concentration)
    qvar0 <- c(A = unname(kin.params["qvar0A"]),
               R = unname(kin.params["qvar0R"]))
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    k2 <- kin.params["k2"]
    if (stoichiom_coeff(model.react) == "n"){
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n"){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_05,
                             parms = list(k1 = k1,k2 = k2,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_05,
                             parms = list(k1 = k1,k2 = k2))
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result) %>%
      `if`(!is.null(data.expr),
           dplyr::filter(time %in% data.expr[[time.expr.series]]), .)
    #
    if (!is.null(data.expr) & isTRUE(model.expr.diff)){
      ## difference
      diff.model.expr <- data.expr[[qvar.expr]] - result.df[["R"]]
    }
    #
    ## data frame for plotting
    result.df.plot <- result.df %>%
      tidyr::pivot_longer(!time,names_to = "Species",values_to = "qvar") %>%
      dplyr::arrange(time)
  }
  if (model.react == "A [k1] <-- (x=1)R --> [k2] B"){
    ## functions for derivative solution of kinetic equation
    react_rates_diff_06 <- function(t,qvar,kin.params,c_x){
      k1 <- kin.params$k1
      k2 <- kin.params$k2
      if (c_x == "n"){
        n <- kin.params$n
      }
      ## initial conditions
      rate <- rep(0,times = 1)
      ## differential equations
      if (inherits(c_x,"numeric")){
        rate[1] <- - k1*(qvar)^(c_x) - k2*(qvar)^(c_x)
      }
      if (c_x == "n"){
        rate[1] <- - k1*(qvar)^n - k2*(qvar)^n
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
    if (stoichiom_coeff(model.react) == "n"){
      n <- kin.params["n"]
    }
    #
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n"){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_06,
                             parms = list(k1 = k1,k2 = k2,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rates_diff_06,
                             parms = list(k1 = k1,k2 = k2))
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result) %>%
      `if`(!is.null(data.expr),
           dplyr::filter(time %in% data.expr[[time.expr.series]]), .)
    ## the first col. is `time` and 2nd has to be renamed
    names(result.df)[2] <- "R"
    #
    if (!is.null(data.expr) & isTRUE(model.expr.diff)){
      ## difference
      diff.model.expr <- data.expr[[qvar.expr]] - result.df[["R"]]
    }
    #
    ## data frame for plotting
    result.df.plot <- result.df
  }
  if (model.react == "(x=1)A + (y=1)B --> [k1] R"){
    ## functions for derivative solution of kinetic equation
    react_rates_diff_07 <- function(t,qvar,kin.params,c_x,c_y){
      k1 <- kin.params$k1
      if (c_x == "n" & inherits(c_y,"numeric")){
        n <- kin.params$n
      }
      if (inherits(c_x,"numeric") & c_y == "m"){
        m <- kin.params$m
      }
      if (c_x == "n" & c_y == "m"){
        n <- kin.params$n
        m <- kin.params$m
      }
      ## initial conditions
      rate <- rep(0,times = 3)
      ## differential equations
      if (inherits(c_x,"numeric") & inherits(c_y,"numeric")){
        rate[1] <- - k1*((qvar["A"])^(c_x))*((qvar["B"])^(c_y))
        rate[2] <- - k1*((qvar["B"])^(c_y))*((qvar["A"])^(c_x))
        rate[3] <- k1*((qvar["A"])^(c_x))*((qvar["B"])^(c_y))
      }
      if (c_x == "n" & c_y == "m"){
        rate[1] <- - k1*((qvar["A"])^n)*((qvar["B"])^m)
        rate[2] <- - k1*((qvar["B"])^m)*((qvar["A"])^n)
        rate[3] <- k1*((qvar["A"])^n)*((qvar["B"])^m)
      }
      if (c_x == "n" & inherits(c_y,"numeric")){
        rate[1] <- - k1*((qvar["A"])^n)*((qvar["B"])^(c_y))
        rate[2] <- - k1*((qvar["B"])^(c_y))*((qvar["A"])^n)
        rate[3] <- k1*((qvar["A"])^n)*((qvar["B"])^(c_y))
      }
      if (inherits(c_x,"numeric") & c_y == "m"){
        rate[1] <- - k1*((qvar["A"])^(c_x))*((qvar["B"])^m)
        rate[2] <- - k1*((qvar["B"])^m)*((qvar["A"])^(c_x))
        rate[3] <- k1*((qvar["A"])^(c_x))*((qvar["B"])^m)
      }
      ## derivative as a list
      return(list(rate))
    }
    #
    ## initial (`0`) qvar (e.g. like concentration)
    qvar0 <- c(A = unname(kin.params["qvar0A"]),
               B = unname(kin.params["qvar0B"]),
               R = unname(kin.params["qvar0R"]))
    #
    ## rate constant and other params. definition
    k1 <- kin.params["k1"]
    if (stoichiom_coeff(model.react) == "n" &
        inherits(stoichiom_coeff(model.react,coeff = "y"),"numeric")){
      n <- kin.params["n"]
    }
    if (inherits(stoichiom_coeff(model.react),"numeric") &
        stoichiom_coeff(model.react,coeff = "y") == "m"){
      m <- kin.params["m"]
    }
    if (stoichiom_coeff(model.react) == "n" &
        stoichiom_coeff(model.react,coeff = "y") == "m"){
      n <- kin.params["n"]
      m <- kin.params["m"]
    }
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n" &
        inherits(stoichiom_coeff(model.react,coeff = "y"),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             c_y = stoichiom_coeff(model.react,coeff = "y"),
                             func = react_rates_diff_07,
                             parms = list(k1 = k1,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric") &
        stoichiom_coeff(model.react,coeff = "y") == "m"){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             c_y = stoichiom_coeff(model.react,coeff = "y"),
                             func = react_rates_diff_07,
                             parms = list(k1 = k1,m = m))
    }
    if (stoichiom_coeff(model.react) == "n" &
        stoichiom_coeff(model.react,coeff = "y") == "m"){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             c_y = stoichiom_coeff(model.react,coeff = "y"),
                             func = react_rates_diff_07,
                             parms = list(k1 = k1,n = n,m = m))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric") &
        inherits(stoichiom_coeff(model.react,coeff = "y"),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             c_y = stoichiom_coeff(model.react,coeff = "y"),
                             func = react_rates_diff_07,
                             parms = list(k1 = k1))
    }
    #
    ## conversion into data frame
    result.df <- data.frame(result) %>%
      `if`(!is.null(data.expr),
           dplyr::filter(time %in% data.expr[[time.expr.series]]), .)
    #
    if (!is.null(data.expr) & isTRUE(model.expr.diff)){
      ## difference
      diff.model.expr <- data.expr[[qvar.expr]] - result.df[["R"]]
    }
    #
    ## data frame for plotting
    result.df.plot <- result.df %>%
      tidyr::pivot_longer(!time,names_to = "Species",values_to = "qvar") %>%
      dplyr::arrange(time)
  }
  #
  ## kinetics-behavior plots
  if (model.react == "(x=1)R --> [k1] B" ||
      model.react == "A [k1] <-- (x=1)R --> [k2] B"){
    plot.base <- ggplot(result.df.plot) +
      geom_point(aes(x = .data[["time"]],
                 y = .data[["R"]]),
                 size = 2.4,
                 shape = 18,
                 color = "darkviolet") +
      geom_line()
  } else {
    plot.base <- ggplot(result.df.plot) +
      geom_point(aes(x = .data[["time"]],
                 y = .data[["qvar"]],
                 color = .data[["Species"]]),
                 size = 2.4,
                 shape = 18) +
      geom_line() +
      theme(legend.title = element_text(size = 14),
            legend.text = element_text(size = 13))
  }
  #
  ## the entire plot
  plotR <- plot.base +
    labs(title = model.react,
         x = bquote(italic(Time)~~"("~s~")"),
         y = bquote(italic(Quantitative~Variable)~~bolditalic(qvar))) +
    plot_theme_In_ticks() +
    scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    theme(plot.title = element_text(hjust = 0.5))
  #
  ## RESULTS
  if (isFALSE(model.expr.diff)){
    return(list(df = result.df,plot = plotR))
  } else{
    return(diff.model.expr)
  }
#
}
