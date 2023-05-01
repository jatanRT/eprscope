#
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
    if (is.null(time.expr.series) || is.null(qvar.expr.series)){
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
    react_rates_diff <- function(t,qvar,kin.params,c_x){
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
                             func = react_rate_diff,
                             parms = list(k1 = k1,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rate_diff,
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
      diff.model.expr <- data.expr[[qvar.expr]] - result.df[[2]]
    }
    #
    ## data frame for plotting
    result.df.plot <- result.df
  }
  if (model.react == "(x=1)A --> [k1] R"){
    ## functions for derivative solution of kinetic equation
    react_rates_diff <- function(t,qvar,kin.params,c_x){
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
                             func = react_rate_diff,
                             parms = list(k1 = k1,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rate_diff,
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
    react_rates_diff <- function(t,qvar,kin.params,c_x){
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
                             func = react_rate_diff,
                             parms = list(k1 = k1,k2 = k2,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rate_diff,
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
    react_rates_diff <- function(t,qvar,kin.params,c_x){
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
                             func = react_rate_diff,
                             parms = list(k1 = k1,k2 = k2,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rate_diff,
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
    react_rates_diff <- function(t,qvar,kin.params,c_x){
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
                             func = react_rate_diff,
                             parms = list(k1 = k1,k2 = k2,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rate_diff,
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
    react_rates_diff <- function(t,qvar,kin.params,c_x){
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
                             func = react_rate_diff,
                             parms = list(k1 = k1,k2 = k2,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             func = react_rate_diff,
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
      diff.model.expr <- data.expr[[qvar.expr]] - result.df[[2]]
    }
    #
    ## data frame for plotting
    result.df.plot <- result.df
  }
  if (model.react == "(x=1)A + (y=1)B --> [k1] R"){
    ## functions for derivative solution of kinetic equation
    react_rates_diff <- function(t,qvar,kin.params,c_x,c_y){
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
    if (x == "n" & y == "m"){
      n <- kin.params["n"]
      m <- kin.params["m"]
    }
    ## solving `ordinary diff. equation(s)`
    if (stoichiom_coeff(model.react) == "n" & 
        inherits(stoichiom_coeff(model.react,coeff = "y"),"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             c_y = stoichiom_coeff(model.react,coeff = "y"), 
                             func = react_rate_diff,
                             parms = list(k1 = k1,n = n))
    }
    if (inherits(stoichiom_coeff(model.react),"numeric") & 
        stoichiom_coeff(model.react,coeff = "y") == "m"){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             c_y = stoichiom_coeff(model.react,coeff = "y"), 
                             func = react_rate_diff,
                             parms = list(k1 = k1,m = m))
    }
    if (x == "n" & y == "m"){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             c_y = stoichiom_coeff(model.react,coeff = "y"), 
                             func = react_rate_diff,
                             parms = list(k1 = k1,n = n,m = m))
    }
    if (inherits(x,"numeric") & inherits(y,"numeric")){
      result <- deSolve::ode(y = qvar0,times = t,
                             c_x = stoichiom_coeff(model.react),
                             c_y = stoichiom_coeff(model.react,coeff = "y"), 
                             func = react_rate_diff,
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
}