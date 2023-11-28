#'
#' Least-Squares Fitting of Isotropic EPR spectra by Simulations
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   A short description...
#'
#'
#' @details
#'   Additional details...
#'
#'
#'
#' @inheritParams eval_gFactor_Spec
#' @param data.spectr.expr Data frame object ... TBC ...
#' @param Intensity.expr Character string ... TBC ...
#' @param Intensity.sim Character string ... TBC ...
#' @param nuclear.system.noA List or nested list ... TBC ... without estimated hyperfine coupling constant values
#' @param baseline.correct Character string ...
#' @param lineG.content Numeric value ...
#' @param lineSpecs.form Character string ...
#' @param optim.method Character string ... TBC ...
#' @param optim.params.init Numeric vector with estimated ... TBC ...1. element = g-value, 2. element = Gaussian
#'   linewidth, 3. element = Lorentzian linewidth, 4. element = baseline constant, 5. element Intensity
#'   multiplication constant and 6, 7... elements baseline linear or quadratic multiplication
#'   and finally hyperfine coupling constants in `MHz`
#' @param optim.params.lower Numeric vector (with the length of \code{optim.params.init}) with the lower bound constraints.
#' @param optim.params.upper Numeric vector (with the length of \code{optim.params.init}) with the upper bound constraints.
#' @param Nmax.evals Numeric value corresp. to maximum number of iterations/evaluations.
#' @param tol.step Numeric value describing the smallest optimization step (tolerance) to stop the optimization.
#' @param pswarm.size Numeric value equal to particle swarm size (i. e. number of particles).
#' @param pswarm.diameter Numeric value corresponding to diameter of search space.
#' @param sim.check Logical, whether to return simple list with overlay plot and the best fitting parameters in a vector.
#'
#'
#' @return List with following components depending on \code{sim.check} ... TBC ...
#'
#'
#' @examples
#' \dontrun{
#' ## fitting of the simulated TMPD radical cation spectrum
#' ## on the experimental one (see also `Introduction` vignette)
#' ## 1. loading the built-in data
#' tempo.data.path <- load_data_example(file = "TMPD_specelchem_accu_b.asc")
#' tempo.data <- readEPR_Exp_Specs(tempo.data.path,
#'                                col.names = c("B_G","dIepr_over_dB"),
#'                                x = 1,
#'                                Intensity = 2,
#'                                qValue = 3500,
#'                                origin = "winepr")
#' ## 2. TMPD EPR spectrum may be simulated with the following hyperfine
#' ## coupling constants coming from
#' ## (see also https://doi.org/10.1007/s00706-004-0224-4) =>
#' ## A (2 x 14N) = `19.75` MHz, A (4 x 1H) = `5.58` MHz
#' ## and A (12 x 1H) = `18.97` MHz with the additional Gaussian
#' ## and Lorentzian linewidth `0.6` G and `0.6` G, respectively.
#' ## Baseline was estimated by the constant with the initial `0` value
#' ## and the initial multiplication factor for the intensity was `3`.
#' ## The initial g-value was estimated from the spectrum, g = `2.0031`.
#' tmpd.test.sim.fit <-
#' eval_sim_EPR_isoFit(data.spectr.expr = tmpd.data,
#'                     nu.GHz = 9.814155,
#'                     nuclear.system.noA = list(list("14N",2),
#'                                               list("1H",4),
#'                                               list("1H",12)),
#'                     optim.params.init = c(2.0031,0.6,0.6,0,3,19.75,5.58,18.97))
#' ## Such evaluation by "Nelder-Mead" optimization method returns list consisting
#' ## of plot with both experimental and simulated spectra (+ displaying residuals)
#' ## and/or the best fitting paramaters (if `sim.check = TRUE` which is by default)
#' tmpd.test.sim.fit$plot
#' ## and
#' tmpd.test.sim$best.fit.params
#' ## respectively. In such case the following best parameters
#' ## for the spectrum fit were found =>
#' ## c(2.00305,0.543,0.489,-1.065e-5,0.992,19.538,5.461,19.544)
#' ## If additional optimization/fitting parameters are required
#' ## (`sim.check = FALSE`) =>
#' tmpd.test.sim.fit <-
#' eval_sim_EPR_isoFit(data.spectr.expr = tmpd.data,
#'                     nu.GHz = 9.814155,
#'                     nuclear.system.noA = list(list("14N",2),
#'                                               list("1H",4),
#'                                               list("1H",12)),
#'                     optim.params.init = c(2.0031,0.6,0.6,0,3,19.75,5.58,18.97),
#'                     sim.check = FALSE)
#' ## returns the following list =>
#' tmpd.test.sim.fit$plot ## publication ready plot (both spectra are not overlayed)
#' tmpd.test.sim.fit$best.fit.params ## the same like before
#' tmpd.test.sim.fit$df ## data frame with sim., sim. without baseline, expr. and residuals
#' tmpd.test.sim.fit$sum.LSQ.min ## min. sum of residual squares
#' tmpd.test.sim.fit$N.evals ## number of evaluations/iterations
#' tmpd.test.sim.fit$N.converg ## "convergence" value indicating successful completion
#' #
#' ## If two subsequent `optim.method`s are used the resulting list is complex/nested
#' ## showing all the info for each method except the plot corresponding to that from
#' ## the last `optim.method`.
#' }
#'
#'
#' @export
#'
#'
#' @importFrom stats median
#' @importFrom dplyr rowwise
eval_sim_EPR_isoFit <- function(data.spectr.expr,
                                Intensity.expr = "dIepr_over_dB",
                                Intensity.sim = "dIeprSim_over_dB",
                                nu.GHz,
                                B.unit = "G",
                                nuclear.system.noA,
                                baseline.correct = "constant", ## "linear" or "quadratic"
                                lineG.content = 0.5,
                                lineSpecs.form = "derivative",
                                optim.method = "neldermead", ## also two consecutive methods as vector
                                optim.params.init,
                                optim.params.lower = NULL,
                                optim.params.upper = NULL,
                                Nmax.evals = 1024,
                                tol.step = 5e-7,
                                pswarm.size = NULL,
                                pswarm.diameter = NULL,
                                sim.check = TRUE){
  #
  ## 'Temporary' processing variables
  . <- NULL
  Residuals <- NULL
  Simulation_NoBasLin <- NULL
  ## delete index column if present
  if (any(grepl("index", colnames(data.spectr.expr)))) {
    data.spectr.expr$index <- NULL
  }
  ## instrumental parameters except the microwave frequency must be read from
  ## the experimental data. It cannot be done by the same way like in simulation
  ## because the relevant instrum. params. like Bsw (B.SW) and Bcf (B.CF) differs
  ## from those presented in `.DSC` and `.par`. The reason is the Teslameter. If it'is
  ## in ON state the measured B values (can be slightly, i.e. approx. 1-3 G) different
  ## from those measured by the Hall probe or from the spectrum parameter settings.
  ## If the Teslameter is in ON state the measured values are automatically
  ## written into the text ASCII file. Therefore, to properly compare the simulated
  ## and experimental spectrum these parameters must be extracted form
  ## the experimental ASCII (`.txt` or `.asc`) ASCII data file =>
  B.cf <- stats::median(data.spectr.expr[[paste0("B_",B.unit)]])
  B.sw <- max(data.spectr.expr[[paste0("B_",B.unit)]]) -
    min(data.spectr.expr[[paste0("B_",B.unit)]])
  N.points <- nrow(data.spectr.expr)
  mw.GHz <- nu.GHz
  ## therefore => the named vector
  instrum.params <- c(Bcf = B.cf,Bsw = B.sw,Npoints = N.points,mwGHz = mw.GHz)
  #
  ## Define the length of `nuclear.system.noA` similarly as in simple simulation
  ## check if the list is nested (several groups) or simple (only one group)
  nested_list <- any(sapply(nuclear.system.noA, is.list))
  if (isFALSE(nested_list)){
    ## redefinition of `nuclear.system.noA` list to calculate the spectra without
    ## any additional conditions just by simple =>
    nuclear.system.noA <- list(nuclear.system.noA)
  } else {
    nuclear.system.noA <- nuclear.system.noA
  }
  #
  ## condition to switch among three values
  baseline.cond.fn <- function(baseline.correct){
    if (baseline.correct == "constant"){
      return(0)
    } else if (baseline.correct == "linear"){
        return(1)
    } else if(baseline.correct == "quadratic"){
        return(2)
    }
  }
  #
  ## functions to parameterize simulation by arguments/parameters
  ## based on `optim.method` and the corresponding argument
  ## therefore => AS a FITNESS FUNCTIONS THEY MUST BE DEFINED SEPARATELY !!
  ## OTHERWISE THE OPTIMIZATION WON'T WORK
  fit_sim_params_par <- function(data,
                                 nucs.system,
                                 Intensity.sim,
                                 baseline,
                                 B.unit,
                                 par){
    #
    ## definition of the 1st param. => g-value
    g.var <- par[1]
    ## definition of the additional params. like line-width
    ## and GL-line content/contribution
    gB.width.var <- par[2]
    lB.width.var <- par[3]
    #
    ## A.vars based on `nucs.system`, such system (definition at the beginning)
    ## must contain only nucleus character string and the corresponding number
    ## of nuclei within the group because As will be varied
    if (is.null(nucs.system)){
      sim.fit.df <-
        eval_sim_EPR_iso(g.iso = g.var,
                         B.unit = B.unit,
                         instrum.params = instrum.params,
                         natur.abund = FALSE,
                         nuclear.system = NULL,
                         lineSpecs.form = lineSpecs.form,
                         lineGL.DeltaB = list(gB.width.var,
                                              lB.width.var),
                         lineG.content = lineG.content,
                         Intensity.sim = Intensity.sim)$df
      #
    } else {
      #
      ## Define the length of `nucs.system` similarly as in simple simulation
      ## check if the list is nested (several groups) or simple (only one group)
      nested_list <- any(sapply(nucs.system, is.list))
      if (isFALSE(nested_list)){
        ## redefinition of `nucs.system` list to calculate the spectra without
        ## any additional conditions just by simple =>
        nucs.system <- list(nucs.system)
      } else{
        nucs.system <- nucs.system
      }
      ## what is the length of the list (how many nuclear groups)
      nucle_us_i <- sapply(1:length(nucs.system), function(e) nucs.system[[e]][[1]])
      #
      ## adding parameters As to nested list
      ## the first par[1,2,3,4,5,...] is reserved for g,linewidths, baseline and intensity
      ## `A.var` should be explicitly expressed by corresp. x0 elements
      A.var <- switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
                      par[8:(7+length(nucle_us_i))],
                      par[7:(6+length(nucle_us_i))],
                      par[6:(5+length(nucle_us_i))]
                      )
      #
      nucs.system.new <- c()
      for (j in seq(nucs.system)) {
        nucs.system.new[[j]] <- c(nucs.system[[j]],A.var[j])
        nucs.system.new[[j]] <- as.list(nucs.system.new[[j]])
      }
      #
      ## evaluating simulated intensity like before (`nucs.system = NULL`)
      sim.fit.df <-
        eval_sim_EPR_iso(g.iso = g.var,
                         B.unit = B.unit,
                         instrum.params = instrum.params,
                         natur.abund = TRUE,
                         nuclear.system = nucs.system.new,
                         lineSpecs.form = lineSpecs.form,
                         lineGL.DeltaB = list(gB.width.var,
                                              lB.width.var),
                         lineG.content = lineG.content,
                         Intensity.sim = Intensity.sim)$df

      #
    }
    if (baseline == "constant"){
      ## Intensity = a + b*Intensity
      data[[Intensity.sim]] <- par[4] + (par[5] * sim.fit.df[[Intensity.sim]])
    }
    if (baseline == "linear"){
      ## Intensity = a + b*Intensity + c*B (B = "magnetic flux density")
      data[[Intensity.sim]] <- par[4] + (par[5] * sim.fit.df[[Intensity.sim]]) +
        (par[6] * sim.fit.df[[paste0("Bsim_",B.unit)]])
    }
    if (baseline == "quadratic"){
      ## Intensity = a + b*Intensity + c*B + d*B^2
      data[[Intensity.sim]] <- par[4] + (par[5] * sim.fit.df[[Intensity.sim]]) +
        (par[6] * sim.fit.df[[paste0("Bsim_",B.unit)]]) +
        (par[7] * (sim.fit.df[[paste0("Bsim_",B.unit)]])^2)
    }
    #
    return(data[[Intensity.sim]])
    #
  }
  #
  ## the second function with `x0`
  fit_sim_params_x0 <- function(data,
                                nucs.system,
                                Intensity.sim,
                                baseline,
                                B.unit,
                                x0){
    #
    ## definition of the 1st param. => g-value
    g.var <- x0[1]
    ## definition of the additional params. like line-width
    ## and GL-line content/contribution
    gB.width.var <- x0[2]
    lB.width.var <- x0[3]
    #
    ## A.vars based on `nucs.system`, such system (definition at the beginning)
    ## must contain only nucleus character string and the corresponding number
    ## of nuclei within the group because As will be varied
    if (is.null(nucs.system)){
      sim.fit.df <-
        eval_sim_EPR_iso(g.iso = g.var,
                         B.unit = B.unit,
                         instrum.params = instrum.params,
                         natur.abund = FALSE,
                         nuclear.system = NULL,
                         lineSpecs.form = lineSpecs.form,
                         lineGL.DeltaB = list(gB.width.var,
                                              lB.width.var),
                         lineG.content = lineG.content,
                         Intensity.sim = Intensity.sim)$df
      #
    } else {
      #
      ## Define the length of `nucs.system` similarly as in simple simulation
      ## check if the list is nested (several groups) or simple (only one group)
      nested_list <- any(sapply(nucs.system, is.list))
      if (isFALSE(nested_list)){
        ## redefinition of `nucs.system` list to calculate the spectra without
        ## any additional conditions just by simple =>
        nucs.system <- list(nucs.system)
      } else{
        nucs.system <- nucs.system
      }
      ## what is the length of the list (how many nuclear groups)
      nucle_us_i <- sapply(1:length(nucs.system), function(e) nucs.system[[e]][[1]])
      #
      ## adding parameters As to nested list
      ## the first par[1,2,3,4,5,...] is reserved for g,linewidths, baseline and intensity
      ## `A.var` should be explicitly expressed by corresp. x0 elements
      A.var <- switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
                      x0[8:(7+length(nucle_us_i))],
                      x0[7:(6+length(nucle_us_i))],
                      x0[6:(5+length(nucle_us_i))]
      )
      #
      nucs.system.new <- c()
      for (j in seq(nucs.system)) {
        nucs.system.new[[j]] <- c(nucs.system[[j]],A.var[j])
        nucs.system.new[[j]] <- as.list(nucs.system.new[[j]])
      }
      #
      ## evaluating simulated intensity like before (`nucs.system = NULL`)
      sim.fit.df <-
        eval_sim_EPR_iso(g.iso = g.var,
                         B.unit = B.unit,
                         instrum.params = instrum.params,
                         natur.abund = TRUE,
                         nuclear.system = nucs.system.new,
                         lineSpecs.form = lineSpecs.form,
                         lineGL.DeltaB = list(gB.width.var,
                                              lB.width.var),
                         lineG.content = lineG.content,
                         Intensity.sim = Intensity.sim)$df

      #
    }
    if (baseline == "constant"){
      ## Intensity = a + b*Intensity
      data[[Intensity.sim]] <- x0[4] + (x0[5] * sim.fit.df[[Intensity.sim]])
    }
    if (baseline == "linear"){
      ## Intensity = a + b*Intensity + c*B (B = "magnetic flux density")
      data[[Intensity.sim]] <- x0[4] + (x0[5] * sim.fit.df[[Intensity.sim]]) +
        (x0[6] * sim.fit.df[[paste0("Bsim_",B.unit)]])
    }
    if (baseline == "quadratic"){
      ## Intensity = a + b*Intensity + c*B + d*B^2
      data[[Intensity.sim]] <- x0[4] + (x0[5] * sim.fit.df[[Intensity.sim]]) +
        (x0[6] * sim.fit.df[[paste0("Bsim_",B.unit)]]) +
        (x0[7] * (sim.fit.df[[paste0("Bsim_",B.unit)]])^2)
    }
    #
    return(data[[Intensity.sim]])
    #
  }
  #
  ## initial parameter guesses for the optimization and definition
  lower.limits <- c(optim.params.init[1] - 0.0005,
                    optim.params.init[2] - (optim.params.init[2] * 0.1),
                    optim.params.init[3] - (optim.params.init[2] * 0.1),
                    optim.params.init[4] - 0.0005,
                    1e-8)
  lower.limits <- switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
                         c(lower.limits,-5,-5),
                         c(lower.limits,-5),
                         lower.limits
                         )
  upper.limits <- c(optim.params.init[1] + 0.0005,
                    optim.params.init[2] + (optim.params.init[2] * 0.1),
                    optim.params.init[3] + (optim.params.init[2] * 0.1),
                    optim.params.init[4] + 0.0005,
                    100)
  upper.limits <- switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
                         c(upper.limits,5,5),
                         c(upper.limits,5),
                         upper.limits
  )
  #
  if (is.null(nuclear.system.noA)){
    lower.limits <- lower.limits
    upper.limits <- upper.limits
  } else {
    ## individual optimization limits for As
    A.lower.limits <- c()
    A.upper.limits <- c()
    if (baseline.correct == "constant"){
      for (a in 6:(5+length(nuclear.system.noA))){
        A.lower.limits[a-5] <- optim.params.init[a] - (optim.params.init[a] * 0.055)
        A.upper.limits[a-5] <- optim.params.init[a] + (optim.params.init[a] * 0.055)
      }
    }
    if (baseline.correct == "linear"){
      for (a in 7:(6+length(nuclear.system.noA))){
        A.lower.limits[a-6] <- optim.params.init[a] - (optim.params.init[a] * 0.055)
        A.upper.limits[a-6] <- optim.params.init[a] + (optim.params.init[a] * 0.055)
      }
    }
    if (baseline.correct == "quadratic"){
      for (a in 8:(7+length(nuclear.system.noA))){
        A.lower.limits[a-7] <- optim.params.init[a] - (optim.params.init[a] * 0.055)
        A.upper.limits[a-7] <- optim.params.init[a] + (optim.params.init[a] * 0.055)
      }
    }
    #
    ## actual `limits`
    lower.limits <- c(lower.limits,A.lower.limits)
    upper.limits <- c(upper.limits,A.upper.limits)
  }
  optim.params.lower <- optim.params.lower %>%
    `if`(is.null(optim.params.lower), lower.limits, .)
  optim.params.upper <- optim.params.upper %>%
    `if`(is.null(optim.params.upper), upper.limits, .)
  #
  ## "general" function for optimization because it depends
  ## on method (`method`) and function (`fun`) and initial params (`x.0`)
  optim_fn <- function(fun,method,x.0){
    optim.list <- optim_for_EPR_fitness(x.0 = x.0,
                                        fn = fun,
                                        method = method,
                                        lower = optim.params.lower,
                                        upper = optim.params.upper,
                                        data = data.spectr.expr,
                                        nucs.system = nuclear.system.noA,
                                        Intensity.sim = Intensity.sim,
                                        baseline = baseline.correct,
                                        B.unit = B.unit,
                                        Nmax.evals = Nmax.evals,
                                        tol.step = tol.step,
                                        pswarm.size = pswarm.size,
                                        pswarm.diameter = pswarm.diameter)
    #
    return(optim.list)
  }
  #
  ## own optimization which can be performed also with two consecutive
  ## methods depending on the `optim.method` vector length
  optimization.list <- c()
  best.fit.params <- c()
  for (m in seq(optim.method)) {
    if (optim.method[m] == "levenmarq"){
      ## LSQ or DIFF. FUNCTIONS
      ## "levelnmarq" is defined by residuals, NOT by sum of the residual squares !!
      min_residuals_lm <- function(data,nucs.system,Intensity.sim,baseline,B.unit,par){
        return(data[[Intensity.expr]] -
                 fit_sim_params_par(data,nucs.system,Intensity.sim,baseline,B.unit,par))
      }
      #
      optimization.list[[m]] <- optim_fn(fun = min_residuals_lm,
                                         method = "levenmarq",
                                         x.0 = optim.params.init)
    }
    if (optim.method[m] == "pswarm"){
      ## LSQ FUNCTION
      min_residuals_ps <- function(data,nucs.system,Intensity.sim,baseline,B.unit,par){
        with(data,sum((data[[Intensity.expr]] -
                         fit_sim_params_par(data,nucs.system,Intensity.sim,baseline,B.unit,par))^2))
      }
      #
      optimization.list[[m]] <- optim_fn(fun = min_residuals_ps,
                                         method = "pswarm",
                                         x.0 = optim.params.init)
    }
    if (optim.method[m] == "slsqp" || optim.method[m] == "neldermead" ||
        optim.method[m] == "crs2lm" || optim.method[m] == "sbplx") { ## with `else` it doesn't work
      ## LSQ FUNCTION
      min_residuals_nl <- function(data,nucs.system,Intensity.sim,baseline,B.unit,x0){
        with(data,sum((data[[Intensity.expr]] -
                         fit_sim_params_x0(data,nucs.system,Intensity.sim,baseline,B.unit,x0))^2))
      }
      #
      optimization.list[[m]] <- optim_fn(fun = min_residuals_nl,
                                         method = optim.method[m],
                                         x.0 = optim.params.init)
    }
    #
    ## best parameters as input (`optim.params.init`) for the next cycle
    ## if several subsequent `optim.method` applied
    best.fit.params[[m]] <- optimization.list[[m]]$par
    if (length(optim.method) > 1){
      optim.params.init <- best.fit.params[[m]]
    }
    #
  }
  #
  ## The best system is the last one from the `best.fit.params` =>
  ## therefore it correspond to `best.fit.params[[length(optim.method)]]`
  ## to simulate and display the spectrum
  #
  ## "best" (i.e. including best As) nuclear system
  if (is.null(nuclear.system.noA)){
    nucs.system.best <- NULL
  } else{
    A.best <- switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
                     best.fit.params[[length(optim.method)]][8:(7+length(nuclear.system.noA))],
                     best.fit.params[[length(optim.method)]][7:(6+length(nuclear.system.noA))],
                     best.fit.params[[length(optim.method)]][6:(5+length(nuclear.system.noA))]
                     )
    A.best <- round(A.best,digits = 3)
    nucs.system.best <- c()
    for (j in seq(nuclear.system.noA)) {
      nucs.system.best[[j]] <- c(nuclear.system.noA[[j]],A.best[j])
      nucs.system.best[[j]] <- as.list(nucs.system.best[[j]])
    }
  }
  #
  ## best simulated spectrum data frame
  best.fit.df <- eval_sim_EPR_iso(g.iso = best.fit.params[[length(optim.method)]][1],
                                  B.unit = B.unit,
                                  instrum.params = instrum.params,
                                  natur.abund = TRUE,
                                  nuclear.system = nucs.system.best,
                                  lineSpecs.form = lineSpecs.form,
                                  lineGL.DeltaB = list(best.fit.params[[length(optim.method)]][2],
                                                       best.fit.params[[length(optim.method)]][3]),
                                  lineG.content = lineG.content,
                                  Intensity.sim = Intensity.sim)$df
  #
  ## best simulated Intensity and add the `Intensity.sim` to experimental
  # spectrum data based on the baseline.correct condition
  ## first of all the intensity part which depends on `baseline.correct`
  Intens.baseline.switch <-
    switch(3-baseline.cond.fn(baseline.correct = baseline.correct),
           ((best.fit.params[[length(optim.method)]][6] * best.fit.df[[paste0("Bsim_",B.unit)]]) +
              (best.fit.params[[length(optim.method)]][7] * (best.fit.df[[paste0("Bsim_",B.unit)]])^2)),
           (best.fit.params[[length(optim.method)]][6] * best.fit.df[[paste0("Bsim_",B.unit)]]),
           0
    )
  ## the overall intensity incl. that defined above
  data.spectr.expr[[Intensity.sim]] <-
    best.fit.params[[length(optim.method)]][4] +
    (best.fit.params[[length(optim.method)]][5] * best.fit.df[[Intensity.sim]]) +
    Intens.baseline.switch
  #
  ## ======================== DATA & PLOTTING =============================
  #
  ## final data frame and rename columns
  data.sim.expr <- data.spectr.expr %>%
    dplyr::select(dplyr::all_of(c(paste0("B_",B.unit),
                                  Intensity.expr,Intensity.sim))) %>%
    dplyr::rename_with(~ c("Experiment","Simulation"),
                       dplyr::all_of(c(Intensity.expr,Intensity.sim)))
  rm(data.spectr.expr) ## not required anymore
  #
  ## calculating `rowwise` difference between expr. and sim. spectra
  data.sim.expr.resid <- data.sim.expr %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Residuals = .data$Experiment - .data$Simulation)
  ## select only `B` and `Residuals`
  data.sim.expr.resid <- data.sim.expr.resid %>%
    dplyr::select(dplyr::all_of(c(paste0("B_",B.unit),"Residuals")))
  #
  ## results (incl. comparison of experimental and simulated spectra)
  ## depending on `sim.check` which shows only the overlay spectra and best
  ## fitting parameters. Otherwise the entire list (see below) will be returned.
  if (isTRUE(sim.check)){
    ## transformation into long table ("tidy") format for visualization
    data.sim.expr.long <- data.sim.expr %>%
      tidyr::pivot_longer(!dplyr::all_of(paste0("B_",B.unit)),
                          names_to = "Spectrum",
                          values_to = Intensity.expr) %>%
      dplyr::arrange(.data$Spectrum)
    #
  } else {
    ## for plotting both spectra as publication ready => spectra will be offset
    ## => recalculate the intensity => shift the simulated intensity
    ## down below by factor of difference between `max()` and `min()`
    Int.diff <- max(data.sim.expr$Experiment) - min(data.sim.expr$Experiment)
    data.sim.expr.long <- data.sim.expr %>%
      # dplyr::mutate(!!rlang::quo_name("Simulation") := .data$Simulation - (0.9 * Int.diff)) %>%
      ## simulation without baseline
      dplyr::mutate(Simulation_NoBasLin = best.fit.params[[length(optim.method)]][5] *
                      best.fit.df[[Intensity.sim]]) %>%
      tidyr::pivot_longer(!dplyr::all_of(paste0("B_",B.unit)),
                          names_to = "Spectrum",
                          values_to = Intensity.expr) %>%
      dplyr::arrange(.data$Spectrum)
  }
  #
  ## Adding residuals and `pure` simulation (without baseline)
  ## to the overall data frame
  data.sim.expr$Residuals <- data.sim.expr.resid$Residuals
  data.sim.expr$Simulation_NoBasLin <- best.fit.params[[length(optim.method)]][5] *
    best.fit.df[[Intensity.sim]]
  #
  ## plotting all spectra
  #
  if (isTRUE(sim.check)){
    ## display both overlay spectra (upper part) and residuals
    ## (lower part) in 1 col. by `{patchwork}`
    plot.sim.expr.upper <- ggplot(data = data.sim.expr.long) +
      geom_line(aes(x = .data[[paste0("B_",B.unit)]],
                    y = .data[[Intensity.expr]],
                    color = .data$Spectrum),
                linewidth = 0.75) +
      scale_color_manual(values = c("darkcyan","magenta")) +
      labs(title = "EPR Simulation Fit",
           color = NULL,
           x = NULL,
           y = bquote(d*italic(I)[EPR]~~"/"~~d*italic(B)~~~"("~p.d.u.~")")) +
      plot_theme_In_ticks() +
      scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
      scale_y_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
      theme(legend.text = element_text(size = 13))
      #
      plot.sim.expr.lower <- ggplot(data = data.sim.expr.resid) +
        geom_line(aes(x = .data[[paste0("B_",B.unit)]],
                      y = Residuals),
                  color = "black",
                  linewidth = 0.75) +
        labs(title = "Residuals",
             x = bquote(italic(B)~~"("~.(B.unit)~")"),
             y = bquote(Diff.~d*italic(I)[EPR]~~"/"~~d*italic(B)~~~"("~p.d.u.~")")) +
        plot_theme_In_ticks() +
        scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
        scale_y_continuous(sec.axis = dup_axis(name = "",labels = NULL))
      #
      ## entire plot
      plot.sim.expr <-
        patchwork::wrap_plots(plot.sim.expr.upper,
                              plot.sim.expr.lower,
                              ncol = 1)
      #
  } else {
    plot.sim.expr <- ggplot(data = data.sim.expr.long) +
      geom_line(aes(x = .data[[paste0("B_",B.unit)]],
                    y = .data[[Intensity.expr]],
                    color = .data$Spectrum),
                linewidth = 0.75) +
<<<<<<< HEAD
      scale_color_manual(values = c("darkcyan","magenta","darkblue"),
                         labels = c("Experiment",
                                    "\nSimulation",
=======
      scale_color_manual(values = c("darkcyan","magenta","blue"),
                         labels = c("Experiment",
                                    "Simulation",
>>>>>>> master
                                    "\nSimulation\nNo Baseline")) +
      labs(color = NULL,
           x = bquote(italic(B)~~"("~.(B.unit)~")"),
           y = bquote(d~italic(I)[EPR]~~"/"~~d~italic(B)~~~"("~p.d.u.~")")) +
      plot_theme_NoY_ticks() +
      scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
      theme(legend.text = element_text(size = 13))
  }
  #
  ## ==================== BASIC OPTIMIZATION INFORMATION/STATISTICS ======================
  #
  ## final list components depending on method
  min.LSQ.sum <- c()
  N.evals <- c()
  N.converg <- c()
  for(m in seq(optim.method)){
    if (optim.method[m] == "levenmarq"){
      min.LSQ.sum[[m]] <-
        optimization.list[[m]]$deviance ## The min sum of the squared residual vector.
      # fn.min <- optimization.list$fvec ## The result of the last `fn` evaluation; i.e. the residuals.
      N.evals[[m]] <-
        optimization.list[[m]]$niter ## The number of iterations/evaluations completed before termination.
      N.converg[[m]] <-
        optimization.list[[m]]$rsstrace ## Sum of squares at each iteration.
    }
    if (optim.method[m] == "pswarm"){
      min.LSQ.sum[[m]] <-
        optimization.list[[m]]$value ## The value of `fn` corresponding to best `par`.
                                     ## because `fn` is sum of squares
      N.evals[[m]] <-
        optimization.list[[m]]$counts ## A three-element vector containing the number of function
                                      ## evals., the number of iterations, and the number of restarts.
      N.converg[[m]] <-
        optimization.list[[m]]$convergence ## An integer code. `0` indicates that the algorithm
                                           ## terminated by reaching the absolute tolerance; otherwise:
                                           ## `1` Maximal number of function evaluations reached.
                                           ## `2` Maximal number of iterations reached.
                                           ## `3` Maximal number of restarts reached.
                                           ## `4` Maximal number of iterations without improvement reached.

    }
    if (optim.method[m] == "slsqp" || optim.method[m] == "neldermead" ||
        optim.method[m] == "crs2lm" || optim.method[m] == "sbplx"){ ## with `else` it doesn't work
      min.LSQ.sum[[m]] <-
        optimization.list[[m]]$value ## the function value corresponding to `par`.
                                     ## because function is sum of squares
      N.evals[[m]] <-
        optimization.list[[m]]$iter ## number of (outer) iterations, see `Nmax.evals`.
      N.converg[[m]] <-
        optimization.list[[m]]$convergence ## integer code indicating successful completion (> 0)
                                           ## or a possible error number (< 0).
    }
  }
  #
  ## ================================= RESULTS =============================
  #
  ## switching between final list components
  result.list <- switch(2-sim.check,
                        list(plot = plot.sim.expr,
                             best.fit.params = best.fit.params), ## all params., for all methods
                        list(plot = plot.sim.expr,
                             best.fit.params = best.fit.params, ## all params., for all methods
                             df = data.sim.expr,
                             sum.LSQ.min = min.LSQ.sum,
                             N.evals = N.evals,
                             N.converg = N.converg))
  #
  return(result.list)
  #
}
