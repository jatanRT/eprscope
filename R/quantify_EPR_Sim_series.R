#
#' Quantify (Components) Area of Simulated EPR Spectral Series Instead of Experimental One
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   Evaluating the linear combination of spectral intensities of components (loaded as ASCII text files corresponding
#'   to simulated spectra). The related intensity multiplication coefficients
#'   (please, refer to the \code{optim.params.init} argument) are optimized by methods gathered in the
#'   \code{\link{optim_for_EPR_fitness}}. The goal is to fit the sum of the simulated components
#'   onto each experimental spectrum within the series. So far, the maximum number of components is set to 6.
#'
#'
#' @details
#'   Analyzed EPR spectra may consists of several components (see also the \code{\link{eval_sim_EPR_iso_combo}}
#'   function), like the overlapped EPR spectra of several radicals. In order to follow the concentration/amount
#'   variations of each individual radical during the kinetic/temperature/...series, one must figure out
#'   how those individual spectral components are actually changed. In the first approximation, it means
#'   to follow the corresponding EPR intensities/integrals, whereas the component positions (\eqn{g}-values)
#'   are assumed to be fixed (or those changes can be neglected). Therefore, the actual function takes the linear
#'   combination of the spectral intensities of components (simulated spectra) and optimizes
#'   the related multiplication coefficients. Additional analysis, where the positions of spectral
#'   components (simulated spectra) are not fixed and can be optimized as well. It is based on the combination
#'   of the actual function with the \code{\link{eval_sim_EPR_iso_combo}} and currently, it is under development.
#'
#'
#'
#' @param data.spectra.series Spectrum data frame/table object containing magnetic flux density
#'   as \code{x} variable. They can be labeled as \code{Field}, \code{B_mT}
#'   in mT (or \code{B_G} in gauss). The \code{y/Intensity} variable
#'   can be labeled as \code{dIepr_over_dB}, in case of derivative intensity, or if
#'   integrated spectral intensities are present, they can be labeled accordingly.
#'   See also \code{Intensity.expr} parameter/argument. A second independent variable
#'   \code{var2nd.series} column (e.g. \code{var2nd.series = "time_s"}) must be available. In such case,
#'   the entire \code{data.spectra} must be present in the form of
#'   \href{https://r4ds.had.co.nz/tidy-data.html}{tidy/long table format}
#'   (see also parameter/argument \code{var2nd.series}). Such data frame can be created, e.g.
#'   by the \code{\link{readEPR_Exp_Specs_kin}} or the \code{\link{readEPR_Exp_Specs_multif}} function.
#' @param dir_ASC_sim Character string, pointing to folder where the simulated EPR spectra of all
#'   components are stored. Path can be alternatively specified by the \code{\link[base]{file.path}} function.
#' @param name.pattern.sim Character string pattern from file names related to simulated EPR spectral data
#'   like \code{name.pattern.sim = "DHMB0_1st_04_SimA"}
#'   or \code{name.pattern.sim = "DHMB0_1st_04_Sim[[:upper:]]"} (for the file names \code{..._SimA},
#'   \code{..._SimB},...etc). It assumes, those file must have similar names and this pattern appears
#'   at the beginning of the file name. One may also consult
#'   how to \href{https://r4ds.hadley.nz/regexps}{use regular expressions in R}.
#' @param sim.origin Character string referring to "origin" of the simulated ASCII data.
#'   There are four possibilities \eqn{\Rightarrow} \code{sim.orimgin = "easyspin"} (\strong{default}),
#'   \code{"xenon"}, \code{"simfonia"} as well as universal \code{"csv"}.
#' @param var2nd.series Character string referred to name of the second independent variable/quantity
#'   column in the original \code{data.spectra.series} (such as time, temperature, electrochemical potential,
#'   Microwave Power) altered upon individual experiments as a second variable.
#'   Data must be available in tidy/long table format.
#'   \strong{Default}: \code{var2nd.series = "time_s"}.
#' @param B.unit Character string pointing to unit of magnetic flux density
#'   like \code{"G"} (Gauss) or \code{"mT"} (millitesla), \strong{default}: \code{B.unit = "G"}.
#'   THE UNIT MUST BE SHARED ACROSS ALL RELEVANT B-ARGUMENTS/DATAFRAMES.
#' @param Intensity.expr Character string pointing to column name of the experimental EPR intensity within
#'   the original \code{data.spectra.series}. \strong{Default}: \code{dIepr_over_dB}.
#' @param Intensity.sim Character string pointing to column name of the simulated EPR intensity within the related
#'   data frames (check the simulated spectral data for all components).
#'   \strong{Default}: \code{Intensity.sim = "dIeprSim_over_dB"}.
#' @param optim.method Character string, pointing to applied optimization method/algorithm.
#'   One may choose one from those listed in \code{\link{optim_for_EPR_fitness}}, \strong{default}:
#'   \code{method = "sbplx"}, setting up
#'   the \href{https://nlopt.readthedocs.io/en/latest/NLopt_Algorithms/#sbplx-based-on-subplex}{"Subplex" method}.
#' @param optim.params.init Numeric vector with the elements: baseline constant/intercept
#'   followed by simulated intensity multiplication coefficient for each EPR spectral component.
#'   Therefore, the length of this vector is equal to: number of components + 1.
#' @param optim.params.lower Numeric vector with the length of \code{optim.params.init} and the lower
#'   bound constraints. \strong{Default}: \code{optim.params.init = NULL}, actually corresponding
#'   to vector with all \code{0} value elements.
#' @param optim.params.upper Numeric vector with the length of \code{optim.params.init}) and the upper
#'   bound constraints. \strong{Default}: \code{optim.params.init = NULL}, actually corresponding
#'   to vector with all \code{0.9} value elements.
#' @param Nmax.evals Numeric value, maximum number of function evaluations and/or iterations.
#'   The only one method, limited by this argument, is \code{\link[minpack.lm]{nls.lm}}, where
#'   \code{Nmax.evals = 1024} (\strong{default}). Higher \code{Nmax.evals} may extend the optimization
#'   time.
#' @param tol.step Numeric, the smallest optimization step (relative change) between
#'   2 iterations to stop the optimization procedure. For the \code{optim.method = "pswarm"}
#'   (particle swarm optimization procedure) it actually corresponds to tolerance for restarting.
#'   Once the maximum distance between the "best" particle and all the others is less
#'   than \code{tol.step} * \code{pswarm.diameter}) the algorithm restarts.
#'   See also \code{\link[pso]{psoptim}}. \strong{Default}: \code{tol.step = 5e-7}.
#' @param pswarm.size Numeric value equal to particle swarm size (i.e. number of particles),
#'   if \code{optim.method = "pswarm"}. The \strong{default} value (\code{pswarm.size = NULL}) actually
#'   corresponds to \code{floor(10+2*sqrt(length(x.0)))}.
#' @param pswarm.diameter Numeric value corresponding to diameter of the particle swarm search space
#'   (in case \code{optim.method = "pswarm"}). The \strong{default} value (\code{pswarm.diameter = NULL})
#'   refers to the Euclidean distance, defined as:
#'   \deqn{\sqrt{\sum_k\,(\text{optim.params.upper}[k] - \text{optim.params.lower}[k])^2}}
#' @param single.integ Character string, setting up the column/variable name related to single-integrated spectrum
#'   within the output data frame, \strong{default}: \code{single.integ = "single_IntegSim"}.
#' @param double.integ Character string, setting up the column/variable name related to double-integrated spectrum
#'   within the output data frame, \strong{default}: \code{double.integ = "single_IntegSim"}.
#'   If \code{double.integ = NULL} only single integrals are calculated/returned (e.g. in the case of
#'   single integrated spectral data).
#' @param output.area.stat Logical, whether to summarize all fitted EPR spectral components, in columns,
#'   for each time/temperature/...etc. point in row. Additional optimization measures are presented as well
#'   (see \code{Details}).\strong{Default}: \code{output.area.stat = TRUE}.
#' @param ... additional arguments specified (see also \code{\link{optim_for_EPR_fitness}}).
#'
#'
#' @return Function provides data frame object, depending on the \code{output.area.stat} argument,
#'   as listed below:
#'   \enumerate{
#'   \item If \code{output.area.stat = TRUE} (\strong{default}), the resulting data frame consists
#'   of columns/variables like integrals/areas for each simulated and fitted EPR spectrum, where
#'   the components are denoted by uppercase letters (\code{Area_Sim_A}, \code{Area_Sim_B},...etc.);
#'   best fitted/optimized coefficients to multiply the intensities (\code{Optim_CoeffInt_Sim_A},
#'   \code{Optim_CoeffInt_Sim_B},...etc); best fitted/optimized intercept (or baseline constant,
#'   \code{Optim_intercept}); minimum sum of the residual squares (\code{minLSQ_sum}); number
#'   of evaluations/iterations (\code{N_evals}) and finally convergence information/number (\code{N_converg},
#'   like already described in \code{\link{optim_for_EPR_fitness}}). These variables are presented for each
#'   \code{var2nd.series} (e.g. time) point like example for one EPR spectral component:
#'   \tabular{lcccccc}{
#'   \strong{time_s} \tab \strong{Area_Sim_A} \tab \strong{Optim_CoeffInt_Sim_A} \tab \strong{Optim_intercept} \tab
#'   \strong{minLSQ_sum} \tab \strong{N_evals} \tab \strong{N_converg} \cr
#'   6 \tab 0.020624473 \tab 0.052843937 \tab 5.508809e-10 \tab 2.289953e-07 \tab 198 \tab 4 \cr
#'   21 \tab 0.020217930\tab 0.051802287\tab 5.401823e-10 \tab 2.438172e-07 \tab 177 \tab 4 \cr
#'   36 \tab 0.018836579 \tab 0.048263010 \tab 5.029705e-10 \tab 2.662651e-07 \tab 201 \tab 4 \cr
#'   }
#'
#'   \item Tidy/long table format of the original \code{data.spectra.series} with additional
#'   columns/variables (best fitted simulated intensities) for all spectral components: A, B, C, ...etc.
#'   }
#'
#'
#' @examples
#' \dontrun{
#' ## example with default arguments corresponding
#' ## to one simulated spectral component,
#' ## `optim.params.init` has the length
#' ## of => number of components + 1,
#' ## because of included intercept/baseline constant
#' quant.data.sim.test.a <-
#'   quantify_EPR_Sim_series(data.spectra.series,
#'      dir_ASC_sim = "./",
#'      optim.method = "pswarm",
#'      name.pattern.sim = "DHMB0_1st_04_SimA",
#'      optim.params.init = c(0,0.8),
#'      output.area.stat = TRUE)
#' #
#' ## similar example with two components
#' ## (simulated spectra) and tidy data frame
#' ## output (not the summarized one)
#' quant.data.sim.test.b <-
#'   quantify_EPR_Sim_series(data.spectra.series,
#'      dir_ASC_sim = "./",
#'      optim.method = "sbplx",
#'      name.pattern.sim = "DHMB0_1st_04_Sim[[:upper:]]",
#'      optim.params.init = c(0,0.8,0.2),
#'      output.area.stat = FALSE)
#' #
#' }
#'
#'
#' @export
#'
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr arrange matches across
quantify_EPR_Sim_series <- function(data.spectra.series,
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
                                    tol.step = 5e-7,
                                    pswarm.size = NULL,
                                    pswarm.diameter = NULL,
                                    single.integ = "single_IntegSim",
                                    double.integ = "double_IntegSim",
                                    output.area.stat = TRUE,
                                    ...) {
  ## 'Temporary' processing variables
  . <- NULL
  Area_Sim_aLL <- NULL
  Optim_intercept <- NULL
  minLSQ_sum <- NULL
  N_evals <- NULL
  N_converg <- NULL
  #
  ## Reading simulated EPR spectra from MATLAB or other simulation sources
  ## sim file paths
  pattern.sim.files <- paste0("^", name.pattern.sim, ".*\\.(txt|asc|csv)$")
  sim.file.orig.paths <- list.files(
    path = dir_ASC_sim,
    pattern = pattern.sim.files,
    full.names = TRUE
  )
  ## load all simulation spectral parts at once
  data.specs.orig.sim <-
    lapply(
      sim.file.orig.paths,
      function(f) {
        readEPR_Sim_Spec(f,
          B.unit = B.unit,
          Intensity.sim = Intensity.sim,
          sim.origin = sim.origin
        )
      }
    )
  #
  ## checking number of points for experimental and simulated spectra
  ## experimental
  resolution.exp <- data.spectra.series %>%
    dplyr::filter(.data[[var2nd.series]] == .data[[var2nd.series]][1]) %>%
    dim.data.frame()
  resolution.exp <- resolution.exp[1]
  ## simulation number of rows
  resolution.sim <- sapply(
    data.specs.orig.sim,
    function(r) dim.data.frame(r)[1]
  )
  #
  ## condition to check resolution of all simulations
  if (length(data.specs.orig.sim) > 1) {
    resolution.check <-
      sapply(
        seq(resolution.sim),
        function(c) if (resolution.sim[c] == resolution.exp) TRUE else FALSE
      )
  } else {
    resolution.check <- if (resolution.sim == resolution.exp) TRUE else FALSE
  }

  ## add simulated (non-processed, original) spectra into one long-table format
  ## to all experimental spectra
  if (isFALSE(any(resolution.check))){
    stop(" Number of points for experimental & simulated spectra do not match ! ")
  } else{
    ## adding columns (simulated spectral parts) in a loop
    data.specs.sim <- data.spectra.series
    for (d in seq(data.specs.orig.sim)) {
      data.specs.sim <- data.specs.sim %>%
        dplyr::group_by(.data[[var2nd.series]]) %>%
        dplyr::mutate(!!rlang::quo_name(paste0(Intensity.sim,"_",LETTERS[d])) :=
                        data.specs.orig.sim[[d]][[Intensity.sim]])
    }
    #
    ## delete the original data (not required anymore)
    rm(data.spectra.series)
  }
  #
  # ================== up to this point => everything OK ===================
  #
  ## parameterize and sum of all simulated spectral components (max = 6 !)
  ## `x0 \equiv par` depending on optimization method => AS a FITNESS FUNCTIONS
  ## THEY MUST BE DEFINED SEPARATELY !!
  fit_params_specs_par <- function(data,col.name.pattern,par){
    #
    ## select only simulation component columns (don't do it by `dplyr`!
    ## because it does not work)
    data <- data[,grep(col.name.pattern,colnames(data),value = TRUE)]
    #
    ## create a sum for all columns/simulated spectra
    ## this cannot be done in any loop like `for`, `sapply` or `lapply` !!!
    if (ncol(data) == 1){
      summa <- par[1] + (par[2] * data[[1]])
    }
    if (ncol(data) == 2){
      summa <- par[1] + (par[2] * data[[1]]) + (par[3] * data[[2]])
    }
    if (ncol(data) == 3){
      summa <- par[1] + (par[2] * data[[1]]) + (par[3] * data[[2]]) +
        (par[4] * data[[3]])
    }
    if (ncol(data) == 4){
      summa <- par[1] + (par[2] * data[[1]]) + (par[3] * data[[2]]) +
        (par[4] * data[[3]]) + (par[5] * data[[4]])
    }
    if (ncol(data) == 5){
      summa <- par[1] + (par[2] * data[[1]]) + (par[3] * data[[2]]) +
        (par[4] * data[[3]]) + (par[5] * data[[4]]) +
        (par[6] * data[[5]])
    }
    if (ncol(data) == 6){
      summa <- par[1] + (par[2] * data[[1]]) + (par[3] * data[[2]]) +
        (par[4] * data[[3]]) + (par[5] * data[[4]]) +
        (par[6] * data[[5]]) + (par[7] * data[[6]])
    }
    #
    return(summa)
  }
  ## SECOND FUNCTION
  fit_params_specs_x0 <- function(data,col.name.pattern,x0){
    #
    ## select only simulation component columns (don't do it by `dplyr`!)
    data <- data[,grep(col.name.pattern,colnames(data),value = TRUE)]
    #
    ## create a sum for all columns/simulated spectra
    ## this cannot be done in any loop like `for`, `sapply` or `lapply` !!!
    if (ncol(data) == 1){
      summa <- x0[1] + (x0[2] * data[[1]])
    }
    if (ncol(data) == 2){
      summa <- x0[1] + (x0[2] * data[[1]]) + (x0[3] * data[[2]])
    }
    if (ncol(data) == 3){
      summa <- x0[1] + (x0[2] * data[[1]]) + (x0[3] * data[[2]]) +
        (x0[4] * data[[3]])
    }
    if (ncol(data) == 4){
      summa <- x0[1] + (x0[2] * data[[1]]) + (x0[3] * data[[2]]) +
        (x0[4] * data[[3]]) + (x0[5] * data[[4]])
    }
    if (ncol(data) == 5){
      summa <- x0[1] + (x0[2] * data[[1]]) + (x0[3] * data[[2]]) +
        (x0[4] * data[[3]]) + (x0[5] * data[[4]]) +
        (x0[6] * data[[5]])
    }
    if (ncol(data) == 6){
      summa <- x0[1] + (x0[2] * data[[1]]) + (x0[3] * data[[2]]) +
        (x0[4] * data[[3]]) + (x0[5] * data[[4]]) +
        (x0[6] * data[[5]]) + (x0[7] * data[[6]])
    }
    #
    return(summa)
  }
  #
  ## `var2nd.series` sequence (e.g. time sequence)
  var2nd_df <- data.specs.sim %>%
    dplyr::group_by(.data[[var2nd.series]]) %>%
    dplyr::group_keys()
  ## vector of `var2nd` sequence
  var2nd_seq <- var2nd_df[[var2nd.series]]
  #
  ## data split into data list with filtering
  ## to be ready to optimize each individual spectrum
  data.list <- lapply(
    var2nd_seq,
    function(t) subset(data.specs.sim,
                       data.specs.sim[[var2nd.series]] == t)
  )
  #
  ## Definition of `lower` and `upper` optim. limits of initial params.
  ## e.g following
  lower.limits <- rep(0,times = (length(data.specs.orig.sim) + 1))
  upper.limits <- rep(0.9,times = (length(data.specs.orig.sim) + 1))
  optim.params.lower <- optim.params.lower %>%
    `if`(is.null(optim.params.lower), lower.limits, .)
  optim.params.upper <- optim.params.upper %>%
    `if`(is.null(optim.params.upper), upper.limits, .)
  #
  ## "general" function for optimization because it depends
  ## on only method (`method`) and function (`fun`)
  ## and initial params (`x.0`)
  optim_fn <- function(fun,method,data){
    optim.list <-
      optim_for_EPR_fitness(x.0 = optim.params.init,
          method = method,
          fn = fun,
          lower = optim.params.lower,
          upper = optim.params.upper,
          Nmax.evals = Nmax.evals,
          tol.step = tol.step,
          pswarm.size = pswarm.size,
          pswarm.diameter = pswarm.diameter,
          data = data,
          col.name.pattern = "Sim.*_[[:upper:]]$",
          ...
      )
    #
    return(optim.list)
  }
  #
  ## optimization list by `data.list` methods depending on the `optim.method`
  # min. function for optimization incl. `fit_params_specs()` based on method
  ## + optimization
  if (optim.method == "levenmarq"){
    ## "levelnmarq" is defined by residuals, NOT by sum of the residual squares !!
    min_residuals_lm <- function(data,col.name.pattern,par){
      return(data[[Intensity.expr]] -
               fit_params_specs_par(data,col.name.pattern,par))
    }
    #
    optimization.list <-
      lapply(seq(data.list),
             function(o) optim_fn(method = optim.method,
                                  data = data.list[[o]],
                                  fun = min_residuals_lm))
  }
  if (optim.method == "pswarm"){
    min_residuals_ps <- function(data,col.name.pattern,par){
      with(data,sum((data[[Intensity.expr]] -
                       fit_params_specs_par(data,col.name.pattern,par))^2))
    }
    #
    optimization.list <-
      lapply(seq(data.list),
             function(o) optim_fn(method = optim.method,
                                  data = data.list[[o]],
                                  fun = min_residuals_ps))
  }
  if (optim.method == "slsqp" || optim.method == "neldermead" ||
      optim.method == "crs2lm" || optim.method == "sbplx" ||
      optim.method == "cobyla" || optim.method == "lbfgs"){
    min_residuals_nl <- function(data,col.name.pattern,x0){
      with(data,sum((data[[Intensity.expr]] -
                       fit_params_specs_x0(data,col.name.pattern,x0))^2))
    }
    #
    optimization.list <-
      lapply(seq(data.list),
             function(o) optim_fn(method = optim.method,
                                  data = data.list[[o]],
                                  fun = min_residuals_nl))
  }
  #
  ## data.list is not needed anymore
  rm(data.list)
  #
  ## 1st constants/parameters (shared intercept for
  ## all sim. spectra) into vectors
  optim.vec.x01 <-
    sapply(seq_along(optimization.list),
           function(l) optimization.list[[l]]$par[1]
           )
  #
  ## all additional constants
  optim.list.x0n <-
    lapply(seq_along(optimization.list),
           function(l)
             optimization.list[[l]]$par[2:(length(data.specs.orig.sim) + 1)]
           )
  ## list to data frame
  optim.list.x0n.df <-
    data.frame(matrix(unlist(optim.list.x0n),
                      nrow = length(optim.list.x0n),
                      byrow = TRUE)
               )
  ## column names ("intensity coefficients")
  colnames(optim.list.x0n.df) <-
    sapply(seq(ncol(optim.list.x0n.df)),
           function(n) paste0("coeffInt_Sim_",LETTERS[n])
           )
  #
  ## minimum value for the least-square optimization method
  method.cond <- ifelse(optim.method == "levenmarq",TRUE,FALSE)
  optim.vec.min.val <-
    switch(2-method.cond,
           sapply(seq_along(optimization.list),
                  function(l) optimization.list[[l]]$deviance),
           sapply(seq_along(optimization.list),
                  function(l) optimization.list[[l]]$value)
           )
  #
  ## number of iterations/function evaluations
  if (optim.method == "levenmarq"){
    optim.vec.no.iter <-
      sapply(seq_along(optimization.list),
             function(l) optimization.list[[l]]$niter)
  }
  if (optim.method == "pswarm"){
    optim.vec.no.iter <-
      sapply(seq_along(optimization.list),
             function(l) optimization.list[[l]]$counts[1])
  }
  if (optim.method == "slsqp" || optim.method == "neldermead" ||
      optim.method == "crs2lm" || optim.method == "sbplx" ||
      optim.method == "cobyla" || optim.method == "lbfgs"){
    optim.vec.no.iter <-
      sapply(seq_along(optimization.list),
             function(l) optimization.list[[l]]$iter)
  }
  #
  ## convergence information
  optim.vec.no.converg <-
    switch(2-method.cond,
           sapply(seq_along(optimization.list),
                  function(l) sum(optimization.list[[l]]$rsstrace)),
           sapply(seq_along(optimization.list),
                  function(l) optimization.list[[l]]$convergence)
           )
  #
  ## creating matrix (`mapply` is creating vectors) with modified
  ## ONLY 1st SIMULATION taking into account the coefficients
  ## obtained from optimization ADDITIONAL SIMULATIONS
  ## WILL BE ADDED LATER
  data.specs.sim.modif <- c()
  data.specs.sim.modif[[1]] <-
    mapply(
      function(s, t) s + (t * data.specs.orig.sim[[1]][[Intensity.sim]]),
      optim.vec.x01,
      optim.list.x0n.df$coeffInt_Sim_A
    )
  #
  ## matrix transformed into data frame
  data.specs.sim.modif[[1]] <-
    as.data.frame(data.specs.sim.modif[[1]])
  #
  ## changing the column names
  names(data.specs.sim.modif[[1]]) <- var2nd_seq
  #
  ## adding column of `B` in order to properly
  ## work with `pivot_longer` (see below)
  data.specs.sim.modif[[1]] <-
    cbind(
      data.specs.sim.modif[[1]],
      data.specs.orig.sim[[1]][[paste0("Bsim_", B.unit)]]
    )
  #
  ## renaming the last column with `B`
  names(data.specs.sim.modif[[1]])[ncol(data.specs.sim.modif[[1]])] <-
    paste0("Bsim_",B.unit)
  #
  ## transformation from wide table to long table
  ## with properly arranged var2nd.series
  data.specs.sim.modif[[1]] <-
    data.specs.sim.modif[[1]] %>%
    tidyr::pivot_longer(!dplyr::all_of(c(paste0("Bsim_", B.unit))),
      names_to = var2nd.series,
      values_to = paste0(Intensity.sim, "_", LETTERS[1])
    ) %>%
    dplyr::mutate(!!rlang::quo_name(var2nd.series) :=
      as.double(as.character(.data[[var2nd.series]]))) %>%
    dplyr::arrange(.data[[var2nd.series]])
  #
  ## the last column of the previous data added
  ## to origin complex data frame `data.specs.sim`
  data.specs.sim[[paste0(Intensity.sim,"_",LETTERS[1])]] <- NULL
  data.specs.sim[[paste0(Intensity.sim,"_",LETTERS[1])]] <-
    data.specs.sim.modif[[1]][[paste0(Intensity.sim,"_",LETTERS[1])]]
  #
  ## creating matrix (`mapply` is creating vectors)
  ## with modified ADDITIONAL SIMULATIONs (B,...)
  ## taking into account the coefficients obtained from optimization
  if (length(data.specs.orig.sim) > 1){
    ## however before delete all `B` & additional... simulations
    ## from the `fundamental` data frame
    for (d in 2:length(data.specs.orig.sim)){
      data.specs.sim <- data.specs.sim %>%
        dplyr::select(-dplyr::matches(paste0("Sim.*_",LETTERS[d],"$")))
    }
    #
    for (d in 2:length(data.specs.orig.sim)) {
      data.specs.sim.modif[[d]] <-
        mapply(function(w) w*data.specs.orig.sim[[d]][[Intensity.sim]],
               optim.list.x0n.df[[d]])
      #
      ## matrix transformed into data frame
      data.specs.sim.modif[[d]] <-
        as.data.frame(data.specs.sim.modif[[d]])
      #
      ## changing the column names
      names(data.specs.sim.modif[[d]]) <- var2nd_seq
      ## adding column of `B` in order to properly
      ## work with `pivot_longer` (see below)
      #
      data.specs.sim.modif[[d]] <-
        cbind(data.specs.sim.modif[[d]],
              data.specs.orig.sim[[d]][[paste0("Bsim_",B.unit)]])
      #
      ## renaming the last column with `B`
      names(data.specs.sim.modif[[d]])[ncol(data.specs.sim.modif[[d]])] <-
        paste0("Bsim_",B.unit)
      #
      ## transformation from wide table to long table with properly arranged time
      data.specs.sim.modif[[d]] <-
        data.specs.sim.modif[[d]] %>%
        tidyr::pivot_longer(!dplyr::all_of(c(paste0("Bsim_", B.unit))),
          names_to = var2nd.series,
          values_to = paste0(Intensity.sim, "_", LETTERS[d])
        ) %>%
        dplyr::mutate(!!rlang::quo_name(var2nd.series) :=
          as.double(as.character(.data[[var2nd.series]]))) %>%
        dplyr::arrange(.data[[var2nd.series]])
      #
      ## adding all columns from the previous temporary
      ## data frames to `data.specs.sim`
      data.specs.sim[[paste0(Intensity.sim,"_",LETTERS[d])]] <-
        data.specs.sim.modif[[d]][[paste0(Intensity.sim,"_",LETTERS[d])]]
    }
    #
  }
  #
  ## removing the `data.specs.sim.modif` which is not needed anymore
  rm(data.specs.sim.modif)
  #
  ## Sum of all simulated spectra intensities
  if (length(data.specs.orig.sim) > 1){
    data.specs.sim <- data.specs.sim %>%
      dplyr::group_by(.data[[var2nd.series]]) %>%
      dplyr::mutate(!!rlang::quo_name(paste0(Intensity.sim, "_aLL")) :=
        rowSums(dplyr::across(dplyr::matches("Sim.*_[[:upper:]]$"))))
  }
  #
  ## -------------------------- INTEGRATION -----------------------------
  ## therefore function to distinguish between units =>
  fn_units <- function(unit){
    if (unit == "G"){
      return(0)
    }
    if (unit == "mT"){
      return(1)
    }
    if (unit == "T"){
      return(2)
    }
  }
  #
  ## data substitution/renaming
  result_df_base <- data.specs.sim
  #
  for (d in seq(data.specs.orig.sim)) {
    result_df_base <- result_df_base %>%
      dplyr::group_by(.data[[var2nd.series]]) %>%
      dplyr::mutate(!!rlang::quo_name(paste0(single.integ,"_",LETTERS[d])) :=
                    switch(3-fn_units(unit = B.unit),
                           pracma::cumtrapz(.data[[paste0("B_", B.unit)]],
                                     .data[[paste0(Intensity.sim,"_",LETTERS[d])]])[,1] * 1e+4,
                           pracma::cumtrapz(.data[[paste0("B_", B.unit)]],
                                     .data[[paste0(Intensity.sim,"_",LETTERS[d])]])[,1] * 10,
                           pracma::cumtrapz(.data[[paste0("B_", B.unit)]],
                                     .data[[paste0(Intensity.sim,"_",LETTERS[d])]])[,1]
      )
    )
  }
  if (length(data.specs.orig.sim) > 1){
    ## single integration of the overall spectrum/signal
    result_df_base <- result_df_base %>%
      dplyr::group_by(.data[[var2nd.series]]) %>%
      dplyr::mutate(!!rlang::quo_name(paste0(single.integ,"_aLL")) :=
                    switch(3-fn_units(unit = B.unit),
                           pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                            .data[[paste0(Intensity.sim,"_aLL")]])[,1] * 1e+4,
                           pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                            .data[[paste0(Intensity.sim,"_aLL")]])[,1] * 10,
                           pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                            .data[[paste0(Intensity.sim,"_aLL")]])[,1]
      )
    )
  }
  #
  ## remove `data.specs.sim` which is not required anymore
  rm(data.specs.sim)
  #
  #
  ## ----------------------- AREAS AND RUSULTS ------------------------
  #
  if (isFALSE(output.area.stat)){
    if (is.null(double.integ)){
      result_df <- result_df_base
    } else{
      ## double_integrals
      ## data substitution/renaming
      result_df <- result_df_base
      #
      for(d in seq(data.specs.orig.sim)){
        result_df <- result_df %>%
          dplyr::group_by(.data[[var2nd.series]]) %>%
          dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_",LETTERS[d])) :=
                        switch(3-fn_units(unit = B.unit),
                               pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                          .data[[paste0(single.integ,"_",LETTERS[d])]])[,1] * 1e+4,
                               pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                          .data[[paste0(single.integ,"_",LETTERS[d])]])[,1] * 10,
                               pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                          .data[[paste0(single.integ,"_",LETTERS[d])]])[,1]
          )
        )
      }
      #
      if (length(data.specs.orig.sim) > 1){
        result_df <- result_df %>%
          dplyr::group_by(.data[[var2nd.series]]) %>%
          dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_aLL")) :=
                          switch(3-fn_units(unit = B.unit),
                                 pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                                  .data[[paste0(single.integ,"_aLL")]])[,1] * 1e+4,
                                 pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                                  .data[[paste0(single.integ,"_aLL")]])[,1] * 10,
                                 pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                                  .data[[paste0(single.integ,"_aLL")]])[,1]
          )
        )
      }
    }
  } else{
    ## calculation of areas by `summarize` + add statistic/info from optimization
    if (is.null(double.integ)){
      ## summarize in loop for all components
      result_df <- c()
      for(d in seq(data.specs.orig.sim)){
        result_df[[d]] <- result_df_base %>%
          dplyr::summarize(!!rlang::quo_name(paste0("Area_Sim_",LETTERS[d])) :=
                      max(.data[[paste0(single.integ,"_",LETTERS[d])]])) %>%
          dplyr::mutate(!!rlang::quo_name(paste0("Optim_coeffInt_Sim_",LETTERS[d])) :=
                          optim.list.x0n.df[[d]])
      }
      #
      ## the resulting data frame (without additional var2nd columns)
      result_df <- data.frame(result_df) %>%
        dplyr::select(-dplyr::matches("\\.[[:digit:]]"))
      #
      if (length(data.specs.orig.sim) > 1){
        result_df_Sim_aLL <- result_df_base %>%
          dplyr::summarize(Area_Sim_aLL =
                             max(.data[[paste0(single.integ,"_aLL")]])) %>%
          dplyr::select(-.data[[var2nd.series]])
        result_df <- cbind.data.frame(result_df,result_df_Sim_aLL)
      }
    } else{
      ## double_integrals
      result_df <- c()
      #
      for (d in seq(data.specs.orig.sim)){
        result_df[[d]] <- result_df_base %>%
          dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_",LETTERS[d])) :=
                        switch(3-fn_units(unit = B.unit),
                               pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                            .data[[paste0(single.integ,"_",LETTERS[d])]])[,1] * 1e+4,
                               pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                            .data[[paste0(single.integ,"_",LETTERS[d])]])[,1] * 10,
                               pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                            .data[[paste0(single.integ,"_",LETTERS[d])]])[,1]
            )
          ) %>%
          dplyr::summarize(!!rlang::quo_name(paste0("Area_Sim_",LETTERS[d])) :=
                             max(.data[[paste0(double.integ,"_",LETTERS[d])]])) %>%
          dplyr::mutate(!!rlang::quo_name(paste0("Optim_coeffInt_Sim_",LETTERS[d])) :=
                          optim.list.x0n.df[[d]]
          )
      }
      #
      ## resulting data frame (without additional var2nd columns)
      result_df <- data.frame(result_df) %>%
        dplyr::select(-dplyr::matches("\\.[[:digit:]]"))
      #
      if (length(data.specs.orig.sim) > 1){
        result_df_Sim_aLL <- result_df_base %>%
          dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_aLL")) :=
                        switch(3-fn_units(unit = B.unit),
                               pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                                .data[[paste0(single.integ,"_aLL")]])[,1] * 1e+4,
                               pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                                .data[[paste0(single.integ,"_aLL")]])[,1] * 10,
                               pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                                .data[[paste0(single.integ,"_aLL")]])[,1]
            )
          ) %>%
          dplyr::summarize(Area_Sim_aLL = max(.data[[paste0(double.integ,"_aLL")]])) %>%
          dplyr::select(!dplyr::all_of(c(var2nd.series)))
        result_df <- cbind.data.frame(result_df,result_df_Sim_aLL)
      }
     #
    }
    #
    ## adding available info/statistics about optimization
    result_df <- result_df %>%
      dplyr::mutate(Optim_intercept = optim.vec.x01) %>%
      dplyr::mutate(minLSQ_sum = optim.vec.min.val) %>%
      dplyr::mutate(N_evals = optim.vec.no.iter) %>%
      dplyr::mutate(N_converg = optim.vec.no.converg)
  }
  #
  return(result_df)
  #
}
