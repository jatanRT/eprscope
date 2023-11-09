#
#' Quantify (Components) Area of Simulated EPR Spectral Series Instead of Experimental One
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description tbc
#'
#'
#' @param data.spectra.series tbc
#' @param dir_ASC_sim tbc
#' @param name_pattern_sim description
#' @param sim.origin description from
#' @param var2nd.series String/Character referred to name of the second independent variable/quantity
#'   column in the original \code{data.spectra} (e.g. like `time`,`Temperature`, `Electrochemical Potential`,
#'   `Microwave Power`...etc) altered upon individual experiments as a second variable
#'   (\code{var2nd.series}) and related to spectra/data. Data must be available in \strong{long table}
#'   (or \strong{tidy}) \strong{format} (see also \code{\link{readEPR_Exp_Specs_multif}}).
#'   \strong{Default}: \code{var2nd.series = NULL}. Otherwise \strong{usually} \code{var2nd.series = "time_s"}.
#' @param B.unit tbc
#' @param Intensity.expr tbc
#' @param Intensity.sim tbc
#' @param optim.method Character string description tbc...following methods from \pkg{nloptr} are available:
#'   \code{optim.method = "slsqp"} (\strong{default}), \code{optim.method = "neldermead"},
#'   \code{optim.method = "mma"} and \code{optim.method = "ccsaq"}...tbc
#' @param optim.params.init Numeric vector...description tbc...inherited from \code{x0} parameter/argument
#'   of a \pkg{nloptr} function (see e.g. \code{\link[nloptr]{mma}})...tbc
#' @param optim.params.lower Numeric vector...description tbc...inherited from \code{lower} parameter/argument
#'   of a \pkg{nloptr} function (see e.g. \code{\link[nloptr]{mma}})...tbc.
#' @param optim.params.upper Numeric vector...description tbc...inherited from \code{upper} parameter/argument
#'   of a \pkg{nloptr} function (see e.g. \code{\link[nloptr]{mma}})...tbc.
#' @param Nmax.evals Numeric, maximum naumber of function evalutions or iterations.
#' @param tol.step Numeric, the smallest optimization step (relative change) to stop
#'   the optimization or fitting procedure.
#' @param single.integ tbc
#' @param double.integ tbc can be also \code{NULL} in case of single integral spectral series input
#' @param output.area.stat tbc
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
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr arrange matches across
quantify_EPR_Sim_series <- function(data.spectra.series,
                                    dir_ASC_sim,
                                    name_pattern_sim,
                                    sim.origin = "easyspin",
                                    var2nd.series = "time_s",
                                    B.unit = "G",
                                    Intensity.expr = "dIepr_over_dB",
                                    Intensity.sim = "dIeprSim_over_dB",
                                    optim.method = "slsqp",
                                    optim.params.init,
                                    optim.params.lower = NULL,
                                    optim.params.upper = NULL,
                                    Nmax.evals = 2000,
                                    tol.step = 1e-6,
                                    single.integ = "single_IntegSim",
                                    double.integ = "double_IntegSim",
                                    output.area.stat = TRUE) {
  ## 'Temporary' processing variables
  . <- NULL
  Area_Sim_aLL <- NULL
  Optim_intercept <- NULL
  Optim_minLSQ_sum <- NULL
  Optim_No_iters <- NULL
  Optim_N_converg <- NULL
  #
  ## Reading simulated EPR spectra from MATLAB
  ## sim file paths
  pattern.sim.files <- paste0("^",name_pattern_sim,".*\\.(txt|asc|csv)$")
  sim.file.orig.paths <- list.files(path = dir_ASC_sim,
                                    pattern = pattern.sim.files,
                                    full.names = TRUE)
  ## load all simulation spectral parts at once
  data.specs.orig.sim <-
    lapply(sim.file.orig.paths,
           function(f) readEPR_Sim_Spec(f,
                                        B.unit = B.unit,
                                        Intensity.sim = Intensity.sim,
                                        sim.origin = sim.origin))
  #
  ## checking number of points for experimental and simulated spectra
  ## experimental
  resolution.exp <- data.spectra.series %>%
    dplyr::filter(.data[[var2nd.series]] == .data[[var2nd.series]][1]) %>%
    dim.data.frame()
  resolution.exp <- resolution.exp[1]
  ## simulation number of rows
  resolution.sim <- sapply(data.specs.orig.sim,
                           function(r) dim.data.frame(r)[1])
  #
  ## condition to check resolution of all simulations
  if (length(data.specs.orig.sim) > 1){
    resolution.check <-
      sapply(seq(resolution.sim),
             function(c) if (resolution.sim[c] == resolution.exp) TRUE else FALSE)
  } else{
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
    ## delete the original data (not needed anymore)
    rm(data.spectra.series)
  }
  #
  # ================== up to this point => everything OK ===================
  #
  ## parameterize and sum of all simulated spectral components (max = 6 !)
  ## `x0 \equiv par`
  fit_params_specs <- function(data,col.name.pattern,x0){
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
        (x0[4] * data[[3]]) + (x0[5] * data[[4]]) + (x0[6] * data[[5]])
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
  ## min. function for optimization incl. `fit_params_specs()`
  min_residuals <- function(data,col.name.pattern,x0){
    with(data,sum((data[[Intensity.expr]] - fit_params_specs(data,col.name.pattern,x0))^2))
  }
  #
  ## `var2nd.series` sequence (e.g. like time sequence)
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
    function(t) subset(data.specs.sim, data.specs.sim[[var2nd.series]] == t)
  )
  #
  ## Definition of `lower` and `upper` optim. limits of initial params.
  ## e.g following
  lower.limits <- rep(0,times = length(data.specs.orig.sim) + 1)
  upper.limits <- rep(0.9,times = length(data.specs.orig.sim) + 1)
  optim.params.lower <- optim.params.lower %>%
    `if`(is.null(optim.params.lower), lower.limits, .)
  optim.params.upper <- optim.params.upper %>%
    `if`(is.null(optim.params.upper), upper.limits, .)
  #
  ## optimization list by `data.list`
  optimization.list <-
    lapply(seq(data.list),
           function(o)
             optim_EPR_by_nloptr(method = optim.method,
                                 x.0 = optim.params.init,
                                 fn = min_residuals,
                                 lower = optim.params.lower,
                                 upper = optim.params.upper,
                                 Nmax.evals = Nmax.evals,
                                 tol.step = tol.step,
                                 data = data.list[[o]],
                                 col.name.pattern = "Sim.*_[[:upper:]]$")
           )
  #
  ## data.list is not needed anymore
  rm(data.list)
  #
  ## 1st constants/parameters (shared intercept for all sim. spectra) into vectors
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
  ## minimal value for the least-square optimization method
  optim.vec.min.val <-
    sapply(seq_along(optimization.list),
           function(l) optimization.list[[l]]$value
           )
  #
  ## number of iterations/function evaluations
  optim.vec.no.iter <-
    sapply(seq_along(optimization.list),
           function(l) optimization.list[[l]]$iter
           )
  #
  ## convergence => integer code indicating successful completion (> 0)
  optim.vec.no.converg <-
    sapply(seq_along(optimization.list),
                     function(l) optimization.list[[l]]$convergence
           )
  #
  ## creating matrix (`mapply` is creating vectors) with modified ONLY 1st SIMULATION
  ## taking into account the coefficients obtained from optimization
  ## ADDITIONAL SIMULATIONS WILL BE ADDED LATER
  data.specs.sim.modif <- c()
  data.specs.sim.modif[[1]] <-
    mapply(function(s,t) s + t*data.specs.orig.sim[[1]][[Intensity.sim]],
           optim.vec.x01,
           optim.list.x0n.df$coeffInt_Sim_A)
  #
  ## matrix transformed into data frame
  data.specs.sim.modif[[1]] <- as.data.frame(data.specs.sim.modif[[1]])
  ## changing the column names
  names(data.specs.sim.modif[[1]]) <- var2nd_seq
  ## adding column of `B` in order to properly work with `pivot_longer` (see below)
  data.specs.sim.modif[[1]] <- cbind(data.specs.sim.modif[[1]],
                                     data.specs.orig.sim[[1]][[paste0("Bsim_",B.unit)]])
  ## renaming the last column with `B`
  names(data.specs.sim.modif[[1]])[ncol(data.specs.sim.modif[[1]])] <- paste0("Bsim_",B.unit)
  #
  ## transformation from wide table to long table with properly arranged var2nd.series
  data.specs.sim.modif[[1]] <- data.specs.sim.modif[[1]] %>%
    tidyr::pivot_longer(!.data[[paste0("Bsim_",B.unit)]],
                        names_to = var2nd.series,
                        values_to = paste0(Intensity.sim,"_",LETTERS[1])) %>%
    dplyr::mutate(!!rlang::quo_name(var2nd.series) :=
                    as.double(as.character(.data[[var2nd.series]]))) %>%
    dplyr::arrange(.data[[var2nd.series]])
  #
  ## the last column of the previous data added to origin complex data frame `data.specs.sim`
  data.specs.sim[[paste0(Intensity.sim,"_",LETTERS[1])]] <- NULL
  data.specs.sim[[paste0(Intensity.sim,"_",LETTERS[1])]] <-
    data.specs.sim.modif[[1]][[paste0(Intensity.sim,"_",LETTERS[1])]]
  #
  ## creating matrix (`mapply` is creating vectors) with modified ADDITIONAL SIMULATIONs (B,...)
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
      data.specs.sim.modif[[d]] <- as.data.frame(data.specs.sim.modif[[d]])
      ## changing the column names
      names(data.specs.sim.modif[[d]]) <- var2nd_seq
      ## adding column of `B` in order to properly work with `pivot_longer` (see below)
      data.specs.sim.modif[[d]] <- cbind(data.specs.sim.modif[[d]],
                                         data.specs.orig.sim[[d]][[paste0("Bsim_",B.unit)]])
      ## renaming the last column with `B`
      names(data.specs.sim.modif[[d]])[ncol(data.specs.sim.modif[[d]])] <- paste0("Bsim_",B.unit)
      #
      ## transformation from wide table to long table with properly arranged time
      data.specs.sim.modif[[d]] <- data.specs.sim.modif[[d]] %>%
        tidyr::pivot_longer(!.data[[paste0("Bsim_",B.unit)]],
                            names_to = var2nd.series,
                            values_to = paste0(Intensity.sim,"_",LETTERS[d])) %>%
        dplyr::mutate(!!rlang::quo_name(var2nd.series) :=
                        as.double(as.character(.data[[var2nd.series]]))) %>%
        dplyr::arrange(.data[[var2nd.series]])
      #
      ## adding all columns from the previous temporary data frames to `data.specs.sim`
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
      dplyr::mutate(!!rlang::quo_name(paste0(Intensity.sim,"_aLL")) :=
                      rowSums(dplyr::across(dplyr::matches("Sim.*_[[:upper:]]$"))))
  }
  #
  ## INTEGRATION
  ## data substitution/renaming
  result_df_base <- data.specs.sim
  #
  if (B.unit == "G"){
    ## single integration
    for (d in seq(data.specs.orig.sim)) {
      result_df_base <- result_df_base %>%
        dplyr::group_by(.data[[var2nd.series]]) %>%
        dplyr::mutate(!!rlang::quo_name(paste0(single.integ,"_",LETTERS[d])) :=
                        pracma::cumtrapz(.data[[paste0("B_", B.unit)]],
                                         .data[[paste0(Intensity.sim,"_",LETTERS[d])]])[,1])
    }
    if (length(data.specs.orig.sim) > 1){
      ## single integration of the overall spectrum/signal
      result_df_base <- result_df_base %>%
        dplyr::group_by(.data[[var2nd.series]]) %>%
        dplyr::mutate(!!rlang::quo_name(paste0(single.integ,"_aLL")) :=
                        pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                         .data[[paste0(Intensity.sim,"_aLL")]])[,1])
    }
   #
  }
  #
  if (B.unit == "mT"){
    ## single integration
    for (d in seq(data.specs.orig.sim)) {
      result_df_base <- result_df_base %>%
        dplyr::group_by(.data[[var2nd.series]]) %>%
        dplyr::mutate(!!rlang::quo_name(paste0(single.integ,"_",LETTERS[d])) :=
                        pracma::cumtrapz(.data[[paste0("B_", B.unit)]],
                                         .data[[paste0(Intensity.sim,"_",LETTERS[d])]])[,1]*10)
    }
    if (length(data.specs.orig.sim) > 1){
      ## single integration of the overall spectrum/signal
      result_df_base <- result_df_base %>%
        dplyr::group_by(.data[[var2nd.series]]) %>%
        dplyr::mutate(!!rlang::quo_name(paste0(single.integ,"_aLL")) :=
                        pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                         .data[[paste0(Intensity.sim,"_aLL")]])[,1]*10)
    }
    #
    ## remove `data.specs.sim` which is not required anymore
    rm(data.specs.sim)
    #
  }
  #
  if (isFALSE(output.area.stat)){
    if (is.null(double.integ)){
      result_df <- result_df_base
    } else{
      ## double_integrals
      ## data substitution/renaming
      result_df <- result_df_base
      if (B.unit == "G"){
        for(d in seq(data.specs.orig.sim)){
          result_df <- result_df %>%
            dplyr::group_by(.data[[var2nd.series]]) %>%
            dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_",LETTERS[d])) :=
                            pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                             .data[[paste0(single.integ,"_",LETTERS[d])]])[,1])
        }
        #
        if (length(data.specs.orig.sim) > 1){
          result_df <- result_df %>%
            dplyr::group_by(.data[[var2nd.series]]) %>%
            dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_aLL")) :=
                            pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                             .data[[paste0(single.integ,"_aLL")]])[,1])
        }
      }
      if (B.unit == "mT"){
        for(d in seq(data.specs.orig.sim)){
          result_df <- result_df %>%
            dplyr::group_by(.data[[var2nd.series]]) %>%
            dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_",LETTERS[d])) :=
                            pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                             .data[[paste0(single.integ,"_",LETTERS[d])]])[,1]*10)
        }
        #
        if (length(data.specs.orig.sim) > 1){
          result_df <- result_df %>%
            dplyr::group_by(.data[[var2nd.series]]) %>%
            dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_aLL")) :=
                            pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                             .data[[paste0(single.integ,"_aLL")]])[,1]*10)
        }
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
      ## the resulting data frame (without additional var2nd columns)
      result_df <- data.frame(result_df) %>% dplyr::select(-dplyr::matches("\\.[[:digit:]]"))
      #
      if (length(data.specs.orig.sim) > 1){
        result_df_Sim_aLL <- result_df_base %>%
          dplyr::summarize(Area_Sim_aLL = max(.data[[paste0(single.integ,"_aLL")]])) %>%
          dplyr::select(-.data[[var2nd.series]])
        result_df <- cbind.data.frame(result_df,result_df_Sim_aLL)
      }
    } else{
      ## double_integrals
      result_df <- c()
      if (B.unit == "G"){
        for (d in seq(data.specs.orig.sim)){
          result_df[[d]] <- result_df_base %>%
            dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_",LETTERS[d])) :=
                            pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                             .data[[paste0(single.integ,"_",LETTERS[d])]])[,1]) %>%
            dplyr::summarize(!!rlang::quo_name(paste0("Area_Sim_",LETTERS[d])) :=
                               max(.data[[paste0(double.integ,"_",LETTERS[d])]])) %>%
            dplyr::mutate(!!rlang::quo_name(paste0("Optim_coeffInt_Sim_",LETTERS[d])) :=
                            optim.list.x0n.df[[d]])
        }
        ## the resulting data frame (without additional var2nd columns)
        result_df <- data.frame(result_df) %>% dplyr::select(-dplyr::matches("\\.[[:digit:]]"))

        #
        if (length(data.specs.orig.sim) > 1){
          result_df_Sim_aLL <- result_df_base %>%
            dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_aLL")) :=
                            pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                             .data[[paste0(single.integ,"_aLL")]])[,1]) %>%
            dplyr::summarize(Area_Sim_aLL = max(.data[[paste0(double.integ,"_aLL")]])) %>%
            dplyr::select(-.data[[var2nd.series]])
          result_df <- cbind.data.frame(result_df,result_df_Sim_aLL)
        }
      }
      if (B.unit == "mT"){
        for (d in seq(data.specs.orig.sim)){
          result_df[[d]] <- result_df_base %>%
            dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_",LETTERS[d])) :=
                            pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                             .data[[paste0(single.integ,"_",LETTERS[d])]])[,1]*10) %>%
            dplyr::summarize(!!rlang::quo_name(paste0("Area_Sim_",LETTERS[d])) :=
                               max(.data[[paste0(double.integ,"_",LETTERS[d])]])) %>%
            dplyr::mutate(!!rlang::quo_name(paste0("Optim_coeffInt_Sim_",LETTERS[d])) :=
                            optim.list.x0n.df[[d]])
        }
        ## the resulting data frame (without additional var2nd columns)
        result_df <- data.frame(result_df) %>% dplyr::select(-dplyr::matches("\\.[[:digit:]]"))

        #
        if (length(data.specs.orig.sim) > 1){
          result_df_Sim_aLL <- result_df_base %>%
            dplyr::mutate(!!rlang::quo_name(paste0(double.integ,"_aLL")) :=
                            pracma::cumtrapz(.data[[paste0("B_",B.unit)]],
                                             .data[[paste0(single.integ,"_aLL")]])[,1]*10) %>%
            dplyr::summarize(Area_Sim_aLL = max(.data[[paste0(double.integ,"_aLL")]])) %>%
            dplyr::select(-.data[[var2nd.series]])
          result_df <- cbind.data.frame(result_df,result_df_Sim_aLL)
        }
      }
    }
    #
    ## adding available info/statistics about optimization
    result_df <- result_df %>%
      dplyr::mutate(Optim_intercept = optim.vec.x01) %>%
      dplyr::mutate(Optim_minLSQ_sum = optim.vec.min.val) %>%
      dplyr::mutate(Optim_No_iters = optim.vec.no.iter) %>%
      dplyr::mutate(Optim_N_converg = optim.vec.no.converg)
  }
  #
  return(result_df)
  #
}
