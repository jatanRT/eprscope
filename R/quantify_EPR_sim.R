#
#' Quantify (Component) Area of Simulated EPR Spectral Series Instead of Experimental One
#'
#'
#' @description tbc
#'
#'
#' @param data.spectra.series tbc
#' @param dir_ASC_sim tbc
#' @param pattern_sim description
#' @param var2nd.series String/Character referred to name of the second independent variable/quantity
#'   column in the original \code{data.spectra} (e.g. like `time`,`Temperature`, `Electrochemical Potential`,
#'   `Microwave Power`...etc) altered upon individual experiments as a second variable
#'   (\code{var2nd.series}) and related to spectra/data. Data must be available in \strong{long table}
#'   (or \strong{tidy}) \strong{format} (see also \code{\link{readEPR_Exp_Specs_multif}}).
#'   \strong{Default}: \code{var2nd.series = NULL}. Otherwise \strong{usually} \code{var2nd.series = "time_s"}.
#' @param B.unit tbc
#' @param Intensity.exp tbc
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
#' @param single.integ tbc
#' @param double.integ tbc can be also \code{NULL} if case of single integral spectral series input
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
#' @importFrom dplyr arrange
#' @importFrom collapse fsubset
#' @importFrom nloptr slsqp neldermead mma ccsaq
quantify_EPR_sim <- function(data.spectra.series,
                             dir_ASC_sim,
                             pattern_sim,
                             var2nd.series = "time_s",
                             B.unit = "G",
                             Intensity.exp = "dIeprExp_over_dB",
                             Intensity.sim = "dIeprSim_over_dB",
                             optim.method = "slsqp",
                             optim.params.init,
                             optim.params.lower = NULL,
                             optim.params.upper = NULL,
                             single.integ = "single_IntegSim",
                             double.integ = "double_IntegSim",
                             output.area.stat = TRUE) {
  ## 'Temporary' processing variables
  . <- NULL
  AreaSim <- NULL
  #
  ## Reading simulated EPR spectra from MATLAB
  ## sim file paths
  pattern.sim.files <- paste0("^",pattern_sim,".*\\.txt$")
  sim.file.orig.paths <- list.files(path = dir_ASC_sim,
                                    pattern = pattern.sim.files,
                                    full.names = TRUE)
  ## load all simulation spectral parts at once
  data.specs.orig.sim <- lapply(sim.file.orig.paths, function(f) readEPR_Sim_Spec(f))
  #
  ## checking number of points for experimental and simulated spectra
  ## experimental
  resolution.exp <- data.specs.exp %>%
    dplyr::filter(.data[[var2nd.series]] == .data[[var2nd.series]][1]) %>%
    dim.data.frame()
  resolution.exp <- resolution.exp[1]
  ## simulation number of rows
  resolution.sim <- sapply(data.specs.orig.sim,
                           function(r) dim.data.frame(r)[1])
  #
  ## condition to check resolution of all simulations
  resolution.check <- sapply(seq(resolution.sim),
                             function(c) if (resolution.sim[c] == resolution.exp) TRUE else FALSE)
  ## add simulated (non-processed, original) spectra into one long-table format
  ## to all experimental spectra
  if (isFALSE(any(resolution.check))){
    stop(" Number of points for experimental & simulated spectra do not match ! ")
  } else{
    ## adding columns (simulated spectral parts) in a loop
    data.specs.sim <- data.specs.exp
    for (d in seq(data.specs.orig.sim)) {
      data.specs.sim <- data.specs.sim %>%
        dplyr::group_by(.data[[var2nd.series]]) %>%
        dplyr::mutate(!!rlang::quo_name(paste0(Intensity.sim,"_",LETTERS[d])) :=
                        data.specs.orig.sim[[d]][[2]])
    }
    #
    ## delete the original data (not needed anymore)
    rm(data.specs.exp,data.all.specs.exp)
  }
  ## parameterize and sum of all simulated spectral components (max = 6 !)
  ## `x0 \equiv par`
  fit_params_specs <- function(data,col.name.pattern,x0){
    #
    ## select only simulation component columns (don't do it by `dplyr`!)
    data <- data[grep(col.name.pattern,colnames(data),value = TRUE)]
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
    with(data,sum((data[["dIeprExp_over_dB"]] - fit_params_specs(data,col.name.pattern,x0))^2))
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
    function(t) collapse::fsubset(data.specs.sim, data.specs.sim[[var2nd.series]] == t)
  )
  ## optimization function
  optim.func <- function(method = "slsqp",x0,fn,lower,upper,data,col.name.pattern){
    if (method == "slsqp"){
      return(nloptr::slsqp(x0 = x0,fn = fn,
                           lower = lower,upper = upper,
                           nl.info = FALSE,data = data,
                           col.name.pattern = col.name.pattern))
    }
    if (method == "neldermead"){
      return(nloptr::neldermead(x0 = x0,fn = fn,
                                lower = lower,upper = upper,
                                nl.info = FALSE,data = data,
                                col.name.pattern = col.name.pattern))
    }
    if (method == "mma"){
      return(nloptr::mma(x0 = x0,fn = fn,
                         lower = lower,upper = upper,
                         nl.info = FALSE,data = data,
                         col.name.pattern = col.name.pattern))
    }
    if (method == "ccsaq"){
      return(nloptr::ccsaq(x0 = x0,fn = fn,
                           lower = lower,upper = upper,
                           nl.info = FALSE,data = data,
                           col.name.pattern = col.name.pattern))
    }
  }
  #
  ## Definition of `lower` and `upper` optim. limits of initial params.
  ## e.g following
  lower.limits <- rep(0,times = length(data.specs.orig.sim))
  upper.limits <- rep(0.8,times = length(data.specs.orig.sim))
  optim.params.lower <- optim.params.lower %>%
    `if`(is.null(optim.params.lower), lower.limits, .)
  optim.params.upper <- optim.params.upper %>%
    `if`(is.null(optim.params.upper), upper.limits, .)
  #
  ## optimization list by `data.list`
  optimization.list <- lapply(seq(data.list),
                              function(o) optim.func(method = optim.method,
                                                     x0 = optim.params.init,
                                                     fn = min_residuals,
                                                     lower = optim.params.lower,
                                                     upper = optim.params.upper,
                                                     data = data.list[[o]],
                                                     col.name.pattern = "Sim.*_[[:upper:]]$"))
  #
  ## data.list is not needed anymore
  rm(data.list)
  #
  ## 1st constants/parameters (shared intercept for all sim. spectra) into vectors
  optim.vec.x01 <- sapply(seq_along(optimization.list),
                          function(l) optimization.list[[l]]$par[1])
  #
  ## all additional constants
  optim.list.x0n <- lapply(seq_along(optimization.list),
                           function(l) optimization.list[[l]]$par[2:(length(data.specs.orig.sim) + 1)])
  ## list to data frame
  optim.list.x0n.df <- data.frame(matrix(unlist(optim.list.x0n),
                                         nrow = length(optim.list.x0n),
                                         byrow = TRUE))
  ## column names ("intensity spectral weights")
  colnames(optim.list.x0n.df) <- sapply(seq(ncol(optim.list.x0n.df)),
                                        function(n) paste0("weight_Sim",LETTERS[n]))
  #
  ## minimal value for the least-square optimization method
  optim.vec.min.val <- sapply(seq_along(optimization.list),
                              function(l) optimization.list[[l]]$value)
  #
  ## number of iterations/function evaluations
  optim.vec.no.iter <- sapply(seq_along(optimization.list),
                              function(l) optimization.list[[l]]$iter)
  #
  ## convergence => integer code indicating successful completion (> 0)
  optim.vec.no.converg <- sapply(seq_along(optimization.list),
                                 function(l) optimization.list[[l]]$convergence)
  #


















  ## creating matrix (`mapply` is creating vectors) with modified simulations
  ## calculating by previous coefficients obtained from optimization
  data.specs.sim.modif <-
    mapply(
      function(s, t) s + t * data.spec.sim[[Intensity.sim]],
      optim.vec.c1,
      optim.vec.c2
    )
  #
  ## matrix transformed into data frame
  data.specs.sim.modif <- collapse::qDF(data.specs.sim.modif)
  ## modifying & changing the column names (incl. adding column of `B`
  ## in order to properly work with `pivot_longer` (see below))
  data.specs.sim.modif <- cbind(
    data.specs.sim.modif,
    data.spec.sim[[paste0("Bsim_", B.unit)]]
  )
  names(data.specs.sim.modif) <- c(var2nd_seq, paste0("Bsim_", B.unit))
  #
  ## transformation from wide table to long table with properly arranged `var2nd.series`
  data.specs.sim.modif <- data.specs.sim.modif %>%
    tidyr::pivot_longer(!.data[[paste0("Bsim_", B.unit)]],
      names_to = var2nd.series,
      values_to = Intensity.sim
    )
  data.specs.sim.modif[[var2nd.series]] <- as.double(as.character(data.specs.sim.modif[[var2nd.series]]))
  data.specs.sim.modif <- data.specs.sim.modif %>%
    dplyr::arrange(.data[[var2nd.series]])
  #
  ## the `Intensity.sim` is replaced by the newer one
  data.specs.sim[[Intensity.sim]] <- NULL
  data.specs.sim[[Intensity.sim]] <- data.specs.sim.modif[[Intensity.sim]]
  #
  ## removing the `data.specs.sim.modif` & `data.spec.sim` which is not needed anymore
  rm(data.specs.sim.modif, data.spec.sim)
  #
  ## base for the output data frame
  if (B.unit == "G"){
    result_df_base <- data.specs.sim %>%
      dplyr::group_by(.data[[var2nd.series]]) %>%
      dplyr::mutate(!!rlang::quo_name(single.integ) := pracma::cumtrapz(
        .data[[paste0("B_", B.unit)]], ## BE AWARE OF B.UNITS and INTEGRAL EVALUATION
        .data[[Intensity.sim]]
      )[, 1]
      )
  }
  if (B.unit == "mT"){
    result_df_base <- data.specs.sim %>%
      dplyr::group_by(.data[[var2nd.series]]) %>%
      dplyr::mutate(!!rlang::quo_name(single.integ) := pracma::cumtrapz(
        .data[[paste0("B_", B.unit)]], ## BE AWARE OF B.UNITS and INTEGRAL EVALUATION
        .data[[Intensity.sim]]
      )[, 1]*10
      )
  }
  if (isFALSE(output.area.stat)) {
    if (is.null(double.integ)) {
      result_df <- result_df_base
    } else {
      if (B.unit == "G"){
        result_df <- result_df_base %>%
          dplyr::mutate(!!rlang::quo_name(double.integ) := pracma::cumtrapz(
            .data[[paste0("B_", B.unit)]], ## BE AWARE OF B.UNITS and INTEGRAL EVALUATION
            .data[[single.integ]]
          )[, 1]
          )
      }
      if (B.unit == "mT"){
        result_df <- result_df_base %>%
          dplyr::mutate(!!rlang::quo_name(double.integ) := pracma::cumtrapz(
            .data[[paste0("B_", B.unit)]],
            .data[[single.integ]]
          )[, 1]*10
          )
      }
    }
  } else {
    if (is.null(double.integ)) {
      result_df <- result_df_base %>%
        dplyr::summarize(AreaSim = max(.data[[single.integ]]))
    } else {
      if (B.unit == "G"){
        result_df <- result_df_base %>%
          dplyr::mutate(!!rlang::quo_name(double.integ) := pracma::cumtrapz(
            .data[[paste0("B_", B.unit)]],
            .data[[single.integ]]
          )[, 1]
          ) %>%
          collapse::fsummarize(AreaSim = max(.data[[double.integ]]))
      }
      if (B.unit == "mT"){
        result_df <- result_df_base %>%
          dplyr::mutate(!!rlang::quo_name(double.integ) := pracma::cumtrapz(
            .data[[paste0("B_", B.unit)]],
            .data[[single.integ]]
          )[, 1]*10
          ) %>%
          collapse::fsummarize(AreaSim = max(.data[[double.integ]]))
      }
    }
  }
  #
  return(result_df)
  #
}
