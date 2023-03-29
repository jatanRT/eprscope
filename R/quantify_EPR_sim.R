#
#' Quantify Area of Simulated EPR Spectral Series Instead of Experimental One
#'
#'
#' @description tbc
#'
#'
#' @param data.spectra.series tbc
#' @param path_to_ASC_sim tbc
#' @param var2nd tbc
#' @param B.unit tbc
#' @param Intensity.exp tbc
#' @param Intensity.sim tbc
#' @param single.integ tbc
#' @param double.integ tbc can be also \code{NULL} if case of single integral spectral series input
#' @param output.area tbc
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
quantify_EPR_sim <- function(data.spectra.series,
                             path_to_ASC_sim,
                             var2nd = "time_s",
                             B.unit = "G",
                             Intensity.exp = "dIepr_over_dB",
                             Intensity.sim = "dIeprSim_over_dB",
                             single.integ = "sIntegral_Sim",
                             double.integ = "dIntegral_Sim",
                             output.area = TRUE) {
  ## 'Temporary' processing variables
  AreaSim <- NULL
  #
  ## Reading simulated EPR spectrum from MATLAB
  data.spec.sim <- readEPR_Sim_Spec(path_to_ASC_sim,
    B.unit = B.unit,
    col.names = c(
      paste0("Bsim_", B.unit),
      Intensity.sim
    )
  )
  #
  ## checking number of points for experimental and simulated spectra
  ## experimental
  resolution.exp <- data.spectra.series %>%
    dplyr::filter(.data[[var2nd]] == .data[[var2nd]][1]) %>%
    dim.data.frame()
  resolution.exp <- resolution.exp[1]
  ## simulated spectrum
  resolution.sim <- nrow(data.spec.sim)
  #
  if (resolution.exp != resolution.sim) {
    stop(" Number of points for experimental & simulated spectrum do not match ! ")
  } else {
    ## ...keep going:-), combining the experimental
    ## and simulated (non-processed, original) spectra into one long-table format
    ## (for each `vr2nd` added column of simulated spectrum)
    data.specs.sim <- data.spectra.series %>%
      dplyr::group_by(.data[[var2nd]]) %>%
      dplyr::mutate(!!rlang::quo_name(Intensity.sim) := data.spec.sim[[Intensity.sim]])
    #
    ## delete the original data (not needed anymore)
    ## substituted by `data.specs.sim`
    rm(data.spectra.series)
  }
  #
  ## new function for optimization to fit the simulated
  ## spectrum on the experimental one
  min_residuals <- function(data, par) {
    with(data, sum((data[[Intensity.exp]] - (par[1] + par[2] * data[[Intensity.sim]]))^2))
  }
  #
  ## `var2nd` sequence (e.g. like time sequence)
  var2nd_df <- data.specs.sim %>%
    dplyr::group_by(.data[[var2nd]]) %>%
    dplyr::group_keys()
  ## vector of `var2nd` sequence
  var2nd_seq <- var2nd_df[[var2nd]]
  #
  ## data split into data list with filtering
  ## to be ready to optimize each individual spectrum
  data.list <- lapply(
    var2nd_seq,
    function(t) subset(data.specs.sim, data.specs.sim[[var2nd]] == t)
  )
  #
  ## optimization list by data.list
  optimization.list <- lapply(
    seq_along(data.list),
    function(o) {
      stats::optim(
        par = c(0, 1),
        fn = min_residuals,
        data = data.list[[o]],
        method = "Nelder-Mead"
      )
    }
  )
  #
  ## data.list is not needed anymore
  rm(data.list)
  #
  ## Constants/parameters from optimization into vectors
  optim.vec.c1 <- sapply(
    seq_along(optimization.list),
    function(f) optimization.list[[f]]$par[1]
  )
  #
  optim.vec.c2 <- sapply(
    seq_along(optimization.list),
    function(f) optimization.list[[f]]$par[2]
  )
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
  data.specs.sim.modif <- data.frame(data.specs.sim.modif)
  ## modifying & changing the column names (incl. adding column of `B`
  ## in order to properly work with `pivot_longer` (see below))
  data.specs.sim.modif <- cbind(
    data.specs.sim.modif,
    data.spec.sim[[paste0("Bsim_", B.unit)]]
  )
  names(data.specs.sim.modif) <- c(var2nd_seq, paste0("Bsim_", B.unit))
  #
  ## transformation from wide table to long table with properly arranged `var2nd`
  data.specs.sim.modif <- data.specs.sim.modif %>%
    tidyr::pivot_longer(!.data[[paste0("Bsim_", B.unit)]],
      names_to = var2nd,
      values_to = Intensity.sim
    )
  data.specs.sim.modif[[var2nd]] <- as.double(as.character(data.specs.sim.modif[[var2nd]]))
  data.specs.sim.modif <- data.specs.sim.modif %>%
    dplyr::arrange(.data[[var2nd]])
  #
  ## the `Intensity.sim` is replaced by the newer one
  data.specs.sim[[Intensity.sim]] <- NULL
  data.specs.sim[[Intensity.sim]] <- data.specs.sim.modif[[Intensity.sim]]
  #
  ## removing the `data.specs.sim.modif` & `data.spec.sim` which is not needed anymore
  rm(data.specs.sim.modif, data.spec.sim)
  #
  ## base for the output data frame
  result_df_base <- data.specs.sim %>%
    dplyr::group_by(.data[[var2nd]]) %>%
    dplyr::mutate(!!rlang::quo_name(single.integ) := pracma::cumtrapz(
      .data[[paste0("B_", B.unit)]], ## BE AWARE OF B.UNITS and INTEGRAL EVALUATION
      .data[[Intensity.sim]]
    )[, 1]
    )
  if (isFALSE(output.area)) {
    if (is.null(double.integ)) {
      result_df <- result_df_base
    } else {
      result_df <- result_df_base %>%
        dplyr::mutate(!!rlang::quo_name(double.integ) := pracma::cumtrapz(
          .data[[paste0("B_", B.unit)]],
          .data[[single.integ]]
        )[, 1]
        )
    }
  } else {
    if (is.null(double.integ)) {
      result_df <- result_df_base %>%
        dplyr::summarize(AreaSim = max(.data[[single.integ]]))
    } else {
      result_df <- result_df_base %>%
        dplyr::mutate(!!rlang::quo_name(double.integ) := pracma::cumtrapz(
          .data[[paste0("B_", B.unit)]],
          .data[[single.integ]]
        )[, 1]
        ) %>%
        dplyr::summarize(AreaSim = max(.data[[double.integ]]))
    }
  }
  #
  return(result_df)
  #
}
