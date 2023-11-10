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
#' @inheritParams eval_sim_EPR_iso
#' @param data.spectrum.expr Data frame object ... TBC ...
#' @param Intensity.expr Character string ... TBC ...
#' @param Intensity.sim Character string ... TBC ...
#' @param instrum.params Named numeric vector ... TBC ...
#' @param nuclear.system.noA List or nested list ... TBC ... without estimated hyperfine coupling constant values
#' @param lineG.content Numeric value ...
#' @param optim.method Character string ... TBC ...
#' @param optim.params.init Numeric vector with estimated ... TBC ...1. element = g-value, 2. element = Gaussian
#'   linewidth, 3. element = Lorentzian linewidth, 4. element = baseline constant,5... elements hyperfine coupling
#'   constants in `MHz`
#' @param optim.params.lower Numeric vector (with the length of \code{optim.params.init}) with the lower bound constraints.
#' @param optim.params.upper Numeric vector (with the length of \code{optim.params.init}) with the upper bound constraints.
#' @param Nmax.evals Numeric value corresp. to maximum number of iterations/evaluations.
#' @param tol.step Numeric value describing the smallest optimization step (tolerance) to stop the optimization.
#' @param sim.check Logical, whether to return simple list with overlay plot and the best fitting parameters in a vector.
#'
#'
#' @return List with following components depending on \code{sim.check} ... TBC ...
#'
#'
#' @examples
#' \dontrun{
#' TODO
#' TODO
#' }
#'
#'
#' @export
#'
#'
eval_sim_EPR_isoFit <- function(data.spectrum.expr,
                                Intensity.expr = "dIepr_over_dB",
                                Intensity.sim = "dIeprSim_over_dB",
                                instrum.params = NULL,
                                path_to_dsc_par,
                                origin = "xenon",
                                B.unit = "G",
                                nuclear.system.noA,
                                lineG.content = 0.5,
                                lineSpecs.form = "derivative",
                                optim.method = "neldermead",
                                optim.params.init,
                                optim.params.lower = NULL,
                                optim.params.upper = NULL,
                                Nmax.evals = 2000,
                                tol.step = 1e-6,
                                sim.check = TRUE){
  #
  ## 'Temporary' processing variables
  . <- NULL
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
  ## function to parameterize simulation
  fit_sim_params <- function(nucs.system,
                             Intensity.sim,
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
                         path_to_dsc_par = path_to_dsc_par,
                         origin = origin,
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
      ## adding parameters As (corresp. to x0[6,7...]) to nested list
      ## the first x0[1,2,3,4,5] is reserved for g,linewidths and intensity
      ## `A.var` should be explicitly expressed by corresp. x0 elements
      A.var <- x0[6:(5+length(nucle_us_i))]
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
                         path_to_dsc_par = path_to_dsc_par,
                         origin = origin,
                         natur.abund = TRUE,
                         nuclear.system = nucs.system.new,
                         lineSpecs.form = lineSpecs.form,
                         lineGL.DeltaB = list(gB.width.var,
                                              lB.width.var),
                         lineG.content = lineG.content,
                         Intensity.sim = Intensity.sim)$df

      #
    }
    intens.sim.fit <- x0[4] + x0[5] * sim.fit.df[[Intensity.sim]]
    #
    return(intens.sim.fit)
    #
  }
  #
  ## min. function for optimization incl. `fit_sim_params()`
  min_residuals <- function(data,nucs.system,Intensity.sim,x0){
    with(data,sum((data[[Intensity.expr]] -
                     fit_sim_params(nucs.system,Intensity.sim,x0))^2))
  }
  #
  ## initial parameter guesses for the optimization and definition
  lower.limits <- c(optim.params.init[1] - 0.0004,
                    optim.params.init[2] - 0.2,
                    optim.params.init[3] - 0.2,
                    optim.params.init[4] - 0.0001,
                    1e-6)
  upper.limits <- c(optim.params.init[1] + 0.0004,
                    optim.params.init[2] + 0.2,
                    optim.params.init[3] + 0.2,
                    optim.params.init[4] + 0.0001,
                    1)
  if (is.null(nuclear.system.noA)){
    lower.limits <- lower.limits
    upper.limits <- upper.limits
  } else {
    ## individual optimization limits for As
    A.lower.limits <- c()
    A.upper.limits <- c()
    for (a in 6:(5+length(nuclear.system.noA))){
      A.lower.limits[a-5] <- optim.params.init[a] - 4
      A.upper.limits[a-5] <- optim.params.init[a] + 4
    }
    ## actuall `limits`
    lower.limits <- c(lower.limits,A.lower.limits)
    upper.limits <- c(upper.limits,A.upper.limits)
  }
  optim.params.lower <- optim.params.lower %>%
    `if`(is.null(optim.params.lower), lower.limits, .)
  optim.params.upper <- optim.params.upper %>%
    `if`(is.null(optim.params.upper), upper.limits, .)
  #
  ## own optimization which can be performed also with two consecutive
  ## methods depending on the `optim.method` vector length
  if (length(optim.method) >= 1){
    optimization.list <- optim_EPR_by_nloptr(method = optim.method[1],
                                             x.0 = optim.params.init,
                                             fn = min_residuals,
                                             lower = optim.params.lower,
                                             upper = optim.params.upper,
                                             data = data.spectrum.expr,
                                             nucs.system = nuclear.system.noA,
                                             Intensity.sim = Intensity.sim,
                                             Nmax.evals = Nmax.evals,
                                             tol.step = tol.step)
    #
    ## sometimes the g-Value is quite far from the g-iso
    ## => therefore try additional optimization in loop
    #   ## difference between region borders (how many points)
    #   No.while.g.points <- (optim.params.upper[1] - optim.params.lower[1]) / 0.0001
    #   #
    #   ## actual minimal evaluation
    #   optim.min.val <- optimization.list$value
    #   #
    #   for (x in 1:No.while.g.points){
    #     optim.params.init[1] <- optim.params.lower[1] + x * 0.0001
    #     #
    #     ## ...TBC...
    #   }
    #
    ## best parameters
    best.fit.params <- optimization.list$par
    #
  }
  if (length(optim.method) == 2){
    optimization.list <-
      optim_EPR_by_nloptr(method = optim.method[2],
                          x.0 = best.fit.params, ## the best values from the first optim.
                          fn = min_residuals,
                          lower = optim.params.lower,
                          upper = optim.params.upper,
                          data = data.spectrum.expr,
                          nucs.system = nuclear.system.noA,
                          Intensity.sim = Intensity.sim,
                          Nmax.evals = Nmax.evals,
                          tol.step = tol.step)
    #
    ## best parameters
    best.fit.params <- optimization.list$par
    #
  }
  #
  ## "best" (i.e. including best As) nuclear system
  if (is.null(nuclear.system.noA)){
    nucs.system.best <- NULL
  } else{
    A.best <- best.fit.params[6:(5+length(nuclear.system.noA))]
    A.best <- round(A.best,digits = 3)
    nucs.system.best <- c()
    for (j in seq(nuclear.system.noA)) {
      nucs.system.best[[j]] <- c(nuclear.system.noA[[j]],A.best[j])
      nucs.system.best[[j]] <- as.list(nucs.system.best[[j]])
    }
  }
  #
  ## best simulated spectrum data frame
  best.fit.df <- eval_sim_EPR_iso(g.iso = best.fit.params[1],
                                  B.unit = B.unit,
                                  instrum.params = instrum.params,
                                  path_to_dsc_par = path_to_dsc_par,
                                  origin = origin,
                                  natur.abund = TRUE,
                                  nuclear.system = nucs.system.best,
                                  lineSpecs.form = lineSpecs.form,
                                  lineGL.DeltaB = list(best.fit.params[2],
                                                       best.fit.params[3]),
                                  lineG.content = lineG.content,
                                  Intensity.sim = Intensity.sim)$df
  #
  ## best simulated Intensity
  best.fit.df[[Intensity.sim]] <-
    best.fit.params[4] + best.fit.params[5] * best.fit.df[[Intensity.sim]]
  #
  ## add the `Intensity.sim` to experimental spectrum data
  data.spectrum.expr[[Intensity.sim]] <- best.fit.df[[Intensity.sim]]
  ## final data frame and rename columns
  data.sim.expr <- data.spectrum.expr %>%
    dplyr::select(dplyr::all_of(c(paste0("B_",B.unit),
                                  Intensity.expr,Intensity.sim))) %>%
    dplyr::rename_with(~ c("Experiment","Simulation"),
                       dplyr::all_of(c(Intensity.expr,Intensity.sim)))
  rm(data.spectrum.expr) ## not required anymore
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
    # result.list <- list(plot = plot.sim.expr,best.fit.params = best.fit.params)
  } else {
    ## for plotting both spectra as publication ready => spectra will be offset
    ## => recalculate the intensity => shift the simulated intensity
    ## down below by factor of difference between `max()` and `min()`
    Int.diff <- max(data.sim.expr[[Intensity.expr]]) - min(data.sim.expr[[Intensity.expr]])
    data.sim.expr.long <- data.sim.expr %>%
      dplyr::mutate(!!rlang::quo_name(Intensity.sim) := .data[[Intensity.sim]] - Int.diff) %>%
      tidyr::pivot_longer(!dplyr::all_of(paste0("B_",B.unit)),
                          names_to = "Spectrum",
                          values_to = Intensity.expr) %>%
      dplyr::arrange(.data$Spectrum)
  }
  #
  ## plotting both spectra together
  plot.sim.expr <- ggplot(data = data.sim.expr.long) +
    geom_line(aes(x = .data[[paste0("B_",B.unit)]],
                  y = .data[[Intensity.expr]],
                  color = .data$Spectrum)) +
    plot_theme_In_ticks() +
    scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    theme(legend.title = element_text(size = 13),
          legend.text = element_text(size = 11))
  #
  ## switching between final list components
  result.list <- switch(2-sim.check,
                        list(plot = plot.sim.expr,
                             best.fit.params = best.fit.params),
                        list(plot = plot.sim.expr,
                             best.fit.params = best.fit.params,
                             df = data.sim.expr,
                             min.LSQ.sum = optimization.list$value,
                             N.evals = optimization.list$iter,
                             N.converg = optimization.list$convergence))
  #
  return(result.list)
  #
}
