#'
#' Interactive Least-Squares Fitting of Isotropic EPR Spectra by Simulation Iteratioins/Evaulations
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   Ordinary least-squares fitting of the isotropic EPR spectra by simulation iterations/evaluations.
#'   In principle, this function is based on the \code{\link{eval_sim_EPR_isoFit}} however,
#'   it represents a more interactive version of the \code{\link{eval_sim_EPR_isoFit}}.
#'   Namely, it provides \code{{ggplot2}} objects (graphs, see the \code{Value} and the \code{plot.fit} description)
#'   in order to simultaneously check/explore the optimization/fitting process at each of the evaluations
#'   (refer to the \code{Nevals} argument). In addition, it also simultaneously shows current values
#'   of all the fitting parameters in the \emph{R} console. The function was built, because during the parallel processing
#'   (see the \code{\link{eval_sim_EPR_isoFit_space}}) it is not possible to display the actual
#'   EPR spectra during the optimization/fitting procedure. In the upcoming package updates, it will be also implemented
#'   into the \code{\link{plot_eval_ExpSim_app}}.
#'
#'
#' @inheritParams eval_sim_EPR_isoFit
#' @param optim.method Character string, setting the optimization method/algorithm. Even though, by default, the argument
#'   is defined as a vector, \code{optim.method = c("neldermead","cobyla","lbfgs")}, only one method from those three
#'   can be selected. For example \code{optim.method = "neldermead"} (\strong{default}). For additional information
#'   to all three available methods, please refer to the \code{\link{optim_for_EPR_fitness}}.
#' @param optim.params.lower Numeric vector (with the same element order like \code{optim.params.init})
#'   with the lower bound constraints. \strong{Default}: \code{optim.params.lower = NULL} which actually
#'   corresponds to relative default limits (refer to the \code{\link{eval_sim_EPR_isoFit}} arguments)
#'   of all fitted/optimized parameters for the actual evaluation. If specified
#'   (e.g. \code{optim.params.lower = c(2.004,0.3,0.3,-1e-3,0.001,46)}),
#'   it represents the general lower boundaries for the entire optimization/fitting procedure.
#' @param optim.params.upper Numeric vector (with the same element order like \code{optim.params.init})
#'   with the upper bound constraints. \strong{Default}: \code{optim.params.upper = NULL} which actually
#'   corresponds to relative default limits (refer to the \code{\link{eval_sim_EPR_isoFit}} arguments)
#'   of all fitted/optimized parameters for the actual evaluation. If specified
#'   (e.g. \code{optim.params.upper = c(2.007,0.5,0.5,1e-3,0.004,48)}),
#'   it represents the general upper boundaries for the entire optimization/fitting procedure.
#' @param Niters.per.eval Numeric value, equal to the number of iterations per one the \code{Nevals} (see the related \code{Nevals}
#'   argument description). \strong{Default}: \code{Niters.per.eval = 128}. This argument, among other things,
#'   depends on the complexity of the \code{nuclear.system} (\code{nuclear.system.noA}). For example, if an aminoxyl
#'   radical with 1 x 14N nucleus interaction is considered, the \code{Niters.per.eval} might be lower than the default
#'   one (e.g. 64). However, the more complex system like N,N,N′,N′-Tetramethyl-p-phenylenediamine radical cation,
#'   with 2 x 14N, 4 x 1H and 12 x 1H nuclei interaction may require higher \code{Niters.per.eval} (e.g. 128). The higher
#'   the \code{Niters.per.eval}, the longer the computational fitting/optimization time.
#' @param Nevals Numeric value, corresponding to total number of evaluations/cycles (i.e. how many loops/runs
#'   will be considered for the entire fitting/optimization procedure). This argument is related to the \code{Niters.per.eval},
#'   where the total number of iterations reads \code{Nevals} x \code{Niters.per.eval}. For example, for \code{Nevals = 16}
#'   (\strong{default}) and \code{Niters.per.eval = 128}, the number of iterations = 16 x 128 = 2048.
#'   Higher \code{Nevals} requires longer computational time. For interactive visualization of EPR spectra (during
#'   the optimization/fitting procedure) no parallel computing is supported
#'   (contrary to the \code{\link{eval_sim_EPR_isoFit_space}}).
#'
#'
#' @returns List with the following elements:
#'   \describe{
#'   \item{plot.fit}{A graph panel (2 rows x 2 columns) with 4 components. Simulated + experimental EPR spectra,
#'   residuals vs \eqn{B} magnetic flux density, sum of the residual squares (RSS) vs iteration as well as AIC+BIC information
#'   criteria vs iteration. Even though it represents the final status of those four dependencies, the actual version,
#'   appears at each of the \code{Nevals} (see the \code{Arguments}) in order to follow the progress of the fitting procedure
#'   interactively.}
#'   \item{best.fit.params}{Named vector of the optimized (best fitting) simulation parameters, corresponding
#'   to minimum \code{RSS}. The actual values also appear at each of the \code{Nevals} (see the \code{Arguments}),
#'   interactively during the fitting procedure.}
#'   \item{df.params.optim}{A data frame object of all simulation parameters + \code{RSS} (residual sum of squares)
#'   + \code{AIC} + \code{BIC} (Akaike and Bayessian information criteria, respectively, see also the \code{\link{eval_ABIC_forFit}}),
#'   evaluated during the fitting process at corresponding actual iteration. Visual progress of all those variables
#'   may be nicely followed by the \code{\link[esquisse]{esquisser}} or other \emph{R} plotting/visualization function.}
#'   \item{nuclear.system}{List consisting of all considered nuclei, and their optimized (best fitted) coupling
#'   constants \eqn{A} in MHz, which may be used for additional EPR simulation (see the \code{\link{eval_sim_EPR_iso}}).}
#'   \item{spec.expr.params}{Named numeric vector of parameters to record the experimental EPR spectrum,
#'   equal to \code{instrum.params} argument from the \code{\link{eval_sim_EPR_iso}}.
#'   To be used for additional simulations.}
#'   \item{ra}{Final (related to the minimum of \code{RSS}) \strong{r}esidual \strong{a}nalysis list
#'   (refer to the \code{\link{plot_eval_RA_forFit}} and/or to \code{\link{eval_sim_EPR_isoFit}}) extended by the \code{message}
#'   which distribution (Normal/Gaussian, Student's t-distribution or Cauchy) fits the residuals at best and was actually
#'   applied to evaluate AIC and BIC (consult the \code{\link{eval_ABIC_forFit}} for a detailed description).}
#'   \item{cor.df}{Function to evaluate correlation \code{matrix} of a data frame, consisting of EPR experimental,
#'   simulated (best fit) and residual intensities as columns/variables. Such matrix can be additionally nicely visualized
#'   by a correlation \code{plot} created by the \code{\link[corrplot]{corrplot}} function. Three \code{methods}
#'   are available: \code{"pearson"} (\strong{default}), \code{"spearman"} (captures monotonic relationships)
#'   and \code{"kendall"} (for small data ensembles), see also \code{\link[stats]{cor}}.}
#'   \item{plot.best.sim.expr}{Visualization function to plot either static \code{ggplot2} (\code{interactive = FALSE},
#'   \strong{default}) or interactive \code{plotly} (\code{interactive = TRUE}) comparison between the experimental
#'   and the simulated EPR spectrum. In addition, both spectra, within the static \code{ggplot}, can be presented
#'   either in the \code{overlay = TRUE} (\strong{default}) or in the offset (\code{overlay = FALSE}) mode.}
#'   \item{df.best.sim.expr}{A data frame object, containing experimental and simulated spectra in long/tidy form,
#'   actually corresponding to \code{plot.best.sim.expr} while \code{interactive = TRUE}.}
#'   }
#'
#'
#' @examples
#' \dontrun{
#' test.list <- eval_sim_EPR_isoFitb(
#'   data.spectr.expr = data.tmpd.spec,
#'   nu.GHz = data.tmpd.params.values[1,2],
#'   nuclear.system.noA = list(
#'     list("14N", 2),
#'     list("1H", 4),
#'     list("1H", 12)
#'   ),
#'   optim.method = "cobyla",
#'   optim.params.init = c(
#'     2.00305, ## g_iso
#'     0.521, ## Gaussian linewidth
#'     0.52, ## Lorentz linewidth
#'     0, ## offset/baseline constant
#'     3.2e5, ## intensity multiplication coeff.
#'     19.5, 5.5, 19.5 ## required As in MHz
#'   ),
#'   ## number of iterations per evaluation
#'   Niters.per.eval = 128,
#'   Nevals = 17 ## total number of evaluations
#'   ## total number of iterations =
#'   ## = Niters.per.eval * Nevals
#' )
#' #
#' ## simulation fit with the lower and upper
#' ## bound constraints
#' epr.spectrum.sim.fit <-
#'   eval_sim_EPR_isoFitb(
#'     data.spectr.expr = epr.spectrum.data,
#'     nu.GHz = 9.793116,
#'     B.unit = 'G',
#'     Blim = c(3435,3537),
#'     lineG.content = 0.93,
#'     optim.method = 'neldermead',
#'     optim.params.init = c(
#'       2.00581,0.57,0.77,0,0.0006,41.1,7.94,2.84
#'     ),
#'     optim.params.lower = c(
#'       2.0055,0.3,0.45,-1e-4,5e-4,40,6,1
#'     ),
#'     optim.params.upper = c(
#'       2.0059,0.7,1.2,1e-4,1e-3,42,9,4
#'     ),
#'     nuclear.system.noA =
#'       list(list('14N',1),list('1H',1),list('1H',1)),
#'     baseline.correct = 'constant',
#'     Nevals = 32,
#'     Niters.per.eval = 116
#'  )
#' }
#'
#'
#' @export
#'
#'
eval_sim_EPR_isoFitb <- function(data.spectr.expr,
                                 Intensity.expr = "dIepr_over_dB",
                                 Intensity.sim = "dIeprSim_over_dB",
                                 nu.GHz,
                                 B.unit = "G",
                                 Blim = NULL,
                                 nuclear.system.noA = NULL, ## no HFCCs, only nucleus and number
                                 baseline.correct = "constant", ## "linear" or "quadratic"
                                 lineG.content = 0.5,
                                 lineSpecs.form = "derivative",
                                 optim.method = c("neldermead","cobyla","lbfgs"),
                                 optim.params.init,
                                 optim.params.lower = NULL,
                                 optim.params.upper = NULL,
                                 optim.params.fix.id = NULL, ## related to `optim.params.init`
                                 Niters.per.eval = 128, ## how many iterations per cycle/evaluation
                                 Nevals = 16 ## total number of evaluations
                                 ## total number of iterations = Niters.per.eval * Nevals
                                 ){
  ## 'Temporary' processing variables
  . <- NULL
  MinRSS <- NULL
  AIC <- NULL
  BIC <- NULL
  Iteration <- NULL
  #
  ## delete index column if present
  if (any(grepl("index", colnames(data.spectr.expr)))) {
    data.spectr.expr$index <- NULL
  }
  ## if method defined by letter case - upper
  ## convert it automatically into lower
  if (any(grepl("^[[:upper:]]+",optim.method))) {
    optim.method <- tolower(optim.method)
  }
  #
  ## check the `optim.method`
  optim.method.check.string <- "neldermead|cobyla|lbfgs"
  if (!any(grepl(optim.method.check.string,optim.method))) {
    stop(" Only 3 optimization algorithms can be applied to fit\n
         the experimental EPR spectrum: `neldermead`\n
         or `cobyla` or `lbfgs` ! ")
  }
  #
  ## redefinition of the `optim.method` by the default `neldermead`
  optim.method <- optim.method %>%
    `if`(length(optim.method) > 1, "neldermead", .)
  #
  ## if `baseline.correct` defined by letter case - upper
  ## convert it automatically into lower
  if (grepl("^[[:upper:]]+",baseline.correct)) {
    baseline.correct <- tolower(baseline.correct)
  }
  #
  ## parameters for the simulation (experimental spectrum + mwGHz)
  B.cf <- stats::median(data.spectr.expr[[paste0("B_",B.unit)]])
  B.sw <- max(data.spectr.expr[[paste0("B_",B.unit)]]) -
    min(data.spectr.expr[[paste0("B_",B.unit)]])
  N.points <- nrow(data.spectr.expr)
  mw.GHz <- nu.GHz
  ## therefore => the named vector
  instrum.params <-
    c(Bcf = B.cf,Bsw = B.sw,Npoints = N.points,mwGHz = mw.GHz)
  #
  ## condition to switch among three values
  ## <==> baseline approximation
  baseline.cond.fn <- function(baseline.correct){
    if (baseline.correct == "constant" ||
        baseline.correct == "Constant"){
      return(0)
    } else if (baseline.correct == "linear" ||
               baseline.correct == "Linear"){
      return(1)
    } else if(baseline.correct == "quadratic" ||
              baseline.correct == "Quadratic"){
      return(2)
    }
  }
  #
  ## nuclear system re-definition (check if it is simple or nested list)
  if (!is.null(nuclear.system.noA)) {
    nested_list <- any(sapply(nuclear.system.noA, is.list))
    if (isFALSE(nested_list)){
      nuclear.system.noA <- list(nuclear.system.noA)
    } else {
      nuclear.system.noA <- nuclear.system.noA
    }
  }
  #
  ## ========================= Initial variables for the loop ============================
  #
  ## total number of iterations
  # Niters.total <- Niters.per.eval * Nevals
  #
  sim.fit.test.loop <- list()
  params.best.vec <- list()
  params.init <- list()
  params.init[[1]] <- optim.params.init
  min.rss.df <- data.frame(
    Iteration = numeric(),
    MinRSS = numeric(),
    AIC = numeric(),
    BIC = numeric()
  )
  #
  ## =========================== The main verbose loop ==================================
  #
  ## Strings comments in the R console
  msg.base <- "EPR simulation parameters are currently being optimized by  "
  msg.method <- toupper(optim.method)
  #
  cat("\n")
  cat("\r",msg.base,msg.method,"...","\n","\n")
  start.time <- Sys.time()
  for (i in 1:Nevals) {
    sim.fit.test.loop[[i]] <-
      eval_sim_EPR_isoFit(
        data.spectr.expr = data.spectr.expr,
        Intensity.expr = Intensity.expr,
        Intensity.sim = Intensity.sim,
        nu.GHz = nu.GHz,
        nuclear.system.noA = nuclear.system.noA,
        Blim = Blim,
        lineSpecs.form = lineSpecs.form,
        lineG.content = lineG.content,
        baseline.correct = baseline.correct, # or linear (with constant it is better)
        optim.method = optim.method, ## only neldermead, cobyla and lbfgs
        optim.params.init = params.init[[i]],
        optim.params.lower = optim.params.lower,
        optim.params.upper = optim.params.upper,
        optim.params.fix.id = optim.params.fix.id,
        Nmax.evals = Niters.per.eval,
        msg.optim.progress = FALSE
      )
    #
    ## create data frame in the loop row-by-row
    row.min.rss.df <- data.frame(
      Iteration = (i * Niters.per.eval),
      MinRSS = unlist(sim.fit.test.loop[[i]]$min.rss),
      AIC = unlist(sim.fit.test.loop[[i]]$abic$abic.vec[1]),
      BIC = unlist(sim.fit.test.loop[[i]]$abic$abic.vec[2])
    )
    min.rss.df <- rbind(min.rss.df,row.min.rss.df)
    #
    ## partial plot for `min.RSS`
    partial.plot.rss <-
      ggplot(
        data = min.rss.df,
        aes(x = Iteration,y = MinRSS)
      ) +
      geom_point(size = 3.2,color = "black",alpha = 0.75) +
      geom_line(color = "magenta",linewidth = 0.75) +
      labs(
        x = NULL,
        y = bquote(italic(RSS))
      ) +
      plot_theme_In_ticks()
    #
    ## partial plot for AIC/BIC
    partial.plot.abic <-
      ggplot(
        data = min.rss.df,
        aes(x = Iteration)
      ) +
      geom_point(aes(y = AIC,color = "AIC"),size = 3.2,alpha = 0.75) +
      geom_line(aes(y = AIC),color = "cornflowerblue",linewidth = 0.75) +
      geom_point(aes(y = BIC,color = "BIC"),size = 3.2,alpha = 0.75) +
      geom_line(aes(y = BIC), color = "lightsalmon2",linewidth = 0.75) +
      labs(
        x = bquote(italic(Iteration)),
        y = bquote(italic(Info-Criterium~~Value))
      ) +
      scale_color_manual(
        name = NULL,
        breaks = c("AIC","BIC"),
        values = c("navy","darkred")
      ) +
      plot_theme_In_ticks()
    #
    ## the entire plot by the `{patchwork}` 📦
    partial.plot.spectrum.RSS <-
      patchwork::wrap_plots(
        sim.fit.test.loop[[i]]$plot,
        patchwork::wrap_plots(
          partial.plot.rss,
          partial.plot.abic,
          ncol = 1
        ),
        ncol = 2
      )
    #
    ## actual plot in the loop
    suppressMessages( ## due to group and geom_line message/warning
      print(partial.plot.spectrum.RSS)
    )
    ## actual parameters in the loop (best as initial for next one)
    params.best.vec[[i]] <- unlist(sim.fit.test.loop[[i]]$best.fit.params)
    print(params.best.vec[[i]])
    if (i < Nevals) {
      params.init[[i + 1]] <- params.best.vec[[i]]
    }
    #
  }
  ##
  end.time <- Sys.time()
  cat("\n")
  cat(
    "\r",
    "Done ! (100 %)",
    " elapsed time ",
    round(as.numeric(
      difftime(time1 = end.time, time2 = start.time, units = "secs")
    ), 3), " s","\n"
  )
  #
  ## ============================= Loop output variables ==================================
  #
  ## rearrange `min.rss.df` (in order to be sure that
  ## no additional values appear)
  min.rss.df <- utils::tail(min.rss.df,Nevals)
  #
  ## create best params. table from list `params.best.vec`
  params.best.df <-
    as.data.frame(do.call(rbind,params.best.vec))
  #
  ## column names for the table `params.best.vec.df`
  names(params.best.df) <-
    sim.fit.test.loop[[1]]$best.fit.par.names
  #
  ## last (best) residual analysis
  final.ra <-
    sim.fit.test.loop[[length(sim.fit.test.loop)]]$ra
  #
  ## last ABIC residuals message
  final.residuals.msg <-
    sim.fit.test.loop[[length(sim.fit.test.loop)]]$abic$message
  final.residuals.msg <- list(message = final.residuals.msg)
  #
  ## last (best) simulated + experimental data frame (incl. residuals)
  sim.fit.expr.best.df <-
    sim.fit.test.loop[[length(sim.fit.test.loop)]]$df
  #
  ## last (best) correlation analysis function
  final.correlation <-  function(
    method = c("pearson","spearman","kendall")
    ) {
    #
    method <- tolower(method)
    ## redefinition
    method <- method %>% `if`(length(method) > 1,"pearson", .)
    #
    return(
      sim.fit.test.loop[[length(sim.fit.test.loop)]]$cor.df(method = method)
    )
  }
  #
  ## final best (optimized) params. named vector
  final.best.params <-
    unlist(params.best.df[nrow(params.best.df),])
  #
  ## add RSS as well as AIC and BIC and finally iteration to `params.best.df`
  params.best.df$RSS <- min.rss.df$MinRSS
  params.best.df$AIC <- min.rss.df$AIC
  params.best.df$BIC <- min.rss.df$BIC
  params.best.df$Iteration <- min.rss.df$Iteration
  #
  ## remove `min.rss.df` (not required anymore)
  rm(min.rss.df)
  #
  ## Iteration as a the first column in the data frame
  params.best.df <-
    params.best.df[,c(ncol(params.best.df),1:(ncol(params.best.df) - 1))]
  #
  ## best A vector and combine it with list without As
  if (!is.null(nuclear.system.noA)) {
    #
    ## list with the best/optimized As in MHz
    nuclear.system.A <-
      sim.fit.test.loop[[length(sim.fit.test.loop)]]$nuclear.system
    #
  } else {
    nuclear.system.A <- NULL
  }
  #
  ## ================== Publication ready + Interactive simulation output ====================
  #
  ## create simulation `df`
  final.sim.df <-
    eval_sim_EPR_iso(
      g.iso = final.best.params[["g_iso"]],
      instrum.params = instrum.params,
      path_to_dsc_par = NULL,
      origin = NULL,
      nuclear.system = nuclear.system.A,
      lineGL.DeltaB = list(
        unname(final.best.params[2]),
        unname(final.best.params[3])
      ),
      lineG.content = lineG.content
    )$df
  #
  ## function to plot experimental and simulated spectrum together
  plot.sim.exp.fn <- function(overlay = TRUE,interactive = FALSE) {
    if (isFALSE(interactive)) {
      sim.plot <-
        present_EPR_Sim_Spec(
          data.spectr.expr = data.spectr.expr,
          data.spectr.sim = final.sim.df,
          Intensity.shift.ratio =
            switch(2 - overlay,NULL,1.15), # 1.15 or NULL
          Blim = Blim
        ) + plot_theme_NoY_ticks(
          legend.text = element_text(size = 13)
        )
    } else {
      sim.plot <-
        plot_EPR_Specs2D_interact(
          data.spectra = sim.fit.expr.best.df,
          var2nd.series = "Spectrum",
          x = paste0("B_",B.unit),
          x.unit = B.unit,
          line.colors = c("darkcyan","darkorange","magenta","blue2"),
          legend.title = "Spectrum"
        )
      if (isFALSE(overlay)) {
        message(" The EPR spectra in interactive mode do not need to be offset.\n
                You can switch on/off (click on) the individual spectra,\n
                within the graph legend. ")
      }
    }
    #
    return(sim.plot)
    #
  }
  #
  ## =============================== RESULTS ====================================
  result.list <- list(
    plot.fit = partial.plot.spectrum.RSS,
    ## named vector:
    best.fit.params = final.best.params,
    ## all parameters with fit metrics:
    df.params.optim = params.best.df,
    ## system of interacting nuclei, all with A:
    nuclear.system = nuclear.system.A,
    ## experimental params. to record EPR spectrum:
    spec.expr.params = instrum.params,
    ## final residual analysis:
    ra = append(final.ra,final.residuals.msg),
    ## function (correlation, final):
    cor.df = final.correlation,
    ## last (best) simulation plot in overlay or offset:
    plot.best.sim.expr = plot.sim.exp.fn,
    ## corresponding last (best) simulation + experimental data frame:
    df.best.sim.expr = sim.fit.expr.best.df
  )
  #
  return(result.list)
  #
}
#
