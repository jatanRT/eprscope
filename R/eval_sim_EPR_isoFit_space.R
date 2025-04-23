
#'
#' Explore the Hyperspace of Initial EPR Simulation Parameters (Searching for the Best Fit)
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   This is an augmented version of the \code{\link{eval_sim_EPR_isoFit}}, providing a broader range of the initial simulation
#'   parameters in order to find a more reliable simulation fit of an experimental isotropic EPR spectrum. The parameter space
#'   (represented by data frame/matrix and/or vector(s)) is divided into several points (see the argument \code{N.points.space})
#'   where each of these points corresponds to starting values (see arguments \code{optim.params.init} +
#'   \code{optim.params.init.dvary} as well as \code{lineG.content} + \code{lineG.content.dvary}), which are optimized
#'   by the \code{\link{eval_sim_EPR_isoFit}} setup. Because such procedure is computationally highly demanding,
#'   the central loop, to iterate/evaluate parameters and the corresponding EPR spectra, uses
#'   \href{https://future.apply.futureverse.org/}{\code{{future.apply}}} package
#'   (see also the \code{\link[future.apply]{future_Map}} function). It enables relatively seamless application
#'   of \href{https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html}{parallel computing}
#'   (please, also refer to the \code{processing} argument),
#'   regardless of the operating system (OS) to dramatically speed-up the entire searching for the best fit.
#'   In addition to graphical outputs, function also provides an animated representation
#'   (using the \href{https://yihui.org/animation/}{\code{{animation}}} package) of the procedure progress
#'   by showing the evaluated EPR spectra.
#'
#'
#'
#' @note
#'   In order to monitor and compare load of the hardware resources when running \code{processing = "parallel"}
#'   and \code{"sequential"}, one might use the following applications depending on the operating system (OS).
#'   For \emph{Windows}: \code{task manager} GUI (graphical user interface), for \emph{Linux}:
#'   terminal applications like \code{top}/\code{htop} or \code{system monitor} GUI and for \code{MacOS}
#'   terminal applications like \code{top}/\code{htop} or \code{activity monitor} GUI.
#'
#'
#'
#' @inheritParams eval_sim_EPR_isoFit
#' @param optim.method Character string (vector), setting the optimization method(s) gathered within
#'   the \code{\link{optim_for_EPR_fitness}}. \strong{Default}: \code{optim.method = "neldermead"}. Additionally,
#'   several consecutive methods can be defined like \code{optim.method = c("levenmarq","neldermead")}, where
#'   the best fit parameters from the previous method are used as input for the next one.
#' @param lineG.content.dvary Numeric value, corresponding to initial \strong{var}iation of \code{lineG.content},
#'   (Gaussian EPR line content in the simulated EPR spectrum) provided as \eqn{\pm} \strong{d}ifference
#'   of the central \code{lineG.content} value. For example, if \code{lineG.content = 0.42}
#'   and \code{lineG.content.dvary = 0.2}, the parameter will be varied within the range of \eqn{0.42\pm 0.2},
#'   which will be divided into \code{N.points.space} points (like already shown for the example
#'   in the \code{N.points.space} argument description). \strong{Default}: \code{lineG.content.dvary = NULL},
#'   actually pointing to constant \code{lineG.value} throughout the space (optimization/fitting procedures).
#' @param optim.params.init.dvary Numeric vector with initial \strong{var}iations of the corresponding
#'   \code{optim.params.init} elements in the form of \strong{d}ifferences from the central \code{optim.params.init}
#'   values. For example, for the aminoxyl radical we may assume \code{optim.params.init = c(2.006,4.8,4.8,0,1.4e-2,49)}
#'   (see the \code{optim.params.init} parameter definition). The \code{optim.params.init.dvary} could be defined
#'   as follows: \code{c(0.002,2.0,2.0,0,1e-2,3.2)}, meaning that \eqn{g = 2.006\pm 0.002},
#'   \eqn{\Delta B_{\text{pp}}^{\text{G}} = 4.8\pm 2.0\,\text{G}},
#'   \eqn{\Delta B_{\text{pp}}^{\text{L}} = 4.8\pm 2.0\,\text{G}}, constant baseline \eqn{0\pm 0}, ...etc.
#'   We may fix one or more initial parameters by putting the corresponding \code{optim.params.init.dvary}
#'   element to \code{0}. If the entire \code{optim.params.init} argument is to be fixed => put
#'   \code{optim.params.init.dvary = NULL} (\strong{default}). In all cases, the related \code{optim.params.init}
#'   space will be created as a matrix or data frame (see also the \code{Value}/\code{init.space.df}) with
#'   variables/columns corresponding to individual parameters, and observations/rows corresponding
#'   to each \code{N.points.space}, dividing the parameter variation range (e.g \eqn{g = 2.006\pm 0.002})
#'   into smaller spaces. Therefore, the fitting process will be performed (by the \code{\link{eval_sim_EPR_isoFit}})
#'   for each row of the initial data frame (\code{init.space.df}) together with the initial \code{lineG.content}
#'   variation vector (see the description of \code{N.points.space} and \code{lineG.content.dvary} arguments).
#'   In case for the \code{optim.params.init.dvary = NULL}, the fitting procedure is just repeated
#'   \code{N.points.space}-times, with the same parameter set. Such processing might be useful to determine
#'   the uncertainty of each optimized EPR simulation parameter by the \code{\link{eval_interval_cnfd_tVec}}
#'   for each column of the \code{optim.space.df} (see the \code{Value}).
#' @param N.points.space Numeric value, identical to number of points by which the initial parameter-hyperspace
#'   (see the \code{lineG.content.dvary} and/or \code{optim.params.init.dvary} and their corresponding
#'   \code{lineG.content} as well as \code{optimi.params.init} arguments)
#'   is divided, in order to find the best optimized parameters for EPR simulation fit of the isotropic
#'   experimental spectrum. \strong{Default}: \code{N.points.space = 16}, e.g. if \code{lineG.content = 0.42}
#'   and \code{lineG.content.dvary = 0.2}, the initial corresponding vector looks like
#'   \code{c(0.220,0.247,0.273,0.300,0.327,...,0.567,0.593,0.620)}, where the length of this vector is equal
#'   to \code{N.points.space = 16}.
#' @param processing Character string, corresponding to \code{"sequential"} (\strong{default} traditional
#'   computing method), or \code{"parallel"} processing/evaluation of EPR spectrum fit (optimization of parameters).
#'   The latter dramatically speeds up the execution time for all points (see the \code{N.points.space}
#'   argument) of the initial parameter-hyperspace, by dividing all the loops/iterations/evaluations
#'   into smaller sub-tasks, which are processed simultaneously. When selecting
#'   \href{https://grantmcdermott.com/ds4e/parallel.html}{parallel processing},
#'   the function/script automatically detects the number of CPU cores of your machine and selects half of them
#'   (e.g. for 4 cores in total, 2 cores are selected) for the computation. Otherwise, if the hardware resources
#'   are limited (2 cores in total), the \code{processing = "parallel"} automatically switches
#'   to \code{"sequential"} mode.
#' @param animation Character string, pointing to name of the animated \code{.gif} file, returned
#'   after processing and stored in the working directory (see the \code{Value}). If the animation
#'   is not desirable, put \code{animation = NULL}. Otherwise, an arbitrary file name can be chosen.
#'   \strong{Default}: \code{animation = "Fitting_of_sim_EPR"}.
#' @param ... additional arguments specified, see also the \code{\link{eval_sim_EPR_isoFit}},
#'   like \code{tol.step}, \code{pswarm} arguments (if \code{optim.method = "pswarm"}), \code{Blim},
#'   \code{Intensity.expr} or \code{Intensity.sim}.
#'
#'
#' @returns If the \code{animation} argument is different from \code{NULL}, the function will return a \code{.gif}
#'   animation of the fitting procedure progress, showing the EPR spectra at each evaluation,
#'   based on the \code{check.fit.plot} argument. The \code{animation} file will be stored in the working directory
#'   of your project. Additionally, a message, appeared in the R console, informs that the animation file was created.
#'   Regardless of the \code{.gif} animation a list with the following elements is provided:
#'   \describe{
#'   \item{init.space.df}{A data frame object representing hyperspace of the initial EPR simulation fitting parameters
#'   corresponding to \code{optim.params.init} and \code{optim.params.init.dvary}. Each variable/column corresponds
#'   to EPR simulation parameter to be optimized and each observation/row is related to one \code{N.points.space},
#'   dividing the range for each parameter defined by the \code{optim.params.init.dvary}. The fitting/optimization
#'   is performed for each row of the \code{init.space.df}.}
#'   \item{optim.space.df}{Data frame object similar to \code{init.space.df}, however with optimized EPR simulation
#'   parameters (after the fitting procedure). In addition, the \code{optim.space.df} contains the following metrics
#'   of the optimization/fitting as variables/columns: sum of the residual squares \code{RSS},
#'   standard deviation of residuals \code{residualSD}, Akaike information criterion \code{AIC} and Bayesian information
#'   criterion \code{BIC}. These four parameters are actually related to optimization/fitting path
#'   (see the \code{optim.space.plot} below).}
#'   \item{init.space.plot}{A \code{ggplot2} object, corresponding to graphical representation of the \code{init.space.df}
#'   created by the \code{\link[ggplot2]{facet_wrap}}.}
#'   \item{optim.space.plot}{A \code{ggplot2} object, corresponding to graphical representation of the \code{optim.space.df}
#'   created by the \code{\link[ggplot2]{facet_wrap}}. One can also easily recognize the best fit/optimized parameter set,
#'   because the \code{Evaluation} with those parameters is highlighted by the green line. Additionally, each optimized parameter
#'   \emph{vs} evaluation relation is fitted by the \code{\link[stats]{loess}} function implemented
#'   in the \code{\link[ggplot2]{geom_smooth}} in order to show the trend and the \eqn{95\,\%} confidence interval
#'   of the parameter optimization. This is especially important for the \code{RSS}, \code{residualSD}, \code{AIC} and \code{BIC},
#'   as they represent "hills" and "valleys" of the optimization/fitting path to identify the minima.}
#'   \item{best.fit.params}{Vector of the best final fitting (optimized) parameters (in the \code{optim.space.plot} distinguished
#'   by the green line) and related to the \code{optim.params.init} argument.}
#'   \item{best.lineG.content}{Numeric value of the Gaussian line content of the simulated EPR spectrum.
#'   If \code{lineG.content.dvary = NULL}  it corresponds to the original/initial value (\code{lineG.content}).
#'   Otherwise, a value from the corresponding vector, defined by the \code{lineG.content} + \code{lineG.content.dvary}
#'   + \code{N.points.space}, and related to the \code{RSS} minimum is returned.}
#'   \item{optim.EPRspec.plots}{List of individual EPR spectra, depending on the \code{check.fit.plot} argument and corresponding to
#'   each \code{Evaluation} (refer also to the \code{optim.space.plot}). These are the actual spectra by which the animation
#'   was created.}
#'   }
#'
#'
#' @examples
#' \dontrun{
#'  ## run parallel processing to fit the EPR spectrum
#'  ## of the TMPD radical cation (+ zoom the spectrum
#'  ## output by `Blim`), animation
#'  ## "Fitting_of_sim_EPR.gif" stored in the working dir.
#'  listfit <-
#'    eval_sim_EPR_isoFit_space(
#'    data.spectr.expr = data.tmpd.spec,
#'    nu.GHz = data.tmpd.params.values[1,2],
#'    nuclear.system.noA = list(
#'      list("14N", 2), # 2 x 14N
#'      list("1H", 4), # 4 x 1H
#'      list("1H", 12) # 12 x 1H
#'    ),
#'    optim.params.init = c(
#'      2.0030, 0.4, 0.4, 0,
#'      2.5e5, 20.0, 5.5, 19
#'    ),
#'    optim.params.init.dvary =
#'    c(0.0002,0.1,0.1,0,
#'      2e4,2,1,2), ## or NULL
#'    # Nmax.evals = 256,
#'    # N.points.space = 16,
#'    lineG.content = 0.3,
#'    lineG.content.dvary = 0.15, ## or NULL
#'    # optim.method = "neldermead",
#'    processing = "parallel" , ## or "sequential"
#'    Blim = c(3455,3545)
#'  )
#'  #
#'  ## optimization/fitting progress
#'  ## (main graphical output)
#'  listfit$optim.space.plot
#' }
#'
#'
#' @export
#'
#' @importFrom ggplot2 ggtitle
eval_sim_EPR_isoFit_space <- function(data.spectr.expr,
                                      # Intensity.expr = "dIepr_over_dB", ## into `...`
                                      # Intensity.sim = "dIeprSim_over_dB", ## into `...`
                                      nu.GHz,
                                      B.unit = "G",
                                      # Blim = NULL, ## into `...`
                                      nuclear.system.noA = NULL, ## no HFCCs, only nucleus and number
                                      baseline.correct = "constant", ## "linear" or "quadratic"
                                      lineG.content = 0.5,
                                      lineG.content.dvary = NULL, ## or value
                                      lineSpecs.form = "derivative",
                                      optim.method = "neldermead", ## also two consecutive methods as vector
                                      optim.params.init,
                                      optim.params.init.dvary = NULL, ## or vector
                                      # optim.params.lower = NULL, ## into `...`
                                      # optim.params.upper = NULL, ## into `...`
                                      Nmax.evals = 256,
                                      N.points.space = 16, # new argument max. number of points in space
                                      # tol.step = 5e-7, ## into `...`
                                      # pswarm.size = NULL, ## into `...`
                                      # pswarm.diameter = NULL, ## into `...`
                                      # pswarm.type = NULL, ## into `...`
                                      check.fit.plot = TRUE,
                                      processing = "sequential", ## or "parallel"
                                      animation = "Fitting_of_sim_EPR", ## or NULL
                                      ## will be saved in working directory
                                      ...) { ## additional arguments from `eval_sim_EPR_isoFit`
  #
  ## 'Temporary' processing variables
  . <- NULL
  Evaluation <- NULL
  Init_Params_Set <- NULL
  Parameter <- NULL
  RSS <- NULL
  Value <- NULL
  #
  ## ================ CHECKING VARIABLES and FUNCTIONS ===============
  #
  ## check the condition for `dvary` arguments
  ## both cannot be NULL !!
  if (is.null(lineG.content.dvary) &
      is.null(optim.params.init.dvary)) {
    message(" Both `dvary` arguments have `NULL` assignment !!
            The fitting procedure will be just repeated for the initial\n
            set of parameters `N.points.space`-times !! ")
  }
  #
  ## checking the number `Nmax.evals`
  if (Nmax.evals > 1024) {
    warning(" The max. number of least square function evaluations\n
            for each point in the  `N.points.space` > 1024. \n
            Please, be aware of long computational time. ")
  }
  #
  ## Checking the high number of space points
  if (N.points.space > 64) {
    warning(
      "The number of points in the initial parameter hyperspace\n
      is higher than 64. Please, be aware of long computational time."
    )
  }
  #
  ## UPPER CASE -> convert automatically into lower
  if (any(grepl("^[[:upper:]]+",processing))) {
    processing <- tolower(processing)
  }
  #
  ## check `processing` (user can make mistakes:-)
  if (grepl("par",processing)) {
    processing <- processing %>% `if`(processing != "parallel","parallel", .)
  } else if (grepl("seq",processing)) {
    processing <- processing %>% `if`(processing != "sequential","sequential", .)
  }
  #
  ## condition to switch among three values
  ## <==> baseline approximation
  baseline.cond.fn <- function(baseline.correct){
    if (baseline.correct == "constant" || baseline.correct == "Constant"){
      return(0)
    } else if (baseline.correct == "linear" || baseline.correct == "Linear"){
      return(1)
    } else if(baseline.correct == "quadratic" || baseline.correct == "Quadratic"){
      return(2)
    }
  }
  #
  ## ================ CREATING SEQUENCES OF PARAMETERS =================
  #
  ## what is the length of the list (how many nuclear groups)
  N_nucle_us_i <- length(nuclear.system.noA)
  #
  ## creating sequences based on `Nmax.eval.dvary`
  ## corresponding to creation of space for initial
  ## parameters
  ## sequence for the `lineG.content`
  if (!is.null(lineG.content.dvary)) {
    lineG.content.vary <-
      seq(
        lineG.content - lineG.content.dvary,
        lineG.content + lineG.content.dvary,
        length.out = N.points.space
      )
  } else {
    lineG.content.vary <- rep(lineG.content,N.points.space)
  }
  ## sequence for the `optim.params.init`
  optim.params.init.vary.list <- c()
  if (!is.null(optim.params.init.dvary)) {
    for (j in 1:length(optim.params.init)) {
      optim.params.init.vary.list[[j]] <-
        seq(
          optim.params.init[j] - optim.params.init.dvary[j],
          optim.params.init[j] + optim.params.init.dvary[j],
          length.out = N.points.space
        )
    }
  } else {
    for (j in 1:length(optim.params.init)) {
      optim.params.init.vary.list[[j]] <-
        rep(optim.params.init[j],N.points.space)
    }
  }
  #
  ## ===================== CREATING INITIAL DATAFRAME ======================
  #
  ## names depending on baseline correction and number of As
  names.start <- c(
    "g",
    paste0("DeltaBG_",B.unit),
    paste0("DeltaBL_",B.unit),
    "const_BaselinCoeff",
    "Intensity_MultiplCoeff"
  )
  names.A <-
    sapply(1:N_nucle_us_i, function(n) paste0("A",n,"_MHz"))
  names.baseline <-
    switch( ## lin. + quadrat. coeffs. for baseline
      3-baseline.cond.fn(baseline.correct = baseline.correct),
      c("lin_BaselinCoeff","quad_BaselinCoeff"),
      "lin_BaselinCoeff",
      NULL
    )
  names(optim.params.init.vary.list) <-
    c(names.start,names.baseline,names.A)
  #
  ## convert the previously created list
  ## into data frame where each row
  ## corresponds to the new `optim.params.init`
  optim.params.init.vary.df <-
    as.data.frame(optim.params.init.vary.list)
  #
  # delete the corresponding list (not required anymore)
  rm(optim.params.init.vary.list)
  #
  ## ===============  SETUP FOR THE MAIN OPTIM FUN. ====================
  ## =================== `vary_sim_iso_fit_fn` =========================
  #
  ## now create general fitting/optimization function
  ## but before check the conditions and define the actual =>
  ## (taking into account, users can make mistakes :-))
  if (exists("msg.optim.progress")) {
    msg.optim.progress <-
      msg.optim.progress %>% `if`(isTRUE(msg.optim.progress), FALSE, .)
  } else {
    msg.optim.progress <- FALSE
  }
  if (exists("output.list.forFitSp")) {
    output.list.forFitSp <-
      output.list.forFitSp %>% `if`(isFALSE(output.list.forFitSp), TRUE, .)
  } else {
    output.list.forFitSp <- TRUE
  }
  if (exists("eval.optim.progress")) {
    eval.optim.progress <-
      eval.optim.progress %>% `if`(isTRUE(eval.optim.progress), FALSE, .)
  } else {
    eval.optim.progress <- FALSE
  }
  #
  ## CENTRAL FITTING FUNCTION (Working with ellipsis)
  ## -----------------------
  argumns <- list(...)
  #
  vary_sim_iso_fit_fn <- function(Gauss.content,optim.params.init.var) {
    #
    inner_optim_fn <- function(lineG.content = Gauss.content,
                               optim.params.init = optim.params.init.var,
                               ...) {
    #
      eval_sim_EPR_isoFit(
        data.spectr.expr = data.spectr.expr,
        nu.GHz = nu.GHz,
        B.unit = B.unit,
        nuclear.system.noA = nuclear.system.noA,
        baseline.correct = baseline.correct,
        lineG.content = lineG.content,
        lineSpecs.form = lineSpecs.form,
        optim.method = optim.method,
        optim.params.init = optim.params.init,
        Nmax.evals = Nmax.evals,
        check.fit.plot = check.fit.plot,
        msg.optim.progress = msg.optim.progress, ## must be FALSE
        eval.optim.progress = eval.optim.progress, ## must be FALSE
        output.list.forFitSp = output.list.forFitSp, ## must be TRUE
        ...## additional arguments from `eval_sim_EPR_isoFit`
      )
    #
    }
    #
    vectr.check <- c(
      "Intensity.expr",
      "Intensity.sim",
      "Blim",
      "optim.params.lower",
      "optim.params.upper",
      "tol.step",
      "pswarm.size",
      "pswarm.diameter",
      "pswarm.type"
    )
    #
    if (any(vectr.check %in% names(argumns))) {
      listfit <- do.call(inner_optim_fn,argumns)
    } else {
      listfit <- inner_optim_fn()
    }
    #
    return(listfit)
    #
  }
  #
  ## ================ OPTIMIZATION LOOPS / SEQUENCES SETUP ===================
  #
  ## ------------------------- PROGRESS BAR SETUP -------------------------
  #
  ## setup the progress bar, this is required
  ## in order to synchronize the time, see
  ## https://progressr.futureverse.org/
  progressr::handlers("debug") ## !!! IMPORTANT
  # progressr::handlers("debug")
  # progressr::handlers("progress")
  progressr::handlers(list(
    progressr::handler_progress(
      format = " [:bar] :percent ",
      width = 104
    )
  ))
  #
  ## ------------------------ PARALLEL PROCESS CONDITION ---------------------
  #
  ## setup the hardware resources (automatically,
  ## approx 50 % of the cores available)
  if (processing == "parallel") {
    #
    ## Detect the operating system (condition definition)
    os.cond <- .Platform$OS.type == "unix" # (unix as base os)
    #
    # Define, how many cores will be used for "parallelization"
    total.cores <- parallel::detectCores(logical = FALSE)
    applied.cores <- NULL
    if (total.cores <= 2) {
      applied.cores <- 1 ## (just to be sure:-))
      processing <- "sequential"
      warning(
        'Due to the limited hardware resources of your system\n
        NO PARALLEL COMPUTATION (no speed-up) CAN BE APPLIED to obtain\n
        the fit of EPR spectrum. Processing automatically \n
        SWITCHED to "SEQUENTIAL" !! '
      )
    } else if (total.cores > 2) {
      ## round values up to nearest integer:
      applied.cores <- ceiling(0.5 * total.cores)
    }
  }
  #
  ## ================ OWN PROCEDURE (PARALLEL OR SEQUENTIAL) ==============
  #
  ## main message for the processing:
  msg.main <- "EPR simulation parameters are currently being explored by  "
  ## "parallel" or "sequential processing"
  #
  ## The general procedure depending on `processing` argument
  #
  ##  ---------- procedure START --------------
  if (processing == "sequential") {
    future::plan("sequential")
  } else {
    future::plan("multisession",workers = applied.cores,gc = FALSE)
    ## definition of the `applied.cores` see above
    ## and maybe automatic run of the "garbage collector"
    ## (gc = TRUE,memory cleaning)
  }
  #
  cat("\n")
  cat("\r",msg.main,toupper(processing),"  processing ......","\n")
  #
  ## start time
  start.tm <- Sys.time()
  #
  progressr::with_progress({
    ## progress bar definition
    p <- progressr::progressor(along = 0:length(lineG.content.vary))
    #
    sim.fit.vary.list <-
      future.apply::future_Map(
        function(c,r) {
          #
          p() ## progressbar
          #
          vary_sim_iso_fit_fn(
            Gauss.content = lineG.content.vary[c],
            optim.params.init.var = unname(unlist(optim.params.init.vary.df[r,]))
          )
        },
        as.numeric(1:length(lineG.content.vary)),
        as.numeric(1:nrow(optim.params.init.vary.df)),
        future.seed = NULL
      )
  })
  ## end time
  end.tm <- Sys.time()
  #
  ## "closing" message
  cat("\n")
  cat("\r","Done!",
      " elapsed time ",
      round(as.numeric(
        difftime(time1 = end.tm, time2 = start.tm, units = "secs")
      ), 3), " s","\n"
  )
  #
  ## ------------ Procedure END -------------
  if (processing == "parallel") {
    future::plan("sequential")
    #
    ## ...+ shutdown the cluster
    # future:::clusterRegistry$stopCluster()
  }
  #
  ## =========== FINAL VARIABLES, ANIMATIONS, DATA FRAMEs ANALYSIS AND PLOTS ============
  #
  ## ---------------------------- FINAL LISTS  -------------------------------
  #
  ## select only list with parameters:
  sim.fit.vary.list.params <-
    lapply(
      seq(sim.fit.vary.list),
      FUN = function(r) {sim.fit.vary.list[[r]][["params"]]}
    )
  #
  ## select only list with plots:
  sim.fit.vary.list.plots <-
    lapply(
      seq(sim.fit.vary.list),
      FUN = function(l) {sim.fit.vary.list[[l]][["plot"]]}
    )
  #
  ## ------------------------ ANIMATION ------------------------------
  #
  ## from the plots create an animation depending
  ## on `check.fit.plot`
  if (!is.null(animation)) {
    if (isTRUE(check.fit.plot)) {
      animation::saveGIF({
        for (p in seq(sim.fit.vary.list.plots)) {
          print(
            sim.fit.vary.list.plots[[p]] +
              patchwork::plot_annotation(title = paste0("Evaluation ",p))
          )
        }},
        interval = 0.32,
        movie.name = paste0(animation,".gif"),
        ani.width = 640,
        ani.height = 640
      )
    } else {
      animation::saveGIF({
        for (p in seq(sim.fit.vary.list.plots)) {
          print(
            sim.fit.vary.list.plots[[p]] +
              ggplot2::ggtitle(label = paste0("Evaluation ",p))
          )
        }},
        interval = 0.32,
        movie.name = paste0(animation,".gif"),
        ani.width = 640,
        ani.height = 640
      )
    }
  }
  #
  ## ------------------------ DATA FRAMES AND VARIABLES --------------------------
  #
  ## from the main previous list (of optimized params.)
  ## create data frame list where each row corresponds
  ## to the best fitted parameter set:
  sim.fit.vary.list.params.df <-
    as.data.frame(do.call(rbind,sim.fit.vary.list.params))
  names(sim.fit.vary.list.params.df) <-
    c(
      names.start,
      names.baseline,
      names.A,
      "RSS", ## minimum sum of residual squares
      "residualSD", ## standard deviation of residuals
      "AIC", ## Akaike information criterium
      "BIC" ## Bayesian information criterium
    )
  #
  ## add new "Evaluation/Iteration" column =>
  sim.fit.vary.list.params.df$Evaluation <-
    1:nrow(sim.fit.vary.list.params.df)
  #
  ## previous data frame into long table (tidy) format
  ## in order to work with `{ggplot2}`
  ## `facet_grid()` to visualize individual parameter changes:
  sim.fit.vary.list.params.df.long <-
    sim.fit.vary.list.params.df %>%
    tidyr::pivot_longer(
      !dplyr::all_of(c("Evaluation")),
      names_to = "Parameter",
      values_to = "Value"
    ) %>% dplyr::arrange(.data$Parameter)
  #
  ## find index for the best fit (minRSS)
  ## in order to extract the data frame:
  best.df.index.minRSS <-
    which.min(sim.fit.vary.list.params.df$RSS)
  #
  ## best lineG.content
  best.lineGcont <-
    lineG.content.vary[best.df.index.minRSS]
  #
  ## find index for the best fit (raSD)
  # best.df.index.raSD <-
  #   which.min(sim.fit.vary.list.params.df$raSD)
  #
  ## find index for the best fit (AIC)
  # best.df.index.AIC <-
  #   which.min(sim.fit.vary.list.params.df$AIC)
  #
  ## find index for the best fit (BIC)
  # best.df.index.BIC <-
  #   which.min(sim.fit.vary.list.params.df$BIC)
  #
  # the best params with minum RSS vector:
  best.params.from.space <-
    sim.fit.vary.list.params.df %>%
    dplyr::filter(RSS == min(RSS)) %>%
    dplyr::select(!dplyr::all_of(
      c("RSS","Evaluation","residualSD","AIC","BIC")
    )) %>%
    unlist() %>% unname()
  #
  # initial data frame into long format in order to plot
  # by `facet_wrap`
  optim.params.init.vary.df.long <-
    optim.params.init.vary.df %>%
    dplyr::mutate(
      Init_Params_Set = 1:nrow(optim.params.init.vary.df)
    ) %>%
    tidyr::pivot_longer(
      !dplyr::all_of(c("Init_Params_Set")),
      names_to = "Parameter",
      values_to = "Value"
    ) %>% dplyr::arrange(.data$Parameter)
  #
  ## the main list as well as the params. list
  ## are not required anymore,
  rm(
    sim.fit.vary.list,
    sim.fit.vary.list.params
  )
  #
  ## ------------------------ PARAMETER ANALYSIS (LATER) -----------------------------
  #
  ## select only EPR simulation parameters without `minRSS` and `raSD`
  # sim.fit.final.epr.params.df <-
  #   dplyr::select(!dplyr::all_of(c("minRSS","raSD")))
  # #
  # ## starting overall parameter fit list
  # linFit.final.epr.params <- list()
  # #
  # for (e in 1:(ncol(sim.fit.final.epr.params.df) - 1)) {
  #   #
  #   ## linear fit for each column
  #   linFit.final.epr.params[[e]] <-
  #     stats::lm(
  #       formula = stats::as.formula(
  #         paste0(names(sim.fit.final.epr.params.df)[e],"~","Evaluation")
  #       ),
  #       data = sim.fit.final.epr.params.df
  #     )
  # }
  #
  ## ------------------------------ PLOTS ------------------------------------
  #
  ## color definitions
  facet.plot.colors <-
    grDevices::colorRampPalette(colors = c("dodgerblue4","darkorange","darkviolet","darkred"))(ncol(sim.fit.vary.list.params.df) - 1)
  #
  ## Plot with optimized parameter space
  plot.facet.optim.space <-
    ggplot(
      data = sim.fit.vary.list.params.df.long,
      aes(x = Evaluation,y = Value,color = Parameter)
    ) + geom_point(size = 2.6) +
    ## to show fit with confidence interval
    ## except the minimum sum of residual squares
    geom_smooth(
      method = "loess", ## or "lm"
      formula = y ~ x,
      span = 1,
      data = subset(
        sim.fit.vary.list.params.df.long,
        subset = !(Parameter %in% c("RSS","residualSD","AIC","BIC"))
      ),
      color = "magenta",
      se = TRUE,
      fill = "darkgray",
      level = 0.95,
      linewidth = 1.1
    ) +
    ## the following `geom_smooth` in order to show progress
    ## of the RSS minimization/optimization criteria
    geom_smooth(
      method = "loess", ## or "lm"
      formula = y ~ x,
      span = 0.5,
      data = subset(
        sim.fit.vary.list.params.df.long,
        subset = Parameter %in% c("RSS","residualSD","AIC","BIC")
      ),
      color = "blue2",
      ## also  "cyan" 2,3, "royalblue", "green2", "greenyellow"
      ## "darkturquoise", "deepskyblue",
      se = TRUE,
      fill = "darkgray",
      level = 0.95,
      linewidth = 1.1
    ) +
    # geom_line(linewidth = 0.2) +
    ## to show the Evaluation within lower `minRSS`:
    geom_vline(
      xintercept = best.df.index.minRSS,
      color = "#129001",
      linewidth = 1.1
    ) +
    #
    facet_wrap(
      ~Parameter,
      ncol = 2,
      scales = "free_y"
    ) + scale_color_manual(
      values = facet.plot.colors
    ) +
    labs(caption = " \u2013 Parameters at minum sum of residual squares ") +
    plot_theme_Out_ticks(
      axis.title.size = 14,
      axis.text.size = 12
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(
        color = "white",
        face = "bold.italic",
        size = 12
      ),
      strip.background = element_rect(fill = "#00205b"),
      axis.title = element_text(face = "italic"),
      plot.caption = element_text(color = "#129001",face = "bold",size = 12)
    ) +
    ggplot2::ggtitle(
      label = "Space for the Set of Optimized EPR Simulation Parameters",
      subtitle = ""
    )
  #
  ## Plot with initial parameter space
  plot.facet.init.space <-
    ggplot(
      data = optim.params.init.vary.df.long,
      aes(x = Init_Params_Set,y = Value,color = Parameter)
    ) + geom_point(size = 2.6) +
    # geom_line(linewidth = 0.2) +
    facet_wrap(
      ~Parameter,
      ncol = 2,
      scales = "free_y"
    ) +
    scale_color_manual(
      values = facet.plot.colors
    ) +
    plot_theme_Out_ticks(
      axis.title.size = 14,
      axis.text.size = 12
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(
        color = "white",
        face = "bold.italic",
        size = 12
      ),
      axis.title.x = element_blank(),
      strip.background = element_rect(fill = "#00205b"),
      axis.title = element_text(face = "italic"),
    ) +
    ggplot2::ggtitle(
      label = "Space for the Set of Initial EPR Simulation Fitting Parameters",
      subtitle = ""
    )
  #
  ## ========================= RESULTS =============================
  #
  result.list.all <-
    list(
      init.space.df = optim.params.init.vary.df,
      optim.space.df = sim.fit.vary.list.params.df,
      init.space.plot = plot.facet.init.space,
      optim.space.plot = plot.facet.optim.space,
      best.fit.params = best.params.from.space,
      best.lineG.content = best.lineGcont,
      optim.EPRspec.plots = sim.fit.vary.list.plots
    )
  #
  return(result.list.all)
  #
}
