#
## more complex optimization/fitting based
## on augmented space of initial parameters
#
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
                                      Nmax.points.space = 16, # new argument max. number of points in space
                                      # tol.step = 5e-7, ## into `...`
                                      # pswarm.size = NULL, ## into `...`
                                      # pswarm.diameter = NULL, ## into `...`
                                      # pswarm.type = NULL, ## into `...`
                                      check.fit.plot = TRUE,
                                      processing = "sequential", ## or "parallel"
                                      animation = "AnimatedFit_of_simEPR_params", ## or NULL
                                      ## will be saved in working directory
                                      ...) { ## additional arguments from `eval_sim_EPR_isoFit`
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## ================ CHECKING VARIABLES and FUNCTIONS ===============
  #
  ## check the condition for `dvary` arguments
  ## both cannot be NULL !!
  if (is.null(lineG.content.dvary) &
      is.null(optim.params.init.dvary)) {
    stop(" Both `dvary` arguments have `NULL` assignment !!
         Please, define at least one of them, corresponding to initial\n
         variations, in the form of  (+-) difference. See description \n
         of the `lineG.content.dvary` and/or `optim.params.init.dvary` argument(s) !! ")
  }
  #
  ## checking the number `Nmax.evals`
  if (Nmax.evals > 1024) {
    message(" The max. number of least square function evaluations\n
            for each point in the  `Nmax.points.space` > 1024. \n
            Please, be aware of long computational time. ")
  }
  #
  ## Checking the high number of space points
  if (Nmax.points.space > 64) {
    message(
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
        length.out = Nmax.points.space
      )
  }
  ## sequence for the `optim.params.init`
  if (!is.null(optim.params.init.dvary)) {
    optim.params.init.vary.list <- c()
    for (j in 1:length(optim.params.init)) {
      optim.params.init.vary.list[[j]] <-
        seq(
          optim.params.init[j] - optim.params.init.dvary[j],
          optim.params.init[j] + optim.params.init.dvary[j],
          length.out = Nmax.points.space
        )
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
  vary_sim_iso_fit_fn <- function(Gauss.content,optim.params.init.var) {
    #
    listfit <- eval_sim_EPR_isoFit(
      data.spectr.expr = data.spectr.expr,
      nu.GHz = nu.GHz,
      B.unit = B.unit,
      nuclear.system.noA = nuclear.system.noA,
      baseline.correct = baseline.correct,
      lineG.content = Gauss.content,
      lineSpecs.form = lineSpecs.form,
      optim.method = optim.method,
      optim.params.init = optim.params.init.var,
      Nmax.evals = Nmax.evals,
      check.fit.plot = check.fit.plot,
      msg.optim.progress = msg.optim.progress, ## must be FALSE
      eval.optim.progress = eval.optim.progress, ## must be FALSE
      output.list.forFitSp = output.list.forFitSp# , ## must be TRUE
      #... ## additional arguments from `eval_sim_EPR_isoFit`
    )
    #
    return(listfit)
    #
  }
  #
  ## ================ OPTIMIZATION LOOPS / SEQUENCES SETUP ===================
  #
  ## ------------------------- PROGRESS BAR SETUP -------------------------
  #
  ## setup the progress bar
  progressr::handlers(global = TRUE,append = TRUE)
  # progressr::handlers("progress")
  progressr::handlers(list(
    progressr::handler_progress(
      format = " [:bar] :percent :current/:total of Evals. ",
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
      applied.cores <- 1
      processing <- "sequential"
      message(
        'Due to the limited hardware resources of your system\n
        NO PARALLEL COMPUTATION (no speed-up) CAN BE APPLIED to obtain\n
        the fit of EPR spectrum. Processing automatically \n
        switched to "sequential" !! '
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
  if (is.null(lineG.content.dvary)) {
    #
    progressr::with_progress({
      ## progress bar definition
      p <- progressr::progressor(steps = Nmax.points.space)
      #
      sim.fit.vary.list <-
        future.apply::future_lapply(
          1:nrow(optim.params.init.vary.df), function(r) {
            #
            p() ## progressbar
            #
            vary_sim_iso_fit_fn( ## see the function definition above
              Gauss.content = lineG.content,
              ## original from the main function's defaults
              optim.params.init.var = unname(unlist(optim.params.init.vary.df[r,]))
            )
          }
        )
    })
  } else { ## if the `lineG.content.vary` is different from NULL
    if (is.null(optim.params.init.dvary)) {
      #
      progressr::with_progress({
        ## progress bar definition
        p <- progressr::progressor(steps = Nmax.points.space)
        #
        sim.fit.vary.list <-
          future.apply::future_lapply(
            1:length(lineG.content.vary), function(c) {
              #
              p() ## progressbarr
              #
              vary_sim_iso_fit_fn( ## see the function def. above
                Gauss.content = lineG.content.vary[c],
                optim.params.init.var = optim.params.init
                ## original from the main function's defaults
              )
            }
          )
      })
    } else { ## if both arguments can vary (use `Map`-based function)
      #
      progressr::with_progress({
        ## progress bar definition
        p <- progressr::progressor(steps = Nmax.points.space)
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
            as.numeric(1:nrow(optim.params.init.vary.df))
          )
      })
    }
  }
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
    future:::ClusterRegistry("stop")
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
              ggtitle(label = paste0("Evaluation ",p))
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
    grDevices::colorRampPalette(colors = c("blue","darkred","darkviolet","darkorange"))(ncol(sim.fit.vary.list.params.df) - 1)
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
    # geom_line(linewidth = 0.2) +
    ## to show the Evaluation within lower `minRSS`:
    geom_vline(
      xintercept = best.df.index.minRSS,
      color = "#129001",
      linewidth = 0.75
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
    ggtitle(
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
    ggtitle(
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
      optim.EPRspec.plots = sim.fit.vary.list.plots
    )
  #
  return(result.list.all)
  #
}
