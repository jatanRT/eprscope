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
                                      Nmax.evals = 512,
                                      Nmax.evals.dvary = 16, # new argument max. number of points in space
                                      # tol.step = 5e-7, ## into `...`
                                      # pswarm.size = NULL, ## into `...`
                                      # pswarm.diameter = NULL, ## into `...`
                                      # pswarm.type = NULL, ## into `...`
                                      check.fit.plot = TRUE,
                                      processing = "sequential", ## or "parallel"
                                      ...) { ## additional arguments from `eval_sim_EPR_isoFit`
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## check the condition for `dvary` arguments
  ## both cannot be NULL !!
  if (is.null(lineG.content.dvary) &
      is.null(optim.params.init.dvary)) {
    stop(" Both `dvary` arguments have `NULL` assignment !! Please, define at least\n
         one of them, corresponding to initial variations, in the form of  +- difference.\n
         See description of the `lineG.content.dvary` and/or `optim.params.init.dvary` argument(s) !! ")
  }
  #
  ## UPPER CASE -> convert automatically into lower
  if (any(grepl("^[[:upper:]]+",processing))) {
    processing <- tolower(processing)
  }
  #
  ## check `processing`
  if (processing != "parallel" || processing != "sequential") {
    stop(" The `processing` argument must be defined as 'parallel' or as 'sequential' !! ")
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
        lineG.content-lineG.content.dvary,
        lineG.content+lineG.content.dvary,
        length.out = Nmax.evals.dvary
      )
  }
  ## sequence for the `optim.params.init`
  optim.params.init.vary.list <- c()
  for (j in 1:length(optim.params.init)) {
    optim.params.init.vary.list[[j]] <-
      seq(
        optim.params.init[j] - optim.params.init.dvary[j],
        optim.params.init[j] + optim.params.init.dvary[j],
        length.out = Nmax.evals.dvary
      )
  }
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
  ## now create general fitting/optimization function
  ## but before check the conditions and define the actual =>
  ## (taking into account, users can make mistakes :-))
  msg.optim.progress <-
    msg.optim.progress %>% `if`(isTRUE(msg.optim.progress), FALSE, .)
  output.list.forFitSp <-
    output.list.forFitSp %>% `if`(isFALSE(output.list.forFitSp), TRUE, .)
  eval.optim.progress <-
    eval.optim.progress %>% `if`(isTRUE(eval.optim.progress), FALSE, .)
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
      msg.optim.progress = FALSE,
      eval.optim.progress = FALSE,
      output.list.forFitSp = TRUE,
      #... ## additional arguments from `eval_sim_EPR_isoFit`
    )
    #
    return(listfit)
    #
  }
  #
  ## setup the progress bar
  progressr::handlers(global = TRUE)
  progressr::handlers("prgogress","beepr")
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
      stop(
        'Due to the limited hardware resources of your system\n
        NO PARALLEL COMPUTATION (no speed-up) CAN BE APPLIED to obtain\n
        the fit of EPR spectrum. Please, set up `processing = "sequential"` !! '
      )
    } else if (total.cores > 2) {
      ## round values up to nearest integer:
      applied.cores <- ceiling(0.5 * total.cores)
    }
  }
  #
  ## =================== PROCESSING FUNCTION ====================
  #
  ## creating function for processing (by `{future}`
  ## and `{future.apply}`), this is in general and does
  ## not depend whether the processing is `parallel` or `sequential`
  ## progress will be shown by `{progressr}`
  general_progress_apply_Map_fn <-
    function(DlineG.content,Dinit.params.df,process) { ## process...TBC...
      ## `DlineG.content` = `lineG.content.vary`
      ## `Dinit.params.df` = `optim.params.init.vary.df`
      ## `process` = `processing` actual from the main fun.
      #
      final.list <- c()
      #
      if (is.null(DlineG.content)) {
        #
        ## progress bar definition
        pb <- progressr::progressor(along = 1:nrow(Dinit.params.df))
        #
        future.apply::future_lapply(
          1:nrow(Dinit.params.df), function(r) {
            final.list[[r]] <- vary_sim_iso_fit_fn( ## see the function definition above
              Gauss.content = lineG.content,
              ## original from the main function's defaults
              optim.params.init.var = unname(unlist(Dinit.params.df[r,]))
            )
            #
            if (process == "sequential") {
              ## print plot during processing ONLY works IN SEQUENTIAL PROCESSING
              print(final.list[[r]][["plot"]])
            }
            #
            pb(sprintf("eval/iter=%g",r)) ## apply progress bar
          }
        )
      } else { ## if the `DlineG.content` is different from NULL
        if (is.null(Dinit.params.df)) {
          #
          ## progress bar definition
          pb <- progressr::progressor(along = 1:length(DlineG.content))
          #
          future.apply::future_lapply(
            1:length(DlineG.content), function(c) {
              final.list[[c]] <- vary_sim_iso_fit_fn( ## see the function def. above
                Gauss.content = DlineG.content[c],
                optim.params.init.var = optim.params.init
                ## original from the main function's defaults
              )
              #
              if (process == "sequential") {
                print(final.list[[c]][["plot"]])
              }
              #
              pb(sprintf("eval/iter=%g",c)) ## apply progress bar
            }
          )
        } else { ## if both arguments can vary (use `Map`-based function)
          #
          ## progress bar definition
          pb <- progressr::progressor(along = 1:length(DlineG.content))
          #
          future.apply::future_Map(
            function(c,r) {
              final.list[[c]] <- vary_sim_iso_fit_fn(
                Gauss.content = DlineG.content[c],
                optim.params.init.var = unname(unlist(Dinit.params.df[r,]))
              )
              #
              if (process == "sequential") {
                print(final.list[[c]][["plot"]])
              }
              #
              pb(sprintf("eval/iter=%g",c)) ## apply progress bar
            },
            as.numeric(1:length(DlineG.content)),
            as.numeric(1:nrow(Dinit.params.df))
          )
        }
      }
      return(final.list)
    }
  #
  ## ================ PROCEDURE (PARALLEL OR SEQUENTIAL) ==============
  #
  ## main message for the processing:
  msg.main <- "EPR simulation parameters are currently being explored by  "
  ## "parallel" or "sequential processing"
  #
  ## The general procedure depending on `processing` argument
  #
  ##  ---------- procedure START --------------
  if (processing == "sequential") {
    future::plan(sequential)
  } else {
    future::plan(multisession,workers = applied.cores,gc = FALSE)
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
  ## processing
  sim.fit.vary.list <-
    general_progress_apply_Map_fn(
      DlineG.content = lineG.content.vary,
      Dinit.params.df = optim.params.init.vary.df,
      process = processing
    )
  ## end time
  end.tm <- Sys.time()
  #
  ## "closing" message
  cat("\n")
  cat("r","Done!",
      " elapsed time ",
      round(as.numeric(
        difftime(time1 = end.tm, time2 = start.tm, units = "secs")
      ), 3), " s","\n"
  )
  #
  ## ------------ Procedure END -------------
  if (processing == "parallel") {
    future::plan(sequential)
    #
    ## ...+ shutdown the cluster
    future:::ClusterRegistry("stop")
  }
  #
  ## ================ FINAL VARIABLES, DATA FRAMEs ANALYSIS AND PLOTS ==================
  #
  ## ---------------------------- Lists and Data Frames -------------------------------
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
  ## from the previous list create data frame list
  ## where each row corresponds
  ## to the best fitted parameter set:
  sim.fit.vary.list.params.df <-
    as.data.frame(do.call(rbind,sim.fit.vary.list.params))
  names(sim.fit.vary.list.params.df) <-
    c(
      names.start,
      names.baseline,
      names.A,
      "minRSS", ## minimum sum of residual squares
      "raSD" ## standard deviation of residuals
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
  # find index for the best fit (minRSS)
  # in order to extract the data frame:
  best.df.index.minRSS <-
    which.min(sim.fit.vary.list.params.df$minRSS)
  #
  # find index for the best fit (raSD)
  best.df.index.raSD <-
    which.min(sim.fit.vary.list.params.df$raSD)
  #
  # the best params with minum RSS vector:
  best.params.from.space <-
    sim.fit.vary.list.params.df %>%
    dplyr::filter(minRSS == min(minRSS)) %>%
    dplyr::select(- c(minRSS,Evaluation,raSD)) %>%
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
  #
  ## ------------------------ PARAMETER ANALYSIS -----------------------------
  #
  ## select only EPR simulation parameters without `minRSS` and `raSD`
  sim.fit.final.epr.params.df <-
    dplyr::select(!dplyr::all_of(c("minRSS","raSD")))
  #
  ## starting overall parameter fit list
  linFit.final.epr.params <- list()
  #
  for (e in 1:(ncol(sim.fit.final.epr.params.df) - 1)) {
    #
    ## linear fit for each column
    linFit.final.epr.params[[e]] <-
      stats::lm(
        formula = stats::as.formula(
          paste0(names(sim.fit.final.epr.params.df)[e],"~","Evaluation")
        ),
        data = sim.fit.final.epr.params.df
      )
  }
  #
  ## ------------------------------ PLOTS ------------------------------------
  #
  ## color definitions
  facet.plot.colors <-
    grDevices::colorRampPalette(colors = c("blue","darkorange","darkviolet"))(ncol(sim.fit.vary.list.params.df) - 1)
  #
  ## Plot with optimized parameter space
  plot.facet.optim.space <-
    ggplot(
      data = sim.fit.vary.list.params.df.long,
      aes(x = Evaluation,y = Value,color = Parameter)
    ) + geom_point(size = 3) +
    ## to show fit with confidence interval
    ## except the minimum sum of residual squares
    geom_smooth(
      method = "lm", ## or "loess"
      formula = y ~ x,
      # span = 1,
      data = subset(
        sim.fit.vary.list.params.df.long,
        subset = !(Parameter %in% c("minRSS","raSD"))
      ),
      color = "magenta",
      se = TRUE,
      fill = "darkgray",
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
      axis.title.size = 13,
      axis.text.size = 11
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(
        color = "white",
        face = "bold.italic",
        size = 11
      ),
      strip.background = element_rect(fill = "#00205b"),
      axis.title = element_text(face = "italic"),
      plot.caption = element_text(color = "#129001",face = "bold")
    ) +
    ggtitle(
      label = "Space for the Optimized Set of EPR Simulation Parameters",
      subtitle = ""
    )
  #
  ## Plot with initial parameter space
  plot.facet.init.space <-
    ggplot(
      data = optim.params.init.vary.df.long,
      aes(x = Init_Params_Set,y = Value,color = Parameter)
    ) + geom_point(size = 3) +
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
      axis.title.size = 13,
      axis.text.size = 11
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(
        color = "white",
        face = "bold.italic",
        size = 11
      ),
      axis.title.x = element_blank(),
      strip.background = element_rect(fill = "#00205b"),
      axis.title = element_text(face = "italic"),
    ) +
    ggtitle(
      label = "Space for the Initial Set of EPR Simulation Fitting Parameters",
      subtitle = ""
    )
  #
  ## ========================= RESULTS =============================
  #
  result.list.all <-
    list(

    )
  #
}
