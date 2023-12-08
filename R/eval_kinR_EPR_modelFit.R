#'
#' Radical Kinetic Models Fitted to Experimental Data
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'  A short description...(Integrals/Areas/Concentration \emph{vs.} Time)...
#'
#'
#' @inheritParams eval_kinR_ODE_model
#' @param data.integs Data frame object Integrals/Areas/Concentration \emph{vs.} Time
#' @param time.unit Character string ... argument/parameter... tbc
#' @param time Character string ... argument/parameter... tbc
#' @param qvarR Character string ... argument/parameter... tbc
#' @param params.guess Named vector ... argument/parameter... tbc see also \code{kin.params}
#'   in \code{\link{eval_kinR_ODE_model}}.
#' @param params.guess.lower description   NULL, NO DEFAULTS ARE TO BE DEFINED...only in case
#'   if other than \code{fit.kin.method = "diff-levenmarq"} applied
#' @param params.guess.upper description   NULL, NO DEFAULTS ARE TO BE DEFINED ..only in case
#'   if other than \code{fit.kin.method = "diff-levenmarq"} applied
#' @param fit.kin.method Character string pointing to optimization/fitting method. If the \strong{default}:
#'   \code{fit.kin.method = "diff-levenmarq"} doesn't work one may try additional \pkg{nloptr} algorithms
#'   as introduced in \code{\link{optim_for_EPR_fitness}}.
#' @param time.correct Logical, if the time of the recorded series of EPR spectra needs to be corrected
#'   (see also \code{\link{correct_time_Exp_Specs}}).
#' @param path_to_dsc_par Character string ... argument/parameter... tbc
#' @param origin Character string ... argument/parameter... tbc
#'
#'
#' @return As a result of "kinetic" fit list with the following components is available:
#'   \describe{
#'   \item{df}{Data frame object with the variables/columns such as \code{time},
#'   experimental quantitative variable like \code{sigmoid_Integ} (sigmoid integral) or \code{Area}
#'   as well as concentration \code{c_M} of the relevant radical EPR spectrum and the corresponding
#'   quantitative variable \code{fitted} vector values .}
#'   \item{plot}{Plot/Graph object \emph{Quantitative variable} \emph{vs.} \emph{Time} with the experimental
#'   data and the corresponding fit.}
#'   \item{df.coeffs}{Data frame object. In case of \code{fit.kin.method = "diff-levenmarq"} it contains
#'   the optimized parameter values (\code{Estimates}), their corresponding \code{standard errors},
#'   \code{t-} and finally \code{p-values}. If the \code{fit.kin.method} is other than \code{"diff-levenmarq"}
#'   it summarizes the best parameter estimates.}
#'   \item{N.evals}{Total number of evaluations/iterations before the best fit is found.}
#'   \item{sum.LSQ.min}{The minimal least-square sum after \code{N.evals}.}
#'   \item{convergence}{In case of \code{fit.kin.method = "diff-levenmarq"} it corresponds to residual sum
#'   of squares at each iteration/evaluation. The length of \code{convergence}
#'   is equal to the length of \code{N.evals}. If the \code{fit.kin.method} is other than \code{"diff-levenmarq"}
#'   the integer code indicates successful completion (> 0) or a possible error number (< 0).}
#'   }
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
#' @importFrom minpack.lm nls.lm
#' @importFrom ggplot2 guide_legend
eval_kinR_EPR_modelFit <- function(data.integs,
                                   time.unit = "s",
                                   time = "time_s",
                                   qvarR = "Area",
                                   model.react = "(n=1)R --> [k1] B",
                                   elementary.react = TRUE,
                                   params.guess = c(
                                     qvar0R = 1e-3,
                                     k1 = 1e-3
                                   ),
                                   params.guess.lower = NULL,
                                   params.guess.upper = NULL,
                                   fit.kin.method = "diff-levenmarq",
                                   time.correct = FALSE,
                                   path_to_dsc_par = NULL,
                                   origin = NULL,
                                   ...) {
  #
  ## 'Temporary' processing variables
  # . <- NULL
  fitted <- NULL
  M <- NULL
  ## convert time if other than `s` appears
  if (time.unit == "min") {
    data.integs[[time]] <- data.integs[[time]] * 60
    ## rename `time`
    names(data.integs[[time]]) <- "time_s"
  }
  if (time.unit == "h") {
    data.integs[[time]] <- data.integs[[time]] * 3600
    ## rename `time`
    names(data.integs[[time]]) <- "time_s"
  }
  #
  ## corrected time for CW EPR experiment
  if (isTRUE(time.correct)) {
    if (is.null(path_to_dsc_par) & is.null(origin)) {
      stop(" Please define the origin and the path for file incl. instrumental parameters ! ")
    } else {
      #
      ## instrumental parameters for time series EPR spectra
      instrum.params.kin <- readEPR_params_slct_kin(path_to_dsc_par, origin = origin)
      #
      ## correct time
      data.integs[[time]] <- correct_time_Exp_Specs(
        time.s = data.integs[[time]],
        Nscans = instrum.params.kin$Nscans,
        sweep.time.s = instrum.params.kin$swTime
      )
      #
    }
  } else {
    data.integs[[time]] <- data.integs[[time]]
  }
  #
  ## `timeLim.model` definition guess fro 0 to 20% over
  ## (an arbitrary value to increase the number of points) the time max
  timeLim.model <- c(0,1.2 * max(data.integs[[time]]))
  #
  ## -------------------- DERIVATIVE FORM Fit by LEVENBERG-MARQUARDT ---------------------
  #
  ## Fit by solution of Ordinary Differential equations
  #
  if (fit.kin.method == "diff-levenmarq") {
      model.react.kin.fit <- minpack.lm::nls.lm(
      par = params.guess,
      fn = eval_kinR_ODE_model,
      model.react = model.react,
      model.expr.diff = TRUE,
      elementary.react = elementary.react,
      timeLim.model = timeLim.model,
      data.expr = data.integs,
      time.expr = time,
      qvar.expr = qvarR,
      ...
    )
    #
    ## Summary as table
    summar.react.kin.fit.df <- as.data.frame(summary(model.react.kin.fit)$coefficients)
    #
    ## number of iterations/evaluations
    iters.react.kin.fit <- model.react.kin.fit$niter
    #
    ## total sum of residual squares
    residsq.react.kin.fit <- model.react.kin.fit$deviance
    #
    ## vector of particular residual squares at each iteration
    converg.react.kin.fit <- model.react.kin.fit$rsstrace
    #
    ## obtained parameters from the fit
    predict.model.params <- as.vector(summar.react.kin.fit.df$Estimate)
    names(predict.model.params) <- rownames(summar.react.kin.fit.df)
    #
    ## the `model.expr.time` and `model.react.kin.fit` is not required anymore
    # rm(model.expr.time, model.react.kin.fit)
  }
  #
  ## ---------------------- ADDITIONAL METHODS -------------------------
  #
  ## ADDITIONAL FUNCTIONS/METHODS
  if (fit.kin.method == "neldermead" || fit.kin.method == "slsqp" ||
      fit.kin.method == "cobyla" || fit.kin.method == "lbfgs" ||
      fit.kin.method == "crs2lm" || fit.kin.method == "sbplx"){
    #
    ## function to parameterize kinetic model by `x0` params for `nloptr`
    fit_kin_params_x0 <- function(model.react,
                                  elementary.react,
                                  data,
                                  time,
                                  qvar,
                                  x0){
      #
      if (grepl("^\\(n=.*R --> \\[k1\\] B$",
                model.react)) {
        #
        if (isTRUE(elementary.react)){
          k1.guess <- x0[1]
          qvar0R.guess <- x0[2]
          #
          guess.params <- c(k1 = k1.guess,
                            qvar0R = qvar0R.guess)
        } else{
          k1.guess <- x0[1]
          qvar0R.guess <- x0[2]
          alpha.guess <- x0[3]
          #
          guess.params <- c(k1 = k1.guess,
                            qvar0R = qvar0R.guess,
                            alpha = alpha.guess)
        }
      }
      if (grepl("^\\(n=.*A --> \\[k1\\] \\(m=.*R$",
                model.react)){
        #
        if (isTRUE(elementary.react)){
          k1.guess <- x0[1]
          qvar0A.guess <- x0[2]
          qvar0R.guess <- x0[3]
          #
          guess.params <- c(k1 = k1.guess,
                            qvar0A = qvar0A.guess,
                            qvar0R = qvar0R.guess)
        } else{
          k1.guess <- x0[1]
          qvar0A.guess <- x0[2]
          qvar0R.guess <- x0[3]
          alpha.guess <- x0[4]
          #
          guess.params <- c(k1 = k1.guess,
                            qvar0A = qvar0A.guess,
                            qvar0R = qvar0R.guess,
                            alpha = alpha.guess)
        }
      }
      if (grepl("^\\(n=.*A --> \\[k1\\] \\(m=.*R <==> \\[k2\\] \\[k3\\] \\(l=.*C$",
                model.react)){
        #
        if (isTRUE(elementary.react)){
          k1.guess <- x0[1]
          k2.guess <- x0[2]
          k3.guess <- x0[3]
          qvar0A.guess <- x0[4]
          qvar0R.guess <- x0[5]
          qvar0C.guess <- x0[6]
          #
          guess.params <- c(k1 = k1.guess,
                            k2 = k2.guess,
                            k3 = k3.guess,
                            qvar0A = qvar0A.guess,
                            qvar0R = qvar0R.guess,
                            qvar0C = qvar0C.guess)
        } else{
          k1.guess <- x0[1]
          k2.guess <- x0[2]
          k3.guess <- x0[3]
          qvar0A.guess <- x0[4]
          qvar0R.guess <- x0[5]
          qvar0C.guess <- x0[6]
          alpha.guess <- x0[7]
          beta.guess <- x0[8]
          gamma.guess <- x0[9]
          #
          guess.params <- c(k1 = k1.guess,
                            k2 = k2.guess,
                            k3 = k3.guess,
                            qvar0A = qvar0A.guess,
                            qvar0R = qvar0R.guess,
                            qvar0C = qvar0C.guess,
                            alpha = alpha.guess,
                            beta = beta.guess,
                            gamma = gamma.guess)
        }
      }
      if (grepl("^\\(n=.*R <==> \\[k1\\] \\[k2\\] \\(m=.*B$",
                model.react)){
        #
        if (isTRUE(elementary.react)){
          k1.guess <- x0[1]
          k2.guess <- x0[2]
          qvar0R.guess <- x0[3]
          qvar0B.guess <- x0[4]
          #
          guess.params <- c(k1 = k1.guess,
                            k2 = k2.guess,
                            qvar0R = qvar0R.guess,
                            qvar0B = qvar0B.guess)
        } else {
          k1.guess <- x0[1]
          k2.guess <- x0[2]
          qvar0R.guess <- x0[3]
          qvar0B.guess <- x0[4]
          alpha.guess <- x0[5]
          beta.guess <- x0[6]
          #
          guess.params <- c(k1 = k1.guess,
                            k2 = k2.guess,
                            qvar0R = qvar0R.guess,
                            qvar0B = qvar0B.guess,
                            alpha = alpha.guess,
                            beta = beta.guess)
        }
      }
      if (grepl("^\\(n=.*A <==> \\[k1\\] \\[k2\\] \\(m=.*R$",
                model.react)){
        #
        if (isTRUE(elementary.react)){
          k1.guess <- x0[1]
          k2.guess <- x0[2]
          qvar0A.guess <- x0[3]
          qvar0R.guess <- x0[4]
          #
          guess.params <- c(k1 = k1.guess,
                            k2 = k2.guess,
                            qvar0A = qvar0A.guess,
                            qvar0R = qvar0R.guess)
        } else {
          k1.guess <- x0[1]
          k2.guess <- x0[2]
          qvar0A.guess <- x0[3]
          qvar0R.guess <- x0[4]
          alpha.guess <- x0[5]
          beta.guess <- x0[6]
          #
          guess.params <- c(k1 = k1.guess,
                            k2 = k2.guess,
                            qvar0A = qvar0A.guess,
                            qvar0R = qvar0R.guess,
                            alpha = alpha.guess,
                            beta = beta.guess)
        }
      }
      if (grepl("^\\(n=.*A \\+ \\(m=.*B --> \\[k1\\] \\(l=.*R$",
                model.react)){
        #
        if (isTRUE(elementary.react)){
          k1.guess <- x0[1]
          qvar0A.guess <- x0[2]
          qvar0B.guess <- x0[3]
          qvar0R.guess <- x0[4]
          #
          guess.params <- c(k1 = k1.guess,
                            qvar0A = qvar0A.guess,
                            qvar0B = qvar0B.guess,
                            qvar0R = qvar0R.guess)
        } else{
          k1.guess <- x0[1]
          qvar0A.guess <- x0[2]
          qvar0B.guess <- x0[3]
          qvar0R.guess <- x0[4]
          alpha.guess <- x0[5]
          beta.guess <- x0[6]
          #
          guess.params <- c(k1 = k1.guess,
                            qvar0A = qvar0A.guess,
                            qvar0B = qvar0B.guess,
                            qvar0R = qvar0R.guess,
                            alpha = alpha.guess,
                            beta = beta.guess)
        }
      }
      if (grepl("^\\(n=.*R \\+ \\(m=.*B --> \\[k1\\] \\(l=.*C$",
                model.react)){
        #
        if (isTRUE(elementary.react)){
          k1.guess <- x0[1]
          qvar0R.guess <- x0[2]
          qvar0B.guess <- x0[3]
          qvar0C.guess <- x0[4]
          #
          guess.params <- c(k1 = k1.guess,
                            qvar0R = qvar0R.guess,
                            qvar0B = qvar0B.guess,
                            qvar0C = qvar0C.guess)
        } else {
          k1.guess <- x0[1]
          qvar0R.guess <- x0[2]
          qvar0B.guess <- x0[3]
          qvar0C.guess <- x0[4]
          alpha.guess <- x0[5]
          beta.guess <- x0[6]
          #
          guess.params <- c(k1 = k1.guess,
                            qvar0R = qvar0R.guess,
                            qvar0B = qvar0B.guess,
                            qvar0C = qvar0C.guess,
                            alpha = alpha.guess,
                            beta = beta.guess)
        }
      }
      #
      difference <-
        eval_kinR_ODE_model(model.react = model.react,
                            model.expr.diff = TRUE,
                            elementary.react = elementary.react,
                            kin.params = guess.params,
                            timeLim.model = timeLim.model,
                            data.expr = data,
                            time.expr = time,
                            qvar.expr = qvar)
      #
      return(difference)
      #
    }
    #
    ## minim function =>
    min_residuals_nl <- function(model.react,elementary.react,data,time,qvar,x0){
      with(data,sum(fit_kin_params_x0(model.react,elementary.react,data,time,qvar,x0)^2))
    }
    #
    ## optimization
    optimize.kin.list <-
      optim_for_EPR_fitness(method = fit.kin.method,
                            x.0 = unname(params.guess),
                            fn = min_residuals_nl,
                            lower = params.guess.lower,
                            upper = params.guess.upper,
                            data = data.integs,
                            model.react = model.react,
                            elementary.react = elementary.react,
                            time = time,
                            qvar = qvarR,
                            ...)
    #
    ## best params obtained from the fit
    predict.model.params <- optimize.kin.list$par
    names(predict.model.params) <- names(params.guess)
  }
  #
  ## ------------------------------ PREDICT BEST FIT ----------------------------
  #
  ## parameters from the fit applied to generate `R` (`qvarR`)
  ## with experimental `time` <=> it corresponds to `predicted`
  model.expr.time <-
    eval_kinR_ODE_model(model.react = model.react,
                        model.expr.diff = FALSE,
                        kin.params = predict.model.params,
                        elementary.react = elementary.react,
                        timeLim.model = timeLim.model,
                        data.expr = data.integs,
                        time.expr = time,
                        qvar.expr = qvarR
    )
  #
  ## starting new data frame only with `time` and `qvar` &
  ## merge both data frames (add `fitted` columns)
  new.predict.df <- data.integs %>%
    dplyr::select(dplyr::all_of(c(time,qvarR))) %>%
    dplyr::mutate(fitted = model.expr.time$df[["R"]])
  #
  ## ---------------------------- EXPERIMENT-FIT PLOT -----------------------------
  #
  ## create plot
  plot.fit.base <- ggplot(new.predict.df) +
    geom_point(
      aes(
        x = .data[[time]],
        y = .data[[qvarR]],
        color = "Experimental\nData"
      ),
      size = 2.6
    ) +
    geom_line(
      aes(
        x = .data[[time]],
        y = .data$fitted,
        color = "\nKinetic\nModel Fit"
      ),
      linewidth = 1.1
    ) +
    scale_color_manual(
      values = c("darkcyan", "magenta"),
      breaks = c("Experimental\nData", "\nKinetic\nModel Fit"),
      guide = guide_legend(override.aes = list(
        shape = c(16, NA),
        linetype = c("blank", "solid")
      ))
    )
  #
  ## condition to concentration plot label
  concM.condition <- ifelse(grepl("c_M|c.M|conc|Conc|molar|Molar",qvarR),TRUE,FALSE)
  #
  ## Caption
  # plot.params.names <- lapply(names(predict.model.params),
  #                                    function(i) bquote(bolditalic(string2lang(.(i)))))
  # plot.caption <- Map(function(i,j) bquote(.(i) == .(j)),plot.params.names,predict.model.params)
  ## final.plot
  plot.fit <- plot.fit.base +
    labs(
      title = model.react,
      color = "",
      caption = paste0("Least-Square Fit by the ",fit.kin.method," Algorithm and
                Numerical Solution of Ordinary Differential Equations System."),
      x = bquote(italic(Time) ~ ~"(" ~ s ~ ")"),
      y = switch(2-concM.condition,
                 plot_labels_xyz(c,M),
                 bquote(italic(Integral ~ ~Intensity) ~ ~"(" ~ p.d.u. ~ ")"))
    ) +
    plot_theme_In_ticks() +
    scale_x_continuous(sec.axis = dup_axis(name = "", labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = "", labels = NULL)) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 13),
      legend.text.align = 0.5,
      legend.key.size = unit(1.4, "lines"),
      legend.box.margin = margin(l = -0.24, unit = "in")
    )
  #
  ## ---------------------------- RESULT LIST -----------------------------
  #
  ## condition for the method/algorithm
  method.fit.cond.fn <- function(method){
    if (method == "diff-levenmarq"){
      return(1)
    }
    if (method == "neldermead" || method == "slsqp" ||
        method == "cobyla" || method == "lbfgs" ||
        method == "crs2lm" || method == "sbplx"){
      return(0)
    }
  }
  #
  ## Parameters/Estimates Data frame
  ## best parameters data frame for `nloptr` methods =>
  summar.best.df <- data.frame(Estimate = unname(predict.model.params))
  rownames(summar.best.df) <- names(predict.model.params)
  #
  df.result <- switch(2-method.fit.cond.fn(method = fit.kin.method),
                      summar.react.kin.fit.df,
                      summar.best.df)
  ## Summary
  fit.summary <- list(
    df = new.predict.df,
    plot = plot.fit,
    df.coeffs = df.result,
    N.evals = switch(2-method.fit.cond.fn(method = fit.kin.method),
                     iters.react.kin.fit,
                     optimize.kin.list$iter),
    sum.LSQ.min = switch(2-method.fit.cond.fn(method = fit.kin.method),
                         residsq.react.kin.fit,
                         optimize.kin.list$value),
    convergence = switch(2-method.fit.cond.fn(method = fit.kin.method),
                         converg.react.kin.fit,
                         optimize.kin.list$convergence)
  )
  #
  return(fit.summary)
  #
}
