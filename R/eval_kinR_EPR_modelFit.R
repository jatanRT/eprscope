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
#' @param data.qt.expr A data frame object containing the concentrations/integral intensities/areas under
#'   the EPR spectra calculated using the \strong{experimental data} as well as time column. These two essential
#'   columns are described by the character strings like those below \code{time} and \code{qvarR}.
#' @param time Character string pointing to \code{time} \strong{column/variable name} in the original
#'   \code{data.qt.expr} data frame. \strong{Default}: \code{time = "time_s"}.
#' @param qvarR Character string pointing to \code{qvarR} \strong{column/variable name} in the original
#'   \code{data.qt.expr} data frame. \strong{Default}: \code{qvarR = "Area"}.
#' @param params.guess Named vector, initial values of \code{kin.params} (see \code{\link{eval_kinR_ODE_model}})
#'   ready for optimization/fitting.
#' @param params.guess.lower Numeric vector of lower bounds on each parameter.
#'   If not given, the \strong{default} (\code{params.guess.lower = NULL}) lower bound
#'   corresponds to \code{-Inf} of each \code{params.guess} component.
#' @param params.guess.upper Numeric vector of upper bounds on each parameter.
#'   If not given, the \strong{default} (\code{params.guess.upper = NULL}) upper bound
#'   corresponds to \code{+Inf} of each \code{params.guess} component.
#' @param fit.kin.method Character string pointing to optimization/fitting method. So far,
#'   the default one (\code{fit.kin.method = "diff-levenmarq"}) is exclusively used.
#'   It corresponds to differential Levenberg-Marquardt (see also \code{\link[minpack.lm]{nls.lm}})
#'   because it is based on the numeric solution of the ordinary differential equations and not on the integration
#'   of rate equations.
#' @param time.correct Logical, if the time of the recorded series of EPR spectra needs to be corrected
#'   (see also \code{\link{correct_time_Exp_Specs}}).
#' @param path_to_dsc_par Character string ... argument/parameter... tbc
#' @param origin Character string ... argument/parameter... tbc
#' @param ... additional parameters for \code{\link[minpack.lm]{nls.lm}}.
#'
#'
#' @return List with the following components is available:
#'   \describe{
#'   \item{df}{Data frame object with the variables/columns such as \code{time},
#'   experimental quantitative variable like \code{sigmoid_Integ} (sigmoid integral) or \code{Area},
#'   concentration \code{c_M} or number of radicals of the relevant EPR spectrum and the corresponding
#'   quantitative variable \code{fitted} vector values.}
#'   \item{plot}{Plot/Graph object \emph{Quantitative variable} \emph{vs.} \emph{Time} with the experimental
#'   data and the corresponding fit.}
#'   \item{df.coeffs}{Data frame object containing the optimized parameter values (\code{Estimates}),
#'   their corresponding \code{standard errors}, \code{t-} as well as \code{p-values}.}
#'   \item{N.evals}{Total number of evaluations/iterations before the best fit is found.}
#'   \item{sum.LSQ.min}{The minimal least-square sum after \code{N.evals}.}
#'   \item{convergence}{Vector corresponding to residual sum of squares at each iteration/evaluation.
#'   The length of \code{convergence} is equal to the length of \code{N.evals}.}
#'   }
#'
#'
#' @examples
#' ## loading example data (incl. `Area` and `time` variables)
#' ## from Xenon: decay of a triarylamine radical cation after its generation
#' ## by electrochemical oxidation
#' triaryl_radCat_path <-
#'   load_data_example(file = "Triarylamine_radCat_decay_a.txt")
#' ## corresponding data (double integrated EPR spectrum = `Area` vs `time`)
#' triaryl_radCat_data <-
#'   readEPR_Exp_Specs(triaryl_radCat_path,
#'                     header = TRUE,
#'                     fill = TRUE,
#'                     select = c(3,7),
#'                     col.names = c("time_s","Area"),
#'                     x.unit = "s",
#'                     x.id = 1,
#'                     Intensity.id = 2,
#'                     qValue = 1700) %>%
#'   na.omit()
#' ## data preview
#' head(triaryl_radCat_data)
#' #
#' ## loading the `.DSC` file
#' triaryl_radCat_dsc_path <-
#'   load_data_example(file = "Triarylamine_radCat_decay_a.DSC")
#' #
#' ## fit previous data by second order kinetics, where the `model.react`
#' ## is considered as an elementary step (`time.correct` of the CW-sweeps
#' ## is included (`TRUE`))
#' triaryl_model_kin_fit_01 <-
#'   eval_kinR_EPR_modelFit(data.qt.expr = triaryl_radCat_data,
#'                          model.react = "(n=2)R --> [k1] B",
#'                          elementary.react = TRUE,
#'                          params.guess = c(qvar0R = 0.019,
#'                                           k1 = 0.04
#'                                          ),
#'                          time.correct = TRUE,
#'                          path_to_dsc_par = triaryl_radCat_dsc_path,
#'                          origin = "xenon")
#' ## data frame preview
#' head(triaryl_model_kin_fit_01$df)
#' #
#' ## plot preview
#' triaryl_model_kin_fit_01$plot
#' #
#' ## coefficients/parameters table preview
#' triaryl_model_kin_fit_01$df.coeffs
#' #
#' ## convergence preview
#' triaryl_model_kin_fit_01$convergence
#' #
#' ## take the same experimental data and perform fit by first order
#' ## kinetics where the `model.react` is considered as an elementary step
#' ## (`time.correct` of the CW-sweeps is included (`TRUE`))
#' triaryl_model_kin_fit_02 <-
#'   eval_kinR_EPR_modelFit(data.qt.expr = triaryl_radCat_data,
#'                          model.react = "(n=1)R --> [k1] B",
#'                          elementary.react = TRUE,
#'                          params.guess = c(qvar0R = 0.019,
#'                                           k1 = 0.0002
#'                                          ),
#'                          time.correct = TRUE,
#'                          path_to_dsc_par = triaryl_radCat_dsc_path,
#'                          origin = "xenon")
#' ## plot preview
#' triaryl_model_kin_fit_02$plot
#' #
#' ## coefficients/parameters table preview
#' triaryl_model_kin_fit_02$df.coeffs
#'
#'
#' @export
#'
#'
#' @importFrom minpack.lm nls.lm
#' @importFrom ggplot2 guide_legend
eval_kinR_EPR_modelFit <- function(data.qt.expr,
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
  . <- NULL
  fitted <- NULL
  M <- NULL
  p.d.u. <- NULL
  Concentration <- NULL
  ## convert time if other than `s` appears
  if (time.unit == "min") {
    data.qt.expr[[time]] <- data.qt.expr[[time]] * 60
    ## rename `time`
    colnames(data.qt.expr)[colnames(data.qt.expr) == time] <- "time_s"
  }
  if (time.unit == "h") {
    data.qt.expr[[time]] <- data.qt.expr[[time]] * 3600
    ## rename `time`
    colnames(data.qt.expr)[colnames(data.qt.expr) == time] <- "time_s"
  }
  #
  ## corrected time for CW EPR experiment
  if (isTRUE(time.correct)) {
    if (is.null(path_to_dsc_par) & is.null(origin)) {
      stop(" Please define the origin and the path\n
           for the file incl. instrumental parameters ! ")
    } else {
      #
      ## instrumental parameters for time series EPR spectra
      instrum.params.kin <- readEPR_params_slct_kin(path_to_dsc_par, origin = origin)
      #
      ## correct time
      data.qt.expr[[time]] <- correct_time_Exp_Specs(
        time.s = data.qt.expr[[time]],
        Nscans = instrum.params.kin$Nscans,
        sweep.time.s = instrum.params.kin$swTime
      )
      #
    }
  } else {
    data.qt.expr[[time]] <- data.qt.expr[[time]]
  }
  #
  ## `timeLim.model` definition guess fro 0 to 20% over
  ## (an arbitrary value to increase the number of points) the time max
  timeLim.model <- c(0,1.2 * max(data.qt.expr[[time]]))
  #
  ## -------------------- DERIVATIVE FORM Fit by LEVENBERG-MARQUARDT ---------------------
  #
  ## conditions/definitions/bounds for `lower` and `upper` +- 20 %
  # params.guess.values <- unname(params.guess)
  # params.guess.lower.def <- sapply(params.guess.values, function(p) p - (p * 0.2))
  # params.guess.upper.def <- sapply(params.guess.values, function(p) p + (p * 0.2))
  # params.guess.lower <- params.guess.lower %>%
  #   `if`(is.null(params.guess.lower),params.guess.lower.def,.)
  # params.guess.upper <- params.guess.upper %>%
  #   `if`(is.null(params.guess.upper),params.guess.upper.def,.)
  #
  ## Fit by solution of Ordinary Differential equations
  #
  if (fit.kin.method == "diff-levenmarq") {
      model.react.kin.fit <- minpack.lm::nls.lm(
      par = params.guess,
      lower = params.guess.lower,
      upper = params.guess.upper,
      fn = eval_kinR_ODE_model, ## funct. from this package (see docu. of `eval_kinR_ODE_model`)
      model.react = model.react,
      model.expr.diff = TRUE,
      elementary.react = elementary.react,
      timeLim.model = timeLim.model,
      data.qt.expr = data.qt.expr,
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
  ## ---------------- ADDITIONAL (INTEGRAL) METHODS --------------------
  #
  ## will be implemented later
  #
  ## -------------------- PREDICT THE BEST FIT -------------------------
  #
  ## parameters from the fit applied to generate `R` (`qvarR`)
  ## with experimental `time` <=> it corresponds to `predicted`
  model.expr.time <-
    eval_kinR_ODE_model(model.react = model.react,
                        model.expr.diff = FALSE,
                        kin.params = predict.model.params,
                        elementary.react = elementary.react,
                        timeLim.model = timeLim.model,
                        data.qt.expr = data.qt.expr,
                        time.expr = time,
                        qvar.expr = qvarR
    )
  #
  ## starting new data frame only with `time` and `qvar` &
  ## merge both data frames (add `fitted` columns)
  new.predict.df <- data.qt.expr %>%
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
  ## concentration character strings
  concentrat.strings <- c("c_M","c.M","conc","Conc")
  num.species.string <- c("num","Num","n","N","moles",
                          "Moles","Species","species",
                          "amount","Amount")
  quant.cond.string.fn <- function(var){
    if (any(grepl(paste(concentrat.strings,collapse = "|"),var))){
      return(0)
    }
    if (any(grepl(paste(num.species.string,collapse = "|"),var))){
      return(1)
    } else {
      return(2)
    }
  }
  #
  ## Caption parameter character vector =>
  caption.params.vec <- mapply(
    function(k,l) paste0(k," = ",l),
    names(predict.model.params),
    formatC(predict.model.params,digits = 3,format = "e") ## scientific notation
    )
  caption.params.vec <- paste(unname(caption.params.vec), collapse = ", ")
  #
  ## final.plot
  plot.fit <- plot.fit.base +
    labs(
      title = model.react,
      color = "",
      caption = paste("Least-Square Fit by the Levenberg-Marquardt Algorithm",
                      "and Numerical Solution of the Ordinary Differential Eqs. System:",
                      caption.params.vec,
                      sep = "\n"),
      x = bquote(italic(Time) ~ ~"(" ~ s ~ ")"),
      y = switch(3-quant.cond.string.fn(var = qvarR),
                 plot_labels_xyz(Integral~~Intensity,p.d.u.),
                 bquote(italic(Number~~of~~Species)),
                 plot_labels_xyz(Concentration,M)
                 )
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
  df.result <- summar.react.kin.fit.df
  ## Summary
  fit.summary <- list(
    df = new.predict.df,
    plot = plot.fit,
    df.coeffs = df.result,
    N.evals = iters.react.kin.fit,
    sum.LSQ.min = residsq.react.kin.fit,
    convergence = converg.react.kin.fit
  )
  #
  return(fit.summary)
  #
}
