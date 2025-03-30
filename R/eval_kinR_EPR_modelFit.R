#'
#' Radical Kinetic Models Fitted onto Experimental Data
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'    Fitting of the integrals/areas/concentration/...etc. \emph{vs} time relation (either from experiment
#'    or from integration of the EPR spectral time series) in order to find the kinetic parameters
#'    (like rate constant, \eqn{k} as well as (partial) reaction order(s)) of proposed radical reaction.
#'    Reaction model is taken from the \code{\link{eval_kinR_ODE_model}}, while the optimization/fitting
#'    is provided by the differential Levenberg-Marquardt optimization method, \code{\link[minpack.lm]{nls.lm}}.
#'    Because the radical concentration is directly proportional to the EPR spectrum (double)
#'    integral (see the \code{\link{quantify_EPR_Abs}}), for a quick evaluation and/or comparison of different
#'    kinetic data, it is possible to obtain the rate constants (\eqn{k}) by the integrals/areas \emph{vs} time fit.
#'    Therefore, the unit of \eqn{k} is expressed in terms of \eqn{\text{s}^{-1}} as well as in units of integrals/areas,
#'    e.g. \code{procedure defined unit} (see \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6803776/}{p.d.u.}),
#'    depending on the order of reaction (see the \code{params.guess} argument).
#'
#'
#' @references
#'  Mullen KM, Elzhov TV, Spiess A, Bolker B (2023). “minpack.lm.” \url{https://github.com/cran/minpack.lm}.
#'
#'  Gavin HP (2024). “The Levenberg-Marquardt algorithm for nonlinear least squares curve-fitting problems.”
#'  \emph{Department of civil and environmental engineering, Duke University},
#'  \url{https://people.duke.edu/~hpgavin/ce281/lm.pdf}.
#'
#'
#' @inheritParams eval_kinR_ODE_model
#' @param data.qt.expr A data frame object, containing the concentrations/integral intensities/areas under
#'   the EPR spectra calculated using the experimental data as well as time column. These two essential
#'   columns are described by character strings like those below (see arguments \code{time} and \code{qvarR}).
#' @param time Character string, pointing to \code{time} column/variable name in the original
#'   \code{data.qt.expr} data frame. \strong{Default}: \code{time = "time_s"}.
#' @param qvarR Character string, pointing to \code{qvarR} (quantitative variable related to radical) column/variable
#'   name in the original \code{data.qt.expr}. \strong{Default}: \code{qvarR = "Area"}.
#' @param params.guess Named vector, initial values of \code{kin.params} (see \code{\link{eval_kinR_ODE_model}})
#'   ready for optimization/fitting. The \code{k1}-unit is eventually expressed
#'   in terms of \eqn{s^{-1}} as well as in units of the applied \code{qvar}
#'   (e.g. \code{c}, concentration) and depends on the partial reaction order(s), which power(s) the \code{qvar(s)}.
#'   For example, the \code{k1}-unit of elementary radical recombination, evaluated by double integrals,
#'   like \code{model.react = "(r=2)R --> [k1] B"}, reads: \eqn{\text{s}^{-1}\,(\text{p.d.u.})^{-1}}.
#' @param params.guess.lower Numeric vector of lower bounds on each parameter in \code{params.guess}.
#'   If not given, the \strong{default} (\code{params.guess.lower = NULL}) lower bound
#'   corresponds to \code{-Inf} of each \code{params.guess} component.
#' @param params.guess.upper Numeric vector of upper bounds on each parameter in \code{params.guess}.
#'   If not given, the \strong{default} (\code{params.guess.upper = NULL}) upper bound
#'   corresponds to \code{+Inf} of each \code{params.guess} component.
#' @param fit.kin.method Character string, pointing to optimization/fitting method. So far,
#'   the default one (\code{fit.kin.method = "diff-levenmarq"}) is exclusively used (additional methods
#'   are planned). It corresponds to differential Levenberg-Marquardt (see also \code{\link[minpack.lm]{nls.lm}})
#'   because it is based on the numeric solution of the ordinary differential equations
#'   and not on the common integration of rate equations.
#' @param ra.densScale.coeff Numeric value. When plotting \strong{r}esidual \strong{a}nalysis probability
#'   density (see \code{Value} and \code{plots.residAnal}), this coefficient multiplies/re-scales
#'   the density in order to be visible with the histogram. \strong{Default}: \code{ra.densScale.coeff = 2}.
#' @param time.correct Logical, if time of recorded series of the EPR spectra needs to be corrected.
#'   \strong{Default}: \code{time.correc = FALSE}, which actually assumes that time correction was done
#'   (either by \code{\link{correct_time_Exp_Specs}} or by \code{\link{readEPR_Exp_Specs_kin}} with
#'   a subsequent integration), prior to fitting procedure. If \code{time.correct = TRUE},
#'   the \code{path} to file with EPR instrumental parameters (like \code{.DSC}/\code{.dsc} or \code{par})
#'   must be defined (see the \code{path_to_dsc_par}).
#' @param path_to_dsc_par Character string, path (also provided by the \code{\link[base]{file.path}})
#'   to \code{.DSC/.dsc} or \code{.par} (depending on \code{origin} parameter)
#'   \code{text} files including instrumental parameters and provided by the EPR machine.
#'   \strong{Default}: \code{path_to_dsc_par = NULL}.
#' @param origin Character string, corresponding to software which was used to acquire the EPR spectra,
#'   essential to load the parameters by the \code{path_to_dsc_par} (see also the \code{\link{readEPR_params_slct_kin}}).
#'   Two origins are available: \code{origin = "winepr"} or \code{origin = "xenon"}.
#' @param ... additional arguments for \code{\link[minpack.lm]{nls.lm}}, e.g. defined
#'   by the \code{control = minpack.lm::nls.lm.control()}.
#'
#'
#' @return List with the following components is available:
#'   \describe{
#'   \item{df}{Data frame object with the variables/columns such as \code{time},
#'   experimental quantitative variable like \code{sigmoid_Integ} (sigmoid integral) or \code{Area},
#'   concentration \code{c_M} or number of radicals of the relevant EPR spectrum; the corresponding
#'   quantitative variable \code{fitted} vector values as well as residual vector (experiment - kinetic model)
#'   related to the \code{qvarR} argument.}
#'   \item{plot}{Plot object \emph{Quantitative variable} \emph{vs} \emph{Time} with the experimental
#'   data and the corresponding fit.}
#'   \item{plots.residAnal}{A list consisting of 2 plots: ggplot2 object (related to simple \strong{resid}ual
#'   \strong{anal}ysis), with two main plots: Q-Q plot and residuals vs predicted/fitted
#'   from the kinetic model. The second ggplot2 shows the \strong{hist}ogram and the scaled probability \strong{dens}ity
#'   function for residuals together with the corresponding mean value (vertical line). Residuals are defined
#'   as a difference between the integrals/concentrations/\code{qvar} values from the experiment
#'   and those evaluated by the model.}
#'   \item{df.coeffs}{Data frame object containing the optimized (best fit) parameter values (\code{Estimates}),
#'   their corresponding \code{standard errors}, \code{t-} as well as \code{p-values}.}
#'   \item{N.evals}{Total number of evaluations/iterations before the best fit is found.}
#'   \item{min.rss}{Minimum sum of residual squares after \code{N.evals}.}
#'   \item{N.converg}{Vector, corresponding to residual sum of squares at each iteration/evaluation.}
#'   }
#'
#'
#' @examples
#' ## loading example data (incl. `Area` and `time` variables)
#' ## from Xenon: decay of a triarylamine radical cation
#' ## after its generation by electrochemical oxidation
#' triaryl_radCat_path <-
#'   load_data_example(file = "Triarylamine_radCat_decay_a.txt")
#' ## corresponding data (double integrated
#' ## EPR spectrum = `Area` vs `time`)
#' triaryl_radCat_data <-
#'   readEPR_Exp_Specs(triaryl_radCat_path,
#'                     header = TRUE,
#'                     fill = TRUE,
#'                     select = c(3,7),
#'                     col.names = c("time_s","Area"),
#'                     x.unit = "s",
#'                     x.id = 1,
#'                     Intensity.id = 2,
#'                     qValue = 1700,
#'                     data.structure = "others") %>%
#'   na.omit()
#' ## data preview
#' head(triaryl_radCat_data)
#' #
#' ## loading the `.DSC` file
#' triaryl_radCat_dsc_path <-
#'   load_data_example(file = "Triarylamine_radCat_decay_a.DSC")
#' #
#' ## fit previous data by second order kinetics,
#' ## where the `model.react` is considered as an elementary
#' ## step (`time.correct` of the CW-sweeps is included (`TRUE`))
#' triaryl_model_kin_fit_01 <-
#'   eval_kinR_EPR_modelFit(data.qt.expr = triaryl_radCat_data,
#'                          model.react = "(r=2)R --> [k1] B",
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
#' triaryl_model_kin_fit_01$N.converg
#' #
#' ## simple residual analysis plots
#' ## showing the random pattern, which indicates that
#' ## kinetic model provides a decent fit to the data +
#' ## normal quantile (Q-Q) plot, indicating that residuals
#' ## are normally distributed; third plot demonstrates
#' ## the probability density with the histogram of residuals
#' triaryl_model_kin_fit_01$plots.residAnal
#' #
#' ## take the same experimental data and perform fit
#' ## by first order kinetics where the `model.react`
#' ## is considered as an elementary step
#' ## (`time.correct` of the CW-sweeps is included (`TRUE`))
#' triaryl_model_kin_fit_02 <-
#'   eval_kinR_EPR_modelFit(data.qt.expr = triaryl_radCat_data,
#'     model.react = "(r=1)R --> [k1] B",
#'     elementary.react = TRUE,
#'     params.guess = c(qvar0R = 0.019,
#'                      k1 = 0.0002
#'                      ),
#'     time.correct = TRUE,
#'     path_to_dsc_par = triaryl_radCat_dsc_path,
#'     origin = "xenon")
#' ## plot preview
#' triaryl_model_kin_fit_02$plot
#' #
#' ## coefficients/parameters table preview
#' triaryl_model_kin_fit_02$df.coeffs
#' #
#' ## simple residual analysis, indicating
#' ## the 1st order kinetics is less convenient
#' ## model than that of the 2nd order (based on
#' ## the decrease of EPR intensity/integral)
#' triaryl_model_kin_fit_02$plots.residAnal$plot.ra
#'
#'
#' @export
#'
#'
#' @importFrom minpack.lm nls.lm
#' @importFrom ggplot2 guide_legend stat_qq stat_qq_line geom_histogram geom_hline after_stat
eval_kinR_EPR_modelFit <- function(data.qt.expr,
                                   time.unit = "s",
                                   time = "time_s",
                                   qvarR = "Area",
                                   model.react = "(r=1)R --> [k1] B",
                                   elementary.react = TRUE,
                                   params.guess = c(
                                     qvar0R = 1e-3,
                                     k1 = 1e-3
                                   ),
                                   params.guess.lower = NULL,
                                   params.guess.upper = NULL,
                                   fit.kin.method = "diff-levenmarq",
                                   ra.densScale.coeff = 2,
                                   solve.ode.method = "lsoda",
                                   time.frame.model = 2,
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
  residuals <- NULL
  density <- NULL
  count <- NULL
  #
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
           for the file with the instrumental parameters ! ")
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
      time.interval.model = timeLim.model,
      time.frame.model = time.frame.model,
      solve.ode.method = solve.ode.method,
      data.qt.expr = data.qt.expr,
      time.expr = time,
      qvar.expr = qvarR,
      ...
    )
    #
    ## Summary as table
    summar.react.kin.fit.df <-
      as.data.frame(summary(model.react.kin.fit)$coefficients)
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
    predict.model.params <-
      as.vector(summar.react.kin.fit.df$Estimate)
    names(predict.model.params) <-
      rownames(summar.react.kin.fit.df)
    #
    ## the `model.expr.time` and `model.react.kin.fit` is not required anymore
    # rm(model.expr.time, model.react.kin.fit)
  }
  #
  ## ================= ADDITIONAL (INTEGRAL) METHODS ===================
  #
  ## will be implemented later
  #
  ## ==================== PREDICT THE BEST FIT =========================
  #
  ## parameters from the fit applied to generate `R` (`qvarR`)
  ## with experimental `time` <=> it corresponds to `predicted`
  model.expr.time <-
    eval_kinR_ODE_model(model.react = model.react,
                        model.expr.diff = FALSE,
                        kin.params = predict.model.params,
                        elementary.react = elementary.react,
                        time.interval.model = timeLim.model,
                        time.frame.model = time.frame.model,
                        solve.ode.method = solve.ode.method,
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
  ## ================== SIMPLE RESIDUAL ANALYSIS/PLOTS ===================
  #
  ## add residuals to `new.predict.df`
  new.predict.df$residuals <- stats::residuals(model.react.kin.fit)
  #
  ## residual plot (`{ggplot2}`)
  plot.resids <-
    ggplot(new.predict.df,
      mapping = aes(
        x = fitted,y = residuals
      )
    ) +
    geom_point(size = 2.6,color = "darkblue") +
    stat_smooth(
      method = "lm",
      formula = y ~ x,
      # span = 1,
      se = TRUE,
      color = "darkviolet",
      fill = "darkgray"
    ) +
    geom_hline(yintercept = 0,color = "darkred") +
    labs(
      x = bquote(italic(Kinetic~~Model~~Fit)*","~~italic(qvarR)),
      y = bquote(italic(Residuals)),
      title = "Residual Plot"
    ) +
    plot_theme_In_ticks()
  #
  ## q-q plot (`{ggplot2}`)
  plot.qq <-
    ggplot(new.predict.df,
      mapping = aes(
        sample = residuals
      )
    ) +
    qqplotr::stat_qq_band(fill = "lightgray") +             ## )
    qqplotr::stat_qq_line(color = "darkred") +              ## } plot confid. interval
    qqplotr::stat_qq_point(size = 2.6,color = "darkblue") + ## )
    labs(
      x = bquote(italic(Theoretical~~Quantiles)),
      y = bquote(italic(Sample~~Quantiles)),
      title = "Normal Q-Q Plot of Residuals"
    ) +
    plot_theme_In_ticks()
  #
  ## histogram with density plot (into results)
  plot.hist.dens <-
    ggplot(data = new.predict.df,
           mapping = aes(
             x = residuals
           )
    ) +
    geom_histogram(
      fill = "darkblue",
      alpha = 0.75,
      bins = 40
    ) +
    geom_density(
      aes(y = after_stat((count / max(count)) * ra.densScale.coeff)), ## relative scaled density
      # stat = "density",
      color = "darkorange",
      fill = "darkorange",
      alpha = 0.32
    ) +
    geom_vline( ## showing mean value
      xintercept = mean(new.predict.df$residuals),
      color = "darkviolet",
      linewidth = 0.75
    ) +
    labs(
      x = bquote(italic(Residuals)),
      y = bquote(italic(Counts)),
      title = "Histogram and Scaled Probability Density of Residuals",
      caption = "\u2013 Residuals mean value"
    ) +
    plot_theme_In_ticks(
      plot.caption = element_text(
        color = "darkviolet",
        face = "bold"
      )
    )
  #
  ## patchwork combination all both plots:
  plot.ra <-
    patchwork::wrap_plots(plot.resids,
                          plot.qq,
                          ncol = 1)
  #
  ## ====================== EXPERIMENT-FIT PLOT ============================
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
      caption = paste("Fit by the Levenberg-Marquardt Algorithm",
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
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 13),
      legend.text.align = 0.5,
      legend.key.size = unit(1.4, "lines"),
      legend.box.margin = margin(l = -0.24, unit = "in")
    )
  #
  ## ========================== RESULT LIST ==============================
  #
  df.result <- summar.react.kin.fit.df
  ## Summary
  fit.summary <- list(
    df = new.predict.df,
    plot = plot.fit,
    plots.residAnal = list(
      plot.ra = plot.ra,
      plot.ra.histDens = plot.hist.dens
    ),
    df.coeffs = df.result,
    N.evals = iters.react.kin.fit,
    min.rss = residsq.react.kin.fit,
    N.converg = converg.react.kin.fit
  )
  #
  return(fit.summary)
  #
}
