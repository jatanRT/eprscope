#'
#' Radical Kinetic Models Fitted onto Experimental Data
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'    Fitting of the integrals/areas/concentration/...etc. \emph{vs} time relation (either from experiment
#'    or by integration of the EPR spectral time series) in order to find the kinetic parameters
#'    (like rate constant, \eqn{k} as well as (partial) reaction order(s)) of proposed radical reaction.
#'    Reaction model is taken from the \code{\link{eval_kinR_ODE_model}}, while the optimization/fitting
#'    is provided by the differential Levenberg-Marquardt method, \code{\link[minpack.lm]{nls.lm}}.
#'    Because the radical concentration is directly proportional to the EPR spectrum (double)
#'    integral (see the \code{\link{quantify_EPR_Abs}}), for a quick evaluation and/or comparison of different
#'    kinetic data, it is possible to obtain the rate constants \eqn{k} by the integrals/areas \emph{vs} time fit.
#'    Therefore, the unit of \eqn{k} might be expressed in terms of \eqn{\text{s}^{-1}} including units of integrals/areas,
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
#'  Hyndman RJ, Athanasopoulos G (2021). \emph{Forecasting: Principles and Practice}, 3rd edition.
#'  OTexts: Melbourne, Australia, \url{https://otexts.com/fpp3/}.
#'
#'  geeksforgeeks (2025). "How to Test the Autocorrelation of the Residuals in R?",
#'  \url{https://www.geeksforgeeks.org/machine-learning/how-to-test-the-autocorrelation-of-the-residuals-in-r/}.
#'
#'  Hanck C, Arnold M, Gerber A, Schmelzer M (2025). \emph{Introduction to Econometrics with R}.
#'  University of Duisburg-Essen, \url{https://www.econometrics-with-r.org/}.
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
#'   \item{ra}{Simple residual analysis - a list consisting of 5 elements: diagnostic plots
#'   \code{plot.rqq()} function, \code{plot.histDens}; original data frame (\code{df}) with residuals and their corresponding
#'   standard deviation (\code{sd}). For details, please refer to the \code{\link{plot_eval_RA_forFit}}.
#'   The last element (\code{plot.acf}) is a diagnostic graph of the \strong{a}uto\strong{c}orrelation \strong{f}unction
#'   \code{\link[stats]{acf}} (please, also refer to the \code{References}). This can be considered as a primary
#'   "lie detector" test for the time series/kinetic models and it is defined as a correlation of residuals
#'   with its own past and future values vs their corresponding lag. For a decent model, the ACF values
#'   (except the first one \eqn{\equiv} perfect self-correlation) usually lie within
#'   the \eqn{95\,\%} confidence band (the light blue area within the \code{plot.acf}) for all lags.
#'   Hence, there is no obvious periodical or whatsoever pattern and only a "white noise" is observed (a small and random
#'   ACF distribution within the confidence band). If the user cannot be sure about a lag exceeding the confidence band,
#'   please perform the Ljung-Box test \code{\link[stats]{Box.test}} and look for the \code{p.value}, whether
#'   the autocorrelation is less (>> 0.05) or highly probable (<< 0.05).}
#'   \item{df.coeffs}{Data frame object containing the optimized (best fit) parameter values (\code{Estimates}),
#'   their corresponding \code{standard errors}, \code{t-} as well as \code{p-values}.}
#'   \item{cov.coeffs}{Covariance \code{matrix}, consisting of fitted/optimized kinetic parameters/coefficients
#'   (see also \code{df.coeffs} above). The corresponding variances (diagonal elements) should be small,
#'   indicating that the estimates possess a lower uncertainties.
#'   The off-diagonal elements show, how the two coefficient estimates change together. For a decent model they should be
#'   as close to \code{0} as possible. Large values may indicate
#'   \href{https://www.geeksforgeeks.org/machine-learning/how-to-test-for-multicollinearity-in-r/}{multicollinearity}
#'   with positive sign suggesting the coefficient are overestimated, and with a negative one, indicating that one coefficient
#'   is over-, while the other one is underestimated.}
#'   \item{cor.coeffs}{Correlation \code{matrix} of fitted/optimized kinetic parameters/coefficients
#'   (see also \code{df.coeffs} and \code{cov.coeffs} above). Such matrix can be additionally nicely visualized
#'   by a correlation \code{plot} created by the \code{\link[corrplot]{corrplot}} function.
#'   The off-diagonal elements should be as small as possible
#'   (ideally close to \code{0}) in order to exclude the multicollinearity (see the \code{cov.coeffs}) and to trust
#'   the output with the optimized kinetic parameters. Larger \code{cor.ceoffs} (typically > (0.7,0.8)) indicate
#'   potential multicollinearity.}
#'   \item{N.evals}{Total number of evaluations/iterations before the best fit is found.}
#'   \item{min.rss}{Minimum sum of residual squares after \code{N.evals}.}
#'   \item{abic}{A list consisting of Akaike and Bayesian information criteria (AIC & BIC) vector (\code{abic.vec})
#'   and \code{message}, denoting the distribution of residuals/errors, applied to evaluate
#'   those criteria. To be used when comparing different kinetic models. The lower the (negative) values,
#'   the better the fit. Please, refer to the \code{\link{eval_ABIC_forFit}}.}
#'   \item{cov.df}{Covariance \code{matrix} of a data frame, consisting of \code{qvarR} (e.g. double integral/Area - experiment),
#'   \code{fitted} (kinetic model fit) and the corresponding residuals as columns/variables. Covariance between
#'   the experiment and kinetic model should be positive and strong for a decent fit. Contrary, the \code{cov} between
#'   the kinetic model fit and residuals should be ideally close to \code{0}, indicating no systematic relationship.
#'   However, the covariance is scale-depended and must be "normalized". Therefore, for such a purpose, the correlation
#'   is defined as shown below.}
#'   \item{cor.df}{Correlation \code{matrix} of a data frame, consisting of \code{qvarR} (e.g. double integral/Area - experiment),
#'   \code{fitted} (kinetic model fit) and the corresponding residuals as columns/variables.
#'   Such matrix can be additionally nicely visualized by a correlation \code{plot} created
#'   by the \code{\link[corrplot]{corrplot}} function. A higher positive correlation
#'   (between the integrals and the kinetic model fit), with the value close to \code{1}, indicates that the kinetic model
#'   fit nicely follows the integral(s) \emph{vs} time relation. Contrary, no clear correlation between the residuals
#'   and the experiment and/or kinetic model must be visible. Therefore, such correlation should be ideally close to \code{0}.}
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
#' triaryl_model_kin_fit_01$ra$plot.rqq()
#' triaryl_model_kin_fit_01$ra$plot.histDens
#' #
#' ## autocorrelation (ACF) of residuals vs lag
#' triaryl_model_kin_fit_01$ra$plot.acf
#' #
#' ## standard deviation of residuals
#' triaryl_model_kin_fit_01$ra$sd
#' #
#' ## Akaike and Bayesian Criteria (AIC & BIC)
#' ##  + information about the residuals distribution
#' triaryl_model_kin_fit_01$abic
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
#' triaryl_model_kin_fit_02$ra$plot.rqq()
#' #
#' ## autocorrelation (ACF) of residuals vs lag
#' triaryl_model_kin_fit_02$ra$plot.acf
#' #
#' ## standard deviation of residuals
#' triaryl_model_kin_fit_02$ra$sd
#' #
#' ## Akaike and Bayesian Criteria (AIC & BIC) +
#' ## + information about the residuals distribution
#' triaryl_model_kin_fit_02$abic
#'
#'
#' @export
#'
#'
#' @importFrom minpack.lm nls.lm
#' @importFrom ggplot2 guide_legend geom_histogram geom_hline after_stat geom_segment
eval_kinR_EPR_modelFit <- function(data.qt.expr,
                                   time.unit = "s", ## in addition, time in "ms","h","min"
                                   time = "time_s",
                                   qvarR = "Area",
                                   model.react = "(r=1)R --> [k1] B",
                                   elementary.react = TRUE,
                                   params.guess = c(
                                     qvar0R = 1e-3,
                                     k1 = 1e-3
                                   ),
                                   params.guess.lower = NULL, ## automatic by `nls.lm()` see below
                                   params.guess.upper = NULL, ## automatic by `nls.lm()` see below
                                   fit.kin.method = "diff-levenmarq",
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
  lag <- NULL
  ACF <- NULL
  #
  ## convert time if other than `s` appears
  if (time.unit == "ms") {
    data.qt.expr[[time]] <- data.qt.expr[[time]] * 0.001
    time.frame.model <- time.frame.model * 0.001
  }
  if (time.unit == "min") {
    data.qt.expr[[time]] <- data.qt.expr[[time]] * 60
    time.frame.model <- time.frame.model * 60
  }
  if (time.unit == "h") {
    data.qt.expr[[time]] <- data.qt.expr[[time]] * 3600
    time.frame.model <- time.frame.model * 3600
  }
  ## right after that rename the 'time' column accordingly
  if (time.unit != "s") {
    ## rename `time`
    colnames(data.qt.expr)[colnames(data.qt.expr) == time] <- "time_s"
    #
    ## `time.frame.model` defined in `time.unit`
    message(" Please, be aware of the `time.frame.argument` value,\n
            defined in `time.unit`, which is automatically converted\n
            into seconds (including the entire `time` variable) !! ")
  }
  #
  ## check/re-define the `time` variable
  ## because the `time` is already converted into "s" +
  ## + to be sure that following operations proceed without
  ## an error
  time <- time %>% `if`(time != "time_s", "time_s", .)
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
  ## `timeLim.model` definition guess from 0 to 20% over
  ## (an arbitrary value to increase the number of points) the time max
  timeLim.model <- c(0,1.2 * max(data.qt.expr[[time]]))
  #
  ## -------------------- DERIVATIVE FORM Fit by LEVENBERG-MARQUARDT ---------------------
  #
  ## Fit by solution of Ordinary Differential equations
  #
  if (fit.kin.method == "diff-levenmarq") {
      model.react.kin.fit <- nls.lm(
      par = params.guess,
      lower = params.guess.lower, ## `nls.lm()` -> automatically `-Inf` if `lower = NULL`
      upper = params.guess.upper, ## `nls.lm()` -> automatically `+Inf` if `upper = NULL`
      fn = eval_kinR_ODE_model, ## fn. from this package (see docu. of `eval_kinR_ODE_model`)
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
    ## coefficients covariance + correlation matrix
    coeff.cov <- stats::vcov(model.react.kin.fit)
    coeff.cor <- stats::cov2cor(coeff.cov)
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
  ## ============ RESIDUAL ANALYSIS/PLOTS (INCL. AUTOCORRELATION/ACF) =============
  #
  ## add residuals to `new.predict.df`
  new.predict.df$residuals <- stats::residuals(model.react.kin.fit)
  #
  resid.anal.simple.list <-
    plot_eval_RA_forFit(
      data.fit = new.predict.df,
      residuals = "residuals",
      fitted = "fitted",
      resid.xlab = "Kinetic Model Fit",
      k = length(params.guess)
    )
  #
  ## calculate `cov()` and `cor()` (covariance + corr. matrices) using
  ## the 'experiment,fitted,residuals' data frame
  df.for.cov.cor <-
    resid.anal.simple.list$df %>%
    dplyr::select(
      dplyr::all_of(
        c(qvarR,"fitted","residuals")
      )
    )
  #
  df.cov <- stats::cov(df.for.cov.cor)
  df.cor <- stats::cor(df.for.cov.cor)
  #
  ## -------------------- Crating the ACF vs. LAG graph ----------------------
  ## -------------------------- of the residuals ------------------------------
  #
  resids.vec <- new.predict.df$residuals # must be sorted by time (ascending)
  #
  ## calculating the autocorrelation function (ACF) time series
  ## diagnostic and the corresponding data frame
  resids.acf.list <- stats::acf(resids.vec,plot = FALSE)
  resids.acf.df <- data.frame(
    ACF = resids.acf.list$acf,
    lag = resids.acf.list$lag
  )
  #
  ## calculate the point-wise 95% confidence band
  z_crit <- stats::qnorm(1 - 0.05/2)
  acf.95.limit <- z_crit / sqrt(length(resids.vec))
  #
  ## finally create a plot + output list
  resids.acf.plot <-
    ggplot(data = resids.acf.df) +
    geom_segment(
      aes(x = lag,xend = lag,y = 0,yend = ACF),
      color = "darkblue",
      linewidth = 0.75
    ) +
    geom_hline(yintercept = 0,color = "darkorange",linewidth = 1.1) +
    annotate(
      geom = "rect",
      xmin = -Inf,
      xmax = +Inf,
      ymin = -acf.95.limit,
      ymax = acf.95.limit,
      fill = "blue",
      alpha = 0.16
    ) +
    labs(
      title = "ACF of Residuals for the Kinetic Model Fit",
      caption = expression("ACF" %==% "Autocorrelation Function"),
      x = bquote(italic(Lag)),
      y = bquote(italic(ACF))
    ) +
    plot_theme_In_ticks()
  #
  ## ... and the ACF graph list added to final `ra` list (see `fit.summary` below)
  resids.acf.list.out <- list(plot.acf = resids.acf.plot)
  #
  ## --------------- AIC and BIC (Akaike and Bayesian) Metrics ----------------
  #
  ## see e.g. https://doi.org/10.1177/0049124104268644,
  ## https://www.amazon.com/Model-Selection-Multimodel-Inference-Information-Theoretic/dp/0387953647,
  ## https://www.jstor.org/stable/41413993,
  ## https://onlinelibrary.wiley.com/doi/pdf/10.1002/9781118856406.app5,
  ## https://rpubs.com/RRD27/ts7
  #
  ## see also the function `eval_ABIC_forFit` automatic recognition
  ## of residual/error distribution
  AB.ic.list <-
    eval_ABIC_forFit(
      data.fit = new.predict.df,
      residuals = "residuals",
      k = length(params.guess) ## number of parameters
    )
  #
  ## THE FOLLOWING REPLACED BY `AB.ic.list` and `eval_ABIC_forFit`
  # negativ.2fold.loglike <-
  #   nrow(new.predict.df) *
  #   (1 + log(2 * pi) + log(residsq.react.kin.fit / nrow(new.predict.df)))
  # #
  # ## ...=> both criteria:
  # A.ic <-
  #   negativ.2fold.loglike + (2 * length(params.guess)) +
  #   (2 * length(params.guess) * (length(params.guess) + 1) /
  #      (nrow(new.predict.df) - length(params.guess) - 1))
  # #
  # B.ic <-
  #   negativ.2fold.loglike + (log(nrow(new.predict.df)) * length(params.guess))
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
    ra = append(resid.anal.simple.list,resids.acf.list.out),
    df.coeffs = df.result,
    cov.coeffs = coeff.cov,
    cor.coeffs = coeff.cor,
    N.evals = iters.react.kin.fit,
    min.rss = residsq.react.kin.fit,
    abic = AB.ic.list, ## list
    ## covariance of "qvarR,fitted,residuals" data frame:
    cov.df = df.cov,
    ## correlation of "qvarR,fitted,residuals" data frame:
    cor.df = df.cor,
    N.converg = converg.react.kin.fit
  )
  #
  return(fit.summary)
  #
}
