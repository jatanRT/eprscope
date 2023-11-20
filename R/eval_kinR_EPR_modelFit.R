#'
#' Radical Kinetic Models Fitted to Experimental Data
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#' A short description...(Integrals/Areas \emph{vs.} Time)...
#'
#'
#'
#' @param data.integs Data frame object Integrals/Areas \emph{vs.} Time
#' @param time.unit Character string ... argument/parameter... tbc
#' @param time Character string ... argument/parameter... tbc
#' @param qvarR Character string ... argument/parameter... tbc
#' @param model.react Character string ... argument/parameter... tbc, the same like in \code{\link{eval_kinR_ODE_model}}
#' @param params.guess Named vector ... argument/parameter... tbc
#' @param fit.kin.method Character string ... argument/parameter... tbc
#' @param time.correct Logical, ... argument/parameter... tbc
#' @param path_to_dsc_par Character string ... argument/parameter... tbc
#' @param origin Character string ... argument/parameter... tbc
#'
#'
#' @return List ... tbc ...
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
                                   model.react = "(x=1)R --> [k1] B",
                                   params.guess = c(
                                     qvar0R = 1e-3,
                                     k1 = 1e-3
                                   ),
                                   fit.kin.method = "diff-levenmarq",
                                   time.correct = FALSE,
                                   path_to_dsc_par = NULL,
                                   origin = NULL) {
  #
  ## 'Temporary' processing variables
  # . <- NULL
  fitted <- NULL
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
      stop(" Please define origin and the path for file incl. instrumental parameters ! ")
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
  ## ---------------------------- DERIVATIVE FORM Fit -----------------------------
  #
  ## Fit by solution of Ordinary Differential equations
  #
  if (fit.kin.method == "diff-levenmarq") {
      model.react.kin.fit <- minpack.lm::nls.lm(
      par = params.guess,
      fn = eval_kinR_ODE_model,
      model.react = model.react,
      model.expr.diff = TRUE,
      data.expr = data.integs,
      time.expr = time,
      qvar.expr = qvarR
    )
    #
    ## Summary as table
    summar.react.kin.fit.df <- as.data.frame(summary(model.react.kin.fit)$coefficients)
    ## number of iterations/evaluations
    iters.react.kin.fit <- model.react.kin.fit$niter
    ## total sum of residual squares
    residsq.react.kin.fit <- model.react.kin.fit$deviance
    ## vector of particular residual squares at each iteration
    converg.react.kin.fit <- model.react.kin.fit$rsstrace
    #
    ## obtained parameters from the fit
    predict.model.params <- as.vector(summar.react.kin.fit.df$Estimate)
    names(predict.model.params) <- rownames(summar.react.kin.fit.df)
    #
    ## parameters from the fit applied to generate `R` (`qvarR`)
    ## with experimental `time` <=> it corresponds to `predicted`
    model.expr.time <- eval_kinR_ODE_model(
      model.react = model.react,
      model.expr.diff = FALSE,
      kin.params = predict.model.params,
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
    ## the `model.expr.time` and `model.react.kin.fit` is not required anymore
    rm(model.expr.time, model.react.kin.fit)
  }
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
  ## Caption
  # plot.params.names <- lapply(names(predict.model.params),
  #                                    function(i) bquote(bolditalic(string2lang(.(i)))))
  # plot.caption <- Map(function(i,j) bquote(.(i) == .(j)),plot.params.names,predict.model.params)
  ## final.plot
  plot.fit <- plot.fit.base +
    labs(
      title = model.react,
      color = "",
      caption = "Least-Square Fit by Levenberg-Marquardt Algorithm and
                    Numerical Solution of Ordinary Differential Equations System.",
      x = bquote(italic(Time) ~ ~"(" ~ s ~ ")"),
      y = bquote(italic(Integral ~ ~Intensity) ~ ~"(" ~ p.d.u. ~ ")")
    ) +
    plot_theme_In_ticks() +
    scale_x_continuous(sec.axis = dup_axis(name = "", labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = "", labels = NULL)) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 13),
      legend.key.size = unit(1.4, "lines"),
      legend.box.margin = margin(l = -0.24, unit = "in")
    )
  #
  ## Summary
  fit.summary <- list(
    df = new.predict.df,
    plot = plot.fit,
    df.coeffs = summar.react.kin.fit.df,
    N.evals = iters.react.kin.fit,
    sum.LSQ.min = residsq.react.kin.fit,
    sum.LSQ.evals = converg.react.kin.fit
  )
  #
  return(fit.summary)
  #
}
