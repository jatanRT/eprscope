#'
#' Radical Kinetic Models Fitted to Experimental Data (Integrals/Areas \emph{vs.} Time)
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#' A short description...
#'
#'
#'
#' @param data.spectra.integ tbc
#' @param time.unit Character string ... argument/parameter... tbc
#' @param time.series Character string ... argument/parameter... tbc
#' @param qvarR Character string ... argument/parameter... tbc
#' @param model.react Character string ... argument/parameter... tbc, the same like in \code{\link{eval_kinR_ODE_model}}
#' @param params.guess Named vector ... argument/parameter... tbc
#' @param algorithm.fit.kin Character string ... argument/parameter... tbc
#' @param time.series.correct Logical, ... argument/parameter... tbc
#' @param path_to_DSC_or_par Character string ... argument/parameter... tbc
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
eval_kinR_EPR_modelFit <- function(data.spectra.integ,
                                   time.unit = "s",
                                   time.series = "time_s",
                                   qvarR = "Area",
                                   model.react = "(x=1)R --> [k1] B",
                                   params.guess = c(qvar0R = 1e-3,
                                                    k1 = 1e-3),
                                   algorithm.fit.kin = "diff-LM",
                                   time.series.correct = FALSE,
                                   path_to_DSC_or_par = NULL,
                                   origin = NULL){
  #
  ## 'Temporary' processing variables
  #. <- NULL
  fitted <- NULL
  ## convert time if other than `s` appears
  if (time.unit == "min"){
    data.spectra.integ[[time.series]] <- data.spectra.integ[[time.series]]*60
    ## rename `time.series`
    names(data.spectra.integ[[time.series]]) <- "time_s"

  }
  if (time.unit == "h"){
    data.spectra.integ[[time.series]] <- data.spectra.integ[[time.series]]*3600
    ## rename `time.series`
    names(data.spectra.integ[[time.series]]) <- "time_s"
  }
  #
  ## corrected time for CW EPR experiment
  if (isTRUE(time.series.correct)){
    if (is.null(path_to_DSC_or_par) & is.null(origin)){
      stop(" Please define origin and the path for file incl. instrumental parameters ! ")
    } else{
      #
      ## instrumental parameters for time series EPR spectra
      instrum.params.kin <- readEPR_params_slct_kin(path_to_DSC_or_par,origin = origin)
      #
      ## correct time
      data.spectra.integ[[time.series]] <- correct_time_Exp_Specs(time.s = data.spectra.integ[[time.series]],
                                                                   Nscans = instrum.params.kin$Nscans,
                                                                   sweep.time.s = instrum.params.kin$sweepTime)
      #
    }
  } else{
    data.spectra.integ[[time.series]] <- data.spectra.integ[[time.series]]
  }
  #
  ## ---------------------------- DERIVATIVE FORM Fit -----------------------------
  #
  ## Fit by solution of Ordinary Differential equations
  #
  if (algorithm.fit.kin == "diff-LM"){
    model.react.kin.fit <- minpack.lm::nls.lm(par = params.guess,
                                              fn = eval_kinR_ODE_model,
                                              model.react = model.react,
                                              model.expr.diff = TRUE,
                                              data.expr = data.spectra.integ,
                                              time.expr.series = time.series,
                                              qvar.expr = qvarR)
    #
    ## Summary as table
    summar.react.kin.fit.df <- as.data.frame(summary(model.react.kin.fit)$coefficients)
    ## number of iterations
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
    model.expr.time <- eval_kinR_ODE_model(model.react = model.react,
                                           model.expr.diff = FALSE,
                                           kin.params = predict.model.params,
                                           data.expr = data.spectra.integ,
                                           time.expr.series = time.series,
                                           qvar.expr = qvarR)
    #
    ## starting new data frame only with `time` and `qvar` &
    ## merge both data frames (add `fitted` columns)
    new.predict.df <- data.spectra.integ %>%
      dplyr::select(.data[[time.series]],.data[[qvarR]]) %>%
      dplyr::mutate(fitted = model.expr.time$df[["R"]])
    #
    ## the `model.expr.time` and `model.react.kin.fit` is not required anymore
    rm(model.expr.time,model.react.kin.fit)
  }
  #
  ## create plot
  plot.fit.base <- ggplot(new.predict.df) +
    geom_point(aes(x = .data[[time.series]],
                   y = .data[[qvarR]],
                   color = "Experimental\nData"),
               size = 2.6) +
    geom_line(aes(x = .data[[time.series]],
                  y = .data$fitted,
                  color = "\nKinetic\nModel Fit"),
              linewidth = 1.1) +
    scale_color_manual(values = c("darkcyan","magenta"),
                       breaks = c("Experimental\nData","\nKinetic\nModel Fit"),
                       guide = guide_legend(override.aes = list(shape = c(16, NA),
                                                                linetype = c("blank", "solid"))))
  #
  ## Caption
  # plot.params.names <- lapply(names(predict.model.params),
  #                                    function(i) bquote(bolditalic(string2lang(.(i)))))
  # plot.caption <- Map(function(i,j) bquote(.(i) == .(j)),plot.params.names,predict.model.params)
  ## final.plot
  plot.fit <- plot.fit.base +
    labs(title = model.react,
         color = "",
         caption = "Least-Square Fit by Levenberg-Marquardt Algorithm and
                    Numerical Solution of Ordinary Differential Equations System.",
         x = bquote(italic(Time)~~"("~s~")"),
         y = bquote(italic(Integral~~Intensity)~~"("~p.d.u.~")")) +
    plot_theme_In_ticks() +
    scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 13),
          legend.key.size = unit(1.4, 'lines'),
          legend.box.margin = margin(l = -0.24,unit = "in"))
  #
  ## Summary
  fit.summary <- list(df = new.predict.df,
                      plot = plot.fit,
                      coeff = summar.react.kin.fit.df,
                      niter = iters.react.kin.fit,
                      resid.sum.sqr = residsq.react.kin.fit,
                      converg =  converg.react.kin.fit)
  #
  return(fit.summary)
  #
}
