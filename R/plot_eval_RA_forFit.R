#'
#' General Diagnostics for Models/Fits by Simple Residual Analysis
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   Three visual diagnostic tools (based on the \href{https://ggplot2.tidyverse.org/}{\code{{ggplot2}}}
#'   and the \href{https://aloy.github.io/qqplotr/}{\code{{qqplotr}}} packages) and one metric (standard
#'   deviation of residuals) are applied to evaluate the accuracy and appropriateness as well as to compare
#'   different models/fits. 1. The first plot represents the Residuals \emph{vs} Fitted/Simulated Values relation.
#'   For a decent model/fit, it will exhibit randomly scattered values around \code{0} and displays a similar
#'   variance over all predicted/fitted values. 2. \emph{Sample Quantiles vs Theoretical Quantiles} (Q-Q plot)
#'   shows, whether the "appearence" of residuals can be described by the normal probability distribution.
#'   3. The latter is combined with a more detailed information about the distribution of residuals
#'   by the \strong{histogram} (\code{\link[ggplot2]{geom_histogram}}) as well as by the \strong{probability density}
#'   (\code{\link[ggplot2]{geom_density}}including the residuals mean value, and median which in ideal case equal to \code{0}).
#'
#'
#' @details
#'   Analysis of residuals is a powerful statistical technique to check whether the model/fit has adequately
#'   captured the information in the data (please, also refer to the \code{\link{eval_ABIC_forFit}}).
#'   The \eqn{i}-th residual/error (in the series) is defined as the difference between the \eqn{i}-th observed response
#'   value (\eqn{y_i}) and that of the predicted/fitted by the model (\eqn{\hat{y}_i} or \eqn{y_{\text{fit/model}}}):
#'   \deqn{e_i = y_i - \hat{y}_i = y_i - y_{\text{fit/model}}}
#'   One might think of residuals as "leftovers", i.e. these are the issues/items which are unexplained,
#'   after the model/fit has been applied. The residual analysis therefore helps us to to determine whether
#'   our model/fit missed an important pattern or not at all (see Chugani (2025) in the \code{References}).
#'   Residuals are also used in least square fitting/optimization procedures where the sum of their squares
#'   is to be minimized throughout the iterations (see e.g. \code{\link{eval_sim_EPR_isoFit}}
#'   or \code{\link{eval_kinR_EPR_modelFit}}).
#'
#'   In addition to the original "raw" residuals defined above, one may come across other types,
#'   like \href{https://bookdown.org/mike/data_analysis/generalized-linear-models.html}{"Pearson" (or "scaled")},
#'   \strong{"standardized" or "studentized"}. \strong{The latter two are helpful to identify outliers},
#'   which are data points that are significantly different from the rest of the data
#'   (though, they can be also identified by the Q-Q plot and histogram-probability density).
#'   For such reason the "raw" residuals (\eqn{e_i}) are divided by their standard
#'   deviation (\eqn{sd} see the \code{Value} below), including the the effect of leverage.
#'   Thus, the formula for standardized residual reads: \eqn{r_i = e_i\,/\,(sd\,\sqrt{1 - h_{ii}})},
#'   where the \eqn{h_{ii}} stands for the diagonal element of the "leverage" \eqn{\hat{H}} matrix
#'   (see e.g. last three \code{References}). The simple explanation of the leverage phenomenon is following
#'   (see e.g. Speegle D, Clair B (2021) or The Pennsylvania State University (2018) in the \code{References}).
#'   The fit or the regression line goes through the center of mass of the (experimental) data (\eqn{x},\eqn{y}).
#'   Lets assume two points \eqn{A} and \eqn{B}, where the \eqn{A} \eqn{x}-coordinate is close to the mean value
#'   of predictors (\eqn{mean(x)}) and the \eqn{B} possesses an extreme \eqn{x} (far from the mean value).
#'   Therefore, changing the \eqn{A\,(y)} will induce a small effect (low leverage) on the fit/regression line,
#'   whereas the \eqn{B\,(y)} change will dramatically influence the fit (high leverage).
#'   \strong{Both standardized and studentized residuals can be automatically calculated for linear models} like
#'   \code{\link[stats]{lm}} and \code{\link[stats]{glm}}, using
#'   the \code{\link[stats:influence.measures]{stats::rstandard}}
#'   and \code{\link[stats:influence.measures]{stats::rstudent}}. A very detailed analysis for linear models is provided
#'   by the \href{https://goodekat.github.io/ggResidpanel/}{\code{{ggResidpanel}}} package. Additionally, a series
#'   of diagnostic plots can be also created in the base \emph{R}, just by \code{plot(var)}, where the \code{var}
#'   represents the variable/object of the linear model. On the other hand, \strong{all these diagnostics are not
#'   available for non-linear models} like \code{\link[stats]{nls}}. Accordingly, \strong{such type of calculations
#'   can be provided by other packages}
#'   (see e.g. \href{https://cran.r-project.org/web/packages/nlstools/vignettes/vignetteJSS.pdf}{\code{{nlstools}}})
#'   \strong{or must be performed "manually", evaluating the numerical approximation of the gradient}
#'   (please, refer to the \href{https://r-forge.r-universe.dev/numDeriv/doc/manual.html#jacobian}{jacobian})
#'   as reported elsewhere (see Nguyen M (2020) in the \code{References}). Consequently, the calculations of standardized
#'   and studentized residuals are not involved in this general \code{plot_eval_RA_forFit} function. Nevertheless, users are advised
#'   to apply above-described options to obtain the desired residuals or diagnostic plots for a specific model/fit
#'   (also refer to the \code{Value}/\code{df}).
#'
#'   \strong{Considerations to support or exclude model/fit based on the residual plot.} If the residuals exhibit
#'   no clear pattern, like they are randomly scattered around the \code{0} with no systematic increase or decrease
#'   in variance, we may trust our fit with the optimized parameters. Such pattern is \strong{homoscedastic}.
#'   However, if one recognizes curved ((inverted-)U-shape, see e.g. \code{Examples}
#'   in the \code{\link{eval_kinR_EPR_modelFit}}), wave or systematic increase (so called "fanning")
#'   or decrease ("funelling"), the model/fit is untrustworthy and one would probably search for a different (better) one.
#'   In particular, the curved pattern in the residual plot may indicate that a model does a poor job of fitting
#'   and likely we need additional parameter(s) to describe our data properly. In the case if residuals suffer
#'   from unequal variance at different levels of the fitted values, the residuals are referred
#'   to as \strong{heteroscedastic}.
#'
#'   \strong{Considerations to support or exclude model/fit based on the Q-Q plot, histogram and probability density.}
#'   If the residuals are assumed to be normally distributed, one can use a normal Q-Q (Quantile-Quantile) plot
#'   to check this assumption or its violation. Quantiles are often referred to as "percentiles". These are actually
#'   the data points below which a certain portion of the data fall or in other words, they divide the distribution
#'   into equal portions. For instance, for a 0.5 quantile (or the 2nd quartile), half of the data lie below this
#'   point and half of them above. This quantile is also referred to as \strong{median}. Similarly, the 0.25 quantile
#'   (or the 1st quartile) would mean that \eqn{25\,\%} of the data fall below this point. The Q-Q plot is actually
#'   presenting the quantiles from our sample related to the theoretical ones calculated for the normal distribution.
#'   If the points follow the diagonal line (or are not far from this line), we may assume a normal
#'   distribution. Additionally, this assumption can be also supported either by the Shapiro-Wilk
#'   (\code{\link[stats]{shapiro.test}}) or by the Kolmogorov-Smirnov (\code{\link[stats]{ks.test}}) tests in the base R.
#'   Deviations from the diagonal line are also nicely reflected in the histogram and probability density
#'   function/graph (PDF, providing the chances that the value of a random continuous variable will
#'   occur within a specific range of values). For the normal symmetric distribution it is represented by the bell-shaped
#'   curve with the maximum at the \strong{mean} (for residuals \eqn{= 0}, median = mean). Thus, the PDF basically
#'   corresponds to histogram with "extremely" high number of bins, having "extremely" small widths.
#'   A Q-Q plot may exhibit several basic
#'   \href{https://stats.libretexts.org/Bookshelves/Advanced_Statistics/Intermediate_Statistics_with_R_(Greenwood)/03\%3A_One-Way_ANOVA}{deviations}.
#'   It can display a U-shape pattern above the diagonal line, which actually mirrors
#'   the situation with the right skewed (or positively skewed, mean > median) PDF. Therefore, we find the extreme
#'   values far from the peak on the high end more frequently than on the lower one (see e.g. \code{Example} in
#'   \code{\link{eval_kinR_Eyring_GHS}}). Contrary, if the Q-Q plot shows
#'   "hill" shape bellow the diagonal line, the opposite situation is observed and the extreme values (outliers) far from the peak
#'   on the low end appear more frequently than on the higher one (PDF is left skewed, mean < median).
#'   The Q-Q plot with so-called light tails, displays extreme values above and below residual minima and maxima,
#'   respectively. This pattern is relatively harmless and one can proceed with methods that assume normality safely.
#'   However, the heavy-tailed Q-Q plot with extreme residuals below and above minima and maxima, respectively,
#'   is somewhat problematic for normal distributions of residuals with outliers on both sides.
#'   On the other hand, such kind of normality violation can be successfully described by Student's t-distribution
#'   with lower degrees of freedom (see e.g. \code{Examples} in the \code{\link{eval_sim_EPR_isoFit}}
#'   as well as \code{\link{eval_ABIC_forFit}}). Nevertheless, if the residuals, in the latter case,
#'   do not exhibit heteroscedasticity, such model/fit is not necessarily untrustworthy.
#'   In a very extreme case the heavy-tailed Q-Q plot may be transformed into situation where only a couple
#'   of points around the "middle" quantile can be found on the diagonal line. This is reflected
#'   by the bimodal behavior in the PDF and it might be the sign of value clustering.
#'
#'   All the above-mentioned violations of the residuals (normal) distribution can disfavor our considered model/fit.
#'   However, one has to perform different diagnostic methods and tests to analyze the residuals in order to compare
#'   several models/fits and select the "best" one. Even in such case, this should be a compromise between
#'   the fit accuracy (fitting the data as well as possible) and the parsimony
#'   (using a simple and replicable model/fit, Kabacoff RI (2022) in the \code{References}).
#'
#'
#' @references
#'   Kabacoff RI (2022). \emph{R in Action}, 3rd edition, Manning Publications Co., ISBN 978-1-617-29605-5,
#'   \url{https://www.manning.com/books/r-in-action-third-edition}.
#'
#'   Svetunkov I (2022). \emph{Statistics for Business Analytics}, Version 2025,
#'   \url{https://openforecast.org/sba/}.
#'
#'   Hyndman RJ, Athanasopoulos G (2021). \emph{Forecasting: Principles and Practise}, 3rd edition,
#'   O Texts, ISBN 978-0-987-50713-6, \url{https://otexts.com/fpp3/}.
#'
#'   Chugani V (2025). "The Concise Guide to Residual Analysis",
#'   \url{https://www.statology.org/concise-guide-residual-analysis/}.
#'
#'   Boehmke B, Greenwell B (2020). \emph{Hand on Machine Learning with R}, 1st edition, Chapman and Hall/CRC,
#'   ISBN 978-1-138-49568-5, \url{https://bradleyboehmke.github.io/HOML/}.
#'
#'   Kuhn M, Silge J (2023). \emph{Tidy Modelling with R}, 1st edition (Version 1.0.0), O'Reilly Media,
#'   ISBN 978-1-492-09648-1, \url{https://www.tmwr.org/}.
#'
#'   Belzile L (2019). "lineaRmodels", \url{https://lbelzile.github.io/lineaRmodels/}
#'
#'   James G, Witten D, Hastie T, Tibshirani R (2021). \emph{An Introduction to Statistical Learning:
#'   with Applications in R}, 2nd edition, Springer, ISBN 978-1-071-61417-4,
#'   \url{https://www.statlearning.com/}.
#'
#'   Speegle D, Clair B (2021). \emph{Probability, Statistics and Data: A Fresh Approach Using R},
#'   1st edition (Version 2024), Chapman and Hall/CRC, ISBN 978-0-367-43667-4,
#'   \url{https://probstatsdata.com/}.
#'
#'   The Pennsylvania State University (2018). "STAT 462 Applied Regression Analysis, Lesson 9: Influential Points",
#'   \url{https://online.stat.psu.edu/stat462/node/87/}.
#'
#'   Nguyen M (2020). "A Guide on Data Analysis", \url{https://bookdown.org/mike/data_analysis/}.
#'
#'   Gray JB, Woodal WH (1994). "The Maximum Size of Standardized and Internally Studentized Residuals in Regression Analysis",
#'   \emph{Am. Stat.}, \strong{48}(2), 111-113, \url{https://www.jstor.org/stable/2684258}
#'
#'   Frost J (2025). "Statistics by Jim: Making Statistics Intuitive", \url{https://statisticsbyjim.com/}.
#'
#'
#' @inheritParams eval_ABIC_forFit
#' @param fitted Character string, pointing to variable/column header with (model) fitted/predicted values, depending
#'   on the \code{data.fit} argument (usually \code{fitted = "fit(ted)"}, \code{fitted = "predicted"}
#'   or \code{fitted = "theory"}). \strong{Default}: \code{fitted = NULL}.
#' @param resid.method.smooth Character string, corresponding to smoothing method (function) to follow
#'   the trends of residual plot (Residuals \emph{vs} Fitted/Predicted values).
#'   \strong{Default}: \code{resid.method.smooth = "loess"} ("local regression" or "locally estimated
#'   scatter plot smoothing"). Additional methods like \code{"lm"} (linear method/model)
#'   or \code{"glm"} can be applied as well. For details and if \code{resid.method = NULL} (related to automatic
#'   method selection, based on number of points), please consult the \code{\link[ggplot2]{geom_smooth}} for documentation.
#' @param resid.xlab Character string, pointing to \eqn{x}-axis label for residual plot
#'   (Residuals \emph{vs} Fitted/Predicted values). \strong{Default}: \code{resid.xlab = NULL}, actually
#'   corresponding to \code{"Fitted or Simulated Values"}. Customized labels like \code{"Kinetic Model Fit (qvar)"}
#'   or \code{"Quantity (unit)"} can be applied as well.
#' @param se Logical ("standard error", \strong{default}: \code{se = TRUE}), whether to display the confidence
#'   interval (CI) around the smooth (inherited from the \code{\link[ggplot2]{geom_smooth}}).
#'   The CI level is controlled by the \code{level.cnfd} argument.
#' @param level.cnfd Numeric value, identical with the level of applied confidence interval
#'   (see also the \code{se} argument). \strong{Default}: \code{level.cnfd = 0.95}.
#'
#'
#' @returns A List, consisting of the following elements is returned:
#'   \describe{
#'   \item{df}{Original \code{data.fit} data frame object, if additional processing/analysis
#'   is required (see the \code{Details} or to perform Residuals \emph{vs} Observation Order to verify
#'   the assumption that the residuals are independent from one another).}
#'   \item{rqq.plot}{Ggplot2 object related to visual \strong{r}esidual
#'   \strong{a}nalysis), with two main plots: Residuals \emph{vs} Predicted/Fitted Values from
#'   the model/fit or simulation, and the Q-Q plot (Sample Quantiles \emph{vs} Theoretical Quantiles,
#'   where the theoretical ones correspond to normal distribution).}
#'   \item{histDens.plot}{Ggplot2 object, showing the \strong{hist}ogram
#'   and the scaled probability \strong{dens}ity function for residuals. The corresponding residuals
#'   mean value and the median are identified by vertical lines.}
#'   \item{sd}{\strong{S}tandard \strong{d}eviation of residuals (or residual standard error (RSE))
#'   for the model/fit defined as:
#'   \deqn{\sqrt{\sum_i (y_i - y_{i,\text{fit/model}})^2\,/\,(N - k - 1)}}
#'   where \eqn{N} is the number of observations/points (see the \code{data.fit} argument) and \eqn{k}
#'   is the number of optimized parameters (see the argument \code{k}). Therefore, the smaller
#'   the \code{sd}, the better the fit, when comparing different models/fits.}
#'   }
#'
#'
#' @examples
#' \dontrun{
#' ## application example for an EPR simulation fit
#' list.test <-
#'   plot_eval_RA_forFit(
#'     data.fit = sim.fit.tmpd.df,
#'     residuals = "Residuals",
#'     fitted = "Simulation",
#'     k = 8, ## number of optimized sim. parameters
#'     resid.xlab = "Simulation",
#'     plot.densScale.coeff = 500,
#'     level.cnfd = 0.99999999
#'    )
#' #
#' ## residual plot with normal
#' ## and studentized residuals
#' list.test$rqq.plot
#' #
#' ## histogram and probability density
#' list.test$histDens.plot
#' #
#' ## standard deviation of residuals
#' list.test$sd
#' #
#' ## from the data quickly create the residuals vs
#' ## observation order plot (assuming, there
#' ## is no index column in the data frame)
#' dataframe <- list.test$df
#' dataframe[["index"]] <- 1:nrow(dataframe)
#' plot(
#'   dataframe$index,
#'   dataframe$Residuals,
#'   xlab = "Observation Order",
#'   ylab = "Residuals"
#' )
#' #
#' ## for additional applications, please,
#' ## refer to the `eval_sim_EPR_isoFit`
#' ## or `eval_kinR_EPR_modelFit`
#' #
#' }
#'
#'
#' @export
#'
#' @importFrom ggplot2 annotate ggplot_build
plot_eval_RA_forFit <- function(data.fit, ## data frame with at least predicted and experimental values
                                residuals = NULL, # string for column name
                                fitted = NULL, ## string for column name like "fitted", "predicted", "Simulation"
                                resid.method.smooth = "loess", # or "lm" for residuals, # the same like `geom_smooth`/`stat_smooth`
                                resid.xlab = NULL, ## label for x axis # character strings like "Kinetic Model Fit"
                                ## also like `parse(text = resid.xlab)[[1]]`
                                se = TRUE, ## the same like `geom_smooth` or `stat_smooth` for residual plot and q-q plot
                                k, # number of parameters
                                level.cnfd = 0.950) { ## confidence level, or 0.99999999 (for simulations)
  #
  ## 'Temporary' processing variables
  . <- NULL
  count <- NULL
  #
  ## check column of `data.fit` like "residuals":
  if (is.null(residuals) || is.null(fitted)) {
    stop(' Does your data frame already contain a column header(s),\n
         pointing to "residuals" and/or "fitted"/"predicted" values?\n
         If yes, please, provide it ! Refer to the corresponding arguments.\n
         If it is not the case, calculate the column(s) within the `data.fit` !! ')
  }
  #
  ## number of observations
  Nobs <- nrow(data.fit)
  #
  ## standard deviation (sometimes as standard error)
  ## of residuals for the model
  ra.sd.model <-
    sqrt(sum(data.fit[[residuals]]^2)) / sqrt(Nobs - k - 1)
  #
  ## condition for the `resid.method.smooth` and formula
  ## see also documentation for `?ggplot2::geom_smooth`
  if (is.null(resid.method.smooth)){
    fm <- NULL ## automatic decision based on `Nobs`
  } else if (resid.method.smooth == "loess" & Nobs < 1000) {
    fm <- y ~ x
  } else if (resid.method.smooth == "loess" & Nobs > 1000) { # automatic decision
    resid.method.smooth <- NULL
    fm <- NULL
  } else if (resid.method.smooth == "lm" ||
             resid.method.smooth == "glm") {
    fm <- y ~ x
  }
  #
  ## xlabel (residual plot) definition
  resid.xlab <-
    resid.xlab %>% `if`(is.null(resid.xlab),"Fitted or Simulated Values", .)
  #
  ## condition for x-axis label in residual plot
  resid.xlab.vec <- unlist(strsplit(resid.xlab,"\\s+"))
  str.parenth.id <- grep("\\(.*\\)",resid.xlab.vec)
  if (!any(grepl("\\(.*\\)",resid.xlab.vec))) {
    xlabel <- bquote(italic(.(resid.xlab)))
  } else {
    str.no.parenth.vec <- resid.xlab.vec[-str.parenth.id]
    str.xlab <- sapply(
      1:length(str.no.parenth.vec),
      function(l) str.no.parenth.vec[l]
    )
    str.xlab.no.parenth <- paste(str.xlab,collapse = " ")
    xlabel <- bquote(
      italic(.(str.xlab.no.parenth))~~.(resid.xlab.vec[str.parenth.id])
    )
  }
  #
  ## residual plot (`{ggplot2}`)
  plot.resids <-
    ggplot(data = data.fit,
           mapping = aes(
             x = .data[[fitted]],
             y = .data[[residuals]] ## based on condition
           )
    ) +
    geom_point(size = 2.6,color = "darkblue") +
    stat_smooth(
      method = resid.method.smooth, ## see definitions above
      formula = fm,
      span = 1,
      se = se,
      level = level.cnfd,
      color = "darkviolet",
      fill = "darkgray"
    ) +
    geom_hline(yintercept = 0,color = "darkred") +
    labs(
      x = xlabel,
      y = bquote(italic(Residuals)),
      title = "Residual Plot"
    ) +
    plot_theme_In_ticks()
  #
  ## q-q plot (`{qqplotr}`)
  plot.qq <-
    ggplot(data.fit,
           mapping = aes(
             sample = .data[[residuals]]
           )
    ) +
    {if(se)qqplotr::stat_qq_band(fill = "lightgray",conf = level.cnfd)} +   ## )
    qqplotr::stat_qq_line(color = "darkred") +                              ## } plot confid. interval
    qqplotr::stat_qq_point(size = 2.6,color = "darkblue") +                 ## )
    labs(
      x = bquote(italic(Theoretical~~Quantiles)),
      y = bquote(italic(Sample~~Quantiles)),
      title = "The Normal Q-Q Plot of Residuals"
    ) +
    plot_theme_In_ticks()
  #
  ## histogram with density plot (into results)
  ## must be done step-by-step, in order to scale density
  ## and annotate, see below
  plot.hist.dens.01 <-
    ggplot(data = data.fit,
           mapping = aes(
             x = .data[[residuals]]
           )
    ) +
    geom_histogram(
      fill = "blue",
      alpha = 0.64,
      bins = 40
    )
  #
  plot.hist.dens.02 <-
    plot.hist.dens.01 +
    geom_density(
      ## relative scaled density:
      aes(y = after_stat(
        (count / max(count)) * 0.82 * max(ggplot_build(plot.hist.dens.01)$data[[1]]$count)
        ## see `annotation` below
      )
      ),
      color = "darkorange",
      linewidth = 0.75,
      fill = "darkorange",
      alpha = 0.32
    ) +
    geom_vline( ## showing mean value
      xintercept = mean(data.fit[[residuals]]),
      color = "darkblue",
      linewidth = 0.75
    ) +
    geom_vline(
      xintercept = stats::median(data.fit[[residuals]]),
      color = "darkred",
      linewidth = 0.75
    ) +
    labs(
      x = bquote(italic(Residuals)),
      y = bquote(italic(Counts)),
      title = "Histogram and Scaled Probability Density" #,
      # caption = "\u2013 Residuals mean value"
    )
  #
  ## max counts to annotate using the `ggplot_build`,
  ## see https://stackoverflow.com/questions/14584093/ggplot2-find-number-of-counts-in-histogram-maximum
  plot.hist.dens.03 <-
    plot.hist.dens.02 +
    annotate(
      geom = "text",
      x = c(mean(data.fit[[residuals]]),stats::median(data.fit[[residuals]])),
      y = c(
        0.5 * max(ggplot_build(plot.hist.dens.02)$data[[1]]$count),
        0.6 * max(ggplot_build(plot.hist.dens.02)$data[[1]]$count)
      ),
      label = c("mean","median"),
      angle = 60,
      color = c("darkblue","darkred"),
      size = 5,
      fontface = "bold"
    ) +
    plot_theme_In_ticks(
      # plot.caption = element_text(
      #   color = "darkviolet",
      #   face = "bold"
      # )
    )
  #
  ## patchwork combination of both plots:
  plot.ra <-
    patchwork::wrap_plots(
      plot.resids,
      plot.qq,
      ncol = 1
    )
  #
  ## results
  result.list <- list(
    df = data.fit,
    rqq.plot = plot.ra,
    histDens.plot = plot.hist.dens.03,
    sd = ra.sd.model
  )
  #
  return(result.list)
  #
}
