#'
#' General Diagnostics for Models/Fits by Simple Residual Analysis
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   Three visual diagnostic tools (based on the \href{https://ggplot2.tidyverse.org/}{\code{{ggplot2}}}
#'   package) and one metric (standard deviation of residuals) are applied to evaluate the appropriateness as well as to compare
#'   different models/fits. 1. The first plot represents the Residuals \emph{vs} Fitted/Simulated Values relation.
#'   For a decent model/fit, it will exhibit randomly scattered values around \code{0} and displays a similar
#'   variance over all predicted/fitted values. 2. \emph{Sample Quantiles (Residuals) vs Theoretical Quantiles} (Q-Q plot)
#'   shows, whether the appearance of residuals can be described by the three probability distributions:
#'   \code{c("norm","t","cauchy")} (i.e. Normal or Student's t or Cauchy, see also the \code{\link{eval_ABIC_forFit}} function).
#'   3. The latter is combined with a more detailed information about the distribution of residuals by the \strong{histogram}
#'   (\code{\link[ggplot2]{geom_histogram}}) as well as by the \strong{probability density} (\code{\link[ggplot2]{geom_density}},
#'   including the residuals \code{mean} value, and \code{median} which in ideal case equal to \code{0}).
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
#'   which are the data points that are significantly different from the rest of the data
#'   (though, they can be also identified by the Q-Q plot and histogram-probability density).
#'   For such reason the "raw" residuals (\eqn{e_i}) are divided by their standard
#'   deviation (\eqn{sd} see the \code{Value} below), including the the effect of leverage.
#'   Thus, the formula for standardized residual reads: \eqn{r_i = e_i\,/\,(sd\,\sqrt{1 - h_{ii}})},
#'   where the \eqn{h_{ii}} stands for the diagonal element of the "leverage" \eqn{\hat{H}} matrix
#'   (see e.g. \code{References} 11-13). The simple explanation of the leverage phenomenon is following
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
#'   to apply above-described options to obtain these specialized residuals or diagnostic plots for a specific model/fit
#'   (also refer to the \code{Value}/\code{df}) if needed.
#'
#'   \strong{Considerations to support or exclude model/fit based on the residual plot.} If the residuals exhibit
#'   no clear pattern, like they are randomly scattered around the \code{0} with no systematic increase or decrease
#'   in variance, we may trust our fit with the optimized parameters. Such pattern is \strong{homoscedastic}.
#'   However, if one recognizes curved ((inverted-)U shape, see e.g. \code{Examples}
#'   in the \code{\link{eval_kinR_EPR_modelFit}}), wave or systematic increase (so called "fanning")
#'   or decrease ("funelling"), the model/fit is untrustworthy and one would probably search for a different (better) one.
#'   In particular, the curved pattern in the residual plot may indicate that a model does a poor job of fitting
#'   and likely, we need additional parameter(s) to describe our data properly. In the case if residuals suffer
#'   from unequal variance at different levels of the fitted values, the residuals are referred
#'   to as \strong{heteroscedastic}. In order to predict pattern of the residual plot, the corresponding relationship
#'   is fitted by the \code{loess} (default) or \code{lm}/\code{glm} function (see the \code{resid.method.smooth} argument) where
#'   the confidence level band of the fitted/regression line can be controlled by the \code{level.cnfd} argument
#'   or by the \code{Value} output function \code{plot.rqq()} (and its \code{confidence} argument).
#'
#'   \strong{Considerations to support or exclude model/fit based on the Q-Q plot, histogram and probability density.}
#'   If the residuals are assumed to be normally distributed, one can use a normal Q-Q (Quantile-Quantile) plot
#'   to check this assumption or its violation. Quantiles are often referred to as "percentiles". These are actually
#'   the data points, dividing the distribution into equal portions. For instance, for a 0.5 quantile (or the 2nd quartile),
#'   half of the data lie below this point and half of them above. This quantile is also referred to as \strong{median}.
#'   Similarly, the 0.25 quantile (or the 1st quartile) would mean, that \eqn{25\,\%} of the data fall below this point.
#'   Thus, the Q-Q plot presents quantiles from our sample (corresponding to residuals) related to the theoretical
#'   ones calculated for a specific distribution. For such purpose the \code{plot_eval_RA_forFit} function supports
#'   three residual distributions: the normal one, Student's t (including degrees of freedom, \code{df}) as well as the Cauchy one.
#'   If the points follow the diagonal line (or are not far from this line), we may describe our residuals by the specific
#'   distribution. Additionally, e.g. for normal "distro", the Q-Q plot can be also supported by the Shapiro-Wilk
#'   (\code{\link[stats]{shapiro.test}}) and/or by the Kolmogorov-Smirnov (\code{\link[stats]{ks.test}}) which are both included
#'   in the \code{\link{eval_ABIC_forFit}}. Therefore, the latter nicely works in combination with the actual function
#'   to get residuals characteristics (distribution) and consequently, to decide which model/fit is the best.
#'   Deviations from the diagonal/fitted line are also reflected in the histogram and probability density function/graph
#'   (PDF, providing the chances that the value of a random continuous variable will
#'   occur within a specific range of values). For the normal symmetric distribution it is represented by the bell-shaped
#'   curve with the maximum at the \strong{mean} (for residuals \eqn{= 0}, median = mean). Thus, the PDF basically
#'   corresponds to histogram with "extremely" high number of bins, having "extremely" small widths.
#'   A Q-Q plot may exhibit several basic
#'   \href{https://stats.libretexts.org/Bookshelves/Advanced_Statistics/Intermediate_Statistics_with_R_(Greenwood)/03\%3A_One-Way_ANOVA/3.04\%3A_ANOVA_model_diagnostics_including_QQ-plots}{deviations}.
#'   It can display a U-shaped pattern, which actually mirrors
#'   the situation with the right skewed (or positively skewed, mean > median) PDF. Therefore, we find the extreme
#'   values far from the peak on the high end more frequently than on the lower one (see e.g. \code{Example} in
#'   \code{\link{eval_kinR_Eyring_GHS}}). Contrary, if the Q-Q plot shows
#'   "hill" shape, the opposite situation is observed and the extreme values (outliers) far from the peak
#'   on the low end appear more frequently than on the higher one (PDF is left skewed, mean < median).
#'   Often, the heavy-tailed Q-Q plot with extreme residuals below and above minima and maxima of the diagonal line,
#'   respectively, may appear and is somewhat problematic for e.g. normal distributions of residuals with outliers on both sides.
#'   On the other hand, such kind of normality violation can be successfully described by Student's t-distribution
#'   with lower degrees of freedom (see e.g. \code{Examples} in the \code{\link{eval_sim_EPR_isoFit}}.
#'   Nevertheless, if the residuals, in the latter case,
#'   do not exhibit heteroscedasticity, such model/fit is not necessarily untrustworthy.
#'   In a very extreme case the heavy-tailed Q-Q plot may be transformed into situation where only a couple
#'   of points around the "middle" quantile can be found on the diagonal line and the remaining points are represented
#'   by the noticeable S-curve. This is reflected as bimodal behavior in the PDF and it might be the sign of a value clustering.
#'   The Q-Q plot (with "pointwise" confidence bands for the diagonal/fitted line, obtained by the \code{\link[MASS]{rlm}})
#'   in the actual function was built in the similar way like for the \href{https://github.com/aloy/qqplotr/}{\code{{qqplotr}}}
#'   package. The "pointwise" bands are based on the normal confidence intervals.
#'   The confidence level of those bands can be controlled by the \code{level.cnfd} argument
#'   or by the \code{Value} output function \code{plot.rqq()} (and its \code{confidence} argument).
#'
#'   All the above-mentioned violations of the residuals (normal) distribution can disfavor our considered model/fit.
#'   However, one has to perform different diagnostic methods and tests to analyze the residuals in order to compare
#'   several models/fits and select the "best" one. Even in such case, this should be a compromise between
#'   the fit accuracy (fitting the data as well as possible, including the physico-chemical reality of the system)
#'   and the parsimony (using a simple and replicable model/fit, Kabacoff RI (2022) in the \code{References}).
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
#'   \emph{Am. Stat.}, \strong{48}(2), 111-113, \url{https://www.jstor.org/stable/2684258}.
#'
#'   Frost J (2025). "Statistics by Jim: Making Statistics Intuitive", \url{https://statisticsbyjim.com/}.
#'
#'   Kross S (2016). "A Q-Q Plot Dissection Kit", \url{https://seankross.com/2016/02/29/A-Q-Q-Plot-Dissection-Kit.html}.
#'
#'   Walker JA (2020). "Normal Q-Q Plots - what is the robust Line and should we prefer it ?",
#'   \url{https://rdoodles.rbind.io/posts-biocstyle/2020-10-15-normal-q-q-plots-what-is-the-robust-line-and-should-we-prefer-it}.
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
#' @param level.cnfd Numeric value, identical with level of the applied confidence interval
#'   (see also the \code{se} argument). \strong{Default}: \code{level.cnfd = 0.95}.
#'
#'
#' @returns A List, consisting of the following elements is returned:
#'   \describe{
#'   \item{df}{Original \code{data.fit} data frame object, if additional processing/analysis
#'   is required (see the \code{Details} or to perform Residuals \emph{vs} Observation Order to verify
#'   the assumption that the residuals are independent from one another).}
#'   \item{plot.rqq()}{Function, related to visual \strong{r}esidual
#'   \strong{a}nalysis), returning two main plots: Residuals \emph{vs} Predicted/Fitted Values from
#'   the model/fit or simulation, and the Q-Q plot: Sample Quantiles \emph{vs} Theoretical Quantiles,
#'   where the theoretical ones correspond to a specific distribution (normal/Student's/Cauchy).
#'   Therefore, function has the following arguments:
#'   \enumerate{
#'   \item \code{residuals.distro = c("norm","t","cauchy")} (\strong{default}: \code{residuals.distro = "norm"})
#'
#'   \item \code{confidence = level.cnfd} (\strong{default}: \code{confidence = level.cnfd = 0.95})
#'
#'   \item \code{...} additional arguments/parameters for the distro/distribution like \code{df} (degrees of freedom)
#'   for the Student's t one.
#'    }
#'   }
#'   \item{plot.histDens}{Ggplot2 object, showing the \strong{hist}ogram
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
#'     data.fit = data.sim.expr,
#'     residuals = "Residuals",
#'     fitted = "Simulation",
#'     resid.xlab = "Simulation",
#'     k = length(optim.params.init),
#'     level.cnfd = 0.99
#'  )
#' #
#' ## residual and the normal Q-Q plot
#' list.test$plot.rqq()
#' #
#' ## residual and Q-Q plot for the Student's t distro
#' ## with 4 degrees of freedom
#' list.test$plot.rqq(residuals.distro = "t",df = 4)
#' #
#' ## histogram and probability density
#' list.test$plot.histDens
#' #
#' ## standard deviation of residuals
#' list.test$sd
#' #
#' ## from the data, quickly create the residuals vs
#' ## observation order plot (assuming there
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
#' ## for additional examples, please,
#' ## refer to the `eval_sim_EPR_isoFit`
#' ## or `eval_kinR_EPR_modelFit`
#' #
#' }
#'
#'
#' @export
#'
#' @importFrom ggplot2 annotate ggplot_build
#' @importFrom MASS rlm
plot_eval_RA_forFit <- function(data.fit, ## data frame with at least predicted and experimental values
                                residuals = NULL, # string for column name
                                fitted = NULL, ## string for column name like "fitted", "predicted", "Simulation"
                                resid.method.smooth = "loess", # or "lm" for residuals, # the same like `geom_smooth`/`stat_smooth`
                                resid.xlab = NULL, ## label for x axis # character strings like "Kinetic Model Fit"
                                ## also like `parse(text = resid.xlab)[[1]]`
                                k, # number of parameters
                                level.cnfd = 0.950 ## confidence level, or 0.999...etc.
) {
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
  ## ------------ xlabel (residual plot) definition ----------------
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
  ## =========================== PLOTS ===============================
  #
  ##  -------------- Complex function for q-q plot with confidence bands -------------
  ## ------------------ Pointwise Confidence Bands for the Q-Q -------------------
  ## ------ also the residual plot (`plot.resids`) included to vary confidence -------
  #
  ## qq-plot built from scratch, inspired by:
  ## `{qqplotr}`: https://github.com/aloy/qqplotr/blob/master/R/stat_qq_band.R ,
  ## https://github.com/aloy/qqplotr/blob/master/R/stat_qq_line.R
  ## as well as by:
  ## https://slowkow.com/notes/ggplot2-qqplot/ and
  ## https://rdoodles.rbind.io/posts-biocstyle/2020-10-15-normal-q-q-plots-what-is-the-robust-line-and-should-we-prefer-it
  #
  ## sorted residuals
  resids.sorted <- sort(data.fit[[residuals]])
  #
  ## `ppoints` function/variable/vector
  ## returns a vector of points equally spaced between 0 and 1
  ## (or unequally-spaced if weights are provided); probabilities
  ## suitable to produce a quantile-quantile (QQ) plot.
  pp <- stats::ppoints(Nobs)
  #
  ##  ------------ function q-q plot with residuals + residual plot ----------------
  #
  plots.qq.resid <- function(residuals.distro = c("norm","t","cauchy"),
                             confidence = level.cnfd, ## vary conf. also for the residual plot
                             ...){ ## additional params. like df (Student's)
    #
    ## 'Temporary' processing variables
    theoretical <- NULL
    low_band <- NULL
    up_band <- NULL
    #
    ## condition for confidence (not specified in % !!)
    if (confidence > 1) {
      stop("Confidence (level) must be specified as a fraction:\n
           e.g. 0.95, corresponding to 95%, NOT IN '%'!! ")
    }
    #
    ## alpha for `level.cnfd`
    alpha.cnfd <- 1 - confidence
    #
    ## z-critical value for `alpha.cnfd` to create confidence bands
    z_crit <- stats::qnorm(1 - alpha.cnfd/2)
    #
    ## redefinition of `residuals.distro`
    residuals.distro <-
      residuals.distro %>% `if`(length(residuals.distro) > 1,"norm", .)
    #
    ## condition for student's t dgrees of freedom
    if (residuals.distro == "t") {
      ## check the degrees of freedom for the Student't t-distro
      df <- list(...)[["df"]]
      if (is.null(df)) {
        stop(" Please specify the degrees of freedom (df) for the t/Student's\n
             distribution !! See the definition of `...` argument\n
             in the `plot.rqq()` function within the output list !! \n
             To figure out the `df`, please run `list$abic` for your\n
             specific fit output `list`.")
      } else {
        degree.free <- df ## for the graph title
      }
    }
    #
    ## theoretical quantiles + probability density fun. (pdf)
    theor.quantiles <- do.call(
      paste0("q",residuals.distro),
      list(pp,...)
    )
    pdf.values <- do.call(
      paste0("d",residuals.distro),
      list(theor.quantiles,...)
    )
    #
    ## Fitting of the q-q plot line
    ## the thing, why the following fit is not performed by `stats:lm()`,
    ## is that the `MASS::rlm()` is more robust against outliers ('havier tails'),
    ## this is also the reason why it is implemented in the `{car}`
    ## package `qqPlot()` function
    fitlm <- rlm(resids.sorted ~ theor.quantiles,maxit = 50)
    intercpt <- stats::coef(fitlm)[1]
    slop <- stats::coef(fitlm)[2]
    #
    # Calculate confidence bands using the fitted line parameters
    SE <- (slop / pdf.values) * sqrt(pp * (1 - pp) / Nobs)
    fitted.values <- intercpt + slop * theor.quantiles
    upper_band <- fitted.values + (z_crit * SE)
    lower_band <- fitted.values - (z_crit * SE)
    #
    ## data frame for the q-q plot pointwise
    df.qqPlot <- data.frame(
      theoretical = theor.quantiles,
      sample = resids.sorted,
      fitted = fitted.values,
      low_band = lower_band,
      up_band = upper_band
    )
    #
    ## title function
    distro.title.fun <- function(distro = residuals.distro){
      if (distro == "norm"){
        return("Normal")
      }
      if (distro == "t"){
        return(paste0("Student's t (df = ",degree.free, ")"))
      }
      if (distro == "cauchy"){
        return("Cauchy")
      }
    }
    #
    ## ------------------ own q-q plot --------------------
    plot.qq.final <-
      ggplot(
        df.qqPlot,
        aes(x = theoretical, y = sample)
      ) +
      geom_ribbon(
        aes(ymin = low_band,ymax = up_band),
        fill = "blue",
        alpha = 0.16
      ) +
      geom_line(
        aes(y = fitted),
        color = "darkorange",
        linewidth = 1.1
      ) +
      geom_point(size = 2.6,color = "darkblue") +
      labs(
        x = bquote(italic(Theoretical~~Quantiles)),
        y = bquote(italic(Residuals)),
        title = paste0(
          "Q-Q Plot with ",
          confidence * 100,
          "% Confidence Bands"
        ),
        subtitle = paste0("Distribution: ", distro.title.fun()),
        caption = "Residuals = Sample Quantiles"
      ) +
      plot_theme_In_ticks()
    #
    ## ------------------ residual plot -----------------
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
        se = TRUE,
        level = confidence,
        color = "darkviolet",
        linewidth = 1.1,
        fill = "blue",
        alpha = 0.16
      ) +
      geom_hline(yintercept = 0,color = "darkorange",linewidth = 1.1) +
      labs(
        x = xlabel,
        y = bquote(italic(Residuals)),
        title = "Residual Plot"
      ) +
      plot_theme_In_ticks()
    #
    ## combination of qq and residual plot
    plot.ra <-
      wrap_plots( ## `{patchwork}`
        plot.resids,
        plot.qq.final,
        ncol = 1
      ) # +
    # patchwork::plot_layout(axis_titles = "collect",heights = c(0.75,0.75))
    #
    return(plot.ra)
    #
  }
  #
  ## ---------- histogram with density plot (into results) -------------
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
        (count / max(count)) * 0.82 * ## coefficient selected to nicely see both histogram + pdf
          max(ggplot_build(plot.hist.dens.01)$data[[1]]$count)
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
  ## results
  result.list <- list(
    df = data.fit,
    plot.rqq = plots.qq.resid,
    plot.histDens = plot.hist.dens.03,
    sd = ra.sd.model
  )
  #
  return(result.list)
  #
}
