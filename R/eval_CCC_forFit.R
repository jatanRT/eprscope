#'
#' General Ranking of Models/Fits by Concordance Correlation Coefficients and Plot
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   Evaluation of the robust and Lin's concordance correlation coefficients (CCCs). A correlation plot
#'   is provided with the \eqn{45\,\degree} identity line as well as with the corresponding linear fit.
#'   Function can be applied as an agreement metric for the model/fit (\code{y.vec}) \emph{vs} observation/experiment
#'   (\code{x.vec}, see e.g. \code{\link{eval_sim_EPR_isoFit}}). An optional original Bland-Altman plot is available
#'   if \code{check.baplot = TRUE}.
#'
#'
#' @details
#'   Correlation between the two continuous random variables (bivariate correlation) is used to analyze their linear relationship
#'   and the most common measure of such correlation/association is expressed by the
#'   \href{https://online.stat.psu.edu/stat462/node/96/}{Pearson's correlation coefficient (CC)}. However, when it comes to agreement
#'   between the experiment/observation and its corresponding model/fit (e.g. simulation fit of an experimental EPR spectrum)
#'   usually one has to consider not only the qualitative aspect (EPR spectrum shape similarity) of agreement
#'   but also the quantitative one (i.e. the EPR spectrum intensity). In addition, the Pearson's CC can be sensitive
#'   to outliers and skewness of the variable(s) distribution. Therefore, a more robust agreement indices
#'   were introduced (see below and Valleyos RO (2026) and King TS, Chinchilli VM (2001) in the \code{References}.)
#'
#'   The evaluation is based on the widely recognized CCC, developed by Lin (see Lin LIK (1989) in the \code{References}),
#'   which, while sharing certain properties with the Pearson correlation coefficient (wide infra), provides a more appropriate measure
#'   of deviation from the \eqn{45\,\degree} line passing through the origin (with the intercept = 0 and the slope = 1).
#'   This is due to the fact that the CCC accounts for both the magnitude and direction of the deviation from the 45-degree line
#'   in the graphical representation of an agreement. The Lin's CCC is denoted as \eqn{\hat{\rho}_c} (see also \code{Value}
#'   and \code{df.ccc}). It can be expressed in the following form:
#'   \deqn{\hat{\rho}_c = 2\,S_{XY}^{}\,/\,(S_X^2 + S_Y^2 + (\bar{X} - \bar{Y})^2)}
#'   where \eqn{S_{XY}^{} = (1/n)\,\sum_{i=1}^n (X_i - \bar{X})(Y_i - \bar{Y})}, \eqn{S_X^2 = (1/n)\,\sum_{i=1}^n (X_i - \bar{X})^2}
#'   and similarly \eqn{S_Y^2 = (1/n)\,\sum_{i=1}^n (Y_i - \bar{Y})^2}. \eqn{\bar{X}} and \eqn{\bar{Y}} correspond to mean values
#'   of the variables/vectors (refer to the arguments \code{x.vec} and \code{y.vec}). The Lin's CCC (\eqn{\hat{\rho}_c})
#'   can be also calculated taking into account the Pearson coefficient \eqn{r}:
#'   \deqn{\hat{\rho}_c = r\,Cb}
#'   where \eqn{Cb = 2\,/\,(v + (1/v) + u^2)} with \eqn{v = S_X^{}\,/\,S_Y^{}} and
#'   \eqn{u = (\bar{X} - \bar{Y})\,/\,\sqrt{S_X^{}\,S_Y^{}}} (\eqn{S_X^{} = \sqrt{S_X^2}} and \eqn{S_Y^{} = \sqrt{S_Y^2}})
#'   denoting as a scale and location shifts, respectively. The \eqn{Cb} is the correction factor, showing how the
#'   linear fit of \code{y.vec} (\eqn{Y}) \emph{vs} \code{x.vec} (\eqn{X}) deviates from the \eqn{45\,\degree}
#'   identity line. Because the Lin's CCC (\eqn{\hat{\rho}_c}) contains squared distances from the mean values,
#'   it can be sensitive to outliers in the Model/Fit \emph{vs} Observations (or in the residual analysis).
#'
#'   In order to "robustify" the Lin's CCC, King, Chinchilli and Valleyos (see articles in the \code{References})
#'   suggested a general distance function \eqn{g(z) = |z|^{\delta}} where the Monte Carlo simulations indicated
#'   that the best performing \eqn{\delta} (in terms of robustness) are \eqn{\delta = 1} and \eqn{\delta = 1.5}
#'   (with \eqn{\delta = 2} approaching the original Lin's CCC). Such concordance correlation coefficient
#'   can be expressed as follows (proposed by Valleyos RO (2026) see \code{References}):
#'   \deqn{\hat{\rho} = 1 - ((1/n)\,\sum_{i=1}^n |X_i - Y_i|^{\delta})\,/\,((1/n^2)\,\sum_{i=1}^n |X_i - Y_j|^{\delta})}
#'   The denominator is the mean distance over all \eqn{n^2} cross-pairs (disagreement under independence);
#'   the numerator is the mean distance over matched pairs. Setting \eqn{\delta = 2} recovers Lin's CCC;
#'   \eqn{\delta = 1} gives the robust, outlier-resistant version. The cross pairs are calculated using
#'   \code{\link[base]{outer}} and the coefficients are evaluated for all three \eqn{\delta = 1},
#'   \eqn{\delta = 1.5} and \eqn{\delta = 2}.
#'   McBride GB (2005) (see the \code{References}) suggests the following descriptive scale for values of the concordance correlation
#'   coefficient (for continuous variables):
#'   \tabular{lc}{
#'   \strong{CCC Value} \tab \strong{Strength of Agreement} \cr
#'   \eqn{< 0.90} \tab Poor \cr
#'   \eqn{0.90 - 0.95} \tab Moderate \cr
#'   \eqn{0.95 - 0.99} \tab Substantial \cr
#'   \eqn{> 0.99} \tab Almost perfect \cr
#'   }
#'
#' @references
#'   Valleyos RO (2026). "Agreement Coefficients for Continuous Variables: A Review", \emph{Environmetric},
#'   \strong{37}(6), e70117-19, \url{https://onlinelibrary.wiley.com/doi/10.1002/env.70117}.
#'
#'   King TS, Chinchilli VM (2001). "Robust Estimators of the Concordance Correlation Coefficient", \emph{J. Biopharm. Stat.},
#'   \strong{11}(3), 83-105, \url{https://www.tandfonline.com/doi/full/10.1081/BIP-100107651}.
#'
#'   Lin LIK (1989). "A Concordance Correlation Coefficient to Evaluate Reproducibility", \emph{Biometrics},
#'   \strong{45}(1), 255-268, \url{https://www.jstor.org/stable/2532051}.
#'
#'   McBride GB (2005) "A proposal for strength-of-agreement criteria for Lin's Concordance Correlation Coefficient",
#'   NIWA Client Report: HAM2005-062, \url{https://www.medcalc.org/en/download/pdf/McBride2005.pdf}.
#'
#'   Oliveira TP, Moral RA, Zocchi SS, Demetrio CGB, Hinde J. (2020).
#'   "lcc: an R package to estimate the concordance correlation, Pearson correlation and accuracy over time",
#'   PeerJ, \strong{8}, e9850-29, \url{https://doi.org/10.7717/peerj.9850}.
#'
#'   Svetunkov I (2022). \emph{Statistics for Business Analytics}, Version 2025,
#'   \url{https://openforecast.org/sba/}.
#'
#'   Bulut H, Zobu M, Saglam V (2026). "A High-Breakdown MCD-Based Robust Concordance Correlation Coefficient",
#'   \emph{Mathematics}, \strong{14}(1), 196-22, \url{https://doi.org/10.3390/math14010196}.
#'
#'   Nguyen M (2020). "A Guide on Data Analysis", \url{https://bookdown.org/mike/data_analysis/}.
#'
#'   James G, Witten D, Hastie T, Tibshirani R (2021). \emph{An Introduction to Statistical Learning:
#'   with Applications in R}, 2nd edition, Springer, ISBN 978-1-071-61417-4,
#'   \url{https://www.statlearning.com/}.
#'
#'   Karun MK, Puranik A (2021). "BA.plot: An R function for Bland-Altman Analysis", \emph{Clin. Epidemiol. Glob. Health},
#'   \strong{12},100831-3, \url{https://doi.org/10.1016/j.cegh.2021.100831}.
#'
#'
#' @param x.vec Numeric vector (points), corresponding to any experiment or computation (observation),
#'   which is going to be compared to \code{y.vec} (i.e. its model/fit/simulation). For example this argument can be represented
#'   by the points (data frame column/vector) of an experimental EPR spectrum intensity to be fitted or simulated
#'   (can be returned by the \code{\link{eval_sim_EPR_isoFit}}
#'   using the \code{Yourlist$ra$df$Experiment}). In general, it can be applied to any vector of points/observations
#'   to be described by a suitable theoretical model/fit (see the \code{y.vec} argument).
#' @param y.vec Numeric vector, equal to points of a model/fit of its corresponding \code{x.vec} counterpart. For example,
#'   this argument can be represented by points (data frame column/vector) of a simulated EPR intensity fit (can be returned
#'   by the \code{\link{eval_sim_EPR_isoFit}} using the \code{Yourlist$ra$df$Simulation}). In general, it can be applied
#'   to any model/fit which can reasonably describe the experimental or computational pattern of the \code{x.vec}.
#' @param check.baplot Logical. If \code{TRUE},
#'   the original \href{https://www.sciencedirect.com/science/article/pii/S2213398421001391}{Bland-Altman Plot} can be presented
#'   as a \code{Value} list component. \strong{Default}: \code{check.baplot = FALSE}.
#'
#'
#' @returns List with the following components:
#'   \describe{
#'   \item{df.ccc}{Data frame object with four variables/columns and one row, summarizing the values of the robust \code{CCC} calculations
#'   as well as that of the Lin's one (see also \code{Details} and \code{References}). Each \code{CCC} value/column possess
#'   explanatory name (therefore it is compiled into \code{df} and not into named vector): "King's exponent \eqn{\delta} = 1",
#'   "King's exponent \eqn{\delta} = 1.5","King's exponent \eqn{\delta} = 2 (Approaches Lin's CCC)" and finally
#'   "Lin's CCC".}
#'   \item{plot.cc}{Ggplot2 object function, built to visualize the correlation of both original vectors (\code{y.vec} vs \code{x.vec}).
#'   In ideal case all data points are perfectly aligned with the \eqn{45\,\degree} identity/concordance line
#'   (with the intercept \code{0} and the slope \code{1}). In addition, a linear fit is presented for a comparison to identity line.
#'   Function controls appearence of the plot: color, size, shape, linewidth and linetype of points and the two lines, separately.
#'   See also \href{https://ggplot2.tidyverse.org/reference/geom_point.html}{point} and
#'   \href{https://ggplot2.tidyverse.org/reference/geom_path.html}{line aesthetics} of the \code{{ggplot2}} package.}
#'   \item{Cb}{Numeric value of Lin's \code{CCC} bias-correction factor (accuracy), which measures how far the linear fit line
#'   deviates from the \eqn{45\,\degree} identity line. No deviation occurs when \code{Cb = 1}. In general, \code{Cb} \eqn{\leq} 1,
#'   where \code{Cb} = Lin's \code{CCC} / \code{r}, with the \code{r} corresponding to Pearson's correlation coefficient.}
#'   \item{baplot}{If \code{check.baplot = FALSE} (default), \code{baplot = NULL}. Otherwise, original (non-robust) Bland-Altman plot
#'   is displayed, as a \code{{ggplot2}} object, and detects the presence of any systematic bias as well as
#'   possible outliers between \code{x.vec} and \code{y.vec}. It actually shows the paired \code{Difference}
#'   (with \eqn{95\,\%} confidence band - the light blue area also known as limits of agreement)
#'   \emph{vs} corresponding paired \code{Mean} value of those two vectors.}
#'   }
#'
#'
#' @examples
#' ## load experimental TMPD radical cation
#' ## EPR data/spectrum
#' data.file.path <-
#'   load_data_example(file =
#'     "TMPD_specelchem_accu_b.asc")
#' data.tmpd.expr <-
#'   readEPR_Exp_Specs(
#'     path_to_file = data.file.path,
#'     col.names = c("B_G","dIepr_over_dB"),
#'     qValue = 3500,
#'     origin = "winepr"
#'    )
#' #
#' ## simulation fit for TMPD radical cation
#' sim.fit.tmpd.spec <- eval_sim_EPR_isoFit(
#'   data.spectr.expr = data.tmpd.expr,
#'   nu.GHz = 9.814155,
#'   nuclear.system.noA = list(
#'     list("14N", 2),
#'     list("1H", 4),
#'     list("1H", 12)
#'   ),
#'   lineG.content = 0.42,
#'   baseline.correct = "constant",
#'   optim.method = "neldermead",
#'   optim.params.init = c(
#'     2.00304,
#'     0.52, 0.51,
#'     0,
#'     3.2e5,
#'     19.62, 5.49, 19.54
#'   ),
#'   Nmax.evals = 64,
#'   Blim = c(3450,3550),
#'   check.fit.plot = TRUE
#' )
#' #
#' ## EPR spectra comparison
#' sim.fit.tmpd.spec$plot
#' #
#' ## experiment + simulation fit concordance
#' concord.list <- eval_CCC_forFit(
#'   x.vec =
#'     sim.fit.tmpd.spec$ra$df$Experiment,
#'   y.vec =
#'     sim.fit.tmpd.spec$ra$df$Simulation
#' )
#' #
#' ## concordance coefficients (CC)
#' concord.list$df.ccc
#' #
#' ## plot
#' concord.list$plot.cc(point.shape = 1)
#' #
#' ## accuracy (related to Lin's CCC)
#' concord.list$Cb
#' #
#' ## Pearson correlation via Lin's CCC and Cb
#' concord.list$df[1,4] / concord.list$Cb
#' #
#' ## Pearson correlations by `cor()`
#' ## function for comparison
#' sim.fit.tmpd.spec$cor.df()
#'
#'
#' @export
#'
#' @importFrom ggplot2 geom_abline
eval_CCC_forFit <- function(x.vec,y.vec,check.baplot = FALSE) {
  #
  ## 'Temporary' processing variables
  X <- NULL
  Y <- NULL
  Mean <- NULL
  Diff <- NULL
  #
  ## setting up the number of digits, general
  ## definition for all variables =>
  options(digits = 9)
  #
  ## check both x,y vectors, having their counterpart,
  ## only `complete.cases` are evaluated
  if (length(x.vec) != length(y.vec)) {
    stop(" Vectors/Arguments `x.vec` and `y.vec` must have the same length ! ")
  }
  ok <- stats::complete.cases(x.vec, y.vec)
  x.vec <- x.vec[ok]
  y.vec <- y.vec[ok]
  #
  ## check the length of `x.vec` or `y.vec`
  if (length(x.vec) <= 4 || length(y.vec) <= 4) {
    stop(" Length of `x.vec` or `y.vec` is less than or equal to 4.\n
         It does not make sense to calculate concordance\n
         correlation coefficient (CCC) for such a small \n
         number of `x.vec`/`y.vec` pairs ! ")
  }
  #
  ## ------------- Correlation Coefficients Calculations ----------------
  #
  ## vector of King's `delta` exponent
  delta <- c(1,1.5, 2)
  #
  ## calculation of `CCC` (King, Chinchilli & Vallejos)
  ## https://doi.org/10.1002/env.70117
  ## https://doi.org/10.1081/BIP-100107651
  ## https://www.jstor.org/stable/2532051
  ## for the entire `delta` vector the last `CCC`
  ## should actually correspond to Lin's one,
  ## see below
  rho.CCC <- sapply(
    delta,
    function(d) {
      #
      ## calculate `E_I` cross-pairs
      E_I <- abs(outer(x.vec,y.vec,"-"))^d %>% mean()
      #
      ## calculate `E_F` matched pairs
      E_F <- abs(x.vec - y.vec)^d %>% mean()
      #
      ## calculate CCC
      1 - (E_F / E_I)
    }
  )
  #
  ## calculation of the Lin's CCC (also incl. Pearson)
  lin.CCC <- function(a = x.vec,b = y.vec) {
    #
    mx <- mean(a)
    my <- mean(b)
    sx2 <- mean((a - mx)^2)
    sy2 <- mean((b - my)^2)
    sxy <- mean((a - mx) * (b - my))
    #
    ## Pearson Correlation
    r <- sxy / (sqrt(sx2) * sqrt(sy2))
    #
    ## Lin's Correlation
    ccc <- 2 * sxy / (sx2 + sy2 + (mx - my)^2)
    #
    result.list <- list(
      CCC = ccc,
      Cb = ccc / r ## bias correction factor (see below)
    )
    #
    return(result.list)
    #
  }
  #
  ## generating the final data frame
  df.CCC <- data.frame(
    d1 = rho.CCC[1],
    d15 = rho.CCC[2],
    d2 = rho.CCC[3],
    lin = lin.CCC()$CCC
  )
  ## renaming the columns
  colnames(df.CCC) <- c(
    "King's exponent \u03B4 = 1",
    "King's exponent \u03B4 = 1.5",
    "King's exponent \u03B4 = 2 (Approaches Lin's CCC)",
    "Lin's CCC"
  )
  #
  ## ------------ Plotting the concordance line graph ------------------
  #
  ## Linear fit and parameters (intercept + slope) for later
  ## displaying those params. in `plot.CCC`
  df.for.plot.CCC <- data.frame(X = x.vec,Y = y.vec)
  #
  lin.fit.lm <- stats::lm(Y ~ X,data = df.for.plot.CCC)
  intercpt <- unname(stats::coef(lin.fit.lm)[1])
  slop <- unname(stats::coef(lin.fit.lm)[2])
  #
  ## concordance/identity line plot with linear fit
  plot.CCC <- function(
    point.color = "darkcyan",
    point.size = 3.2,
    point.shape = 1,
    identity.line.color = "magenta",
    linearFit.line.color = "blue",
    identity.linewidth = 2.0,
    linearFit.linewidth = 1.2,
    identity.linetype = "solid",
    linearFit.linetype = "solid"
  ) {
    cccplot <- ggplot(data = df.for.plot.CCC,aes(x = X, y = Y)) +
      geom_point(
        size = point.size,
        shape = point.shape,
        color = point.color
      ) +
      geom_abline(
        intercept = 0,
        slope = 1,
        linewidth = identity.linewidth,
        color = identity.line.color,
        linetype = identity.linetype
      ) +
      stat_smooth(
        method = "lm",
        se = TRUE,
        color = linearFit.line.color,
        linewidth = linearFit.linewidth,
        linetype = linearFit.linetype
      ) +
      labs(
        title = bquote(
          "Concordance Plot with the Ideal " * 45 * degree * " Identity Line and the Linear Fit"
        ),
        x = bquote(italic(Observation)),
        y = bquote(italic(Model~~~or~~~Fit)),
        caption = bquote(
          "Identity line: " ~ .(identity.line.color) * "," ~~
            " Linear fit: " ~ .(linearFit.line.color) * "," ~~
            " intercept = " ~ .(intercpt) * "," ~~
            " slope = " ~ .(slop)
        )
      ) + plot_theme_In_ticks()
    #
    suppressMessages(print(cccplot))
  }
  #
  ## ------------- Constructing the Bland-Altman Table and Plot -------------------
  #
  if (isTRUE(check.baplot)) {
    #
    ## rowwise mean value of all couples:
    df.for.plot.CCC[["Mean"]] <- rowMeans(df.for.plot.CCC)
    df.for.plot.CCC[["Diff"]] <-
      df.for.plot.CCC[["X"]] - df.for.plot.CCC[["Y"]] ## difference
    Mean.Diff <- mean(df.for.plot.CCC$Diff) ## ...its mean value
    #
    ## calculate 95 % confidence band
    lower.Diff <- Mean.Diff -
      (stats::qnorm(0.975) * stats::sd(df.for.plot.CCC$Diff))
    upper.Diff <- Mean.Diff +
      (stats::qnorm(0.975) * stats::sd(df.for.plot.CCC$Diff))
    #
    ## Bland-Altman Plot
    bland.alt.plot <-
      ggplot(data = df.for.plot.CCC,aes(x = Mean,y = Diff)) +
      geom_point(color = "darkblue",size = 3.2) +
      geom_hline(
        yintercept = Mean.Diff,
        color = "darkorange",
        linewidth = 1.1
      ) +
      annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = +Inf,
        ymin = lower.Diff,
        ymax = upper.Diff,
        fill = "blue",
        alpha = 0.16
      ) +
      labs(
        title = "Bland-Altman Plot (Model/Fit vs Observation)",
        x = bquote(italic(Mean)),
        y = bquote(italic(Difference)),
        caption = bquote(Mean~~Difference~~or~~bias == .(Mean.Diff))
      ) +
      plot_theme_In_ticks()
  }
  #
  ## ---------------------- RESULTS (LIST) ----------------------
  result <- list(
    df.ccc = df.CCC,
    plot.cc = plot.CCC,
    ## bias correction factor = Lin's CCC / Pearson (see above)
    ## how far the reduced major axis departs from the 45° identity line:
    Cb = lin.CCC()$Cb,
    ## Bland-Altman plot:
    baplot = switch(2 - check.baplot,bland.alt.plot,NULL)
  )
  #
  return(result)
  #
}
