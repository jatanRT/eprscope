#'
#' Smoothing and Fitting of an EPR Spectrum by Splines
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   Smoothing of the EPR spectra by non-parametric fitting a smoothing spline, \code{\link[npreg]{ss}}
#'   from the \href{https://cran.r-project.org/web/packages/npreg/npreg.pdf}{npreg} package, onto the experimental
#'   EPR spectrum/spectra.
#'
#'
#' @details
#'   The EPR spectrum is fitted by splines which, by default, correspond to cubic Bernoulli polynomials
#'   like
#'   \deqn{I_{\text{EPR}}(B) = B^3 - (3/2)\,B^2 + (1/2)\,B}
#'   where \eqn{I_{\text{EPR}}} equals to general EPR intensity and \eqn{B} is the magnetic flux density.
#'   This may be applied to both derivative or single integrated EPR spectrum forms. Also a higher/lower polynomial
#'   degree may be applied by the \code{method} (see the argument description) and by the \code{m} optional argument
#'   (see \code{...} additional arguments) which is a penalty order (integer). For the above-mentioned cubic
#'   spline/polynomial \code{m = 2}. Linear polynomial corresponds
#'   to \code{m = 1} and the highest quintic polynomial/spline is referred as \code{m = 3}:
#'   \deqn{I_{\text{EPR}}(B) = B^5 - (5/2)\,B^4 + (5/3)\,B^3 - (1/6)\,B}
#'   If the optional argument \code{bernoulli} is set to \code{FALSE} then "classic" definition
#'   of a smoothing spline is produced. In such case, the function estimate is a piece-wise polynomial function
#'   with pieces of degree 2m−1. Additional optional arguments from \code{\link[npreg]{ss}} like knots
#'   definition, equivalent degrees of freedom, vector of weights...etc. can be applied as well.
#'
#'
#' @references
#'   Berry LN, Helwig NE (2021). “Cross-Validation, Information Theory, or Maximum Likelihood?
#'   A Comparison of Tuning Methods for Penalized Splines.” \emph{Stats}, \strong{4}(3), 701–724,
#'   \url{https://doi.org/10.3390/stats4030042}.
#'
#'   Elezović N (2016). “Generalized Bernoulli Polynomials and Numbers, Revisited.”
#'   \emph{Mediterr. J. Math.}, \strong{13}(1), 141–151. ISSN 1660-5454,
#'   \url{https://doi.org/10.1007/s00009-014-0498-7}.
#'
#'   Weisstein EW (2023). “Bernoulli Polynomial”,
#'   \url{https://mathworld.wolfram.com/BernoulliPolynomial.html}, MathWorld–A Wolfram Web Resource.
#'
#'   Helwig NE (2022). "Non-parametric Regression via Smoothing Splines",
#'   R package version 1.0-9, \url{https://cran.r-project.org/web/packages/npreg/npreg.pdf}.
#'
#'
#' @inheritParams eval_gFactor_Spec
#' @param method Character string, corresponding to method in order to fit an EPR spectrum by a smoothing spline
#'   from \code{{npreg}} package (see the \code{method} argument in \code{\link[npreg]{ss}}).
#'   The following methods are available (they are shortly described
#'   in \href{http://users.stat.umn.edu/~helwig/notes/smooth-spline-notes.html}{Nathaniel's E. Helwig Introduction}
#'   or additional information may be found in \code{References}) =>
#'   \tabular{ll}{
#'   \strong{Method} \tab \strong{Short Description} \cr
#'   "GCV" \tab Generalized Cross-Validation \cr
#'   "OCV" \tab Ordinary Cross-Validation \cr
#'   "GACV" \tab Generalized Approximate Cross-Validation \cr
#'   "ACV" \tab Approximate Cross-Validation \cr
#'   "REML" \tab Restricted Maximum Likelihood \cr
#'   "ML" \tab Maximum Likelihood \cr
#'   "AIC" \tab Akaike’s Information Criterion \cr
#'   "BIC" \tab Bayesian Information Criterion (\strong{default}) \cr
#'   }
#' @param output.vec Logical, whether the function output have to be vectorized, i.e. only the vector
#'   of smoothed EPR intensity is provided. This is especially useful for the EPR spectral (time) series,
#'   which can be handily processed by the \code{\link[dplyr]{group_by}} using the
#'   \code{pipe} operators (\code{\link[magrittr]{\%>\%}}). \strong{Default}: \code{output.vec = FALSE}.
#' @param ... additional arguments passed to the function (see also the \code{\link[npreg]{ss}}).
#'
#'
#' @return If \code{output.vec = TRUE} the output corresponds to vector of the smoothed EPR intensity
#'   (either derivative \code{lineSpecs.form = "derivative"} or integrated
#'   \code{lineSpecs.form = "integrated"/"absorption"}) with the length of the original \code{Intensity}.
#'   Contrarily, if the \code{output.vec} is set to \code{FALSE} the following list is returned =>
#'   \describe{
#'   \item{df}{Original data frame with the addition column, corresponding to smoothed EPR intensity
#'   (derivative or integrated).}
#'   \item{plot}{Plot object (list) \emph{EPR intensity} \emph{vs.} \emph{B} with the experimental
#'   data and its corresponding smoothed relation performed by splines.}
#'   \item{rss}{Weighted (if the optional parameter \code{w} is defined) sum of residual squares.}
#'   \item{degs.freedom}{Equivalent degrees of freedom used.}
#'   \item{fit}{List with elements to characterize the spline fit (Details see \code{fit} value
#'   in the \code{\link[npreg]{ss}} function documentation).}
#'   \item{ra.sd}{Standard deviation of residuals.}
#'   \item{abic}{Numeric vector/value of Akaike’s Information Criterion (if the \code{method = "AIC"})
#'   or Bayesian Information Criterion (if the \code{method = "BIC"}). These are negative numbers,
#'   having the largest modulus (deepest down in the negative territory) and therefore, indicating
#'   the preferred model (the lower, the better, see also \code{\link{eval_ABIC_forFit}}.}
#'   \item{log.lik}{Likelihood logarithm, if the \code{method = "REML"/"ML"}. Log likelihood value
#'   is a measure of goodness of fit for any model. The higher the value, the better the model.}
#'   }
#'
#'
#' @examples
#' ## loading the built-in package example
#' ## time series EPR spectra:
#' triarylamine.decay.series.dsc.path <-
#' load_data_example(file =
#'         "Triarylamine_radCat_decay_series.DSC")
#' triarylamine.decay.series.asc.path <-
#' load_data_example(file =
#'         "Triarylamine_radCat_decay_series.zip")
#' unzip(triarylamine.decay.series.asc.path,
#'       exdir = tempdir()
#'       )
#' ## loading the kinetics:
#' triarylamine.decay.series.data <-
#'   readEPR_Exp_Specs_kin(name.root =
#'     "Triarylamine_radCat_decay_series",
#'     dir_ASC = tempdir(),
#'     dir_dsc_par =
#'       system.file("extdata",
#'                   package = "eprscope")
#'    )
#' #
#' ## select the first spectrum
#' triarylamine.decay.series.data1st <-
#'    triarylamine.decay.series.data$df %>%
#'      dplyr::filter(time_s ==
#'        triarylamine.decay.series.data$time[1])
#' #
#' ## smoothing the 1st EPR spectrum with default
#' ## arguments/parameters
#' triarylamine.1st.spec.smooth <-
#'   smooth_EPR_Spec_by_npreg(data.spectr =
#'     triarylamine.decay.series.data1st
#'     )
#' #
#' ## plot preview
#' triarylamine.1st.spec.smooth$plot
#' #
#' ## sum of residual squares preview
#' triarylamine.1st.spec.smooth$rss
#' #
#' ## estimated standard deviation
#' triarylamine.1st.spec.smooth$ra.sd
#' #
#' ## Bayesian information criterion (BIC)
#' triarylamine.1st.spec.smooth$abic
#' #
#' ## smoothing of all EPR spectra in the series
#' ## with default arguments using the data
#' ## "pipe" ("%>%") `dplyr` processing
#' triarylamine.all.spec.smooth <-
#'   triarylamine.decay.series.data$df %>%
#'     dplyr::group_by(time_s) %>%
#'     dplyr::mutate(smoothed =
#'       smooth_EPR_Spec_by_npreg(
#'         dplyr::pick(B_mT,dIepr_over_dB),
#'         output.vec = TRUE
#'      )
#'   )
#' #
#' ## data frame preview
#' head(triarylamine.all.spec.smooth)
#' #
#' ## plot all smoothed spectra in the series
#' plot_EPR_Specs(data.spectra =
#'           triarylamine.all.spec.smooth,
#'   Intensity = "smoothed",
#'   var2nd.series = "time_s",
#'   var2nd.series.slct.by = 10,
#'   line.colors = c("darkorange",
#'                   "darkblue"),
#'   legend.title = "Time (s)",
#'   yTicks = TRUE
#'   )
#'
#'
#' @export
#'
#' @importFrom npreg ss
smooth_EPR_Spec_by_npreg <- function(data.spectr,
                                     B = "B_mT",
                                     B.unit = "mT",
                                     lineSpecs.form = "derivative",
                                     Intensity = "dIepr_over_dB",
                                     method = "BIC",
                                     output.vec = FALSE,
                                     ...){
  #
  ## 'Temporary' processing variables
  y <- NULL
  smoothed <- NULL
  #
  ## smooth EPR spectrum data
  smooth.epr.spec.list <-
    ss(x = data.spectr[[B]],
              y = data.spectr[[Intensity]],
              method = method,
              ...)
  #
  ## new column with smoothed Intensity
  data.spectr[["smoothed"]] <- smooth.epr.spec.list$y
  #
  ## derivative/integrated spectrum condition
  deriv.form.cond <- grepl("deriv|Deriv",lineSpecs.form)
  #
  ## plot both EPR spectra
  plot.expr.smoothed <- ggplot(data.spectr) +
    geom_point(
      aes(
        x = .data[[B]],
        y = .data[[Intensity]],
        color = "Experimental\nData"
      ),
      size = 2.6
    ) +
    geom_line(
      aes(
        x = .data[[B]],
        y = .data$smoothed,
        color = "\nSmoothed"
      ),
      linewidth = 1.1
    ) +
    scale_color_manual(
      values = c("darkcyan", "magenta"),
      breaks = c("Experimental\nData", "\nSmoothed"),
      guide = guide_legend(override.aes = list(
        shape = c(16, NA),
        linetype = c("blank", "solid")
      ))
    ) +
    labs(color = NULL,
         x = bquote(italic(B)~~"("~.(B.unit)~")"),
         y = switch(2-deriv.form.cond,
                    bquote(d*italic(I)[EPR]~~"/"~~d*italic(B)~~~"("~p.d.u.~")"),
                    bquote(italic(Intensity) ~ ~"(" ~ p.d.u. ~ ")"))
         ) +
    plot_theme_In_ticks() +
    theme(legend.text = element_text(size = 13),
          legend.text.align = 0.5)
  #
  ## RESULTS
  if (isFALSE(output.vec)){
    results <- list(
      df = data.spectr,
      plot = plot.expr.smoothed,
      rss = smooth.epr.spec.list$pen.crit, ## (weighted) sum of residual squares
      degs.freedom = smooth.epr.spec.list$df, ## corresponding degrees of freedom
      fit = smooth.epr.spec.list$fit, ## list with fit characteristics
      ra.sd = smooth.epr.spec.list$sigma, ## estimated residual/error standard deviation.
      ## see https://bradleyboehmke.github.io/HOML/linear-regression.html
      ## Akaike's Information Criterion (if method is AIC)
      ## Bayesian Information Criterion (if method is BIC)
      abic = c(smooth.epr.spec.list$aic,smooth.epr.spec.list$bic),
      log.lik = smooth.epr.spec.list$logLik ## log-likelihood (if method is REML or ML)
    )
  } else {
    results <- data.spectr$smoothed
  }
  #
  return(results)
  #
}
