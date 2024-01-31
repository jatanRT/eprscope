#'
#' Smoothing and Fitting of an EPR Spectrum by Splines
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   Smoothing of the EPR spectra by non-parametric fitting a smoothing spline \code{\link[npreg]{ss}}
#'   from \pkg{npreg} package.
#'
#'
#' @details
#'   The EPR spectrum is fitted by splines which, by default, correspond to cubic Bernoulli polynomial,
#'   like e.g.
#'   \deqn{I_{\text{EPR}}(B) = B^3 - (3/2)\,B^2 + (1/2)\,B}
#'   where \eqn{I_{\text{EPR}}} equals to general EPR intensity and \eqn{B} is the magnetic flux density.
#'   This may be applied to both derivative or single integrated EPR spectral forms. Also a higher/lower polynomial
#'   degree may be applied by the \code{method} (see the argument description) and by the \code{m} optional argument
#'   (see \code{...} additional arguments) which is a penalty order (integer). For the above-mentioned cubic
#'   spline/polynomial \code{m = 2}. Linear polynomial corresponds
#'   to \code{m = 1} and the highest quintic polynomial/spline is referred as \code{m = 3}:
#'   \deqn{I_{\text{EPR}}(B) = B^5 - (5/2)\,B^4 + (5/3)\,B^3 - (1/6)\,B}
#'   If the optional argument \code{bernoulli} is set to \code{FALSE} then "classic" definition
#'   of a smoothing spline, where the function estimate is a piece-wise polynomial function with pieces
#'   of degree 2m−1, is produced. Additional optional arguments from \code{\link[npreg]{ss}} like e.g. knots
#'   definition, equivalent degrees of freedom, vector of weights...etc. can be applied as well.
#'
#'
#' @references
#'   \insertRef{BerryCrossValid2021}{eprscope}
#'
#'   \insertRef{ElezovicGeneral2016}{eprscope}
#'
#'   \insertRef{WeissteinBernPol2023}{eprscope}
#'
#'   \insertRef{HelwigMan2022}{eprscope}
#'
#'
#' @inheritParams eval_gFactor_Spec
#' @param method Character string corresponding to method to fit a smoothing spline provided by \pkg{npreg}
#'   package (see the \code{method} argument in \code{\link[npreg]{ss}}). The following methods are available
#'   (they are shortly described
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
#'   "BIC" \tab Bayesian Information Criterion \cr
#'   }
#' @param output.vec Logical, whether the function output have to be vectorized, i.e. only the vector
#'   of smoothed EPR intensity is provided. This is especially useful for the spectral (time) series EPR data,
#'   which can be handily processed by \code{\link[dplyr]{group_by}} using the
#'   `pipe` operators (\code{\link[magrittr]{\%>\%}}). \strong{Default}: \code{output.vec = NULL}.
#' @param ... additional arguments passed to the function (see also \code{\link[npreg]{ss}} function).
#'
#'
#' @return If \code{output.vec = TRUE} the output corresponds to vector of the smoothed EPR intensity
#'   (either dereivative \code{lineSpecs.form = "derivative"} or integrated
#'   \code{lineSpecs.form = "integrated"/"absorption"}) with the length of the original \code{Intensity}.
#'   Contrarily if the \code{output.vec} is set to \code{FALSE} the following list is returned =>
#'   \describe{
#'   \item{df}{Original data frame with the addition column corresponding to smoothed EPR intensity
#'   (derivative or integrated).}
#'   \item{plot}{Plot/Graph object \emph{EPR intensity} \emph{vs.} \emph{B} with the experimental
#'   data and its corresponding smoothed relationship performed by splines.}
#'   \item{rss}{Weighted (if the optional parameter \code{w} is defined) residual sum of squares.}
#'   \item{degs.freedom}{Equivalent degrees of freedom used.}
#'   \item{fit}{List with components to characterize the spline fit (Details see \code{fit} value
#'   in \code{\link[npreg]{ss}} function documentation).}
#'   \item{sigma}{Estimated error standard deviation.}
#'   \item{aic}{Akaike’s Information Criterion (if \code{method = "AIC"}). A negative number that has
#'   the largest modulus (deepest down in the negative territory) indicates the preferred model.}
#'   \item{bic}{Bayesian Information Criterion (if \code{method = "BIC"}). A negative number that has
#'   the largest modulus (deepest down in the negative territory) indicates the preferred model.}
#'   \item{log.likehood}{Likelihood logarithm if \code{method = "REML"/"ML"}. Log likelihood value
#'   is a measure of goodness of fit for any model. The higher the value, the better the model.}
#'   }
#'
#'
#' @examples
#' \dontrun{
#' TODO
#' TODO
#' }
#'
#'
#' @export
#'
#'
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
    npreg::ss(x = data.spectr[[B]],
              y = data.spectr[[Intensity]],
              method = method,
              ...)
  #
  ## new column with smoothed Intensity
  data.spectr[["smoothed"]] <- smooth.epr.spec.list$y
  #
  ## derivative/integrated spectrum condition
  deriv.form.cond <- ifelse(lineSpecs.form == "derivative",TRUE,FALSE)
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
    scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    theme(legend.text = element_text(size = 13),
          legend.text.align = 0.5)
  #
  ## RESULTS
  if (isFALSE(output.vec)){
    results <- list(
      df = data.spectr,
      plot = plot.expr.smoothed,
      rss = smooth.epr.spec.list$pen.crit, ## (weighted) residual sum of squares
      degs.freedom = smooth.epr.spec.list$df, ## corresponding degrees of freedom
      fit = smooth.epr.spec.list$fit, ## list with fit characteristics
      sigma = smooth.epr.spec.list$sigma, ## estimated error standard deviation.
      aic = smooth.epr.spec.list$aic, ## Akaike's Information Criterion (if method is AIC)
      bic = smooth.epr.spec.list$bic, ## Bayesian Information Criterion (if method is BIC)
      log.likehood = smooth.epr.spec.list$logLik ## log-likelihood (if method is REML or ML)
    )
  } else{
    results <- data.spectr$smoothed
  }
  #
  return(results)
  #
}
