#'
#' General Ranking of Models/Fits Using the AIC and BIC Metrics
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   When comparing different (simulation) fits for the same experimental data (see
#'   \code{\link{eval_sim_EPR_isoFit}}, \code{\link{eval_kinR_EPR_modelFit}}, \code{\link{eval_kinR_Eyring_GHS}}
#'   or \code{\link{smooth_EPR_Spec_by_npreg}}), the fits can be scored/ranked by different
#'   metrics (e.g. minimum sum of residual squares or standard deviation of residuals), including
#'   Akaike and Bayesian Information Criteria (\code{\link[stats]{AIC}} and BIC, respectively).
#'   These are also applied for the best model selection in machine learning (refer to e.g.
#'   \href{https://www.modernstatisticswithr.com/mlchapter.html}{Predictive Modelling and Machine Learning} or
#'   \href{https://www.louisaslett.com/StatML/notes/error-estimation-and-model-selection.html#ref-yang05}{Error Estimation and Model Selection}).
#'   As described in details, both metrics depends on maximum logarithmic likelihood (based on residuals calculation)
#'   to the same data. \strong{The smaller the (negative) AIC or BIC, the better the model/fit.}
#'
#'
#' @details
#'   Estimation of model errors, that model/fit makes in respect to our (experimental) data, becomes
#'   one of the most consequential aspects of a statistical (machine learning) analysis. Often, different
#'   modelling/fitting approaches are used, with the attempt to identify or select the best model/fit. Therefore,
#'   for such purpose, one tries to minimize the errors/residuals more and more with each model. Or to put it another way,
#'   \strong{there is an information loss when the model/fit approximates the reality} and a good model minimizes
#'   those losses. The evaluation of AIC and BIC actually approaches the problem from the other site,
#'   because it uses the technique called \strong{maximum likelihood estimate (MLE)}. The idea is to maximize the chance
#'   that each observation in the sample follows a pre-selected distribution with specific
#'   set of parameters (corresponding to a model/fit). For practical reasons a logarithmic likelihood
#'   (or log-likelihood,\eqn{LL}) is used, and the formulae for both criteria read:
#'   \deqn{AIC = -2\,LL + 2\,k + (2\,k\,(k + 1)\,/\,(N - k -1))}
#'   and
#'   \deqn{BIC = -2\,LL + k\,ln(N)}
#'   where \eqn{k} and \eqn{N} correspond to number of (model/fit) parameters and number of observations, respectively.
#'   The 3rd term in the \eqn{AIC} definition represents the correction for small sample/observation ensemble, which
#'   for high number of observations becomes very small (and can be neglected,
#'   see e.g. Burnham and Anderson (2004) or Kumar (2023) in the \code{References}). For example, for EPR simulation
#'   fit with 2048 points and 8 parameters it equals to \eqn{16 \cdot 9\,/\,2039 \approx 0.0706}. However, for
#'   radical kinetic measurements with 42 EPR spectra and 3 parameters, the 3rd term results
#'   in \eqn{6 \cdot 4\,/\,38 \approx 0.6316}.
#'
#'   \strong{The original MLE/\eqn{LL} calculation is based on the model. Nevertheless, such computation can be quite often
#'   impractical or even impossible to perform.} To overcome this difficulty, the formulae for both criteria
#'   use a \strong{standard assumption that the model and the data residuals/errors are identically distributed.}
#'   Therefore, \strong{the residuals/errors are applied as a proxy for the MLE/\eqn{LL}} (see e.g. Rossi et al. (2020)
#'   and Kumar (2023) in the \code{References}). Evaluation of the latter, in the actual function, proceeds through \code{sum}
#'   of the \code{\link[stats:Normal]{stats::dnorm}} (for the normal/Gaussian distribution)
#'   and of the \code{\link[stats:TDist]{stats::dt}} (for the Student's t-distribution), using the \code{log = TRUE}
#'   option. For t-distribution the \code{df}/\eqn{\nu} parameter is unknown, therefore it is optimized
#'   by the above-described \eqn{LL} as well as by the \code{\link[stats]{optimize}} function. Both probability distributions
#'   are included in the function because not always the residuals/errors follow the normal one. Sometimes, heavier tails
#'   may appear, e.g. for EPR simulation fits (please, refer to the \code{Examples} in the \code{\link{eval_sim_EPR_isoFit}}).
#'   Consequently, the function may automatically (see the argument \code{rs.prob.distro}) decide which distribution
#'   fits the residuals/errors the best, based on the lower AIC, BIC values.
#'   \strong{It is recommended to evaluate/apply both information criteria}. The AIC tends to favor a more complex model
#'   (over a simpler one) and thus suggests to "overfit" the data, whereas the BIC is in favor of simpler models because
#'   it possesses a stronger penalty (\eqn{k\,ln(N)}) for complex models than AIC (\eqn{2\,k},see e.g. Fabozzi et al. (2014)
#'   and Zhang Y, Meng G (2023) in the \code{References}).
#'
#'
#' @references
#'   Fabozzi FJ, Focardi FM, Rachev ST, Arshanapalli BG (2014). \emph{The Basics of Financial Econometrics:
#'   Tools, Concepts, and Asset Management Applications (Appendix E)}, John Wiley and Sons, Inc. ISBN 978-1-118-57320-4,
#'   \url{https://onlinelibrary.wiley.com/doi/book/10.1002/9781118856406}.
#'
#'   Soch J et al. (2024). StatProofBook/StatProofBook.github.io: \emph{The Book of Statistical Proofs (Version 2023).},
#'   \url{https://statproofbook.github.io/}, \url{https://doi.org/10.5281/ZENODO.4305949}.
#'
#'   Burnham KP, Anderson DR (2004). "Multimodel Interference: Understanding AIC and BIC in Model Selection",
#'   \emph{Sociol. Methods  Res.}, \strong{33}(2), 261-304, \url{https://doi.org/10.1177/0049124104268644}.
#'
#'   Thulin M (2025). \emph{Modern Statistics with R: From Wrangling and Exploring Data to Inference
#'   and Predictive Modeling}, 2nd edition (Version 2.0.2), CRC Press and Taylor and Francis Group, LLC.
#'   ISBN 978-1-032-51244-0, \url{https://www.modernstatisticswithr.com/}.
#'
#'   Zhang Y, Meng G (2023). "Simulation of an Adaptive Model Based on AIC and BIC ARIMA Predictions",
#'   \emph{J. Phys.: Conf. Ser.}, \strong{2449}, 012027-7, \url{https://doi.org/10.1088/1742-6596/2449/1/012027}.
#'
#'   Svetunkov I (2022). \emph{Statistics for Business Analytics}, Version 2025,
#'   \url{https://openforecast.org/sba/}.
#'
#'   Rossi R, Murari R, Gaudio P, Gelfusa M (2020). "Upgrading Model Selection Criteria with Goodness
#'   of Fit Tests for Practical Applications", \emph{Entropy}, \strong{22}(4), 447-13,
#'   \url{https://doi.org/10.3390/e22040447}.
#'
#'   Hyndman RJ (2013). "Facts and Fallacies of the AIC", \url{https://robjhyndman.com/hyndsight/aic/}.
#'
#'   Kumar A (2023). "AIC and BIC for Selecting Regression Models: Formula, Examples",
#'   \url{https://vitalflux.com/aic-vs-bic-for-regression-models-formula-examples/#comments}.
#'
#'
#' @param data.fit Data frame object, usually containing variables/columns like \code{experiment},
#'   \code{fit(ted)}/\code{predicted} as well as \code{residuals}/\code{errors}. If the latter is missing
#'   (see the argument \code{residuals} below) one can easily create/calculate the variable/column
#'   as a difference between the experimental and fitted/predicted values.
#' @param residuals Character string, pointing to variable/column header with residuals/errors, depending
#'   on the \code{data.fit} argument (usually \code{residuals = "Residuals"} or \code{residuals = "errors"}).
#'   \strong{Default}: \code{residuals = NULL}.
#' @param k Numeric value identical to number of parameters used for the model/fit
#'   (see e.g. \code{Examples} in the \code{\link{eval_kinR_EPR_modelFit}} where \code{k = 2}).
#' @param rs.prob.distro Character string, corresponding to proposed residuals/errors probability distribution.
#'   If set to \strong{default} (\code{rs.prob.distro = "auto"}), it automatically decides which distribution
#'   (Normal/Gaussian or Student's t-distribution) fits the best to residuals/errors based on the implemented
#'   AIC and BIC calculations. This is particularly suitable for the situation when residual analysis detects
#'   heavier tails (see e.g. \code{Example} in \code{\link{eval_sim_EPR_isoFit}}) and one is not quite
#'   sure of the corresponding probability distribution. Otherwise, the argument may also specify individual
#'   distributions like: \code{rs.prob.distro = "normal"}, \code{"Gaussian"}, \code{"Student"} or
#'   \code{"t-distribution"} (\code{"t-distro"}).
#'
#'
#' @returns Function returns a list with the following components:
#'   \describe{
#'   \item{abic.vec}{A numeric vector containing the values of estimated AIC and BIC, respectively.}
#'   \item{message}{Sentence (Message), describing the residuals/errors probability distribution,
#'         that has been proposed for the AIC and BIC calculation (see also the \code{rs.prob.distro}
#'         argument).}
#'   }
#'
#'
#' @examples
#' \dontrun{
#' ## to decide which probability distribution fits
#' ## the best to residuals/errors
#' calc.abic.list.01 <-
#'   eval_ABIC_forFit(
#'     data.fit = triaryl_model_kin_fit_01$df,
#'     residuals = "residuals",
#'     k = 2,
#'     rs.prob.distro = "auto"
#'  )
#' #
#' ## AIC and BIC values
#' calc.abic.list.01$abic
#' #
#' ## ...and the corresponding message
#' calc.abic.list.01$message
#' #
#' ## calculation of AIC and BIC, taking into
#' ## account the Student's t-distribution:
#' calc.abic.list.01 <-
#'   eval_ABIC_forFit(
#'     data.fit = best.sim.fit.df,
#'     residuals = "Errors",
#'     k = 8,
#'     rs.prob.distro = "t-distro"
#'   )
#' #
#' ## for additional applications please,
#' ## refer to the Examples in `eval_sim_EPR_isoFit()`
#' ## or `eval_kinR_EPR_modelFit()`
#' #
#' }
#'
#'
#' @export
#'
#'
eval_ABIC_forFit <- function(data.fit, # data frame with at least predicted and experimental values
                             residuals = NULL, # string for column name
                             k, # number of params for the model/fit
                             rs.prob.distro = "auto") { ## or "t-","(S)student's",
                             ## or "normal", or "automatic", "gauss"
  #
  ## 'Temporary' processing variables
  #
  #
  ## if `rs.prob.distro` defined by letter case - upper
  ## convert it automatically into lower:
  if (grepl("^[[:upper:]]+",rs.prob.distro)) {
    rs.prob.distro <- tolower(rs.prob.distro)
  }
  #
  ## vector string to check `rs.prob.distro`
  rs.prob.distro.string.vec <- c("norm","t-dist","student","gauss","auto")
  #
  ## check the `rs.prob.distro` type:
  if (!any(grepl(paste(rs.prob.distro.string.vec,collapse = "|"),rs.prob.distro))) {
    stop(' Please, provide the name for the proposed distribution\n
         of errors/residuals, like "normal" or "gaussian" or "automatic",\n
         "t-distribution"...etc, refer to the `rs.prob.distro` argument !! ')
  }
  #
  ## check column of `data.fit` like "residuals":
  if (is.null(residuals)) {
    stop(' Does your data frame already contain a column header,\n
         pointing to "residuals" ? If yes, please, provide it !\n
         Refer to the argument `residuals`. If it is not the case, \n
         calculate the column of residuals within the `data.fit` !! ')
  } else {
    ## residual vector
    resids <- data.fit[[residuals]]
    #
    ## number of observations
    Nobs <- length(resids)
  }
  #
  ## ======================= Calculation of the log likelihood =======================
  #
  ## --------------------------- Gaussian/Normal (`dnorm()`) ------------------------
  log_likehood_norm <-
    sum(
      stats::dnorm(
        resids,
        mean = mean(resids),
        sd = (sd(resids) * sqrt((Nobs - 1)/Nobs)),
        ## because the MaxLikelihood uses `/n` and sd `/n-1`
        log = TRUE
      )
    )
  #
  ## ------------------------- t-Student's distro (`dt()`) ------------------------
  #
  ## ...as a function in order to be more robust (optimize parameter(s),
  ## because they are (it is) unknown)
  log_likehood_t_fun <- function(nu) { ## only nu = df (degrees of freedom) optimization
    scale <- sqrt(sum(resids^2) / Nobs) ## estimation of scale
    #
    return(sum(stats::dt(resids/scale, df = nu, log = TRUE) - log(scale)))
  }
  #
  ## now optimize the previous function in order to get `nu`
  ## and max. likelihood
  opt_logLik_t <-
    stats::optimize(
      log_likehood_t_fun,
      interval = c(1, 100),
      maximum = TRUE
    )
  #
  ## final logLik for t-Distro:
  log_likehood_t <- opt_logLik_t$objective
  #
  ## compare likelihoods, if the same => set automatically to "normal"
  ## t-distrobution reaches normal for N >= 30
  if (log_likehood_norm == log_likehood_t) {
    rs.prob.distro <- "normal"
  }
  #
  ## =========================== CALCULATION OF AIC and BIC ==============================
  #
  abic_fun <- function(N.params = k,N.obs = Nobs,logLik) {
    #
    ## ...also taking into account the correction for small `N.obs` (AIC),
    ## where this third term can be neglected for higher `N.obs` (is very small)
    AIC <- (-2 * logLik) + (2 * N.params) +
      ((2 * N.params * (N.params + 1)) / (N.obs - N.params - 1))
    #
    BIC <- (-2 * logLik) + (N.params * log(N.obs))
    #
    return(c(AIC,BIC))
  }
  #
  ## -------------------------- Conditions for calculations -------------------------------
  #
  ## strings for normal/Gaussian
  norm.string.vec <- c("norm","gauss")
  #
  ## strings for t-Distro (Student)
  t.string.vec <- c("t-dist","student")
  #
  if (any(grepl(paste(norm.string.vec,collapse = "|"),rs.prob.distro)) ||
      grepl("auto",rs.prob.distro)) {
    #
    norm.abic.vec <- stats::setNames(
      abic_fun(
        logLik = log_likehood_norm
      ), c("normaic,normbic")
    )
    #
  }
  if (any(grepl(paste(t.string.vec,collapse = "|"),rs.prob.distro)) ||
      grepl("auto",rs.prob.distro)) {
    #
    t.abic.vec <- stats::setNames(
      abic_fun(
        logLik = log_likehood_t
      ), c("taic","tbic")
    )
    #
  }
  #
  ## ------------------------ AIC/BIC Vectors for Comparison -------------------------------
  #
  if (grepl("auto",rs.prob.distro)) {
    #
    ## AIC vector for comparison
    a.ic.compar.vec <- stats::setNames(
      c(unname(norm.abic.vec[1]),
        unname(t.abic.vec[1])
      ),c("normaic","taic")
    )
    #
    ## BIC vector for comparison
    b.ic.compar.vec <- stats::setNames(
      c(unname(norm.abic.vec[2]),
        unname(t.abic.vec[2])
      ),c("normbic","tbic")
    )
    #
    ## find the index with the lowest AIC and BIC
    a.ic.min.idx <- which.min(unname(a.ic.compar.vec))
    b.ic.min.idx <- which.min(unname(b.ic.compar.vec))
    #
    ## check if names comes from the same series => test
    if (a.ic.min.idx != b.ic.min.idx) {
      stop('The indices for both AIC and BIC minimal values\n
           do not correspond. No decision can be made !!\n
           Please, specify the `rs.prob.distro` argument accordingly.\n
           No "automatic" can be used !! ')
    }
    #
    ## take the name of that element
    a.ic.min.name <- names(a.ic.compar.vec)[a.ic.min.idx]
    b.ic.min.name <- names(b.ic.compar.vec)[b.ic.min.idx]
    #
  }
  #
  ## function/conditions for relevant strings which will be print out
  abic_name_msg_fun <- function(name) {
    #
    if (grepl("norm",name)) {
      return("normal distribution.")
    } else if (grepl("t",name)) {
      return("Student's t-distribution.")
    }
  }
  #
  ## "root" message
  root.msg <- "Residuals/Errors of your Fit follow the "
  #
  ## function to switch between different options of `rs.prob.distro`
  distro_results_switch <- function(distro) {
    if (grepl("norm",distro) || grepl("gauss",distro)) {
      return(1)
    } else if (grepl("t-dist",distro) || grepl("student",distro)) {
      return(0)
    }
  }
  #
  ## ======================== RESULTS =============================
  #
  if (grepl("auto",rs.prob.distro)) {
    result.list <-
      list(
        abic.vec = c(
          unname(a.ic.compar.vec[a.ic.min.idx]),
          unname(b.ic.compar.vec[b.ic.min.idx])
        ),
        message = paste0(root.msg,abic_name_msg_fun(name = a.ic.min.name))
      )
  } else {
    result.list <-
      list(
        abic.vec = switch(
          2 - distro_results_switch(distro = rs.prob.distro),
          unname(norm.abic.vec),
          unname(t.abic.vec)
        ),
        message = paste0(root.msg,abic_name_msg_fun(name = rs.prob.distro))
      )
  }
  #
  return(result.list)
  #
}
