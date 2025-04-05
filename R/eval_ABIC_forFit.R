#'
#' General Ranking of Models/Fits Using the AIC and BIC Metrics
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   When comparing different (simulation) fits for the same experimental data (see e.g.
#'   \code{\link{eval_sim_EPR_isoFit}}, \code{\link{eval_kinR_EPR_modelFit}}
#'   or \code{\link{smooth_EPR_Spec_by_npreg}}) ...TBC...
#'
#'
#' @details
#'   Additional details...
#'
#'
#'
#' @param data.fit Data frame object, usually containing variables/columns like \code{experiment},
#'   \code{fit(ted)}/\code{predicted} as well as \code{residuals}/\code{errors}. If the latter is missing
#'   (see the argument \code{residuals} below) one can easily create/calculate the variable/column
#'   as a difference between the experimental and fitted/predicted values.
#' @param residuals Character string, pointing to variable/column header with residuals/errors, depending
#'   on the \code{data.fit} argument (usually \code{residuals = "Residuals"} or \code{residuals = "errors"}).
#'   \strong{Default}: \code{residuals = NULL}.
#' @param k Numeric value identical to number of parameters used in your model/fit
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
#'   \item{abic}{A numeric vector containing the values of estimated AIC and BIC, respectively.}
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
    stop(' Please, provide the name for proposed distribution\n
         of errors/residuals, like "normal" or "gaussian" or "automatic",\n
         "t-distribution"...etc, refer to the `rs.prob.distro` argument !! ')
  }
  #
  ## check column of `data.fit` like "residuals":
  if (is.null(residuals)) {
    stop(' Does your data frame already contain a column header,\n
         pointing to "residuals" ? If yes, please, provide it,\n
         see the argument `residuals`. If it is not the case, \n
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
    ## ...also taking into account the correction for small `N.obs`
    AIC <- (-2 * logLik) + (2 * k) +
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
           Please, specify the `rs.prob.distro` argument. No "automatic"\n
           can be used !! ')
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
        abic = c(
          unname(a.ic.compar.vec[a.ic.min.idx]),
          unname(b.ic.compar.vec[b.ic.min.idx])
        ),
        message = paste0(root.msg,abic_name_msg_fun(name = a.ic.min.name))
      )
  } else {
    result.list <-
      list(
        abic = switch(
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
