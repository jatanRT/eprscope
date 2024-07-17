#
#' Confidence Interval of a Vector/Data Frame-Column
#'
#'
#' @family Evaluations
#'
#'
#' @description Evaluation of the mean value and its confidence limits (according to Student's t-Distribution)
#'  corresponding to column (within data frame) or vector characterizing dispersion of the individual
#'  value, e.g. like double integral in quantitative EPR analysis, g-value or linewidth.
#'
#'
#' @details
#'   The confidence interval evaluation suggests that values/observations obey two-tailed Student's t-distribution,
#'   which for number of observations \eqn{> 30} approaches the normal \eqn{z}-distribution (see also listed References).
#'
#'
#'
#'
#' @references
#'   \insertRef{miller2018chmem}{eprscope}
#'
#'   \insertRef{finnstats2021}{eprscope}
#'
#'   \insertRef{nist2012confid}{eprscope}
#'
#'   \insertRef{kaleta2012carb}{eprscope}
#'
#'   \insertRef{psy2020confid}{eprscope}
#'
#'
#'
#' @param data.vec.col Numeric vector pointing to column of interest (within a data frame)
#'   to calculate the confidence interval or uncertainty.
#' @param level.cnfd Numeric (floating) value corresponding to confidence level \strong{default}:
#'   \code{level.cnfd = 0.95}.
#' @param lw.tail Logical, indicating the way how to calculate \code{qt} quantile function
#'   for the two-tailed \eqn{t}-distribution. It is inherited from \code{\link[stats:TDist]{stats::qt}}.
#'   If \code{lw.tail = TRUE} (\strong{default}), probabilities are \eqn{P[X\leq x]} and the vector of probabilities (\code{p})
#'   is equal to \code{level.cnfd} + \eqn{\alpha / 2}, where \eqn{\alpha} stands for the significance level = 1 - \code{level.cnfd}.
#'   Otherwise, \eqn{P[X>x]} (\code{lw.tail = FALSE}) and the \code{p} is defined as \eqn{\alpha / 2} and equal to
#'   (1 - \code{level.cnfd})/2.
#' @param separate Logical, whether to separate the mean value and the uncertainty, corresponding to non-negative right/left
#'   confidence limit of the mean. If \code{separate = TRUE}, the result is shown as a named vector with the (mean)
#'   \code{value} and the \code{uncertaity}.
#'   Otherwise, the result is returned in the format of \eqn{value\pm uncertainty}.
#'
#' @return Named vector of (mean) \code{value} and \code{uncertaity} or \eqn{value\pm uncertainty}
#'   format depending on the \code{separate} argument, where the uncertainty actually represents non-negative
#'   limit for the mean (one side of the confidence interval not including the mean value).
#'
#'
#' @examples
#' ## double integral/intensity values
#' ## coming from several experiments:
#' di.vec <- c(0.025,0.020,0.031,0.022,0.035)
#' #
#' ## evaluation of the confidence interval
#' ## in different formats:
#' eval_interval_cnfd_tVec(di.vec)
#' #
#' eval_interval_cnfd_tVec(di.vec,lw.tail = FALSE)
#' #
#' eval_interval_cnfd_tVec(di.vec,
#'                         level.cnfd = 0.99,
#'                         separate = TRUE)
#'
#'
#' @export
#'
#'
#' @importFrom stats qt sd
#' @importFrom errors set_errors
eval_interval_cnfd_tVec <- function(data.vec.col,
                                    level.cnfd = 0.95,
                                    lw.tail = TRUE,
                                    separate = FALSE) {
  #
  ## alpha (significance level)
  alpha <- 1 - level.cnfd
  #
  ## number of experiments
  n.data <- length(data.vec.col)
  #
  ## `qt` value
  if (isTRUE(lw.tail)) {
    qt.data <- stats::qt(
      p = level.cnfd + (alpha / 2),
      df = n.data - 1,
      lower.tail = lw.tail
    )
  } else {
    qt.data <- stats::qt(
      p = alpha / 2,
      df = n.data - 1,
      lower.tail = lw.tail
    )
  }
  #
  ## uncertainty (confidence limit for the mean)
  uncrt.data <- qt.data * (stats::sd(data.vec.col) / sqrt(n.data))
  #
  ## calculation
  if (isTRUE(separate)) {
    value.cnfd <- c(
      "value" = mean(data.vec.col),
      "uncertainty" = uncrt.data
    )
  } else {
    value.cnfd <- errors::set_errors(mean(data.vec.col), uncrt.data)
  }
  #
  return(value.cnfd)
  #
}
