#
#' Confidence Interval of a Vector/Data Frame-Column
#'
#'
#' @family Evaluations
#'
#'
#' @description Mean value and uncertainty (according to Student's t-Distribution) corresponding to column
#'  (within data frame) or vector which characterize dispersion of individual value elements
#'  (e.g. like double integrals in quantitative EPR analysis)
#'
#'
#' @param data.vec.col Numeric vector pointing to column of interest (within a data frame)
#'   to calculate the confidence interval or uncertainty.
#' @param level.cnfd Numeric (float) corresponding to confidence level \strong{default}:
#'   \code{level.cnfd = 0.95}.
#' @param lw.tail Logical, indicating the way how to calculate \code{qt} quantile function
#'   for the \eqn{t}-distribution, it is inherited from \code{\link[stats:TDist]{stats::qt}},
#'   if \code{TRUE} (\strong{default}), probabilities are \eqn{P[X\leq x]}, otherwise, \eqn{P[X>x]}.
#' @param separate Logical, whether to separate the mean value and the uncertainty,
#'   if \code{TRUE}, the result is shown as a named vector with (mean) `value` & `uncertaity`,
#'   otherwise, the result is in the format \eqn{value\pm uncertainty}.
#'
#' @return Named vector of (mean) `value` and `uncertaity` or \eqn{value\pm uncertainty}
#'   format depending \code{separate} parameter.
#'
#' @examples
#' eval_interval_cnfd_tVec(c(0.025,0.020,0.031,0.022,0.035))
#' eval_interval_cnfd_tVec(c(0.025,0.020,0.031,0.022,0.035),
#'                          level.cnfd = 0.99,
#'                          separate = TRUE)
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
  level.sgnf <- 1 - level.cnfd
  #
  ## number of experiments
  n.data <- length(data.vec.col)
  #
  ## `qt` value
  if (isTRUE(lw.tail)) {
    qt.data <- stats::qt(
      p = 1 - level.sgnf / 2,
      df = n.data - 1,
      lower.tail = lw.tail
    )
  } else {
    qt.data <- stats::qt(
      p = level.sgnf / 2,
      df = n.data - 1,
      lower.tail = lw.tail
    )
  }
  #
  ## uncertainty
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
