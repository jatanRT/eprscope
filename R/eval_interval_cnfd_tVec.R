#
#' Confidence Interval of a Vector/Data Frame-Column
#'
#'
#' @family Evaluations
#'
#'
#' @description Evaluation of the mean value and its confidence limits (according to Student's t-Distribution)
#'  corresponding to column (within data frame) or vector characterizing dispersion of the individual
#'  values, e.g. like double integrals in quantitative EPR analysis, g-value or linewidth series.
#'
#'
#' @details
#'   Additional details...
#'
#'
#'
#' @references
#'
#'
#' @param data.vec.col Numeric vector pointing to column of interest (within a data frame)
#'   to calculate the confidence interval or uncertainty.
#' @param level.cnfd Numeric (floating) value corresponding to confidence level \strong{default}:
#'   \code{level.cnfd = 0.95}.
#' @param lw.tail Logical, indicating the way how to calculate \code{qt} quantile function
#'   for the \eqn{t}-distribution. It is inherited from \code{\link[stats:TDist]{stats::qt}}.
#'   If \code{lw.tail = TRUE} (\strong{default}), probabilities are \eqn{P[X\leq x]}, otherwise, \eqn{P[X>x]}.
#' @param separate Logical, whether to separate the mean value and the uncertainty, corresponding to limits of the mean.
#'   If \code{separate = TRUE}, the result is shown as a named vector with the (mean) \code{value} and the \code{uncertaity}.
#'   Otherwise, the result is returned in the format \eqn{value\pm uncertainty}.
#'
#' @return Named vector of (mean) \code{value} and \code{uncertaity} or \eqn{value\pm uncertainty}
#'   format depending \code{separate} parameter, where the uncertainty actually represents one side
#'   of the limits for the mean.
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
