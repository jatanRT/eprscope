#
#' Confidence Interval of a Vector/Data Frame-Column
#'
#'
#' @description Mean value and uncertainty (according to Sudent's t-Distribution) corresponding to column
#'  (within data frame) or vector which characterize dispersion of individual values elements
#'  (e.g. double integrals in quantitative EPR analysis)
#'
#'
#' @param data.vec.col Numeric (vector) pointing to column of interest (within a data frame) to calculate the confidence
#'  interval or uncertainty
#' @param level.cnfdnc Numeric (float) corresponding to confidence level \strong{default}: \code{level.cnfdnc = 0.95}
#' @param lw.tail Boolean, indicating the way how to calculate \code{qt} quantile function for the \eqn{t}-distribuion,
#'  it is inherited from \code{\link[stats:TDist]{stats::qt}}, if \code{TRUE} (\strong{default}), probabilities are \eqn{P[X\leq x]},
#'  otherwise, \eqn{P[X>x]}
#' @param separate Boolean, whether to separate the mean value and the uncertainty, if \code{TRUE}, the result is
#'  shown as a list with (mean) `value` & `uncertaity` names, otherwise, the result is in the format \eqn{value\pm uncertainty}
#'
#' @return Named vector of (mean) `value` and `uncertaity` or \eqn{value\pm uncertainty} format depending \code{separate} parameter
#'
#' @examples
#' \dontrun{
#' interval_cnfdnc_t_vecCol(c(0.025,0.020,0.031,0.022,0.035))
#' interval_cnfdnc_t_vecCol(c(0.025,0.020,0.031,0.022,0.035),
#'                          level.cnfdnc = 0.99,
#'                          separate = F)
#' }
#'
#' @export
#'
#' @importFrom stats qt sd
#' @importFrom errors set_errors
interval_cnfdnc_t_vecCol <- function(data.vec.col,
                                     level.cnfdnc = 0.95,
                                     lw.tail = TRUE,
                                     separate = FALSE) {
  #
  ## alpha (significance level)
  level.sgnfcn <- 1 - level.cnfdnc
  #
  ## number of experiments
  n.data <- length(data.vec.col)
  #
  ## `qt` value
  if (isTRUE(lw.tail)) {
    qt.data <- stats::qt(
      p = 1 - level.sgnfcn / 2,
      df = n.data - 1,
      lower.tail = lw.tail
    )
  } else {
    qt.data <- stats::qt(
      p = level.sgnfcn / 2,
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
    value.cnfdnc <- c(
      "value" = mean(data.vec.col),
      "uncertainty" = uncrt.data
    )
  } else {
    value.cnfdnc <- errors::set_errors(mean(data.vec.col), uncrt.data)
  }
  #
  return(value.cnfdnc)
  #
}
