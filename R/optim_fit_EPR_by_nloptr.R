#'
#'
#' General Function to Non-Linear Optimization or Fitting the EPR Spectral Data
#'
#'
#' @family Simulation and Optimization
#'
#'
#' @description
#' A short description...
#'
#'
#' @details
#' Additional details...
#'
#'
#'
#' @inheritParams nloptr::slsqp
#' @param method Character string, method...
#' @param x.0 Numeric vector ...
#' @param fn Objective function that is to be minimized ...+ examples
#' @param data Data object ...
#' @param col.name.pattern Character string...
#'
#'
#' @return description
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
#' @importFrom nloptr slsqp neldermead mma ccsaq
optim_fit_EPR_by_nloptr <- function(method = "slsqp",
                                    x.0,
                                    fn,
                                    lower,
                                    upper,
                                    data,
                                    col.name.pattern) {
  if (method == "slsqp") {
    return(nloptr::slsqp(
      x0 = x0, fn = fn,
      lower = lower, upper = upper,
      nl.info = FALSE, data = data,
      col.name.pattern = col.name.pattern
    ))
  }
  if (method == "neldermead") {
    return(nloptr::neldermead(
      x0 = x0, fn = fn,
      lower = lower, upper = upper,
      nl.info = FALSE, data = data,
      col.name.pattern = col.name.pattern
    ))
  }
  if (method == "mma") {
    return(nloptr::mma(
      x0 = x0, fn = fn,
      lower = lower, upper = upper,
      nl.info = FALSE, data = data,
      col.name.pattern = col.name.pattern
    ))
  }
  if (method == "ccsaq") {
    return(nloptr::ccsaq(
      x0 = x0, fn = fn,
      lower = lower, upper = upper,
      nl.info = FALSE, data = data,
      col.name.pattern = col.name.pattern
    ))
  }
}
