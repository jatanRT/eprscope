#'
#'
#' General Function to Non-Linear Optimization of the EPR Spectral Data
#'
#'
#' @family Simulations and Optimization
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
optim_EPR_by_nloptr <- function(method = "slsqp",
                                x.0,
                                fn,
                                lower,
                                upper,
                                data,
                                    ...) {
  if (method == "slsqp") {
    return(nloptr::slsqp(
      x0 = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      nl.info = FALSE,
      data = data,
      ...
    ))
  }
  if (method == "neldermead") {
    return(nloptr::neldermead(
      x0 = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      nl.info = FALSE,
      data = data,
      ...
    ))
  }
  if (method == "mma") {
    return(nloptr::mma(
      x0 = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      nl.info = FALSE,
      data = data,
      ...
    ))
  }
  if (method == "ccsaq") {
    return(nloptr::ccsaq(
      x0 = x.0,
      n = fn,
      lower = lower,
      upper = upper,
      nl.info = FALSE,
      data = data,
      ...
    ))
  }
}
