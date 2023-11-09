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
#' @param Nmax.evals Numeric, maximum naumber of function evalutions or iterations.
#' @param tol.step Numeric, the smallest optimization step (relative change) to stop
#'   the optimization or fitting procedure.
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
optim_EPR_by_nloptr <- function(method = "neldermead",
                                x.0,
                                fn,
                                lower,
                                upper,
                                data,
                                Nmax.evals = 2000,
                                tol.step = 1e-6,
                                ...) {
  ## controlling the optimization
  contrl.list <- list(maxeval = Nmax.evals,xtol_rel = tol.step)
  #
  ## Sequential (least-squares) quadratic programming
  ## (SQP) algorithm
  if (method == "slsqp") {
    return(nloptr::slsqp(
      x0 = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      nl.info = FALSE,
      data = data,
      control = contrl.list,
      ...
    ))
  }
  #
  ## "Nelder-Mead" simplex method
  if (method == "neldermead") {
    return(nloptr::neldermead(
      x0 = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      nl.info = FALSE,
      data = data,
      control = contrl.list,
      ...
    ))
  }
  #
  ## Globally-convergent method-of-moving-asymptotes
  ## (MMA) algorithm
  if (method == "mma") {
    return(nloptr::mma(
      x0 = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      nl.info = FALSE,
      data = data,
      control = contrl.list,
      ...
    ))
  }
  #
  ## This is a variant of CCSA ("conservative convex separable approximation")
  ## which, instead of constructing local MMA approximations, constructs simple
  ## quadratic approximations (or rather, affine approximations plus
  ## a quadratic penalty term to stay conservative)
  if (method == "ccsaq") {
    return(nloptr::ccsaq(
      x0 = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      nl.info = FALSE,
      data = data,
      control = contrl.list,
      ...
    ))
  }
  #
  ## The Controlled Random Search (CRS) algorithm (and in particular,
  ## the CRS2 variant) with the `local mutation' modification.
  if (method == "crs2lm") {
    return(nloptr::crs2lm(
      x0 = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      maxeval = Nmax.evals,
      pop.size = 10 * (length(x.0) + 1),
      xtol_rel = tol.step,
      nl.info = FALSE,
      data = data,
      ...
    ))
  }
  #
  ## Variable-metric methods are a variant of the quasi-Newton methods,
  ## especially adapted to large-scale unconstrained
  ## (or bound constrained) minimization.
  if (method == "varmetric") {
    return(nloptr::varmetric(
      x0 = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      nl.info = FALSE,
      data = data,
      control = contrl.list,
      ...
    ))
  }
  #
  ##  Variant of Nelder-Mead that uses Nelder-Mead
  ## on a sequence of subspaces.
  if (method == "sbplx") {
    return(nloptr::sbplx(
      x0 = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      nl.info = FALSE,
      data = data,
      control = contrl.list,
      ...
    ))
  }
  #
  ## The Improved Stochastic Ranking Evolution Strategy (ISRES)
  ## algorithm for nonlinearly constrained global optimization
  ## (or at least semi-global: although it has heuristics
  ## to escape local optima.
  if (method == "isres") {
    return(nloptr::isres(
      x0 = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      nl.info = FALSE,
      data = data,
      maxeval = Nmax.evals,
      pop.size = 20 * (length(x.0) + 1),
      xtol_rel = tol.step,
      ...
    ))
  }
  #
}
