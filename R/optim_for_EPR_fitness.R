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
#' @param Nmax.evals Numeric, maximum number of function evaluations and/or iterations.
#' @param tol.step Numeric, the smallest optimization step (relative change) between
#'   2 iterations to stop the optimization or fitting procedure. For the \code{method == "pswarm"}
#'   (particle swarm optimization procedure) it actually corresponds to tolerance for restarting
#'   Once the maximal distance between the best particle and all other particles is less
#'   than \code{tol.step} * \code{pswarm.diameter}) the algorithm restarts.
#'   See also \code{\link[pso]{psoptim}}.
#' @param pswarm.size Numeric value equal to particle swarm size (i. e. number of particles).
#' @param pswarm.diameter Numeric value corresponding to diameter of search space.
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
#' @importFrom nloptr slsqp neldermead mma ccsaq sbplx isres crs2lm
#' @importFrom minpack.lm nls.lm.control
optim_for_EPR_fitness <- function(method = "neldermead",
                                  x.0,
                                  fn,
                                  lower,
                                  upper,
                                  data,
                                  Nmax.evals = 1000,
                                  tol.step = 1e-6,
                                  pswarm.size = NULL,
                                  pswarm.diameter = NULL,
                                  ...) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## controlling the optimization for NLOPTR
  ## (control of the PSO function see below)
  contrl.list.nloptr <- list(maxeval = Nmax.evals,
                             xtol_rel = tol.step)
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
      control = contrl.list.nloptr,
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
      control = contrl.list.nloptr,
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
      control = contrl.list.nloptr,
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
      control = contrl.list.nloptr,
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
      control = contrl.list.nloptr,
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
  ## Levenberg-Marquardt algorithm
  if (method == "levenmarq"){
    return(minpack.lm::nls.lm(
      par = x.0,
      fn = fn, ## !! NOT SQUARES BUT ONLY THE DIFFERENCE !!
      lower = lower,
      upper = upper,
      data = data,
      control = minpack.lm::nls.lm.control(ptol = tol.step,
                                           maxiter = Nmax.evals),
      ...
    ))
  }
  #
  ## Particle Swarm Optimizatioin/Algorithm
  if (method == "pswarm"){
    #
    ## definition => size of particle swarm
    ## `floor(10+2*sqrt(length(x.0)))` is the default one
    pswarm.size <- pswarm.size %>% `if`(is.null(pswarm.size),
                                        floor(10+2*sqrt(length(x.0))), .)
    #
    ## definition => particle swarm diameter
    ## `sqrt(sum((upper - lower)^2))` ("Euclidean Distance") is the default one
    pswarm.diameter <- pswarm.diameter %>% `if`(is.null(pswarm.diameter),
                                                sqrt(sum((upper - lower)^2)), .)
    #
    ## control of the PSO function
    contrl.list.pso <-
      list(maxit = Nmax.evals, ## The maximum number of iterations.
           maxf = Nmax.evals, ## The maximum number of function evaluations.
           reltol = tol.step, ## The tolerance for restarting (see arguments above).
           max.restart = 500, ## The maximum number of restarts.
           maxit.stagnate = 500, ## The maximum number of iterations without improvement.
           s = pswarm.size, ## The swarm size.
           d = pswarm.diameter) ## The diameter of the search space.
    ## + maybe in the future add average percentage of informants for each particle (`p`)
    ## and The exponent for calculating number of informants (`k`)
    #
    return(pso::psoptim(
      par = x.0,
      fn = fn,
      lower = lower,
      upper = upper,
      data = data,
      control = contrl.list.pso,
      ...
    ))
  }
  #
  ## all other parameters take the default values
}
