#'
#'
#' General Function to Non-Linear Optimization of the EPR Data
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   General-purpose optimization of the objective \code{fn} function (also called "fitness")
#'   which is to be minimized in order to fit theoretical models (EPR simulations)
#'   onto the experimental data. Several methods/algorithms implemented
#'   in \href{https://astamm.github.io/nloptr/}{nloptr} (\code{\link[nloptr]{slsqp}}, \code{\link[nloptr]{neldermead}},
#'   \code{\link[nloptr]{crs2lm}}, \code{\link[nloptr]{sbplx}}, \code{\link[nloptr]{cobyla}}, \code{\link[nloptr]{lbfgs}}),
#'   \href{https://cran.r-universe.dev/minpack.lm/doc/manual.html}{minpack.lm} (\code{\link[minpack.lm]{nls.lm}})
#'   and \href{https://cran.r-project.org/web/packages/pso/pso.pdf}{pso} (\code{\link[pso]{psoptim}}) are used.
#'
#'
#' @details
#'   All algorithms are based on the least-square minimization however,
#'   the \code{fn} definition in case of \code{nls.lm} must be provided as a difference/residual
#'   vector (see also \code{\link{eval_kinR_EPR_modelFit}}) and not as sum of difference/residual squares.
#'   The applied optimization/fitting methods are summarized in the following table =>
#'   \tabular{lcl}{
#'   \strong{Method/Algorithm} \tab \strong{Package} \tab \strong{Short Description} \cr
#'   \code{slsqp} \tab \code{{nloptr}} \tab Sequential quadratic programming method for non-linearly
#'   constrained, gradient-based optimization. \cr
#'   \code{cobyla} \tab \code{{nloptr}} \tab Constrained optimization by linear approximations,
#'   algorithm for derivative-free optimization with nonlinear inequality and equality constraints. \cr
#'   \code{lbfgs} \tab \code{{nloptr}} \tab Low-storage version of the Broyden-Fletcher-Goldfarb-Shanno (BFGS) method.
#'   This is a quasi-Newton method well suited for the optimization problems with a large number of variables. \cr
#'   \code{neldermead} \tab \code{{nloptr}} \tab Nelder-Mead ("N-M") simplex algorithm. \cr
#'   \code{crs2lm} \tab \code{{nloptr}} \tab Controlled Random Search (CRS) algorithm (and in particular,
#'   the CRS2 variant) with the `local mutation' modification. \cr
#'   \code{sbplx} \tab \code{{nloptr}} \tab Subplex algorithm, which is a variant of the "N-M" method
#'   on a sequence of subspaces. \cr
#'   \code{nls.lm} (\code{levenmarq}) \tab \code{{minpack.lm}} \tab Modified Levenberg-Marquardt algorithm.
#'   It is a combination of gradient descent and Guass-Newton method. \cr
#'   \code{psoptim} (\code{pswarm}) \tab \code{{pso}} \tab Particle swarm optimization, which is a population-based
#'   stochastic optimization algorithm motivated by the intelligent collective behavior of some animals such
#'   as flocks of birds or schools of fish. \cr
#'   }
#'   Not all \code{{nloptr}}-methods are implemented into the \code{optim_for_EPR_fitness}. Those summarized above
#'   were tested by the EPR simulation fit (see \code{\link{eval_sim_EPR_isoFit}}) on the experimental spectra
#'   of TEMPOL and Wuster's blue radicals. They provide the best results (without extensive "playing"
#'   with \code{\link[nloptr]{nl.opts}}, i.e. with options to control the optimization procedure) and proceeds
#'   relatively fast.
#'
#' @references
#'  \insertRef{nlopt2023}{eprscope}
#'
#'  \insertRef{nloptr2023}{eprscope}
#'
#'  \insertRef{levenmarq2023}{eprscope}
#'
#'  \insertRef{gavin2019levenberg}{eprscope}
#'
#'  \insertRef{AdyatamaPSO2019}{eprscope}
#'
#'  \insertRef{TamPSO2021}{eprscope}
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
#'   2 iterations to stop the optimization or fitting procedure. For the \code{method = "pswarm"}
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
#' @importFrom nloptr slsqp neldermead cobyla sbplx lbfgs crs2lm
#' @importFrom minpack.lm nls.lm.control
optim_for_EPR_fitness <- function(method = "neldermead",
                                  x.0,
                                  fn,
                                  lower,
                                  upper,
                                  data,
                                  Nmax.evals = 1024,
                                  tol.step = 5e-7,
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
  ## Constrained Optimization by Linear Approximations
  ## algorithm for derivative-free optimization with nonlinear
  ## inequality and equality constraints
  if (method == "cobyla"){
    return(nloptr::cobyla(
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
  ## Low-storage version of the Broyden-Fletcher-Goldfarb-Shanno (BFGS) method.
  ## quasi-Newton optimization methods. It is well suited for optimization problems
  ## with a large number of variables.
  if (method == "lbfgs"){
    return(nloptr::lbfgs(
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
           max.restart = 512, ## The maximum number of restarts.
           maxit.stagnate = 512, ## The maximum number of iterations without improvement.
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
