#'
#'
#' General Function for Non-Linear Optimization/Fitting of EPR Parameters/Data
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   General-purpose optimization of the objective \code{fn} function (also called "fitness")
#'   which is to be minimized in order to fit theoretical models (EPR simulations)
#'   onto the experimental data. Several methods/algorithms are implemented (see also \code{Details}):
#'   from the \href{https://astamm.github.io/nloptr/}{nloptr} package: \code{\link[nloptr]{slsqp}},
#'   \code{\link[nloptr]{neldermead}}, \code{\link[nloptr]{crs2lm}}, \code{\link[nloptr]{sbplx}},
#'   \code{\link[nloptr]{cobyla}}, \code{\link[nloptr]{lbfgs}}; from the
#'   \href{https://cran.r-universe.dev/minpack.lm/doc/manual.html}{minpack.lm} package:
#'   \code{\link[minpack.lm]{nls.lm}} and finally
#'   from the \href{https://cran.r-project.org/web/packages/pso/pso.pdf}{pso} package:
#'   \code{\link[pso]{psoptim}}.
#'
#'
#' @details
#'   All algorithms are based on the least-square minimization however,
#'   the \code{fn} definition in case of \code{nls.lm} must be provided as a difference/residual
#'   vector (see also \code{\link{eval_kinR_EPR_modelFit}}) and not as sum of differences/residual squares.
#'   The applied optimization/fitting methods are summarized in the following table (please, consult the details
#'   in \code{References} or in the individual function documentation - links in the \code{Description}) =>
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
#'   on a sequence of sub-spaces. \cr
#'   \code{nls.lm} (\code{levenmarq}) \tab \code{{minpack.lm}} \tab Modified Levenberg-Marquardt algorithm.
#'   It is a combination of gradient descent and Gauss-Newton method. \cr
#'   \code{psoptim} (\code{pswarm}) \tab \code{{pso}} \tab Particle swarm optimization, which is a population-based
#'   stochastic optimization algorithm motivated by the intelligent collective behavior of some animals such
#'   as flocks of birds or schools of fish. \cr
#'   }
#'   Not all \code{{nloptr}}-methods are implemented into the \code{optim_for_EPR_fitness}. Those summarized above
#'   were tested by the EPR simulation fit (see \code{\link{eval_sim_EPR_isoFit}}) on the experimental spectra
#'   of TEMPOL and Wuster's blue radicals. They provide the best results (without extensive "playing"
#'   with \code{\link[nloptr]{nl.opts}}, i.e. with options to control the optimization procedure) and proceed
#'   relatively fast.
#'
#'
#' @references
#'  Johnson SG (2023). “The NLopt nonlinear-optimization package.” \url{https://github.com/stevengj/nlopt}.
#'
#'  Stamm A (2023). “nloptr.” \url{https://github.com/astamm/nloptr/}.
#'
#'  Mullen KM, Elzhov TV, Spiess A, Bolker B (2023). “minpack.lm.” \url{https://github.com/cran/minpack.lm}.
#'
#'  Gavin HP (2019). “The Levenberg-Marquardt algorithm for nonlinear least squares curve-fitting problems.”
#'  \emph{Department of civil and environmental engineering, Duke University},
#'  \url{https://people.duke.edu/~hpgavin/lm.pdf}.
#'
#'  Adyatama A (2019). “Particle Swarm Optimization.” \url{https://rpubs.com/argaadya/intro-pso}.
#'
#'  Tam A (2021). “A Gentle Introduction to Particle Swarm Optimization.”
#'  \url{https://machinelearningmastery.com/a-gentle-introduction-to-particle-swarm-optimization/}.
#'
#'  Ugolotti R, Cagnoni S (2016). "A Fair Comparison Between Standard PSO Versions."
#'  In: Rossi F, Mavelli F, Stano P, Caivano D (eds), \emph{Advances in Artificial Life, Evolutionary
#'  Computation and Systems Chemistry}, WIVACE 2015, \emph{Communications in Computer and Information Science},
#'  Springer, \url{https://doi.org/10.1007/978-3-319-32695-5_1}.
#'
#'  Zambrano-Bigiarini M, Clerc M, Rojas-Mujica R (2013). "Standard Particle Swarm Optimisation 2011
#'  at CEC-2013: A baseline for future PSO improvements." \emph{2013 IEEE Congress on Evolutionary Computation},
#'  \url{https://ieeexplore.ieee.org/document/6557848}.
#'
#'
#' @inheritParams nloptr::slsqp
#' @param method Character string, pointing to applied optimization method/algorithm. One may choose one from
#'   those listed in \code{Details}, \strong{default}: \code{method = "neldermead"}, setting up
#'   the \href{https://brandewinder.com/2022/03/31/breaking-down-Nelder-Mead/}{"Nelder-Mead" simplex method}.
#' @param x.0 Numeric vector with the initial values to be optimized in order to fit onto the experimental data.
#' @param fn Objective function that is to be minimized. Usually it is the function calculating the sum of residual squares,
#'   in which a more general parameterized one can be implemented (see \code{Details} and \code{Examples}).
#' @param data Data frame object, containing columns/variables (e.g. intensity of an EPR spectrum),
#'   required to undergo a fitting/optimization process.
#' @param Nmax.evals Numeric value, maximum number of function evaluations and/or iterations.
#'   The only one method, limited by this argument, is \code{\link[minpack.lm]{nls.lm}}, where
#'   \code{Nmax.evals = 1024}. Higher \code{Nmax.evals} may extremely extend the optimization
#'   time, therefore the \strong{default} value reads \code{Nmax.evals = 512}. However, the \code{"pswarm"}
#'   method requires at least the default or even higher values.
#' @param tol.step Numeric, the smallest optimization step (relative change) between
#'   2 iterations to stop the optimization procedure. For the \code{method = "pswarm"}
#'   (particle swarm optimization procedure) it actually corresponds to tolerance for restarting.
#'   Once the maximum distance between the "best" particle and all the others is less
#'   than \code{tol.step} * \code{pswarm.diameter}) the algorithm restarts.
#'   See also \code{\link[pso]{psoptim}}. \strong{Default}: \code{tol.step = 5e-7}.
#' @param pswarm.size Numeric value, which equals to particle swarm size (i.e. number of particles),
#'   if \code{method = "pswarm"}. The \strong{default} value (\code{pswarm.size = NULL}) actually
#'   corresponds to \code{floor(10+2*sqrt(length(x.0)))} (for \code{SPSO2007}, see the \code{pswarm.type}
#'   argument), e.g. to optimize 8 parameters, number of particles = 15. For the \code{SPSO2011}
#'   the default number of particles equals to \code{40}.
#' @param pswarm.diameter Numeric value, corresponding to diameter of the particle swarm search space
#'   (in case \code{method = "pswarm"}). The \strong{default} value (\code{pswarm.diameter = NULL})
#'   refers to the Euclidean distance, defined as:
#'   \deqn{\sqrt{\sum_k\,(\text{upper}[k] - \text{lower}[k])^2}}
#' @param pswarm.type Character string, setting the type/version of particle swarm algorithm
#'   if \code{method = "pswarm"}. There are two types available: \code{pswarm.type = "SPSO2007"}
#'   and \code{pswarm.type = "SPSO2011"}. The latter introduced an adaptive random topology,
#'   which allows the swarm to dynamically adjust its communication structure.
#'   This helps in maintaining diversity in the swarm and improves the algorithm's ability
#'   to escape local optima. This type generally offers better performance on larger multidimensional spaces
#'   than the \code{pswarm.type = "SPSO2007"}, which uses a more static topology. Details may be found
#'   in the \code{References}. \strong{Default}: \code{pswarm.type = NULL} (actually corresponding
#'   to \code{"SPSO2007"}, that performs slightly better on smaller scales such as common simulations of EPR spectra
#'   with lower number of parameters like hyperfine coupling constants).
#' @param eval.optim.progress Logical. If \code{TRUE} a progress of the optimization/fitting, defined by the \code{method}
#'   argument, is monitored/tracked in the R console. The \strong{default} value is set to \code{FALSE}
#'   because higher number of evaluations/iterations might result in several tens or hundreds of rows
#'   with the information, depending on the applied \code{method}. In the case of \code{{nloptr}} methods/functions
#'   as well as \code{method = "pswarm"} it displays the iteration number (each 10-th iteration per particle
#'   shown for \code{pswarm}) and the value of the objective/fitness function (e.g. least-square minimization or RSS).
#'   Additionally, \code{pswarm} method shows the shrinking of the particle swarm diameter by the convergence.
#'   For the \code{method = "levenmarq"} it shows the iteration/evaluation number, sum of residual squares (RSS)
#'   and the corresponding parameter (Par.) values. In the \code{\link{eval_sim_EPR_isoFit}}
#'   and \code{\link{quantify_EPR_Sim_series}} this argument can be combined with the \code{msg.optim.progress}.
#'
#'
#' @return For all listed algorithms the function returns \code{list} with the elements like
#'   (please, refer to e.g. \code{Value} in \code{\link{eval_sim_EPR_isoFit}})
#'   \enumerate{
#'   \item The best parameters found (\code{par} vector, depending on the initial \code{x.0} set of parameters).
#'
#'   \item The value of \code{fn} (minimum value) corresponding to the best \code{par}.
#'
#'   \item Number of evaluations and/or iterations (in case of the \code{method = "pswarm"},
#'   see also \code{Value} in the \code{\link[pso]{psoptim}}) before the termination.
#'
#'   \item (Un)successful termination information (\code{convergence} or \code{rsstrace}), usually
#'   corresponding either to integer value showing the (un)successful termination like \code{2: Maximum number
#'   of iterations reached} (or integer code > 0 indicating successful completion) or in the case
#'   of \code{\link[minpack.lm]{nls.lm}}, it returns a vector with the values equal
#'   to sum of the residual squares at each iteration.
#'
#'   \item A descriptive message/character string, giving the additional information about the optimization
#'   procedure/termination. \strong{By default}, this is however \strong{"turned off"}, for the sake of simplicity,
#'   because most of the information can be found in the previous convergence list element
#'   or can be activated by the \code{eval.optim.progress} argument.
#'   }
#'
#'
#' @examples
#' \dontrun{
#' ## following code snippets were taken from
#' ## the `quantify_EPR_Sim_series` function
#' #
#' ## if an EPR spectrum consists of several
#' ## components or several radical spectra partly overlay,
#' ## following simple function, taking the linear
#' ## combination of max. 6 EPR simulated intensities,
#' ## can be applied in order to fit the sum of the individual
#' ## simulations onto the experimental EPR spectrum
#' ## envelope, the input parameters therefore correspond
#' ## to zero-point/intercept(par[1]) and coefficients
#' ## of the linear combinations(par[2]...par[7]),
#' ## equal to individual intensity multiplications:
#' fit_params_specs_par <- function(data,
#'                                  col.name.pattern,
#'                                  par){
#'   #
#'   # data contains variables/columns of simulated
#'   # intensities with the headers characterized
#'   # by the `col.name.patter`
#'   #
#'   data <- data[,grep(col.name.pattern,
#'                      colnames(data),
#'                      value = TRUE)]
#'   #
#'   ## create a sum for all columns/simulated spectra
#'   if (ncol(data) == 1){
#'     summa <- par[1] + (par[2] * data[[1]])
#'   }
#'   if (ncol(data) == 2){
#'       summa <- par[1] + (par[2] * data[[1]]) +
#'       (par[3] * data[[2]])
#'   }
#'   if (ncol(data) == 3){
#'     summa <- par[1] + (par[2] * data[[1]]) +
#'       (par[3] * data[[2]]) +
#'       (par[4] * data[[3]])
#'   }
#'   if (ncol(data) == 4){
#'     summa <- par[1] + (par[2] * data[[1]]) +
#'       (par[3] * data[[2]]) +
#'       (par[4] * data[[3]]) +
#'       (par[5] * data[[4]])
#'   }
#'   if (ncol(data) == 5){
#'     summa <- par[1] + (par[2] * data[[1]]) +
#'       (par[3] * data[[2]]) +
#'       (par[4] * data[[3]]) +
#'       (par[5] * data[[4]]) +
#'       (par[6] * data[[5]])
#'   }
#'   if (ncol(data) == 6){
#'     summa <- par[1] + (par[2] * data[[1]]) +
#'       (par[3] * data[[2]]) +
#'       (par[4] * data[[3]]) +
#'       (par[5] * data[[4]]) +
#'       (par[6] * data[[5]]) +
#'       (par[7] * data[[6]])
#'   }
#'   #
#'  return(summa)
#'  }
#'  #
#'  ## following function is applied to vary only
#'  ## `method`, `function` and `data`
#' optim_fn <- function(fun,method,data){
#'   optim.list <-
#'     optim_for_EPR_fitness(x.0 = optim.params.init,
#'                           method = method,
#'                           fn = fun,
#'                           lower = optim.params.lower,
#'                           upper = optim.params.upper,
#'                           Nmax.evals = Nmax.evals,
#'                           tol.step = tol.step,
#'                           pswarm.size = pswarm.size,
#'                           pswarm.diameter = pswarm.diameter,
#'                           data = data,
#'                           col.name.pattern =
#'                           "Sim.*_[[:upper:]]$"
#'     )
#'   #
#'   return(optim.list)
#'  }
#' #
#' ## finally, the following function is to be minimized:
#' min_residuals_ps <- function(data,col.name.pattern,par){
#'   with(data,sum((data[[Intensity.expr]] -
#'     fit_params_specs_par(data,col.name.pattern,par))^2)
#'     )
#'   }
#' #
#' ## therefore, the final `optimization` list may look like
#' optimization.list <-
#'   lapply(seq(data.list),
#'          function(o) {
#'          optim_fn(method = optim.method,
#'          data = data.list[[o]],
#'          fun = min_residuals_ps)
#'        }
#'     )
#' ## where `data.list` represents the list of data frames
#' ## including all individual simulated spectra
#' ## (one data frame for each spectrum + original
#' ## experimental spectrum intensity column)
#' #
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
                                  Nmax.evals = 512,
                                  tol.step = 5e-7,
                                  pswarm.size = NULL,
                                  pswarm.diameter = NULL,
                                  pswarm.type = NULL,
                                  eval.optim.progress = FALSE, ## print/show progress of fitting/optimization.
                                  ...) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## if method defined by letter case - upper
  ## convert it automatically into lower
  if (grepl("^[[:upper:]]+",method)) {
    method <- tolower(method)
  }
  #
  ## controlling the optimization for NLOPTR
  ## (control of the PSO function see below)
  contrl.list.nloptr <-
    list(maxeval = Nmax.evals,
         xtol_rel = tol.step,
         print_level = switch( ## showing progress (iterations) of the optim.
           2-eval.optim.progress,1,0
         ))
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
    #
    if (isTRUE(eval.optim.progress)) {
      message(" The optimization/evaluation progress cannot be tracked for this method/function ! ")
    }
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
      control =
        minpack.lm::nls.lm.control(
          ptol = tol.step,
          maxiter = Nmax.evals,
          nprint = switch( ## showing progress (iterations) of the optim.
            2-eval.optim.progress,1,0
          )),
      ...
    ))
  }
  #
  ## Particle Swarm Optimizatioin/Algorithm
  if (method == "pswarm"){
    #
    ## definition for the `pswarm.type`
    pswarm.type <- pswarm.type %>% `if`(is.null(pswarm.type),"SPSO2007", .)
    #
    ## definition => size of particle swarm
    ## `floor(10+2*sqrt(length(x.0)))` is the default one (for "SPSO2007")
    ## or 40 (for "SPSO2011")
    if (pswarm.type == "SPSO2007") {
      pswarm.size <- pswarm.size %>% `if`(is.null(pswarm.size),
                                          floor(10+2*sqrt(length(x.0))), .)
      if (pswarm.size >= 40) {
        message(" Please, use the `pswarm.type = SPSO2011` for such high number of particles ! ")
      }
    }
    if (pswarm.type == "SPSO2011") {
      pswarm.size <- pswarm.size %>% `if`(is.null(pswarm.size),40, .)
    }
    #
    ## definition => particle swarm diameter
    ## `sqrt(sum((upper - lower)^2))` ("Euclidean Distance") is the default one
    pswarm.diameter <- pswarm.diameter %>% `if`(is.null(pswarm.diameter),
                                                sqrt(sum((upper - lower)^2)), .)
    #
    ## control of the PSO function
    contrl.list.pso <-
      list(maxit = 256, ## The maximum number of iterations (per one particle).
           maxf = Nmax.evals, ## The maximum number of function evaluations.
           reltol = tol.step, ## The tolerance for restarting (see arguments above).
           max.restart = 128, ## The maximum number of restarts.
           maxit.stagnate = 128, ## The maximum number of iterations without improvement.
           s = pswarm.size, ## The swarm size.
           d = pswarm.diameter, ## The diameter of the search space.
           k = 6, ## The exponent for calculating the number of informants.
           type = pswarm.type, ## which reference implementation of SPSO is followed.
           ## Can take the value of “SPSO2007” or “SPSO2011”. Defaults to “SPSO2007”.
           vectorize = FALSE, ## Particles are processed in a vectorized manner,
           ## more time efficient for cheap function evaluations.
           trace = switch(2-eval.optim.progress,1,0) ## showing progress (iterations) of the optim.,
           ## the frequency of tracing is set to `10`,
           ## otherwise can be changed by `REPORT = ...` (20 or 50 or 100 or ...)
      )
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
