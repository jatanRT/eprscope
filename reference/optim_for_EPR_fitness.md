# General Function for Non-Linear Optimization/Fitting of EPR Parameters/Data

General-purpose optimization of the objective `fn` function (also called
"fitness") which is to be minimized in order to fit theoretical models
(EPR simulations) onto the experimental data. Several methods/algorithms
are implemented (see also `Details`): from the
[nloptr](https://astamm.github.io/nloptr/) package:
[`slsqp`](https://astamm.github.io/nloptr/reference/slsqp.html),
[`neldermead`](https://astamm.github.io/nloptr/reference/neldermead.html),
[`crs2lm`](https://astamm.github.io/nloptr/reference/crs2lm.html),
[`sbplx`](https://astamm.github.io/nloptr/reference/sbplx.html),
[`cobyla`](https://astamm.github.io/nloptr/reference/cobyla.html),
[`lbfgs`](https://astamm.github.io/nloptr/reference/lbfgs.html); from
the [minpack.lm](https://cran.r-universe.dev/minpack.lm/doc/manual.html)
package: [`nls.lm`](https://rdrr.io/pkg/minpack.lm/man/nls.lm.html) and
finally from the
[pso](https://cran.r-project.org/web/packages/pso/pso.pdf) package:
[`psoptim`](https://rdrr.io/pkg/pso/man/psoptim.html).

## Usage

``` r
optim_for_EPR_fitness(
  method = "neldermead",
  x.0,
  fn,
  lower,
  upper,
  data,
  Nmax.evals = 512,
  tol.step = 5e-07,
  pswarm.size = NULL,
  pswarm.diameter = NULL,
  pswarm.type = NULL,
  eval.optim.progress = FALSE,
  ...
)
```

## Arguments

- method:

  Character string, pointing to applied optimization method/algorithm.
  One may choose one from those listed in `Details`, **default**:
  `method = "neldermead"`, setting up the ["Nelder-Mead" simplex
  method](https://brandewinder.com/2022/03/31/breaking-down-Nelder-Mead/).

- x.0:

  Numeric vector with the initial values to be optimized in order to fit
  onto the experimental data.

- fn:

  Objective function that is to be minimized. Usually it is the function
  calculating the sum of residual squares, where a more general
  parameterized one can be implemented in (see `Details` and
  `Examples`).

- lower, upper:

  lower and upper bound constraints.

- data:

  Data frame object, containing columns/variables (e.g. intensity of an
  EPR spectrum), required to undergo a fitting/optimization process.

- Nmax.evals:

  Numeric value, maximum number of function evaluations and/or
  iterations. The only one method, limited by this argument, is
  [`nls.lm`](https://rdrr.io/pkg/minpack.lm/man/nls.lm.html), where
  `Nmax.evals = 1024`. Higher `Nmax.evals` may extremely extend the
  optimization time, therefore the **default** value reads
  `Nmax.evals = 512`. However, the `"pswarm"` method requires at least
  the default or even higher values.

- tol.step:

  Numeric, the smallest optimization step (relative change) between 2
  iterations to stop the optimization procedure. For the
  `method = "pswarm"` (particle swarm optimization procedure) it
  actually corresponds to tolerance for restarting. Once the maximum
  distance between the "best" particle and all the others is less than
  `tol.step` \* `pswarm.diameter`) the algorithm restarts. See also
  [`psoptim`](https://rdrr.io/pkg/pso/man/psoptim.html). **Default**:
  `tol.step = 5e-7`.

- pswarm.size:

  Numeric value, which equals to particle swarm size (i.e. number of
  particles), if `method = "pswarm"`. The **default** value
  (`pswarm.size = NULL`) actually corresponds to
  `floor(10+2*sqrt(length(x.0)))` (for `SPSO2007`, see the `pswarm.type`
  argument), e.g. to optimize 8 parameters, number of particles = 15.
  For the `SPSO2011` the default number of particles equals to `40`.

- pswarm.diameter:

  Numeric value, corresponding to diameter of the particle swarm search
  space (in case `method = "pswarm"`). The **default** value
  (`pswarm.diameter = NULL`) refers to the Euclidean distance, defined
  as: \$\$\sqrt{\sum_k\\(\text{upper}\[k\] - \text{lower}\[k\])^2}\$\$

- pswarm.type:

  Character string, setting the type/version of particle swarm algorithm
  if `method = "pswarm"`. There are two types available:
  `pswarm.type = "SPSO2007"` and `pswarm.type = "SPSO2011"`. The latter
  introduced an adaptive random topology, which allows the swarm to
  dynamically adjust its communication structure. This helps in
  maintaining diversity in the swarm and improves the algorithm's
  ability to escape local optima. This type generally offers better
  performance on larger multidimensional spaces than the
  `pswarm.type = "SPSO2007"`, which uses a more static topology. Details
  may be found in the `References`. **Default**: `pswarm.type = NULL`
  (actually corresponding to `"SPSO2007"`, that performs slightly better
  on smaller scales such as common simulations of EPR spectra with lower
  number of parameters like hyperfine coupling constants).

- eval.optim.progress:

  Logical. If `TRUE` a progress of the optimization/fitting, defined by
  the `method` argument, is monitored/tracked in the R console. The
  **default** value is set to `FALSE` because higher number of
  evaluations/iterations might result in several tens or hundreds of
  rows with the information, depending on the applied `method`. In the
  case of `{nloptr}` methods/functions as well as `method = "pswarm"` it
  displays the iteration number (each 10-th iteration per particle shown
  for `pswarm`) and the value of the objective/fitness function (e.g.
  least-square minimization or RSS). Additionally, `pswarm` method shows
  the possible shrinking of the particle swarm diameter by the
  convergence. For the `method = "levenmarq"` it shows the
  iteration/evaluation number, sum of residual squares (RSS) and the
  corresponding parameter (Par.) values. In the
  [`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md)
  and
  [`quantify_EPR_Sim_series`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Sim_series.md)
  this argument can be combined with the `msg.optim.progress`.

- ...:

  additional arguments passed to the function.

## Value

For all listed algorithms the function returns `list` with the elements
like (please, refer to e.g. `Value` in
[`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md))

1.  The best parameters found (`par` vector, depending on the initial
    `x.0` set of parameters).

2.  The value of `fn` (minimum value) corresponding to the best `par`.

3.  Number of evaluations and/or iterations (in case of the
    `method = "pswarm"`, see also `Value` in the
    [`psoptim`](https://rdrr.io/pkg/pso/man/psoptim.html)) before the
    termination.

4.  (Un)successful termination information (`convergence` or
    `rsstrace`), usually corresponding either to integer value showing
    the (un)successful termination like
    `2: Maximum number of iterations reached` (or integer code \> 0
    indicating successful completion) or in the case of
    [`nls.lm`](https://rdrr.io/pkg/minpack.lm/man/nls.lm.html), it
    returns a vector with the values equal to sum of the residual
    squares at each iteration.

5.  A descriptive message/character string, giving the additional
    information about the optimization procedure/termination. **By
    default**, this is however **"turned off"**, for the sake of
    simplicity, because most of the information can be found in the
    previous convergence list element or can be activated by the
    `eval.optim.progress` argument.

## Details

All algorithms are based on the least-square minimization however, the
`fn` definition in case of `nls.lm` must be provided as a
difference/residual vector (see also
[`eval_kinR_EPR_modelFit`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md))
and not as sum of differences/residual squares. The applied
optimization/fitting methods are summarized in the following table
(please, consult the details in `References` or in the individual
function documentation - links in the `Description`) =\>

|                        |                |                                                                                                                                                                                                         |
|------------------------|----------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Method/Algorithm**   | **Package**    | **Short Description**                                                                                                                                                                                   |
| `slsqp`                | `{nloptr}`     | Sequential quadratic programming method for non-linearly constrained, gradient-based optimization.                                                                                                      |
| `cobyla`               | `{nloptr}`     | Constrained optimization by linear approximations, algorithm for derivative-free optimization with nonlinear inequality and equality constraints.                                                       |
| `lbfgs`                | `{nloptr}`     | Low-storage version of the Broyden-Fletcher-Goldfarb-Shanno (BFGS) method. This is a quasi-Newton method well suited for the optimization problems with a large number of variables.                    |
| `neldermead`           | `{nloptr}`     | Nelder-Mead ("N-M") simplex algorithm.                                                                                                                                                                  |
| `crs2lm`               | `{nloptr}`     | Controlled Random Search (CRS) algorithm (and in particular, the CRS2 variant) with the \`local mutation' modification.                                                                                 |
| `sbplx`                | `{nloptr}`     | Subplex algorithm, which is a variant of the "N-M" method on a sequence of sub-spaces.                                                                                                                  |
| `nls.lm` (`levenmarq`) | `{minpack.lm}` | Modified Levenberg-Marquardt algorithm. It is a combination of gradient descent and Gauss-Newton method.                                                                                                |
| `psoptim` (`pswarm`)   | `{pso}`        | Particle swarm optimization, which is a population-based stochastic optimization algorithm motivated by the intelligent collective behavior of some animals such as flocks of birds or schools of fish. |

Not all `{nloptr}`-methods are implemented into the
`optim_for_EPR_fitness`. Those summarized above were tested by the EPR
simulation fit (see
[`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md))
on the experimental spectra of TEMPOL and Wuster's blue radicals. They
provide the best results (without extensive "playing" with
[`nl.opts`](https://astamm.github.io/nloptr/reference/nl.opts.html),
i.e. with options to control the optimization procedure) and proceed
relatively fast.

## References

Johnson SG (2023). “The NLopt nonlinear-optimization package.”
<https://github.com/stevengj/nlopt>.

Stamm A (2023). “nloptr.” <https://github.com/astamm/nloptr/>.

Mullen KM, Elzhov TV, Spiess A, Bolker B (2023). “minpack.lm.”
<https://github.com/cran/minpack.lm>.

Gavin HP (2019). “The Levenberg-Marquardt algorithm for nonlinear least
squares curve-fitting problems.” *Department of civil and environmental
engineering, Duke University*,
<https://people.duke.edu/~hpgavin/lm.pdf>.

Adyatama A (2019). “Particle Swarm Optimization.”
<https://rpubs.com/argaadya/intro-pso>.

Tam A (2021). “A Gentle Introduction to Particle Swarm Optimization.”
<https://machinelearningmastery.com/a-gentle-introduction-to-particle-swarm-optimization/>.

Ugolotti R, Cagnoni S (2016). "A Fair Comparison Between Standard PSO
Versions." In: Rossi F, Mavelli F, Stano P, Caivano D (eds), *Advances
in Artificial Life, Evolutionary Computation and Systems Chemistry*,
WIVACE 2015, *Communications in Computer and Information Science*,
Springer, <https://doi.org/10.1007/978-3-319-32695-5_1>.

Zambrano-Bigiarini M, Clerc M, Rojas-Mujica R (2013). "Standard Particle
Swarm Optimisation 2011 at CEC-2013: A baseline for future PSO
improvements." *2013 IEEE Congress on Evolutionary Computation*,
<https://ieeexplore.ieee.org/document/6557848>.

## See also

Other Simulations and Optimization:
[`eval_ABIC_forFit()`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md),
[`eval_sim_EPR_iso()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md),
[`eval_sim_EPR_isoFit()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md),
[`eval_sim_EPR_isoFit_space()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit_space.md),
[`eval_sim_EPR_iso_combo()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso_combo.md),
[`plot_eval_EPRtheo_mltiplet()`](https://jatanrt.github.io/eprscope/reference/plot_eval_EPRtheo_mltiplet.md),
[`plot_eval_RA_forFit()`](https://jatanrt.github.io/eprscope/reference/plot_eval_RA_forFit.md),
[`quantify_EPR_Sim_series()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Sim_series.md),
[`smooth_EPR_Spec_by_npreg()`](https://jatanrt.github.io/eprscope/reference/smooth_EPR_Spec_by_npreg.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## following code snippets were taken from
## the `quantify_EPR_Sim_series` function
#
## if an EPR spectrum consists of several
## components or several radical spectra partly overlay,
## following simple function, taking the linear
## combination of max. 6 EPR simulated intensities,
## can be applied in order to fit the sum of the individual
## simulations onto the experimental EPR spectrum
## envelope, the input parameters therefore correspond
## to zero-point/intercept(par[1]) and coefficients
## of the linear combinations(par[2]...par[7]),
## equal to individual intensity multiplications:
fit_params_specs_par <- function(data,
                                 col.name.pattern,
                                 par){
  #
  # data contains variables/columns of simulated
  # intensities with the headers characterized
  # by the `col.name.patter`
  #
  data <- data[,grep(col.name.pattern,
                     colnames(data),
                     value = TRUE)]
  #
  ## create a sum for all columns/simulated spectra
  if (ncol(data) == 1){
    summa <- par[1] + (par[2] * data[[1]])
  }
  if (ncol(data) == 2){
      summa <- par[1] + (par[2] * data[[1]]) +
      (par[3] * data[[2]])
  }
  if (ncol(data) == 3){
    summa <- par[1] + (par[2] * data[[1]]) +
      (par[3] * data[[2]]) +
      (par[4] * data[[3]])
  }
  if (ncol(data) == 4){
    summa <- par[1] + (par[2] * data[[1]]) +
      (par[3] * data[[2]]) +
      (par[4] * data[[3]]) +
      (par[5] * data[[4]])
  }
  if (ncol(data) == 5){
    summa <- par[1] + (par[2] * data[[1]]) +
      (par[3] * data[[2]]) +
      (par[4] * data[[3]]) +
      (par[5] * data[[4]]) +
      (par[6] * data[[5]])
  }
  if (ncol(data) == 6){
    summa <- par[1] + (par[2] * data[[1]]) +
      (par[3] * data[[2]]) +
      (par[4] * data[[3]]) +
      (par[5] * data[[4]]) +
      (par[6] * data[[5]]) +
      (par[7] * data[[6]])
  }
  #
 return(summa)
 }
 #
 ## following function is applied to vary only
 ## `method`, `function` and `data`
optim_fn <- function(fun,method,data){
  optim.list <-
    optim_for_EPR_fitness(x.0 = optim.params.init,
                          method = method,
                          fn = fun,
                          lower = optim.params.lower,
                          upper = optim.params.upper,
                          Nmax.evals = Nmax.evals,
                          tol.step = tol.step,
                          pswarm.size = pswarm.size,
                          pswarm.diameter = pswarm.diameter,
                          data = data,
                          col.name.pattern =
                          "Sim.*_[[:upper:]]$"
    )
  #
  return(optim.list)
 }
#
## finally, the following function is to be minimized:
min_residuals_ps <- function(data,col.name.pattern,par){
  sum((data[[Intensity.expr]] -
    fit_params_specs_par(data,col.name.pattern,par))^2)
  }
#
## therefore, the final `optimization` list may look like
optimization.list <-
  lapply(seq(data.list),
         function(o) {
         optim_fn(method = optim.method,
         data = data.list[[o]],
         fun = min_residuals_ps)
       }
    )
## where `data.list` represents the list of data frames
## including all individual simulated spectra
## (one data frame for each spectrum + original
## experimental spectrum intensity column)
#
} # }

```
