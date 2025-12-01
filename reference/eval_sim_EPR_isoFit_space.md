# Explore the Hyperspace of Initial EPR Simulation Parameters (Searching for the Best Fit)

This is an extended version of the
[`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md),
providing a broader range of the initial simulation parameters in order
to find a more reliable simulation fit of an experimental isotropic EPR
spectrum. The parameter space (represented by data frame/matrix and/or
vector(s)) is divided into several points (see the argument
`N.points.space`) where each of these points corresponds to starting
values (see arguments `optim.params.init` + `optim.params.init.dvary` as
well as `lineG.content` + `lineG.content.dvary`), which are optimized by
the **default**
[`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md)
**setup**. Because such procedure is computationally highly demanding,
the central loop, to iterate/evaluate parameters and the corresponding
EPR spectra, uses the
[`{future.apply}`](https://future.apply.futureverse.org/) package (see
also the
[`future_Map`](https://future.apply.futureverse.org/reference/future_mapply.html)
function). It enables relatively seamless application of [parallel
computing](https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html)
(please, also refer to the `processing` argument), regardless of the
operating system (OS) to dramatically speed-up the entire searching for
the best fit. In addition to graphical outputs, function also provides
an animated representation (using the
[`{animation}`](https://yihui.org/animation/) package) of the procedure
progress by showing the evaluated EPR spectra at each point.

## Usage

``` r
eval_sim_EPR_isoFit_space(
  data.spectr.expr,
  nu.GHz,
  B.unit = "G",
  nuclear.system.noA = NULL,
  baseline.correct = "constant",
  lineG.content = 0.5,
  lineG.content.dvary = NULL,
  lineSpecs.form = "derivative",
  optim.method = "neldermead",
  optim.params.init,
  optim.params.init.dvary = NULL,
  Nmax.evals = 256,
  N.points.space = 16,
  check.fit.plot = TRUE,
  processing = "sequential",
  animation = "Fitting_of_sim_EPR",
  ...
)
```

## Arguments

- data.spectr.expr:

  Data frame object/table, containing the experimental spectral data
  with the magnetic flux density (`"B_mT"` or `"B_G"`) and the intensity
  (see the `Intensity.expr` argument) columns.

- nu.GHz:

  Numeric value, microwave frequency in `GHz`.

- B.unit:

  Character string, denoting the magnetic flux density unit e.g.
  `B.unit = "G"` (gauss, **default**) or `B.unit = "mT"`/`"T"`
  (millitesla/tesla).

- nuclear.system.noA:

  List or nested list **without estimated hyperfine coupling constant
  values**, such as `list("14N",1)` or
  `list(list("14N", 2),list("1H", 4),list("1H", 12))`. The \\A\\-values
  are already defined as elements of the `optim.params.init`
  argument/vector. If the EPR spectrum does not display any hyperfine
  splitting, the argument definition reads `nuclear.system.noA = NULL`
  (**default**).

- baseline.correct:

  Character string, referring to baseline correction of the
  simulated/fitted spectrum. Corrections like `"constant"`
  (**default**), `"linear"` or `"quadratic"` can be applied.

- lineG.content:

  Numeric value between `0` and `1`, referring to content of the
  *Gaussian* line form. If `lineG.content = 1` (**default**) it
  corresponds to "pure" *Gaussian* line form and if `lineG.content = 0`
  it corresponds to *Lorentzian* one. The value from (0,1) (e.g.
  `lineG.content = 0.5`) represents the linear combination (for the
  example above, with the coefficients 0.5 and 0.5) of both line forms
  =\> so called *pseudo-Voigt*.

- lineG.content.dvary:

  Numeric value, corresponding to initial **var**iation of
  `lineG.content`, (Gaussian EPR line content in the simulated EPR
  spectrum) provided as \\\pm\\ **d**ifference of the central
  `lineG.content` value. For example, if `lineG.content = 0.42` and
  `lineG.content.dvary = 0.2`, the parameter will be varied within the
  range of \\0.42\pm 0.2\\, which will be divided into `N.points.space`
  points (like already shown for the example in the `N.points.space`
  argument description). **Default**: `lineG.content.dvary = NULL`,
  actually pointing to constant `lineG.value` throughout the space
  (optimization/fitting procedures).

- lineSpecs.form:

  Character string, describing either `"derivative"` (**default**) or
  `"integrated"` (i.e. `"absorption"` which can be used as well) line
  form of the analyzed EPR spectrum/data.

- optim.method:

  Character string (vector), setting the optimization method(s) gathered
  within the
  [`optim_for_EPR_fitness`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md).
  **Default**: `optim.method = "neldermead"`. Additionally, several
  consecutive methods can be defined like
  `optim.method = c("levenmarq","neldermead")`, where the best fit
  parameters from the previous method are used as input for the next
  one.

- optim.params.init:

  Numeric vector with the initial parameter guess (elements) where the
  **first five elements are immutable**

  1.  g-value (g-factor)

  2.  **G**aussian linewidth

  3.  **L**orentzian linewidth

  4.  baseline constant (intercept or offset)

  5.  intensity multiplication constant

  6.  baseline slope (only if `baseline.correct = "linear"` or
      `baseline.correct = "quadratic"`), if
      `baseline.correct = "constant"` it corresponds to the **first
      HFCC** (\\A_1\\)

  7.  baseline quadratic coefficient (only if
      `baseline.correct = "quadratic"`), if
      `baseline.correct = "constant"` it corresponds to the **second
      HFCC** (\\A_2\\), if `baseline.correct = "linear"` it corresponds
      to the **first HFCC** (\\A_1\\)

  8.  additional HFCC (\\A_3\\) if `baseline.correct = "constant"` or if
      `baseline.correct = "linear"` (\\A_2\\), if
      `baseline.correct = "quadratic"` it corresponds to the **first
      HFCC** (\\A_1\\)

  9.  ...additional HFCCs (\\A_k...\\, each vector element is reserved
      only for one \\A\\)

  DO NOT PUT ANY OF THESE PARAMETERS to `NULL`. If the lineshape is
  expected to be pure **L**orentzian or pure **G**aussian then put the
  corresponding vector element to `0`.

- optim.params.init.dvary:

  Numeric vector with initial **var**iations of the corresponding
  `optim.params.init` elements in the form of **d**ifferences from the
  central `optim.params.init` values. For example, for the aminoxyl
  radical we may assume
  `optim.params.init = c(2.006,4.8,4.8,0,1.4e-2,49)` (see the
  `optim.params.init` argument definition). Thus, the
  `optim.params.init.dvary` could be defined as follows:
  `c(0.002,2.0,2.0,0,1e-2,3.2)`, meaning that \\g = 2.006\pm 0.002\\,
  \\\Delta B\_{\text{pp}}^{\text{G}} = 4.8\pm 2.0\\\text{G}\\, \\\Delta
  B\_{\text{pp}}^{\text{L}} = 4.8\pm 2.0\\\text{G}\\, constant baseline
  \\0\pm 0\\, ...etc. The user may "fix" one or more initial parameter
  values by putting the corresponding `optim.params.init.dvary` element
  to `0`. However, this does not fix that (those) parameter(s) in a true
  sense. Rather, the **initial** parameter **value remains constant**
  over the whole space (however, it will be optimized within the default
  bound constraints anyway, see the
  [`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md)
  documentation). If one really wants to fix one or more parameters
  (which won't be optimized) an optional `optim.params.fix.id` (see also
  description of the `...` argument) should be used together with `0` of
  the corresponding `optim.params.init.dvary` element. For example, to
  fix the g-Value (i.e. it won't be optimized) of the above-described
  aminoxyl, one must add `optim.params.fix.id = 1` and
  `optim.params.init.dvary = c(0,2.0,2.0,0,1e-2,3.2)`, like already
  demostrated in the `Examples`. If the entire `optim.params.init`
  argument is to be "fixed" =\> put `optim.params.init.dvary = NULL`
  (**default**). In all cases, the related `optim.params.init` space
  will be created as a matrix or data frame (see also the
  `Value`/`init.space.df`) with variables/columns corresponding to
  individual parameters, and observations/rows corresponding to each
  `N.points.space`, dividing the parameter variation range (e.g \\g =
  2.006\pm 0.002\\) into smaller spaces. Therefore, the fitting process
  will be performed (by the default
  [`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md)
  configuration) for each row of the initial data frame
  (`init.space.df`) together with the initial `lineG.content` variation
  vector (see the description of `N.points.space` and
  `lineG.content.dvary` arguments). For the
  `optim.params.init.dvary = NULL`, the fitting procedure is just
  repeated `N.points.space`-times, with the same parameter set. Such
  processing might be useful to determine the uncertainty (represented
  by the confidence interval) of each optimized EPR simulation parameter
  by the
  [`eval_interval_cnfd_tVec`](https://jatanrt.github.io/eprscope/reference/eval_interval_cnfd_tVec.md)
  for each column of the `optim.space.df` (see the `Value`).

- Nmax.evals:

  Numeric value, maximum number of function evaluations and/or
  iterations. The only one method, limited by this argument, is
  [`nls.lm`](https://rdrr.io/pkg/minpack.lm/man/nls.lm.html), where
  `Nmax.evals = 1024`. Higher `Nmax.evals` may extremely extend the
  optimization time, therefore the **default** value reads
  `Nmax.evals = 512`. However, the `"pswarm"` method requires at least
  the default or even higher values.

- N.points.space:

  Numeric value, identical to number of points by which the initial
  parameter-hyperspace (see the `lineG.content.dvary` and/or
  `optim.params.init.dvary` and their corresponding `lineG.content` as
  well as `optim.params.init` arguments) is divided, in order to find
  the best optimized parameters for EPR simulation fit of the isotropic
  experimental spectrum. **Default**: `N.points.space = 16`, e.g. if
  `lineG.content = 0.42` and `lineG.content.dvary = 0.2`, the initial
  corresponding vector looks like
  `c(0.220,0.247,0.273,0.300,0.327,...,0.567,0.593,0.620)`, where the
  length of this vector is equal to `N.points.space = 16`.

- check.fit.plot:

  Logical, whether to return overlay plot with the initial simulation +
  the best simulation fit + experimental spectrum (including residuals
  in the lower part of the plot, `check.fit.plot = TRUE`, **default**)
  or with the following three spectra (`check.fit.plot = FALSE`): 1.
  experimental, 2. the best simulated one with the baseline fit and 3.
  the best simulated spectrum with the baseline fit subtracted. The
  latter two are offset for clarity, within the plot.

- processing:

  Character string, corresponding to `"sequential"` (**default**
  traditional computing method), or `"parallel"` processing/evaluation
  of EPR spectrum fit (optimization of parameters). The latter
  dramatically speeds up the execution time for all points (see the
  `N.points.space` argument) of the initial parameter-hyperspace, by
  dividing all the loops/iterations/evaluations into smaller sub-tasks,
  which are processed simultaneously. When selecting [parallel
  processing](https://grantmcdermott.com/ds4e/parallel.html), the
  function/script automatically detects the number of CPU cores of your
  machine and selects half of them (e.g. for 4 cores in total, 2 cores
  are selected) for the computation. Otherwise, if the hardware
  resources are limited (2 cores in total), the
  `processing = "parallel"` automatically switches to `"sequential"`
  mode.

- animation:

  Character string, pointing to name of the animated `.gif` file,
  returned after processing and stored in the working directory (see the
  `Value`). If the animation is not desirable, put `animation = NULL`.
  Otherwise, an arbitrary file name can be chosen. **Default**:
  `animation = "Fitting_of_sim_EPR"`.

- ...:

  additional arguments specified, see also the
  [`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md),
  like `tol.step`, `pswarm` arguments (if `optim.method = "pswarm"`),
  `Blim`, `Intensity.expr` or `Intensity.sim` and `optim.params.fix.id`.

## Value

If the `animation` argument is different from `NULL`, the function will
return a `.gif` animation of the fitting procedure progress, showing the
EPR spectra at each evaluation, based on the `check.fit.plot` argument.
The `animation` file will be stored in the working directory of your
project. Additionally, a message, appeared in the R console, informs
that the animation file was created. Regardless of the `.gif` animation
a list with the following elements is provided:

- df.init.space:

  A data frame object representing hyperspace of the initial EPR
  simulation fitting parameters corresponding to `optim.params.init` and
  `optim.params.init.dvary`. Each variable/column corresponds to EPR
  simulation parameter to be optimized and each observation/row is
  related to one `N.points.space`, dividing the range for each parameter
  defined by the `optim.params.init.dvary`. The fitting/optimization is
  performed for each row of the `init.space.df`.

- df.optim.space:

  Data frame object similar to `init.space.df`, however with optimized
  EPR simulation parameters (after the fitting procedure). In addition,
  the `optim.space.df` contains the following metrics of the
  optimization/fitting as variables/columns: sum of the residual squares
  `RSS`, standard deviation of residuals `residualSD`, Akaike
  information criterion `AIC` and Bayesian information criterion `BIC`.
  These four parameters are actually related to optimization/fitting
  path (see the `plot.optim.space` below).

- plot.init.space:

  A `ggplot2` object, corresponding to graphical representation of the
  `init.space.df` created by the
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- plot.optim.space:

  A `ggplot2` object, corresponding to graphical representation of the
  `optim.space.df` created by the
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  One can also easily recognize the best fit/optimized parameter set,
  because the `Evaluation` with those parameters is highlighted by the
  green line. Additionally, each optimized parameter *vs* evaluation
  relation is fitted by the
  [`loess`](https://rdrr.io/r/stats/loess.html) function implemented in
  the
  [`geom_smooth`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)
  in order to show the trend and the \\95\\\\\\ confidence interval of
  the parameter optimization. This is especially important for the
  `RSS`, `residualSD`, `AIC` and `BIC`, as they represent "hills" and
  "valleys" of the optimization/fitting path to identify the minima.

- best.fit.params:

  Vector of the best final fitting (optimized) parameters (in the
  `plot.optim.space` distinguished by the green line) and related to the
  `optim.params.init` argument.

- best.lineG.content:

  Numeric value of the Gaussian line content of the simulated EPR
  spectrum. If `lineG.content.dvary = NULL` it corresponds to the
  original/initial value (`lineG.content`). Otherwise, a value from the
  corresponding vector, defined by the `lineG.content` +
  `lineG.content.dvary` + `N.points.space`, and related to the `RSS`
  minimum is returned.

- plots.fit.EPRspec:

  List of individual EPR spectra, depending on the `check.fit.plot`
  argument and corresponding to each `Evaluation` (refer also to the
  `plot.optim.space`). These are the actual spectra by which the
  animation was created.

## Note

In order to monitor and compare load of the hardware resources when
running `processing = "parallel"` and `"sequential"`, one might use the
following applications depending on the OS. For *Windows*:
`task manager` GUI (graphical user interface), for *Linux*: terminal
applications like `top`/`htop` or `system monitor` GUI and for `MacOS`
terminal applications like `top`/`htop` or `activity monitor` GUI.

## See also

Other Simulations and Optimization:
[`eval_ABIC_forFit()`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md),
[`eval_sim_EPR_iso()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md),
[`eval_sim_EPR_isoFit()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md),
[`eval_sim_EPR_iso_combo()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso_combo.md),
[`optim_for_EPR_fitness()`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md),
[`plot_eval_EPRtheo_mltiplet()`](https://jatanrt.github.io/eprscope/reference/plot_eval_EPRtheo_mltiplet.md),
[`plot_eval_RA_forFit()`](https://jatanrt.github.io/eprscope/reference/plot_eval_RA_forFit.md),
[`quantify_EPR_Sim_series()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Sim_series.md),
[`smooth_EPR_Spec_by_npreg()`](https://jatanrt.github.io/eprscope/reference/smooth_EPR_Spec_by_npreg.md)

## Examples

``` r
if (FALSE) { # \dontrun{
 ## run parallel processing to fit the EPR spectrum
 ## of the TMPD radical cation (+ zoom the spectrum
 ## output by `Blim`), animation
 ## "Fitting_of_sim_EPR.gif" stored in the working dir.
 listfit01 <-
   eval_sim_EPR_isoFit_space(
     data.spectr.expr = data.tmpd.spec,
     nu.GHz = data.tmpd.params.values[1,2],
     nuclear.system.noA = list(
       list("14N", 2), # 2 x 14N
       list("1H", 4), # 4 x 1H
       list("1H", 12) # 12 x 1H
     ),
     optim.params.init = c(
       2.0030, 0.4, 0.4, 0,
       2.5e5, 20.0, 5.5, 19
     ),
     optim.params.init.dvary =
     c(0.0002,0.1,0.1,0,
       2e4,2,1,2), ## or NULL
     # Nmax.evals = 256,
     # N.points.space = 16, # total number of iters. 4096
     lineG.content = 0.3,
     lineG.content.dvary = 0.15, ## or NULL
     # optim.method = "neldermead",
     processing = "parallel" , ## or "sequential"
     Blim = c(3455,3545)
 )
 #
 ## optimization/fitting progress
 ## (main graphical output)
 listfit01$plot.optim.space
 #
 ## run the similar processing and evaluation
 ## like before, but now with the fixed g-value
 ## (won't be optimized) and with 24 `N.points.space`
 listFit02 <-
    eval_sim_EPR_isoFit_space(
      data.spectr.expr = data.tmpd.spec,
      nu.GHz = data.tmpd.params.values[1,2],
      nuclear.system.noA = list(
        list("14N", 2), # 2 x 14N
        list("1H", 4), # 4 x 1H
        list("1H", 12) # 12 x 1H
      ),
      optim.params.init = c(
        2.00305, 0.4, 0.4, 0,
        1.25e4, 20.0, 5.5, 19
      ),
      optim.params.init.dvary =
      c(0,0.1,0.1,0, # `.dvary` for g-value = 0
        2e3,2,1,2),
      # Nmax.evals = 256,
      N.points.space = 24, # total number of iters. 6144
      lineG.content = 0.3,
      lineG.content.dvary = 0.15,
      processing = "parallel" ,
      optim.params.fix.id = 1, # fix g-value
      Blim = c(3455,3545)
    )
  #
  ## space (plot) for initial parameters:
  listFit02$plot.init.space
  #
  ## space (plot) for optimized parameters
  listFit02$plot.optim.space
} # }

```
