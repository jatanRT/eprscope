# Interactive Least-Squares Fitting of Isotropic EPR Spectra by Simulation Iteratioins/Evaulations

Ordinary least-squares fitting of the isotropic EPR spectra by
simulation iterations/evaluations. In principle, this function is based
on the
[`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md)
however, it represents a more interactive version of the
[`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md).
Namely, it provides `{ggplot2}` objects (graphs, see the `Value` and the
`plot.fit` description) in order to simultaneously check/explore the
optimization/fitting process at each of the evaluations (refer to the
`Nevals` argument). In addition, it also simultaneously shows current
values of all the fitting parameters in the *R* console. The function
was built, because during the parallel processing (see the
[`eval_sim_EPR_isoFit_space`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit_space.md))
it is not possible to display the actual EPR spectra during the
optimization/fitting procedure. In the upcoming package updates, it will
be also implemented into the
[`plot_eval_ExpSim_app`](https://jatanrt.github.io/eprscope/reference/plot_eval_ExpSim_app.md).

## Usage

``` r
eval_sim_EPR_isoFitb(
  data.spectr.expr,
  Intensity.expr = "dIepr_over_dB",
  Intensity.sim = "dIeprSim_over_dB",
  nu.GHz,
  B.unit = "G",
  Blim = NULL,
  nuclear.system.noA = NULL,
  baseline.correct = "constant",
  lineG.content = 0.5,
  lineSpecs.form = "derivative",
  optim.method = c("neldermead", "cobyla", "lbfgs"),
  optim.params.init,
  optim.params.lower = NULL,
  optim.params.upper = NULL,
  optim.params.fix.id = NULL,
  Niters.per.eval = 128,
  Nevals = 16
)
```

## Arguments

- data.spectr.expr:

  Data frame object/table, containing the experimental spectral data
  with the magnetic flux density (`"B_mT"` or `"B_G"`) and the intensity
  (see the `Intensity.expr` argument) columns.

- Intensity.expr:

  Character string, pointing to column name of the experimental EPR
  intensity within the original `data.spectr.expr`. **Default**:
  `dIepr_over_dB`.

- Intensity.sim:

  Character string, pointing to column name of the simulated EPR
  intensity within the related output data frame. **Default**:
  `Intensity.sim = "dIeprSim_over_dB"`.

- nu.GHz:

  Numeric value, microwave frequency in `GHz`.

- B.unit:

  Character string, denoting the magnetic flux density unit e.g.
  `B.unit = "G"` (gauss, **default**) or `B.unit = "mT"`/`"T"`
  (millitesla/tesla).

- Blim:

  Numeric vector, magnetic flux density in `mT`/`G` corresponding to
  lower and upper **visual limit** of the selected \\B\\-region, such as
  `Blim = c(3495.4,3595.4)`. **Default**: `Blim = NULL` (corresponding
  to the entire \\B\\-range of EPR spectrum). **This does not correspond
  to simulation fit data region !**. If narrower \\B\\-region (in
  comparison to the original one) is required to fit the EPR spectrum,
  the filtering has to be done prior to own fitting procedure. For
  example, if the original data frame (`df.spectr.orgin` within 200 G),
  of the experimental EPR spectrum, should be fitted within the region
  of `B = c(3450,3550)` (100 G), following operation must be performed:
  `df.spectr.actuall <- df.spectr.origin |> dplyr::filter(dplyr::between(B_G,3450,3550))`,
  where the `df.spectr.actuall` serves as an input (represented by the
  `data.spectr.expr` argument) for the function.

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

- lineSpecs.form:

  Character string, describing either `"derivative"` (**default**) or
  `"integrated"` (i.e. `"absorption"` which can be used as well) line
  form of the analyzed EPR spectrum/data.

- optim.method:

  Character string, setting the optimization method/algorithm. Even
  though, by default, the argument is defined as a vector,
  `optim.method = c("neldermead","cobyla","lbfgs")`, only one method
  from those three can be selected. For example
  `optim.method = "neldermead"` (**default**). For additional
  information to all three available methods, please refer to the
  [`optim_for_EPR_fitness`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md).

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

- optim.params.lower:

  Numeric vector (with the same element order like `optim.params.init`)
  with the lower bound constraints. **Default**:
  `optim.params.lower = NULL` which actually corresponds to relative
  default limits (refer to the
  [`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md)
  arguments) of all fitted/optimized parameters for the actual
  evaluation. If specified (e.g.
  `optim.params.lower = c(2.004,0.3,0.3,-1e-3,0.001,46)`), it represents
  the general lower boundaries for the entire optimization/fitting
  procedure.

- optim.params.upper:

  Numeric vector (with the same element order like `optim.params.init`)
  with the upper bound constraints. **Default**:
  `optim.params.upper = NULL` which actually corresponds to relative
  default limits (refer to the
  [`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md)
  arguments) of all fitted/optimized parameters for the actual
  evaluation. If specified (e.g.
  `optim.params.upper = c(2.007,0.5,0.5,1e-3,0.004,48)`), it represents
  the general upper boundaries for the entire optimization/fitting
  procedure.

- optim.params.fix.id:

  Numeric value/vector of index/indices of the `optim.params.init`,
  corresponding to optimization/simulation parameter(s) to be fixed. For
  example, if the g-Value and the (intensity) multiplication constant
  should not be optimized (their values should be fixed during the
  procedure), the argument must be defined as follows:
  `optim.params.fix.id = c(1,5)` (see also the `optim.params.init`
  description for the simulation/optimized parameters order).
  **Default**: `optim.params.fix.id = NULL`, indicating that none of the
  `optim.params.init` is fixed, i.e. all parameters are optimized within
  their default/defined boundaries (see the `optim.params.lower` and the
  `optim.params.upper`). Alternatively, the parameter value(s) can be
  also adjusted by assigning the `optim.params.init` +
  `optim.params.lower` + `optim.params.upper` elements to the same
  value.

- Niters.per.eval:

  Numeric value, equal to the number of iterations per one the `Nevals`
  (see the related `Nevals` argument description). **Default**:
  `Niters.per.eval = 128`. This argument, among other things, depends on
  the complexity of the `nuclear.system` (`nuclear.system.noA`). For
  example, if an aminoxyl radical with 1 x 14N nucleus interaction is
  considered, the `Niters.per.eval` might be lower than the default one
  (e.g. 64). However, the more complex system like
  N,N,N′,N′-Tetramethyl-p-phenylenediamine radical cation, with 2 x 14N,
  4 x 1H and 12 x 1H nuclei interaction may require higher
  `Niters.per.eval` (e.g. 128). The higher the `Niters.per.eval`, the
  longer the computational fitting/optimization time.

- Nevals:

  Numeric value, corresponding to total number of evaluations/cycles
  (i.e. how many loops/runs will be considered for the entire
  fitting/optimization procedure). This argument is related to the
  `Niters.per.eval`, where the total number of iterations reads `Nevals`
  x `Niters.per.eval`. For example, for `Nevals = 16` (**default**) and
  `Niters.per.eval = 128`, the number of iterations = 16 x 128 = 2048.
  Higher `Nevals` requires longer computational time. For interactive
  visualization of EPR spectra (during the optimization/fitting
  procedure) no parallel computing is supported (contrary to the
  [`eval_sim_EPR_isoFit_space`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit_space.md)).

## Value

List with the following elements:

- plot.fit:

  A graph panel (2 rows x 2 columns) with 4 components. Simulated +
  experimental EPR spectra, residuals vs \\B\\ magnetic flux density,
  sum of the residual squares (RSS) vs iteration as well as AIC+BIC
  information criteria vs iteration. Even though it represents the final
  status of those four dependencies, the actual version, appears at each
  of the `Nevals` (see the `Arguments`) in order to follow the progress
  of the fitting procedure interactively.

- best.fit.params:

  Named vector of the optimized (best fitting) simulation parameters,
  corresponding to minimum `RSS`. The actual values also appear at each
  of the `Nevals` (see the `Arguments`), interactively during the
  fitting procedure.

- df.params.optim:

  A data frame object of all simulation parameters + `RSS` (residual sum
  of squares) + `AIC` + `BIC` (Akaike and Bayessian information
  criteria, respectively, see also the
  [`eval_ABIC_forFit`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md)),
  evaluated during the fitting process at corresponding actual
  iteration. Visual progress of all those variables may be nicely
  followed by the
  [`esquisser`](https://dreamrs.github.io/esquisse/reference/esquisser.html)
  or other *R* plotting/visualization function.

- nuclear.system:

  List consisting of all considered nuclei, and their optimized (best
  fitted) coupling constants \\A\\ in MHz, which may be used for
  additional EPR simulation (see the
  [`eval_sim_EPR_iso`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md)).

- spec.expr.params:

  Named numeric vector of parameters to record the experimental EPR
  spectrum, equal to `instrum.params` argument from the
  [`eval_sim_EPR_iso`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md).
  To be used for additional simulations.

- ra:

  Final (related to the minimum of `RSS`) **r**esidual **a**nalysis list
  (refer to the
  [`plot_eval_RA_forFit`](https://jatanrt.github.io/eprscope/reference/plot_eval_RA_forFit.md)
  and/or to
  [`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md))
  extended by the `message` which distribution (Normal/Gaussian,
  Student's t-distribution or Cauchy) fits the residuals at best and was
  actually applied to evaluate AIC and BIC (consult the
  [`eval_ABIC_forFit`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md)
  for a detailed description).

- cor.df:

  Function to evaluate correlation `matrix` of a data frame, consisting
  of EPR experimental, simulated (best fit) and residual intensities as
  columns/variables. Such matrix can be additionally nicely visualized
  by a correlation `plot` created by the
  [`corrplot`](https://rdrr.io/pkg/corrplot/man/corrplot.html) function.
  Three `methods` are available: `"pearson"` (**default**), `"spearman"`
  (captures monotonic relationships) and `"kendall"` (for small data
  ensembles), see also [`cor`](https://rdrr.io/r/stats/cor.html).

- plot.best.sim.expr:

  Visualization function to plot either static `ggplot2`
  (`interactive = FALSE`, **default**) or interactive `plotly`
  (`interactive = TRUE`) comparison between the experimental and the
  simulated EPR spectrum. In addition, both spectra, within the static
  `ggplot`, can be presented either in the `overlay = TRUE`
  (**default**) or in the offset (`overlay = FALSE`) mode.

- df.best.sim.expr:

  A data frame object, containing experimental and simulated spectra in
  long/tidy form, actually corresponding to `plot.best.sim.expr` while
  `interactive = TRUE`.

## See also

Other Simulations and Optimization:
[`eval_ABIC_forFit()`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md),
[`eval_sim_EPR_iso()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md),
[`eval_sim_EPR_isoFit()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md),
[`eval_sim_EPR_isoFit_space()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit_space.md),
[`eval_sim_EPR_iso_combo()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso_combo.md),
[`optim_for_EPR_fitness()`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md),
[`plot_eval_EPRtheo_mltiplet()`](https://jatanrt.github.io/eprscope/reference/plot_eval_EPRtheo_mltiplet.md),
[`plot_eval_RA_forFit()`](https://jatanrt.github.io/eprscope/reference/plot_eval_RA_forFit.md),
[`quantify_EPR_Sim_series()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Sim_series.md),
[`smooth_EPR_Spec_by_npreg()`](https://jatanrt.github.io/eprscope/reference/smooth_EPR_Spec_by_npreg.md)

## Examples

``` r
if (FALSE) { # \dontrun{
test.list <- eval_sim_EPR_isoFitb(
  data.spectr.expr = data.tmpd.spec,
  nu.GHz = data.tmpd.params.values[1,2],
  nuclear.system.noA = list(
    list("14N", 2),
    list("1H", 4),
    list("1H", 12)
  ),
  optim.method = "cobyla",
  optim.params.init = c(
    2.00305, ## g_iso
    0.521, ## Gaussian linewidth
    0.52, ## Lorentz linewidth
    0, ## offset/baseline constant
    3.2e5, ## intensity multiplication coeff.
    19.5, 5.5, 19.5 ## required As in MHz
  ),
  ## number of iterations per evaluation
  Niters.per.eval = 128,
  Nevals = 17 ## total number of evaluations
  ## total number of iterations =
  ## = Niters.per.eval * Nevals
)
#
## simulation fit with the lower and upper
## bound constraints
epr.spectrum.sim.fit <-
  eval_sim_EPR_isoFitb(
    data.spectr.expr = epr.spectrum.data,
    nu.GHz = 9.793116,
    B.unit = 'G',
    Blim = c(3435,3537),
    lineG.content = 0.93,
    optim.method = 'neldermead',
    optim.params.init = c(
      2.00581,0.57,0.77,0,0.0006,41.1,7.94,2.84
    ),
    optim.params.lower = c(
      2.0055,0.3,0.45,-1e-4,5e-4,40,6,1
    ),
    optim.params.upper = c(
      2.0059,0.7,1.2,1e-4,1e-3,42,9,4
    ),
    nuclear.system.noA =
      list(list('14N',1),list('1H',1),list('1H',1)),
    baseline.correct = 'constant',
    Nevals = 32,
    Niters.per.eval = 116
 )
} # }

```
