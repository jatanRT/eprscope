# Quantify EPR Simulated Spectral Components in the Experimental Series

Evaluating the linear combination of spectral intensities of components
(loaded as ASCII text files corresponding to simulated spectra). The
related intensity multiplication coefficients (please, refer to the
`optim.params.init` argument) are optimized by methods gathered in the
[`optim_for_EPR_fitness`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md).
The goal is to fit the sum of the simulated components onto each
experimental spectrum within series. The maximum number of components is
set to 10.

## Usage

``` r
quantify_EPR_Sim_series(
  data.spectra.series,
  dir_ASC_sim,
  name.pattern.sim,
  origin.sim = "easyspin",
  var2nd.series = "time_s",
  B.unit = "G",
  col.names.sim = c("Bsim_G", "dIeprSim_over_dB"),
  x.sim.id = 1,
  Intensity.sim.id = 2,
  Intensity.expr = "dIepr_over_dB",
  optim.method = "sbplx",
  optim.params.init,
  optim.params.lower = NULL,
  optim.params.upper = NULL,
  Nmax.evals = 512,
  single.integ = "single_IntegSim",
  double.integ = "double_IntegSim",
  msg.optim.progress = TRUE,
  output.area.stat = TRUE,
  ...
)
```

## Arguments

- data.spectra.series:

  Spectrum data frame/table object containing magnetic flux density as
  `x` variable. They can be labeled as `Field`, `B_mT` in mT (or `B_G`
  in gauss). The `y/Intensity` variable can be labeled as
  `dIepr_over_dB`, in case of derivative intensity, or if integrated
  spectral intensities are present, they can be labeled accordingly. See
  also `Intensity.expr` parameter/argument. A second independent
  variable `var2nd.series` column (e.g. `var2nd.series = "time_s"`) must
  be available. In such case, the entire `data.spectra` must be present
  in the form of [tidy/long table
  format](https://r4ds.had.co.nz/tidy-data.html) (see also the
  parameter/argument `var2nd.series`). Such data frame can be created,
  e.g. by the
  [`readEPR_Exp_Specs_kin`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_kin.md)
  or the
  [`readEPR_Exp_Specs_multif`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_multif.md)
  function.

- dir_ASC_sim:

  Character string, pointing to folder where the simulated EPR spectra
  (data files) of all components are stored. Path can be alternatively
  specified by the [`file.path`](https://rdrr.io/r/base/file.path.html)
  function.

- name.pattern.sim:

  Character string pattern from file names related to simulated EPR
  spectral data like `name.pattern.sim = "DHMB0_1st_04_SimA"` or
  `name.pattern.sim = "DHMB0_1st_04_Sim[[:upper:]]"` (for the file names
  `..._SimA`, `..._SimB`,...etc). It assumes, those files must have
  similar names and this pattern appears at the beginning of the file
  name. One may also consult how to [use regular expressions in
  R](https://r4ds.hadley.nz/regexps).

- origin.sim:

  Character string, referring to "origin" of the simulated ASCII data.
  There are four possibilities \\\Rightarrow\\
  `sim.orimgin = "easyspin"` (**default**), `"xenon"`, `"simfonia"` as
  well as universal `"csv"`.

- var2nd.series:

  Character string referred to name of the second independent
  variable/quantity column in the original `data.spectra.series` (such
  as time, temperature, electrochemical potential, Microwave Power)
  altered during individual experiments as a second variable. Data must
  be available in tidy/long table format. **Default**:
  `var2nd.series = "time_s"`.

- B.unit:

  Character string, pointing to unit of magnetic flux density like `"G"`
  (Gauss) or `"mT"` (millitesla), **default**: `B.unit = "G"`. THE UNIT
  MUST BE SHARED ACROSS ALL RELEVANT B-ARGUMENTS/DATAFRAMES.

- col.names.sim:

  Character string vector, pointing to column names/headers of the
  original ASCII data series (refer also to the
  [`readEPR_Sim_Spec`](https://jatanrt.github.io/eprscope/reference/readEPR_Sim_Spec.md)
  function). **Default**:
  `col.names.sim = c("Bsim_G","dIeprSim_over_dB")`, corresponding to the
  `origin.sim = "easyspin"`.

- x.sim.id:

  Numeric index related to the `col.names.sim` vector pointing to
  independent variable (like "B" or "Bsim" - magnetic flux density),
  which corresponds to \\x\\-axis in the simulated spectra (refer also
  to the original ASCII data of the simulated spectrum). **Default**:
  `x.sim.id = 1` (see also default of the `col.names.sim`).

- Intensity.sim.id:

  Numeric index related to the `col.names.sim` vector pointing to
  simulated EPR intensity column name/header in the original ASCII data.
  **Default**: `x.sim.id = 2` (see also default of the `col.names.sim`).

- Intensity.expr:

  Character string, pointing to column name of the experimental EPR
  intensity within the original `data.spectra.series`. **Default**:
  `dIepr_over_dB`.

- optim.method:

  Character string, pointing to applied optimization method/algorithm.
  One may choose **one** from those listed in
  [`optim_for_EPR_fitness`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md),
  **default**: `method = "sbplx"`, setting up the ["Subplex"
  method](https://nlopt.readthedocs.io/en/latest/NLopt_Algorithms/#sbplx-based-on-subplex).

- optim.params.init:

  Numeric vector with the elements: baseline constant/intercept followed
  by simulated intensity multiplication coefficient for each EPR
  spectral component. Therefore, the length of this vector is equal to
  number of components + 1.

- optim.params.lower:

  Numeric vector with the length of `optim.params.init` and the lower
  bound constraints. **Default**: `optim.params.init = NULL`, actually
  corresponding to vector with all `0` value elements.

- optim.params.upper:

  Numeric vector with the length of `optim.params.init`) and the upper
  bound constraints. **Default**: `optim.params.init = NULL`, actually
  corresponding to vector with all `0.9` value elements.

- Nmax.evals:

  Numeric value, maximum number of function evaluations and/or
  iterations. The only one method, limited by this argument, is
  [`nls.lm`](https://rdrr.io/pkg/minpack.lm/man/nls.lm.html), where
  `Nmax.evals = 1024`. Higher `Nmax.evals` may extremely extend the
  optimization time, therefore the **default** value reads
  `Nmax.evals = 512`. However, the `"pswarm"` method requires at least
  the default or even higher values.

- single.integ:

  Character string, setting up the column/variable name related to
  single-integrated spectrum within the output data frame, **default**:
  `single.integ = "single_IntegSim"`.

- double.integ:

  Character string, setting up the column/variable name related to
  double-integrated spectrum within the output data frame, **default**:
  `double.integ = "double_IntegSim"`. If `double.integ = NULL`, only
  single integrals are calculated/returned (e.g. in the case of single
  integrated spectral data).

- msg.optim.progress:

  Logical, whether to display message (in the R console) about progress
  of the `optim.method` during the optimization/fitting procedure, e.g.
  `Intensities of simulated EPR spectral component(s) are currently being evaluated/optimized`
  `by NELDERMEAD method (4 Component(s))` and at the end it shows the
  elapsed time. **Default**: `msg.optim.progress = TRUE`. If `FALSE`, no
  message is displayed. This argument can be combined with the optional
  `eval.optim.progress` (see below or described in the
  [`optim_for_EPR_fitness`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md)).

- output.area.stat:

  Logical, whether to summarize all fitted EPR spectral components, in
  columns, for each time/temperature/...etc. point in row. Additional
  optimization measures are presented as well (see `Value`).**Default**:
  `output.area.stat = TRUE`. Otherwise, tidy/long format table is
  returned, including all EPR spectra and their corresponding integrals.

- ...:

  additional arguments specified, see also
  [`optim_for_EPR_fitness`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md),
  like `eval.optim.progress = TRUE` (which is `FALSE` by **default**),
  `pswarm.size`, `pswarm.diameter`, `pswarm.type`, `tol.step` +
  `optim.method` depended additional arguments.

## Value

Function provides data frame object, depending on the `output.area.stat`
argument, as listed below:

1.  If `output.area.stat = TRUE` (**default**), the resulting data frame
    consists of columns/variables like integrals/areas for each
    simulated and fitted EPR spectrum, where the components are denoted
    by the uppercase letters (`Area_Sim_A`, `Area_Sim_B`,...etc.); best
    fitted/optimized coefficients to multiply the intensities
    (`Optim_CoeffInt_Sim_A`, `Optim_CoeffInt_Sim_B`,...etc); best
    fitted/optimized intercept (or baseline constant,
    `Optim_intercept`); minimum sum of residual squares (`min_RSS`);
    number of evaluations/iterations (`N_evals`) and finally convergence
    information/number (`N_converg`, like already described in the
    [`optim_for_EPR_fitness`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md)).
    These variables are presented for each `var2nd.series` (e.g. time)
    point like example for one EPR spectral component:

    |            |                |                          |                     |              |             |               |
    |------------|----------------|--------------------------|---------------------|--------------|-------------|---------------|
    | **time_s** | **Area_Sim_A** | **Optim_CoeffInt_Sim_A** | **Optim_intercept** | **min_RSS**  | **N_evals** | **N_converg** |
    | 6          | 0.020624473    | 0.052843937              | 5.508809e-10        | 2.289953e-07 | 198         | 4             |
    | 21         | 0.020217930    | 0.051802287              | 5.401823e-10        | 2.438172e-07 | 177         | 4             |
    | 36         | 0.018836579    | 0.048263010              | 5.029705e-10        | 2.662651e-07 | 201         | 4             |

2.  If `output.area.stat = FALSE` Tidy/long table format of the original
    `data.spectra.series` with additional columns/variables (best fitted
    simulated intensities and their corresponding integrals) for all
    spectral components: A, B, C, ...etc.

## Details

Analyzed EPR spectra may consists of several components (see also the
[`eval_sim_EPR_iso_combo`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso_combo.md)
function), like the overlapped EPR spectra of several radicals. In order
to follow the concentration/amount variations of each individual radical
during the kinetic/temperature/...series, one must figure out, how those
individual spectral components are actually changed. In the first
approximation, it means to follow the corresponding EPR
intensities/integrals, whereas the component positions (\\g\\-values)
are assumed to be fixed (or those changes can be neglected). Therefore,
the actual function takes the linear combination of the spectral
intensities of components (simulated spectra) and optimizes the related
multiplication coefficients. Additional analysis, where the positions of
spectral components (simulated spectra) are not fixed and can be
optimized as well, is under development.

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
[`smooth_EPR_Spec_by_npreg()`](https://jatanrt.github.io/eprscope/reference/smooth_EPR_Spec_by_npreg.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## example with default arguments corresponding
## to one simulated spectral component,
## `optim.params.init` has the length
## of => number of components + 1,
## because of included intercept/baseline constant
quant.data.sim.test.a <-
  quantify_EPR_Sim_series(data.spectra.series,
     dir_ASC_sim = "./",
     optim.method = "pswarm",
     name.pattern.sim = "DHMB0_1st_04_SimA",
     optim.params.init = c(0,0.8),
     output.area.stat = TRUE)
#
## similar example with two components
## (simulated spectra) and tidy data frame
## output (not the summarized one) with all spectra
## and their corresponding integrals, additionally,
## display of the iteration progress for each EPR
## spectrum within the series is activated
quant.data.sim.test.b <-
  quantify_EPR_Sim_series(data.spectra.series,
     dir_ASC_sim = "./",
     optim.method = "sbplx",
     name.pattern.sim = "DHMB0_1st_04_Sim[[:upper:]]",
     optim.params.init = c(0,0.8,0.2),
     output.area.stat = FALSE,
     eval.optim.progress = TRUE
     )
#
} # }

```
