# Quantify EPR Simulated Spectral Components in the Experimental Series

**Please wait for the images to load, while rendering the web page.**
Evaluating the linear combination of spectral intensities of components
(loaded as ASCII text files corresponding to individual simulated EPR
spectra) of the overall spectral response. The related intensity
multiplication coefficients (please, refer to the `optim.params.init`
argument) are optimized by methods gathered in the
[`optim_for_EPR_fitness`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md).
The goal is to fit the sum of the simulated components onto each
experimental spectrum within the (time) series in order to quantify each
individual EPR (radical) spectrum. The maximum number of components is
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
  col.name.pattern.sim = "Sim.*_[[:upper:]]$",
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

  Character string pattern ([regular
  expression](https://r4ds.hadley.nz/regexps)) from file names related
  to simulated EPR spectral data like
  `name.pattern.sim = "DHMB0_1st_04_SimA"` or
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

- col.name.pattern.sim:

  Character string ([regular
  expression](https://r4ds.hadley.nz/regexps)), pointing to columns with
  simulated EPR intensity (within the newly generated tidy data frame),
  which are fitted onto the experimental spectra in the time series. A
  good rule of thumb is that it follows the structure the
  `name.pattern.sim` argument.

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
  `optim.method` depended additional arguments as well as
  `fix.optim.x.0.id` to fix one or several parameters defined by the
  `optim.params.init` argument.

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
    point like example for one EPR spectral component (see also
    `Examples`):

    |  |  |  |  |  |  |  |
    |----|----|----|----|----|----|----|
    | **time_s** | **Area_Sim_A** | **Optim_CoeffInt_Sim_A** | **Optim_intercept** | **min_RSS** | **N_evals** | **N_converg** |
    | 6 | 0.020624473 | 0.052843937 | 5.508809e-10 | 2.289953e-07 | 198 | 4 |
    | 21 | 0.020217930 | 0.051802287 | 5.401823e-10 | 2.438172e-07 | 177 | 4 |
    | 36 | 0.018836579 | 0.048263010 | 5.029705e-10 | 2.662651e-07 | 201 | 4 |

2.  If `output.area.stat = FALSE` Tidy/long table format of the original
    `data.spectra.series` with additional columns/variables (best fitted
    simulated intensities and their corresponding integrals) for all
    spectral components: A, B, C, ...etc.

## Details

Analyzed EPR spectra may consists of several components (see also the
[`eval_sim_EPR_iso_combo`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso_combo.md)
function), like the superimposed EPR spectra of several radicals. In
order to follow the concentration/amount variations of each individual
radical, during the kinetic/temperature/...series, one must figure out,
how those individual spectral components are actually changed. In the
first approximation, it means to follow the corresponding EPR
intensities/integrals, whereas the component positions (\\g\\-values)
are assumed to be fixed (or those changes can be neglected). Therefore,
the actual function takes the linear combination of the spectral
intensities of components (simulated spectra) and optimizes the related
intensity multiplication coefficients. Additional analysis, where the
positions of spectral components (simulated spectra) are not fixed and
can be optimized as well, will be developed for higher package version.

## See also

Other Simulations and Optimization:
[`eval_ABIC_forFit()`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md),
[`eval_CCC_forFit()`](https://jatanrt.github.io/eprscope/reference/eval_CCC_forFit.md),
[`eval_sim_EPR_iso()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md),
[`eval_sim_EPR_isoFit()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md),
[`eval_sim_EPR_isoFit_space()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit_space.md),
[`eval_sim_EPR_isoFitb()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFitb.md),
[`eval_sim_EPR_iso_combo()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso_combo.md),
[`optim_for_EPR_fitness()`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md),
[`plot_eval_EPRtheo_mltiplet()`](https://jatanrt.github.io/eprscope/reference/plot_eval_EPRtheo_mltiplet.md),
[`plot_eval_RA_forFit()`](https://jatanrt.github.io/eprscope/reference/plot_eval_RA_forFit.md),
[`smooth_EPR_Spec_by_npreg()`](https://jatanrt.github.io/eprscope/reference/smooth_EPR_Spec_by_npreg.md)

## Examples

``` r
## quantify noisy EPR spectra of triarylamine
## radical cation time series (only 1 component),
## first of all, load the entire series
triarylamine.decay.series.bin.path <-
load_data_example(file =
        "Triarylamine_radCat_decay_series.DTA")
## loading the kinetics:
triarylamine.decay.series.data <-
  readEPR_Exp_Specs_kin(
    path_to_file =
      triarylamine.decay.series.bin.path
   )
#
## quantitative information as a data frame,
## if `output.area.stat = TRUE`
quant.sim.triarylamine.df <-
 quantify_EPR_Sim_series(
    data.spectra.series =
      triarylamine.decay.series.data$df,
    # this is an `{eprscope}` pkg. dir:
    dir_ASC_sim = system.file(
      "extdata",
      package = "eprscope",
      mustWork = TRUE
    ),
    optim.method = "sbplx",
    # name pattern for `csv` EPR sim. data:
    name.pattern.sim =
      "Triarylamine_radCat_decay_9thSpec",
    col.names.sim = c(
      "Bsim_G",
      "B_G",
      "dIeprSim_over_dB",
      "dIepr_over_dB",
      "Norm_dIeprSim_over_dB"
    ),
    x.sim.id = 1,
    Intensity.sim.id = 3,
    optim.params.init = c(0,0.8),
    output.area.stat = TRUE,
    msg.optim.progress = FALSE
)
## if `msg.optim.progress = TRUE` the progress
## bar is displayed together with elapsed time
## for the entire fitting process,
## if additional `eval.optim.progress = TRUE`
## (see the `...` argument) is activated, the
## iteration process is displayed in the console,
## however without the progress bar
#
## data frame preview
quant.sim.triarylamine.df %>% head(n = 10)
#>    time_s  Area_Sim_A Optim_coeffInt_Sim_A Optim_intercept       min_RSS
#> 1       6 0.016555782          0.016924688   3.0913633e-10 7.6343589e-07
#> 2      21 0.016071703          0.016429822   3.0011773e-10 7.0784394e-07
#> 3      36 0.015599617          0.015947217   2.9131257e-10 6.8111566e-07
#> 4      51 0.015142568          0.015479985   2.8272424e-10 7.0771503e-07
#> 5      66 0.014660566          0.014987242   2.7371385e-10 6.7191227e-07
#> 6      81 0.014345326          0.014664976   2.6790890e-10 6.8113214e-07
#> 7      95 0.013978944          0.014290432   2.6101361e-10 6.8733420e-07
#> 8     110 0.013495567          0.013796283   2.5202880e-10 6.8857790e-07
#> 9     125 0.013320680          0.013617499   2.4874822e-10 7.1659238e-07
#> 10    140 0.012972439          0.013261498   2.4222224e-10 7.0110203e-07
#>    N_evals N_converg
#> 1      205         4
#> 2      216         4
#> 3      195         4
#> 4      212         4
#> 5      181         4
#> 6      196         4
#> 7      188         4
#> 8      192         4
#> 9      188         4
#> 10     197         4
#
## plot double integral (`Area_Sim_A`) vs time
ggplot2::ggplot(
  data = quant.sim.triarylamine.df,
  ggplot2::aes(x = time_s)
) +
ggplot2::geom_point(
  ggplot2::aes(y = Area_Sim_A),
  size = 3.2,
  color = "darkorange"
) +
ggplot2::labs(
  x = plot_labels_xyz(time,s),
  y = plot_labels_xyz(Double~~Integral,p.d.u.)
) + plot_theme_In_ticks()

#
## plot minimum sum of residual squares vs time
ggplot2::ggplot(
  data = quant.sim.triarylamine.df,
  ggplot2::aes(x = time_s)
) +
ggplot2::geom_point(
  ggplot2::aes(y = min_RSS),
  size = 3.2,
  color = "darkblue"
) +
ggplot2::labs(
  x = plot_labels_xyz(time,s),
  y = bquote(italic(RSS))
) + plot_theme_In_ticks()

#
## plot number of evaluations/iterations
## for each time point
ggplot2::ggplot(
  data = quant.sim.triarylamine.df,
  ggplot2::aes(x = time_s)
) +
ggplot2::geom_point(
  ggplot2::aes(y = N_evals),
  size = 3.2,
  color = "darkgreen"
) +
ggplot2::labs(
  x = plot_labels_xyz(time,s),
  y = bquote(italic(Number~~of~~Evaluations))
) + plot_theme_In_ticks()

#
#
## the same time series fitting, now with all
## EPR spectra and integrals,
## i.e. `output.area.stat = FALSE`
spectra.sim.triarylamine.df <-
 quantify_EPR_Sim_series(
    data.spectra.series =
      triarylamine.decay.series.data$df,
    # this is an `{eprscope}` pkg. dir:
    dir_ASC_sim = system.file(
      "extdata",
      package = "eprscope",
      mustWork = TRUE
    ),
    optim.method = "sbplx",
    # name pattern for `csv` EPR sim. data:
    name.pattern.sim =
      "Triarylamine_radCat_decay_9thSpec",
    col.names.sim = c(
      "Bsim_G",
      "B_G",
      "dIeprSim_over_dB",
      "dIepr_over_dB",
      "Norm_dIeprSim_over_dB"
    ),
    x.sim.id = 1,
    Intensity.sim.id = 3,
    optim.params.init = c(0,0.8),
    output.area.stat = FALSE,
    msg.optim.progress = FALSE
)
#
## data frame preview
spectra.sim.triarylamine.df %>% head(n = 10)
#> # A tibble: 10 × 7
#> # Groups:   time_s [1]
#>       B_G time_s dIepr_over_dB   B_mT dIeprSim_over_dB_A single_IntegSim_A
#>     <dbl>  <dbl>         <dbl>  <dbl>              <dbl>             <dbl>
#>  1 3390        6  0.000013629  339             1.6861e-8         0        
#>  2 3390.1      6 -0.0000010134 339.01          1.6904e-8         1.4068e-9
#>  3 3390.2      6 -0.000019795  339.02          1.6947e-8         2.8173e-9
#>  4 3390.3      6 -0.000029827  339.03          1.6990e-8         4.2313e-9
#>  5 3390.3      6 -0.000016871  339.03          1.7033e-8         5.6490e-9
#>  6 3390.4      6  0.0000025629 339.04          1.7077e-8         7.0703e-9
#>  7 3390.5      6  0.000011223  339.05          1.7121e-8         8.4952e-9
#>  8 3390.6      6  0.000014956  339.06          1.7165e-8         9.9238e-9
#>  9 3390.7      6 -0.000016646  339.07          1.7209e-8         1.1356e-8
#> 10 3390.8      6  0.0000082986 339.08          1.7253e-8         1.2792e-8
#> # ℹ 1 more variable: double_IntegSim_A <dbl>
#
## plot simulated SINGLE INTEGRATED EPR spectra
## (fitted, with optimized intensities)
plot_EPR_Specs(
  data.spectra = spectra.sim.triarylamine.df,
  x = "B_G",
  x.unit = "G",
  Intensity = "single_IntegSim_A",
  var2nd.series = "time_s",
  legend.title = "Time (s)",
  line.colors = c("darkred", "blue", "green"),
  lineSpecs.form = "integrated"
)

#
## plot simulated DOUBLE INTEGRATED EPR spectra
## ## (fitted, with optimized intensities)
plot_EPR_Specs(
  data.spectra = spectra.sim.triarylamine.df,
  x = "B_G",
  x.unit = "G",
  Intensity = "double_IntegSim_A",
  var2nd.series = "time_s",
  legend.title = "Time (s)",
  line.colors = c("darkred", "blue", "green"),
  lineSpecs.form = "integrated"
)


```
