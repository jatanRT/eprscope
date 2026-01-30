# Read and Process Spectral Data of Time Dependent CW EPR Experiments

Reading the continuous wave (CW) EPR time series spectral data (recorded
by e.g. `2D_Field_Delay` experiment in "Xenon" acquisition/processing
software). Function (based on
[`readEPR_Exp_Specs`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md))
includes automatic time correction for CW EPR `time.series` experiments
(see also
[`correct_time_Exp_Specs`](https://jatanrt.github.io/eprscope/reference/correct_time_Exp_Specs.md)).

## Usage

``` r
readEPR_Exp_Specs_kin(
  name.root,
  dir_ASC,
  dir_dsc_par,
  time.unit = "s",
  time.delta.slice.s = NULL,
  col.names = c("index", "B_G", "time_s", "dIepr_over_dB"),
  x.unit = "G",
  convertB.unit = TRUE,
  qValue = NULL,
  norm.vec.add = NULL,
  origin = "xenon",
  ...
)
```

## Arguments

- name.root:

  Character string, corresponding to entire **file name without
  extension**.

- dir_ASC:

  Character string, path (can be also defined by the
  [`file.path`](https://rdrr.io/r/base/file.path.html)) to directory
  where the `ASCII` spectral data is stored.

- dir_dsc_par:

  Character string, path (can be also defined by the
  [`file.path`](https://rdrr.io/r/base/file.path.html)) to directory
  where the `.DSC`/`.dsc` or `.par` parameter file is stored (in order
  to calculate \\g\\-value and/or normalize intensities).

- time.unit:

  Character string, specifying the `"s"`,`"min"`, `"h"` or
  `time.unit = "unitless"` (if `time.delta.slice.s` is different from
  `NULL`). **Default**: `time.unit = "s"`

- time.delta.slice.s:

  Numeric, time interval in seconds between `slices`, in the case if
  `origin = "winepr"`. **Default**: `time.delta.slice = NULL` (actually,
  corresponding to `1 s`).

- col.names:

  Character/String vector inherited from
  [`fread`](https://rdrr.io/pkg/data.table/man/fread.html),
  corresponding to column/variable names. A safe rule of thumb is to use
  column names incl. physical quantity notation with its units,
  `Quantity_Unit` like `"B_G"`, `"RF_MHz"`, `"Bsim_mT"` (e.g. pointing
  to simulated EPR spectrum \\x\\-axis)...etc, **default**:
  `col.names = c("index","B_G","time_s","dIepr_over_dB")` (for
  `origin = "xenon"`).

- x.unit:

  Character string, corresponding to original `x` variable/column unit,
  such as `"G"`, `"mT"` or `"MHz"`.

- convertB.unit:

  Logical (**default**: `convertB.unit = TRUE`), whether upon reading,
  an automatic conversion between `G` and `mT` should be performed. If
  default is chosen, a new column/variable \\B\\ in `mT`/`G` is created.

- qValue:

  Numeric, Q value (quality factor, number) displayed at specific `dB`
  by spectrometer. In case of "Xenon" software the parameter is included
  in `.DSC` file, therefore **default**: `qValue = NULL` (actually
  corresponding to value `1`). If EPR spectra were acquired by the
  "Winepr" software, the Q value must be defined like `qValue = 3400`.

- norm.vec.add:

  Numeric vector, additional normalization constant in the form of
  vector, involving all (in addition to `qValue`) normalization(s) such
  as concentration, powder sample weight, number of scans, ...etc (e.g.
  `norm.vec.add = c(2000,0.5,2)`). **Default**: `norm.vec.add = NULL`
  (actually corresponding to value `1`).

- origin:

  Character string, corresponding to **origin** of the ASCII data, like
  from the most common spectrometers (from which the data are loaded
  automatically using the default parameters). Options are summarized in
  the following table (any other specific `origin` may be added later)
  =\>

  |                                        |                                                                                                                                                                                                                               |
  |----------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
  | **String**                             | **Description**                                                                                                                                                                                                               |
  | "xenon"                                | **default** automatically loads data from the "Xenon" software with the default parameters.                                                                                                                                   |
  | "winepr"                               | automatically loads data from the "WinEpr" software.                                                                                                                                                                          |
  | "magnettech"                           | automatically loads data from the new "Magnettech" software (ESR5000 \[11-0422\]).                                                                                                                                            |
  | "other" (arbitrary string, e.g. "csv") | general, loads any other original data like `csv`, `txt`, `asc` incl. also data from other instrumental/spectrometer software. **In such case, all the arguments for** `readEPR_Exp_Specs` **have to be set up accordingly**. |

- ...:

  additional arguments specified, see also the
  [`readEPR_Exp_Specs`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md)
  and [`fread`](https://rdrr.io/pkg/data.table/man/fread.html).

## Value

List of EPR spectrum data (including time) in tidy long table format
(`df`) + corrected time vector (`time`). For the `origon = "winepr"`
"time" slices/indices must be already converted into time domain by
`time.delta.slice.s` (see arguments and examples).

## See also

Other Data Reading:
[`readEPR_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md),
[`readEPR_Exp_Specs_multif()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_multif.md),
[`readEPR_Sim_Spec()`](https://jatanrt.github.io/eprscope/reference/readEPR_Sim_Spec.md),
[`readEPR_param_slct()`](https://jatanrt.github.io/eprscope/reference/readEPR_param_slct.md),
[`readEPR_params_slct_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_kin.md),
[`readEPR_params_slct_quant()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_quant.md),
[`readEPR_params_slct_sim()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_sim.md),
[`readEPR_params_tabs()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_tabs.md),
[`readEPR_solvent_props()`](https://jatanrt.github.io/eprscope/reference/readEPR_solvent_props.md),
[`readMAT_params_file()`](https://jatanrt.github.io/eprscope/reference/readMAT_params_file.md)

## Examples

``` r
## loading the built-in package example to demonstrate
## the reading of time series EPR spectra/kinetics:
triarylam.decay.series.dsc.path <-
  load_data_example(file = "Triarylamine_radCat_decay_series.DSC")
triarylam.decay.series.asc.path <-
  load_data_example(file = "Triarylamine_radCat_decay_series.zip")
unzip(triarylam.decay.series.asc.path,exdir = tempdir())
#
## loading the kinetics:
triarylam.decay.series.data <-
  readEPR_Exp_Specs_kin(name.root = "Triarylamine_radCat_decay_series",
                        dir_ASC = tempdir(),
                        dir_dsc_par =
                          system.file("extdata",
                                      package = "eprscope")
                       )
#
## data preview
head(triarylam.decay.series.data$df)
#>    index       B_G time_s  dIepr_over_dB      B_mT
#>    <int>     <num>  <num>          <num>     <num>
#> 1:     1 3390.0000      6  1.3629316e-05 339.00000
#> 2:     2 3390.0833      6 -1.0134169e-06 339.00833
#> 3:     3 3390.1667      6 -1.9794802e-05 339.01667
#> 4:     4 3390.2500      6 -2.9826537e-05 339.02500
#> 5:     5 3390.3333      6 -1.6870754e-05 339.03333
#> 6:     6 3390.4167      6  2.5629187e-06 339.04167
#
## preview of corrected time vector
## (the uncorrected one actually starts from `0`)
triarylam.decay.series.data$time
#>   [1]    6   21   36   51   66   81   95  110  125  140  155  170  184  199  214
#>  [16]  229  244  259  274  288  303  318  333  348  363  378  392  407  422  437
#>  [31]  452  467  482  496  511  526  541  556  571  586  600  615  630  645  660
#>  [46]  675  690  705  719  734  749  764  779  794  809  824  838  853  868  883
#>  [61]  898  913  927  942  957  972  987 1002 1016 1031 1046 1061 1076 1091 1106
#>  [76] 1120 1135 1150 1165 1180 1195 1210 1225 1239 1254 1269 1284 1299 1314 1329
#>  [91] 1344 1358 1373 1388 1403 1418 1433 1448 1463 1477
#
if (FALSE) { # \dontrun{
## reading by the "WinEPR" software
readEPR_Exp_Specs_kin("Sample_spectra_irradiation",
                      file.path(".","ASCII_data_dir"),
                      file.path(".","dsc_data_dir"),
                      time.unit = "s",
                      time.delta.slice.s = 24.1,
                      col.names = c("B_G",
                                    "Slice",
                                    "Intensity"),
                      x.unit = "G",
                      qValue = 2900,
                      origin = "winepr")

} # }

```
