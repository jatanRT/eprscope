# Read and Process CW EPR Time Series Experiments

Reading the continuous wave (CW) EPR time series spectral data (recorded
by e.g. `2D_Field_Delay` experiment in "Xenon" acquisition/processing
software). Function is based on the
[`readEPR_Exp_Specs`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md)
and includes automatic time correction for CW EPR `time.series`
experiments (see also the
[`correct_time_Exp_Specs`](https://jatanrt.github.io/eprscope/reference/correct_time_Exp_Specs.md)
description and documentation). If the time series EPR spectra are
stored individually (one file per one spectrum, e.g. for
`origin = "Magnettech"`, ESR5000 \[11-0422\]), such time series needs to
be loaded by
[`readEPR_Exp_Specs_multif`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_multif.md).
For `origin = "WinEpr"` the `time_s` column usually possesses the form
of spectrum slices, i.e. an integer number (0,1,2,...) is assigned to
each recorded spectrum. Therefore, for a radical kinetic analysis it has
to be converted to `time_s` (see the argument `time.delta.slice`).

## Usage

``` r
readEPR_Exp_Specs_kin(
  path_to_file,
  path_to_dsc_par = NULL,
  path_to_ygf = NULL,
  time.unit = "s",
  time.delta.slice = NULL,
  col.names = c("index", "B_G", "time_s", "dIepr_over_dB"),
  x.unit = "G",
  var2nd.series.id = 3,
  origin = "xenon",
  qValue = NULL,
  ...
)
```

## Arguments

- path_to_file:

  Character string, path to any spectrometer/instrumental file, having
  one the following extensions: `.txt`, `.csv`, `.asc`, `.DTA` or
  `.spc`, including the 2D-experimental (i.e. \\Intensity\\ vs \\B\\ vs
  \\time\\) EPR data. The path can be also defined by the
  [`file.path`](https://rdrr.io/r/base/file.path.html) function.

- path_to_dsc_par:

  Character string, path (can be also provided by the
  [`file.path`](https://rdrr.io/r/base/file.path.html)) to `.DSC/.dsc`
  (`origin = "xenon"`/`origin = "magnettech"`) or `.par`
  (`origin = "winepr"`) ASCII `text` file, including instrumental
  parameters of the recorded spectra provided by the EPR machine.
  **Default**: `path_to_dsc_par = NULL`. The latter assignment actually
  means that the argument automatically inherits the `path_to_file`,
  however with the appropriate extension (`.DSC/.dsc` or `.par`). In
  other words, the function is looking for the same filename like the
  `path_to_file` in the working directory. If the file does not exist,
  it will ask to provide/define the right file path.

- path_to_ygf:

  Character string, path (can be also provided by the
  [`file.path`](https://rdrr.io/r/base/file.path.html)) to binary `.YGF`
  file (`origin = "xenon"`/`origin = "magnettech"`), storing the values
  of the 2nd independent variable in the spectral series like time,
  temperature, microwave power, ...etc (different from magnetic flux
  density and EPR intensity, see also `Details` and/or the
  `var2nd.series.id` argument description for 2D experiments).
  **Default**: `path_to_ygf = NULL`. The latter assignment actually
  means that the argument automatically inherits the `path_to_file`,
  however with the appropriate extension `.YGF`. In other words, the
  function is looking for the same file name like the `path_to_file` in
  the working directory. If the file does not exist, it automatically
  grabs those values based on the information provided by the
  `.DSC/.dsc` (`origin = "xenon"`/`origin = "magnettech"`) or `.par`
  (`origin = "winepr"`) files (see the argument `path_to_dsc_par`
  description). In order to read individual `.YGF` files just apply the
  [`readBin`](https://rdrr.io/r/base/readBin.html) function with the
  following arguments `con = path_to_file` ,`what = "numeric"`,
  `size = 8`, `n = file_length / 8`, `signed = TRUE`,
  `endian = "big"`/`endian = "little"`, the latter depending on the
  origin xenon/magnettech, respectively. The `file_length` can be
  calculated by
  `readBin(con = path_to_file,what = "raw",n = 1e+4) %>% length()`.

- time.unit:

  Character string, specifying the `"s"`,`"min"`, `"h"` or
  `time.unit = "unitless"` (if `time.delta.slice` is different from
  `NULL`). **Default**: `time.unit = "s"`

- time.delta.slice:

  Numeric, time interval in `time.unit` between `slices`, in the case of
  `origin = "winepr"`. **Default**: `time.delta.slice = NULL` (actually,
  corresponding to `1 time.unit`).

- col.names:

  Character string vector, corresponding to desired column/variable
  names/headers of the returned data frame/table. A safe rule of thumb
  is to use column names incl. physical quantity notation with its unit,
  `Quantity_Unit` like `"B_G"`, `"RF_MHz"` or `"Bsim_mT"` (e.g. pointing
  to simulated EPR spectrum \\x\\-axis). **Default**:
  `col.names = c("index","B_G","time_s","dIepr_over_dB")` (if
  `orgin = "xenon"`). For the **spectral 2D-time series** `col.names`
  **must include character string** (**such as** `"time_s"`) **in order
  to identify the time at which the EPR spectra were recorded**.

- x.unit:

  Character string, corresponding to original `x` variable/column unit,
  such as `"G"`, `"mT"` or `"MHz"`.

- var2nd.series.id:

  Numeric index related to `col.names` vector and pointing to column for
  the EPR spectral 2D- time series variable. **Default**:
  `var2nd.series.id = 3` (for "Xenon" time series experiment,
  corresponding to "time" column).

- origin:

  Character string, corresponding to **origin** of the data and related
  to EPR acquisition software at the spectrometer (from which the data
  are loaded automatically using the default parameters). Options are
  summarized in the following table (any other specific `origin` may be
  added later) =\>

  |                                        |                                                                                                                                                                                                                                |
  |----------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
  | **String**                             | **Description**                                                                                                                                                                                                                |
  | "xenon"                                | **default** automatically loads data from the "Xenon" software with the default parameters.                                                                                                                                    |
  | "winepr"                               | automatically loads data from the "WinEpr" software.                                                                                                                                                                           |
  | "magnettech"                           | automatically loads data from the new "Magnettech" software (ESR5000 \[11-0422\]).                                                                                                                                             |
  | "other" (arbitrary string, e.g. "csv") | general, loads any other original data like `csv`, `txt`, `asc` incl. also data from other instrumental/spectrometer software. **In such case, all the arguments for** `readEPR_Exp_Specs` **must be configured accordingly**. |

- qValue:

  Numeric, Q value (quality or sensitivity factor number) displayed at
  specific `dB` by the spectrometer, in case of *Xenon* or *new
  Magnettech* software it is returned automatically, however in the case
  of *WinEPR*, it must be provided by the user. **default**:
  `qValue = NULL`, actually corresponding to value `1`.

- ...:

  additional arguments specified, see also the
  [`readEPR_Exp_Specs`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md)
  and [`fread`](https://rdrr.io/pkg/data.table/man/fread.html).

## Value

List of EPR spectrum time series data in tidy long table format (`df`) +
corrected time vector (`time`). For the `origon = "winepr"` "time"
slices/indices must be already converted into time domain by
`time.delta.slice` (see arguments and examples).

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
triarylam.decay.series.dta.path <-
  load_data_example(file =
    "Triarylamine_radCat_decay_series.DTA")
#
## loading the kinetics:
triarylam.decay.series.data <-
  readEPR_Exp_Specs_kin(
    path_to_file = triarylam.decay.series.dta.path
 )
#
## data preview
head(triarylam.decay.series.data$df)
#>   index       B_G time_s  dIepr_over_dB      B_mT
#> 1     1 3390.0000      6  1.3629316e-05 339.00000
#> 2     2 3390.0833      6 -1.0134169e-06 339.00833
#> 3     3 3390.1667      6 -1.9794802e-05 339.01667
#> 4     4 3390.2500      6 -2.9826537e-05 339.02500
#> 5     5 3390.3333      6 -1.6870754e-05 339.03333
#> 6     6 3390.4167      6  2.5629187e-06 339.04167
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
## reading the EPR time series
## returned by the "WinEPR" software
tmpd.se.cv.b.bin.path <-
  load_data_example("TMPD_specelchem_CV_b.spc")
tmpd.se.cv.b.dat <-
  readEPR_Exp_Specs_kin(
    path_to_file = tmpd.se.cv.b.bin.path,
    col.names = c(
      "B_G",
      "Slice",
      "dIepr_over_dB"
     ),
  var2nd.series.id = 2,
  time.delta.slice = 18, # 18 seconds
  origin = "winepr"
  )
#> Warning: Sensitivity factor or Q-Value WAS NOT DEFINED !!
#> 
#>               It is automatically switched to `1`, unless you define
#> 
#>               the `qValue` argument !! 
#
## data preview
head(tmpd.se.cv.b.dat$df)
#>       B_G time_s dIepr_over_dB    B_mT
#> 1 3449.17      8    -19928.596 344.917
#> 2 3449.22      8    -16053.596 344.922
#> 3 3449.27      8     56915.406 344.927
#> 4 3449.32      8     24445.404 344.932
#> 5 3449.37      8    -56462.594 344.937
#> 6 3449.42      8     12250.404 344.942
#
## time preview
tmpd.se.cv.b.dat$time
#>  [1]   8  26  44  62  80  98 116 134 152 170 188 206 224 242 260 278 296 314 332
#> [20] 350 368

```
