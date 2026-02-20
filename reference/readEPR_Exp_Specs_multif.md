# Load Several/Multiple EPR Data/Spectra Files Simultaneously

Loading the EPR spectra from several/multiple `text` files (including
the instrumental parameters in `.DSC`/`.dsc` or `.par` format) at once.
Finally, the data are transformed either into a list of data frames or
into a [tidy/long table format](https://r4ds.had.co.nz/tidy-data.html).
According to experimental quantity (e.g. temperature, microwave power,
recording time...etc), `names` and `var2nd.series` (in the case of
`tidy = TRUE`) arguments have to be specified.

## Usage

``` r
readEPR_Exp_Specs_multif(
  name.pattern,
  dir_ASC,
  dir_dsc_par,
  col.names = c("index", "B_G", "dIepr_over_dB"),
  x.unit = "G",
  convertB.unit = TRUE,
  qValues = NULL,
  norm.list.add = NULL,
  names,
  tidy = FALSE,
  var2nd.series = NULL,
  var2nd.series.factor = FALSE,
  origin = "xenon",
  ...
)
```

## Arguments

- name.pattern:

  Character string ('specimen'), inherited from
  [`list.files`](https://rdrr.io/r/base/list.files.html). A pattern from
  file name which might not necessarily appear at the beginning of the
  file name. One might also consult how to [use regular expressions in
  R](https://r4ds.hadley.nz/regexps). THE SAME NAME AND `name.pattern`
  MUST BE USED FOR ALL FILE NAMES WITHIN THE SERIES.

- dir_ASC:

  Path (defined by [`file.path`](https://rdrr.io/r/base/file.path.html)
  or by character string) to directory where the `ASCII` files are
  stored.

- dir_dsc_par:

  Path (defined by [`file.path`](https://rdrr.io/r/base/file.path.html)
  or by character string) to directory where the `.DSC`/`.dsc` or `.par`
  files,including instrumental parameters, are stored.

- col.names:

  Character string vector, corresponding to column/variable names **for
  individual file** (see also
  [`readEPR_Exp_Specs`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md)).
  A safe rule of thumb is to use column names including the physical
  quantity notation with its units, `Quantity_Unit` like `"B_G"`,
  `"RF_MHz"`, `"Bsim_mT"` (e.g. pointing to simulated EPR spectrum
  \\x\\-axis)...etc, **default**:
  `col.names = c("index","B_G",dIepr_over_dB)` referring to column names
  coming from *Xenon* software.

- x.unit:

  Character string pointing to unit of quantity (coming from the
  original ASCII data, see also `col.names` argument) which is to be
  presented on \\x\\-axis of the EPR spectrum. Units like `"G"`
  ("Gauss"), `"mT"` ("millitesla"), `"MHz"` ("megahertz" in case of
  ENDOR spectra) or `"Unitless"` in case of \\g\\-values can be used.
  **Default**: `x.unit = "G"`.

- convertB.unit:

  Logical (**default**: `convertB.unit = TRUE`), whether upon reading,
  an automatic conversion between `G` and `mT` should be performed. If
  default is chosen, a new column/variable \\B\\ in `mT`/`G` is created.

- qValues:

  Numeric vector of Q-values (sensitivity factors to normalize EPR
  intensities) either automatically loaded from the instrumental
  parameter files (`.DSC` or `.par`) or specified by the user.
  Therefore, `qValues = NULL` (**default**), actually corresponding to
  vector with all elements equal to `1`. In case of the
  `origin = "winepr"`, the elements of `qValues` have to be provided by
  the spectrometer operator.

- norm.list.add:

  Numeric list of vectors. Additional (division) normalization constants
  in the form of vectors, involving all (i.e. in addition to `qValue`)
  normalization(s) like e.g. concentration, powder sample weight, number
  of scans, ...etc (e.g.
  `norm.list.add = list(c(2000,0.5,2),c(1500,1,3))`). **Default**:
  `norm.list.add = NULL`, corresponding to values of `1`.

- names:

  Character string vector, corresponding either to values of
  **additional quantity** (e.g. temperature,microwave power...etc,
  `c("240","250","260","270")`) or to **general sample coding** by alpha
  character (e.g. `c("a","b","c","d")`) being varied by the individual
  experiments.

- tidy:

  Logical, whether to transform the list of data frames into the long
  table (`tidy`) format, **default**: `tidy = FALSE`.

- var2nd.series:

  Character string, if `tidy = TRUE` (see also the `tidy` argument) it
  is referred to name of the variable/quantity (such as
  "time","Temperature","Electrochemical Potential", "Microwave
  Power"...etc) altered during individual experiments as a second
  variable series (`var2nd.series`) and related to the spectral data.

- var2nd.series.factor:

  Logical, whether to factorize `var2nd.series` column vector which is
  useful for plotting the spectra in overlay form. **Default**:
  `var2nd.series.factor = FALSE`, which is the case to visualize EPR
  spectra by `plot`-functions.

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

  additional arguments specified, see
  also[`readEPR_Exp_Specs`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md)
  and [`fread`](https://rdrr.io/pkg/data.table/man/fread.html).

## Value

List of Data Frames (or long table `tidy` format) corresponding to
multiple spectral data files/data sets. g-Value column (if
`x.unit = "mT"` or `"G"`) is automatically calculated during the
processing and it is included in the data frame list/database as well.

## See also

Other Data Reading:
[`readEPR_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md),
[`readEPR_Exp_Specs_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_kin.md),
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
if (FALSE) { # \dontrun{
## multiple ENDOR spectra at different temperatures recorded
## by `Xenon` software read and transformed into `longtable`,
## ready to plot the overlay
## EPR spectra => `var2nd.series.factor = FALSE` (default).
readEPR_Exp_Specs_multif(
  name.pattern = "^.*_sample_VT_",
  file.path(".","ASCII_data_dir"),
  file.path(".","DSC_data_dir"),
  col.names = c("index",
                "RF_MHz",
                "Intensity"),
  x.unit = "MHz",
  names = c("210","220","230","240"),
  tidy = TRUE,
  var2nd.series = "Temperature_K"
  )
#
## multiple EPR spectra recorded at different temperatures
## by `WinEPR` software, experiments performed with a powder
## sample (m = 10 mg) and each spectrum acquired
## as 7 accumulations, the resulting database
## corresponds to list of data frames
readEPR_Exp_Specs_multif(
  "^Sample_VT_",
  file.path(".","ASCII_data_dir"),
  file.path(".","DSC_data_dir"),
  col.names = c("B_G","dIepr_over_dB"),
  x.unit = "G",
  names = c("210","220","230","240"),
  qValues = c(3400,3501,3600,2800),
  norm.list.add = rep(list(c(10,7)),times = 4),
  origin = "winepr"
  )
#
## multiple `Xenon` EPR spectra related to one powder
## sample (m = 8 mg) where several instrumental parameters
## are changed at once, the file names (files are stored
## in the actual directory) start with the "R5228_AV_powder_",
## function returns all spectral data in `tidy` (long) table
## format
readEPR_Exp_Specs_multif(
  name.pattern = "R5228_AV_powder_",
  dir_ASC = ".",
  dir_dsc_par = ".",
  names = c("a","b","c","d"),
  tidy = TRUE,
  var2nd.series = "sample",
  norm.list.add = rep(list(8),4)
  )
#
## multiple `Magnettech` EPR spectra (in `.csv` ASCII format)
## recorded at different times
readEPR_Exp_Specs_multif(
  name.pattern = "Kinetics_Magnettech_",
  dir_ASC = ".",
  dir_dsc_par = ".",
  col.names = c("B_mT","dIepr_over_dB"),
  x.unit = "mT",
  names = c("20","30","40","50","60"), ## in seconds
  tidy = TRUE,
  var2nd.series = "time_s",
  qValues = c(1829,1922.325,1891.25,1835,1850.327),
  origin = "magnetech"
)
} # }

```
