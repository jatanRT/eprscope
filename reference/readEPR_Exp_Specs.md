# Reading of the Experimental ASCII or Binary EPR Spectra/Data.

Based on the [`fread`](https://rdrr.io/pkg/data.table/man/fread.html) or
[`readBin`](https://rdrr.io/r/base/readBin.html) R functions, the
experimental EPR/ENDOR spectra (referred to as 1D- or 2D-Experiments) or
other original (pre-processed) data from the EPR spectrometers are
transformed into data frames (tables). The function can read several
data formats such as `.txt`, `.csv`, `.asc`, `.DTA`, `.spc` as well as
`.YGF` with the latter three extensions, corresponding to binary files
(function automatically recognizes which type of data, ASCII or binary,
are loaded). Reading of such data requires information (instrumental
parameters of the acquired spectra/data) provided by the `.DSC`/`.dsc`
or `.par` files, respectively (see the `path_to_dsc_par` as well as
`path_to_ygf` arguments description). Because the original file
structure depends on the EPR spectrometer acquisition software or data
processing, the `origin` argument is necessary to specify the data/file
source. Default function arguments are pre-configured for
`origin ="xenon"`.

## Usage

``` r
readEPR_Exp_Specs(
  path_to_file,
  path_to_dsc_par = NULL,
  path_to_ygf = NULL,
  sep = "auto",
  skip = 1,
  header = FALSE,
  col.names = c("index", "B_G", "dIepr_over_dB"),
  x.id = 2,
  x.unit = "G",
  Intensity.id = 3,
  var2nd.series.id = NULL,
  convertB.unit = TRUE,
  qValue = NULL,
  norm.vec.add = NULL,
  origin = "xenon",
  data.structure = "spectra",
  ...
)
```

## Arguments

- path_to_file:

  Character string, path to any spectrometer/instrumental file, having
  one the following extensions: `.txt`, `.csv`, `.asc`, `.DTA` or
  `.spc`, including the 1D- (e.g. \\Intensity\\ vs \\B\\, Field) or
  2D-experimental (e.g. \\Intensity\\ vs \\B\\ vs \\time\\) EPR data.
  The path can be also defined by the
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

- sep:

  Character string. The separator between columns/variables in the
  original ASCII text file. **Default**: `sep = "auto"`, pointing to
  automatic recognition of the separator. If required, additional
  separators like `sep = "\t"` ("tab") or `sep = "\s+"` ("more white
  space") can be applied as well. Use `sep = NULL` or `sep = ""` to
  specify no separator. For any details, please consult the
  [`fread`](https://rdrr.io/pkg/data.table/man/fread.html)
  documentation.

- skip:

  Numeric value, referring to the number of rows, at the beginning of
  ASCII text file, to be skipped (not included by loading into the data
  frame). **Default**: `skip = 1`, corresponding to skip the first line
  in the `origin = "Xenon"` text file.

- header:

  Logical. Does the first data line, in the original ASCII file, contain
  column names? Defaults according to whether every non-empty field on
  the first data line is character type. If so, or TRUE is supplied, any
  empty column names are given by a default name. **Default**:
  `header = FALSE`.

- col.names:

  Character string vector, corresponding to desired column/variable
  names/headers of the returned data frame/table. A safe rule of thumb
  is to use column names incl. physical quantity notation with its unit,
  `Quantity_Unit` like `"B_G"`, `"RF_MHz"` or `"Bsim_mT"` (e.g. pointing
  to simulated EPR spectrum \\x\\-axis). **Default**:
  `col.names = c("index","B_G",dIepr_over_dB)`. For the spectral
  2D-series `col.names` must include character string (such as
  `"time_s"` or `"T_K"`) in order to identify the corresponding quantity
  for the series in the original file (please refer also to the
  `var2nd.series.id` and `Details`). Additional
  [`fread`](https://rdrr.io/pkg/data.table/man/fread.html) documentation
  may be helpful to read the ASCII text files.

- x.id:

  Numeric index related to `col.names` vector, pointing to the
  independent variable, which corresponds to \\x\\-axis in the spectra
  or other plots (e.g. \\B\\ or \\\nu\_{\text{RF}}\\). **Default**:
  `x.id = 2` (for *Xenon*).

- x.unit:

  Character string, corresponding to original `x` variable/column unit,
  such as `"G"`, `"mT"` or `"MHz"`.

- Intensity.id:

  Numeric index related to `col.names` vector, pointing to general
  intensity of EPR spectrum, like derivative intensity
  (`dIepr_over_dB`), integral one (e.g. `single_Integ`), double or
  sigmoid integral (e.g. `Area`)...etc. This corresponds to
  column/vector which should be presented on the \\y\\-axis in the EPR
  spectra or other plots. **Default**: `Intensity.id = 3` (for *Xenon*).

- var2nd.series.id:

  Numeric index related to `col.names` vector and pointing to column for
  the EPR spectral 2D-series variable like time, temperature or
  microwave power. If data contains simple relation like \\Area\\ vs
  \\time\\, use the `x` and `x.unit` parameters/arguments instead (see
  also `Examples`). This argument is dedicated to kinetic-like
  experiments. **Default**: `var2nd.series.id = NULL` (for 1D
  experiments), see also the `data.structure` argument.

- convertB.unit:

  Logical (**default**: `convertB.unit = TRUE`), whether upon reading,
  an automatic conversion between `G` and `mT` should be performed. If
  default is chosen, a new column/variable \\B\\ in `mT`/`G` is created,
  accordingly.

- qValue:

  Numeric, Q value (quality factor, number) displayed at specific `dB`
  by the spectrometer, in case of *Xenon* or *new Magnettech* software
  the parameter is included in `.DSC`/`.dsc` file, **default**:
  `qValue = NULL`, which actually corresponds to value `1`.

- norm.vec.add:

  Numeric vector. Additional (division) normalization constant(s) in the
  form of vector, including all (in addition to `qValue`)
  normalization(s) like concentration, powder sample weight, number of
  scans, ...etc. (e.g. `norm.vec.add = c(2000,0.5,2)`). **Default**:
  `norm.vec.add = NULL`, which actually corresponds to value(s) `1`. If
  `qValue = NULL`, the Q-factor/value might be also included in the
  `norm.vec.add`.

- origin:

  Character string, corresponding to **origin** of the data and related
  to EPR acquisition software at the spectrometer (from which the data
  are loaded automatically using the default parameters). Options are
  summarized in the following table (any other specific `origin` may be
  added later) =\>

  |  |  |
  |----|----|
  | **String** | **Description** |
  | "xenon" | **default** automatically loads data from the "Xenon" software with the default parameters. |
  | "winepr" | automatically loads data from the "WinEpr" software. |
  | "magnettech" | automatically loads data from the new "Magnettech" software (ESR5000 \[11-0422\]). |
  | "other" (arbitrary string, e.g. "csv") | general, loads any other original data like `csv`, `txt`, `asc` incl. also data from other instrumental/spectrometer software. **In such case, all the arguments for** `readEPR_Exp_Specs` **must be configured accordingly**. |

- data.structure:

  Character string, referring to structure of the ASCII data. Common
  spectral data files with \\Intensity\\ vs. \\x(B,g,RF(\text{MHz}))\\
  and/or \\time\\ columns (including spectral time series) correspond to
  `data.structure = "spectra"` (**default**). For the more complex ASCII
  data structure (such as spectral series processed by the acquisition
  spectrometer software, see `Examples`, or any other data) put
  `data.structure = "others"`. **In such case, all the arguments for**
  the `readEPR_Exp_Specs` **have to be set up accordingly**. The
  `data.structure` argument (assuming `var2nd.series.id = NULL`) is
  helping to simplify the reading of `"spectra"` by the predefined
  `origin` argument.

- ...:

  additional arguments specified (see also the
  [`fread`](https://rdrr.io/pkg/data.table/man/fread.html) function).

## Value

Data frame/table consisting of the magnetic flux density column `B_mT`
in millitesla (as well as `B_G` in gauss) or `RF_MHz` (in case of ENDOR
spectrum) or unitless `g-factor` and of the derivative intensity column
(`dIepr_over_dB`) or any other intensities (like integrated spectral
form) in `procedure defined unit` (see
[p.d.u.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6803776/)), which
is normalized by the above-described parameters/function arguments. For
2D experiments (spectral series) an additional column with the 2nd
independent variable (like time, temperature or microwave power, ...etc.
) is created. In such case the actual data frame is return in the
[tidy/long table
format](https://r4ds.hadley.nz/data-tidy.html#sec-tidy-data).

## Details

Right after the instrumental or pre-processed data/files are transformed
into data frames, they can be easily handled by the actual or additional
R packages, e.g. by [dplyr](https://dplyr.tidyverse.org/)), afterwards.
Spectral intensities are normalized by the common experimental
parameters like Q-factor, concentration, weight...etc. These are defined
by the two arguments: `qValue` and `norm.vec.add`. The latter actually
corresponds to values of the above-mentioned quantities represented by
the vector. If `qValue = NULL` (it actually equals to `1`), it can be
also defined as a component of the `norm.vec.add`. Finally, the
normalized (derivative) intensity is calculated by the following
expression (depending on the `qValue` and/or `norm.vec.add`):
\$\$dI\_{EPR} / dB = Original~Intensity \\ (1/qValue)\$\$ or
\$\$dI\_{EPR} / dB = Original~Intensity \\ (1/qValue) \\ \prod\_{k}
1/(norm.vec.add\[k\])\$\$ or \$\$dI\_{EPR} / dB = Original~Intensity \\
\prod\_{k} 1/(norm.vec.add\[k\])\$\$ where the \\k\\ is iterating
through all components of the `norm.vec.add`. The structure of files
depends on the origin/software used to acquire the EPR spectra. This is
mainly mirrored by the `origin` and `data.structure` arguments. Default
arguments are set to read the data from *Xenon* acquisition/processing
software. However, additional `origins` can be configured like
`origin = "winepr"` or `origin = "magnettech"` or even any arbitrary
string e.g. `origin = "csv"` (see also description of the `origin`
argument). For the latter, all arguments must be defined accordingly, as
already demonstrated in `Examples`. When reading the spectrometer files,
any 2D-experiment (e.g. time/temperature/microwave power series) can be
loaded as well. This must be activated by the `var2nd.series.id`
argument (which is `NULL` by default to load the 1D-experiments),
pointing to `col.names` element index in order to define relevant column
of the returned data frame. For example, if the second variable series
corresponds to time (in seconds) column: e.g.
`col.names = c("index","B_G","time_s","dIepr_over_dB")`, the `id` must
be defined as `var2nd.series.id = 3`.

## See also

Other Data Reading:
[`readEPR_Exp_Specs_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_kin.md),
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
## simple ASCII EPR spectrum acquired by "xenon"
## and with `B` conversion "G" <=> "mT"
## Loading the data
aminoxyl.ascii.data.path <-
  load_data_example(file = "Aminoxyl_radical_a.txt")
aminoxyl.data.01 <-
  readEPR_Exp_Specs(
    aminoxyl.ascii.data.path,
    qValue = 2100
  )
## preview
head(aminoxyl.data.01)
#>   index       B_G  dIepr_over_dB      B_mT
#> 1     1 3332.7000 -1.7738564e-06 333.27000
#> 2     2 3332.9005  1.2209461e-07 333.29005
#> 3     3 3333.1009 -1.8403788e-07 333.31009
#> 4     4 3333.3014 -7.1641412e-08 333.33014
#> 5     5 3333.5019 -1.7361444e-06 333.35019
#> 6     6 3333.7023 -5.1531169e-07 333.37023
#
## the same EPR spectrum data, reading from
## the original `.DTA` binary (+ `.DSC` text) file(s)
## Loading the data
aminoxyl.bin.data.path <-
  load_data_example(file = "Aminoxyl_radical_a.DTA")
aminoxyl.data.02 <-
  readEPR_Exp_Specs(
    aminoxyl.bin.data.path,
    qValue = 2100
  )
## preview
head(aminoxyl.data.02)
#>   index       B_G  dIepr_over_dB      B_mT
#> 1     1 3332.7000 -1.7738564e-06 333.27000
#> 2     2 3332.9005  1.2209461e-07 333.29005
#> 3     3 3333.1009 -1.8403788e-07 333.31009
#> 4     4 3333.3014 -7.1641412e-08 333.33014
#> 5     5 3333.5019 -1.7361444e-06 333.35019
#> 6     6 3333.7023 -5.1531169e-07 333.37023
#
# simple EPR spectrum acquired by "xenon"
## and without `B` conversion "G" <=> "mT"
aminoxyl.data.03 <-
  readEPR_Exp_Specs(aminoxyl.bin.data.path,
                    convertB.unit = FALSE,
                    qValue = 2100)
## preview
head(aminoxyl.data.03)
#>   index       B_G  dIepr_over_dB
#> 1     1 3332.7000 -1.7738564e-06
#> 2     2 3332.9005  1.2209461e-07
#> 3     3 3333.1009 -1.8403788e-07
#> 4     4 3333.3014 -7.1641412e-08
#> 5     5 3333.5019 -1.7361444e-06
#> 6     6 3333.7023 -5.1531169e-07
#
## the simple spectrum acquired by "winepr"
## (and 20 scans) on a 1 mM sample concentration:
## Loading the data
TMPD.ascii.data.path <-
  load_data_example(file = "TMPD_specelchem_accu_b.asc")
TMPD.data.01 <-
  readEPR_Exp_Specs(
    TMPD.ascii.data.path,
    col.names = c("B_G","dIepr_over_dB"),
    qValue = 3500,
    norm.vec.add = c(20,0.001),
    origin = "winepr"
  )
## preview
head(TMPD.data.01)
#>         B_G dIepr_over_dB      B_mT
#> 1 3439.1699 -2802.3680804 343.91699
#> 2 3439.2200 -1525.3253348 343.92200
#> 3 3439.2700 -1926.1109375 343.92700
#> 4 3439.3201     1.9604213 343.93201
#> 5 3439.3701 -4631.0111607 343.93701
#> 6 3439.4199 -3595.7537946 343.94199
#
## the same EPR spectrum data, reading from
## the original `.spc` binary (+ `.par` text) file(s)
## Loading the data
TMPD.bin.data.path <-
  load_data_example(file = "TMPD_specelchem_accu_b.spc")
TMPD.data.02 <-
  readEPR_Exp_Specs(
    path_to_file = TMPD.bin.data.path,
    col.names = c("B_G","dIepr_over_dB"),
    qValue = 3500,
    norm.vec.add = c(20,0.001),
    origin = "winepr"
  )
## preview
head(TMPD.data.02)
#>       B_G dIepr_over_dB    B_mT
#> 1 3439.17 -2802.3680804 343.917
#> 2 3439.22 -1525.3253348 343.922
#> 3 3439.27 -1926.1109375 343.927
#> 4 3439.32     1.9604213 343.932
#> 5 3439.37 -4631.0111607 343.937
#> 6 3439.42 -3595.7537946 343.942
#
## the ENDOR spectrum recorded by "xenon"
## and 8 accumulation sweeps (ASCII data)
## loading the data
PNT.ENDOR.ascii.data.path <-
  load_data_example(file = "PNT_ENDOR_a.txt")
PNT.ENDOR.data.01 <-
  readEPR_Exp_Specs(
    PNT.ENDOR.ascii.data.path,
    col.names = c("index",
                  "RF_MHz",
                  "dIepr_over_dB"),
    x.unit = "MHz",
    norm.vec.add = 8
  )
## preview
head(PNT.ENDOR.data.01)
#>   index    RF_MHz dIepr_over_dB
#> 1     1 2.0000000 0.00018947409
#> 2     2 2.0400400 0.00102584647
#> 3     3 2.0800801 0.00159380721
#> 4     4 2.1201201 0.00143519925
#> 5     5 2.1601602 0.00215950297
#> 6     6 2.2002002 0.00125284480
#
## previous ENDOR spectrum data, reading from
## the original `.DTA` binary (+ `.DSC` text) file(s)
## Loading the data
PNT.ENDOR.bin.data.path <-
  load_data_example(file = "PNT_ENDOR_a.DTA")
PNT.ENDOR.data.02 <-
  readEPR_Exp_Specs(
    PNT.ENDOR.bin.data.path,
    col.names = c("index",
                  "RF_MHz",
                  "dIepr_over_dB"),
    x.unit = "MHz",
    norm.vec.add = 8
  )
## preview
head(PNT.ENDOR.data.02)
#>   index    RF_MHz dIepr_over_dB
#> 1     1 2.0000000 0.00018947409
#> 2     2 2.0400400 0.00102584647
#> 3     3 2.0800801 0.00159380721
#> 4     4 2.1201201 0.00143519925
#> 5     5 2.1601602 0.00215950297
#> 6     6 2.2002002 0.00125284480
#
## reading the (pre-processed) ASCII
## data file (data.structure = "others") from (by)
## the "Xenon" software corresponding to kinetics with
## `Area` and `time` columns/variables , these
## two have to be selected from several
## others + normalize `Area` by the `qValue`
## (first of all load the path of package example file)
triarylamine.rc.decay.path <-
  load_data_example("Triarylamine_radCat_decay_a.txt")
## data
triarylamine.rc.decay.data <-
  readEPR_Exp_Specs(path_to_file = triarylamine.rc.decay.path,
                    header = TRUE,
                    fill = TRUE,
                    select = c(3,7),
                    col.names = c("time_s","Area"),
                    x.unit = "s",
                    x.id = 1,
                    Intensity.id = 2,
                    qValue = 1700,
                    data.structure = "others") %>%
    na.omit()
## preview
head(triarylamine.rc.decay.data)
#>   time_s        Area
#> 1   0.00 0.018741176
#> 2  15.17 0.018117647
#> 3  30.01 0.017617647
#> 4  44.82 0.017194118
#> 5  59.66 0.016511765
#> 6  74.52 0.016347059
#
## reading the "magnettech" file example,
## first of all load the package example file
acridineRad.data.path <-
  load_data_example("AcridineDeriv_Irrad_365nm.csv.zip")
## unzip
acridineRad.data <-
  unzip(acridineRad.data.path,
        files = c("AcridineDeriv_Irrad_365nm.csv"),
        exdir = tempdir())
## reading "magnettech"
acridineRad.data.01 <-
  readEPR_Exp_Specs(acridineRad.data,
                    col.names = c("B_mT","dIepr_over_dB"),
                    x.unit = "mT",
                    qValue = 1829,
                    origin = "magnettech")
## preview
head(acridineRad.data.01)
#>       B_mT dIepr_over_dB      B_G
#> 1 325.0000 -0.0052751586 3250.000
#> 2 325.0005 -0.0052804348 3250.005
#> 3 325.0010 -0.0052857110 3250.010
#> 4 325.0015 -0.0052909872 3250.015
#> 5 325.0020 -0.0052962635 3250.020
#> 6 325.0025 -0.0053014848 3250.025
#
## previous acridine EPR spectrum data, reading from
## the original `.DTA` binary (+ `.dsc` text) file(s)
## Loading the data
acridineRad.bin.data.path <-
  load_data_example("AcridineDeriv_Irrad_365nm.DTA")
acridineRad.data.02 <-
  readEPR_Exp_Specs(
    path_to_file = acridineRad.bin.data.path,
    origin = "magnetech",
    qValue = 1829
  )
#
## preview
head(acridineRad.data.02)
#>   index      B_G  dIepr_over_dB     B_mT
#> 1     1 3250.000 -0.00029514466 325.0000
#> 2     2 3250.005 -0.00030042089 325.0005
#> 3     3 3250.010 -0.00030569711 325.0010
#> 4     4 3250.015 -0.00031097333 325.0015
#> 5     5 3250.020 -0.00031624955 325.0020
#> 6     6 3250.025 -0.00032147089 325.0025
#
## EPR time series (2D Experiment) acquired
## by the "Winepr"/"WinEpr", reading the original
## `.spc` binary (+ `.par` text) file(s)
## Loading the data
TMPD.se.bin.data.2D.path <-
  load_data_example("TMPD_specelchem_CV_b.spc")
TMPD.se.data.2D <-
  readEPR_Exp_Specs(
    path_to_file = TMPD.se.bin.data.2D.path,
    col.names = c(
      "B_G",
      "Slice",
      "dIepr_over_dB"
    ),
    var2nd.series.id = 2,
    origin = "winepr"
  )
#
## preview
head(TMPD.se.data.2D)
#>       B_G Slice dIepr_over_dB    B_mT
#> 1 3449.17     0    -19928.596 344.917
#> 2 3449.22     0    -16053.596 344.922
#> 3 3449.27     0     56915.406 344.927
#> 4 3449.32     0     24445.404 344.932
#> 5 3449.37     0    -56462.594 344.937
#> 6 3449.42     0     12250.404 344.942
#
## EPR time series (2D experiment) acquired
## by the "Xenon", reading the original
## `.DTA` binary (+ `.DSC` text) file(s)
## Loading the data
triarylamine.rc.decay.series.path <-
  load_data_example("Triarylamine_radCat_decay_series.DTA")
triarylamine.rc.decay.series.data <-
  readEPR_Exp_Specs(
    path_to_file = triarylamine.rc.decay.series.path,
    col.names = c(
      "index",
      "B_G",
      "time_s",
      "dIepr_over_dB"
    ),
    var2nd.series.id = 3,
    qValue = 1700
  )
#
## preview
head(triarylamine.rc.decay.series.data)
#>   index       B_G time_s  dIepr_over_dB      B_mT
#> 1     1 3390.0000      0  1.3629316e-05 339.00000
#> 2     2 3390.0833      0 -1.0134169e-06 339.00833
#> 3     3 3390.1667      0 -1.9794802e-05 339.01667
#> 4     4 3390.2500      0 -2.9826537e-05 339.02500
#> 5     5 3390.3333      0 -1.6870754e-05 339.03333
#> 6     6 3390.4167      0  2.5629187e-06 339.04167
#
## reading of the individual `.YGF` file (Xenon)
## into vector
tam.rc.decay.series.ygf.path <-
  load_data_example("Triarylamine_radCat_decay_series.YGF")
#
## file length
tam.rc.decay.series.ygf.len <-
  readBin(
    con = tam.rc.decay.series.ygf.path,
    what = "raw",
    n = 1e+4
  ) %>% length()
#
## `.YGF` file reading
tam.rc.decay.series.ygf <-
  readBin(
    con = tam.rc.decay.series.ygf.path,
    what = "numeric",
    size = 8, ## Xenon
    n = tam.rc.decay.series.ygf.len / 8, ## Xenon
    signed = TRUE,
    endian = "big" ## Xenon
  )
#
## preview of the first 10 values (time in s)
tam.rc.decay.series.ygf[1:10]
#>  [1]   0.00  15.17  30.01  44.82  59.66  74.52  89.36 104.19 119.00 133.82
#
if (FALSE) { # \dontrun{
## read the `.csv` file which is an output
## from the online converter:
## https://www.spectra.tools/bin/controller.pl?body=Xepr2gfac
readEPR_Exp_Specs("data.csv",
                  skip = 0,
                  col.names = c("B_G",
                                "g_Value",
                                "dIepr_over_dB"),
                  x.id = 1,
                  Intensity.id = 3,
                  origin = "csv")
} # }

```
