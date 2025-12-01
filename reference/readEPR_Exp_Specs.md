# Read the Experimental ASCII or other Text-Based EPR Data.

This function is based on the
[`fread`](https://rdatatable.gitlab.io/data.table/reference/fread.html)
with the purpose to read the experimental EPR/ENDOR spectra or other
original (pre-processed) data, from the EPR spectrometers, in tabular
ASCII format (such as `.txt`, `.csv` or `.asc`). Default argument values
correspond to data reading from *Xenon* files (see the argument
`origin`).

## Usage

``` r
readEPR_Exp_Specs(
  path_to_ASC,
  sep = "auto",
  skip = 1,
  header = FALSE,
  col.names = c("index", "B_G", "dIepr_over_dB"),
  x.id = 2,
  x.unit = "G",
  Intensity.id = 3,
  time.series.id = NULL,
  convertB.unit = TRUE,
  qValue = NULL,
  norm.vec.add = NULL,
  origin = "xenon",
  data.structure = "spectra",
  ...
)
```

## Arguments

- path_to_ASC:

  Character string, path to ASCII file/table (e.g. in `.txt`, `.csv` or
  `.asc` format) with the spectral data (\\Intensity\\ vs \\B\\, Field)
  including additional `index` and/or `time` variables). The path can be
  also defined by the
  [`file.path`](https://rdrr.io/r/base/file.path.html) function.

- sep:

  The separator between columns. Defaults to the character in the set
  `[,\t |;:]` that separates the sample of rows into the most number of
  lines with the same number of fields. Use `NULL` or `""` to specify no
  separator; i.e. each line a single character column like
  [`base::readLines`](https://rdrr.io/r/base/readLines.html) does.

- skip:

  If 0 (default) start on the first line and from there finds the first
  row with a consistent number of columns. This automatically avoids
  irregular header information before the column names row. `skip>0`
  means ignore the first `skip` rows manually. `skip="string"` searches
  for `"string"` in the file (e.g. a substring of the column names row)
  and starts on that line (inspired by read.xls in package gdata).

- header:

  Does the first data line contain column names? Defaults according to
  whether every non-empty field on the first data line is type
  character. If so, or TRUE is supplied, any empty column names are
  given a default name.

- col.names:

  Character string vector, inherited from the
  [`fread`](https://rdatatable.gitlab.io/data.table/reference/fread.html),
  corresponding to column/variable names. A safe rule of thumb is to use
  column names incl. physical quantity notation with its unit,
  `Quantity_Unit` like `"B_G"`, `"RF_MHz"`, `"Bsim_mT"` (e.g. pointing
  to simulated EPR spectrum \\x\\-axis)...etc, **default**:
  `col.names = c("index","B_G",dIepr_over_dB)`. For spectral time series
  `col.names` must include `"T(t)ime"` or `"S(s)lice"` character string
  in order to identify the corresponding time column/variable in the
  original ASCII file. The default (for the original
  [`fread`](https://rdatatable.gitlab.io/data.table/reference/fread.html))
  is to use the header column if present or detected. If not, the name
  is denoted as `"V"` followed by the column number.

- x.id:

  Numeric index related to `col.names` vector pointing to independent
  variable, which corresponds to \\x\\-axis in the spectra or other
  plots. **Default**: `x.id = 2` (for *Xenon*).

- x.unit:

  Character string, corresponding to original `x` variable/column unit,
  such as `"G"`, `"mT"` or `"MHz"`.

- Intensity.id:

  Numeric index related to `col.names` vector, pointing to `general`
  intensity, like derivative intensity (`dIepr_over_dB`), integral one
  (e.g. `single_Integ`), double or sigmoid integral (e.g. `Area`)...etc.
  This corresponds to column/vector which should be presented like
  \\y\\-axis in the EPR spectra or other plots. **Default**:
  `Intensity.id = 3` (for *Xenon*).

- time.series.id:

  Numeric index related to `col.names` vector and pointing to `time`
  column for time series EPR spectra. If data contains simple relation
  like \\Area\\ vs \\time\\, use the `x` and `x.unit`
  parameters/arguments instead (see also `Examples`). This argument is
  dedicated to kinetic-like experiments. **Default**:
  `time.series.id = NULL` (see also the `data.structure` argument).

- convertB.unit:

  Logical (**default**: `convertB.unit = TRUE`), whether upon reading,
  an automatic conversion between `G` and `mT` should be performed. If
  default is chosen, a new column/variable \\B\\ in `mT`/`G` is created.

- qValue:

  Numeric, Q value (quality factor, number) displayed at specific `dB`
  by the spectrometer, in case of *Xenon* or *new Magnettech* software
  the parameter is included in `.DSC`/`.dsc` file, **default**:
  `qValue = NULL`, which actually corresponds to value `1`.

- norm.vec.add:

  Numeric vector. Additional normalization constant in the form of
  vector, including all (in addition to `qValue`) normalization(s) like
  concentration, powder sample weight, number of scans, ...etc. (e.g.
  `norm.vec.add = c(2000,0.5,2)`). **Default**: `norm.vec.add = NULL`,
  which actually corresponds to value `1`. If `qValue = NULL`, the
  Q-factor/value might be also included in the `norm.vec.add`.

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

- data.structure:

  Character string, referring to structure of the ASCII data. Common
  spectral data files with \\Intensity\\ vs. \\x(B,g,RF(\text{MHz}))\\
  and/or \\time\\ columns (including the spectral time series)
  correspond to `data.structure = "spectra"` (**default**). For more
  complex ASCII data structure (such as spectral series processed by the
  acquisition spectrometer software, see `Examples`, or any other data)
  put `data.structure = "others"`. **In such case, all the arguments
  for** the `readEPR_Exp_Specs` **have to be set up accordingly**. The
  `data.structure` argument (assuming `time.series.id = NULL`) is
  helping to simplify the reading of `"spectra"` by the predefined
  `origin` argument.

- ...:

  additional arguments specified (see also the
  [`fread`](https://rdatatable.gitlab.io/data.table/reference/fread.html)
  function).

## Value

Data frame/table consisting of the magnetic flux density column `B_mT`
in millitesla (as well as `B_G` in gauss) or `RF_MHz` (in case of ENDOR
spectrum) or unitless `g-factor` and of the derivative intensity column
(`dIepr_over_dB`) or any other intensities (like integrated spectral
form) in `procedure defined unit` (see
[p.d.u.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6803776/)), which
is normalized by the above-described parameters and finally the `index`
and/or a `time` (in the case of time series experiment) columns are
displayed as well.

## Details

ASCII data are transformed into *R* data frames, which can be then
easily processed by the actual or other R packages, e.g.
[dplyr](https://dplyr.tidyverse.org/)), afterwards. Spectral intensities
are automatically normalized by the common experimental parameters like
Q-factor, concentration, weight...etc. These are defined by the two
arguments: `qValue` and `norm.vec.add`. The latter actually corresponds
to values of the above-mentioned quantities represented by the vector.
If `qValue = NULL` (actually corresponding to `1`), the Q-value can be
also defined as a component of the `norm.vec.add`. Finally, the
normalized intensity is calculated by the following expression
(depending on the `qValue` and/or `norm.vec.add`): \$\$dI\_{EPR} / dB =
Original~Intensity \\ (1/qValue)\$\$ or \$\$dI\_{EPR} / dB =
Original~Intensity \\ (1/qValue) \\ \prod\_{k} 1/(norm.vec.add\[k\])\$\$
where \\k\\ is iterating through all components of the `norm.vec.add`.
The structure of ASCII files/tables depends on the origin/software used
to acquire the EPR spectra. This is mirrored mainly by the `origin` and
`data.structure` arguments. Default arguments are set to read the data
from *Xenon* acquisition/processing software. However, additional
`origins` can be set like `origin = "winepr"` or `origin = "magnettech"`
or even any arbitrary string e.g. `origin = "csv"` (see also the
`origin` argument). For the latter, all arguments must be set
accordingly, as already demonstrated in `Examples`. Time series (time
evolution of EPR spectra/kinetics) is defined by the `time.series.id`
argument. In such case the ASCII data table also contains additional
column either with recorded time (see also
[`correct_time_Exp_Specs`](https://jatanrt.github.io/eprscope/reference/correct_time_Exp_Specs.md))
or with slice number for each spectrum.

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
## simple EPR spectrum acquired by "xenon"
## and with `B` conversion "G" <=> "mT"
## Loading the data
aminoxyl.data.path <-
  load_data_example(file = "Aminoxyl_radical_a.txt")
aminoxyl.data.01 <-
  readEPR_Exp_Specs(aminoxyl.data.path,
                    qValue = 2100)
## preview
head(aminoxyl.data.01)
#>    index       B_G  dIepr_over_dB      B_mT
#>    <int>     <num>          <num>     <num>
#> 1:     1 3332.7000 -1.7738564e-06 333.27000
#> 2:     2 3332.9005  1.2209461e-07 333.29005
#> 3:     3 3333.1009 -1.8403788e-07 333.31009
#> 4:     4 3333.3014 -7.1641412e-08 333.33014
#> 5:     5 3333.5019 -1.7361444e-06 333.35019
#> 6:     6 3333.7023 -5.1531169e-07 333.37023
#
# simple EPR spectrum acquired by "xenon"
## and without `B` conversion "G" <=> "mT"
aminoxyl.data.02 <-
  readEPR_Exp_Specs(aminoxyl.data.path,
                    convertB.unit = FALSE,
                    qValue = 2100)
## preview
head(aminoxyl.data.02)
#>    index       B_G  dIepr_over_dB
#>    <int>     <num>          <num>
#> 1:     1 3332.7000 -1.7738564e-06
#> 2:     2 3332.9005  1.2209461e-07
#> 3:     3 3333.1009 -1.8403788e-07
#> 4:     4 3333.3014 -7.1641412e-08
#> 5:     5 3333.5019 -1.7361444e-06
#> 6:     6 3333.7023 -5.1531169e-07
#
## the simple spectrum acquired by "winepr"
## (and 20 scans) on a 1 mM sample concentration:
## Loading the data
TMPD.data.path <-
  load_data_example(file = "TMPD_specelchem_accu_b.asc")
TMPD.data <-
  readEPR_Exp_Specs(TMPD.data.path,
                    col.names = c("B_G","dIepr_over_dB"),
                    qValue = 3500,
                    norm.vec.add = c(20,0.001),
                    origin = "winepr")
## preview
head(TMPD.data)
#>          B_G dIepr_over_dB      B_mT
#>        <num>         <num>     <num>
#> 1: 3439.1699 -2802.3680804 343.91699
#> 2: 3439.2200 -1525.3253348 343.92200
#> 3: 3439.2700 -1926.1109375 343.92700
#> 4: 3439.3201     1.9604213 343.93201
#> 5: 3439.3701 -4631.0111607 343.93701
#> 6: 3439.4199 -3595.7537946 343.94199
#
## the ENDOR spectrum recorded by "xenon"
## and 8 accumulation sweeps
## loading the data
PNT.ENDOR.data.path <-
  load_data_example(file = "PNT_ENDOR_a.txt")
PNT.ENDOR.data <-
  readEPR_Exp_Specs(PNT.ENDOR.data.path,
                    col.names = c("index",
                                  "RF_MHz",
                                  "dIepr_over_dB"),
                    x.unit = "MHz",
                    norm.vec.add = 8)
## preview
head(PNT.ENDOR.data)
#>    index    RF_MHz dIepr_over_dB
#>    <int>     <num>         <num>
#> 1:     1 2.0000000 0.00018947409
#> 2:     2 2.0400400 0.00102584647
#> 3:     3 2.0800801 0.00159380721
#> 4:     4 2.1201201 0.00143519925
#> 5:     5 2.1601602 0.00215950297
#> 6:     6 2.2002002 0.00125284480
#
## reading the (pre-processed) data file
## (data.structure = "mixed") from (by) the "Xenon" software
## corresponding to kinetics with `Area` and `time`
## columns/variables , these two have to be selected
## from several others + normalize `Area`
## against the `qValue` (first of all load the path
## of package example file)
triarylamine.rc.decay.path <-
  load_data_example("Triarylamine_radCat_decay_a.txt")
## data
triarylamine.rc.decay.data <-
  readEPR_Exp_Specs(path_to_ASC = triarylamine.rc.decay.path,
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
#>    time_s        Area
#>     <num>       <num>
#> 1:   0.00 0.018741176
#> 2:  15.17 0.018117647
#> 3:  30.01 0.017617647
#> 4:  44.82 0.017194118
#> 5:  59.66 0.016511765
#> 6:  74.52 0.016347059
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
acridineRad.data <-
  readEPR_Exp_Specs(acridineRad.data,
                    col.names = c("B_mT","dIepr_over_dB"),
                    x.unit = "mT",
                    origin = "magnettech")
## preview
head(acridineRad.data)
#>        B_mT dIepr_over_dB      B_G
#>       <num>         <num>    <num>
#> 1: 325.0000    -9.6482650 3250.000
#> 2: 325.0005    -9.6579153 3250.005
#> 3: 325.0010    -9.6675655 3250.010
#> 4: 325.0015    -9.6772157 3250.015
#> 5: 325.0020    -9.6868659 3250.020
#> 6: 325.0025    -9.6964157 3250.025
#
if (FALSE) { # \dontrun{
## EPR time series acquired by "Winepr"/"WinEpr"
readEPR_Exp_Specs(path_to_ASC,
                  col.names = c("B_G",
                                "Slice",
                                "dIepr_over_dB"),
                  origin = "Winepr")
#
## example for "xenon" time series experiment
## (evolution of EPR spectra in time, e.g. in case of
## EPR spectroelectrochemistry or photochemistry):
## together with `B` conversion "G" <=> mT
## and intensity normalized against `qValue`
readEPR_Exp_Specs(path_to_ASC,
                  col.names = c("index",
                                "B_G",
                                "time_s",
                                "dIepr_over_dB"),
                  qValue = 2800)
#
## read `.csv` file which is an output from online
## converter:
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
