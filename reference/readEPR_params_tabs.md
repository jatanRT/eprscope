# Read the EPR Instrumental Parameters and Information for Tabular Outputs

Taking the instrumental parameters from the `.DSC/.dsc` or `.par` files,
applied to record the EPR Spectra, and transferring them into list of
`Tables/Data Frames`. They include either parameter values and their
units or character/string information about the measurement, see also
the
[`readEPR_param_slct`](https://jatanrt.github.io/eprscope/reference/readEPR_param_slct.md)
function.

## Usage

``` r
readEPR_params_tabs(path_to_dsc_par, origin = "xenon", interact = NULL)
```

## Arguments

- path_to_dsc_par:

  Character string, path (also provided by
  [`file.path`](https://rdrr.io/r/base/file.path.html)) to `.DSC/.dsc`
  or `.par` (depending on `origin` parameter) `text` files, including
  instrumental parameters and provided by the EPR machine.

- origin:

  Character string, corresponding to software used to acquire EPR
  spectra. The files are slightly different depending on whether they
  were recorded by the "WinEpr",`origin = "winepr"`, "Xenon"
  (**default**: `origin = "xenon"`) or by the "Magnettech" (ESR5000
  \[11-0422\], `origin = "magnettech"`).

- interact:

  Character string, whether to display interactive tables by the
  [`datatable`](https://rdrr.io/pkg/DT/man/datatable.html). **Default**:
  `interact = NULL`. Interactive table with parameters can be displayed
  by `interact = "params"` or to display the additional information
  table: `interact = "info"`.

## Value

List of data frames/tables containing:

- params:

  Instrumental parameters with their numeric values and units.

- info:

  Information character string, such as date, operator, comment...etc.

Both data frames may be depicted in the form of interactive tables by
the `interact` function argument.

## See also

Other Data Reading:
[`readEPR_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md),
[`readEPR_Exp_Specs_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_kin.md),
[`readEPR_Exp_Specs_multif()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_multif.md),
[`readEPR_Sim_Spec()`](https://jatanrt.github.io/eprscope/reference/readEPR_Sim_Spec.md),
[`readEPR_param_slct()`](https://jatanrt.github.io/eprscope/reference/readEPR_param_slct.md),
[`readEPR_params_slct_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_kin.md),
[`readEPR_params_slct_quant()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_quant.md),
[`readEPR_params_slct_sim()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_sim.md),
[`readEPR_solvent_props()`](https://jatanrt.github.io/eprscope/reference/readEPR_solvent_props.md),
[`readMAT_params_file()`](https://jatanrt.github.io/eprscope/reference/readMAT_params_file.md)

## Examples

``` r
## loading built-in example file =>
## "AcridineDeriv_Irrad_365nm.dsc" by `Magnettech`
## spectrometer software
AcridinRad.data.file <-
  load_data_example("AcridineDeriv_Irrad_365nm.dsc")
## reading and displaying parameters as data frame
AcridinRad.params.data <-
  readEPR_params_tabs(AcridinRad.data.file,
                      origin = "magnettech")
#
## parameters preview
AcridinRad.params.data$params
#>               Parameter        Value     Unit
#> 1             Frequency     9.433125      GHz
#> 2                QValue  1828.859253 Unitless
#> 3         Central Field  3400.000000        G
#> 4           Sweep Width   300.000000        G
#> 5  Modulation Amplitude     0.100000       mT
#> 6         Num. of Scans     1.000000 Unitless
#> 7      Number of Points 60000.000000 Unitless
#> 8                 Power     3.240000       mW
#> 9       Conversion Time     0.013000        s
#> 10           Sweep Time   780.000000        s
#> 11          Temperature   302.502520        K
#> 12 Modulation Frequency   100.000000      kHz
#
## info preview
AcridinRad.params.data$info
#>                 Parameter                                  Information
#> 1                Operator                                         user
#> 2                    Date                                     12/07/22
#> 3          Recording Time                                     10:22:51
#> 4                 Comment                                           ''
#> 5        Experiment Title 20221207_102412944_Ms-tBu-acr-DIPEA-365nm_01
#> 6 X Var. (e.g. B/RF) Unit                                            G
#
## built-in example file => "TMPD_specelchem_accu_b.par"
## by the `WinEPR` spectrometer software
tmpd.params.file <-
  load_data_example(file = "TMPD_specelchem_accu_b.par")
## reading and displaying parameters as data frame
tmpd.params.tab <-
  readEPR_params_tabs(tmpd.params.file,
                      origin = "winepr")
#
## preview
tmpd.params.tab$params
#>               Parameter        Value     Unit
#> 1             Frequency     9.814155      GHz
#> 2         Central Field  3499.170000        G
#> 3           Sweep Width   120.000000        G
#> 4  Modulation Amplitude     0.500000        G
#> 5       Number of Scans    20.000000 Unitless
#> 6      Number of Points  2401.000000 Unitless
#> 7                 Power     5.024000       mW
#> 8       Conversion Time     0.008000        s
#> 9            Sweep Time    19.208000        s
#> 10         Acquire Time   384.160000        s
#> 11        Time Constant     0.005120        s
#> 12          Temperature   295.068344        K
#> 13        Receiver Gain 39905.250000 Unitless
##
## the same data frame, now in interactive table form
readEPR_params_tabs(tmpd.params.file,
                    origin = "winepr",
                    interact = "params")

{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13"],["Frequency","Central Field","Sweep Width","Modulation Amplitude","Number of Scans","Number of Points","Power","Conversion Time","Sweep Time","Acquire Time","Time Constant","Temperature","Receiver Gain"],[9.814155,3499.17,120,0.5,20,2401,5.024,0.008,19.208,384.16,0.00512,295.068344,39905.25],["GHz","G","G","G","Unitless","Unitless","mW","s","s","s","s","K","Unitless"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Parameter<\/th>\n      <th>Value<\/th>\n      <th>Unit<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"Parameter","targets":1},{"name":"Value","targets":2},{"name":"Unit","targets":3}],"order":[],"autoWidth":false,"orderClasses":false},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[]}
```
