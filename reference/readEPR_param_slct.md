# Read the Selected EPR Instrumental Parameters and Information

Taking selected instrumental parameters or information from the
`.DSC/.dsc` or `.par` file of an EPR spectrum (written by the
`Xenon`/`Magnettech` or `WinEpr` software, respectively).

## Usage

``` r
readEPR_param_slct(path_to_dsc_par, string, origin = "xenon")
```

## Arguments

- path_to_dsc_par:

  Character string, path to `.DSC/.dsc` or `.par` file including the
  instrumental parameters provided by the EPR machine. File path can be
  also defined by the
  [`file.path`](https://rdrr.io/r/base/file.path.html) function.

- string:

  Character (vector) string (appeared at the line beginning) within the
  `.DSC/.dsc` or `.par` file corresponding to instrumental parameter or
  information. Following **strings are defined for all three main
  acquisition softwares described-above** (**in parenthesis for the
  "winepr" origin**):

  |                             |                                                                                                                          |
  |-----------------------------|--------------------------------------------------------------------------------------------------------------------------|
  | **String**                  | **Instrumental Parameter**                                                                                               |
  | "OPER" ("JON")              | operator (of the EPR instrument)                                                                                         |
  | "CMNT" ("JCO")              | comment (in order to describe the measurement)                                                                           |
  | "DATE" ("JDA")              | date (when the EPR spectrum was recorded)                                                                                |
  | "TIME" ("JTM")              | time (when the EPR spectrum was recorded)                                                                                |
  | "SAMP"                      | name/description of the sample, not available in "magnettech" `.dsc`                                                     |
  | "TITL" ("JEX")              | experiment title                                                                                                         |
  | "YNAM" ("JEY")              | name/title for the second variable/dimension of the experiment                                                           |
  | "B0MF"                      | modulation frequency in `Hz`                                                                                             |
  | "MWFQ" ("MF")               | microwave frequency in `Hz` (`GHz`)                                                                                      |
  | "QValue"                    | recorded quality-Factor (required for intensity normalization) `unitless`                                                |
  | "A1CT" ("HCF")              | central field (B) in `T` (`G`)                                                                                           |
  | "A1SW" ("HSW")              | sweep width in `T` (`G`)                                                                                                 |
  | "STMP" ("TE")               | temperature in `K`                                                                                                       |
  | "B0MA" ("RMA")              | modulation amplitude in `T` (`G`)                                                                                        |
  | "AVGS" ("JSD")              | number of accumulations for each spectrum                                                                                |
  | "XPTS"/"A1RS" ("RES"/"SSX") | number of points/resolution                                                                                              |
  | "XUNI" ("JUN"/"XXUN")       | spectrum abscissa unit                                                                                                   |
  | "YUNI" ("XYUN")             | unit corresponding to second variable (e.g. time) in spectral series                                                     |
  | "XMIN" ("GST"/"XXLB")       | actual *x*/*B* starting point (especially if teslameter in ON state)                                                     |
  | "YPTS"/"A2RS" ("REY"/"SSY") | *y*-resolution, corresponding to second variable (e.g. time) in spectral series                                          |
  | "XWID" ("GSI"/"XXWI")       | actual *x*/*B* width (entire range, especially if teslameter in ON state)                                                |
  | "MWPW" ("MP")               | microwave power in `W` (`mW`)                                                                                            |
  | "SPTP" ("RCT")              | conversion time in `s` (`ms`)                                                                                            |
  | "RCTC" ("RTC")              | time constant in `s` (ms), not available in "magnettech" `.dsc`                                                          |
  | "RCAG" ("RRG")              | signal receiver gain in `dB` (unitless), not available in "magnettech" `.dsc`                                            |
  | "ConvFact"                  | conversion factor/instr. calibration constant for quantitative analysis `unitless`, not available in "magnettech" `.dsc` |

- origin:

  Character string, corresponding to software used to acquire EPR
  spectra. The files are slightly different depending on whether they
  were recorded by the "WinEpr",`origin = "winepr"`, "Xenon"
  (**default**: `origin = "xenon"`) or by the "Magnettech" (ESR5000
  \[11-0422\], `origin = "magnettech"`).

## Value

Numeric or character string (e.g. date or comment), corresponding to
selected instrumental parameter(s) applied to record the EPR spectra. In
case of `string` character vector, named list, containing either
character and/or numeric values, is returned with the names
corresponding to `string`.

## See also

Other Data Reading:
[`readEPR_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md),
[`readEPR_Exp_Specs_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_kin.md),
[`readEPR_Exp_Specs_multif()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_multif.md),
[`readEPR_Sim_Spec()`](https://jatanrt.github.io/eprscope/reference/readEPR_Sim_Spec.md),
[`readEPR_params_slct_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_kin.md),
[`readEPR_params_slct_quant()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_quant.md),
[`readEPR_params_slct_sim()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_sim.md),
[`readEPR_params_tabs()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_tabs.md),
[`readEPR_solvent_props()`](https://jatanrt.github.io/eprscope/reference/readEPR_solvent_props.md),
[`readMAT_params_file()`](https://jatanrt.github.io/eprscope/reference/readMAT_params_file.md)

## Examples

``` r
## loading `.DSC` (`Xenon`) parameter file example
triaryl_radCat_dsc_path <-
  load_data_example(file = "Triarylamine_radCat_decay_a.DSC")
#
## reading modulation amplitude (in T) from the `Xenon` spectrometer file
readEPR_param_slct(triaryl_radCat_dsc_path,string = "B0MA")
#> [1] 7.2e-05
#
## reading Q-Value from the `Xenon` spectrometer file
readEPR_param_slct(triaryl_radCat_dsc_path,string = "QValue")
#> [1] 1700
#
## reading `CMNT` (comment) and `MWFQ` (microwave frequency in Hz)
## from the `Xenon` spectrometer file
readEPR_param_slct(triaryl_radCat_dsc_path,
                   string = c("CMNT","MWFQ"))
#> $CMNT
#> [1] ""
#> 
#> $MWFQ
#> [1] 9792281000
#> 
#
## loading `.par` (`WinEPR`) parameter file example
TMPD_radCat_par_path <-
  load_data_example(file = "TMPD_specelchem_accu_b.par")
#
## reading `JDA` (date) from `WinEPR` spectrometer file
readEPR_param_slct(TMPD_radCat_par_path,
                   string = "JDA",
                   origin = "winepr")
#> [1] "08/01/2011"
#
## reading `RMA` (modulation amplitude in G) and `TE`
## (temperature in K) as well as `JCO` (comment)
## from `WinEPR` spectrometer file
readEPR_param_slct(TMPD_radCat_par_path,
                   string = c("RMA","TE","JCO"),
                   origin = "WinEPR")
#> $RMA
#> [1] 0.5
#> 
#> $TE
#> [1] 295.06834
#> 
#> $JCO
#> [1] "Q=3500, 1mM solut. tetramethyl phenylene diamine, accu 20 spectra"
#> 
#
## loading and reading the `.DSC` file from `Xenon`
## corresponding to phenalenyl (PNT) CW ENDOR spectrum,
## read expr. date (`TIME`), microwave frequency (`MWFQ`)
## in Hz and the corresponding field for saturation (`B0VL`)
## in Tesla:
pnt_endor_dsc_path <-
  load_data_example(file = "PNT_ENDOR_a.DSC")
readEPR_param_slct(pnt_endor_dsc_path,
                string = c("TIME","MWFQ","B0VL")
               )
#> $TIME
#> [1] "15:37:43"
#> 
#> $MWFQ
#> [1] 9500219000
#> 
#> $B0VL
#> [1] 0.3385339
#> 


```
