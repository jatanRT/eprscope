# Read the Selected Instrumental Parameters of EPR Time Series Experiment

Function takes selected instrumental parameters relevant to **time
series ("kinetic")** experiment from the `.DSC/.dsc` or `.par` file of
an EPR Spectrum, obtained from the "Xenon", "WinEpr" or "Magnettech"
software. These parameters are required for time correction of the CW
(continuous wave) EPR spectra, see the
[`correct_time_Exp_Specs`](https://jatanrt.github.io/eprscope/reference/correct_time_Exp_Specs.md).

## Usage

``` r
readEPR_params_slct_kin(path_to_dsc_par, origin = "xenon")
```

## Arguments

- path_to_dsc_par:

  String, path to `.DSC/.dsc` or `.par` file including all instrumental
  parameters provided by the EPR machine.

- origin:

  Character string, corresponding to software used to acquire EPR
  spectra. The files are slightly different depending on whether they
  were recorded by the "WinEpr",`origin = "winepr"`, "Xenon"
  (**default**: `origin = "xenon"`) or by the "Magnettech" (ESR5000
  \[11-0422\], `origin = "magnettech"`).

## Value

List containing:

- Nscans:

  Number of scans.

- swTime:

  Sweep time in `s` required for time correction during the
  `2D_Field_Delay` (time series EPR experiment).

- Npoints:

  Number of points (spectral resolution).

## See also

Other Data Reading:
[`readEPR_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md),
[`readEPR_Exp_Specs_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_kin.md),
[`readEPR_Exp_Specs_multif()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_multif.md),
[`readEPR_Sim_Spec()`](https://jatanrt.github.io/eprscope/reference/readEPR_Sim_Spec.md),
[`readEPR_param_slct()`](https://jatanrt.github.io/eprscope/reference/readEPR_param_slct.md),
[`readEPR_params_slct_quant()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_quant.md),
[`readEPR_params_slct_sim()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_sim.md),
[`readEPR_params_tabs()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_tabs.md),
[`readEPR_solvent_props()`](https://jatanrt.github.io/eprscope/reference/readEPR_solvent_props.md),
[`readMAT_params_file()`](https://jatanrt.github.io/eprscope/reference/readMAT_params_file.md)

## Examples

``` r
## loading `.DSC` (`Xenon`) parameter file example
aminoxyl_dsc_path <-
  load_data_example(file = "Triarylamine_radCat_decay_series.DSC")
#
readEPR_params_slct_kin(aminoxyl_dsc_path)
#> $Nscans
#> [1] 1
#> 
#> $swTime
#> [1] 12.216
#> 
#> $Npoints
#> [1] 2400
#> 

```
