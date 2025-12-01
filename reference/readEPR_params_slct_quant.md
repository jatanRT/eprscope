# Read the Selected Instrumental Parameters Relevant to EPR Quantitative Analysis

Reading the `.DSC/.dsc` or `.par` file to extract the important
parameters like "modulation amplitude", "temperature", "microwave power"
as well as "microwave frequency" which are are required for the absolute
EPR quantitative analysis (\\\equiv\\ radical or paramagnetic species
number determination, see the
[`quantify_EPR_Abs`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Abs.md)
function).

## Usage

``` r
readEPR_params_slct_quant(path_to_dsc_par, origin = "xenon")
```

## Arguments

- path_to_dsc_par:

  Character string, path (also provided by
  [`file.path`](https://rdrr.io/r/base/file.path.html)) to `.DSC/.dsc`
  or `.par` (depending on OS, see the `origin` argument) `text` files
  including all instrumental parameters and provided by the EPR machine.

- origin:

  Character string, corresponding to software used to acquire EPR
  spectra. The files are slightly different depending on whether they
  were recorded by the "WinEpr",`origin = "winepr"`, "Xenon"
  (**default**: `origin = "xenon"`) or by the "Magnettech" (ESR5000
  \[11-0422\], `origin = "magnettech"`).

## Value

List consisting of:

- BmmT:

  Modulation amplitude value in `mT`.

- PmW:

  Microwave source power in `mW`.

- TK:

  Experimental temperature in `K`.

- mwGHz:

  Microwave frequency value in `GHz`.

## See also

Other Data Reading:
[`readEPR_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md),
[`readEPR_Exp_Specs_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_kin.md),
[`readEPR_Exp_Specs_multif()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_multif.md),
[`readEPR_Sim_Spec()`](https://jatanrt.github.io/eprscope/reference/readEPR_Sim_Spec.md),
[`readEPR_param_slct()`](https://jatanrt.github.io/eprscope/reference/readEPR_param_slct.md),
[`readEPR_params_slct_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_kin.md),
[`readEPR_params_slct_sim()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_sim.md),
[`readEPR_params_tabs()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_tabs.md),
[`readEPR_solvent_props()`](https://jatanrt.github.io/eprscope/reference/readEPR_solvent_props.md),
[`readMAT_params_file()`](https://jatanrt.github.io/eprscope/reference/readMAT_params_file.md)

## Examples

``` r
## loading `.DSC` (`Xenon`) parameter file example
aminoxyl_dsc_path <-
  load_data_example(file = "Aminoxyl_radical_a.DSC")
#
readEPR_params_slct_quant(aminoxyl_dsc_path)
#> $BmmT
#> [1] 0.12
#> 
#> $PmW
#> [1] 3.17
#> 
#> $TK
#> [1] 248
#> 
#> $mwGHz
#> [1] 9.806665
#> 

```
