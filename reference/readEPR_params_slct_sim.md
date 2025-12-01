# Read the Selected Instrumental Parameters Required for EPR Simulations

Reading the `.DSC/.dsc` or `.par` file to extract the important
parameters like "sweep width", "central field", "number of points" as
well as "microwave frequency" which are are required for the simulations
of EPR spectra (see also the
[`eval_sim_EPR_iso`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md)
function).

## Usage

``` r
readEPR_params_slct_sim(path_to_dsc_par, origin = "xenon", B.unit = "G")
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

- B.unit:

  Character string, pointing to unit of magnetic flux density which is
  the output "unit", `"G"` ("Gauss") or `"mT"` ("millitesla"), for
  `"sweep width"` and `"central field"` (see also the
  [`eval_sim_EPR_iso`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md)).
  **Default**: `B.unit = "G"`.

## Value

List consisting of:

- Bcf:

  Central field (magnetic flux density, *B*) value in `B.unit`.

- Bsw:

  Sweep width (magnetic flux density, *B*, experimental range) value in
  `B.unit`.

- Npoints:

  Number of points (spectral resolution).

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
[`readEPR_params_slct_quant()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_quant.md),
[`readEPR_params_tabs()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_tabs.md),
[`readEPR_solvent_props()`](https://jatanrt.github.io/eprscope/reference/readEPR_solvent_props.md),
[`readMAT_params_file()`](https://jatanrt.github.io/eprscope/reference/readMAT_params_file.md)

## Examples

``` r
## loading `.par` (`WinEPR`) parameter file example
TMPD_radCat_par_path <-
  load_data_example(file = "TMPD_specelchem_accu_b.par")
#
## `B` parameters in `mT`
readEPR_params_slct_sim(TMPD_radCat_par_path,
                        origin = "winepr",
                        B.unit = "mT")
#> $Bcf
#> [1] 349.917
#> 
#> $Bsw
#> [1] 12
#> 
#> $Npoints
#> [1] 2401
#> 
#> $mwGHz
#> [1] 9.814155
#> 
#
## loading `.dsc` (`Magnettech`) parameter
## file example
AcridineRad.params.path <-
  load_data_example("AcridineDeriv_Irrad_365nm.dsc")
readEPR_params_slct_sim(AcridineRad.params.path,
                        origin = "magnettech")
#> $Bcf
#> [1] 3400
#> 
#> $Bsw
#> [1] 300
#> 
#> $Npoints
#> [1] 60000
#> 
#> $mwGHz
#> [1] 9.433125
#> 

```
