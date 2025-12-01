# Reading Solvent Properties from the `solvents_ds` Dataset

Gathering the solvent properties from
[`solvents_ds`](https://jatanrt.github.io/eprscope/reference/solvents_ds.md)
in order to filter out specific `solvent` and its corresponding
properties (by the `prop` argument). See also
[`vignette("datasets")`](https://jatanrt.github.io/eprscope/articles/datasets.md).

## Usage

``` r
readEPR_solvent_props(solvent, prop = NULL)
```

## Arguments

- solvent:

  Character string, pointing to solvent name (or any string from the
  solvent name/abbreviation), such as
  `solvent = "DMSO"`,`solvent = "acetone"`, `solvent = "xylene"`. If
  more than one rows/observations are being returned (e.g. in case of
  `solvent = "xylene"`) =\> additional solvent specification must be
  provided e.g. `solvent = "o-xylene"`.

- prop:

  Character string related to a data frame variable/column/property e.g.
  `"boiling"`, `"formula"`, `"dens"`, `"dielectric"`...etc (see also
  [`solvents_ds`](https://jatanrt.github.io/eprscope/reference/solvents_ds.md)).

## Value

Data frame with row/rows of selected solvent(s) and the corresponding
properties. If a specific property is called like `code = "melting"`
(\\\equiv\\ melting point in °C), the function returns
`value/character`.

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
[`readEPR_params_tabs()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_tabs.md),
[`readMAT_params_file()`](https://jatanrt.github.io/eprscope/reference/readMAT_params_file.md)

## Examples

``` r
## Properties of `DMSO`:
solvent_01 <- readEPR_solvent_props("DMSO")
head(solvent_01)
#> # A tibble: 1 × 10
#>   Solvent            Formula    MW Boiling_Point_oC Melting_Point_oC Density_gmL
#>   <chr>              <chr>   <dbl>            <dbl>            <dbl>       <dbl>
#> 1 dimethyl sulfoxid… C2H6OS  78.13              189             18.4       1.092
#> # ℹ 4 more variables: Solubility_g100gW <chr>, Dielectric_Const <chr>,
#> #   Flash_Point_oC <dbl>, Viscosity_cp <chr>
#
## All `xylene` solvent specifications:
solvent_02 <- readEPR_solvent_props(solvent = "xylene")
head(solvent_02)
#> # A tibble: 3 × 10
#>   Solvent  Formula     MW Boiling_Point_oC Melting_Point_oC Density_gmL
#>   <chr>    <chr>    <dbl>            <dbl>            <dbl>       <dbl>
#> 1 o-xylene C8H10   106.17            144              -25.2       0.897
#> 2 m-xylene C8H10   106.17            139.1            -47.8       0.868
#> 3 p-xylene C8H10   106.17            138.4             13.3       0.861
#> # ℹ 4 more variables: Solubility_g100gW <chr>, Dielectric_Const <chr>,
#> #   Flash_Point_oC <dbl>, Viscosity_cp <chr>
#
## Boiling point of `o-xylene`:
readEPR_solvent_props(solvent = "o-xylene",
                      prop = "boiling")
#> [1] 144


```
