# Loading the Built-In Data Files for Package Examples

Loads built-in package data files which are required either to
demonstrate the package functionality by examples within the
documentation, or to run functions involving essential characteristics
of nuclei important for EPR spectroscopy (e.g.
[`eval_nu_ENDOR`](https://jatanrt.github.io/eprscope/reference/eval_nu_ENDOR.md)
or
[`eval_sim_EPR_iso`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md),
see also
[`isotopes_ds`](https://jatanrt.github.io/eprscope/reference/isotopes_ds.md)).
Details of all data are summarized in
[`vignette("datasets")`](https://jatanrt.github.io/eprscope/articles/datasets.md).

## Usage

``` r
load_data_example(file = NULL)
```

## Arguments

- file:

  Character string, corresponding to file name + extension.

## Examples

``` r
## list of all files within the "extdata" directory =>
load_data_example()
#>  [1] "AcridineDeriv_Irrad_365nm.csv.zip"   
#>  [2] "AcridineDeriv_Irrad_365nm.dsc"       
#>  [3] "Aminoxyl_radical_a.DSC"              
#>  [4] "Aminoxyl_radical_a.mat"              
#>  [5] "Aminoxyl_radical_a.txt"              
#>  [6] "PNT_ENDOR_a.DSC"                     
#>  [7] "PNT_ENDOR_a.txt"                     
#>  [8] "TMPD.sdf"                            
#>  [9] "TMPDAradCatEPRa.inp.log.zip"         
#> [10] "TMPD_specelchem_accu_b.asc"          
#> [11] "TMPD_specelchem_accu_b.par"          
#> [12] "Triarylamine_ECh_CV_ivium.txt"       
#> [13] "Triarylamine_radCat_decay_a.DSC"     
#> [14] "Triarylamine_radCat_decay_a.txt"     
#> [15] "Triarylamine_radCat_decay_series.DSC"
#> [16] "Triarylamine_radCat_decay_series.zip"
#> [17] "_extensions"                         
#> [18] "isotopes_DS.txt"                     
#
## additionally, you may refer to several function examples

```
