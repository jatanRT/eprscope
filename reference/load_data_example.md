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
#>  [1] "AcridineDeriv_Irrad_365nm.DTA"       
#>  [2] "AcridineDeriv_Irrad_365nm.csv.zip"   
#>  [3] "AcridineDeriv_Irrad_365nm.dsc"       
#>  [4] "Aminoxyl_radical_a.DSC"              
#>  [5] "Aminoxyl_radical_a.DTA"              
#>  [6] "Aminoxyl_radical_a.mat"              
#>  [7] "Aminoxyl_radical_a.txt"              
#>  [8] "PNT_ENDOR_a.DSC"                     
#>  [9] "PNT_ENDOR_a.DTA"                     
#> [10] "PNT_ENDOR_a.txt"                     
#> [11] "TMPD.sdf"                            
#> [12] "TMPDAradCatEPRa.inp.log.zip"         
#> [13] "TMPD_specelchem_CV_b.asc"            
#> [14] "TMPD_specelchem_CV_b.par"            
#> [15] "TMPD_specelchem_CV_b.spc"            
#> [16] "TMPD_specelchem_accu_b.asc"          
#> [17] "TMPD_specelchem_accu_b.par"          
#> [18] "TMPD_specelchem_accu_b.spc"          
#> [19] "Triarylamine_ECh_CV_ivium.txt"       
#> [20] "Triarylamine_radCat_decay_a.DSC"     
#> [21] "Triarylamine_radCat_decay_a.txt"     
#> [22] "Triarylamine_radCat_decay_series.DSC"
#> [23] "Triarylamine_radCat_decay_series.DTA"
#> [24] "Triarylamine_radCat_decay_series.YGF"
#> [25] "_extensions"                         
#> [26] "isotopes_DS.txt"                     
#
## additionally, you may refer to several function examples

```
