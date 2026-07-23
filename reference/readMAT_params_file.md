# Reading EPR Simulation Parameters and Information from the *MATLAB* `.mat` File

Function is based on the
[`readMat`](https://rdrr.io/pkg/R.matlab/man/readMat.html) and provides
the reading of a `.mat` (MATLAB workspace) simulation (+ fitting) file
content from *EasySpin*, including structures/variables and fields. It
can be also used to read and store simulated EPR spectrum in the form of
R data frame (see `Examples`). In order to present the simulated/fitted
spectra, represented by such a data frame, please refer to the
[`present_EPR_Sim_Spec`](https://jatanrt.github.io/eprscope/reference/present_EPR_Sim_Spec.md)
function.

## Usage

``` r
readMAT_params_file(path_to_MAT, str.var = NULL, field.var = NULL)
```

## Arguments

- path_to_MAT:

  Character string, path to `.mat` MATLAB file with all variables saved
  in MATLAB workspace. The file path can be also defined by the
  [`file.path`](https://rdrr.io/r/base/file.path.html).

- str.var:

  Character string, denoting structure/variable, which may contain
  `fields`, such as `Sys` and `g` \\\Rightarrow\\ **Sys**.g,
  respectively. **Default**: `str.var = NULL`.

- field.var:

  Character string, corresponding to field variable after 'dot', which
  is available only for certain structures/variables, see e.g. example
  above (Sys.**g**). Therefore, the **default** value is `NULL` and the
  `string` **is applied only for structures with fields**.

## Value

Unless the `str.var` and/or `field.var` are not specified, the output is
`list` consisting of all original parameters/structures from MATLAB
file. Otherwise, the function returns either numeric/character
`vector/value` or list depending on `class` of the original
parameter/field variable.

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
[`readEPR_solvent_props()`](https://jatanrt.github.io/eprscope/reference/readEPR_solvent_props.md)

## Examples

``` r
## loading the package built-in
## `Aminoxyl_radical_a.mat` file as an example
aminoxyl.mat.file <-
  load_data_example(file = "Aminoxyl_radical_a.mat")
#
## reading the entire `mat` file as list
## and assign variable
aminoxyl.mat.list <-
  readMAT_params_file(aminoxyl.mat.file)
#
## read the `Sim1` structure/variable content into list
aminoxyl.mat.sim1 <-
  readMAT_params_file(aminoxyl.mat.file,
                      str.var = "Sim1")
#
## list preview
aminoxyl.mat.sim1
#> $g
#> [1] 2.007017
#> 
#> $Nucs
#> [1] "14N"
#> 
#> $n
#> [1] 1
#> 
#> $A
#> [1] 52.324276
#> 
#> $lwpp
#> [1] 0.4400000 0.1008846
#> 
#
## compare the previous simulation parameters with
## those obtained by the `eval_sim_EPR_isoFit()`
## function (look at the corresponding examples)
#
## alternatively the `Sim1` (its dimension > 2)
## can be also read by the following command,
## however the returned output has a complex
## array-list structure
aminoxyl.mat.list$Sim1[, , 1]
#> $g
#>          [,1]
#> [1,] 2.007017
#> 
#> $Nucs
#>      [,1] 
#> [1,] "14N"
#> 
#> $n
#>      [,1]
#> [1,]    1
#> 
#> $A
#>           [,1]
#> [1,] 52.324276
#> 
#> $lwpp
#>      [,1]      [,2]
#> [1,] 0.44 0.1008846
#> 
#
## read the `Sim1` structure/variable
## and the field `Nucs` corresponding to nuclei
## considered in the EPR simulation
aminoxyl.mat.sim1.nucs <-
  readMAT_params_file(aminoxyl.mat.file,
                      str.var = "Sim1",
                      field.var = "Nucs")
#
## preview
aminoxyl.mat.sim1.nucs
#> [1] "14N"
#
## reading the magnetic flux density
## `B` column/vector corresponding to simulated
## and experimental EPR spectrum
aminoxyl.B.G <-
  readMAT_params_file(aminoxyl.mat.file,
                      str.var = "B")
#
## preview of the first 6 values
aminoxyl.B.G[1:6]
#> [1] 3332.7000 3332.9005 3333.1009 3333.3014 3333.5019 3333.7023
#
## reading the intensity related to simulated/fitted
## EPR spectrum
aminoxyl.sim.fitSpec <-
  readMAT_params_file(aminoxyl.mat.file,
                      str.var = "fit1",
                      field.var = "fitSpec")
#
## for the newer EasySpin version (> 6.0.0),
## the "fitSpec" is replaced by the simple "fit"
## or "fitraw", corresponding to scaled and raw intensity
## of the simulated EPR spectrum, please refer also
## to the EasySpin documentantion:
## https://easyspin.org/easyspin/documentation/esfit.html,
## or to the example below
#
## preview of the first 6 values
aminoxyl.sim.fitSpec[1:6]
#> [1] 0.00068169341 0.00066601473 0.00047674618 0.00015739402 0.00020037360
#> [6] 0.00016455010
#
## In the following example load
## the simulated/fitted EPR spectrum
## by the EasySpin from `mat` workspace file =>
simulation.aminoxyl.spectr.df <-
  data.frame(Bsim_G = aminoxyl.B.G,
             dIeprSim_over_dB = aminoxyl.sim.fitSpec) %>%
             dplyr::mutate(Bsim_mT = Bsim_G * 0.1)
#
## preview
head(simulation.aminoxyl.spectr.df)
#>      Bsim_G dIeprSim_over_dB   Bsim_mT
#> 1 3332.7000    0.00068169341 333.27000
#> 2 3332.9005    0.00066601473 333.29005
#> 3 3333.1009    0.00047674618 333.31009
#> 4 3333.3014    0.00015739402 333.33014
#> 5 3333.5019    0.00020037360 333.35019
#> 6 3333.7023    0.00016455010 333.37023
#
## load newer EasySpin (> 6.0.0) workspace file
## related to TMPD radical cation
tmpd.easyspin6.path <-
  load_data_example(file = "TMPD_specelchem_accu_b.mat")
#
## create a vector of scaled intensity
## of the simulated/fitted EPR spectrum
tmpd.easyspin6.fit <-
  readMAT_params_file(
    tmpd.easyspin6.path,
    str.var = "fit1",
    field.var = "fit"
  )
#
## preview of the first 6 values
tmpd.easyspin6.fit[1:6]
#> [1] 9.1079658 9.3724788 9.0710514 8.4339157 6.3840355 3.3278115
#
## load all simulation fitted arguments from
## the `.mat` workspace
tmpd.easyspin6.argsfit <-
  readMAT_params_file(
    tmpd.easyspin6.path,
    str.var = "fit1",
    field.var = "argsfit"
  )
#
## create a list (temporary variable)
list.argsfit <- tmpd.easyspin6.argsfit[[1]][[1]][ , , 1]
#
## convert it into nicely structured list with
## all arguments/parameters
list.argsfit.final <-
  lapply(list.argsfit, function(x) c(x))
#
## final list preview
list.argsfit.final
#> $g
#> [1] 2.0030057
#> 
#> $Nucs
#> [1] "14N,1H,1H"
#> 
#> $n
#> [1]  2  4 12
#> 
#> $A
#> [1] 19.5286649  5.4959467 19.5414672
#> 
#> $lwpp
#> [1] 0.034971964 0.015990064
#> 


```
