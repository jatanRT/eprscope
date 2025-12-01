# Calculation of \\g\\-factor from the Quantum Chemical Computational Output

In the Gaussian and ORCA outputs, the \\g\\-value (its 3 principal
components) is presented in the form of differences from the \\g_e\\
(\\g\\ of the free electron). Therefore, the function takes those values
to calculate the entire \\g\\-factor components or parses the
corresponding \\g\\-mean value from the outputs.

## Usage

``` r
eval_gFactor_QCHcomp(path_to_QCHoutput, mean = TRUE, origin = "gaussian")
```

## Arguments

- path_to_QCHoutput:

  Character string, corresponding to path of "Gaussian" or "ORCA" output
  text files including all \\g\\-factors. Alternatively, the
  [`file.path`](https://rdrr.io/r/base/file.path.html) can be applied to
  get the full/relative path of that file.

- mean:

  Logical, whether to calculate the `mean value/iso` from the principal
  components, **default**: `mean = TRUE`, or return the entire vector
  with the all 3 components.

- origin:

  Character string, pointing to origin of the EPR calculation parameters
  \<=\> which software package was used. Only two values are available
  =\> `"Gaussian"` (**default**) or `"ORCA"`.

## Value

Numeric mean \\g\\-factor value from the principal difference (from
\\g_e\\) components calculated by the QCH method (e.g. by DFT) or
numeric vector with the principal \\g\\-components if `mean = FALSE`.

## See also

Other Evaluations and Quantum Chemistry:
[`rearrange_aAiso_QCHcomp()`](https://jatanrt.github.io/eprscope/reference/rearrange_aAiso_QCHcomp.md),
[`rearrange_aAiso_QCHorgau()`](https://jatanrt.github.io/eprscope/reference/rearrange_aAiso_QCHorgau.md)

## Examples

``` r
## built-in package file example and path:
gauss.file.path <-
  load_data_example(file = "TMPDAradCatEPRa.inp.log.zip")
gauss.file <- unzip(gauss.file.path)
## g_iso-value calculation from Gaussian output file:
eval_gFactor_QCHcomp(gauss.file)
#> [1] 2.00317

```
