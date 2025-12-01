# Basic Calculation of \\g\\-Factor

Calculation of \\g\\-factor according to fundamental formula (see
`Value`). The magnetic flux density (`B.val`) and microwave frequency
(`nu.val`,\\\nu\\) can be defined, having the common units like `G`
(Gauss) `mT` (millitesla) or `T` (tesla) as well as `GHz` or `Hz`.

## Usage

``` r
eval_gFactor(nu.val, nu.unit = "GHz", B.val, B.unit = "mT")
```

## Arguments

- nu.val:

  Numeric, microwave Frequency value.

- nu.unit:

  Character string, frequency unit defined by `GHz` or `Hz`,
  **default**: `nu.unit = "GHz"`.

- B.val:

  Numeric, magnetic flux density value.

- B.unit:

  Character string, magnetic flux density unit in `G` or `mT` or `T`,
  **default**: `B.unit = "mT"`.

## Value

\\g\\-Value from \\(\nu h)/(\mu\_{\text{B}} B)\\. Physical constants
(\\\mu\_{\text{B}}\\ and \\h\\) are taken from
[constants](https://r-quantities.github.io/constants/) package by the
[`syms`](https://rdrr.io/pkg/constants/man/syms.html).

## See also

Other Evaluations:
[`eval_DeltaXpp_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_DeltaXpp_Spec.md),
[`eval_FWHMx_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_FWHMx_Spec.md),
[`eval_extremeX_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_extremeX_Spec.md),
[`eval_gFactor_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor_Spec.md),
[`eval_interval_cnfd_tVec()`](https://jatanrt.github.io/eprscope/reference/eval_interval_cnfd_tVec.md),
[`eval_kinR_Eyring_GHS()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_Eyring_GHS.md),
[`eval_nu_ENDOR()`](https://jatanrt.github.io/eprscope/reference/eval_nu_ENDOR.md),
[`eval_peakPick_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_peakPick_Spec.md)

## Examples

``` r
eval_gFactor(9.8020458,
             nu.unit = "GHz",
             350.214,
             B.unit = "mT")
#> [1] 1.99973
#
eval_gFactor(nu.val = 9.8020458e+9,
             nu.unit = "Hz",
             B.val = 3502.14,
             B.unit = "G")
#> [1] 1.99973
#
eval_gFactor(9.5421,"GHz",0.333251,"T")
#> [1] 2.04579

```
