# Convert Time \\t\\ into Variable Linearly Depending on \\t\\.

Conversion of time (\\t\\) into variable (\\var\\), linearly changing in
time.

## Usage

``` r
convert_time2var(
  time.vals,
  time.unit = "s",
  var0,
  var.switch = NULL,
  var.rate,
  var.rate.unit = "s^{-1}"
)
```

## Arguments

- time.vals:

  Numeric value or vector, corresponding to time (points) where the
  variable `var` is changed.

- time.unit:

  Character string, time unit defined by `s`,`min` or `h`. **Default**:
  `time.unit = "s"`.

- var0:

  Numeric, the initial value (ALSO WITH NEGATIVE SIGN, if required, e.g.
  negative electrochemical potential).

- var.switch:

  Numeric, the switching point `var` value, in case when a linear CYCLIC
  CHANGE (or 'triangular ramp') of `var` on time is applied (e.g. in
  cyclic voltammetry). **Default**: `var.switch = NULL` (in case there
  is no such cyclic change).

- var.rate:

  Numeric, corresponding to rate of linear `var` change (INCLUDING ALSO
  NEGATIVE SIGN, if required, e.g. in the case of electrochemical
  reduction or sample cooling).

- var.rate.unit:

  Character string, corresponding to `var.rate` unit defined by
  following strings `"s^{-1}"` \\\equiv \text{s}^{-1}\\, `"min^{-1}"`
  \\\equiv \text{min}^{-1}\\ or `"h^{-1}"` \\\equiv \text{h}^{-1}\\.
  **Default**: `var.rate.unit = "s^{-1}"`.

## Value

Numeric value or vector of the variable such as electrochemical
potential or temperature, linearly changing in time.

## Details

The linear relationship between \\var\\ and time (\\t\\) can be
expressed like \$\$var = var0 + rate~ t\$\$ This is especially suitable
for time conversion of EPR time series experiments (see e.g.
[`readEPR_Exp_Specs_kin`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_kin.md))
simultaneously performed either during electrochemical/voltammetric or
variable temperature experiment. When cyclic series experiment is
performed (e.g. cyclic voltammetry), that \\var\\ value depends on the
switching one, like =\> \$\$var = var0 + rate~ t \~~ \text{for} \~~ t
\leq t\_{\text{switch}}\$\$ \$\$var = var\_{\text{switch}} - rate\\ (t -
t\_{\text{switch}}) \~~ \text{for} \~~ t \geq t\_{\text{switch}}\$\$
where the \\t\_{\text{switch}}\\, corresponding to
\\var\_{\text{switch}}\\, are the quantities at the turning point( see
also the `var.switch` argument).

## See also

Other Conversions and Corrections:
[`convert_A_MHz_2a()`](https://jatanrt.github.io/eprscope/reference/convert_A_MHz_2a.md),
[`convert_B()`](https://jatanrt.github.io/eprscope/reference/convert_B.md),
[`convert_a_mT_2A()`](https://jatanrt.github.io/eprscope/reference/convert_a_mT_2A.md),
[`correct_time_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/correct_time_Exp_Specs.md)

## Examples

``` r
## calculate potential after 30 s, starting from 200 mV
## into cathodic direction (reduction) by 5 mV s^{-1}
convert_time2var(30,var0 = 0.2,var.rate = - 0.005)
#> [1] 0.05
#
## heating sample after 5 min starting from 293 K
## by the temperature rate of 4 K min^{-1}
convert_time2var(5,
                 time.unit = "min",
                 var0 = 293,
                 var.rate = 4,
                 var.rate.unit = "min^{-1}")
#> [1] 313
#
## create/evaluate vector containing the applied
## cell potential (in V) from the simultaneously
## performed electrochemical oxidation experiment
## (e.g. cyclic voltammetry from -0.1V to 0.45V and back
## to -0.1V). Time series vector is labeled as "time_s".
time_s <- seq(0,360,by = 18)
E_V <- convert_time2var(time.vals = time_s,
                        var0 = -0.1,
                        var.switch = 0.45,
                        var.rate = 0.003)
## preview
as.matrix(E_V)
#>         [,1]
#>  [1,] -0.100
#>  [2,] -0.046
#>  [3,]  0.008
#>  [4,]  0.062
#>  [5,]  0.116
#>  [6,]  0.170
#>  [7,]  0.224
#>  [8,]  0.278
#>  [9,]  0.332
#> [10,]  0.386
#> [11,]  0.440
#> [12,]  0.406
#> [13,]  0.352
#> [14,]  0.298
#> [15,]  0.244
#> [16,]  0.190
#> [17,]  0.136
#> [18,]  0.082
#> [19,]  0.028
#> [20,] -0.026
#> [21,] -0.080


```
