# Normalization Constant Calculation for the Quantitative EPR Analysis

Normalization constant used by
[`quantify_EPR_Abs`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Abs.md)
or to normalize the EPR spectrum intensity. Calculation depends on the
receiver gain expression.

## Usage

``` r
quantify_EPR_Norm_const(
  conv.time.ms,
  Nscans,
  Npoints = NULL,
  Bsw = NULL,
  rg,
  rg.unit = "dB"
)
```

## Arguments

- conv.time.ms:

  Numeric, conversion time in milliseconds.

- Nscans:

  Numeric, number of scans.

- Npoints:

  Numeric, number of points (resolution) corresponding to individual
  sweep, if `rg.unit = "unitless"` (`rg.unit = "Unitless"`).
  **Default**: `Npoints = NULL`.

- Bsw:

  Numeric, experimental sweep width (magnetic flux density recording
  region, \\B\_{\text{SW}}\\) in "G" if `rg.unit = "unitless"`
  (`rg.unit = "Unitless"`). **Default**: `Bsw = NULL`.

- rg:

  Numeric, receiver gain value.

- rg.unit:

  Character string corresponding to unit of the receiver gain. Either
  `rg.unit = "db"` (`rg.unit = "dB"`, **default**) or
  `rg.unit = "unitless"` (`rg.unit = "Unitless"`).

## Value

Numeric value of the normalization constant for quantitative EPR and
intensity normalization.

## Details

For the receiver gain expressed in \\\text{dB}\\ units the normalization
constant is defined by the following relation =\> \$\$N\_{\text{norm}} =
t\_{\text{C}}(\text{ms})\\N\_{\text{Scans}}\\(20)\\
10^{(G\_{\text{R}}(\text{dB})/20)}\$\$ where
\\t\_{\text{C}}(\text{ms})\\ depicts the conversion time in
\\\text{ms}\\; \\N\_{\text{Scans}}\\ corresponds to number of scans and
\\G\_{\text{R}}(\text{dB})\\ is the receiver gain in \\\text{dB}\\. In
the case that the receiver gain is unitless, the normalization constant
is defined by =\> \$\$N\_{\text{norm}} =
t\_{\text{C}}(\text{ms})\\G\_{\text{R}}\\ (N\_{\text{points}} -
1)\\N\_{\text{scans}}\\/\\B\_{\text{SW}}(\text{G})\$\$ where
\\G\_{\text{R}}\\ and \\B\_{\text{SW}}(\text{G})\\ correspond to
unitless receiver gain and sweep width in Gauss, respectively.
\\N\_{\text{points}}\\ equals to the number of points (resolution of an
individual sweep). One can gather all parameters by the
[`readEPR_param_slct`](https://jatanrt.github.io/eprscope/reference/readEPR_param_slct.md)
or by the
[`readEPR_params_tabs`](https://jatanrt.github.io/eprscope/reference/readEPR_params_tabs.md)
function from the corresponding `.DSC`/`.dsc` or `.par` file. **If
during recording of EPR spectra the option** `Normalize Acquisition` (in
Spectrometer Configuration/Acquisition Options) **is activated, THE
INTENSITY is ALREADY NORMALIZED and DOESN'T REQUIRE ANY ADDITIONAL
NORMALIZATION !**. Please, refer also to the
[`quantify_EPR_Abs`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Abs.md)
function.

## References

Weber RT (2011). *Xenon User's Guide*. Bruker BioSpin Manual Version
1.3, Software Version 1.1b50.

Bruker Biospin (2007). *WIN-EPR User's Manual*.

## See also

Other Evaluations and Quantification:
[`eval_integ_EPR_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_integ_EPR_Spec.md),
[`eval_kinR_EPR_modelFit()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md),
[`eval_kinR_ODE_model()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_ODE_model.md),
[`quantify_EPR_Abs()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Abs.md)

## Examples

``` r
quantify_EPR_Norm_const(conv.time.ms = 8.2,
                        Nscans = 10,
                        rg = 32)
#> [1] 65290
#
quantify_EPR_Norm_const(conv.time.ms = 13.1,
                        Bsw = 180,
                        Nscans = 10,
                        Npoints  = 1024,
                        rg = 3.2e+4,
                        rg.unit = "Unitless")
#> [1] 23824533

```
