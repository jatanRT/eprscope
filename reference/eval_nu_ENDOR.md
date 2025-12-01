# ENDOR/Larmor Frequency of Specific Nuclei

Larmor/ENDOR frequency calculations for the analysis in
EPR/Electron-Nuclear Double Resonance (ENDOR). Function is inspired by
the similar [Larmor Frequency
Calculator](https://bio.groups.et.byu.net/LarmourFreqCal.phtml),
available online.

## Usage

``` r
eval_nu_ENDOR(nucle_us_i, B.unit = "G", B.val)
```

## Arguments

- nucle_us_i:

  (Vector) character string in the form like `"14N"` or `c("1H","13C")`,
  pointing to specific nucleus/nuclei, for which the frequency should by
  calculated. The nuclear *g*-factors for those nuclei are taken from
  the
  [`isotopes_ds`](https://jatanrt.github.io/eprscope/reference/isotopes_ds.md).

- B.unit:

  Character string, denoting the magnetic flux density *B* unit.
  **Default**: `B.unit = "G"`.

- B.val:

  Numeric, magnetic flux density \\B\\-`value`. This actually
  corresponds to \\B\\ at which the EPR signal saturates to record the
  ENDOR spectrum/spectra.

## Value

Numeric value or vector of nuclear Larmor/ENDOR frequencies in MHz for
selected nuclei at \\B\\ = `B.val`.

## Details

The frequency in MHz is calculated according to relation
\$\$\nu\_{\text{ENDOR}}^{} = -
(1/h)\\\mu\_{\text{N}}^{}\\g\_{\text{n}}^{}\\B\\10^{-6}\$\$ where \\h\\
is the Planck's constant, \\\mu\_{\text{N}}^{}\\ is the nuclear magneton
available from [constants](https://r-quantities.github.io/constants/)
package (`constants::syms$mun`), \\g\_{\text{n}}^{}\\ is the nuclear
\\g\\-factor of the specific nucleus (reported in the package
[`isotopes_ds`](https://jatanrt.github.io/eprscope/reference/isotopes_ds.md)
data frame as `g_Nuclear`) and finally, the \\B\\ denotes the magnetic
flux density at which the ENDOR spectra are recorded (see also `B.val`
in arguments). The \\10^{-6}\\ coefficient is referred to the resulting
frequency in MHz. The negative sign "\\-\\" mirrors the convention to
describe the direction of magnetic spin moments precession either
counter-clockwise (\\+\\, if \\\gamma\_{\text{n}}^{} \< 0\\) or
clockwise (\\-\\, if \\\gamma\_{\text{n}}^{} \> 0\\, Levitt MH (2013)).

## References

Levitt MH (2013). *Spin Dynamics: Basics of Nuclear Magnetic Resonance*.
Wiley, ISBN 978-1-118-68184-8,
<https://books.google.cz/books?id=bysFAa4MPQcC>.

## See also

Other Evaluations:
[`eval_DeltaXpp_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_DeltaXpp_Spec.md),
[`eval_FWHMx_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_FWHMx_Spec.md),
[`eval_extremeX_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_extremeX_Spec.md),
[`eval_gFactor()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor.md),
[`eval_gFactor_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor_Spec.md),
[`eval_interval_cnfd_tVec()`](https://jatanrt.github.io/eprscope/reference/eval_interval_cnfd_tVec.md),
[`eval_kinR_Eyring_GHS()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_Eyring_GHS.md),
[`eval_peakPick_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_peakPick_Spec.md)

## Examples

``` r
## Larmor/ENDOR frequency for one selected nucleus
## only, e.g. "14N" at 3486 G
eval_nu_ENDOR(nucle_us_i = "14N",
              B.val = 3486)
#> [1] -1.0728883
#
## Larmor/ENDOR frequency for selected nuclei
## e.g. "1H" and "31P" at saturation
## field of B = 349.9 mT
eval_nu_ENDOR(nucle_us_i = c("1H","31P"),
              B.unit = "mT",
              B.val = 349.9)
#> [1] -14.8978597  -6.0362834

```
