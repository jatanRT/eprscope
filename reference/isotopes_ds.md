# Nuclear Isotope Data Frame (Dataset) with ENDOR Frequencies

Data frame/Dataset summarizing the essential characteristics of nuclei
in EPR spectroscopy.

## Usage

``` r
isotopes_ds
```

## Format

A data frame with 351 rows and 9 variables/columns:

- No_Proton:

  Numeric, proton number.

- Isotope:

  Character, ponting to isotope in format like "14N".

- Stability:

  Character, pointing either to stable, "STB", or to radio-active, "RA",
  isotope.

- Name:

  Character, corresponding to isotope name.

- Spin:

  Numeric, denoting the spin quantum number.

- g_Nuclear:

  Numeric, corresponding to nuclear \\g\\-factor (\\g\_{\text{n}}^{}\\).

- Abund_Natur_Percent:

  Numeric, pointing to natural abundance of an isotope in \\\\\\.

- Q_Barn:

  Numeric, corresponding to nuclear quadrupolar moment in
  \\10^{-28}\\\text{m}^2\\.

- nu_ENDOR_MHz_035T:

  Numeric, specific Larmor/ENDOR frequency (\\\nu\_{\text{ENDOR}}^{}\\)
  at \\0.35\\\text{T}\\.

## Source

<https://easyspin.org/easyspin/documentation/isotopetable.html>

## Details

This dataset was taken form [\`EasySpin\`
toolbox](https://easyspin.org/easyspin/documentation/isotopetable.html)
and only it's format was slightly modified. Therefore, IT CONTAINS THE
ENTIRE INFORMATION LIKE THE ORIGINAL DATASET (see the `Source`). For
better orientation in ENDOR spectra, column with the Larmor/ENDOR
frequencies (in MHz) at 0.35 T was added according to =\>
\$\$\nu\_{\text{ENDOR}}^{} = -
(1/h)\\\mu\_{\text{N}}^{}\\g\_{\text{n}}^{}\\B\\10^{-6}\$\$ where \\h\\
is the Planck's constant, \\\mu\_{\text{N}}^{}\\ is the nuclear magneton
available from [constants](https://r-quantities.github.io/constants/)
package
(`constants::syms$mun`,[`syms`](https://rdrr.io/pkg/constants/man/syms.html)),
\\g\_{\text{n}}^{}\\ is the nuclear \\g\\-factor of the specific nucleus
(reported in the data frame as `g_Nuclear`) and finally, the \\B =
0.35\\\text{T}\\ denotes the magnetic flux density. The negative sign
"\\-\\" mirrors convention to describe the direction of magnetic spin
moments precession (see also
[`eval_nu_ENDOR`](https://jatanrt.github.io/eprscope/reference/eval_nu_ENDOR.md))
either counter-clockwise (\\+\\, if \\\gamma\_{\text{n}}^{} \< 0\\) or
clockwise (\\-\\, if \\\gamma\_{\text{n}}^{} \> 0\\) Please, consult the
[`vignette("datasets")`](https://jatanrt.github.io/eprscope/articles/datasets.md)
as well.

## See also

Other Built-In Datasets:
[`solvents_ds`](https://jatanrt.github.io/eprscope/reference/solvents_ds.md)
