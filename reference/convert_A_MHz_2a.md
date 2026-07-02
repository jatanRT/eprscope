# Convert Coupling Constants into Splitting Ones.

Converting hyperfine coupling constants (HFCCs, \\A\\ values in `MHz`)
into hyperfine splitting ones (HFSCs, \\a\\ values in `mT`).

## Usage

``` r
convert_A_MHz_2a(A.MHz, g.val = 2.0023193)
```

## Arguments

- A.MHz:

  Numeric value/vector, corresponding to HFCCs in `MHz`.

- g.val:

  Numeric value/vector, corresponding to actual \\g\\-factor
  (`unitless`). **Default:** `g.val = 2.00231930` (corresponding to free
  electron).

## Value

Numeric value/vector corresponding to HFSCs (\\a\\) in `mT`.

## Details

Conversion performed according to the following relation: \$\$a =
A\\h\\10^6 / (g\\\mu\_{\text{B}}\\10^{-3})\$\$ where \\h\\ corresponds
to Planck's constant and \\\mu\_{\text{B}}\\ to Bohr's magneton. Both
constants are obtained by the `constans::syms$h` and
`constants::syms$mub`, respectively, using the
[constants](https://r-quantities.github.io/constants/) package (see
[`syms`](https://rdrr.io/pkg/constants/man/syms.html)). The
\\10^6\\/\\10^{-3}\\ factor is introduced due to specific \\\[A\] =
\text{MHz}\\ \\\rightarrow\\ \\\[a\] = \text{mT}\\ conversion. The
latter is suitable for the EPR simulations and/or ENDOR.

## See also

Other Conversions and Corrections:
[`convert_B()`](https://jatanrt.github.io/eprscope/reference/convert_B.md),
[`convert_a_mT_2A()`](https://jatanrt.github.io/eprscope/reference/convert_a_mT_2A.md),
[`convert_time2var()`](https://jatanrt.github.io/eprscope/reference/convert_time2var.md),
[`correct_time_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/correct_time_Exp_Specs.md)

## Examples

``` r
# all a (HFSCs) in mT
convert_A_MHz_2a(A.MHz = 16)
#> [1] 0.57
#
convert_A_MHz_2a(20,2.0059) # 20 MHz
#> [1] 0.71
#
convert_A_MHz_2a(4,g.val = 2.0036) # 4 MHz
#> [1] 0.14

```
