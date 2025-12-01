# Convert Splitting Constants into Coupling Ones.

Converting hyperfine splitting constants (HFSCs, \\a\\ values in `mT`)
into hyperfine coupling ones (HFCCs, \\A\\ values in `MHz`).

## Usage

``` r
convert_a_mT_2A(a.mT, g.val = 2.0023193)
```

## Arguments

- a.mT:

  Numeric value/vector of HFSCs in `mT` ('line distances' from EPR
  spectrum)

- g.val:

  Numeric value/vector, corresponding to actual \\g\\-factor
  (`unitless`). **Default:** `g.val = 2.00231930` (corresponding to free
  electron).

## Value

Numeric value/vector corresponding to HFCCs (\\A\\) in `MHz`.

## Details

Conversion performed according to the following relation: \$\$A =
(a\\g\\\mu\_{\text{B}}) / h\$\$ where \\h\\ corresponds to Planck's
constant and \\\mu\_{\text{B}}\\ to Bohr's magneton. Both latter are
obtained by the `constans::syms$h` and `constants::syms$mub`,
respectively, using the
[constants](https://r-quantities.github.io/constants/) package (see
[`syms`](https://rdrr.io/pkg/constants/man/syms.html)). Conversion is
suitable for the EPR simulations and/or ENDOR.

## See also

Other Conversions and Corrections:
[`convert_A_MHz_2a()`](https://jatanrt.github.io/eprscope/reference/convert_A_MHz_2a.md),
[`convert_B()`](https://jatanrt.github.io/eprscope/reference/convert_B.md),
[`convert_time2var()`](https://jatanrt.github.io/eprscope/reference/convert_time2var.md),
[`correct_time_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/correct_time_Exp_Specs.md)

## Examples

``` r
convert_a_mT_2A(a.mT = 0.5)
#> [1] 14.01
#
convert_a_mT_2A(0.6,2.0059)
#> [1] 16.85
#
convert_a_mT_2A(0.15,g.val = 2.00036)
#> [1] 4.2

```
