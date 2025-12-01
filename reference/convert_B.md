# Conversion of Magnetic Flux Density

Conversion of magnetic flux density/field (*B*) values depending on the
input and the required output units.

## Usage

``` r
convert_B(B.val, B.unit, B.2unit)
```

## Arguments

- B.val:

  Numeric value/vector, corresponding to input value(s) of magnetic flux
  density.

- B.unit:

  Character string, referring to input magnetic flux density units.
  Usually `T`, `mT` or `G` are used.

- B.2unit:

  Character string, referring to output magnetic flux density units.
  Usually `T`, `mT` or `G` are used.

## Value

Numeric value or vector as a result of the B conversion. Depending on
the output unit (`B.2unit`) the values are rounded to:

- B.2unit = "T":

  7 decimal places

- B.2unit = "mT":

  4 decimal places

- B.2unit = "G":

  3 decimal places

## See also

Other Conversions and Corrections:
[`convert_A_MHz_2a()`](https://jatanrt.github.io/eprscope/reference/convert_A_MHz_2a.md),
[`convert_a_mT_2A()`](https://jatanrt.github.io/eprscope/reference/convert_a_mT_2A.md),
[`convert_time2var()`](https://jatanrt.github.io/eprscope/reference/convert_time2var.md),
[`correct_time_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/correct_time_Exp_Specs.md)

## Examples

``` r
## simple conversion:
convert_B(B.val = 3500,B.unit = "G",B.2unit = "T")
#> [1] 0.35
#
## conversion of B.seq vector with the Sweep Width = 100 G
## and the central field 3496 G :
B.seq <- seq(3496-100/2,3496+100/2,length.out = 1024)
Bnew <- convert_B(B.seq,B.unit = "G",B.2unit = "mT")
head(as.matrix(Bnew),n = 20)
#>           [,1]
#>  [1,] 344.6000
#>  [2,] 344.6098
#>  [3,] 344.6196
#>  [4,] 344.6293
#>  [5,] 344.6391
#>  [6,] 344.6489
#>  [7,] 344.6587
#>  [8,] 344.6684
#>  [9,] 344.6782
#> [10,] 344.6880
#> [11,] 344.6978
#> [12,] 344.7075
#> [13,] 344.7173
#> [14,] 344.7271
#> [15,] 344.7369
#> [16,] 344.7466
#> [17,] 344.7564
#> [18,] 344.7662
#> [19,] 344.7760
#> [20,] 344.7857

```
