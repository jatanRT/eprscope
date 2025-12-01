# Rearrangement of \\A\_{iso}\\/\\a\_{iso}\\ from the Gaussian & ORCA Computations

Providing table, specifically from `Gaussian` or `ORCA` output text
files to summarize the \\A\_{iso}\\/\\a\_{iso}\\ mean values of groups
with equivalent nuclei, according to proposed molecular
structure/symmetry (see also the
[`rearrange_aAiso_QCHcomp`](https://jatanrt.github.io/eprscope/reference/rearrange_aAiso_QCHcomp.md)).

## Usage

``` r
rearrange_aAiso_QCHorgau(
  path_to_QCHoutput,
  N.nuclei,
  nuclei.list.slct,
  origin = "gaussian",
  output.text.origin = FALSE,
  output.text.path = NULL
)
```

## Arguments

- path_to_QCHoutput:

  Character string, corresponding to path of `Gaussian` or `ORCA` output
  text files.

- N.nuclei:

  Numeric value that equals to number of atoms/nuclei within the
  calculated structure.

- nuclei.list.slct:

  List of numeric values for the rearrangement of selected atoms/nuclei
  according to symmetry, e.g.
  `nuclei.list.slct <- list(3,c(21,22),c(20,23),c(24,25),c(27,26))`
  where the numbers correspond to indices of proposed equivalent nuclei
  in the ASCII text file.

- origin:

  Character string, pointing to origin of DFT EPR calculation parameters
  \<=\> which software package was used. Only two values are available
  =\> `"gaussian"` (**default**) or `"orca"`.

- output.text.origin:

  Logical, whether to write a text file containing the extracted
  \\A\_{iso}\\/\\a\_{iso}\\ values from the the original output file
  defined by the `path_to_QCHoutput`. **Default**:
  `output.text.origin = FALSE`.

- output.text.path:

  Character string, setting the path to file containing the extracted
  \\A\_{iso}\\/\\a\_{iso}\\ values from the original output file defined
  by the `path_to_QCHoutput`. See also the previous argument.

## Value

Data frame/Table of \\A\_{iso}\\/\\a\_{iso}\\ mean values, corresponding
to groups of proposed equivalent nuclei within the structure/symmetry
constructed directly from *Gaussian* or *ORCA* output text files.

## See also

Other Evaluations and Quantum Chemistry:
[`eval_gFactor_QCHcomp()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor_QCHcomp.md),
[`rearrange_aAiso_QCHcomp()`](https://jatanrt.github.io/eprscope/reference/rearrange_aAiso_QCHcomp.md)

## Examples

``` r
## built-in file and path
gauss.file.path <-
  load_data_example(file = "TMPDAradCatEPRa.inp.log.zip")
gauss.file <- unzip(gauss.file.path)
symmetry.As.df <-
  rearrange_aAiso_QCHorgau(gauss.file,
    N.nuclei = 28,
    nuclei.list.slct =
    list(c(7, 8), ## 2 x 14N
         c(13, 14, 15, 16), ## 4 x 1H (aromatic)
         c(17, 18, 19, 20,
           21, 22, 23, 24,
           25, 26, 27, 28) ## 12 x 1H (methyl groups)
         )
     )
#
## preview
symmetry.As.df
#> # A tibble: 3 × 3
#>   NuclearGroup                                  Aiso_MHz_QCH aiso_mT_QCH
#>   <chr>                                                <dbl>       <dbl>
#> 1 12 x 1H (17,18,19,20,21,22,23,24,25,26,27,28)        20.25        0.72
#> 2 2 x 14N (7,8)                                        17.52        0.63
#> 3 4 x 1H (13,14,15,16)                                  5.24        0.19

```
