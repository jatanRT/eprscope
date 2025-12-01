# Rearrangement of \\A\_{iso}\\/\\a\_{iso}\\ from the Quantum Chemical (QCH) Computations

Providing table, based on Gaussian/ORCA/...etc. output text files in
order to summarize the mean \\A\_{iso}\\/\\a\_{iso}\\ values of groups
with equivalent nuclei, according to proposed molecular
structure/symmetry.

## Usage

``` r
rearrange_aAiso_QCHcomp(path_to_ASC, col.names, nuclei.list.slct)
```

## Arguments

- path_to_ASC:

  Character string, pointing to path of ASCII file (`txt`,`csv`...etc,
  it may be also provided by the
  [`file.path`](https://rdrr.io/r/base/file.path.html) function). The
  file must include characteristic \\A\_{iso}\\ or \\a\_{iso}\\ values.

- col.names:

  Character string vector, containing names of all columns from QCH
  computational output, for the names see example in `Details`, they
  must contain atomic/structure number, isotop value with element label
  (nucleus characterization) and \\A\\ in MHz as well as \\a\\ in Gauss.

- nuclei.list.slct:

  List of numeric values for the rearrangement of selected atoms/nuclei
  according to symmetry, e.g.
  `nuclei.list.slct <- list(3,c(21,22),c(20,23),c(24,25),c(27,26))`
  where the numbers correspond to indices of proposed equivalent nuclei
  in the ASCII text file.

## Value

Data frame/Table of \\A\_{iso}\\/\\a\_{iso}\\ mean values corresponding
to groups of equivalent nuclei within the structure/symmetry.

## Details

The \\A\_{iso}\\/\\a\_{iso}\\ values are computed for each atom/nucleus
(with its corresponding `atomic number within the structure` as well as
with the characteristic `isotopic number/value`), such an entire table
can be copied e.g. from **Gaussian** output (after
`'Isotropic Fermi Contact Couplings'` line) or can be constructed from
**ORCA** (or any other) output, example for such a file structure (from
**Gaussian**):

|             |                  |               |           |               |
|-------------|------------------|---------------|-----------|---------------|
| **No_atom** | **Atom_Nucleus** | **MegaHertz** | **Gauss** | **1e-4_cm-1** |
| 1           | N(14)            | 0.00643       | 0.00229   | 0.00214       |
| 17          | N(14)            | 13.99707      | 4.9945    | 4.66892       |
| 28          | H(1)             | 16.34971      | 5.83398   | 5.45368       |

The input table/data frame, like the previous one, must include
following columns: atomic/nucleus number, atom/nucleus notation,
hyperfine coupling constant in `MHz` and finally hyperfine splitting
constant in `G`. These columns/variables are essential for the
evaluation.

## See also

Other Evaluations and Quantum Chemistry:
[`eval_gFactor_QCHcomp()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor_QCHcomp.md),
[`rearrange_aAiso_QCHorgau()`](https://jatanrt.github.io/eprscope/reference/rearrange_aAiso_QCHorgau.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rearrange_aAiso_QCHcomp(
"./iso_values_Gaussian.txt",
c("No","Nucleus","au","Megahertz","Gauss","10^n4_cm^n1"),
list(3,c(21,22),c(20,23),c(24,25),c(27,26))
)
} # }

```
