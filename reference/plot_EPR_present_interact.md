# Present/Save Interactive Plot for Publication in `.html`,`.pdf` or `.docx`

Add the interactive plots (see
[`plot_EPR_Specs2D_interact`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs2D_interact.md)
or
[`plot_EPR_Specs3D_interact`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs3D_interact.md))
to various document formats like `.html`, `.pdf` or `.docx`. Function is
based on the
[`saveWidget`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html) as
well as on the
[`webshot`](https://rstudio.github.io/webshot2/reference/webshot.html)
with the help of [knitr](https://github.com/yihui/knitr) package
functions. Depending on the output format, plot is saved in
working/actual directory either as `.html` or as `.png`. The file name
inherits the name of the object/variable (see argument `p` and example).
Afterwards, during the [R markdown](https://rmarkdown.rstudio.com/docs/)
or [Quarto](https://quarto.org/docs/guide/) processing, the image is
automatically attached to the document in the above-described format.
Therefore, this function is quite handy in interactive notebooks (such
as `.Rmd` or `.qmd`, please also consult the
[`create_qmdReport_proj`](https://jatanrt.github.io/eprscope/reference/create_qmdReport_proj.md)
function).

## Usage

``` r
plot_EPR_present_interact(p, size.width = 7, size.height = 5, res.ppi = 200)
```

## Arguments

- p:

  Plot object/variable name.

- size.width:

  Numeric, `width` of the image/object window in `in`, **default**:
  `size.width = 7`.

- size.height:

  Numeric, `height` of the image/object window in `in`, **default**:
  `size.height = 5`.

- res.ppi:

  Numeric, `resolution` in `ppi`, **default**: `res.ppi = 200`.

## Value

Interactive plot format corresponding to that of the output document. If
the desired document format \\\equiv\\ `.html`, interactive plotly graph
is saved in working/actual directory in the same format. Otherwise, for
`.pdf` and `.docx` it is saved as `.png` bitmap with the resolution of
`size.with`\\\cdot\\`res.ppi` x `size.height`\\\cdot\\`res.ppi`.

## See also

Other Visualizations and Graphics:
[`draw_molecule_by_rcdk()`](https://jatanrt.github.io/eprscope/reference/draw_molecule_by_rcdk.md),
[`plot_EPR_Specs()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs.md),
[`plot_EPR_Specs2D_interact()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs2D_interact.md),
[`plot_EPR_Specs3D_interact()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs3D_interact.md),
[`plot_EPR_Specs_integ()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs_integ.md),
[`plot_labels_xyz()`](https://jatanrt.github.io/eprscope/reference/plot_labels_xyz.md),
[`plot_layout2D_interact()`](https://jatanrt.github.io/eprscope/reference/plot_layout2D_interact.md),
[`plot_theme_In_ticks()`](https://jatanrt.github.io/eprscope/reference/plot_theme_In_ticks.md),
[`plot_theme_NoY_ticks()`](https://jatanrt.github.io/eprscope/reference/plot_theme_NoY_ticks.md),
[`plot_theme_Out_ticks()`](https://jatanrt.github.io/eprscope/reference/plot_theme_Out_ticks.md),
[`present_EPR_Sim_Spec()`](https://jatanrt.github.io/eprscope/reference/present_EPR_Sim_Spec.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## plotting EPR spectrum of verdazyl radical (`verdazyl.rad.data`),
## where the actual plot is stored under
## the `verdazyl.epr.plot.interact` variable name
verdazyl.epr.plot.interact <-
  plot_EPR_Specs2D_interact(data.spectra = verdazyl.rad.data)
#
## afterwards, it is automatically transformed and attached
## to document with the desired format by `knitr`
plot_EPR_present_interact(verdazyl.epr.plot.interact)
#
## remaining image files are stored in the working directory
} # }

```
