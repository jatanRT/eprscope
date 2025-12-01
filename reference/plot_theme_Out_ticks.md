# Custom `ggplot2` Theme with Axis Ticks Oriented Outside the Panel

Change the [ggplot2](https://ggplot2.tidyverse.org/)-based theme in
order to meet the needs of graph (such as EPR spectrum, kinetic
profiles...etc) visuals/non-data components of the actual plot. The
theme can be mainly applied for the basic `ggplot2` components like
`ggplot() + geom_...() + ...` and consists of highlighted panel borders,
grid and axis ticks pointing **outside of the plot panel**. For details
of `ggplot2` theme elements please, refer to [Modify Components of a
Theme](https://ggplot2.tidyverse.org/reference/theme.html) (see also
[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)) or to
[ggplot2 Elements Demonstration by Henry
Wang](https://henrywang.nl/ggplot2-theme-elements-demonstration/).

## Usage

``` r
plot_theme_Out_ticks(
  axis.text.size = 14,
  axis.title.size = 15,
  grid = TRUE,
  border.line.color = "black",
  border.line.type = 1,
  border.line.width = 0.5,
  bg.transparent = FALSE,
  ...
)
```

## Arguments

- axis.text.size:

  Numeric, text size (in `pt`) for the axes units/descriptions,
  **default**: `axis.text.size = 14`.

- axis.title.size:

  Numeric, text size (in `pt`) for the axes title, **default**:
  `axis.title.size = 15`.

- grid:

  Logical, whether to display the `grid` within the plot/graph panel,
  **default**: `grid = TRUE`.

- border.line.color:

  Character string, setting up the color of the plot panel border line,
  **default**: `border.line.color = "black"`.

- border.line.type:

  Character string or integer, corresponding to width of the graph/plot
  panel border line. Following types can be specified: `0 = "blank"`,
  `1 = "solid"` (**default**), `2 = "dashed"`, `3 = "dotted"`,
  `4 = "dotdash"`, `5 = "longdash"` and `6 = "twodash"`..

- border.line.width:

  Numeric, width (in `mm`) of the plot panel border line, **default**:
  `border.line.width = 0.5`.

- bg.transparent:

  Logical, whether the **entire plot background** (excluding the
  **panel**) should be transparent, **default**:
  `bg.transparent = FALSE`, i.e. no transparent background.

- ...:

  additional arguments specified by the
  [`theme`](https://ggplot2.tidyverse.org/reference/theme.html) (such as
  `panel.backgroud`, `axis.line`,...etc), which are not specified
  otherwise.

## Value

Custom `ggplot2` `theme` (list) with `x,y-axis` ticks pointing outside
of the graph/plot panel.

## See also

Other Visualizations and Graphics:
[`draw_molecule_by_rcdk()`](https://jatanrt.github.io/eprscope/reference/draw_molecule_by_rcdk.md),
[`plot_EPR_Specs()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs.md),
[`plot_EPR_Specs2D_interact()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs2D_interact.md),
[`plot_EPR_Specs3D_interact()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs3D_interact.md),
[`plot_EPR_Specs_integ()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs_integ.md),
[`plot_EPR_present_interact()`](https://jatanrt.github.io/eprscope/reference/plot_EPR_present_interact.md),
[`plot_labels_xyz()`](https://jatanrt.github.io/eprscope/reference/plot_labels_xyz.md),
[`plot_layout2D_interact()`](https://jatanrt.github.io/eprscope/reference/plot_layout2D_interact.md),
[`plot_theme_In_ticks()`](https://jatanrt.github.io/eprscope/reference/plot_theme_In_ticks.md),
[`plot_theme_NoY_ticks()`](https://jatanrt.github.io/eprscope/reference/plot_theme_NoY_ticks.md),
[`present_EPR_Sim_Spec()`](https://jatanrt.github.io/eprscope/reference/present_EPR_Sim_Spec.md)

## Examples

``` r
## loading TMPD built-in example file:
tmpd.data.file.path <-
  load_data_example(file = "TMPD_specelchem_accu_b.asc")
## reading data:
tmpd.data.file <-
  readEPR_Exp_Specs(path_to_ASC = tmpd.data.file.path,
                    col.names = c("B_G","dIepr_over_dB"),
                    qValue = 3500,
                    norm.vec.add = 20,
                    origin = "winepr")
#
ggplot2::ggplot(data = tmpd.data.file,
       ggplot2::aes(x = B_G,y = dIepr_over_dB)
       ) +
  ggplot2::geom_line(linewidth = 0.75,color = "darkgreen") +
  ggplot2::labs(
    x = "B (G)",
    y = "dIepr / dB  (p.d.u.)",
    title = "EPR Spectrum"
  ) +
  plot_theme_Out_ticks()


```
