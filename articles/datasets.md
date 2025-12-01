# Package Datasets

## Built-In Datasets

There are two important datasets built into the
[eprscope](https://jatanrt.github.io/eprscope/) 📦. The first one is the
nuclear isotope table, summarizing the essential properties of nuclei in
order to analyze the EPR spectra with HF (hyperfine) structure. While,
the second one shows the solvent properties important for variable
temperature (VT), double-resonance (ENDOR) and EPR
spectroelectrochemical experiments.

### Nuclear Isotope Data Frame

This dataset was taken from the open source
[EasySpin](https://easyspin.org/easyspin/documentation/isotopetable.html)
package, reformatted and column of Larmor-frequencies in $MHz$ at
$0.35\,{mT}$ were added for better orientation in double-resonance ENDOR
spectra, see the details in
[`?isotopes_ds`](https://jatanrt.github.io/eprscope/reference/isotopes_ds.md)
documentation as well as [R file in \`data-raw\`
folder](https://github.com/jatanRT/eprscope/blob/master/data-raw/isotopes_ds.R).
The data frame is not only used to analyze the ENDOR spectra however, it
is also essential for the simulations of EPR spectra and their fitting
onto the experimental ones. See the documentation of the
[`eval_sim_EPR_iso()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md),
[`eval_sim_EPR_isoFit()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md)
as well as
[`eval_nu_ENDOR()`](https://jatanrt.github.io/eprscope/reference/eval_nu_ENDOR.md).

``` r

# interactive data frame by `{DT}` package with option to select columns
# and to save table as `.csv`, `.pdf` or `.xlsx` format
DT::datatable(isotopes_ds,
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",
    buttons = c("colvis","csv","pdf","excel")
  ),
  caption = "Dataset with the following variables/columns: Proton number, isotope,
  stability (either stable, STB or radio-active, RA), isotope name, 
  nuclear spin quantum number, nuclear g-value, natural abundance in %, 
  nuclear quadrupolar moment in Barns and the ENDOR/Larmor frequency 
  in MHz at 0.35 T. The negative sign of Larmor frequency values 
  points to clockwise precession direction according to convention."
  ) %>%
  DT::formatRound("nu_ENDOR_MHz_035T",digits = 3) # rounding must be also performed by the `DT` pkg.
```

### Solvent Properties

When performing EPR experiments, especially the **X-band continuous wave
(CW) spectroscopy in solution**, solvent plays an important role.
Namely, prior to measurement one has to decide which type of cell will
be applied depending on solvent polarity. For polar solvents, such as
acetonitrile or dimethyl sulfoxide, either capillary (with
$i.d. \leq 1\,{mm}$ ) or special [quartz flat
cell](https://sp-wilmadlabglass.com/product/standard-te102-aqueous-cells/)
( with a flat-part thickness $\approx (0.3 - 0.6)\,{mm}$) must be used.
While for non-polar solvents or those with lower polarity, e.g. toluene,
chloroform or tetrahydrofuran, a sample can be analyzed within any kind
of cell including [common EPR
quartz-tubes](https://sp-wilmadlabglass.com/product/4-mm-thin-wall-quartz-epr-sample-tube-250-mm-l/)
with $i.d. \approx (2 - 4)\,{mm}$. Additionally, the solvent properties
like melting/boiling point as well as viscosity are essential for the
variable temperature (VT) experiments and particularly for the CW ENDOR
as well as for spectroelectrochemical ones. Table details can be found
in the
[`?solvents_ds`](https://jatanrt.github.io/eprscope/reference/solvents_ds.md)
documentation and in the [R file in \`data-raw\`
folder](https://github.com/jatanRT/eprscope/blob/master/data-raw/solvents_ds.R).
Solvent properties can be also obtained by the specialized
[`readEPR_solvent_props()`](https://jatanrt.github.io/eprscope/reference/readEPR_solvent_props.md)
function.

``` r

# similar interactive table like before
DT::datatable(solvents_ds,
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",
    buttons = c("colvis","csv","pdf","excel"),
    columnDefs = list(list(visible=FALSE, targets=c(7))) # hide solubility/miscibility column 
  ),
  caption = "Dataset with the following variables/columns: solvent name, 
  molecular formula, relative molecular weight, boiling point in °C, 
  melting point in °C, density in g/mL, solubility 
  in g/(100 g of water)-not shown, visibility can be switched 
  by the 'column visibility', relative electric permittivity, 
  flash point in °C and dynamic viscosity in cp."
)
```

## Datasets for Examples and Tests

These involve the [ASCII
text](https://www.computerhope.com/jargon/a/ascii.htm) data (with the
extensions like `.txt` , `.asc` , `.csv` ), coming from EPR
spectrometers and correspond either to an EPR spectrum data
frame[¹](#fn1) or accompanying ASCII text files with the (instrumental)
parameters (having the `.par` or `.DSC` / `.dsc` extensions) used to
record the corresponding spectra. While the files containing parameters
are generated automatically during the data saving, the tabular ones,
related to an EPR spectrum, must be generated by the instrument operator
(usually by `File` ➝ `Export ASCII` workflow). Some of these files are
compressed (as `.zip`) in order to save storage space within the
package. Additionally, the
[eprscope](https://jatanrt.github.io/eprscope/) 📦 contains `.mat`
(*Matlab*) file from the *EasySpin* simulation as well as output from
DFT quantum chemical computation or related structural data either with
`.inp.log.zip` or `.sdf` extensions, respectively. All these file types
are summarized within the following table.

[TABLE]

------------------------------------------------------------------------

1.  Usually the following variables/columns are included in the ASCII
    tables of the corresponding EPR spectra: “**index**”, “**Field
    \[G\]**”/“**X \[G\]**”/“**RF \[MHz\]**” and “**Intensity
    \[\]**”/“**Intensity**”
