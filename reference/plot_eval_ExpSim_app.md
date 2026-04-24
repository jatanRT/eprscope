# Interactive Application to Plot and Evaluate CW Isotropic EPR Spectra

Launch this app in order to quickly check recorded individual EPR
spectra with their instrumental parameters. Additionally, you may try to
interactively simulate the isotropic continuous wave (CW) EPR spectra
using the simple graphical user interface (GUI) based on the [R
Shiny](https://shiny.posit.co/) and several functions from the package
(see `Details`). Screenshots of the app (e.g. visualization of g-Factor
scale as well as interactive simulation) you may find in the
[`vignette("functionality")`](https://jatanrt.github.io/eprscope/articles/functionality.md).

## Usage

``` r
plot_eval_ExpSim_app()
```

## Details

Based on the acquisition EPR spectrum `origin`, by using the graphical
user interface, one can load a spectrum and its
instrumental/experimental parameters by the
[`readEPR_Exp_Specs`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md)
as well as by the
[`readEPR_params_tabs`](https://jatanrt.github.io/eprscope/reference/readEPR_params_tabs.md)
functions, respectively. Such spectrum is immediately depicted in the
interactive `{plotly}` format by
[`plot_EPR_Specs2D_interact`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs2D_interact.md).
The magnetic flux density \\B\\ of the EPR spectrum can be converted
from `mT` to `G` (or vice versa) and to `g`-values. Moreover, if the
compound/radical structure is already known, one can display its
structure by entering its corresponding `SMILES` code/character string,
using the
[`draw_molecule_by_rcdk`](https://jatanrt.github.io/eprscope/reference/draw_molecule_by_rcdk.md).
All the data (spectrum + parameters) can be also used to interactively
simulate the isotropic EPR spectrum by the
[`eval_sim_EPR_iso`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md)
and to visualize it, with its experimental counterpart, by the
[`present_EPR_Sim_Spec`](https://jatanrt.github.io/eprscope/reference/present_EPR_Sim_Spec.md).
Additionally, one can also print and download the corresponding data
frame(s)/table(s) in several formats (`.csv`,`.pdf` or `.xlsx`) and thus
to use the data not only in the `R` environment but also within other
software tools. All instantaneously presented EPR spectra can be also
exported in common formats like `.png`, `.jpeg` as well as `.pdf`.

The result of the isotropic simulation (with the actual parameters) can
be additionally exported as an `.R` code snippet/script to eventually
fit the simulated spectrum (optimize simulation parameters) onto the
experimental one by the
[`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md).
By saving the script in the working directory, the isotropic simulation
fit can be performed right after closing the shiny app (window) in order
to fine tune the simulation parameters and thus to speed up the analysis
of the EPR spectrum. If the shiny application is running within the web
browser, the user may also print/save the entire "dashboard" in the
`.pdf` format to share/present results with/to students or colleagues.

## Examples

``` r
if (FALSE) { # \dontrun{
## run the app just by the following
## command in R console:
plot_eval_ExpSim_app()
#
## this is an examples of the raw
## shiny R code to run the app,
## code example from the `SERVER` part:
server <- function(input, output, session) {
  #
  ## redefinition of `norm.vec.add`
  norm_vec <- reactiveValues(val = 1)
  observe({
    if (isTRUE(input$normVec)){
      norm_vec$val <- as.numeric(
        unlist(strsplit(input$vecNorm,","))
      )
    } else {
      norm_vec$val <- 1
    }
  })
  #
  ## conditions to read either ASCII or binary files
  ## from EPR spectrometer
  ascii.cond <- reactive({
    #
    ## if `input$SpectrumFile` is NULL (no file yet), execution stops silently
    ## here — no error, no downstream cascade.
    shiny::req(input$SpectrumFile)
    #
    ## extension definition
    file.extens <-
      tolower(tools::file_ext(input$SpectrumFile$name))
    #
    ## function and condition for `file.extens`
    extens.map.vec <- c(
      txt = 1,
      asc = 1,
      csv = 1,
      dta = 0,
      spc = 0
    )
    #
    ## ...if the extension is not recognized
    validate(
      need(file.extens %in% names(extens.map.vec),
           paste0("Unrecognised extension: '.", file.extens,
                  "' of EPR data. Accepted: .txt, .csv, .asc, .DTA or .spc"))
    )
    #
    extens.map.vec[[file.extens]]
    #
  })
  #
  ## Load experimental spectrum data frame
  expr_data <- reactive({
    #
    shiny::req(input$ParamsFile)
    shiny::req(input$SpectrumFile)
    #
    ## this is required. otherwise the `bin` files
    ## cannot be loaded (the `ascii` will load anyway)
    ascii.params.path <- input$ParamsFile$datapath
    file.spec.path <- input$SpectrumFile$datapath
    #
    spectr.data <-
      readEPR_Exp_Specs(
        path_to_file = file.spec.path,
        path_to_dsc_par = switch(2 - ascii.cond(),NULL,ascii.params.path),
        col.names = switch(
          3 - origin.cond(orig = input$origin),
          c("index","B_G", "dIepr_over_dB"),
          c("B_G", "dIepr_over_dB"),
          switch(
            2 - ascii.cond(),
            c("B_mT","dIepr_over_dB"),
            c("index","B_G", "dIepr_over_dB")
          )
        ),
        x.unit = switch(
          3 - origin.cond(orig = input$origin),
          "G",
          "G",
          ## `.csv` file with "mT", while binary with "G"
          switch(2 - ascii.cond(),"mT","G")
        ),
        qValue = input$qValue,
        origin = input$origin,
        norm.vec.add = norm_vec$val
      )
    spectr.data
  })
#
# -------------------- INTERACTIVE SPECTRUM ---------------------
#
output$plot <- plotly::renderPlotly({
 #
 ## add g-Value condition:
 if (isTRUE(input$gval)) {
   expr_data_g <- expr_data() %>%
     dplyr::mutate(g_Value =
                     eval_gFactor(
                       nu.val = input$MWnuGHz,
                       B.val = .data$B_mT
                     ))
 }
...})
 }
} # }

```
