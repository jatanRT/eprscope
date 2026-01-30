# Read the ASCII Data of a Simulated EPR Spectrum

Loading the ASCII data like `.txt`,`.asc` or `.csv`, related to
simulated EPR spectrum from different sources like "EasySpin"
(*Matlab*), "Xenon" (EPR spectrometer), "SimFonia" (WinEPR system) or
"csv" (comma separated values, universal format or MS Excel). Finally,
they are automatically converted into data frames by the
[`fread`](https://rdrr.io/pkg/data.table/man/fread.html).

## Usage

``` r
readEPR_Sim_Spec(
  path_to_ASC,
  B.unit = "G",
  col.names.sim = c("Bsim_G", "dIeprSim_over_dB"),
  x.sim.id = 1,
  Intensity.sim.id = 2,
  origin.sim = "easyspin"
)
```

## Arguments

- path_to_ASC:

  Character string, path to ASCII file/table with simulated spectral
  data (\\Intensity\\\\vs\\\\B\\(Field) obtained from various sources.
  Path can be alternatively defined by the
  [`file.path`](https://rdrr.io/r/base/file.path.html) function.

- B.unit:

  Character string, pointing to unit of magnetic flux density coming
  from the original data which is to be presented on the \\B\\-axis of
  an EPR spectrum, like `"G"` ("Gauss"), `"mT"` ("millitesla").
  **Default**: `B.unit = "G"`.

- col.names.sim:

  Character string vector, pointing to column names/headers of the
  original ASCII data (refer also to the `path_to_ASC` argument and
  description). **Default**:
  `col.names.sim = c("Bsim_G","dIeprSim_over_dB")`, corresponding to the
  `origin.sim = "easyspin"`.

- x.sim.id:

  Numeric index related to the `col.names.sim` vector pointing to
  independent variable (like "B" or "Bsim" - magnetic flux density),
  which corresponds to \\x\\-axis in the simulated spectra (refer also
  to the original ASCII data `path_to_ASC` of the simulated spectrum).
  **Default**: `x.sim.id = 1` (see also default of the `col.names.sim`).

- Intensity.sim.id:

  Numeric index related to the `col.names.sim` vector pointing to
  simulated EPR intensity column name/header in the original ASCII data
  (refer also to the `path_to_ASC` argument and description). If used
  together with the quantification of radicals, this argument must be
  equal to that of the
  [`quantify_EPR_Sim_series`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Sim_series.md).
  **Default**: `x.sim.id = 2` (see also default of the `col.names.sim`).

- origin.sim:

  Character string, referring to the "origin" of a simulated spectrum
  ASCII data. There are four possibilities \\\Rightarrow\\
  `sim.orimgin = "easyspin"` (**default**), `"xenon"`, `"simfonia"` as
  well as universal `"csv"`.

## Value

Data frame, consisting of magnetic flux density and intensity
variable/column (depending on the `col.names.sim` and `.id` arguments),
corresponding to simulated EPR spectrum.

## See also

Other Data Reading:
[`readEPR_Exp_Specs()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs.md),
[`readEPR_Exp_Specs_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_kin.md),
[`readEPR_Exp_Specs_multif()`](https://jatanrt.github.io/eprscope/reference/readEPR_Exp_Specs_multif.md),
[`readEPR_param_slct()`](https://jatanrt.github.io/eprscope/reference/readEPR_param_slct.md),
[`readEPR_params_slct_kin()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_kin.md),
[`readEPR_params_slct_quant()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_quant.md),
[`readEPR_params_slct_sim()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_sim.md),
[`readEPR_params_tabs()`](https://jatanrt.github.io/eprscope/reference/readEPR_params_tabs.md),
[`readEPR_solvent_props()`](https://jatanrt.github.io/eprscope/reference/readEPR_solvent_props.md),
[`readMAT_params_file()`](https://jatanrt.github.io/eprscope/reference/readMAT_params_file.md)

## Examples

``` r
if (FALSE) { # \dontrun{
readEPR_Sim_Spec(path_to_ASC =
                  "./Simulations/TEMPO_simulation.txt",
                 origin.sim = "xenon")
#
readEPR_Sim_Spec("Cu_complex_simulation.txt",
                 B.unit = "mT",
                 col.names.sim = c("Bsim_G","dIeprSim_over_dB"),
                 x.sim.id = 1,
                 Intensity.sim.id = 2,
                 origin.sim = "easyspin")
} # }


```
