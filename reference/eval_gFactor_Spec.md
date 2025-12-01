# Calculation of \\g\\-factor ("Position") from the EPR Spectrum/Data

Calculation of g-value according to fundamental formula, see
[`eval_gFactor`](https://jatanrt.github.io/eprscope/reference/eval_gFactor.md).
\\g\\-related magnetic flux density (like \\B\_{\text{iso}}\\ or
\\B\_{\text{center}}\\) is directly taken from the EPR spectrum. If
positive and negative derivative intensities of the spectral line are
similar and their distance from the middle point of the spectrum equals,
the \\B\_{\text{iso}}\\ should be considered. Otherwise, the
\\B\_{\text{center}}\\ must be taken into account. In case of integrated
EPR spectrum/data, the \\B\_{\text{max}}\\ is used for the \\g\\-value
calculation.

## Usage

``` r
eval_gFactor_Spec(
  data.spectr,
  nu.GHz,
  B.unit = "G",
  B = "B_G",
  Intensity = "dIepr_over_dB",
  lineSpecs.form = "derivative",
  Blim = NULL,
  iso = TRUE
)
```

## Arguments

- data.spectr:

  Spectrum data frame object where the magnetic flux density (in `mT` or
  `G` or `T`) column can be labeled as `Field` or `B_G` and that of the
  derivative intensity as `dIepr_over_dB` or single integrated intensity
  like `Integrated_Intensity` (`index` column might be included as
  well).

- nu.GHz:

  Numeric value, microwave frequency in `GHz`.

- B.unit:

  Character string, denoting the magnetic flux density unit e.g.
  `B.unit = "G"` (gauss, **default**) or `B.unit = "mT"`/`"T"`
  (millitesla/tesla).

- B:

  Character string, pointing to magnetic flux density `column` of the
  EPR spectrum data frame `data.spectr` either in "millitesla"/"tesla"
  or in "gauss", that is `B = "B_mT"` (**default**) or
  `B = "B_G"`/`B = "T"` or `B = "Bsim_G"` to include simulated EPR
  spectra as well.

- Intensity:

  Character string, pointing to `intensity column` if other than
  `dIepr_over_dB` name/label is used (e.g. for simulated spectra),
  **default**: `Intesity = "dIepr_over_dB"`

- lineSpecs.form:

  Character string, describing either `"derivative"` (**default**) or
  `"integrated"` (i.e. `"absorption"` which can be used as well) line
  form of the analyzed EPR spectrum/data.

- Blim:

  Numeric vector, magnetic flux density in `mT`/`G`/`T` corresponding to
  lower and upper limit of the selected \\B\\-region, such as
  `Blim = c(3495.4,3595.4)`. **Default**: `Blim = NULL` (corresponding
  to the entire \\B\\-range of the EPR spectrum).

- iso:

  Logical, whether to calculate the \\g\\-factor from the \\B\\-value
  corresponding to that between the `min.` and `max.` derivative
  intensities (`dIepr_over_dB`, that is \\g\_{\text{iso}}\\ (this is the
  **default** one: `iso = TRUE`), or by finding the \\B\\-value
  corresponding to `dIepr_over_dB = 0` (close to zero, which is
  `iso = FALSE`). For the `lineSpecs.form = "integrated"` (or
  `absorptiion`), the `iso` is related to magnetic flux density with
  `max.` intensity.

## Value

Numeric \\g\_{\text{iso}}\\-value ('iso' = 'isotropic') or
\\g\_{\text{center}}\\, from the EPR spectrum, according to
\\(h\\\nu)/(\mu\_{\text{B}}\\B)\\.

## See also

Other Evaluations:
[`eval_DeltaXpp_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_DeltaXpp_Spec.md),
[`eval_FWHMx_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_FWHMx_Spec.md),
[`eval_extremeX_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_extremeX_Spec.md),
[`eval_gFactor()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor.md),
[`eval_interval_cnfd_tVec()`](https://jatanrt.github.io/eprscope/reference/eval_interval_cnfd_tVec.md),
[`eval_kinR_Eyring_GHS()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_Eyring_GHS.md),
[`eval_nu_ENDOR()`](https://jatanrt.github.io/eprscope/reference/eval_nu_ENDOR.md),
[`eval_peakPick_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_peakPick_Spec.md)

## Examples

``` r
## load package built-in EPR spectral data example:
data.file.path <-
  load_data_example(file = "TMPD_specelchem_accu_b.asc")
data.epr <-
  readEPR_Exp_Specs(path_to_ASC = data.file.path,
                    col.names = c("B_G",
                                  "dIepr_over_dB"),
                    qValue = 3500,
                    origin = "winepr")
#
## g_iso calculation from EPR spectrum/data:
eval_gFactor_Spec(data.spectr = data.epr,
                  nu.GHz = 9.814155,
                  B.unit = "mT",
                  B = "B_mT",
                  Blim = c(349.677, 350.457))
#> [1] 2.00304

```
