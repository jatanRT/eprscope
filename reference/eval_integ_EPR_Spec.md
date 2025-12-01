# Integration of EPR Spectrum/Data for Quantitative Analysis

Evaluates integrals of EPR spectra (based on the
[`pracma::cumtrapz`](https://rdrr.io/pkg/pracma/man/trapz.html)
function) depending on input data =\> either corresponding to
`derivative` or single `integrated` EPR signal form, with the option to
correct the single integral baseline by the polynomial fit of the
`poly.degree` level. **For EPR time/temperature/...etc spectral
series**, (data frame must be available in [tidy/long table
format](https://r4ds.had.co.nz/tidy-data.html)), there is an **option to
integrate all EPR spectra literally in one step** (see also `Examples`),
similarly to function available in acquisition/processing software at
EPR spectrometers.

## Usage

``` r
eval_integ_EPR_Spec(
  data.spectr,
  B = "B_G",
  B.unit = "G",
  Intensity = "dIepr_over_dB",
  lineSpecs.form = "derivative",
  Blim = NULL,
  correct.integ = FALSE,
  BpeaKlim = NULL,
  poly.degree = NULL,
  sigmoid.integ = FALSE,
  output.vecs = FALSE
)
```

## Arguments

- data.spectr:

  Spectrum data frame/table object with magnetic flux density (in `mT`
  or `G` or `T`) and that of the derivative or already single integrated
  intensity. `index` column may be already present as well.

- B:

  Character string, pointing to magnetic flux density column header (in
  the original `data.spectr`) either in `millitesla`/`tesla` or in
  `Gauss`, such as `B = "B_mT"` or `B = "B_G"` (**default**) or
  `B = "Field"`...etc.

- B.unit:

  Character string pointing to unit of magnetic flux density, which is
  to be presented on \\x(B)\\-axis of the EPR spectrum, like `"G"`
  ("Gauss"), `"mT"` ("millitesla") or `"T"` ("Tesla"). **Default**:
  `B.unit = "G"`.

- Intensity:

  Character string, pointing to column name of either derivative (e.g.
  `Intensity = "dIepr_over_dB"`, **default**) or single integrated EPR
  spectrum (e.g. `Intensity = "single_Integrated"`) within the actual
  `data.spectr`.

- lineSpecs.form:

  Character string, describing either `"derivative"` (**default**) or
  `"integrated"` (i.e. `"absorption"`, which can be used as well) line
  form of the analyzed EPR spectrum/data.

- Blim:

  Numeric vector, magnetic flux density in `mT`/`G`/`T` corresponding to
  lower and upper limit of the selected \\B\\-region, e.g.
  `Blim = c(3495.4,3595.4)`. **Default**: `Blim = NULL` (corresponding
  to the entire spectral \\B\\-range).

- correct.integ:

  Logical, whether to correct the integral by baseline polynomial model
  fit. **Default**: `correct.integ = FALSE`.

- BpeaKlim:

  Numeric vector, magnetic flux density in `mT`/`G`/`T` corresponding to
  lower and upper limit of the SELECTED \\B\\-PEAK REGION, e.g.
  `BpeaKlim = c(3535.4,3555.4)`. This is the region (without the peak),
  which is actually not considered for the baseline fit.

- poly.degree:

  Numeric, degree of the polynomial function used to fit baseline under
  the single integrated curve of the original EPR spectrum (see also
  `BpeaKlim`).

- sigmoid.integ:

  Logical, whether to involve (column in data frame) double integral or
  single integral (if the `data.spectr` and `Intesity` are already in
  single integrated form), in sigmoid shape, which is required for the
  quantitative analysis, **default**: `sigmoid.integ = FALSE`.

- output.vecs:

  Logical, whether the "integral" columns are presented within the
  original `data.spectr` data frame (`output.vecs = FALSE`, **default**)
  or called as a vectors or list for additional processing of spectral
  data series by [dplyr](https://dplyr.tidyverse.org/) (see `Value` and
  `Examples`).

## Value

The integration results may be divided into following types, depending
on the above-described arguments. Generally, they are either data
frames, including the original data and the integrals
(`output.vecs = FALSE`) or vectors/vectors list, corresponding to
individual baseline corrected/uncorrected integrals
(`output.vecs = TRUE`). This is especially useful for spectral (time)
series EPR data, which can be handily processed by the
[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html) using
the `pipe` operators (`%>%`).

1.  Data frame/table, including EPR spectral data (general `Intensity`
    (integrated or derivative) *vs* \\B\\) as well as its corresponding
    `single` (column `single_Integ`) integral. This is the case if only
    a single uncorrected integral is required.

2.  Data frame/table with single integral/intensity already corrected by
    a certain degree of polynomial baseline (fitted to experimental
    baseline without peak). Single integrals are related either to
    derivative or already integrated EPR spectra where corrected
    integral column header is denoted as `single_Integ_correct`. This is
    the case if `correct.integ = TRUE` and `sigmoid.integ = FALSE` +
    `output.vecs = FALSE`.

3.  Data frame with `single` and `double/sigmoid` integral
    column/variable (`sigmoid_Integ`), essential for the quantitative
    analysis. For such case it applies: `output.vecs = FALSE` and
    `correct.integ = FALSE`.

4.  Data frame in case of `correct.integ = TRUE`, `sigmoid.integ = TRUE`
    and `output.vecs = FALSE`. It contains the original data frame
    columns + corrected single integral (`single_Integ_correct`) and
    double/sigmoid integral (`sigmoid_Integ`) which is evaluated from
    the baseline corrected single one. Therefore, such double/sigmoid
    integral is suitable for the accurate determination of radical
    (paramagnetic centers) amount.

5.  Numeric vector, corresponding to baseline uncorrected/corrected
    single integral in case of `sigmoid.integ = FALSE` +
    `output.vecs = TRUE`.

6.  List of numeric vectors (if `output.vecs = TRUE`) corresponding to:

    - single:

      Corrected or uncorrected single integral (in case of derivative
      form), depending on the `correct.integ` argument.

    - sigmoid:

      Double integral (in case of derivative form) or single integral
      (in case of integrated spectral form) for quantitative analysis.

## Details

The relative error of the cumulative trapezoidal (`cumtrapz`) function
is minimal, usually falling into the range of \\\langle 1,5\rangle\\\\\\
or even lower, depending on the spectral data resolution (see Epperson
JF (2013) and Seeburger P (2023) in the `References`). Therefore, the
better the resolution, the more accurate the integral. If the initial
EPR spectrum displays low signal-to-noise ratio, the integral often
looses its sigmoid-shape and thus, the EPR spectrum has to be either
simulated (see also
[`vignette("functionality")`](https://jatanrt.github.io/eprscope/articles/functionality.md))
or smoothed by the
[`smooth_EPR_Spec_by_npreg`](https://jatanrt.github.io/eprscope/reference/smooth_EPR_Spec_by_npreg.md),
prior to integration. Afterwards, integrals are evaluated from the
simulated or smoothed EPR spectra. For the purpose of quantitative
analysis, the integrals are evaluated using the `B.units = "G"` (see
Arguments). Hence, depending on \\B\\-unit (`G` or `mT` or `T`) each
resulting integral column have to be optionally (in case of `mT` or `T`)
multiplied by the factor of `10` or `10000`, respectively, because
\\1\\\text{mT}\equiv 10\\\text{G}\\ and \\1\\\text{T}\equiv
10^4\\\text{G}\\. Such corrections are already included in the
function/script. Instead of "double integral/integ." the term "sigmoid
integral/integ." is used. "Double integral" **in the case of originally
single integrated EPR spectrum** (see `data.spectr` and `Intensity`)
**is confusing. In such case, the EPR spectrum is integrated just
once.**

## References

Weber RT (2011). *Xenon User's Guide*. Bruker BioSpin Manual Version
1.3, Software Version 1.1b50.

Hans W. Borchers (2023). *pracma: Practical Numerical Math Functions*. R
package version 2.4.4,
<https://cran.r-project.org/web/packages/pracma/index.html>.

Seeburger P (2023). “Numerical Integration - Midpoint, Trapezoid,
Simpson's rule”, <http://tinyurl.com/trapz-integral>.

Svirin A (2023). “Calculus, Integration of Functions-Trapezoidal Rule”,
<https://math24.net/trapezoidal-rule.html>.

Epperson JF (2013). *An Introduction to Numerical Methods and Analysis*.
Wiley and Sons, ISBN 978-1-118-36759-9,
<https://books.google.cz/books?id=310lAgAAQBAJ>.

## See also

Other Evaluations and Quantification:
[`eval_kinR_EPR_modelFit()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md),
[`eval_kinR_ODE_model()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_ODE_model.md),
[`quantify_EPR_Abs()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Abs.md),
[`quantify_EPR_Norm_const()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Norm_const.md)

## Examples

``` r
## loading the built-in package example
## time series EPR spectra:
triarylamine.decay.series.dsc.path <-
load_data_example(file =
        "Triarylamine_radCat_decay_series.DSC")
triarylamine.decay.series.asc.path <-
load_data_example(file =
        "Triarylamine_radCat_decay_series.zip")
unzip(triarylamine.decay.series.asc.path,
      exdir = tempdir()
      )
## loading the kinetics:
triarylamine.decay.series.data <-
  readEPR_Exp_Specs_kin(name.root =
    "Triarylamine_radCat_decay_series",
    dir_ASC = tempdir(),
    dir_dsc_par =
      system.file("extdata",
                  package = "eprscope")
   )
#
## select the first spectrum
triarylamine.decay.series.data1st <-
   triarylamine.decay.series.data$df %>%
     dplyr::filter(time_s ==
       triarylamine.decay.series.data$time[1])
#
## integrate the first spectrum with default arguments
triarylamine.decay.data1st.integ01 <-
  eval_integ_EPR_Spec(triarylamine.decay.series.data1st)
#
## data frame preview
head(triarylamine.decay.data1st.integ01)
#>          B_G time_s  dIepr_over_dB      B_mT  single_Integ
#>        <num>  <num>          <num>     <num>         <num>
#> 1: 3390.0000      6  1.3629316e-05 339.00000 1.0353596e-04
#> 2: 3390.0833      6 -1.0134169e-06 339.00833 1.0406162e-04
#> 3: 3390.1667      6 -1.9794802e-05 339.01667 1.0319461e-04
#> 4: 3390.2500      6 -2.9826537e-05 339.02500 1.0112706e-04
#> 5: 3390.3333      6 -1.6870754e-05 339.03333 9.9181337e-05
#> 6: 3390.4167      6  2.5629187e-06 339.04167 9.8585177e-05
#
## integration (including baseline correction)
## of the 1st spectrum from the series
triarylamine.decay.data1st.integ02 <-
  eval_integ_EPR_Spec(triarylamine.decay.series.data1st,
    ## limits obtained from interactive spectrum:
    BpeaKlim = c(3471.5,3512.5),
    Blim = c(3425,3550),
    correct.integ = TRUE,
    poly.degree = 3,
    sigmoid.integ = TRUE
    )
#
## data frame preview
head(triarylamine.decay.data1st.integ02)
#> # A tibble: 6 × 8
#>     B_G time_s dIepr_over_dB  B_mT single_Integ baseline_Integ_fit
#>   <dbl>  <dbl>         <dbl> <dbl>        <dbl>              <dbl>
#> 1 3425       6   -0.0000160   342.    0.0000326          0.0000227
#> 2 3425.      6   -0.00000338  343.    0.0000317          0.0000227
#> 3 3425.      6    0.0000173   343.    0.0000323          0.0000226
#> 4 3425.      6    0.0000157   343.    0.0000337          0.0000226
#> 5 3425.      6   -0.0000271   343.    0.0000332          0.0000226
#> 6 3425.      6    0.00000128  343.    0.0000322          0.0000225
#> # ℹ 2 more variables: single_Integ_correct <dbl>, sigmoid_Integ <dbl>
#
## plot the single integrated EPR spectrum,
## including baseline correction
plot_EPR_Specs(triarylamine.decay.data1st.integ02,
               x = "B_G",
               x.unit = "G",
               Intensity = "single_Integ_correct",
               lineSpecs.form = "integrated"
             )

#
## plot, corresponding to double/sigmoid integral,
## which is related to corrected single integral
plot_EPR_Specs(triarylamine.decay.data1st.integ02,
               x = "B_G",
               x.unit = "G",
               Intensity = "sigmoid_Integ",
               lineSpecs.form = "integrated"
             )

#
## vectorized output of the uncorrected `sigmoid_integral`
triarylamine.decay.data1st.integ03 <-
  eval_integ_EPR_Spec(triarylamine.decay.series.data1st,
                      sigmoid.integ = TRUE,
                      output.vecs = TRUE)[["sigmoid"]]
#
## preview of the first 6 values
triarylamine.decay.data1st.integ03[1:6]
#> [1] 0.0000000e+00 8.6498993e-06 1.7285576e-05 2.5798979e-05 3.4145162e-05
#> [6] 4.2385433e-05
#
## incorporation of vectorized integration into
## data "pipe" ("%>%") `dplyr` processing of EPR spectral
## time series, creating column with `sigmoid` integral
## where its corresponding single integral (intensity)
## has undergone a baseline correction, finally the max. value
## of all sigmoid integrals along with the time is
## summarized in data frame for quantitative kinetic analysis
triarylamine.decay.data.integs <-
  triarylamine.decay.series.data$df %>%
  dplyr::group_by(time_s) %>%
  dplyr::filter(dplyr::between(B_G,3425,3550)) %>%
  dplyr::mutate(sigmoid_Integ =
    eval_integ_EPR_Spec(dplyr::pick(B_G,dIepr_over_dB),
                        correct.integ = TRUE,
                        BpeaKlim = c(3471.5,3512.5),
                        poly.degree = 3,
                        sigmoid.integ = TRUE,
                        output.vecs = TRUE)$sigmoid
                       ) %>%
  dplyr::summarize(Area = max(sigmoid_Integ))
## in such case `Blim` range is not defined by `eval_integ_EPR_Spec`,
## the `Blim = NULL` and `dplyr::between()` must be set !!!
#
## preview of the final data frame
head(triarylamine.decay.data.integs)
#> # A tibble: 6 × 2
#>   time_s   Area
#>    <dbl>  <dbl>
#> 1      6 0.0169
#> 2     21 0.0179
#> 3     36 0.0169
#> 4     51 0.0172
#> 5     66 0.0184
#> 6     81 0.0158
#
## preview of the simple plot
ggplot2::ggplot(triarylamine.decay.data.integs) +
  ggplot2::geom_point(ggplot2::aes(x = time_s,y = Area))

#
## this does not correspond to example
## in `eval_kinR_EPR_modelFit`, `eval_kin_EPR_ODE_model`
## or in `plot_theme_NoY_ticks` based on the same input data,
## as those `Area` vs `time` relations were evaluated using
## the simulated EPR spectra (see also `vignette("datasets")`)
#
if (FALSE) { # \dontrun{
## Similar to previous data processing, creating both: corrected
## single integral + sigmoid integral for each time within the spectral
## series. Sigmoid integral was evalutated from the single one by
## `cumtrapz()` function from `pracma` package and finally re-scaled.
triarylamine.decay.data.integs <-
  triarylamine.decay.series.data$df %>%
  dplyr::group_by(time_s) %>%
  eval_integ_EPR_Spec(correct.integ = TRUE,
                      Blim = c(3425,3550),
                      BpeaKlim = c(3472.417,3505.5),
                      poly.degree = 3) %>%
 dplyr::group_by(time_s) %>%
 dplyr::mutate(sigmoid_Integ =
   pracma::cumtrapz(B_G,single_Integ_correct)[,1]) %>%
 dplyr::mutate(sigmoid_Integ_correct =
   abs(min(sigmoid_Integ) - sigmoid_Integ))
} # }

```
