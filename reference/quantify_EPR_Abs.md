# Absolute Quantification of Radicals/Spins

Estimate the number (or concentration) of "spins"/radicals/paramagnetic
species using the absolute quantitative method by sigmoid integral as
well as by the instrumental parameters without the need for a standard
sample with known concentration/amount of radicals/"spins". **The
calculation assumes that the sample height middle point, within an EPR
tube, matches the cavity/resonator center**.

## Usage

``` r
quantify_EPR_Abs(
  integ.sigmoid.max,
  instrum.params = NULL,
  path_to_dsc_par,
  origin = "xenon",
  qValue = NULL,
  tube.sample.id.mm,
  point.sample.factor = 8.51e-09,
  fill.sample.h.mm,
  eff.cavity.h.mm = 23,
  fn.B1.Bm.fit.y = c(1.00179, -0.00307086, -0.0265409, 0.000297603, 0.000223277,
    -4.53833e-06, -4.1451e-07, 1.89417e-08, -1.48241e-09),
  fn.B1.Bm.fit.y.max = 0.28,
  Norm.const = NULL,
  Temp.K = NULL,
  S = 0.5
)
```

## Arguments

- integ.sigmoid.max:

  Numeric value or vector of the entire EPR spectrum sigmoid integral.

- instrum.params:

  Named numeric vector, containing instrumental parameters required for
  the quantification =\>

  |         |                                                                                      |
  |---------|--------------------------------------------------------------------------------------|
  | `PmW`   | power of the MW source in mW                                                         |
  | `BmmT`  | modulation amplitude (magnetic flux density modulation, \\B\_{\text{m}}\\) in mT     |
  | `TK`    | temperature in K                                                                     |
  | `mwGHz` | applied microwave frequency in `GHz` to record the continuous wave (CW) EPR spectrum |

  **Default**: `instrum.params = NULL` because they are primarily
  extracted from the `path_to_dsc_par` based on the `origin`.

- path_to_dsc_par:

  Character string, path (can be also acquired by the
  [`file.path`](https://rdrr.io/r/base/file.path.html)) to `.DSC/.dsc`
  or `.par` (depending on the OS, see the `origin` argument) `text`
  files including all instrumental parameters from the EPR machine. If
  the `instrum.params` is already defined, the `path_to_dsc_par = NULL`.
  Otherwise, BOTH the `path_to_dsc_par` AS WELL AS the `origin` MUST BE
  SPECIFIED !

- origin:

  Character string, corresponding to software which was used to obtain
  the EPR spectra on spectrometers, because the files are slightly
  different, whether they were recorded by the "WinEpr"
  (`origin = "winepr"`) or by the "Xenon" (**default**). If
  `origin = NULL` as well as `path_to_dsc_par = NULL`, the
  `instrum.params` have to be set.

- qValue:

  Numeric value of the sensitivity `Q` factor. For the processed EPR
  spectra by the `{eprscope}` package the `integ.sigmoid.max` is usually
  normalized by the `Q` value. Therefore, **default**: `qValue = NULL`
  (corresponding to `1`).

- tube.sample.id.mm:

  Numeric value, which equals to internal diameter (in `mm`) of the
  tube/cell used for the quantitative EPR experiment.

- point.sample.factor:

  Numeric value, corresponding to point sample correction factor,
  provided by the cavity/probehead manufacturer. Value for the standard
  Bruker rectangular cavity is set as **default**.

- fill.sample.h.mm:

  Numeric value, referring to sample height (in `mm`) within the
  tube/cell.

- eff.cavity.h.mm:

  Numeric value, which equals to effective cavity/probehead
  height/length, usually provided by the probehead manufacturer.

- fn.B1.Bm.fit.y:

  Numeric vector (coefficients) of the polynomial degree from 5 to 12.
  Coefficients for the standard Bruker rectangular cavity are set as
  **default**.

- fn.B1.Bm.fit.y.max:

  Numeric value, corresponding to maximum value of the polynomial fit.
  Value for the standard Bruker rectangular cavity is set as
  **default**.

- Norm.const:

  Numeric value, corresponding to normalization constant (see
  [`quantify_EPR_Norm_const`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Norm_const.md)).
  **Default**: `Norm.const = NULL` in case if the EPR spectrum was
  normalized by such constant either during the measurement or
  processing. Otherwise it must be provided by the
  [`quantify_EPR_Norm_const`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Norm_const.md).

- Temp.K:

  Numeric value, temperature value in `K`. Because the `instrum.params`
  also contains temperature input one may choose which definition
  (`Temp.K` or `TK`) is taken for the calculation. Either `Temp.K` or
  `TK` CAN BE ALSO `NULL` but NOT BOTH !! In the latter case, **default
  value** `298 K` is considered.

- S:

  Numeric value, total spin sample quantum number. For radicals
  `S = 0.5` (**default**).

## Value

List of the following quantities:

- N.cm:

  Number of spins per effective centimeter. It is defined as the cm
  around the maximum, \\\pm 5\\\text{mm}\\, of the intensity
  distribution curve/polynomial fit within the cavity
  \\f(B_1,B\_{\text{m}})\\ from the equation shown in `Details`.

- N.cm3:

  Number of spins per \\\text{cm}^3\\.

- c.M:

  Concentration of spins/radicals in \\\text{mol}\\\text{dm}^{-3}\\.

## Details

There are two approaches how to quantify the number of paramagnetic
species/radicals/spins. The **relative** one needs a standard sample
with a known spin number and can be evaluated by the sigmoid integral
ratio of the sample under study and that of the standard. While the
**absolute** method do not need the reference sample, it requires a
precise cavity signal calibration as well as standardized cell geometry.
Both are provided by the EPR instrument and lab-glass manufacturers (see
e.g. Hirschmann-Laborgeräte (2023), `References`). In case of absolute
quantitative EPR analysis the sigmoid integral (its maximum value),
\\I\_{\text{sigmoid}}\\,can be used to calculate the number of
"spins"/radicals/paramagnetic species, \\N\_{\text{Spins}}\\ (see also
`References`) =\> \$\$N\_{\text{Spins}} =
I\_{\text{sigmoid}}\\/\\\[(c/f(B_1,B\_{\text{m}}))\\(G\_{\text{R}}\\t\_{\text{C}}
\\N\_{\text{Scans}})\\(\sqrt{P\_{\text{MW}}}\\B\_{\text{m}}\\Q\\n\_{\text{B}}\\S(S+1))\]\$\$
where the quantity notations possess the following meaning (parentheses
denote whether it is an instrumental or sample dependent parameter):

|                        |                                                                                                                              |
|------------------------|------------------------------------------------------------------------------------------------------------------------------|
| **Quantity Symbol**    | **Meaning/Short Desription**                                                                                                 |
| \\c\\                  | Point sample calibration factor (instrumental).                                                                              |
| \\f(B_1,B\_\text{m})\\ | Spatial distribution of the microwave \\B_1\\ and modulation amplitude within the cavity/probehead/resonator (instrumental). |
| \\G\_{\text{R}}\\      | Receiver gain (commonly in \\\text{dB}\\ units (instrumental)).                                                              |
| \\t\_{\text{C}}\\      | Conversion time (commonly in \\\text{ms}\\) which is an analogy to integration time in other spectroscopies (instrumental).  |
| \\N\_{\text{Scans}}\\  | Number of scans/accumulations during the experiment (instrumental).                                                          |
| \\P\_{\text{MW}}\\     | Microwave power (instrumental).                                                                                              |
| \\B\_{\text{m}}\\      | Modulation amplitude (instrumental).                                                                                         |
| \\Q\\                  | *Q*-Value or *Q*-Factor characterizing the resonator/cavity/probehead sensitivity (unitless and instrumental).               |
| \\n\_{\text{B}}\\      | Boltzmann factor for temperature dependence (instrumental-sample).                                                           |
| \\S\\                  | Total electronic spin quantum number (sample). Commonly, for radicals \\S = 1/2\\.                                           |

Almost all summarized quantities are instrument-dependent. Most of them
correspond to the essential parameters for the experiment and can be
easily acquired from the `.DSC`/`.dsc`/`.par` file(s). The Boltzmann
factor describes the population of spin states by \\\exp{(\Delta
\varepsilon)\\/\\(k\_{\text{B}}\\T)}\\, where \\\Delta \varepsilon\\
denotes the energy difference between the basic spin states,
\\k\_{\text{B}}\\ is the Boltzmann constant (available from
[`syms`](https://rdrr.io/pkg/constants/man/syms.html)) and the \\T\\
represents the temperature in \\\text{K}\\. For temperatures \\\geq
4\\\text{K}\\ and continuous wave experiments where the \\\Delta
\varepsilon = h\\\nu\_{\text{MW}}^{}\\ is constant, this factor may be
very well estimated by the following formula: \$\$n\_{\text{B}} =
h\\\nu\_{\text{MW}}^{}\\/\\(2\\k\_{\text{B}}\\T)\$\$ The term
\\(G\_{\text{R}}\\t\_{\text{C}}\\N\_{\text{Scans}})\\ actually
corresponds to normalization constant which is available from
[`quantify_EPR_Norm_const`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Norm_const.md).
Besides the above-described parameters which can be easily estimated,
there are however characteristics that requires precise calibration and
usually are provided by the spectrometer manufacturers. The spatial
distribution of the microwave, \\B_1\\, and modulation amplitude,
\\B\_\text{m}\\, influences the intensity of the sample predominantly
along the \\y\\-axis direction (i.e. when moving the sample tube up or
down within the cavity). Such intensity distribution, depending on the
cavity/probehead for different sample length and positions, can be
approximated by a polynomial (see the `fn.B1.Bm.fit.y` argument) that is
supplied by the manufacturer as well (the coefficients of the polynomial
can be sometimes found in `.DSC`/`.dsc`/`.par` file(s)). For
quantitative purposes, such polynomial is integrated over the length of
the sample.

## References

Eaton GR, Eaton SS, Barr DP, Weber RT (2010). *Quantitative EPR*.
Springer Science and Business Media. ISBN 978-3-211-92947-6,
<https://link.springer.com/book/10.1007/978-3-211-92948-3>.

Weber RT (2011). *Xenon User's Guide*. Bruker BioSpin Manual Version
1.3, Software Version 1.1b50.

Hirschmann-Laborgeräte (2023). “Ringcaps.”
<https://hirschmannlab.de/en/produkt/ringcaps/>.

Mazúr M, Valko M, Morris H (2000). “Analysis of the Radial and
Longitudinal Effect in a Double TE104 and a Single TE102 Rectangular
Cavity.” *J. Magn. Reson.*, **142**(1), 37–56. ISSN 1090-7807,
<https://doi.org/10.1006/jmre.1999.1915>.

Portis AM (1953). “Electronic Structure ofF Centers: Saturation of the
Electron Spin Resonance.” *Phys. Rev.*, **91**(5), 1071–1078,
<https://doi.org/10.1103/PhysRev.91.1071>.

Mailer C, Sarna T, Swartz HM, Hyde JS (1977). “Quantitative Studies of
Free Radicals in Biology: Corrections to ESR Saturation Data.” *J. Magn.
Reson.* (1969), **25**(1), 205–210, ISSN 0022-2364,
<https://doi.org/10.1016/0022-2364(77)90133-0>.

## See also

Other Evaluations and Quantification:
[`eval_integ_EPR_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_integ_EPR_Spec.md),
[`eval_kinR_EPR_modelFit()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md),
[`eval_kinR_ODE_model()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_ODE_model.md),
[`quantify_EPR_Norm_const()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Norm_const.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## quantitative analysis (determining the
## radical concentration `c.M`) of a sample measured
## at different temperatures
## all data summarized in `data.tidy.integ`
data.quant <- mapply(function(x,y)
  {quantify_EPR_Abs(x,
    instrum.params = c(PmW = 2.518,
                       BmmT = 5.4e-02,
                       TK = NULL, ## 298 K
                       mwGHz = 9.530265),
    path_to_dsc_par = NULL,
    origin = NULL,
    tube.sample.id.mm = 2.86,
    fill.sample.h.mm = 23,
    Temp.K = y)$c.M
    },
  data.tidy.integ$Area,
  data.tidy.integ$Temperature_K
  )
} # }

```
