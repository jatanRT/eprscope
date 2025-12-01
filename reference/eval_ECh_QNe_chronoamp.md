# Transferred Charge and Number of Electrons from Chronoamperogram

Evaluation of the transferred charge and the corresponding number of
electrons from [chronoamperogram](https://doi.org/10.1515/pac-2018-0109)
related to electrochemical experiment, performed simultaneously with the
EPR time series measurement or independently of the latter. To acquire
charge, the input \\I\\ *vs* \\time\\ relation (coming from the
`data.at`) is integrated by the
[`pracma::cumtrapz`](https://rdrr.io/pkg/pracma/man/trapz.html)
function. **Prior to integration a capacitive current correction must be
done**, especially if it is relatively high in comparison to
[faradaic](https://doi.org/10.1515/pac-2018-0109) one. Afterwards, the
number of electrons is calculated by Faraday's law (see details). The
output plot can be visualized either as \\Q(N\_{\text{e}})\\ *vs* \\t\\
(time) or as \\Q(N\_{\text{e}})\\ *vs* \\E\\ (potential, if available in
the input `data.at`).

## Usage

``` r
eval_ECh_QNe_chronoamp(
  data.at,
  time = "time_s",
  time.unit = "s",
  tlim = NULL,
  Current = "I_A",
  Current.unit = "A",
  E = NULL,
  E.unit = NULL,
  ref.electrode = NULL,
  Ne.output = TRUE,
  separate.plots = FALSE
)
```

## Arguments

- data.at:

  Data frame (table) object, including required columns like `Time`
  (\\t\\), `Current` (\\I\\). Even though an arbitrary column label can
  be used, the best option is to use labels such as `time_s`, `I_uA` or
  `I_mA`. Optionally, column related to potential (\\E\\) may be present
  as well for the transferred charge (\\Q\\) or number of electrons
  (\\N\_{\text{e}}\\) *vs* \\E\\ visualization (see also the arguments
  `E`, `E.unit` and `ref.electrode`).

- time:

  Character string, pointing to `time`-axis/column header in the
  original `data.at`. **Default**: `time = "time_s"` (time in seconds).

- time.unit:

  Character string, pointing to `time`-quantity unit. There are
  following units available: `time.unit = "s"` (**default**),
  `time.unit = "ms"`, `time.unit = "us"` (microseconds),
  `time.unit = "ns"` or `time.unit = "min"`.

- tlim:

  Numeric vector of the `time`-quantity lower and upper limit, e.g.
  `xlim = c(5,400)` (time in seconds. **Default**: `tlim = NULL`
  actually setting the entire `time` interval from the original dataset.

- Current:

  Character string, indicating the `Current`(\\I\\)-axis/column quantity
  name in the original `data.at` object. **Default**: `Current = "I_A"`
  (current in \\\text{A}\\).

- Current.unit:

  Character string, pointing to `Current` quantity unit like
  `Current.unit = "uA"` (microamps) `Current.unit = "A"` (**default**),
  `Current.unit = "mA"` and `Current.unit = "nA"`.

- E:

  Character string, referring to \\E\\(potential) column name within the
  input `data.at` dataset. **Default**: `E = NULL`, corresponding to
  situation, when one doesn't want to visualize transferred charge (or
  number of electrons) *vs* \\E\\.

- E.unit:

  Character string, setting the potential unit (see `E` argument),
  usually `E.unit = "mV"` or `E.unit = "V"`. **Default**:
  `E.unit = NULL`, corresponding to situation, when one doesn't want to
  visualize transferred charge (or number of electrons) *vs* \\E\\.

- ref.electrode:

  Character string, corresponding to reference electrode
  notiation/label, e.g. `ref.electrode = "Ag-quasiref"` or
  `ref.electrode = "Fc/Fc+"`. **Default**: `ref.electrode = NULL`
  (displayed potential is not related to any `ref.electrode`).

- Ne.output:

  Logical. Should be the number of transferred electrons (`Ne`)
  presented within the plot ? **Default**: `Ne.output = TRUE`.

- separate.plots:

  Logical. By **default**, both relations: \\Q(N\_{\text{e}})\\ *vs*
  \\t,E\\ (time or potential) are shown in one plot
  (`separate.plots = FALSE`). One can separate \\N\_{\text{e}}\\ *vs*
  \\t,E\\ and \\Q\\ *vs* \\t,E\\ into individual plots setting up the
  `separate.plots = TRUE`.

## Value

List containing the following elements, depending on `separate.plots`:

1.  If `separate.plots = FALSE`

    - df:

      Original `data.at` data frame object with the following additional
      columns/variables: `Q_C` (charge in coulombs), `Q_mC` (charge in
      millicoulombs, if the maximum charge \\\leq 0.099\\\text{C}\\) and
      `Ne` (number of transferred electrons, if `Ne.output = TRUE`).

    - plot:

      Side-by-side plot object (list) of \\N\_{\text{e}}\\ *vs* \\t,E\\
      as well as \\Q\\ *vs* \\t,E\\.

2.  If `separate.plots = TRUE`

    - df:

      Original `data.at` data frame object with the following additional
      columns/variables: `Q_C` (charge in coulombs), `Q_mC` (charge in
      millicoulombs, if the maximum charge \\\leq 0.099\\\text{C}\\) and
      `Ne` (number of transferred electrons, if `Ne.output = TRUE`).

    - plot.Ne:

      Plot object (list) of \\N\_{\text{e}}\\ *vs* \\t,E\\.

    - plot.Q:

      Plot object (list) of \\Q\\ *vs* \\t,E\\.

## Details

When quantitative EPR is carried out along with electrochemistry
simultaneously, one can easily compare the number of radicals with the
number of transferred electrons. Number of radicals (\\N\_{\text{R}}\\)
are evaluated from quantitative measurements (see also
[`quantify_EPR_Abs`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Abs.md)),
whereas number of transferred electrons (\\N\_{\text{e}}\\) is related
to charge (\\Q\\), according to: \$\$N\_{\text{e}} =
(Q\\N\_{\text{A}})/F\$\$ where \\N\_{\text{A}}\\ stands for the
Avogadro's number and \\F\\ for the Faraday's constants. Both are
obtained by the `constans::syms$na` and the `constants::syms$f`,
respectively, using the
[constants](https://r-quantities.github.io/constants/) package (see
[`syms`](https://rdrr.io/pkg/constants/man/syms.html)). If both numbers
are close (\\N\_{\text{R}} \approx N\_{\text{e}}\\), it reveals the
presence of one-electron oxidation/reduction, while totally different
numbers may point to a more complex mechanism (such as
comproportionation, follow-up reactions, multiple electron transfer).

## References

Bard AJ, Faulkner LR, White HS (2022). *Electrochemical methods:
Fundamentals and Applications*, 3rd edition, John Wiley and Sons, Inc.,
ISBN 978-1-119-33405-7, <https://www.wiley.com/en-us/9781119334064>.

Pingarrón JM, Labuda J, Barek J, Brett CMA, Camões MF, Fojta M, Hibbert
DB (2020). “Terminology of Electrochemical Methods of Analysis (IUPAC
Recommendations 2019).” *Pure Appl. Chem.*, **92**(4), 641–694,
<https://doi.org/10.1515/pac-2018-0109>.

Neudeck A, Petr A, Dunsch L (1999). “The redox mechanism of Polyaniline
Studied by Simultaneous ESR–UV–vis Spectroelectrochemistry.” *Synth.
Met.*, **107**(3), 143–158,
<https://doi.org/10.1016/S0379-6779(99)00135-6>.

Hans W. Borchers (2023). *pracma: Practical Numerical Math Functions*. R
package version 2.4.4,
<https://cran.r-project.org/web/packages/pracma/index.html>.

## See also

Other EPR Spectroelectrochemistry:
[`plot_ECh_VoC_amperogram()`](https://jatanrt.github.io/eprscope/reference/plot_ECh_VoC_amperogram.md)

## Examples

``` r
## loading package built-in example file =>
## `.txt` file generated by the IVIUM potentiostat software
triarylamine.path.cv <-
load_data_example(file = "Triarylamine_ECh_CV_ivium.txt")
## the data frame contains following variables:
## time, desired potential, current and the actual/applied
## potential
triarylamine.data.cv <-
  data.table::fread(file = triarylamine.path.cv,
    skip = 2,
    col.names = c("time_s",
                  "E_V_des", # desired potential
                  "I_A",
                  "E_V_app") # applied potential
  )
#
## simple chronoamperogram plot
plot_ECh_VoC_amperogram(data.vat = triarylamine.data.cv,
  x = "time_s",
  x.unit = "s",
  Current = "I_A",
  Current.unit = "A",
  ticks = "in"
 )

#
## transferred charge and the number of electrons
## with default parameters
triarylamine.data.QNe <-
  eval_ECh_QNe_chronoamp(data.at = triarylamine.data.cv)
#
## data frame preview
triarylamine.data.QNe$df
#>      time_s E_V_des          I_A      E_V_app           Q_C          Q_mC
#>       <num>   <num>        <num>        <num>         <num>         <num>
#>   1:    0.0  0.0000 -3.95429e-08 -0.000251648 1.0036104e-07 1.0036104e-04
#>   2:    0.5  0.0025 -3.35184e-08  0.002265060 8.2095711e-08 8.2095711e-05
#>   3:    1.0  0.0050 -2.92210e-08  0.004787570 6.6410861e-08 6.6410861e-05
#>   4:    1.5  0.0075 -2.54160e-08  0.007305210 5.2751611e-08 5.2751611e-05
#>   5:    2.0  0.0100 -2.19019e-08  0.009820580 4.0922136e-08 4.0922136e-05
#>  ---                                                                     
#> 794:  396.5  0.0125 -4.48087e-07  0.012252300 2.0162832e-04 2.0162832e-01
#> 795:  397.0  0.0100 -4.49558e-07  0.009752980 2.0140391e-04 2.0140391e-01
#> 796:  397.5  0.0075 -4.51201e-07  0.007249400 2.0117872e-04 2.0117872e-01
#> 797:  398.0  0.0050 -4.52992e-07  0.004751440 2.0095267e-04 2.0095267e-01
#> 798:  398.5  0.0025 -4.54648e-07  0.002258640 2.0072576e-04 2.0072576e-01
#>                 Ne
#>              <num>
#>   1: 6.2640432e+11
#>   2: 5.1240113e+11
#>   3: 4.1450399e+11
#>   4: 3.2924966e+11
#>   5: 2.5541589e+11
#>  ---              
#> 794: 1.2584650e+15
#> 795: 1.2570643e+15
#> 796: 1.2556588e+15
#> 797: 1.2542479e+15
#> 798: 1.2528316e+15
#
## graphical representation
triarylamine.data.QNe$plot



```
