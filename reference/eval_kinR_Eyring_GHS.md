# Reaction Activation Parameters Obtained by the Essential Transition State Theory

Finding the temperature-dependence of a rate constant (\\k\\) related to
the elementary radical reaction, using the essential transition state
theory (TST). The activation parameters, such as \\\Delta^{\ddagger}
S^o\\ and \\\Delta^{\ddagger} H^o\\ are obtained either by the
non-linear fit (see the general
[`nls`](https://rdrr.io/r/stats/nls.html) R function) or by the linear
fit (using the [`lm`](https://rdrr.io/r/stats/lm.html)) of the Eyring
expression (or its logarithmic linear form) on the original \\k\\ *vs*
\\T\\ relation (please, refer to the `data.kvT` argument). The latter
can be acquired by the
[`eval_kinR_EPR_modelFit`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md)
from sigmoid-integrals of the EPR spectra recorded at different
temperatures. Finally, the activation Gibbs energy (\\\Delta^{\ddagger}
G^o\\) is calculated, using the optimized \\\Delta^{\ddagger} S^o\\ and
\\\Delta^{\ddagger} H^o\\, for each temperature in the series.

## Usage

``` r
eval_kinR_Eyring_GHS(
  data.kvT,
  rate.const,
  rate.const.unit = "s^{-1}",
  Temp,
  Temp.unit = "K",
  transmiss.coeff = 1,
  fit.method = "default"
)
```

## Arguments

- data.kvT:

  Data frame object, which must include two essential columns: rate
  constant (\\k\\ of an elementary radical reaction) and the
  corresponding temperatures at which the \\k\\ was acquired.

- rate.const:

  Character string, pointing to rate constant column header in the
  actual `data.kvT` data frame.

- rate.const.unit:

  Character string, referring to rate constant unit. This has to be
  specified using the
  [`plotmath`](https://rdrr.io/r/grDevices/plotmath.html) notation, like
  `rate.const.unit = "M^{-1}~s^{-1}"` or `rate.const.unit = "s^{-1}"`
  (**default**), because it is automatically applied as \\y\\-axis unit
  in the graphical output by the `{ggplot2}`.

- Temp:

  Character string, pointing to temperature column header within the
  original `data.kvT` data frame.

- Temp.unit:

  Character string, corresponding to temperature unit related to `Temp`.
  Temperature can be defined in the following units: `Temp.unit = "K"`
  (kelvin, **default**), `Temp.unit = "oC"` (degree Celsius) or
  `Temp.unit = "oF"` (degree Fahrenheit). If other than **default**
  specified, temperature values (column characterized by the `Temp`
  argument) are automatically converted into `"K"` (kelvins).

- transmiss.coeff:

  Numeric value, corresponding to probability that the activated complex
  is transformed into products. **Default**: `transmiss.coeff = 1`
  (\\100\\\\\\).

- fit.method:

  Character string, corresponding to method applied to fit the
  theoretical Eyring relation (by optimizing the activation parameters,
  see `Details`) to the experimental \\k\\\\vs\\\\T\\ dependence. For
  this purpose, either `fit.method = "linear" ("lm")` (using the
  [`lm`](https://rdrr.io/r/stats/lm.html)) or non-linear methods
  (available by the [`nls`](https://rdrr.io/r/stats/nls.html) function)
  are applied. The latter includes `"default"` (corresponding to
  [Gauss-Newton
  algorithm](https://journal.r-project.org/articles/RJ-2023-091/)),
  `"plinear"`, which is
  [Golub-Pereyra](https://geo-ant.github.io/blog/2020/variable-projection-part-1-fundamentals/)
  algorithm or `"port"` ([Fortran PORT, "portable" library for numerical
  computation](https://ms.mcmaster.ca/%7Ebolker/misc/port.pdf)).

## Value

As a result of the Eyring-relation fit, list with the following
components is available:

- df:

  Data frame, including the original `data.kvT` + the column of
  \\\Delta^{\ddagger} G^o\\, with the name of
  `DeltaG_active_kJ_per_mol`, as well as `fitted`/predicted values of
  the rate constant and finally, the corresponding residuals. If
  `fit.method = "linear"` additional columns like `reciprocal_T` and
  `lnk_per_T` are available, corresponding to \\1\\/\\T\\ and
  \\ln(k\\/\\T)\\, respectively.

- df.fit:

  Data frame including temperature (in the same region like in the
  original `data.kvT`, however with the resolution of 1024 points) and
  the corresponding `.fitted` \\k\\, according to Eyring model.

- plot:

  Static ggplot2-based object/list, showing graphical representation of
  the (non-)linear fit, together with the Eyring equation.

- ra:

  Simple residual analysis - a list consisting of 4 elements: diagnostic
  plots `plot.rqq()` function, `plot.histDens`; original data frame
  (`df`) with residuals and their corresponding standard deviation
  (`sd`). For details, please refer to the
  [`plot_eval_RA_forFit`](https://jatanrt.github.io/eprscope/reference/plot_eval_RA_forFit.md).

- df.coeffs.HS:

  Data frame object, containing the optimized (best fit) parameter
  values (`Estimates`), their corresponding `standard errors`, `t-` as
  well as `p-values` for the corresponding Eyring model.

- df.model.HS:

  Data frame object, containing model characteristics (including
  information criteria like AIC and BIC, see also
  [`eval_kinR_EPR_modelFit`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md)
  and
  [`eval_ABIC_forFit`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md)).

- cov.df:

  Covariance `matrix` of a data frame, consisting of rate constant
  (\\k\\ or \\ln\\(k)\\/\\T\\, depending on the `fit.method` argument),
  `fitted` (Eyring fit) and the corresponding residuals as
  columns/variables. Covariance between the experiment (\\k\\ *vs* \\T\\
  or \\ln\\(k)\\/\\T\\ *vs* \\1\\/\\T\\) and "Eyring" should be positive
  and strong for a decent fit. Contrary, the `cov` between the Eyring
  fit and residuals should be ideally close to `0`, indicating no
  systematic relationship. However, the covariance is scale-depended and
  must be "normalized". Therefore, for such a purpose, the correlation
  is defined as shown below.

- cor.df:

  Correlation `matrix` of a data frame, consisting of rate constant
  (\\k\\ or \\ln\\(k)\\/\\T\\, depending on the `fit.method` argument),
  `fitted` (Eyring fit) and the corresponding residuals as
  columns/variables. Such matrix can be additionally nicely visualized
  by a correlation `plot` created by the
  [`corrplot`](https://rdrr.io/pkg/corrplot/man/corrplot.html) function.
  A higher positive correlation (between the \\k\\ *vs* \\T\\ or
  \\ln\\(k)\\/\\T\\ *vs* \\1\\/\\T\\ and the Eyring fit), with the value
  close to `1`, indicates that the Eyring fit nicely follows the
  \\k\\-temperature dependence. Contrary, no clear correlation between
  the residuals and the experiment and/or "Eyring" must be visible.
  Therefore, such correlation should be ideally close to `0`.

- vec.HS.uncert:

  Numeric vector, consisting of \\\Delta^{\ddagger} S^o\\ as well as
  \\\Delta^{\ddagger} H^o\\, together with their uncertainties, all in
  SI units like J / (mol(\* K)). Calculation of uncertainties for linear
  model is performed by the error propagation, implemented in
  [`{errors}` R
  package](https://r-quantities.github.io/errors/articles/rjournal.html),
  using the first order Taylor series method.

- converg:

  If the `fit.method` IS DIFFERENT FROM `"linear"` a list, containing
  fitting/optimization characteristics like number of
  evaluations/iterations (`N.evals`) and character denoting the
  (un)successful convergence (`message`).

## Details

The basic assumption of the Transition State Theory (TST) is the
existence of activated state/complex, formed by the collision of
reactant molecules, which does not actually lead to reaction products
directly. The activated state (AS) is formed as highly energized, and
therefore as an unstable intermediate, decomposing into products of the
reaction. Accordingly, the reaction rate is given by the rate of its
decomposition. Additional important assumption for TST is the presence
of pre-equilibrium (characterized by the \\K^{\ddagger}\\ constant) of
the reactants with the activated complex (AC). Because the latter is not
stable, it dissociates with motion along the corresponding
bond-stretching coordinate. For this reason, the rate constant (\\k\\)
must be related to the associated vibration frequency (\\\nu\\). Thus,
every time, if the AC is formed, the \\k\\ of AC-dissociation actually
equals to \\\nu\\. Nevertheless, it is possible that the AC will revert
back to reactants and therefore, only a fraction of ACs will lead to
product(s). Such situation is reflected by the transmission coefficient
\\\kappa\\ (see also the argument `transmiss.coeff`), where \\k =
\kappa\\\nu\\.

According to statistical thermodynamics, the equilibrium constant can be
expressed by the partition function (\\q\\) of the reactants and that of
the AC. By definition, each \\q\\ corresponds to ratio of total number
of particles to the number of particles in the ground state. In essence,
it is the measure of degree to which the particles are spread out
(partitioned among) over the energy levels. Therefore, taking into
account the energies of a harmonic quantum oscillator vibrating along
the reaction coordinate as well as partition functions of the AC and
those of the reactants, the rate constant can be expressed as follows
(see e.g. Ptáček P, Šoukal F, Opravil T (2018) in the `References`):
\$\$k = \kappa\\(k\_{\text{B}}\\T\\/\\h)\\K^{\ddagger}\$\$ where the
\\k\_{\text{B}}\\ and \\h\\ are the Boltzmann and Planck constants,
respectively, \\T\\ corresponds to temperature and finally, the
\\K^{\ddagger}\\ represents the equilibrium constant including the
partition functions of reactants and that of the AC. In order to
evaluate the AC partition function, its structure must be known.
However, often, due to the lack of structural information, it is
difficult (if not impossible) to evaluate the corresponding
\\q^{\ddagger}(\text{AC})\\. Therefore, considering the equilibrium
between the reactants and the AC, one may express the \\K^{\ddagger}\\
in terms of *Gibbs* activation energy (\\\Delta^{\ddagger} G^o\\),
because \\\Delta^{\ddagger} G^o = - R\\T\\ln K^{\ddagger}\\ and thus the
Eyring equation reads: \$\$k = \kappa\\(k\_{\text{B}}\\T\\/\\h)\\exp\[-
(\Delta^{\ddagger} G^o)/(R\\T)\] =
\kappa\\(k\_{\text{B}}\\T\\/\\h)\\exp\[- (\Delta^{\ddagger}
H^o)/(R\\T)\]\\exp\[\Delta^{\ddagger} S^o / R\]\$\$ where \\R\approx
8.31446\\\text{J}\\\text{mol}^{-1}\\\text{K}^{-1}\\ is the universal gas
constant and the upper index \\^o\\ denotes the standard molar state
(see IUPAC (2019) in the `References`). Previous formula is applied as a
model to fit the experimental \\k\\\\vs\\\\T\\ (see the argument
`data.kvT`) relation, where both the \\\Delta^{\ddagger} S^o\\ and the
\\\Delta^{\ddagger} H^o\\ (in the graphical output, are also denoted as
\\\Delta^{active} S^o\\ and \\\Delta^{active} H^o\\, respectively) are
optimized using the `fit.method` (by the
[`nls`](https://rdrr.io/r/stats/nls.html) function). In the first
approach, both latter are considered as temperature independent within
the selected temperature range. Often, the Eyring equation is not
applied in the original form, however in the linear one,like
\$\$ln(k\\/\\T) = - (\Delta^{\ddagger} H^o)\\/\\(R\\T) +
(\Delta^{\ddagger} S^o\\/\\R) + ln(\kappa\\k\_{\text{B}}\\/\\h)\$\$ with
the slope/coefficient \\- (\Delta^{\ddagger} H^o)\\/\\R\\ and the
\\(\Delta^{\ddagger} S^o\\/\\R) + ln(\kappa\\k\_{\text{B}}\\/\\h)\\ as
intercept of the \\ln(k\\/\\T)\\\\vs\\\\1/T\\ relation. Nevertheless,
the latter is not recommended as a model for fitting the experimental
\\k(T)\\ (see also Lente G, Fábián I, Poë AJ (2005) in the
`References`). The reason inherits in the misinterpretation of the
extrapolation to \\T\rightarrow \infty\\ (or \\1/T\rightarrow 0\\) by
which the \\\Delta^{\ddagger} S^o\\ is obtained and thus it is
unreliable. Accordingly, **the original exponential Eyring form is
recommended** as a model to fit the experimental \\k(T)\\. Contrary, it
may happen that \\k\\\\vs\\\\T\\ can vary in several (\\\approx 2-3\\)
orders of magnitude within the studied temperature range and therefore,
it is necessary to proportionally weight the \\k\\ (see also Lente G,
Fábián I, Poë AJ (2005) in the `References`). For the linear form,
weighting does not make a difference because the transformed
\\ln(k\\/\\T)\\ span over a narrow range of values. Therefore, it is
advisable to perform the original exponential fit as well as the linear
one within the studied temperature range and compare both outcomes (both
methods are involved in this function, see the argument `fit.method`).
The [\\k\\-unit depends on the molecularity of the
reaction](https://goldbook.iupac.org/terms/view/E02142), please also
refer to the `rate.const.unit` argument. Therefore, the left hand site
of the Eyring equation above must be multiplied by the standard molar
concentration \\c^o = 1\\\text{mol}\\\text{dm}^{-3}\\: \$\$k\\(c^o)^{-
\sum_i \nu_i^{\ddagger}}\$\$ where the \\\sum_i \nu_i^{\ddagger}\\ goes
through stoichiometric coefficients (including the negative sign for
reactants) of the AC formation reaction (therefore the index
\\^{\ddagger}\\ is used), i.e. for the bi-molecular reaction, the sum
results in `-1`, however for the mono-molecular one, the sum results in
`0`.

While the transition state theory (TST) is a helpful tool to get
information about the mechanism of an elementary reaction, it has some
limitations, particularly for radical reactions. Couple of them are
listed below.

1.  One should be very careful if applied to elementary steps in a
    multistep reaction kinetics (like consecutive reactions, example
    shown in
    [`eval_kinR_ODE_model`](https://jatanrt.github.io/eprscope/reference/eval_kinR_ODE_model.md)).
    If the intermediate (e.g. in the consecutive reaction mechanism)
    possesses a short life-time, the TST probably fails.

2.  For very fast reactions the assumed equilibrium between the
    reactants and the AC won't be reached. Therefore, the spin trapping
    reactions, which \\k\\s may actually fall into the order of
    \\10^9\\\text{dm}^3\\\text{mol}^{-1}\\\text{s}^{-1}\\ (or oven
    higher, see Kemp TJ (1999) in the `References`) should be taken with
    extreme caution in terms of TST.

3.  Formation of AC in TST is based on classical mechanics, that is
    molecules/atoms will only collide, having enough energy (to form the
    AC), otherwise reaction does not occur. Whereas, taking into account
    the quantum mechanical principle, molecules/atoms with any finite
    energy may (with a certain probability) tunnel across the energy
    barrier. For reactions like radical-radical recombination, where the
    barriers are typically very low, the tunneling probability is low
    (when there's essentially "no barrier", there's nothing to tunnel
    through). In addition, such reactions can proceed relatively fast
    and therefore the TST (Eyring fit) can also strongly bias the
    activation parameters. However, for radical reactions, involving the
    H-atom transfer or abstraction the tunneling can be important.

## References

Engel T, Reid P (2013). *Physical Chemistry, 3rd Edition*, Pearson
Education, ISBN 978-0-321-81200-1,
<https://elibrary.pearson.de/book/99.150005/9781292035444>.

Ptáček P, Šoukal F, Opravil T (2018). "Introduction to the Transition
State Theory", *InTech.*, <https://doi.org/10.5772/intechopen.78705>.

International Union of Pure and Applied Chemistry (IUPAC) (2019).
“Transition State Theory”,
<https://goldbook.iupac.org/terms/view/T06470>.

Anslyn EV, Dougherty DA (2006). *Modern Physical Organic Chemistry*,
University Science Books, ISBN 978-1-891-38931-3,
<https://uscibooks.aip.org/books/modern-physical-organic-chemistry/>.

Lente G, Fábián I, Poë AJ (2005). "A common Misconception about the
Eyring Equation", *New J. Chem.*, **29**(6), 759–760,
<https://doi.org/10.1039/B501687H>.

Kemp TJ (1999), "Kinetic Aspects of Spin Trapping", *Progress in
Reaction Kinetics*, **24**(4), 287-358,
<https://doi.org/10.3184/007967499103165102>.

Wardlaw DM and Marcus RA (1986), "Unimolecular reaction rate theory for
transition states of any looseness. 3. Application to methyl radical
recombination", *J. Phys. Chem.*, **90**(21), 5383-5393,
<https://doi.org/10.1021/j100412a098>.

## See also

Other Evaluations:
[`eval_DeltaXpp_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_DeltaXpp_Spec.md),
[`eval_FWHMx_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_FWHMx_Spec.md),
[`eval_extremeX_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_extremeX_Spec.md),
[`eval_gFactor()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor.md),
[`eval_gFactor_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor_Spec.md),
[`eval_interval_cnfd_tVec()`](https://jatanrt.github.io/eprscope/reference/eval_interval_cnfd_tVec.md),
[`eval_nu_ENDOR()`](https://jatanrt.github.io/eprscope/reference/eval_nu_ENDOR.md),
[`eval_peakPick_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_peakPick_Spec.md)

## Examples

``` r
## demonstration on raw data, presented
## in https://www.rsc.org/suppdata/nj/b5/b501687h/b501687h.pdf
## considering reaction H+ + (S2O6)2- <==> SO2 + (HSO4)-
kinet.test.data <-
  data.frame(k_per_M_per_s =
               c(9.54e-7,1.91e-6,3.76e-6,
                 7.33e-6,1.38e-5,2.56e-5,
                 4.71e-5,8.43e-5,1.47e-4),
             T_oC = c(50,55,60,65,70,
                      75,80,85,90)
)
#
## original "Eyring" model
activ.kinet.test01.data <-
  eval_kinR_Eyring_GHS(
    data.kvT = kinet.test.data,
    rate.const = "k_per_M_per_s",
    rate.const.unit = "M^{-1}~s^{-1}",
    Temp = "T_oC",
    Temp.unit = "oC"
  )
#
## preview of the original data
## + ∆G (activated) + fitted + residuals
activ.kinet.test01.data$df
#>   k_per_M_per_s    T_K DeltaG_active_kJ_per_mol        fitted      residuals
#> 1      9.54e-07 323.15                116.46945 1.0051565e-06 -5.1156505e-08
#> 2      1.91e-06 328.15                116.43347 2.0020717e-06 -9.2071651e-08
#> 3      3.76e-06 333.15                116.39749 3.9069922e-06 -1.4699221e-07
#> 4      7.33e-06 338.15                116.36151 7.4767876e-06 -1.4678765e-07
#> 5      1.38e-05 343.15                116.32553 1.4043225e-05 -2.4322501e-07
#> 6      2.56e-05 348.15                116.28955 2.5908753e-05 -3.0875267e-07
#> 7      4.71e-05 353.15                116.25357 4.6987553e-05  1.1244654e-07
#> 8      8.43e-05 358.15                116.21759 8.3827467e-05  4.7253271e-07
#> 9      1.47e-04 363.15                116.18161 1.4721446e-04 -2.1446395e-07
#
## preview of the non-linear fit plot
activ.kinet.test01.data$plot

#
## preview of the optimized (activated)
## ∆S and ∆H parameters
activ.kinet.test01.data$df.coeffs.HS
#> # A tibble: 2 × 5
#>   term   estimate std.error statistic  p.value
#>   <chr>     <dbl>     <dbl>     <dbl>    <dbl>
#> 1 dS         7.20      1.08      6.66 2.89e- 4
#> 2 dH    118795.      390.      305.   1.09e-15
#
## compare values with those presented in
## https://www.rsc.org/suppdata/nj/b5/b501687h/b501687h.pdf
## ∆S = (7.2 +- 1.1) J/(mol*K) &
## ∆H = (118.80 +- 0.41) kJ/mol
#
## brief model summary
activ.kinet.test01.data$df.model.HS
#> # A tibble: 1 × 9
#>         sigma isConv      finTol logLik   AIC   BIC deviance df.residual  nobs
#>         <dbl> <lgl>        <dbl>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
#> 1 0.000000265 TRUE   0.000000397   125. -243. -243. 4.91e-13           7     9
#
## ...and the corresponding analysis of residuals
activ.kinet.test01.data$ra$plot.histDens

#
## graphical representation the correlation matrix
## corresponding to Eyring fit
activ.kinet.test01.data$cor.df %>%
  corrplot::corrplot(addCoef.col = "#c2c2c2")

#
## preview of the convergence
activ.kinet.test01.data$converg
#> $N.evals
#> [1] 46
#> 
#> $message
#> [1] "converged"
#> 
#
## this was an untransformed dataset,
## when the k values span more than 2 orders
## of magnitude => a proportional weighting
## or linear logarithmic model might be applied
#
## take the same above-defined dataset and assign
## it to a new variable
kinet.test.data.new <-
  data.frame(k_per_M_per_s =
               c(9.54e-7,1.91e-6,3.76e-6,
                 7.33e-6,1.38e-5,2.56e-5,
                 4.71e-5,8.43e-5,1.47e-4),
             T_oC = c(50,55,60,65,70,
                      75,80,85,90)
)
#
## linear logarithmic Eyring model
activ.kinet.test02.data <-
  eval_kinR_Eyring_GHS(
    data.kvT = kinet.test.data.new,
    rate.const = "k_per_M_per_s",
    rate.const.unit = "M^{-1}~s^{-1}",
    Temp = "T_oC",
    Temp.unit = "oC",
    fit.method = "linear"
  )
#
## preview of the original data
## + ∆G (activated) + fitted + residuals +
## + additional variables of the linear model
activ.kinet.test02.data$df
#>   k_per_M_per_s    T_K DeltaG_active_kJ_per_mol reciprocal_T  lnk_per_T
#> 1      9.54e-07 323.15                116.60728 0.0030945381 -19.640719
#> 2      1.91e-06 328.15                116.55062 0.0030473869 -18.961878
#> 3      3.76e-06 333.15                116.49395 0.0030016509 -18.299684
#> 4      7.33e-06 338.15                116.43729 0.0029572675 -17.647025
#> 5      1.38e-05 343.15                116.38063 0.0029141775 -17.029010
#> 6      2.56e-05 348.15                116.32397 0.0028723251 -16.425552
#> 7      4.71e-05 353.15                116.26730 0.0028316579 -15.830130
#> 8      8.43e-05 358.15                116.21064 0.0027921262 -15.262081
#> 9      1.47e-04 363.15                116.15398 0.0027536831 -14.719894
#>       fitted      residuals
#> 1 -19.639781 -9.3824585e-04
#> 2 -18.957734 -4.1445913e-03
#> 3 -18.296159 -3.5252103e-03
#> 4 -17.654149  7.1248091e-03
#> 5 -17.030849  1.8393107e-03
#> 6 -16.425452 -9.9954603e-05
#> 7 -15.837197  7.0667278e-03
#> 8 -15.265367  3.2869097e-03
#> 9 -14.709284 -1.0609755e-02
#
## preview of the linear fit plot
## also with confidence interval (99 %)
activ.kinet.test02.data$plot

#
## preview of the optimized (activated)
## ∆S and ∆H parameters together with
## their uncertainties
activ.kinet.test02.data$vec.HS.uncert
#> Errors: 152.98487705   0.44677457
#> [1] 120269.372077     11.332491
#
## compare values with those presented in
## https://www.rsc.org/suppdata/nj/b5/b501687h/b501687h.pdf
## ∆S = (11.33 +- 0.45) J/(mol*K) &
## ∆H = (120.27 +- 0.15) kJ/mol
#
## brief model summary
activ.kinet.test02.data$df.model.HS
#> # A tibble: 1 × 12
#>   r.squared adj.r.squared   sigma statistic  p.value    df logLik   AIC   BIC
#>       <dbl>         <dbl>   <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1     1.000         1.000 0.00607   618035. 1.42e-18     1   34.3 -62.6 -62.0
#> # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>
#
## corresponding analysis of residuals
## with residual standard deviation
activ.kinet.test02.data$ra$plot.rqq()

activ.kinet.test02.data$ra$sd
#> [1] 0.0065568073


```
