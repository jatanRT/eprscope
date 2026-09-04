# General Ranking of Models/Fits Using the AIC and BIC Metrics

In order to compare different (simulation) fits for the same
experimental data (see
[`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md),
[`eval_kinR_EPR_modelFit`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md),
[`eval_kinR_Eyring_GHS`](https://jatanrt.github.io/eprscope/reference/eval_kinR_Eyring_GHS.md),
[`smooth_EPR_Spec_by_npreg`](https://jatanrt.github.io/eprscope/reference/smooth_EPR_Spec_by_npreg.md)
or
[`eval_sim_EPR_isoFit_space`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit_space.md)),
we can score/rank them by different metrics (e.g. by the minimum sum of
residual squares or standard deviation of residuals), including Akaike
and Bayesian Information Criteria
([`AIC`](https://rdrr.io/r/stats/AIC.html) and BIC, respectively). These
are also applied for the best model selection in machine learning (refer
to e.g. [Predictive Modelling and Machine
Learning](https://www.modernstatisticswithr.com/mlchapter.html) or
[Error Estimation and Model
Selection](https://www.louisaslett.com/StatML/notes/error-estimation-and-model-selection.html#ref-yang05)),
because those criteria can help to detect [the optimal model without
under- or
overfitting](https://sesen.ai/blog/aic-bic-model-selection-information-criteria).
As described in details, both metrics depends on maximum logarithmic
likelihood (based on residuals calculation) to the same data. **The
smaller the (negative) AIC or BIC, the better the model/fit.**

## Usage

``` r
eval_ABIC_forFit(data.fit, residuals = NULL, k, residuals.distro = "auto")
```

## Arguments

- data.fit:

  Data frame object, usually containing variables/columns like
  `experiment`, `fit(ted)`/`predicted` as well as `residuals`/`errors`.
  If the latter is missing (see the argument `residuals` below) one can
  easily create/calculate the variable/column as a difference between
  the experimental and fitted/predicted values.

- residuals:

  Character string, pointing to variable/column header with
  residuals/errors, depending on the `data.fit` argument (usually
  `residuals = "Residuals"` or `residuals = "errors"`). **Default**:
  `residuals = NULL`.

- k:

  Numeric value identical to number of parameters used for the model/fit
  (see e.g. `Examples` in the
  [`eval_kinR_EPR_modelFit`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md)
  where `k = 2`).

- residuals.distro:

  Character string, corresponding to proposed probability distribution
  that describes the residuals/errors appearance. If set to **default**
  (`residuals.distro = "auto"`), it automatically decides which
  distribution (Normal/Gaussian, Student's t-distribution or Cauchy)
  fits the best to residuals/errors based on the implemented AIC and BIC
  calculations. Additionally, the fit can be supported by the
  Shapiro-Wilk (see
  [`shapiro.test`](https://rdrr.io/r/stats/shapiro.test.html)) and/or
  Kolmogorov-Smirnov (see
  [`ks.test`](https://rdrr.io/r/stats/ks.test.html)) tests. This is
  particularly suitable for the situation when the residual analysis
  detects heavier tails (see e.g. `Example` in
  [`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md))
  and one is not quite sure of the corresponding probability
  distribution (such situation is best described by the Cauchy or
  t-distrubution). Otherwise, the argument may also specify individual
  distributions like: `residuals.distro = "N(n)ormal"`, `"G(g)aussian"`,
  `"S(s)tudent"` or `"t-distribution"` (`"t-distro"`) as well as
  `"(C)cauchy"`.

## Value

Function returns a list with the following components:

- abic.vec:

  A numeric vector containing the values of estimated AIC and BIC,
  respectively.

- message:

  Sentence (message), describing the residuals/errors appearance by the
  probability distribution which has been proposed for the AIC and BIC
  calculation (see also the `residuals.distro` argument). Such
  information is additionally supported by the Shapiro-Wilk and/or by
  the Kolmogorov-Smirnov tests, whether the residuals distribution can
  be considered as a normal or not. These tests are based on the
  corresponding p-values. However, in order to clearly support the
  lowest AIC/BIC, depending on residuals distribution, a slightly
  "broader", than the commonly applied "sharp"/"rigid" \\p = 0.05\\
  threshold, is applied. If \\p \< 0.01\\, there is a stronger evidence
  against the null-hypothesis that the residuals are normally
  distributed. Therefore, we can only weakly support the normal
  distribution, however may prefer the other ones (Student's or Cauchy).
  Contrary, if \\p \> 0.05\\, we may support the null-hypothesis that
  the residuals are normally distributed. Consequently, there is less
  evidence for other distributions like Student's or Cauchy. If the
  \\p\\ is from the "gray" (0.01,0.05) zone/interval no support by the
  above-mentioned normality tests is provided.

## Details

Estimation of model errors, that model/fit makes in respect to our
(experimental) data, becomes one of the most consequential aspects of a
statistical (machine learning) analysis. Often, different
modelling/fitting approaches are used, with the attempt to identify or
select the best model/fit. Therefore, for such purpose, one tries to
minimize the errors/residuals more and more with each model. Or to put
it another way, **there is an information loss when the model/fit
approximates the reality** and a good model minimizes those losses. The
evaluation of AIC and BIC actually approaches the problem from the other
side, because it uses the technique called **maximum likelihood estimate
(MLE)**. The idea is to maximize the chance that each observation in the
sample follows a pre-selected distribution with specific set of
parameters (corresponding to a model/fit). For practical reasons a
logarithmic likelihood (or log-likelihood,\\LL\\) is used, and the
formulae for both criteria read: \$\$AIC = -2\\LL + 2\\k + (2\\k\\(k +
1)\\/\\(N - k -1))\$\$ and \$\$BIC = -2\\LL + k\\ln(N)\$\$ where \\k\\
and \\N\\ correspond to number of (model/fit) parameters and number of
observations, respectively. The 3rd term in the \\AIC\\ definition
represents the correction for small sample/observation ensemble, which
for high number of observations becomes very small (and can be
neglected, see e.g. Burnham and Anderson (2004) in the `References`).
For example, for EPR simulation fit with 2048 points and 8 parameters it
equals to \\16 \cdot 9\\/\\2039 \approx 0.0706\\. However, for the
radical kinetic measurements with 42 EPR spectra and 3 parameters, the
3rd term results in \\6 \cdot 4\\/\\38 \approx 0.6316\\.

**The original MLE/\\LL\\ calculation is based on the model.
Nevertheless, such computation can be quite often impractical or even
impossible to perform.** To overcome this difficulty, the formulae for
both criteria use a **standard assumption that the model and the data
residuals/errors are identically distributed.** Therefore, **the
residuals/errors are applied as a proxy for the MLE/\\LL\\** (see e.g.
Rossi et al. (2020) or Sesen et al. (2026) in the `References`).
Evaluation of the latter, in the actual function, proceeds via `sum` of
three main probability distributions for residuals (additional
distributions may be added in the newer package versions): 1. the
[`stats::dnorm`](https://rdrr.io/r/stats/Normal.html) (for the
normal/Gaussian distribution); 2. the
[`stats::dt`](https://rdrr.io/r/stats/TDist.html) (for the Student's
t-distribution) and 3. the
[`stats::dcauchy`](https://rdrr.io/r/stats/Cauchy.html), using the
`log = TRUE` option. The location parameter of every distribution is
fixed at `0` (due to residuals). For t-distribution the `df`/\\\nu\\ and
the `scale` parameters are unknown, therefore it is optimized by the
above-described \\LL\\ (in order to find the minimum a negative value is
considered) as well as by the
[`nlminb`](https://rdrr.io/r/stats/nlminb.html) function (the same
applies to Cauchy "distro" and the `scale` parameter). Even though the
Student's distribution may approach the normal one at `df > 29`,
sometimes the heavy tails of residuals for high number of observations
can be modeled by t-distribution with lower `df`. All probability
distributions are included in the function because not always the
residuals/errors follow the normal one. Particularly, if heavier tails
appear, e.g. for EPR simulation fits (please, refer to the `Examples` in
the
[`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md)).
Consequently, the function may automatically (see the argument
`residuals.distro`) decide which distribution fits the residuals/errors
the best, based on the lowest AIC, BIC values. This is additionally
supported by the Shapiro-Wilk
([`shapiro.test`](https://rdrr.io/r/stats/shapiro.test.html)) as well as
by the Kolmogorov-Smirnov
([`ks.test`](https://rdrr.io/r/stats/ks.test.html)) tests, providing the
information whether the residuals distribution can be considered as
normal (or not at all). **It is recommended to evaluate/apply both
information criteria**. The AIC tends to favor a more complex model
(over a simpler one) and thus suggests to "overfit" the data, whereas
the BIC is in favor of simpler models because it possesses a stronger
penalty (\\k\\ln(N)\\) for complex models than AIC (\\2\\k\\,see e.g.
Fabozzi et al. (2014) and Zhang Y, Meng G (2023) in the `References`).

## References

Fabozzi FJ, Focardi FM, Rachev ST, Arshanapalli BG (2014). *The Basics
of Financial Econometrics: Tools, Concepts, and Asset Management
Applications (Appendix E)*, John Wiley and Sons, Inc. ISBN
978-1-118-57320-4,
<https://onlinelibrary.wiley.com/doi/book/10.1002/9781118856406>.

Soch J et al. (2024). StatProofBook/StatProofBook.github.io: *The Book
of Statistical Proofs (Version 2023).*,
<https://statproofbook.github.io/>,
<https://doi.org/10.5281/ZENODO.4305949>.

Burnham KP, Anderson DR (2004). "Multimodel Interference: Understanding
AIC and BIC in Model Selection", *Sociol. Methods Res.*, **33**(2),
261-304, <https://doi.org/10.1177/0049124104268644>.

Thulin M (2025). *Modern Statistics with R: From Wrangling and Exploring
Data to Inference and Predictive Modeling*, 2nd edition (Version 2.0.2),
CRC Press and Taylor and Francis Group, LLC. ISBN 978-1-032-51244-0,
<https://www.modernstatisticswithr.com/>.

Zhang Y, Meng G (2023). "Simulation of an Adaptive Model Based on AIC
and BIC ARIMA Predictions", *J. Phys.: Conf. Ser.*, **2449**, 012027-7,
<https://doi.org/10.1088/1742-6596/2449/1/012027>.

Svetunkov I (2022). *Statistics for Business Analytics*, Version 2025,
<https://openforecast.org/sba/>.

Rossi R, Murari R, Gaudio P, Gelfusa M (2020). "Upgrading Model
Selection Criteria with Goodness of Fit Tests for Practical
Applications", *Entropy*, **22**(4), 447-13,
<https://doi.org/10.3390/e22040447>.

Hyndman RJ, Athanasopoulos G (2021). *Forecasting: Principles and
Practise*, 3rd edition, O Texts, ISBN 978-0-987-50713-6,
<https://otexts.com/fpp3/>.

Hyndman RJ (2013). "Facts and Fallacies of the AIC",
<https://robjhyndman.com/hyndsight/aic/>.

Sesen B et al. (2026). "AIC and BIC: Choosing the Right Model Without
Overfitting",
<https://sesen.ai/blog/aic-bic-model-selection-information-criteria>.

## See also

Other Simulations and Optimization:
[`eval_sim_EPR_iso()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md),
[`eval_sim_EPR_isoFit()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md),
[`eval_sim_EPR_isoFit_space()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit_space.md),
[`eval_sim_EPR_iso_combo()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso_combo.md),
[`optim_for_EPR_fitness()`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md),
[`plot_eval_EPRtheo_mltiplet()`](https://jatanrt.github.io/eprscope/reference/plot_eval_EPRtheo_mltiplet.md),
[`plot_eval_RA_forFit()`](https://jatanrt.github.io/eprscope/reference/plot_eval_RA_forFit.md),
[`quantify_EPR_Sim_series()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Sim_series.md),
[`smooth_EPR_Spec_by_npreg()`](https://jatanrt.github.io/eprscope/reference/smooth_EPR_Spec_by_npreg.md)

## Examples

``` r
## generate data frame with residuals,
## defined by the Student's t-distribution
set.seed(42)
res <- stats::rt(200, df = 4) * 1.3 ## heavy-tailed resids.
res.data.df <- data.frame(Residuals = res)
#
## evaluate the `ABIC_forFit` by setting
## the `residuals.distro = "auto"` (default)
list.abic <- eval_ABIC_forFit(
  data.fit = res.data.df,
  residuals = "Residuals",
  k = 5 ## hypothetical number of model parameters
)
#
## AIC and BIC vvector
list.abic$abic.vec
#> [1] 799.9209 822.4258
#
## Message in order to fit and prove the distribution
## with the degrees of freedom 5-6
list.abic$message
#> [1] "Information criteria evaluated using the"  
#> [2] "Student's t-distribution of residuals with"
#> [3] "5.6 degrees of freedom. Additionally"      
#> [4] "supported by the Shapiro-Wilk test."       
#
## compare the degrees of freedom with
## robust statistics by `MASS::fitdistr()`
mass.pkg.fit.t <- MASS::fitdistr(
  res.data.df$Residuals,
  "t", ## Student's distro
  lower = 1e-4
)
mass.pkg.fit.t$estimate[["df"]]
#> [1] 5.550807
#
## generate data frame with residuals
## defined by the Normal/Gaussian distribution
res.norm <- stats::rnorm(200,mean = 0,sd = 0.5)
res.norm.data.df <- data.frame(Residuals = res.norm)
#
## evaluate the `ABIC_forFit` by setting
## the `residuals.distro = "auto"` (default)
list.norm.abic <- eval_ABIC_forFit(
  data.fit = res.norm.data.df,
  residuals = "Residuals",
  k = 3 ## hypothetical number of model parameters
)
#
## the entire list for the distribution fit
list.norm.abic
#> $abic.vec
#> [1] 301.3228 314.3109
#> 
#> $message
#> [1] "Information criteria evaluated using the"   
#> [2] "normal distribution of residuals,"          
#> [3] "additionally supported by the Shapiro-Wilk" 
#> [4] "as well as by the Kolmogorov-Smirnov tests."
#> 
#
## check, if the Student's t-distribution fit
## gives higher AIC/BIC + no clear support
## by the Kolmogorov-Smirnov
## and/or by the Shapiro-Wilk tests,
## the number of degrees of freedom (`df`) should be
## relatively high, because with such an extreme `df`
## Student's distro -> Normal/Gaussian one
list.stud.abic <- eval_ABIC_forFit(
  data.fit = res.norm.data.df,
  residuals = "Residuals",
  residuals.distro = "t-dist",
  k = 3
)
list.stud.abic
#> $abic.vec
#> [1] 303.4228 319.6051
#> 
#> $message
#> [1] "Information criteria evaluated using the"  
#> [2] "Student's t-distribution of residuals with"
#> [3] "384894505.2 degrees of freedom. No clear"  
#> [4] "support by the Shapiro-Wilk and/or by the" 
#> [5] "Kolmogorov-Smirnov tests. p.value in"      
#> [6] "<0.01,0.05>."                              
#> 
#
## generate data frame with residuals
## defined by the Cauchy distribution
res.cauchy <-
  stats::rcauchy(2401,location = 24.06,scale = 241.43)
res.cauchy.data.df <- data.frame(Residuals = res.cauchy)
#
## automatic decision
list.cauchy.abic <- eval_ABIC_forFit(
  data.fit = res.cauchy.data.df,
  residuals = "Residuals",
  k = 3 ## hypothetical number of model parameters
)
#
## message
list.cauchy.abic$message
#> [1] "Information criteria evaluated using the"  
#> [2] "Cauchy distribution of residuals,"         
#> [3] "additionally supported by the Shapiro-Wilk"
#> [4] "test as well as by the Kolmogorov-Smirnov" 
#> [5] "tests."                                    
#
## Cauchy probability/residuals distribution can be considered
## as a special case of Student's t-distro with df(nu) = 1,
## therefore check by robust statistics:
mass.pkg.fit.tcauchy <-
  MASS::fitdistr(
    res.cauchy.data.df$Residuals,
    "t",
    lower = 1e-4
  )
mass.pkg.fit.tcauchy$estimate[["df"]]
#> [1] 0.9731462
#
## for additional applications, please
## refer to the Examples in `eval_sim_EPR_isoFit()`
## or `eval_kinR_EPR_modelFit()`
#

```
