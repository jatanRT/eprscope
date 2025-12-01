# General Diagnostics for Models/Fits by Simple Residual Analysis

Three visual diagnostic tools (based on the
[`{ggplot2}`](https://ggplot2.tidyverse.org/) package) and one metric
(standard deviation of residuals) are applied to evaluate the
appropriateness as well as to compare different models/fits. 1. The
first plot represents the Residuals *vs* Fitted/Simulated Values
relation. For a decent model/fit, it will exhibit randomly scattered
values around `0` and displays a similar variance over all
predicted/fitted values. 2. *Sample Quantiles (Residuals) vs Theoretical
Quantiles* (Q-Q plot) shows, whether the appearance of residuals can be
described by the three probability distributions:
`c("norm","t","cauchy")` (i.e. Normal or Student's t or Cauchy, see also
the
[`eval_ABIC_forFit`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md)
function). 3. The latter is combined with a more detailed information
about the distribution of residuals by the **histogram**
([`geom_histogram`](https://ggplot2.tidyverse.org/reference/geom_histogram.html))
as well as by the **probability density**
([`geom_density`](https://ggplot2.tidyverse.org/reference/geom_density.html),
including the residuals `mean` value, and `median` which in ideal case
equal to `0`).

## Usage

``` r
plot_eval_RA_forFit(
  data.fit,
  residuals = NULL,
  fitted = NULL,
  resid.method.smooth = "loess",
  resid.xlab = NULL,
  k,
  level.cnfd = 0.95
)
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

- fitted:

  Character string, pointing to variable/column header with (model)
  fitted/predicted values, depending on the `data.fit` argument (usually
  `fitted = "fit(ted)"`, `fitted = "predicted"` or `fitted = "theory"`).
  **Default**: `fitted = NULL`.

- resid.method.smooth:

  Character string, corresponding to smoothing method (function) to
  follow the trends of residual plot (Residuals *vs* Fitted/Predicted
  values). **Default**: `resid.method.smooth = "loess"` ("local
  regression" or "locally estimated scatter plot smoothing"). Additional
  methods like `"lm"` (linear method/model) or `"glm"` can be applied as
  well. For details and if `resid.method = NULL` (related to automatic
  method selection, based on number of points), please consult the
  [`geom_smooth`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)
  for documentation.

- resid.xlab:

  Character string, pointing to \\x\\-axis label for residual plot
  (Residuals *vs* Fitted/Predicted values). **Default**:
  `resid.xlab = NULL`, actually corresponding to
  `"Fitted or Simulated Values"`. Customized labels like
  `"Kinetic Model Fit (qvar)"` or `"Quantity (unit)"` can be applied as
  well.

- k:

  Numeric value identical to number of parameters used for the model/fit
  (see e.g. `Examples` in the
  [`eval_kinR_EPR_modelFit`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md)
  where `k = 2`).

- level.cnfd:

  Numeric value, identical with level of the applied confidence interval
  (see also the `se` argument). **Default**: `level.cnfd = 0.95`.

## Value

A List, consisting of the following elements is returned:

- df:

  Original `data.fit` data frame object, if additional
  processing/analysis is required (see the `Details` or to perform
  Residuals *vs* Observation Order to verify the assumption that the
  residuals are independent from one another).

- plot.rqq():

  Function, related to visual **r**esidual **a**nalysis), returning two
  main plots: Residuals *vs* Predicted/Fitted Values from the model/fit
  or simulation, and the Q-Q plot: Sample Quantiles *vs* Theoretical
  Quantiles, where the theoretical ones correspond to a specific
  distribution (normal/Student's/Cauchy). Therefore, function has the
  following arguments:

  1.  `residuals.distro = c("norm","t","cauchy")` (**default**:
      `residuals.distro = "norm"`)

  2.  `confidence = level.cnfd` (**default**:
      `confidence = level.cnfd = 0.95`)

  3.  `...` additional arguments/parameters for the distro/distribution
      like `df` (degrees of freedom) for the Student's t one.

- plot.histDens:

  Ggplot2 object, showing the **hist**ogram and the scaled probability
  **dens**ity function for residuals. The corresponding residuals mean
  value and the median are identified by vertical lines.

- sd:

  **S**tandard **d**eviation of residuals (or residual standard error
  (RSE)) for the model/fit defined as: \$\$\sqrt{\sum_i (y_i -
  y\_{i,\text{fit/model}})^2\\/\\(N - k - 1)}\$\$ where \\N\\ is the
  number of observations/points (see the `data.fit` argument) and \\k\\
  is the number of optimized parameters (see the argument `k`).
  Therefore, the smaller the `sd`, the better the fit, when comparing
  different models/fits.

## Details

Analysis of residuals is a powerful statistical technique to check
whether the model/fit has adequately captured the information in the
data (please, also refer to the
[`eval_ABIC_forFit`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md)).
The \\i\\-th residual/error (in the series) is defined as the difference
between the \\i\\-th observed response value (\\y_i\\) and that of the
predicted/fitted by the model (\\\hat{y}\_i\\ or
\\y\_{\text{fit/model}}\\): \$\$e_i = y_i - \hat{y}\_i = y_i -
y\_{\text{fit/model}}\$\$ One might think of residuals as "leftovers",
i.e. these are the issues/items which are unexplained, after the
model/fit has been applied. The residual analysis therefore helps us to
to determine whether our model/fit missed an important pattern or not at
all (see Chugani (2025) in the `References`). Residuals are also used in
least square fitting/optimization procedures where the sum of their
squares is to be minimized throughout the iterations (see e.g.
[`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md)
or
[`eval_kinR_EPR_modelFit`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md)).

In addition to the original "raw" residuals defined above, one may come
across other types, like ["Pearson" (or
"scaled")](https://bookdown.org/mike/data_analysis/generalized-linear-models.html),
**"standardized" or "studentized"**. **The latter two are helpful to
identify outliers**, which are the data points that are significantly
different from the rest of the data (though, they can be also identified
by the Q-Q plot and histogram-probability density). For such reason the
"raw" residuals (\\e_i\\) are divided by their standard deviation
(\\sd\\ see the `Value` below), including the the effect of leverage.
Thus, the formula for standardized residual reads: \\r_i =
e_i\\/\\(sd\\\sqrt{1 - h\_{ii}})\\, where the \\h\_{ii}\\ stands for the
diagonal element of the "leverage" \\\hat{H}\\ matrix (see e.g.
`References` 11-13). The simple explanation of the leverage phenomenon
is following (see e.g. Speegle D, Clair B (2021) or The Pennsylvania
State University (2018) in the `References`). The fit or the regression
line goes through the center of mass of the (experimental) data
(\\x\\,\\y\\). Lets assume two points \\A\\ and \\B\\, where the \\A\\
\\x\\-coordinate is close to the mean value of predictors (\\mean(x)\\)
and the \\B\\ possesses an extreme \\x\\ (far from the mean value).
Therefore, changing the \\A\\(y)\\ will induce a small effect (low
leverage) on the fit/regression line, whereas the \\B\\(y)\\ change will
dramatically influence the fit (high leverage). **Both standardized and
studentized residuals can be automatically calculated for linear
models** like [`lm`](https://rdrr.io/r/stats/lm.html) and
[`glm`](https://rdrr.io/r/stats/glm.html), using the
[`stats::rstandard`](https://rdrr.io/r/stats/influence.measures.html)
and
[`stats::rstudent`](https://rdrr.io/r/stats/influence.measures.html). A
very detailed analysis for linear models is provided by the
[`{ggResidpanel}`](https://goodekat.github.io/ggResidpanel/) package.
Additionally, a series of diagnostic plots can be also created in the
base *R*, just by `plot(var)`, where the `var` represents the
variable/object of the linear model. On the other hand, **all these
diagnostics are not available for non-linear models** like
[`nls`](https://rdrr.io/r/stats/nls.html). Accordingly, **such type of
calculations can be provided by other packages** (see e.g.
[`{nlstools}`](https://cran.r-project.org/web/packages/nlstools/vignettes/vignetteJSS.pdf))
**or must be performed "manually", evaluating the numerical
approximation of the gradient** (please, refer to the
[jacobian](https://r-forge.r-universe.dev/numDeriv/doc/manual.html#jacobian))
as reported elsewhere (see Nguyen M (2020) in the `References`).
Consequently, the calculations of standardized and studentized residuals
are not involved in this general `plot_eval_RA_forFit` function.
Nevertheless, users are advised to apply above-described options to
obtain these specialized residuals or diagnostic plots for a specific
model/fit (also refer to the `Value`/`df`) if needed.

**Considerations to support or exclude model/fit based on the residual
plot.** If the residuals exhibit no clear pattern, like they are
randomly scattered around the `0` with no systematic increase or
decrease in variance, we may trust our fit with the optimized
parameters. Such pattern is **homoscedastic**. However, if one
recognizes curved ((inverted-)U shape, see e.g. `Examples` in the
[`eval_kinR_EPR_modelFit`](https://jatanrt.github.io/eprscope/reference/eval_kinR_EPR_modelFit.md)),
wave or systematic increase (so called "fanning") or decrease
("funelling"), the model/fit is untrustworthy and one would probably
search for a different (better) one. In particular, the curved pattern
in the residual plot may indicate that a model does a poor job of
fitting and likely, we need additional parameter(s) to describe our data
properly. In the case if residuals suffer from unequal variance at
different levels of the fitted values, the residuals are referred to as
**heteroscedastic**. In order to predict pattern of the residual plot,
the corresponding relationship is fitted by the `loess` (default) or
`lm`/`glm` function (see the `resid.method.smooth` argument) where the
confidence level band of the fitted/regression line can be controlled by
the `level.cnfd` argument or by the `Value` output function `plot.rqq()`
(and its `confidence` argument).

**Considerations to support or exclude model/fit based on the Q-Q plot,
histogram and probability density.** If the residuals are assumed to be
normally distributed, one can use a normal Q-Q (Quantile-Quantile) plot
to check this assumption or its violation. Quantiles are often referred
to as "percentiles". These are actually the data points, dividing the
distribution into equal portions. For instance, for a 0.5 quantile (or
the 2nd quartile), half of the data lie below this point and half of
them above. This quantile is also referred to as **median**. Similarly,
the 0.25 quantile (or the 1st quartile) would mean, that \\25\\\\\\ of
the data fall below this point. Thus, the Q-Q plot presents quantiles
from our sample (corresponding to residuals) related to the theoretical
ones calculated for a specific distribution. For such purpose the
`plot_eval_RA_forFit` function supports three residual distributions:
the normal one, Student's t (including degrees of freedom, `df`) as well
as the Cauchy one. If the points follow the diagonal line (or are not
far from this line), we may describe our residuals by the specific
distribution. Additionally, e.g. for normal "distro", the Q-Q plot can
be also supported by the Shapiro-Wilk
([`shapiro.test`](https://rdrr.io/r/stats/shapiro.test.html)) and/or by
the Kolmogorov-Smirnov
([`ks.test`](https://rdrr.io/r/stats/ks.test.html)) which are both
included in the
[`eval_ABIC_forFit`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md).
Therefore, the latter nicely works in combination with the actual
function to get residuals characteristics (distribution) and
consequently, to decide which model/fit is the best. Deviations from the
diagonal/fitted line are also reflected in the histogram and probability
density function/graph (PDF, providing the chances that the value of a
random continuous variable will occur within a specific range of
values). For the normal symmetric distribution it is represented by the
bell-shaped curve with the maximum at the **mean** (for residuals \\=
0\\, median = mean). Thus, the PDF basically corresponds to histogram
with "extremely" high number of bins, having "extremely" small widths. A
Q-Q plot may exhibit several basic
[deviations](https://stats.libretexts.org/Bookshelves/Advanced_Statistics/Intermediate_Statistics_with_R_(Greenwood)/03%3A_One-Way_ANOVA/3.04%3A_ANOVA_model_diagnostics_including_QQ-plots).
It can display a U-shaped pattern, which actually mirrors the situation
with the right skewed (or positively skewed, mean \> median) PDF.
Therefore, we find the extreme values far from the peak on the high end
more frequently than on the lower one (see e.g. `Example` in
[`eval_kinR_Eyring_GHS`](https://jatanrt.github.io/eprscope/reference/eval_kinR_Eyring_GHS.md)).
Contrary, if the Q-Q plot shows "hill" shape, the opposite situation is
observed and the extreme values (outliers) far from the peak on the low
end appear more frequently than on the higher one (PDF is left skewed,
mean \< median). Often, the heavy-tailed Q-Q plot with extreme residuals
below and above minima and maxima of the diagonal line, respectively,
may appear and is somewhat problematic for e.g. normal distributions of
residuals with outliers on both sides. On the other hand, such kind of
normality violation can be successfully described by Student's
t-distribution with lower degrees of freedom (see e.g. `Examples` in the
[`eval_sim_EPR_isoFit`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md).
Nevertheless, if the residuals, in the latter case, do not exhibit
heteroscedasticity, such model/fit is not necessarily untrustworthy. In
a very extreme case the heavy-tailed Q-Q plot may be transformed into
situation where only a couple of points around the "middle" quantile can
be found on the diagonal line and the remaining points are represented
by the noticeable S-curve. This is reflected as bimodal behavior in the
PDF and it might be the sign of a value clustering. The Q-Q plot (with
"pointwise" confidence bands for the diagonal/fitted line, obtained by
the [`rlm`](https://rdrr.io/pkg/MASS/man/rlm.html)) in the actual
function was built in the similar way like for the
[`{qqplotr}`](https://github.com/aloy/qqplotr/) package. The "pointwise"
bands are based on the normal confidence intervals. The confidence level
of those bands can be controlled by the `level.cnfd` argument or by the
`Value` output function `plot.rqq()` (and its `confidence` argument).

All the above-mentioned violations of the residuals (normal)
distribution can disfavor our considered model/fit. However, one has to
perform different diagnostic methods and tests to analyze the residuals
in order to compare several models/fits and select the "best" one. Even
in such case, this should be a compromise between the fit accuracy
(fitting the data as well as possible, including the physico-chemical
reality of the system) and the parsimony (using a simple and replicable
model/fit, Kabacoff RI (2022) in the `References`).

## References

Kabacoff RI (2022). *R in Action*, 3rd edition, Manning Publications
Co., ISBN 978-1-617-29605-5,
<https://www.manning.com/books/r-in-action-third-edition>.

Svetunkov I (2022). *Statistics for Business Analytics*, Version 2025,
<https://openforecast.org/sba/>.

Hyndman RJ, Athanasopoulos G (2021). *Forecasting: Principles and
Practise*, 3rd edition, O Texts, ISBN 978-0-987-50713-6,
<https://otexts.com/fpp3/>.

Chugani V (2025). "The Concise Guide to Residual Analysis",
<https://www.statology.org/concise-guide-residual-analysis/>.

Boehmke B, Greenwell B (2020). *Hand on Machine Learning with R*, 1st
edition, Chapman and Hall/CRC, ISBN 978-1-138-49568-5,
<https://bradleyboehmke.github.io/HOML/>.

Kuhn M, Silge J (2023). *Tidy Modelling with R*, 1st edition (Version
1.0.0), O'Reilly Media, ISBN 978-1-492-09648-1, <https://www.tmwr.org/>.

Belzile L (2019). "lineaRmodels",
<https://lbelzile.github.io/lineaRmodels/>

James G, Witten D, Hastie T, Tibshirani R (2021). *An Introduction to
Statistical Learning: with Applications in R*, 2nd edition, Springer,
ISBN 978-1-071-61417-4, <https://www.statlearning.com/>.

Speegle D, Clair B (2021). *Probability, Statistics and Data: A Fresh
Approach Using R*, 1st edition (Version 2024), Chapman and Hall/CRC,
ISBN 978-0-367-43667-4, <https://probstatsdata.com/>.

The Pennsylvania State University (2018). "STAT 462 Applied Regression
Analysis, Lesson 9: Influential Points",
<https://online.stat.psu.edu/stat462/node/87/>.

Nguyen M (2020). "A Guide on Data Analysis",
<https://bookdown.org/mike/data_analysis/>.

Gray JB, Woodal WH (1994). "The Maximum Size of Standardized and
Internally Studentized Residuals in Regression Analysis", *Am. Stat.*,
**48**(2), 111-113, <https://www.jstor.org/stable/2684258>.

Frost J (2025). "Statistics by Jim: Making Statistics Intuitive",
<https://statisticsbyjim.com/>.

Kross S (2016). "A Q-Q Plot Dissection Kit",
<https://seankross.com/2016/02/29/A-Q-Q-Plot-Dissection-Kit.html>.

Walker JA (2020). "Normal Q-Q Plots - what is the robust Line and should
we prefer it ?",
<https://rdoodles.rbind.io/posts-biocstyle/2020-10-15-normal-q-q-plots-what-is-the-robust-line-and-should-we-prefer-it>.

## See also

Other Simulations and Optimization:
[`eval_ABIC_forFit()`](https://jatanrt.github.io/eprscope/reference/eval_ABIC_forFit.md),
[`eval_sim_EPR_iso()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso.md),
[`eval_sim_EPR_isoFit()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit.md),
[`eval_sim_EPR_isoFit_space()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_isoFit_space.md),
[`eval_sim_EPR_iso_combo()`](https://jatanrt.github.io/eprscope/reference/eval_sim_EPR_iso_combo.md),
[`optim_for_EPR_fitness()`](https://jatanrt.github.io/eprscope/reference/optim_for_EPR_fitness.md),
[`plot_eval_EPRtheo_mltiplet()`](https://jatanrt.github.io/eprscope/reference/plot_eval_EPRtheo_mltiplet.md),
[`quantify_EPR_Sim_series()`](https://jatanrt.github.io/eprscope/reference/quantify_EPR_Sim_series.md),
[`smooth_EPR_Spec_by_npreg()`](https://jatanrt.github.io/eprscope/reference/smooth_EPR_Spec_by_npreg.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## application example for an EPR simulation fit
list.test <-
  plot_eval_RA_forFit(
    data.fit = data.sim.expr,
    residuals = "Residuals",
    fitted = "Simulation",
    resid.xlab = "Simulation",
    k = length(optim.params.init),
    level.cnfd = 0.99
 )
#
## residual and the normal Q-Q plot
list.test$plot.rqq()
#
## residual and Q-Q plot for the Student's t distro
## with 4 degrees of freedom
list.test$plot.rqq(residuals.distro = "t",df = 4)
#
## histogram and probability density
list.test$plot.histDens
#
## standard deviation of residuals
list.test$sd
#
## from the data, quickly create the residuals vs
## observation order plot (assuming there
## is no index column in the data frame)
dataframe <- list.test$df
dataframe[["index"]] <- 1:nrow(dataframe)
plot(
  dataframe$index,
  dataframe$Residuals,
  xlab = "Observation Order",
  ylab = "Residuals"
)
#
## for additional examples, please,
## refer to the `eval_sim_EPR_isoFit`
## or `eval_kinR_EPR_modelFit`
#
} # }

```
