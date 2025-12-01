# Confidence Interval of a Vector or Data Frame Column

Calculation of the mean value and its confidence limits (according to
Student's t-distribution), corresponding to data frame column or vector,
characterizing dispersion of the individual values such as double
integrals in quantitative EPR analysis, g-values or linewidths,...etc
from several experiments/observations.

## Usage

``` r
eval_interval_cnfd_tVec(data.vec.col, level.cnfd = 0.95, separate = TRUE)
```

## Arguments

- data.vec.col:

  Numeric vector (pointing to column) of interest (within a data frame)
  to calculate the confidence interval or uncertainty (margin of error).

- level.cnfd:

  Numeric (floating) value, corresponding to confidence level
  (**default**: `level.cnfd = 0.95`). This value is related to
  significance level \\alpha\\ defined by \\1 - confidence\\\\level\\.

- separate:

  Logical, whether to separate the mean value and the uncertainty
  (margin of error), corresponding to non-negative right/left confidence
  limit of the mean. If `separate = TRUE` (**default**), the result is
  shown as a named vector with the (mean) `value` and the `uncertaity`.
  Otherwise, the result is returned in the format of
  \\value\pm\\\\uncertainty\\.

## Value

Named vector of (mean) `value` and `uncertaity` or
\\value\pm\\\\uncertainty\\ format depending on the `separate` argument,
where the uncertainty actually represents non-negative limit for the
mean (one side of the confidence interval not including the mean value).

## Details

The confidence interval evaluation suggests two-tailed Student's
t-distribution, which for number of observations \\N \> 30\\ approaches
the normal \\z\\-distribution. Evaluation of the confidence interval
and/or its limits can be well documented on \\g\\-factor series example:
\$\$g = \overline{g}\pm (t\_{(1-\alpha/2),df}\\s/\sqrt{N})\$\$ where
\\\overline{g}\\ is the mean value and the \\t\_{(1-\alpha/2),df}\\
corresponds to the quantile of the t-distribution having the
significance level of \\\alpha\\ (\\1-\alpha = condfidence\\\\level\\,
see the `level.cnfd` argument) and the degrees of freedom \\df = N -1\\.
Finally, the \\s\\ represents the sample standard deviation, defined by
the following relation (regarding the \\g\\-value series example): \$\$s
= \sqrt{(1/(N-1))\\\sum\_{i=1}^N (g_i - \overline{g})^2}\$\$ which is
computed by the [`sd`](https://rdrr.io/r/stats/sd.html) function. The
above-mentioned \\t\\-quantile is actually calculated by the
[`stats::qt`](https://rdrr.io/r/stats/TDist.html). Alternatively, one
could also evaluate confidence interval by the one sample
[`t.test`](https://rdrr.io/r/stats/t.test.html) for a certain level of
confidence, giving a descriptive output with statistical
characteristics.

## References

Miller JN, Miller JC, Miller RD (2018). *Statistics and Chemometrics for
Analytical Chemistry*, 7th edition, Pearson Education. ISBN
978-1-292-18674-0,
<https://elibrary.pearson.de/book/99.150005/9781292186726>.

Hayes A, Moller-Trane R, Jordan D, Northrop P, Lang MN, Zeileis A
(2022). *distributions3: Probability Distributions as S3 Objects*.
<https://github.com/alexpghayes/distributions3>,
<https://alexpghayes.github.io/distributions3/>, see also *Vignette:
t-confidence interval for a mean*,
<https://alexpghayes.github.io/distributions3/articles/one-sample-t-confidence-interval.html>.

National Institute of Standards and Technology (NIST) (2012).
“Confidence Limits for the Mean”,
<https://www.itl.nist.gov/div898/handbook/eda/section3/eda352.htm>.

Kaleta J, Tarábek J, Akdag A, Pohl R, Michl J (2012). “The 16
CB11(CH3)n(CD3)12–N• Radicals with 5-Fold Substitution Symmetry: Spin
Density Distribution in CB11Me12•”, *Inorg. Chem.*, **51**(20),
10819–10824, <https://doi.org/10.1021/ic301236s>.

Curley JP, Milewski TM (2020). “PSY317L Guidebook - Confidence
Intervals”,
<https://bookdown.org/curleyjp0/psy317l_guides5/confidence-intervals.html>.

Goodson DZ (2011). *Mathematical Methods for Physical and Analytical
Chemistry*, 1st edition, John Wiley and Sons, Inc. ISBN
978-0-470-47354-2,
<https://onlinelibrary.wiley.com/doi/book/10.1002/9781118135204>.

## See also

Other Evaluations:
[`eval_DeltaXpp_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_DeltaXpp_Spec.md),
[`eval_FWHMx_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_FWHMx_Spec.md),
[`eval_extremeX_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_extremeX_Spec.md),
[`eval_gFactor()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor.md),
[`eval_gFactor_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_gFactor_Spec.md),
[`eval_kinR_Eyring_GHS()`](https://jatanrt.github.io/eprscope/reference/eval_kinR_Eyring_GHS.md),
[`eval_nu_ENDOR()`](https://jatanrt.github.io/eprscope/reference/eval_nu_ENDOR.md),
[`eval_peakPick_Spec()`](https://jatanrt.github.io/eprscope/reference/eval_peakPick_Spec.md)

## Examples

``` r
## double integral/intensity values
## coming from several experiments:
di.vec <- c(0.025,0.020,0.031,0.022,0.035)
#
## evaluation of the confidence interval
## in different formats:
eval_interval_cnfd_tVec(di.vec)
#>        value  uncertainty 
#> 0.0266000000 0.0077839559 
#
eval_interval_cnfd_tVec(di.vec,
                        level.cnfd = 0.95,
                        separate = FALSE)
#> 0.027(8)

```
