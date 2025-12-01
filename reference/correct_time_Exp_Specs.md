# Time Correction for the Experimental CW EPR Time Series.

Providing more accurate time for EPR spectral line/spectrum appearance.
It is assumed that the middle \\B\\ (or \\g\\,
\\\nu\_{\text{MHz}}\\...etc.) of the EPR spectrum is set as the CF
(`central field`) for the spectrum sweep.

## Usage

``` r
correct_time_Exp_Specs(time.s, Nscans, sweep.time.s)
```

## Arguments

- time.s:

  Numeric value/vector/column in `data frame`, corresponding to `time`
  (in `s`) at which the individual EPR spectra were recorded (supplied
  by the EPR acquisition software).

- Nscans:

  Numeric, number of accumulations (number of scans usually denoted as
  `AVGS`) for each spectrum in EPR time series.

- sweep.time.s:

  Numeric, time (in `s`) for recording individual EPR spectrum
  \\\equiv\\ one "accumulation".

## Value

Numeric value/vector, corresponding to time at which the middle
(\\x\\-axis) of EPR spectrum/spectra were recorded during the kinetic
measurements (e.g. radical formation, stability, electrochemical and/or
photochemical measurements).

## Details

The actual time at the middle/crossing point is different from that
recorder by the EPR acquisition software, see below. This is especially
important in determining the kinetics of radical generation or decay.
Time is recorded according to the following scheme, where "^v" in the
scheme denotes the derivative form of an EPR spectrum:

|        |                  |              |         |        |                  |
|--------|------------------|--------------|---------|--------|------------------|
|        | EPR Spectr.      |              |         |        | EPR Spectr.      |
| `t[1]` | ——^v——\>         | `t[2]`-delay | —-\>    | `t[2]` | ——^v——-\> ...    |
|        | `N_scans`\*`swt` |              | `delay` |        | `N_scans`\*`swt` |

The recorded times are: `t[1]`,`t[2]`,`t[3]`,... and the `N_scans`
corresponds to `number of scans`, `swt` to `sweep time` for the
individual scan. These parameters can be obtained by the
[`readEPR_params_slct_kin`](https://jatanrt.github.io/eprscope/reference/readEPR_params_slct_kin.md)
or other functions which can read the instrumental parameter files.

## See also

Other Conversions and Corrections:
[`convert_A_MHz_2a()`](https://jatanrt.github.io/eprscope/reference/convert_A_MHz_2a.md),
[`convert_B()`](https://jatanrt.github.io/eprscope/reference/convert_B.md),
[`convert_a_mT_2A()`](https://jatanrt.github.io/eprscope/reference/convert_a_mT_2A.md),
[`convert_time2var()`](https://jatanrt.github.io/eprscope/reference/convert_time2var.md)

## Examples

``` r
## 12 s recorded by spectrometer, 6 accumulations
## by the sweep time of 6 s
correct_time_Exp_Specs(12,Nscans = 6,6)
#> [1] 45

```
