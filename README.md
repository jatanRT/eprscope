eprscope
================

# Overview <img src="man/figures/logo_new.png" align="right" height="139"/>

The main goal of this package is to bring handy functions corresponding
to “everyday” data processing/operations in EPR ([Electron Paramagnetic
Resonance](https://goldbook.iupac.org/terms/view/E02005)) spectroscopy.
The package doesn’t want to replace the great EPR simulation/processing
[EasySpin Toolbox](https://www.easyspin.org/) for
[MATLAB](https://www.mathworks.com/products/matlab.html) . Rather, it
may be considered like a complimentary package/toolbox with practical
functions which have to be otherwise performed by the combination of
several proprietary software platforms like *MS Office*,
*Orgin/SigmaPlot/Igor* (in general graphing software) as well as
acquisition/processing software supplied by the EPR spectrometer
manufacturers (see
e.g. [*Xenon/WinEPR*](https://www.bruker.com/en/products-and-solutions/mr/epr-instruments/epr-software.html)).
These involve the following principal operation groups:

- Reading the universal ASCII data format (e.g. like `.asc`, `.txt`,
  `.csv`) and transforming into tables (data frames) incl. operations
  like intensity normalization and conversion between magnetic flux
  density (*B*) units (G ⟺ mT). Additionally, the functions enable the
  automatic reading of time series experiments like a kinetic one
  (e.g. also with the combination of *in situ* electrochemistry or
  photochemistry/irradiation) or reading and processing several EPR
  spectra data/files, at once, for the series of individual
  experiments/data files upon changing the quantities like
  time,microwave power, electrochemical potential, temperature,
  concentration…etc. Finally, reading the *MATLAB* (`.mat`) files and
  the instrumental parameter ones ( in case of *BRUKER* instruments, so
  far[^1] ) and the corresponding parameter extraction can be performed
  as well.

- Plotting the EPR spectra incl. time series/kinetics or their
  dependency on other quantities (like already mentioned above) as well
  as presentation of simulated spectra are provided by several functions
  based on either [ggplot2](https://ggplot2.tidyverse.org/)
  package/system (one of the most comprehensive system for data
  visualization) or interactive [plotly](https://plotly.com/r/) graphing
  library. Especially, the later may represent a valuable alternative to
  EPR instrumental software or other proprietary graphing systems,
  because it includes visualization tools like zooming, panning,
  data/values hovering and much more.

- Quantifying the number of paramagnetic species (e.g. radicals) based
  on integrated forms of derivative EPR spectra and instrumental
  parameters. In this respect also baseline polynomial corrections are
  included to precisely evaluate the double integrals by numeric
  [trapezoidal
  method](https://mathworld.wolfram.com/TrapezoidalRule.html). In case
  of lower signal-to-noise ratios i.e. in noisy EPR spectra, they can be
  optimized by least-square fitting of simulated ones (e.g. from
  EasySpin) and single or double integrals can be evaluated using the
  optimized intensity of the simulated EPR spectrum. This is
  particularly advantageous in (time) series of many noisy EPR spectra.
  Such operations have been available only on EPR
  instruments/spectrometers, up to now.

- Last but not least, there are functions to find extremes (maxima,
  minima), linewidths (incl. 𝚫*B*<sub>pp</sub>) as well as *g*-values
  directly from the spectral data. Eexperimentally (or by spectral
  simulations) determined EPR spectral parameters like *g*-values and
  hyperfine coupling (*A*)/splitting (*a*) constants are quite often
  compared to theoretically predicted by quantum chemical calculations
  (e.g. by [Density Functional Theory](https://gaussian.com/dft/)).
  Therefore

# Installation

``` r
## So far, the package can be installed by
```

# Usage

Blah

[^1]: Reading parameter files from additional EPR
    instruments/manufacturers can be added acccordingly
