EPRphysChemSpec
================

# Overview <img src="man/figures/logo.png" align="right" height="139"/>

The main goal of this package is to bring handy functions corresponding
to “everyday” data processing/operations in EPR ([Electron Paramagnetic
Resonance](https://goldbook.iupac.org/terms/view/E02005)) spectroscopy.
The package doesn’t want to replace the great EPR simulation/processing
[EasySpin Toolbox](https://www.easyspin.org/) for
[MATLAB](https://www.mathworks.com/products/matlab.html) . Rather, it
may be considered like a complimentary package/toolbox with practical
functions which have to be otherwise performed by the combination of
several software platforms like *MS Office*, *Orgin/SigmaPlot/Igor* (in
general graphing software) as well as acquisition/processing software
supplied by the EPR spectrometer producers (see
e.g. [*Xenon/WinEPR*](https://www.bruker.com/en/products-and-solutions/mr/epr-instruments/epr-software.html)).
These involve the following principal operations:

- Reading the universal ASCII data format (e.g. like `.asc`, `.txt`,
  `.csv`) and transforming into tables (data frames) incl. operations
  like intensity normalization and conversion between magnetic flux
  density (*B*) units ($\mathrm{G} \Leftrightarrow \mathrm{mT}$).
  Additionally, the available functions enable the automatic reading of
  time series experiments like a kinetic one (e.g. also with the
  combination of *in situ* electrochemistry or
  photochemistry/irradiation) or reading and processing several EPR
  spectra data/files at once for the series of individual
  experiments/data files upon changing the quantities like
  (time,microwave power, electrochemical potential, temperature,
  concentration…etc.). Finally, reading the *MATLAB* (`.mat`) files and
  the instrumental parameter ones ( in case of *BRUKER* instruments, so
  far[^1] ) and the corresponding parameter extraction can be performed
  as well.

- Plotting the EPR spectra incl. time series/kinetics or their
  dependency on other quantities (like already mentioned above) as well
  as presentation of simulated spectra are provided by several functions
  based on either [ggplot2](https://ggplot2.tidyverse.org/)
  package/system (one of the most comprehensive visualization system in
  data science and data analytics) or interactive
  [plotly](https://plotly.com/r/) graphing library. Especially the later
  …

- Blah

- Blah

# Installation

``` r
## So far, the package can be installed by
```

# Usage

Blah

[^1]: Reading parameter files from additional EPR
    instruments/manufacturers can be added acccordingly
