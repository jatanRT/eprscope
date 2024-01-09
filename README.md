eprscope
================

## Overview <img src="man/figures/logo_new.png" align="right" height="139"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The key objective of this fully open source package is to bring handy
functions corresponding to “everyday” data processing/analysis in the
EPR ([Electron Paramagnetic
Resonance](https://chem.libretexts.org/Bookshelves/Physical_and_Theoretical_Chemistry_Textbook_Maps/Electron_Paramagnetic_Resonance_(Jenschke)))
spectroscopy mainly in chemistry. The package doesn’t want to replace
the excellent EPR simulation/processing [EasySpin
Toolbox](https://www.easyspin.org/) for
[MATLAB](https://www.mathworks.com/products/matlab.html) . Rather, it
may be considered like a complimentary package/toolbox with practical
functions which have to be otherwise performed by several tools,
e.g. like by the acquisition/processing software, supplied by the EPR
spectrometer manufacturers (see
e.g. [*Xenon/WinEPR*](https://www.bruker.com/en/products-and-solutions/mr/epr-instruments/epr-software.html))
as well as by several other proprietary software platforms like the *MS
Office* and/or *Orgin/SigmaPlot/Igor*. Therefore, the `{eprscope}` also
tries to reduce such many steps if the above-mentioned software
combination is adopted.

## Installation

``` r
# So far, the package can be installed by =>

# install.packages("devtools")
# devtools::install_github("jatanRT/eprscope.git")
```

Before the installation, please make sure that you have already followed
instructions for the [{nloptr} package
installation](https://astamm.github.io/nloptr/) depending on your
operating system. This package is required for the proper running of
optimization/fitting functions of the `{eprscope}` .

Completely new
<img src="https://www.r-project.org/Rlogo.png" width="16" height="16"/>
users or people who haven’t already installed the *R* environment,
please consult ➨

1.  [the R installation procedure](https://cran.rstudio.com/)

2.  [installation of the Rstudio
    IDE](https://posit.co/download/rstudio-desktop/)

3.  [the latest R tools
    release](https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html)
    **ONLY for WINDOWS OS**

Additionally, **an open-source scientific and technical publishing
system** [Quarto](https://quarto.org/) together with
[Pandoc](https://pandoc.org/), **a document converter system**, may be
needed for sharing the results coming from `{eprscope}` in desired
formats like `pdf` , `html` , `docx` , `pptx` or `tex`.

## Usage

In this section couple of examples are shown in order to briefly
demonstrate functionality of the package. More detailed description can
be found within the package articles/vignettes or documentation
examples.

### Reading Files with Instrumental Parameters

``` r
library(eprscope)
#
# built-in package file => "TMPD_specelchem_accu_b.par"
tmpd.params.file <- 
  load_data_example(file = "TMPD_specelchem_accu_b.par")
#
# parameters into interactive table (data frame)
tmpd.params.dt <- 
  readEPR_params_tabs(path_to_dsc_par = tmpd.params.file,
                      origin = "winepr",
                      interact = "params")
#
# table preview
tmpd.params.dt
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Depict Molecular Structures

``` r
# Phenylalenyl (Perinaphthenyl or PNT) radical from `SMILES`
# "C1([C.]23)=CC=CC2=CC=CC3=CC=C1" code
draw_molecule_by_rcdk(molecule = "C1([C.]23)=CC=CC2=CC=CC3=CC=C1",
                      mol.label = "Phenylalenyl",
                      mol.label.color = "black",
                      mol.label.xy.posit = c(8.8,1.2))
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Simulation of Isotropic EPR Spectra

As an example “PNT” radical (see the structure above)

``` r
# Simulation of phenylalenyl (perinaphthenyl or PNT) radical,
# see also https://pubs.rsc.org/en/content/articlelanding/2006/CS/b500509b.
# additional experimental/instrumental parameters are not shown,
# and posses their default values => see the corresponding documentation 
# of `eval_sim_EPR_iso()` function.
#
simulation.iso <- 
  eval_sim_EPR_iso(g.iso = 2.0027,
                   B.unit = "G",
                   nuclear.system = list(list("1H",3,5.09),
                                         list("1H",6,17.67)),
                   natur.abund = T,
                   lineGL.DeltaB = list(0.42,NULL))
#
# spectrum preview
simulation.iso$plot + ggplot2::coord_cartesian(xlim = c(3462,3528))
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Help, Questions and Contribution

There are several ways how to get help. If the users are already
familiar with the [R statistical language](https://www.r-project.org/)
please, follow either the individual package function documentation or
the corresponding articles/vignettes. These might be also considered as
a suitable source for those having not so deep knowledge about the EPR
spectroscopy. **In case you are new to *R***, there are couple of great
tutorials enabling a quite straightforward diving into
<img src="https://www.r-project.org/Rlogo.png" width="16" height="16"/>.
**Please, refer to e.g.**

- [R for Data Science (2e)](https://r4ds.hadley.nz/)

- [ggplot2: Elegant Graphics for Data Analysis
  (3e)](https://ggplot2-book.org/)

- [Advanced R](https://adv-r.hadley.nz/)

- [POSIT Homepage](https://posit.co/)

- [R Crash
  Course](https://colauttilab.github.io/RCrashCourse/1_fundamentals.html)

- [Statology R Guides](https://www.statology.org/r-guides/)

- [An Introduction to R](https://intro2r.com/)

- [Introverse](https://sjspielman.github.io/introverse/)

- [Why should I use R ?
  (Series)](https://www.jumpingrivers.com/blog/comparing-r-excel-data-wrangling/)

- [R to Python Data
  Wrangling](https://gist.github.com/conormm/fd8b1980c28dd21cfaf6975c86c74d07)

- [Official Contributed Documentation of
  R](https://cran.r-project.org/other-docs.html)

- [Thi Big Book of R](https://www.bigbookofr.com/)

- Video Tutorial (Series) ➨

  - [Introduction to R Programming for Excel
    Users](https://www.youtube.com/watch?v=Ekp2mfxQSzw)

  - [Bioinformatics and R
    Programming](https://www.youtube.com/@LiquidBrain)

  - [R Programming 101](https://www.youtube.com/@RProgramming101)

  - [Equitable Equations](https://www.youtube.com/@EquitableEquations)

  - [R Programming Full Course
    2023](https://www.youtube.com/watch?v=Q5g6lYUn6Q4)

  - [Plotting Anything with
    ggplot2](https://www.youtube.com/watch?v=h29g21z0a68)

Even though the **EPR spectroscopy** is a quite complex field there are
some introductory on-line materials which may help to start with this
special magnetic resonance method and how to gather the direct
structural evidence of unpaired electronic centers within the molecules
➨

- [EasySpin Documentation](https://easyspin.org/easyspin/documentation/)

- [EPR LibreTexts in
  Chemistry](https://chem.libretexts.org/Bookshelves/Physical_and_Theoretical_Chemistry_Textbook_Maps/Electron_Paramagnetic_Resonance_(Jenschke))

- [NMR Spectroscopy of Organic Compounds (Lesson
  10)](https://nmr.group.uochb.cz/en/nmr-organic-compounds)

- [Bruker EPR
  Instruments](https://www.bruker.com/en/products-and-solutions/mr/epr-instruments.html)

- [EPR
  Everywhere](https://researchoutreach.org/articles/electron-paramagnetic-resonance-epr-everywhere/)

- [Basic Concepts of
  EPR](https://epr.ethz.ch/education/basic-concepts-of-epr.html)

Any questions, comments remarks…social contacts (github issues,X,
discord, email) in case someone is interested in the `{eprscope}`
package development … tbc …

## Acknowledgements

I would like to express a deep gratitude to my colleagues from the [NMR
Spectroscopy Group](https://nmr.group.uochb.cz/en) of the [Institute of
Organic Chemistry and Biochemistry](https://www.uochb.cz/en), namely
[Radek Pohl](https://orcid.org/0000-0001-7898-946X)
<img src="https://orcid.org/assets/vectors/orcid.logo.icon.svg" width="16" height="16"/>,
[Ondřej Socha](https://www.uochb.cz/en/directory/510/ondrej-socha) and
[Martin Dračínský](https://orcid.org/0000-0002-4495-0070)
<img src="https://orcid.org/assets/vectors/orcid.logo.icon.svg" width="16" height="16"/>…
TBC … Without the fruitful environment within the NMR Spectroscopy Team
it wouldn’t be possible to develop such a project like this.
