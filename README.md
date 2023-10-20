eprscope
================

## Overview <img src="man/figures/logo_new.png" align="right" height="139"/>

The key objective of this fully open source package is to bring handy
functions corresponding to “everyday” data processing/analysis in EPR
([Electron Paramagnetic
Resonance](https://chem.libretexts.org/Bookshelves/Physical_and_Theoretical_Chemistry_Textbook_Maps/Electron_Paramagnetic_Resonance_(Jenschke)))
spectroscopy mainly in chemistry. The package doesn’t want to replace
the excellent EPR simulation/processing [EasySpin
Toolbox](https://www.easyspin.org/) for
[MATLAB](https://www.mathworks.com/products/matlab.html) . Rather, it
may be considered like a complimentary package/toolbox with practical
functions which have to be otherwise performed by the
acquisition/processing software, supplied by the EPR spectrometer
manufacturers (see
e.g. [*Xenon/WinEPR*](https://www.bruker.com/en/products-and-solutions/mr/epr-instruments/epr-software.html)),
and/or by several proprietary software platforms like *MS Office* and/or
*Orgin/SigmaPlot/Igor* i.e. in general text editing in combination with
a graphing software.

## Installation

``` r
## So far, the package can be installed by =>

# install.packages("devtools")
# devtools::install_github("jatanRT/eprscope.git")
```

Before the installation, please make sure that you have already followed
the instructions for [{nloptr} package
installation](https://astamm.github.io/nloptr/) depending on your
operating system. This package is required for the proper running of
optimization/fitting functions of the `{eprscope}` .

Completely new
<img src="https://www.r-project.org/Rlogo.png" width="16" height="16"/>
users have to consult ➨

1.  [the R installation procedure](https://cran.rstudio.com/)

2.  [installation of the Rstudio
    IDE](https://posit.co/download/rstudio-desktop/)

3.  [the latest R tools
    release](https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html)
    **ONLY for WINDOWS OS**

Additionally, **an open-source scientific and technical publishing
system** [Quarto](https://quarto.org/) together with
[Pandoc](https://pandoc.org/), **a document converter system**, may be
required for sharing of the results coming from `{eprscope}` in desired
formats like `pdf` , `html` , `docx` , `pptx` or `tex`.

## Usage

In this section couple of examples are shown in order to briefly
demonstrate functionality of the package. More detailed description can
be found within the package articles/vignettes (see above).

## Questions, Help and Contribution

There are several ways how to get help. If the users familiar with the
[R statistical language](https://www.r-project.org/) please, consult
either the individual package function documentation or the
corresponding articles/vignettes on this site. These might be also
considered as a suitable source for the students having not so deep
knowledge about the EPR spectroscopy. **In case you are new to *R***,
there are couple of great tutorials enabling a quite straightforward
diving into
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

Even though the **field of EPR spectroscopy** is quite complex there are
some introductory on-line materials which may help to get a picture how
we are gathering the direct structural evidence of unpaired electronic
centers within the molecules ➨

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

Any questions, comments remarks…social contacts (github issueas,X,
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
TBC …
