# Introduction

The usual way of a spectroscopist workflow consists in data
transformation from the instrument via creation of figures and tables
and finally using them in reports, publications, thesis or
presentations. Quite often, several software packages/tools may be
applied on this path, including the proprietary ones. Among them, an
acquisition/processing software at the instrument enables basic or more
advanced operations with the EPR outcomes/spectra. As already mentioned
(see the [README file /
Homepage](https://jatanrt.github.io/eprscope/index.html)) there is an
excellent open source toolbox for EPR specialists
([1](#ref-Easyspin2025)) only working under proprietary *MATLAB*
([2](#ref-Matlab2025)). It can handle the raw files from the EPR
machines process them and create reports in an interactive form
([3](#ref-MatlabLE2025)) or in `.pdf`. There are, however, only a few
examples of fully open source EPR packages, working under *Python*
![](python-logo-official.png)([4](#ref-FOSSspec2025)).

The open source *R statistical language*
![](RcoreLogo.png)([5](#ref-Rmanual2025)) is not so widely used as the
general purpose language ![](python-logo-official.png), which is also
applied in many scientific fields
([6](#ref-Fangohr2024)–[8](#ref-VaughanPy2023)). Nonetheless, owing to
its nature, the ![](RcoreLogo.png) is very much **focused on
mathematics, statistics, graphing (it was actually built for such a
purpose) and, last but not least, on data processing and analysis
especially in research and academia**. Even though so specialized, it
actually represents an advantage in data handling. Therefore, it has
been also very well established in chemistry and physics
([9](#ref-PhysChemR2025)), biology ([10](#ref-BiocondR2025)), pharma
industry ([11](#ref-PositPharma2025), [12](#ref-Pharmaverse2025)) as
well as in finance area ([13](#ref-Finance2025)). Unsurprisingly, many
big companies, universities or financial and healthcare institutions
rely on a very robust and multiplatform[¹](#fn1)
![](RcoreLogo.png)([14](#ref-Rconsortium2025), [15](#ref-PositDS2025)).

Because the ![](RcoreLogo.png) belongs to the **family of programming
languages,** it also **provides reproducible workflow functionality.
This is especially important for process validation in research and
academia**. Namely, one can easily follow all the data and visualization
operations, unlike within the other software with “icon-based”
procedures. Moreover, **such a data wrangling and the related analytics,
together with the outstanding visualization/graphing**
([16](#ref-ggplotWickh2016)–[19](#ref-plotly2025)) **and publishing
capabilities**
([20](#ref-BookdownBook2016)–[24](#ref-RmarkdownMan2025)), **makes it a
very powerful tool in the research area**. Hence, the decision to create
a package for EPR spectroscopy in ![](RcoreLogo.png) is obvious and
strongly supported by the aforementioned features, even though the
![](RcoreLogo.png) is not so widely used in comparison to
![](python-logo-official.png) .

Moreover, **the entire** ![](RcoreLogo.png)**ecosystem with thousands of
packages** ([5](#ref-Rmanual2025), [10](#ref-BiocondR2025),
[25](#ref-Runiverse2025)) **and *RStudio* IDE**[²](#fn2)
([26](#ref-Rstudio2025)) **support workflows which are almost free from
any other additional software/toolboxes** (see also explanation below).
Therefore, **together with perfectly and uniformly structured**
documentation of R packages (which is already available within the
*RStudio*) **it represents an excellent facilitation of data processing
workflows by extensive reduction of many steps which have to be
otherwise performed by several programs,** as already mentioned above.
Great spectroscopy packages 📦 have been developed in ![](RcoreLogo.png)
e.g. for IR, MS, NMR, Fluorescence, UV-Vis(-NIR), Raman. However, non of
them, even the general one like [{ChemoSpec}](#id_0) or
[{hyperSpec}](#id_0), are suitable for EPR, which actually possesses a
special position among the spectroscopic techniques due to the
paramagnetic (unpaired electronic state) nature of the studied molecules
([27](#ref-epr2025)).

Unlike the *EasySpin* ([1](#ref-Easyspin2025)), which is more
concentrated on the simulation of EPR spectra, **the primary aim of
the** [eprscope](https://jatanrt.github.io/eprscope/) **package is to
process, analyze and visualize the EPR spectral data similarly to
functionality of the acquisition/processing software available at
spectrometers**. This is otherwise not available in any other software
packages/toolboxes. Especially, the function performing an absolute
quantitative EPR analysis is just a rarely present within the
acquisition programs of the EPR instruments. Subsequently, **the
quantitative analysis is tightly connected with the determination of
kinetic rate constants,** \\k\\ **where the radicals (paramagnetic
species) are involved in the studied chemical reactions**. Last but not
least, the \\k\\ temperature dependence can be applied to determine the
activation parameters (\\\Delta^{\ddagger} G^o\\, \\\Delta^{\ddagger}
H^o\\ and \\\Delta^{\ddagger} S^o\\ ) of the elementary radical
reactions. Finally, **the EPR spectroscopy is quite often coupled with
the *in situ* (directly within the EPR cavity/probehead) radical
formation techniques like electrochemistry (usually voltammetry or
potentiostatic/galvanostatic electrolysis) and irradiation or
UV-Vis(-NIR) spectroscopy** ([28](#ref-dunsch2011)). Therefore, **the
presented open source EPR** ![](RcoreLogo.png) 📦 **will address not
only the basic processing, simulation and visualization of the EPR
spectra but also quantitative description of the radical reaction
kinetics as well as that of the electrochemical redox ones**. For the
latter, this actually means that one could easily compare the number of
transferred electrons from the voltammogram with the number of radical
cations/anions determined by the quantitative EPR. Such information
reveals if one-electron transfer is associated with the formation of a
single radical, otherwise a more complex mechanism must be taken into
account ([28](#ref-dunsch2011)) (see also the
[`eval_ECh_QNe_chronoamp()`](https://jatanrt.github.io/eprscope/reference/eval_ECh_QNe_chronoamp.md)
function).

![Schematic representation of the data workflow in EPR spectroscopy and
the compatibility of the newly developed \`{eprscope}\` software package
within the R ecosystem.](introduction_b.png)

Figure 1: Schematic representation of the data workflow in EPR
spectroscopy and the compatibility of the newly developed
[eprscope](https://jatanrt.github.io/eprscope/) software package within
the R ecosystem.

As mentioned above, the data analysis workflow also covers sharing of
the results/outputs. However, quite often, in addition to essential
processing of EPR spectra, the spectral analysis (which usually
corresponds to determination of the hyperfine coupling/splitting
constants and the *g*-factors) requires quantum chemical calculations on
the high-performance computing servers and/or the above-mentioned
simulations of EPR spectra done within the *MATLAB* by
*EasySpin*[³](#fn3)*.* Therefore, one must combine diverse outputs in
order to gather the entire structural information about the paramagnetic
center. As can be seen in Figure [1](#fig:scheme-intro) **the**
[eprscope](https://jatanrt.github.io/eprscope/) **together with the**
*Rmarkdown* ([23](#ref-RmarkdownBook2021)) **and/or the** *Quarto*
**publishing system** ([22](#ref-Quarto2025)) **are capable** (**without
living the** ![](RcoreLogo.png)**ecosystem**) **to process all the input
data into suitable dissemination form** (e.g. report, presentation,
manuscript, webpage…etc.) and thus very effectively completing the last
stage of the reproducible research workflow (please, refer to the
[`create_qmdReport_proj()`](https://jatanrt.github.io/eprscope/reference/create_qmdReport_proj.md)
function).

## References

1\.

STOLLLAB. *EasySpin*. 2025. <https://github.com/StollLab/EasySpin>.

2\.

MATHWORKS. *MATLAB: Math, Graphics, Programming.* 2025.
<https://www.mathworks.com/products/matlab.html?s_tid=hp_products_matlab>.

3\.

MATHWORKS. *MATLAB live editor.* 2025.
<https://www.mathworks.com/products/matlab/live-editor.html>.

4\.

HANSON, Bryan. *Free and open source software (FOSS) for spectroscopy.*
2025. <https://bryanhanson.github.io/FOSS4Spectroscopy/>.

5\.

R CORE TEAM. *R: A language and environment for statistical computing.*
\[online\]. Vienna, Austria : R Foundation for Statistical Computing,
2025. Available from: <https://www.R-project.org>

6\.

FANGOHR, Hans. *Introduction to python for computational science and
engineering.* 2024.
<https://github.com/fangohr/introduction-to-python-for-computational-science-and-engineering>.

7\.

STAVEREN, Marie van. Integrating python into a physical chemistry lab.
*J. Chem. Ed.* 2022. Vol. 99, no. 7, p. 2604–2609.
DOI [10.1021/acs.jchemed.2c00193](https://doi.org/10.1021/acs.jchemed.2c00193).

8\.

VAUGHAN, Lee. *Python tools for scientists: An introduction to using
anaconda, JupyterLab, and python’s scientific libraries.* San
Francisco : No Starch Press, 2023. ISBN 978-1-71850-266-6.

9\.

MULLEN, Katharine. *Chemometrics and computational physics.* 2025.
<https://cran.r-project.org/web/views/ChemPhys.html>.

10\.

BIOCUNDUCTOR. *Bioconductor open source software for bioinformatics.*
2025. <https://www.bioconductor.org/>.

11\.

POSIT PBC, THE OPEN SOURCE DATA SCIENCE CO. *The future of pharma is
open source.* 2025. <https://posit.co/solutions/pharma/>.

12\.

PHARMAVRSE. *A connected network of companies and individuals working to
promote collaborative development of curated open source r packages for
clinical reporting usage in pharma.* 2025. <https://pharmaverse.org/>.

13\.

EDDELBUETTEL, Dirk. *Empirical Finance*. 2025.
<https://cran.r-project.org/web/views/Finance.html>.

14\.

R CONSORTIUM. *R Consortium, A Linux Foundation Project.* 2025.
<https://www.r-consortium.org/>.

15\.

POSIT PBC, THE OPEN SOURCE DATA SCIENCE CO. *Celebrating Data Science
Success.* 2025. <https://posit.co/about/customer-stories/>.

16\.

WICKHAM, Hadley. *ggplot2: Elegant graphics for data analysis.*
\[online\]. Springer-Verlag New York, 2016. ISBN 978-3-319-24277-4.
Available from: <https://ggplot2.tidyverse.org>

17\.

EMAASIT, Daniel. *ggplot2 Extensions*. 2016.
<https://exts.ggplot2.tidyverse.org/>.

18\.

WICKHAM, Hadley, NAVARRO, Danielle and PEDERSEN, Thomas Lin. *ggplot2:
Elegant Graphics for Data Analysis (3e).* 2023.
<https://ggplot2-book.org/>.

19\.

PLOTLY TEAM. *Plotly R Open Source Graphing Library*. 2025.
<https://plotly.com/r/>.

20\.

XIE, Yihui. *Bookdown: Authoring books and technical documents with R
markdown.* \[online\]. Boca Raton, Florida : Chapman; Hall/CRC, 2016.
ISBN 978-1138700109. Available from:
<https://bookdown.org/yihui/bookdown>

21\.

*Bookdown: Authoring books and technical documents with r markdown.*
\[online\]. 2025. Available from: <https://github.com/rstudio/bookdown>

R package version 0.43.3

22\.

ALLAIRE, J. J., TEAGUE, Charles, SCHEIDEGGER, Carlos, XIE, Yihui,
DERVIEUX, Christophe and WOODHULL, Gordon. *Quarto* \[online\]. 1.7.
April 2025. Available from: <https://github.com/quarto-dev/quarto-cli>

23\.

XIE, Yihui, DERVIEUX, Christophe and RIEDERER, Emily. *R markdown
cookbook* \[online\]. Boca Raton, Florida : Chapman; Hall/CRC, 2021.
ISBN 9780367563837. Available from:
<https://bookdown.org/yihui/rmarkdown-cookbook>

24\.

*Rmarkdown: Dynamic documents for r.* \[online\]. 2025. Available from:
<https://github.com/rstudio/rmarkdown>

R package version 2.29.1

25\.

OOMS, Jeroen. *R-Universe*. 2025. <https://r-universe.dev/search/>.

26\.

*RStudio an integrated development environment (IDE) for the r
programming language.* \[online\]. 2025. Available from:
<https://github.com/rstudio/rstudio>

27\.

JESCHKE, Gunnar. *Electron Paramagnetic Resonance (Jeschke)*. 2025.
<https://chem.libretexts.org/@go/page/370916>; ETH Zürich.

28\.

DUNSCH, Lothar. Recent Advances in in situ
multi-spectroelectrochemistry. *J. Solid State Electrochem.* July 2011.
Vol. 15, no. 7, p. 1631–1646.
DOI [10.1007/s10008-011-1453-1](https://doi.org/10.1007/s10008-011-1453-1).

------------------------------------------------------------------------

1.  The ![](RcoreLogo.png) is available for all three main operating
    systems ➨ `windows`, `macos`as well as `linux`

2.  Integrated Development Environment

3.  A simple simulation of isotropic EPR spectra of paramagnetic
    centers/radicals is also included in the
    [eprscope](https://jatanrt.github.io/eprscope/) ![](RcoreLogo.png)
    📦 .
