# Basic Quarto Reproducible Project File/Folder Structure for EPR Reports

Creating files and folders for the basic [Quarto](https://quarto.org/)
project having the structure shown in `Details` and corresponding to
reproducible report with processing and analysis of EPR data. The main
`.qmd` ("quarto markdown") file, `wd.subdir.name.qmd`, is editable (so
do the additional files as well) and used for rendering. The latter can
be done directly within the [RStudio
IDE](https://docs.posit.co/ide/user/) (by activating the `Render` button
and selecting the desired output format like `.html`,`.pdf` or `.docx`).
Alternatively, the rendering can be also performed in the
[terminal](https://quarto.org/docs/computations/r.html) or [R
console](https://quarto-dev.github.io/quarto-r/). The `.pdf` format
requires one of the \\L^{A}T\_{E}X\\ distributions:
[`{tinytex}`](https://yihui.org/tinytex/) (R package), [\\T\_{E}X\\
Live](https://tug.org/texlive/) or
[\\MikT\_{E}X\\](https://miktex.org/). Instead of \\L^{A}T\_{E}X\\, the
[Quarto](https://quarto.org/) can be also used in combination with the
[Typst markup language](https://typst.app/) (written in
[Rust](https://rust-lang.org/)). Please, refer to the [Typst-Quarto
documentation](https://quarto.org/docs/output-formats/typst.html). The
complete, above-described R-environmental setup is also available at
[Posit Cloud](https://posit.cloud/) or at
[CoCalc](https://cocalc.com/features), which a is platform for
collaborative calculations and data science.

## Usage

``` r
create_qmdReport_proj(
  title = "Project Report",
  path_to_wd = ".",
  wd.subdir.name = "Project_Report",
  citation.style = NULL,
  Rproj.init = TRUE,
  git.init = FALSE
)
```

## Arguments

- title:

  Character string, corresponding to title of the report like the
  **default** one: `title = "Project Report"`. It appears on the title
  page in all formats: `.pdf`, `.html` and `.docx`.

- path_to_wd:

  Character string, setting up the path for **w**orking **d**irectory,
  i.e. the parent one, where the project with `wd.subdir.name` will be
  stored (see also `Details`). This usually corresponds to a workstation
  directory/repository where all your projects "live". Alternatively,
  the [`file.path`](https://rdrr.io/r/base/file.path.html) can be used
  to set the path. **Default**: `path_to_wd = "."`, referring to actual
  directory.

- wd.subdir.name:

  Character string, pointing to `subdirectory` (name, see also
  `path_to_wd`), under which the entire report project is stored in
  `path_to_wd`. **This actually corresponds to main project directory**.
  **Default**: `wd.subdir.name = "Project_Report"`.

- citation.style:

  Character string, referring to citation style used for `References`
  and citations in the main `.qmd` document, which inherits the name
  from `wd.subdir.name`. This file is automatically created under the
  `subdirectory`. The argument must be added in the form of `https` url,
  like
  `citation.style = "https://www.zotero.org/styles/american-chemical-society"`.
  All available citation styles can be found at [Zotero Citation Style
  Language Repository](https://www.zotero.org/styles). **Default**:
  `citation.style = NULL`, actually corresponding to [Chicago Manual of
  Style
  (Author-Date)](https://quarto.org/docs/authoring/citations.html).

- Rproj.init:

  Logical, whether to initiate the newly created repository/directory as
  [R-project when working in
  RStudio](https://docs.posit.co/ide/user/ide/guide/code/projects.html).
  Therefore, **default**: `Rproj.init = TRUE`, which triggers the
  creation of `.Rproj` file, with the name inherited from
  `wd.subdir.name`. If the RStudio is not the preferred IDE of your
  choice, set `Rproj.init = FALSE`.

- git.init:

  Logical, if `git.init = TRUE`, the whole repository/directory becomes
  (initiated by the
  [`use_git`](https://usethis.r-lib.org/reference/use_git.html))
  version-controlled, using the [git
  system](https://git-scm.com/book/en/v2/Getting-Started-What-is-Git).
  PLEASE, FOLLOW THE R CONSOLE PROMPT and select whether to commit your
  changes immediately or later. **Default**: `git.init = FALSE`. The
  latter is meant to be an option either for novice users or for those
  who do not want track changes within the repository by the `git`.
  Instead, they prefer cloud storage services like
  [Nexcloud](https://nextcloud.com/)/[Owncloud](https://owncloud.com/),
  [Open Science Framework](https://osf.io/), [Google
  Drive](https://www.about.google/drive/)...etc., supporting version
  control (history of changes). For those, who want to track changes by
  the `git` and synchronize them with a remote repository like
  [Github](https://github.com/) or [Gitlab](https://about.gitlab.com/),
  there is a short instruction summary in `Details`.

## Value

File-folder structure ("tree") for the basic [Quarto report with
R](https://quarto.org/docs/guide/), which may be used for the
reproducible data processing and analysis in Electron Paramagnetic
Resonance (EPR) studies.

## Details

In order to support reproducible research workflow (see `References`) in
EPR from scratch, a central data hub (repository/directory) with a
well-defined structure must be available. The one, presented below, is
created using the essential
[`dir.create`](https://rdrr.io/r/base/files2.html) and
[`file.create`](https://rdrr.io/r/base/files.html) file-folder
R-management functions. For several files (like `wd.subdir.name.qmd`,
`header.tex`, `title.tex`, `styles.scss` and `_quarto.yml`) customized
templates (stored under `/extdata/_extensions`) are used. Remaining
`wd.subdir.name.bib` and `README.Rmd` files are generated "ab initio".
The `wd.subdir.name` is everywhere replaced by the actual character
string defined by the argument of the same name. Therefore, if we take
the default string like "Project_Report", file/directory names turn into
`Project_Report/Project_Report.ext` (`.ext` \\\equiv\\ `.qmd`,
`.bib`,...etc). Prior to rendering, you may provide information about
the author like `name:`, `email:`, `orcid:` and affiliations `name:` and
`url:`, directly within the main `.qmd` file. The `.bib` file is already
pre-populated by one example, actually corresponding to `{eprscope}`
package citation. The `.bib` reference/citation database/file can be
extended and organized by the online service called
[CiteDrive](https://www.citedrive.com/en/docs/), which can be also
applied as a web clipper for your references/citations, please refer to
the [available browser
extensions](https://www.citedrive.com/en/docs/collect/browser-extension/).

`path_to_wd`  
\|  
\|  
\|—– `wd.subdir.name`  
` `\|  
` `\|  
` `\|—– `wd.subdir.name.qmd`..."dynamic" document, main file for the
entire  
` `\|` ` data processing and analysis workflow  
` `\|  
` `\|  
` `\|—– `header.tex`...file to set up the `.qmd` (`.tex`)  
` `\|` ` ==\> `.pdf` conversion, usually containing additional
\\L^{A}T\_{E}X\\  
` `\|` ` packages and visual setup for the `.pdf` output  
` `\|  
` `\|  
` `\|—– `title.tex`...file for setting up the title and authors  
` `\|` ` in the `.pdf` output  
` `\|  
` `\|  
` `\|—– `styles.scss`...style sheet to set up visual style  
` `\|` ` of the `.html` output format  
` `\|  
` `\|  
` `\|—– `wd.subdir.name.bib`...bibliographic file database of all  
` `\|` ` reference-list entries related to the project report  
` `\|  
` `\|  
` `\|—– `README.Rmd`...general documentation for the entire
project/repository  
` `\|  
` `\|  
` `\|—– `_quarto.yml`...setup for the main `wd.subdir.name.qmd` file,  
` `\|` ` providing different format outputs (`.html`,`.pdf`,`.docx`)  
` `\|  
` `\|  
` `\|—– `Input_Data`  
` `\|` `\|  
` `\|` `\|  
` `\|` `\|—– `EPR_RAW`  
` `\|` `\|` `\|  
` `\|` `\|` `\|  
` `\|` `\|` `\|—–...folder intended for all raw files from EPR
spectrometer,  
` `\|` `\|` `` `like `.dsc/.DSC/.par`, `.DTA`, `.YGF`  
` `\|` `\|  
` `\|` `\|  
` `\|` `\|—– `EPR_ASCII`  
` `\|` `\|` `\|  
` `\|` `\|` `\|  
` `\|` `\|` `\|—–...folder intended for all additional text files from
EPR spectrometer,  
` `\|` `\|` `` `like `.txt`, `.csv`, `.asc`  
` `\|` `\|  
` `\|` `\|  
` `\|` `\|—– `EasySpin_Simulations`  
` `\|` `\|` `\|  
` `\|` `\|` `\|  
` `\|` `\|` `\|—–...folder intended for output files from the
`EasySpin`(-MATLAB),  
` `\|` `\|` `` `like `.mat` or `.txt` corresponding to EPR simulated
spectral data  
` `\|  
` `\|  
` `\|—– `_output`  
` ` ` `\|  
` ` ` `\|  
` ` ` `\|—– `Figures`  
` ` ` `\|  
` ` ` `\|  
` ` ` `\|—– `Tables`  
` ` ` `\|  
` ` ` `\|  
` ` ` `\|—–...+ `.html`,`.pdf`,`.docx` formats and supporting
files/folders  
` ` ` ` ` ` of the report, these are created by rendering the main  
` ` ` ` ` ` `wd.subdir.name.qmd` file (they are not present after  
` ` ` ` ` ` the project is created)  

Rendering of the `wd.subdir.name.qmd` into different formats
(`.html`,`.pdf`, `.docx`) is provided by the open-source scientific and
technical publishing system (based on [pandoc](https://pandoc.org/)),
called Quarto (Allaire JJ et al. (2024) in the `References`). The main
`.qmd` file represents a "dynamic" document, combining
[text](https://quarto.org/docs/authoring/markdown-basics.html), code
(besides R, also other programming languages like *Python*, *Julia* or
*Observable* can be used as well) and outputs (usually, figures and/or
tables). Upon rendering, they are nicely combined into shareable
above-listed formats stored under the `_output`. Among them, the `.html`
output possesses a distinctive position, because it preserves the
structure of the interactive EPR spectra or tables (see e.g.
[`plot_EPR_Specs3D_interact`](https://jatanrt.github.io/eprscope/reference/plot_EPR_Specs3D_interact.md)
or
[`readEPR_params_tabs`](https://jatanrt.github.io/eprscope/reference/readEPR_params_tabs.md)).
File-Folder structure, presented above, is flexible and customizable to
meet the user's needs, right after its creation by the actual function.
For such purpose, please consult the [Quarto
documentation](https://quarto.org/docs/guide/) as well. Additionally, if
somebody wants to use templates for
ACS/Elsevier/PLOS/Nature-manuscripts, please refer to the [Quarto Jornal
Articles
Extenstion](https://quarto.org/docs/extensions/listing-journals.html)
for details of the installation and usage.

Users, applying a `git` version control of the project/report, may
synchronize their entire directory (as well as the corresponding
changes) with the free remote services like
[Github](https://github.com/) or [Gitlab](https://about.gitlab.com/). If
you've already set up your service account, create either `public` or
`private` repository (WITHOUT `.gitignore`, `README` and `License` !!)
via desired web browser. Afterwards, on your desktop/workstation,
populate the file/folder structure, by the actual function (refer also
to the `Examples`), however DON'T FORGET to put `git.init` argument to
`TRUE`! In the (IDE) `terminal` (NOT IN THE R CONSOLE) set the path to
your project (if it is not already set, check by `pwd`) and execute the
following commands step-by-step:

1.  `git remote add origin https://<service>/<user>/<remote repo name>.git`,
    where the `url` address can be copied by the clicking on the
    `< > Code` button, when viewing the repository via the web browser.

2.  `git branch -M main`, which renames the local `master` branch in
    order to match the remote name, depending on the applied service.
    Please check the remote main/master branch name and adjust
    accordingly.

3.  `git push -u origin main`. Before executing this command, please
    make sure that you've already created a "Personal Access Token"
    (PAT) in your service account via the web browser, because it will
    be required upon the command execution. Refer to the
    [Github](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens)
    or
    [Gitlab](https://docs.gitlab.com/user/profile/personal_access_tokens/)
    documentation.

The strength of the local git - remote Github repositories
synchronization lies in the [seamless
connection/integration](https://docs.github.com/en/repositories/archiving-a-github-repository/referencing-and-citing-content)
of your `Zenodo` and `Github` accounts in order to archive and point to
the entire data (`DOI` is automatically created) for a publication. The
users may refer to the [Github
repository](https://github.com/jatanRT/EPR_Photo_BODIPY_TEMPO_new2025)
example in order to demonstrate such Github-Zenodo connection.

## References

Alston JM, Rick JA (2021). “A Beginner's Guide to Conducting
Reproducible Research”, *Bull. Ecol. Soc. Am.*, **102**(2), e01801–14,
<https://doi.org/10.1002/bes2.1801>.

Gandrud C (2020). *Reproducible Research with R and RStudio, 3rd
edition*, Chapman and Hall/CRC. ISBN 978-0-429-03185-4,
<https://doi.org/10.1201/9780429031854>.

National Academies of Sciences, Engineering, and Medicine, Policy and
Global Affairs, Committee on Science, Engineering, Medicine, and Public
Policy, Board on Research Data and Information, Division on Engineering
and Physical Sciences, Committee on Applied and Theoretical Statistics,
Board on Mathematical Sciences and Analytics, Division on Earth and Life
Studies, Nuclear and Radiation Studies Board, Division of Behavioral and
Social Sciences and Education, Committee on National Statistics, Board
on Behavioral, Cognitive, and Sensory Sciences, Committee on
Reproducibility and Replicability in Science (2019). “Reproducibility
and Replicability in Science: Understanding Reproducibility and
Replicability”, <https://www.ncbi.nlm.nih.gov/books/NBK547546/>,
National Academies Press (US).

Allaire JJ, Teague C, Scheidegger C, Xie Y, Dervieux C (2024). *Quarto*.
<https://doi.org/10.5281/zenodo.5960048>, v1.5,
<https://github.com/quarto-dev/quarto-cli>.

Vanderhaeghe F (2022). "Set up Zenodo - Github Integration. How to
configure Zenodo to publish each new release of a GitHub repository?",
<https://tutorials.inbo.be/tutorials/git_zenodo/>.

## Examples

``` r
if (FALSE) { # \dontrun{
## creating reproducible report structure
## with the default parameters
create_qmdReport_proj()
#
## creating report with the specified citation style (ACS)
## and with versioning, controlled by the `git` within
## the `RStudio` (Rproj.init = TRUE)
create_qmdReport_proj(
  citation.style =
    "https://www.zotero.org/styles/american-chemical-society",
  git.init = TRUE
)
#
## the following command will create "My_Lovely_EPR_Project"
## subdirectory/repository under the "Projects" (dir) entitled
## "EPR Studies on N-Centered Organic Radicals" using
## the `Positron` IDE (i.e. without `Rproj` initialization)
create_qmdReport_proj(
  path_to_wd = "/home/Username/Projects",
  wd.subdir.name = "My_Lovely_EPR_Project",
  title = "EPR Studies on N-Centered Organic Radicals",
  Rproj.init = FALSE
)
#
## afterwards, the `README.md` file can be generated by rendering
## the corresponding `.Rmd`, in the newly populated repo,
## which is automatically created upon executing
## the previous command
} # }

```
