#'
#' Basic Quarto Reproducible Project File/Folder Structure for EPR Reports
#'
#'
#' @description
#'   Creating files and folders for the basic \href{https://quarto.org/}{Quarto} project having
#'   the structure shown in \code{Details} and corresponding to reproducible report with processing and analysis of EPR data.
#'   The main \code{.qmd} ("quarto markdown") file, \code{wd.subdir.name.qmd}, is editable
#'   (so do the additional files as well) and used for rendering. The latter can be done directly within
#'   the \href{https://docs.posit.co/ide/user/}{RStudio IDE} (by activating
#'   the \code{Render} button and selecting the desired output format
#'   like \code{.html},\code{.pdf} or \code{.docx}).
#'   Alternatively, the rendering can be also performed
#'   in the \href{https://quarto.org/docs/computations/r.html}{terminal}
#'   or \href{https://quarto-dev.github.io/quarto-r/}{R console}. The \code{.pdf} format requires
#'   one of the \eqn{\LaTeX} distributions: \href{https://yihui.org/tinytex/}{\code{{tinytex}}} (R package),
#'   \href{https://tug.org/texlive/}{\eqn{\TeX} Live} or \href{https://miktex.org/}{\eqn{Mik\TeX}}.
#'   The complete, above-described, R-environmental setup is also available
#'   at \href{https://posit.cloud/}{Posit Cloud}.
#'
#'
#' @details
#'   In order to support reproducible research workflow (see \code{References}) in EPR from scratch,
#'   a central data hub (repository/directory) with a well-defined structure must be available.
#'   The one, presented below, is created using the essential \code{\link[base]{dir.create}}
#'   and \code{\link[base]{file.create}} file-folder R-management functions. For several files
#'   (like \code{wd.subdir.name.qmd}, \code{header.tex}, \code{title.tex}, \code{styles.scss}
#'   and \code{_quarto.yml}) customized templates (stored under \code{/extdata/_extensions}) are used.
#'   Remaining \code{wd.subdir.name.bib} and \code{README.Rmd} files are generated "ab initio".
#'   The \code{wd.subdir.name} is everywhere replaced by the actual character string defined
#'   by the argument of the same name. Therefore, if we take the default string like "Project_Report",
#'   file/directory names turn into \code{Project_Report/Project_Report.ext} (\code{.ext} \eqn{\equiv}
#'   \code{.qmd}, \code{.bib},...etc). Prior to rendering,
#'   you may provide information about the author like \code{name:}, \code{email:},
#'   \code{orcid:} and affiliations \code{name:} and \code{url:}, directly within the main \code{.qmd} file.
#'   The \code{.bib} file is already pre-populated by one example, actually corresponding to \code{{eprscope}}
#'   package citation. The \code{.bib} reference/citation database/file can be extended and organized
#'   by the online service called \href{https://www.citedrive.com/en/docs/}{CiteDrive}, which can be also applied
#'   as a web clipper for your references/citations,
#'   please refer to the \href{https://www.citedrive.com/en/docs/collect/browser-extension/}{available browser extensions}.
#'
#'   \code{path_to_wd} \cr
#'   | \cr
#'   | \cr
#'   |----- \code{wd.subdir.name} \cr
#'   \verb{    }| \cr
#'   \verb{    }| \cr
#'   \verb{    }|----- \code{wd.subdir.name.qmd}..."dynamic" document, main file for the entire \cr
#'   \verb{    }|\verb{            } data processing and analysis workflow \cr
#'   \verb{    }| \cr
#'   \verb{    }| \cr
#'   \verb{    }|----- \code{header.tex}...file to set up the \code{.qmd} (\code{.tex}) \cr
#'   \verb{    }|\verb{            } ==> \code{.pdf} conversion, usually containing additional \eqn{LaTeX} \cr
#'   \verb{    }|\verb{            } packages and visual setup for the \code{.pdf} output \cr
#'   \verb{    }| \cr
#'   \verb{    }| \cr
#'   \verb{    }|----- \code{title.tex}...file for setting up the title and authors \cr
#'   \verb{    }|\verb{            } in the \code{.pdf} output \cr
#'   \verb{    }| \cr
#'   \verb{    }| \cr
#'   \verb{    }|----- \code{styles.scss}...style sheet to set up visual style \cr
#'   \verb{    }|\verb{            } of the \code{.html} output format \cr
#'   \verb{    }| \cr
#'   \verb{    }| \cr
#'   \verb{    }|----- \code{wd.subdir.name.bib}...bibliographic file database of all \cr
#'   \verb{    }|\verb{            } reference-list entries related to the project report \cr
#'   \verb{    }| \cr
#'   \verb{    }| \cr
#'   \verb{    }|----- \code{README.Rmd}...general documentation for the entire project/repository \cr
#'   \verb{    }| \cr
#'   \verb{    }| \cr
#'   \verb{    }|----- \code{_quarto.yml}...setup for the main \code{wd.subdir.name.qmd} file, \cr
#'   \verb{    }|\verb{            } providing different format outputs (\code{.html},\code{.pdf},\code{.docx}) \cr
#'   \verb{    }| \cr
#'   \verb{    }| \cr
#'   \verb{    }|----- \code{Input_Data} \cr
#'   \verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }|----- \code{EPR_RAW} \cr
#'   \verb{    }|\verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }|\verb{    }|-----...folder dedicated for all raw files from EPR spectrometer, \cr
#'   \verb{    }|\verb{    }|\verb{    }\verb{    }like \code{.dsc/.DSC/.par}, \code{.DTA}, \code{.YGF} \cr
#'   \verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }|----- \code{EPR_ASCII} \cr
#'   \verb{    }|\verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }|\verb{    }|-----...folder dedicated to all additional text files from EPR spectrometer, \cr
#'   \verb{    }|\verb{    }|\verb{    }\verb{    }like \code{.txt}, \code{.csv}, \code{.asc} \cr
#'   \verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }|----- \code{EasySpin_Simulations} \cr
#'   \verb{    }|\verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }|\verb{    }| \cr
#'   \verb{    }|\verb{    }|\verb{    }|-----...folder dedicated for output files from the \code{EasySpin}(-MATLAB), \cr
#'   \verb{    }|\verb{    }|\verb{    }\verb{    }like \code{.mat} or \code{.txt} corresponding to EPR simulated spectral data \cr
#'   \verb{    }| \cr
#'   \verb{    }| \cr
#'   \verb{    }|----- \code{_output} \cr
#'   \verb{    } \verb{    }| \cr
#'   \verb{    } \verb{    }| \cr
#'   \verb{    } \verb{    }|----- \code{Figures} \cr
#'   \verb{    } \verb{    }| \cr
#'   \verb{    } \verb{    }| \cr
#'   \verb{    } \verb{    }|----- \code{Tables} \cr
#'   \verb{    } \verb{    }| \cr
#'   \verb{    } \verb{    }| \cr
#'   \verb{    } \verb{    }|-----...+ \code{.html},\code{.pdf},\code{.docx} formats and supporting files \cr
#'   \verb{    } \verb{    } \verb{    } of the report, these are created by rendering the main \cr
#'   \verb{    } \verb{    } \verb{    } \code{wd.subdir.name.qmd} file (they are not present after \cr
#'   \verb{    } \verb{    } \verb{    } the project is created) \cr
#'
#'   Rendering of the \code{wd.subdir.name.qmd} into different formats (\code{.html},\code{.pdf},
#'   \code{.docx}) is provided by the open-source scientific and technical publishing system
#'   (based on \href{https://pandoc.org/}{pandoc}), called Quarto (Allaire JJ et al. (2024) in the \code{References}).
#'   The main \code{.qmd} file represents a "dynamic" document, combining
#'   \href{https://quarto.org/docs/authoring/markdown-basics.html}{text}, code (besides R, also other
#'   programming languages like \emph{Python}, \emph{Julia} or \emph{Observable} can be used as well) and outputs
#'   (usually, figures and/or tables). Upon rendering, they are nicely combined into shareable above-listed
#'   formats stored under the \code{_output}. Among them, the \code{.html} output possesses
#'   a distinctive position, because it preserves the structure of the interactive EPR spectra or tables
#'   (see e.g. \code{\link{plot_EPR_Specs3D_interact}} or \code{\link{readEPR_params_tabs}}).
#'   File-Folder structure, presented above, is flexible and customizable to meet the user's needs,
#'   right after its creation by the actual function. For such purpose, please consult
#'   the \href{https://quarto.org/docs/guide/}{Quarto documentation} as well. Additionally, if somebody wants
#'   to use templates for ACS/Elsevier/PLOS/Nature-manuscripts, please refer
#'   to the \href{https://quarto.org/docs/extensions/listing-journals.html}{Quarto Jornal Articles Extenstion}
#'   for details of the installation and usage.
#'
#'
#'
#' @references
#'  Alston JM, Rick JA (2021). “A Beginner's Guide to Conducting Reproducible Research”,
#'  \emph{Bull. Ecol. Soc. Am.}, \strong{102}(2), e01801–14, \url{https://doi.org/10.1002/bes2.1801}.
#'
#'  Gandrud C (2020). \emph{Reproducible Research with R and RStudio, 3rd edition},
#'  Chapman and Hall/CRC. ISBN 978-0-429-03185-4, \url{https://doi.org/10.1201/9780429031854}.
#'
#'  National Academies of Sciences, Engineering, and Medicine, Policy and Global Affairs,
#'  Committee on Science, Engineering, Medicine, and Public Policy, Board on Research Data and Information,
#'  Division on Engineering and Physical Sciences, Committee on Applied and Theoretical Statistics,
#'  Board on Mathematical Sciences and Analytics, Division on Earth and Life Studies,
#'  Nuclear and Radiation Studies Board, Division of Behavioral and Social Sciences and Education,
#'  Committee on National Statistics, Board on Behavioral, Cognitive, and Sensory Sciences,
#'  Committee on Reproducibility and Replicability in Science (2019).
#'  “Reproducibility and Replicability in Science: Understanding Reproducibility and Replicability”,
#'  \url{https://www.ncbi.nlm.nih.gov/books/NBK547546/}, National Academies Press (US).
#'
#'  Allaire JJ, Teague C, Scheidegger C, Xie Y, Dervieux C (2024). \emph{Quarto}.
#'  \url{https://doi.org/10.5281/zenodo.5960048}, v1.5, \url{https://github.com/quarto-dev/quarto-cli}.
#'
#'
#'
#' @param title Character string, corresponding to title of the report like the \strong{default} one:
#'   \code{title = "Project Report"}. It appears on the title page in all formats:
#'   \code{.pdf}, \code{.html} and \code{.docx}.
#' @param path_to_wd Character string, setting up the path for \strong{w}orking \strong{d}irectory,
#'   i.e. the parent one, where the project with \code{wd.subdir.name} will be stored (see also \code{Details}).
#'   Alternatively, the \code{\link[base]{file.path}} can be used to set the path.
#'   \strong{Default}: \code{path_to_wd = "."}, referring to actual directory.
#' @param wd.subdir.name Character string, pointing to \code{subdirectory} (name, see also \code{path_to_wd}),
#'   under which the entire report project is stored. \strong{This actually corresponds to main project directory}.
#'   \strong{Default}: \code{wd.subdir.name = "Project_Report"}.
#' @param citation.style Character string, referring to citation style used for \code{References} and citations
#'   in the main \code{.qmd} document, which inherits the name from \code{wd.subdir.name}. This file
#'   is automatically created under the \code{subdirectory}. The argument must be added in the form
#'   of \code{https} url, like \code{citation.style = "https://www.zotero.org/styles/american-chemical-society"}.
#'   All available citation styles can be found
#'   at \href{https://www.zotero.org/styles}{Zotero Citation Style Language Repository}. \strong{Default}:
#'   \code{citation.style = NULL}, actually corresponding
#'   to \href{https://quarto.org/docs/authoring/citations.html}{Chicago Manual of Style (Author-Date)}.
#' @param Rproj.init Logical, whether to initiate the newly created repository/directory as
#'   \href{https://docs.posit.co/ide/user/ide/guide/code/projects.html}{R-project
#'   when working in RStudio}. Therefore, \strong{default}: \code{Rproj.init = TRUE}, which triggers
#'   the creation of \code{.Rproj} file, with the name inherited from \code{wd.subdir.name}. If the RStudio
#'   is not the preferred IDE of your choice, set \code{Rproj.init = FALSE}.
#' @param git.init Logical, if \code{git.init = TRUE}, the whole repository/directory
#'   becomes (initiated by the \code{\link[usethis]{use_git}}) version-controlled,
#'   using the \href{https://git-scm.com/book/en/v2/Getting-Started-What-is-Git}{git system}. PLEASE, FOLLOW
#'   THE R CONSOLE PROMPT and select whether to commit your changes immediately or later.
#'   \strong{Default}: \code{git.init = FALSE}. This is meant to be an option either for novice users
#'   or for those who do not want track changes within the repository by the \code{git}. Instead, they prefer
#'   cloud storage services like
#'   \href{https://nextcloud.com/}{nexcloud}/\href{https://owncloud.com/}{owncloud}/
#'   \href{https://osf.io/}{Open Science Framework}...etc., supporting version (history of changes) control.
#'
#'
#' @return File-folder structure ("tree") for the basic Quarto reproducible report with R, which may be used
#'   for data processing and analysis in Electron Paramagnetic Resonance (EPR).
#'
#'
#' @examples
#' \dontrun{
#' ## creating reproducible report structure
#' ## with the default parameters
#' create_qmdReport_proj()
#' #
#' ## creating report with the specified citation style (ACS)
#' ## and with versioning controlled by the `git` within
#' ## the RStudio (Rproj.init = TRUE)
#' create_qmdReport_proj(
#'   citation.style =
#'     "https://www.zotero.org/styles/american-chemical-society",
#'   git.init = TRUE
#' )
#' }
#'
#'
#' @export
#'
#'
create_qmdReport_proj <- function(title = "Project Report",
                                  path_to_wd = ".",
                                  wd.subdir.name = "Project_Report",
                                  citation.style = NULL, ## Character string (`https://...` =>
                                  ## https://www.zotero.org/styles)
                                  Rproj.init = TRUE,
                                  git.init = FALSE){
  #
  ## create folder structure for input data within the project =>
  setwd(path_to_wd)
  main_dir <- getwd()
  #
  subdir_path <- file.path(main_dir,wd.subdir.name)
  #
  if (isTRUE(Rproj.init)){
    #
    invisible(
      suppressMessages(usethis::create_project(path = subdir_path,open = FALSE,rstudio = TRUE))
    )
    invisible(
      ## at the beginning this is not required:
      file.remove(file.path(main_dir,wd.subdir.name,".gitignore"))
    )
    ## at the beginning this is not required:
    unlink(file.path(main_dir,wd.subdir.name,"R"),recursive = TRUE)
    #
  } else {
    if (!dir.exists(subdir_path)) dir.create(subdir_path,recursive = TRUE)
  }
  #
  input_data_path <- file.path(subdir_path,"Input_Data")
  #
  ## create folders
  #
  if (!dir.exists(input_data_path)) dir.create(input_data_path,recursive = TRUE)
  #
  if (!dir.exists(file.path(input_data_path,"EPR_RAW"))) {
    dir.create(file.path(input_data_path,"EPR_RAW"),recursive = TRUE)
  }
  #
  if (!dir.exists(file.path(input_data_path,"EPR_ASCII"))) {
    dir.create(file.path(input_data_path,"EPR_ASCII"),recursive = TRUE)
  }
  #
  if (!dir.exists(file.path(input_data_path,"EasySpin_Simulations"))) {
    dir.create(file.path(input_data_path,"EasySpin_Simulations"),recursive = TRUE)
  }
  #
  ## create folder structure for output Figs. and Tabs.
  output_data_path <- file.path(subdir_path,"_output")
  #
  if (!dir.exists(output_data_path)) dir.create(output_data_path,recursive = TRUE)
  #
  if (!dir.exists(file.path(output_data_path,"Figures"))) {
    dir.create(file.path(output_data_path,"Figures"),recursive = TRUE)
  }
  #
  if (!dir.exists(file.path(output_data_path,"Tables"))) {
    dir.create(file.path(output_data_path,"Tables"),recursive = TRUE)
  }
  #
  ## create quarto main `.qmd` file according to template =>
  path_to_qmdTemplate <- system.file("extdata",
                                     "_extensions",
                                     "template-qmdRep-proj.qmd",
                                     package = "eprscope")
  qmd.txt <- readLines(path_to_qmdTemplate)
  ## all the quarto template files should be saved in `inst/extdata/_extensions`
  ## see also https://spencerschien.info/post/r_for_nonprofits/quarto_template/
  ## replace title:
  qmd.txt <- gsub(pattern = "MainTitle",replacement = title,x = qmd.txt)
  ## write new file
  qmd.new.file <- file.path(subdir_path,paste0(wd.subdir.name,".qmd"))
  invisible( ## because the `!file.exists()` always returns true
    if(!file.exists(qmd.new.file)) file.create(qmd.new.file)
  )
  writeLines(qmd.txt,con = qmd.new.file)
  #
  ## create quarto `.yml` file according to template,
  ## however, it depends on `header.tex`, `title.tex`
  ## as well as `scss`, therefore, first of all, create all those =>
  path_to_headTemplate <- system.file("extdata",
                                      "_extensions",
                                      "header.tex",
                                      package = "eprscope")
  header.tex.txt <- readLines(path_to_headTemplate)
  header.tex.file <- file.path(subdir_path,"header.tex")
  invisible(
    if (!file.exists(header.tex.file)) file.create(header.tex.file)
  )
  writeLines(header.tex.txt,con = header.tex.file)
  #
  path_to_titleTemplate <- system.file("extdata",
                                       "_extensions",
                                       "title.tex",
                                       package = "eprscope")
  title.tex.txt <- readLines(path_to_titleTemplate)
  title.tex.file <- file.path(subdir_path,"title.tex")
  invisible(
    if (!file.exists(title.tex.file)) file.create(title.tex.file)
  )
  writeLines(title.tex.txt,con = title.tex.file)
  #
  path_to_scssTemplate <- system.file("extdata",
                                      "_extensions",
                                      "styles.scss",
                                      package = "eprscope")
  styles.scss.txt <- readLines(path_to_scssTemplate)
  styles.scss.file <- file.path(subdir_path,"styles.scss")
  invisible(
    if (!file.exists(styles.scss.file)) file.create(styles.scss.file)
  )
  writeLines(styles.scss.txt,con = styles.scss.file)
  #
  ## write bibliography and README supporting files
  bib.content <- paste0(
    "@Manual{eprscope2024,\n",
    "  title = {eprscope R package - Processing and Analysis of Electron \n",
    "Paramagnetic Resonance Data and Spectra in Chemistry},\n",
    "  author = {Jan Tarabek},\n",
    "  year = {2024},\n",
    "  url = {https://jatanrt.github.io/eprscope/} \n",
    "} \n",
    "% to add a bibliography item,\n",
    "% see https://bibtex.eu/types/, \n",
    "% or https://www.citedrive.com/en/quarto/ or https://bibtex.eu/quarto/ \n",
    "% \n",
    " \n"
  )
  #
  bib.file <- file.path(subdir_path,paste0(wd.subdir.name,".bib"))
  invisible(
    if (!file.exists(bib.file)) file.create(bib.file)
  )
  writeLines(bib.content,con = bib.file)
  #
  readme.content <- paste0(
    "---\n",
    "title: ",title," \n",
    "output: github_document \n",
    "---\n\n",
    "<!-- README.md is generated from README.Rmd. Please edit that file --> \n",
    " \n\n",
    "```{r setup, include=FALSE} \n",
    " \n",
    "knitr::opts_chunk$set(echo = TRUE) \n",
    " \n",
    "``` \n",
    " \n\n",
    "## Introduction \n\n",
    "This is an R Markdown document that generates a github README.md file. \n",
    "Please, write your project documentation here...and finally `Knit -> knit github_document` \n",
    " \n"
  )
  #
  rmd.file <- file.path(subdir_path,"README.Rmd")
  invisible(
    if (!file.exists(rmd.file)) file.create(rmd.file)
  )
  writeLines(readme.content,con = rmd.file)
  #
  ## finally create the required `yaml` file
  ## warning is suppressed due to missing value for `csl: ` argument
  path_to_ymlTemplate <- system.file("extdata",
                                     "_extensions",
                                     "template-ymlRep-proj.yml",
                                     package = "eprscope")
  suppressWarnings({yml.txt <- readLines(path_to_ymlTemplate)})
  ## replace title + `bib` in `bibliography`:
  yml.txt <- gsub(pattern = "MainTitle",replacement = title,x = yml.txt)
  yml.txt <- gsub(pattern = "Project_Report.bib",
                  replacement = paste0(wd.subdir.name,".bib"),
                  x = yml.txt)
  ## define citation style based on `citation.style`
  if (is.null(citation.style)){
    ## get rid off `csl`
    yml.txt <- gsub(pattern = "csl:",
                    replacement = " \n",
                    x = yml.txt
    )
  } else {
    ## define the citation
    yml.txt <- gsub(pattern = "csl: ",
                    replacement = paste0("csl: ",citation.style," \n"),
                    x = yml.txt
    )
  }
  #
  ## write new file
  yml.new.file <- file.path(subdir_path,"_quarto.yml")
  invisible(
    if (!file.exists(yml.new.file)) file.create(yml.new.file)
  )
  writeLines(yml.txt,con = yml.new.file)
  #
  ## initial git repo based on `git.init` argument/condition
  if (isTRUE(git.init)){
    setwd(subdir_path)
    usethis::use_git()
  }
  #
  ## remove all template `txt` variables which are not required anymore
  rm(qmd.txt,
     header.tex.txt,
     title.tex.txt,
     styles.scss.txt,
     yml.txt
  )
  #
  ## finally the message with the check mark (v ...heavy check mark `\u2714`) =>
  message(" \u2714 The basic structure for your reproducible EPR project repository was successfully created ! ")
  #
}
