#'
#' Basic Quarto Reproducible Report Project Structure for EPR
#'
#'
#' @description
#'   A short description...
#'
#'
#' @param title Character string, corresponding to title of the report like the \strong{default} one:
#'   \code{title = "Project Report"}. It appears on the title page in all document formats:
#'   \code{.pdf}, \code{.html} and \code{.docx}.
#' @param path_to_wd Character string, setting up the path for \strong{w}orking \strong{d}irectory,
#'   i.e. the parent one, where the entire project with \code{wd.subdir.name} will be stored.
#'   Alternatively, the \code{\link[base]{file.path}} can be used to set the path.
#'   \strong{Default}: \code{path_to_wd = "."}, referring to actual directory.
#' @param wd.subdir.name Character string, pointing to \code{subdirectory} (name), under which the entire
#'   report project is stored. This actually corresponds to main project directory. \strong{Default}:
#'   \code{wd.subdir.name = "Project_Report"}.
#' @param citation.style Character string
#' @param Rproj.init Logical,
#' @param git.init Logical,
#'
#'
#' @return
#'
#'
#' @examples
#'
#'
#' @importFrom usethis create_project use_git
create_qmdReport_proj <- function(title = "Project Report",
                                  path_to_wd = ".",
                                  wd.subdir.name = "Project_Report",
                                  citation.style = NULL, ## Character string (`https://...` =>
                                  ## https://www.zotero.org/styles)
                                  Rproj.init = FALSE,
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
      usethis::create_project(path = subdir_path,open = FALSE,rstudio = TRUE),
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
    "  author = {Ján Tarábek},\n",
    "  year = {2024},\n",
    "  url = {https://jatanrt.github.io/eprscope/} \n",
    "} \n",
    "% \n"
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
    "This is an R Markdown document that generates a github readme.md file. \n",
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
  ## replace title:
  yml.txt <- gsub(pattern = "MainTitle",replacement = title,x = yml.txt)
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
  ## finally the message with the check mark ('\u2713') =>
  message(" \u2713 The basic structure of your reproducible EPR project repository was successfully created !")
  #
}
