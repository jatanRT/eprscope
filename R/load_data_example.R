#'
#' Loading Built-In Data Files for Package Examples
#'
#'
#' @description
#'   Loads built-in package data files which are required either to demonstrate the package functionality
#'   by examples within the documentation, or to run functions involving essential characteristics
#'   of nuclei important for EPR spectroscopy (e.g. \code{\link{eval_nu_ENDOR}} or \code{\link{eval_sim_EPR_iso}},
#'   see also \code{\link{isotopes_ds}}). Details of all data are summarized in \code{vignette("datasets")}.
#'
#'
#' @param file Character string corresponding to file name + extension.
#'
#'
#' @examples
#' ## list of all files within the "extdata" directory =>
#' load_data_example()
#' #
#' ## additionally, you may refer to several function examples
#'
#'
#' @export
#'
#'
load_data_example <- function(file = NULL){
  if (is.null(file)){
    ## displaying all data files in "extdata"
    dir(system.file("extdata",package = "eprscope"))
  } else {
    ## loading the file(s) from "extdata" directory
    ## which is transferred one level up by the installation
    system.file("extdata",file,package = "eprscope",mustWork = TRUE)
  }
  #
}
