#'
#' Loading Built-in Data Files for Package Examples
#'
#'
#' @description
#' Loads built-in package data files which are required to demonstrate the the package functionality
#' by the examples within documentation.
#'
#'
#' @param file Character string corresponding to file name + extension.
#'
#'
#' @examples
#' ## List of all files within the "extdata" directory =>
#' load_data_example()
#'
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
