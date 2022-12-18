#
#' Read Parameters/Variables/Fields (including the Simulation ones) from \emph{MATLAB} \code{.mat} File
#'
#'
#' @description
#' tbc
#'
#'
#' @param path_to_MAT String, path to \code{.mat} MATLAB file with all variables saved in workspace
#' @param str.var String, \code{structure/variable}, which may contain \code{fields}, e.g. like \code{Sys}
#'   and \code{g} => \strong{Sys}.g, respectively
#' @param field.var String, \code{field variable after 'dot'}, which is available only for certain
#'   structures/variables, see e.g. example above (Sys.\strong{g}), therefore
#'   the \strong{default} 'value' is \code{NULL} and \code{string} \strong{is applied only for structures with fields}.
#'
#'
#' @return Numeric or String/Character depending on MATLAB variable used for simulation of EPR spectra
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#'
#' @export
#'
#'
readMAT_params_file <- function(path_to_MAT,
                               str.var,
                               field.var = NULL){
  data.params <- R.matlab::readMat(path_to_MAT)
  #
  if (is.null(field.var)){
    params <- data.params[[str.var]]
  } else{
    params <- data.params[[str.var]][, ,1][[field.var]]
  }
  #
  return(params)
 #
}
