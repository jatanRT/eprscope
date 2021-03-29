#
#' @title Read Parameters/Variables/Fields (including the Simulation ones) from \emph{MATLAB} \code{.mat} File
#'
#'
#' @description TODO
#'
#'
#' @param path_to_MAT String, path to \code{.mat} MATLAB file with all variables saved in workspace
#' @param str.var String, \code{structure/variable}, which may contain \code{fields}, e.g. like \code{\strong{Sys}}
#'   and \code{g} => \code{\strong{Sys}.g}, respectively
#' @param var.field String, \code{field variable after 'dot'}, which is available only for certain
#'   structures/variables, see e.g. example above (\code{Sys.\strong{g}}), therefore
#'   the \strong{default} 'value' is \code{NULL} and \code{string} \strong{is applied only for structures with fields}.
#'
#'
#' @return TODO
#'
#'
#' @examples
#' TODO
#' TODO
#'
#'
#' @export
#'
#'
readMATfile_params <- function(path_to_MAT,
                               str.var,
                               var.field = NULL){
  data.params <- R.matlab::readMat(path_to_MAT)
  #
  if (is.null(var.field)){
    params <- data.params[[str.var]]
  } else{
    params <- data.params[[str.var]][, ,1][[var.field]]
  }
  #
  return(params)
 #
}
