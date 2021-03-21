#
#' @title TODO
#'
#'
#' @description TODO
#'
#'
#' @param path_to_MAT TODO
#' @param str.var TODO
#' @param var.field TODO
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
  if (is.null(var.field)){
    params <- data.params[[str.var]]
  } else{
    params <- data.params[[str.var]][, ,1][[var.field]]
  }

  return(params)

}
