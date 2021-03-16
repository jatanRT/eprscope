#
#' @title TODO
#'
#'
#' @description TODO
#'
#'
#' @param path_to_DSC TODO
#' @param string TODO
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
readEPRparam_slct <- function(path_to_DSC,string){
  ## COMMENT path corresponds to file (.DSC) from which the params. are read
  ## string is the selected 'string' pattern e.g. like "QValue" or "MWFQ"
  sel.str.line <- grep(string,readLines(path_to_DSC),value = T)
  ## COMMENT such line is then separated (split) into two ('n = 2') string parts
  ## by 'str_split' comming from 'stringr' pckg.
  sel.str.split <- stringr::str_split(sel.str.line,"[[:space:]]+",n = 2)
  ## COMMENT the result is list, therefore select the second list element ('[[1]][2]')
  if (string == "OPER" || string == "CMNT" || string == "DATE" || string == "TIME" || string == "SAMPLE"){
    param.slct <- as.character(sel.str.split[[1]][2])
  } else{
    param.slct <- as.double(sel.str.split[[1]][2])
  }
  return(param.slct)
}
