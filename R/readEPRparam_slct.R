#
#' @title Read the \strong{selected} Instrumental Parameters from \code{.DSC} to Record
#'   the EPR Spectra (within the \code{Xenon} Software)
#'
#'
#' @description TODO
#'
#'
#' @param path_to_DSC String, path to \code{.DSC} file including all instrumental parameters provided by the EPR machine
#' @param string String, within the \code{.DSC} file corresponding to instrumental parameter,
#'  following \strong{strings are defined}:
#'  \tabular{rl}{
#'   \strong{String}  \tab \strong{Instrumental Parameter} \cr
#'    "OPER" \tab  operator (of the EPR instrument) \cr
#'    "CMNT" \tab  comment (in order to describe the measurement) \cr
#'    "DATE" \tab  date (when the EPR spectrum was recorded) \cr
#'    "TIME"  \tab  time (when the EPR spectrum was recorded) \cr
#'    "SAMPLE" \tab   name/decsript. of the sample \cr
#'    "MWFQ"  \tab microwave frequency in \code{Hz} \cr
#'    "QValue" \tab   recorded quality-Factor (required for intensity norm.) \cr
#'    "A1CT" \tab   central field (B) in \code{T} \cr
#'    "A1SW" \tab   sweep width in \code{T} \cr
#'    "B0MA" \tab   modulation amplitude in \code{T} \cr
#'    "AVGS" \tab   number of accumulations for each spectrum \cr
#'    "A1RS" \tab   number of points/resolution \cr
#'    "MWPW" \tab   microwave power in \code{W} \cr
#'    "SPTP" \tab   conversion time in \code{s} \cr
#'    "RCTC" \tab   time constant in \code{s} \cr
#'  }
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
