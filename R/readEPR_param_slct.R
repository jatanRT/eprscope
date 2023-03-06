#
#' Read the \strong{Selected} EPR Instrumental Parameters and Information
#'
#'
#' @description
#'  Function takes the \strong{selected} instrumental parameters or information
#'  from \code{.DSC} or \code{.par} file of an EPR Spectrum (written by the \code{Xenon}
#'  or \code{WinEpr} Software, respectively)
#'
#'
#' @param path_to_DSC_or_par String, path to \code{.DSC} or \code{.par} file including all instrumental
#'   parameters provided by the EPR machine, path can be provided by \code{\link[base]{file.path}}
#' @param string String, within the \code{.DSC} or \code{.par} (at the line beginning) file
#'   corresponding to instrumental parameter,
#'  following \strong{strings are defined} (in parenthesis for "winepr" software):
#'  \tabular{rl}{
#'   \strong{String}  \tab \strong{Instrumental Parameter} \cr
#'    "OPER" ("JON") \tab  operator (of the EPR instrument) \cr
#'    "CMNT" ("JCO") \tab  comment (in order to describe the measurement) \cr
#'    "DATE" ("JDA") \tab  date (when the EPR spectrum was recorded) \cr
#'    "TIME" ("JTM")  \tab  time (when the EPR spectrum was recorded) \cr
#'    "SAMP" \tab   name/decsript. of the sample \cr
#'    "MWFQ"  ("MF") \tab microwave frequency in \code{Hz} (\code{GHz}) \cr
#'    "QValue" \tab   recorded quality-Factor (required for intensity norm.) \code{unitless} \cr
#'    "A1CT" ("HCF") \tab central field (B) in \code{T} (\code{G}) \cr
#'    "A1SW" ("HSW") \tab   sweep width in \code{T} (\code{G}) \cr
#'    "B0MA" ("RMA") \tab   modulation amplitude in \code{T} (\code{G}) \cr
#'    "AVGS" ("JSD") \tab   number of accumulations for each spectrum \cr
#'    "A1RS" ("RES") \tab   number of points/resolution \cr
#'    "MWPW" ("MP") \tab   microwave power in \code{W} (\code{mW}) \cr
#'    "SPTP" ("RCT") \tab   conversion time in \code{s} (\code{ms}) \cr
#'    "RCTC" ("RTC") \tab   time constant in \code{s} (ms) \cr
#'    "RCAG" ("RRG") \tab  signal receiver gain in \code{dB} (unitless) \cr
#'    "ConvFact" \tab conversion factor/instr. calibration constant for quantitative analysis \code{unitless} \cr
#'  }
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the files are slightly different depending on whether they were recorded
#'   by the windows based softw. ("WinEpr",\code{origin = "winepr"}) or by the Linux one ("Xenon"),
#'   \strong{default}: \code{origin = "xenon"}
#'
#' @return Numeric or String/Character (e.g. date or comment) corresponding to selected (\code{slct}) instrumental
#'   parameter applied to record the EPR spectra
#'
#'
#' @examples
#' \dontrun{
#' ## Reading modulation amplitude from 'Xenon' spectrometer file
#' readEPR_param_slct(path_to_DSC_or_par,
#'                    string = "B0MA")
#'
#' ## Reading Q Value from 'Xenon' spectrometer file
#' read_param_slct(file.path(".",
#'                           "dir_DSC",
#'                           "EPR_spectrum.DSC"),
#'                 string = "QValue")
#'
#' ## Reading `date` from 'WinEPR' spectrometer file
#' read_param_slct(file.path(".",
#'                           "dir_par",
#'                           "EPR_spectrum.par"),
#'                 string = "JDA",
#'                 origin = "winepr")
#' }
#'
#'
#' @export
#'
readEPR_param_slct <- function(path_to_DSC_or_par,string,origin = "xenon"){
  #
  ## path corresponds to file (.DSC) from which the params. are read
  ## string is the selected 'string' pattern e.g. like "QValue" or "MWFQ"
  sel.str.line <- grep(string,readLines(path_to_DSC_or_par),value = T)
  #
  ## such line is then separated (split) into two ('n = 2') string parts
  ## by 'str_split' comming from 'stringr' pckg.
  sel.str.split <- stringr::str_split(sel.str.line,"[[:space:]]+",n = 2)
  #
  ## the result is list, therefore select the second list element ('[[1]][2]'):
  if (origin == "xenon"){
    if (string == "OPER" || string == "CMNT" || string == "DATE" || string == "TIME" || string == "SAMP"){
      param.slct <- as.character(sel.str.split[[1]][2])
    } else{
      param.slct <- as.double(sel.str.split[[1]][2])
    }
  }
  if (origin == "winepr"){
    if (string == "JON" || string == "JCO" || string == "JDA" || string == "JTM"){
      param.slct <- as.character(sel.str.split[[1]][2])
    } else{
      param.slct <- as.double(sel.str.split[[1]][2])
    }
  }
  #
  return(param.slct)
  #
}
