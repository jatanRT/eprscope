#
#' Read the Instrumental Parameters (from \code{.DSC} or \code{.par} file) to Record the EPR Spectra
#'   and Transfer it into \code{Table/Data Frame}
#'
#'
#' @description
#' tbc
#'
#'
#' @param path_to_DSC_or_par String, path to \code{.DSC} or \code{.par} (depending on OS, see \code{origin} parameter)
#'   \code{text} files including all instrumental parameters and provided by the EPR machine
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the files are slightly different depending on whether they were recorded
#'   by the windows based softw. ("WinEpr",\code{origin = "winepr"}) or by the Linux one ("Xenon"),
#'   \strong{default}: \code{origin = "xenon"}
#'
#'
#' @return Data frame/table containing instrumental parameters (numeric values not strings) applied
#'   to acquire the EPR spectra
#'
#'
#' @examples
#' tbc
#' tbc
#'
#'
#' @export
#'
readEPRparams_vals_tab <- function(path_to_DSC_or_par,origin = "xenon"){
  #
  ## required string patterns from 'DSC' or 'par' file:
  if (origin == "xenon"){
    str.epr.Instr.params.V <- c("MWFQ","QValue","A1CT","A1SW","B0MA",
                                "AVGS","A1RS","MWPW","SPTP","RCTC","RCAG","ConvFact")
  }
  if (origin == "winepr"){
    str.epr.Instr.params.V <- c("MF","HCF","HSW","RMA","JSD","RES","MP","RCT","RTC","RRG")
  }
  #
  ## select all corresponding lines (which contain string pattern) from 'DSC' or 'par' file:
  str.dsc.sel.V <- sapply(str.epr.Instr.params.V,
                          function(x) grep(x,readLines(path_to_DSC_or_par),value = T))
  #
  ## split these strings into string couples (n=2) by space "  " ("\\s+"):
  str.dsc.sel.split.V <- sapply(str.dsc.sel.V,function(y) stringr::str_split(y,"\\s+",n = 2))
  #
  ## Create a "parameter" data frame ('[[2]]' means second string in line / couple):
  ## depending on the original parameter list coming either from "xenon" or from "winepr" software
  if (origin == "xenon"){
    data.instrument.V <- data.frame(
      Parameter <- c("Frequency","QValue","Central Field","Sweep Width","Modulation Amplitude",
                   "Number of Scans","Number of Points","Power","Conversion Time","Sweep Time","Acquire Time",
                   "Time Constant","Receiver Gain","Conversion Factor"),
      Value <- c(as.double(as.character(str.dsc.sel.split.V$MWFQ[[2]]))*1e-9,
                 as.double(as.character(str.dsc.sel.split.V$QValue[[2]])),
               as.double(as.character(str.dsc.sel.split.V$A1CT[[2]]))*1e+3,
               as.double(as.character(str.dsc.sel.split.V$A1SW[[2]]))*1e+3,
               as.double(as.character(str.dsc.sel.split.V$B0MA[[2]]))*1e+3,
               as.double(as.character(str.dsc.sel.split.V$AVGS[[2]])),
               as.double(as.character(str.dsc.sel.split.V$A1RS[[2]])),
               as.double(as.character(str.dsc.sel.split.V$MWPW[[2]]))*1e+3,
               as.double(as.character(str.dsc.sel.split.V$SPTP[[2]])),
               as.double(as.character(str.dsc.sel.split.V$SPTP[[2]]))*
                 as.double(as.character(str.dsc.sel.split.V$A1RS[[2]])),
               as.double(as.character(str.dsc.sel.split.V$SPTP[[2]]))*
                 as.double(as.character(str.dsc.sel.split.V$A1RS[[2]]))*as.double(as.character(str.dsc.sel.split.V$AVGS[[2]])),
               as.double(as.character(str.dsc.sel.split.V$RCTC[[2]])),
               as.double(as.character(str.dsc.sel.split.V$RCAG[[2]])),
               as.double(as.character(str.dsc.sel.split.V$ConvFact[[2]]))),
      Unit <- c("GHz","Unitless","mT","mT","mT","Unitless","Unitless","mW","s","s","s","s","dB","Unitless")
    )
  }
  if (origin == "winepr"){
    data.instrument.V <- data.frame(
      Parameter <- c("Frequency","Central Field","Sweep Width","Modulation Amplitude","Number of Scans","Number of Points",
                     "Power","Conversion Time","Sweep Time","Acquire Time","Time Constant","Receiver Gain"),
      Value <- c(as.double(as.character(str.dsc.sel.split.V$MF[[2]])),
                 as.double(as.character(str.dsc.sel.split.V$HCF[[2]]))*0.1,
                 as.double(as.character(str.dsc.sel.split.V$HSW[[2]]))*0.1,
                 as.double(as.character(str.dsc.sel.split.V$RMA[[2]]))*0.1,
                 as.double(as.character(str.dsc.sel.split.V$JSD[[2]])),
                 as.double(as.character(str.dsc.sel.split.V$RES[[2]])),
                 as.double(as.character(str.dsc.sel.split.V$MP[[2]])),
                 as.double(as.character(str.dsc.sel.split.V$RCT[[2]])),
                 as.double(as.character(str.dsc.sel.split.V$RCT[[2]]))*
                   as.double(as.character(str.dsc.sel.split.V$RES[[2]])),
                 as.double(as.character(str.dsc.sel.split.V$RCT[[2]]))*
                   as.double(as.character(str.dsc.sel.split.V$RES[[2]]))*as.double(as.character(str.dsc.sel.split.V$JSD[[2]])),
                 as.double(as.character(str.dsc.sel.split.V$RTC[[2]])),
                 as.double(as.character(str.dsc.sel.split.V$RRG[[2]]))),
      Unit <- c("GHz","mT","mT","mT","Unitless","Unitless","mW","s","s","s","s","Unitless")
    )
  }
  names(data.instrument.V) <- c("Parameter","Value","Unit")
  #
  return(data.instrument.V)
 #
}
