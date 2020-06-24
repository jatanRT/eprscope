#
#' @title
#'
#'
#' @description
#'
#'
#' @param path_to_DSC_or_par
#' @param origin
#'
#'
#' @return
#'
#'
#' @examples
#'
#'
#' @export
#'
instrum_epr_data_vals <- function(path_to_DSC_or_par,origin = "xenon"){
  ## required string patterns from 'DSC' or 'par' file:
  if (origin == "xenon"){
    str.epr.Instr.params.V <- c("MWFQ","QValue","A1CT","A1SW","B0MA","AVGS","A1RS","MWPW","SPTP","RCTC")
  }
  if (origin == "winepr"){
    str.epr.Instr.params.V <- c("MF","HCF","HSW","RMA","JSD","RES","MP","RCT","RTC")
  }
  ## select all corresponding lines (which contain string pattern) from 'DSC' of 'par' file:
  str.dsc.sel.V <- sapply(str.epr.Instr.params.V,function(x) grep(x,readLines(path_to_DSC_or_par),value = T))
  ## split these strings into string couples (n=2) by space "  " ("\\s+"):
  str.dsc.sel.split.V <- sapply(str.dsc.sel.V,function(y) stringr::str_split(y,"\\s+",n = 2))
  ## Create a "parameter" data frame ('[[2]]' means second string in line / couple):
  ## depending on the original parameter list coming from either "xenon" or "winepr" software
  if (origin == "xenon"){
    data.instrument.V <- data.frame(
      Parameter <- c("Frequency","QValue","Central Field","Sweep Width","Modulation Amplitude",
                   "Number of Scans","Number of Points","Power","Conversion Time","Sweep Time","Acquire Time","Time Constant"),
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
               as.double(as.character(str.dsc.sel.split.V$RCTC[[2]]))),
      Unit <- c("GHz","Unitless","mT","mT","mT","Unitless","Unitless","mW","s","s","s","s")
    )
  }
  if (origin == "winepr"){
    data.instrument.V <- data.frame(
      Parameter <- c("Frequency","Central Field","Sweep Width","Modulation Amplitude","Number of Scans","Number of Points",
                     "Power","Conversion Time","Sweep Time","Acquire Time","Time Constant"),
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
                 as.double(as.character(str.dsc.sel.split.V$RTC[[2]]))),
      Unit <- c("GHz","mT","mT","mT","Unitless","Unitless","mW","s","s","s","s")
    )
  }
  names(data.instrument.V) <- c("Parameter","Value","Unit")

  return(data.instrument.V)

}
