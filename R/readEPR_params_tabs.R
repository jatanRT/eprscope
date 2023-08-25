#
#' Read the EPR Instrumental Parameters and Experiment Information
#'
#'
#' @family Data Reading
#'
#'
#' @description
#' Function takes the instrumental parameters (from \code{.DSC} or \code{.par} file) applied
#' to record the EPR Spectra and transfers them into list of \code{Tables/Data Frames} incl.
#' parameter values as well as character/string information about the measurement see also
#' \code{\link{readEPR_param_slct}}
#'
#'
#' @param path_to_DSC_or_par String, path (also provided by \code{\link[base]{file.path}})
#'   to \code{.DSC} or \code{.par} (depending on OS, see \code{origin} parameter)
#'   \code{text} files including all instrumental parameters and provided by the EPR machine
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the files are slightly different depending on whether they were recorded
#'   by the windows based softw. ("WinEpr",\code{origin = "winepr"}) or by the Linux one ("Xenon"),
#'   \strong{default}: \code{origin = "xenon"}
#'
#'
#' @return List of data frames/tables containing instrumental parameters (\code{params}) and information (\code{info}),
#'  i.e. numeric values & strings related to an EPR experiment
#'
#'
#' @examples
#' \dontrun{
#' readEPR_params_tabs(path_to_DSC_or_par)
#' readEPR_params_tabs(file.path(".",
#'                               "dir_par",
#'                               "EPR_spectrum.par"),
#'                     origin = "winepr")
#' }
#'
#'
#' @export
#'
#'
readEPR_params_tabs <- function(path_to_DSC_or_par,
                                origin = "xenon") {
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## condition for checking the temperature because
  ## "STMP"/"TE" (at the beginning of the line) is sometimes missing + basic quantities
  ## character vector
  if (origin == "xenon"){
    temperature.check <- isTRUE(any(grepl("^STMP",readLines(path_to_DSC_or_par))))
    #
    str.epr.Instr.params.V <- c(
      "MWFQ", "QValue", "A1CT", "A1SW", "B0MA",
      "AVGS", "NbScansDone", "NbScansToDo", "A1RS",
      "MWPW", "SPTP", "RCTC", "RCAG", "STMP", "B0MF", "ConvFact"
    )
  }
  if (origin == "winepr"){
    temperature.check <- isTRUE(any(grepl("^TE",readLines(path_to_DSC_or_par))))
    #
    str.epr.Instr.params.V <- c(
      "MF", "HCF", "HSW", "RMA", "JSD",
      "RES", "MP", "RCT", "RTC", "TE", "RRG"
    )
  }
  #
  ## required string patterns from 'DSC' or 'par' file:
  ## depending on `temperature.check` remove "STMP" or "TE" element
  if (origin == "xenon") {
    str.epr.Instr.params.V <- str.epr.Instr.params.V %>%
      `if`(isTRUE(temperature.check),.,
           str.epr.Instr.params.V[!(str.epr.Instr.params.V == "STMP")]) ## corresp. to value
    #
    str.epr.Instr.params.Ch <- c("OPER", "DATE", "TIME", "CMNT", "SAMP") ## corresp. to character
  }
  if (origin == "winepr") {
    str.epr.Instr.params.V <- str.epr.Instr.params.V %>%
      `if`(isTRUE(temperature.check),.,
           str.epr.Instr.params.V[!(str.epr.Instr.params.V == "TE")]) ## corresp. to value
    #
    str.epr.Instr.params.Ch <- c("JON", "JDA", "JTM", "JCO") ## corresp. to character
  }
  #
  ## select all corresponding lines (which contain string pattern) from 'DSC' or 'par' file:
  ## reading parameters and the correspond. values
  str.dsc.sel.V <- sapply(
    str.epr.Instr.params.V,
    function(x) grep(x, readLines(path_to_DSC_or_par), value = TRUE)
  )
  #
  str.dsc.sel.Ch <- sapply(
    str.epr.Instr.params.Ch,
    function(w) grep(w, readLines(path_to_DSC_or_par), value = TRUE)
  )
  #
  ## split these strings into string couples (n=2) by space "  " ("\\s+"):
  str.dsc.sel.split.V <- sapply(str.dsc.sel.V, function(y) stringr::str_split(y, "\\s+", n = 2))
  #
  str.dsc.sel.split.Ch <- sapply(str.dsc.sel.Ch, function(z) stringr::str_split(z, "\\s+", n = 2))
  #
  ## Parameters, Values and Units Definitions:
  ## based upon `temperature.check` (TRUE or FALSE) condition =>
  if (origin == "xenon"){
    ParameterV <- c(
      "Frequency", "QValue", "Central Field", "Sweep Width", "Modulation Amplitude",
      "Num. of Scans", "Num. of Scans Done", "Num. of Scans ToDo", "Number of Points",
      "Power", "Conversion Time", "Sweep Time", "Time Constant", "Receiver Gain",
      "Temperature","Modulation Frequency", "Conversion Factor"
    )
    ParameterV <- ParameterV %>% `if`(isTRUE(temperature.check),.,ParameterV[-15])
    Value <- c(
      as.numeric(str.dsc.sel.split.V$MWFQ[[2]]) * 1e-9,
      as.numeric(str.dsc.sel.split.V$QValue[[2]]),
      as.numeric(str.dsc.sel.split.V$A1CT[[2]]) * 1e+3,
      as.numeric(str.dsc.sel.split.V$A1SW[[2]]) * 1e+3,
      as.numeric(str.dsc.sel.split.V$B0MA[[2]]) * 1e+3,
      as.numeric(str.dsc.sel.split.V$AVGS[[2]]),
      as.numeric(str.dsc.sel.split.V$NbScansDone[[2]]),
      as.numeric(str.dsc.sel.split.V$NbScansToDo[[2]]),
      as.numeric(str.dsc.sel.split.V$A1RS[[2]]),
      as.numeric(str.dsc.sel.split.V$MWPW[[2]]) * 1e+3,
      as.numeric(str.dsc.sel.split.V$SPTP[[2]]),
      as.numeric(str.dsc.sel.split.V$SPTP[[2]]) *
        as.numeric(str.dsc.sel.split.V$A1RS[[2]]),
      as.numeric(str.dsc.sel.split.V$RCTC[[2]]),
      as.numeric(str.dsc.sel.split.V$RCAG[[2]]),
      as.numeric(str.dsc.sel.split.V$STMP[[2]]),
      as.numeric(str.dsc.sel.split.V$B0MF[[2]]) * 1e-3,
      as.numeric(str.dsc.sel.split.V$ConvFact[[2]])
    )
    Value <- Value %>% `if`(isTRUE(temperature.check),.,Value[-15])
    Unit <- c(
      "GHz", "Unitless", "mT", "mT", "mT", "Unitless", "Unitless", "Unitless",
      "Unitless", "mW", "s", "s", "s", "dB","K", "KHz", "Unitless"
    )
    Unit <- Unit %>% `if`(isTRUE(temperature.check),.,Unit[-15])
  }
  if (origin == "winepr"){
    ParameterV <- c(
      "Frequency", "Central Field", "Sweep Width", "Modulation Amplitude",
      "Number of Scans", "Number of Points", "Power", "Conversion Time",
      "Sweep Time", "Acquire Time", "Time Constant", "Temperature", "Receiver Gain"
    )
    ParameterV <- ParameterV %>% `if`(isTRUE(temperature.check),.,ParameterV[-12])
    Value <- c(
      as.numeric(str.dsc.sel.split.V$MF[[2]]),
      as.numeric(str.dsc.sel.split.V$HCF[[2]]) * 0.1,
      as.numeric(str.dsc.sel.split.V$HSW[[2]]) * 0.1,
      as.numeric(str.dsc.sel.split.V$RMA[[2]]) * 0.1,
      as.numeric(str.dsc.sel.split.V$JSD[[2]]),
      as.numeric(str.dsc.sel.split.V$RES[[2]]),
      as.numeric(str.dsc.sel.split.V$MP[[2]]),
      as.numeric(str.dsc.sel.split.V$RCT[[2]]),
      as.numeric(str.dsc.sel.split.V$RCT[[2]]) *
        as.numeric(str.dsc.sel.split.V$RES[[2]]),
      as.numeric(str.dsc.sel.split.V$RCT[[2]]) *
        as.numeric(str.dsc.sel.split.V$RES[[2]]) *
        as.numeric(str.dsc.sel.split.V$JSD[[2]]),
      as.numeric(str.dsc.sel.split.V$RTC[[2]]),
      as.numeric(str.dsc.sel.split.V$TE[[2]]),
      as.numeric(str.dsc.sel.split.V$RRG[[2]])
    )
    Value <- Value %>% `if`(isTRUE(temperature.check),.,Value[-12])
    Unit <- c("GHz", "mT", "mT", "mT", "Unitless",
              "Unitless", "mW", "s", "s", "s", "s",
              "K", "Unitless")
    Unit <- Unit %>% `if`(isTRUE(temperature.check),.,Unit[-12])
  }
  #
  ## Create a "parameter" data frame ('[[2]]' means second string in line / couple):
  ## data frame from values
  data.instrument.V <- data.frame(
    ParameterV,Value,Unit
  )
  #
  ## data frame from characters
  if (origin == "xenon") {
    data.instrument.Ch <- data.frame(
      ParameterCh <- c("Operator", "Date", "Recording Time", "Comment", "Sample"),
      Information <- c(
        as.character(str.dsc.sel.split.Ch$OPER[[2]]),
        as.character(str.dsc.sel.split.Ch$DATE[[2]]),
        as.character(str.dsc.sel.split.Ch$TIME[[2]]),
        as.character(str.dsc.sel.split.Ch$CMNT[[2]]),
        as.character(str.dsc.sel.split.Ch$SAMP[[2]])
      )
    )
  }
  if (origin == "winepr") {
    data.instrument.Ch <- data.frame(
      ParameterCh <- c("Operator", "Date", "Recording Time", "Comment", "Sample"),
      Information <- c(
        as.character(str.dsc.sel.split.Ch$JON[[2]]),
        as.character(str.dsc.sel.split.Ch$JDA[[2]]),
        as.character(str.dsc.sel.split.Ch$JTM[[2]]),
        as.character(str.dsc.sel.split.Ch$JCO[[2]])
      )
    )
  }
  names(data.instrument.V) <- c("Parameter", "Value", "Unit")
  names(data.instrument.Ch) <- c("Parameter", "Information")
  #
  return(list(params = data.instrument.V, info = data.instrument.Ch))
  #
}
