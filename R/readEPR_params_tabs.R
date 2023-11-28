#
#' Read the EPR Instrumental Parameters and Experiment Information
#'
#'
#' @family Data Reading
#'
#'
#' @description
#' Function takes the instrumental parameters (from \code{.DSC/.dsc} or \code{.par} file) applied
#' to record the EPR Spectra and transfers them into list of \code{Tables/Data Frames} incl.
#' parameter values as well as character/string information about the measurement see also
#' \code{\link{readEPR_param_slct}}
#'
#'
#' @param path_to_dsc_par String, path (also provided by \code{\link[base]{file.path}})
#'   to \code{.DSC/.dsc} or \code{.par} (depending on OS, see \code{origin} parameter)
#'   \code{text} files including all instrumental parameters and provided by the EPR machine
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the files are slightly different depending on whether they
#'   were recorded by the windows based softw. ("WinEpr",\code{origin = "winepr"}) or by the Linux
#'   one ("Xenon"), \strong{default}: \code{origin = "xenon"}
#' @param interact Character string, whether or not display tables by \code{\link[DT]{datatable}}.
#'   \strong{Default}: \code{interact = NULL}. To display interactive table with parameters:
#'   \code{interact = "params"} as well as to display that of the additional information:
#'   \code{interact = "info"}.
#'
#'
#' @return List of data frames/tables containing:
#'  \describe{
#'  \item{params}{Instrumental parameters with their numeric values and units.}
#'  \item{info}{Information character string, e.g. like date, operator, comment...etc.}
#'  }
#'  Both data frames tables may be depicted in the form of interactive tables
#'  by \code{interact} function argument.
#'
#'
#' @examples
#' ## built-in package file => "TMPD_specelchem_accu_b.par"
#' tmpd.params.file <- load_data_example(file = "TMPD_specelchem_accu_b.par")
#' ## reading and displaying parameters as data frame
#' tmpd.params.tab <- readEPR_params_tabs(tmpd.params.file,
#'                                        origin = "winepr")
#' ##
#' ## preview
#' utils::head(tmpd.params.tab)
#' ##
#' ## the same like interactive table form
#' readEPR_params_tabs(tmpd.params.file,
#'                     origin = "winepr",
#'                     interact = "params")
#'
#'
#' @export
#'
#'
readEPR_params_tabs <- function(path_to_dsc_par,
                                origin = "xenon",
                                interact = NULL) {
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## condition for checking the temperature because
  ## "STMP"/"TE" (at the beginning of the line) is sometimes missing + basic quantities
  ## character vector
  if (origin == "xenon"){
    temperature.check <- isTRUE(any(grepl("^STMP",readLines(path_to_dsc_par))))
    #
    str.epr.Instr.params.V <- c(
      "MWFQ", "QValue", "A1CT", "A1SW", "B0MA",
      "AVGS", "NbScansDone", "NbScansToDo", "A1RS",
      "MWPW", "SPTP", "RCTC", "RCAG", "STMP", "B0MF", "ConvFact"
    )
  }
  if (origin == "winepr"){
    temperature.check <- isTRUE(any(grepl("^TE",readLines(path_to_dsc_par))))
    #
    str.epr.Instr.params.V <- c(
      "MF", "HCF", "HSW", "RMA", "JSD",
      "RES", "MP", "RCT", "RTC", "TE", "RRG"
    )
  }
  #
  ## required string patterns from 'DSC'/'dsc' or 'par' file:
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
  ## select all corresponding lines (which contain string pattern) from 'DSC'/'dsc' or 'par' file:
  ## reading parameters and the correspond. values at the beginning of the line => +"^"
  str.dsc.sel.V <- sapply(
    str.epr.Instr.params.V,
    function(x) grep(paste0("^",x), readLines(path_to_dsc_par), value = TRUE)
  )
  #
  str.dsc.sel.Ch <- sapply(
    str.epr.Instr.params.Ch,
    function(w) grep(paste0("^",w), readLines(path_to_dsc_par), value = TRUE)
  )
  #
  ## split these strings into string couples (n=2) by space "  " ("\\s+")
  str.dsc.sel.split.V <- lapply(str.dsc.sel.V, function(y) unlist(stringr::str_split(y, "\\s+", n = 2)))
  #
  str.dsc.sel.split.Ch <- lapply(str.dsc.sel.Ch, function(z) unlist(stringr::str_split(z, "\\s+", n = 2)))
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
    if (isTRUE(temperature.check)){
      Value <- c(
        as.numeric(str.dsc.sel.split.V$MWFQ[2]) * 1e-9,
        as.numeric(str.dsc.sel.split.V$QValue[2]),
        as.numeric(str.dsc.sel.split.V$A1CT[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$A1SW[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$B0MA[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$AVGS[2]),
        as.numeric(str.dsc.sel.split.V$NbScansDone[2]),
        as.numeric(str.dsc.sel.split.V$NbScansToDo[2]),
        as.numeric(str.dsc.sel.split.V$A1RS[2]),
        as.numeric(str.dsc.sel.split.V$MWPW[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$SPTP[2]),
        as.numeric(str.dsc.sel.split.V$SPTP[2]) *
          as.numeric(str.dsc.sel.split.V$A1RS[2]),
        as.numeric(str.dsc.sel.split.V$RCTC[2]),
        as.numeric(str.dsc.sel.split.V$RCAG[2]),
        as.numeric(str.dsc.sel.split.V$STMP[2]),
        as.numeric(str.dsc.sel.split.V$B0MF[2]) * 1e-3,
        as.numeric(str.dsc.sel.split.V$ConvFact[2])
      )
    } else{
      Value <- c(
        as.numeric(str.dsc.sel.split.V$MWFQ[2]) * 1e-9,
        as.numeric(str.dsc.sel.split.V$QValue[2]),
        as.numeric(str.dsc.sel.split.V$A1CT[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$A1SW[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$B0MA[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$AVGS[2]),
        as.numeric(str.dsc.sel.split.V$NbScansDone[2]),
        as.numeric(str.dsc.sel.split.V$NbScansToDo[2]),
        as.numeric(str.dsc.sel.split.V$A1RS[2]),
        as.numeric(str.dsc.sel.split.V$MWPW[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$SPTP[2]),
        as.numeric(str.dsc.sel.split.V$SPTP[2]) *
          as.numeric(str.dsc.sel.split.V$A1RS[2]),
        as.numeric(str.dsc.sel.split.V$RCTC[2]),
        as.numeric(str.dsc.sel.split.V$RCAG[2]),
        as.numeric(str.dsc.sel.split.V$B0MF[2]) * 1e-3,
        as.numeric(str.dsc.sel.split.V$ConvFact[2])
      )
    }
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
    if (isTRUE(temperature.check)){
      Value <- c(
        as.numeric(str.dsc.sel.split.V$MF[2]),
        as.numeric(str.dsc.sel.split.V$HCF[2]) * 0.1,
        as.numeric(str.dsc.sel.split.V$HSW[2]) * 0.1,
        as.numeric(str.dsc.sel.split.V$RMA[2]) * 0.1,
        as.numeric(str.dsc.sel.split.V$JSD[2]),
        as.numeric(str.dsc.sel.split.V$RES[2]),
        as.numeric(str.dsc.sel.split.V$MP[2]),
        as.numeric(str.dsc.sel.split.V$RCT[2]) * 1e-3,
        as.numeric(str.dsc.sel.split.V$RCT[2]) * 1e-3 *
          as.numeric(str.dsc.sel.split.V$RES[2]),
        as.numeric(str.dsc.sel.split.V$RCT[2]) * 1e-3 *
          as.numeric(str.dsc.sel.split.V$RES[2]) *
          as.numeric(str.dsc.sel.split.V$JSD[2]),
        as.numeric(str.dsc.sel.split.V$RTC[2]) * 1e-3,
        as.numeric(str.dsc.sel.split.V$TE[2]),
        as.numeric(str.dsc.sel.split.V$RRG[2])
      )
    } else {
      Value <- c(
        as.numeric(str.dsc.sel.split.V$MF[2]),
        as.numeric(str.dsc.sel.split.V$HCF[2]) * 0.1,
        as.numeric(str.dsc.sel.split.V$HSW[2]) * 0.1,
        as.numeric(str.dsc.sel.split.V$RMA[2]) * 0.1,
        as.numeric(str.dsc.sel.split.V$JSD[2]),
        as.numeric(str.dsc.sel.split.V$RES[2]),
        as.numeric(str.dsc.sel.split.V$MP[2]),
        as.numeric(str.dsc.sel.split.V$RCT[2]) * 1e-3,
        as.numeric(str.dsc.sel.split.V$RCT[2]) * 1e-3 *
          as.numeric(str.dsc.sel.split.V$RES[2]),
        as.numeric(str.dsc.sel.split.V$RCT[2]) * 1e-3 *
          as.numeric(str.dsc.sel.split.V$RES[2]) *
          as.numeric(str.dsc.sel.split.V$JSD[2]),
        as.numeric(str.dsc.sel.split.V$RTC[2]) * 1e-3,
        as.numeric(str.dsc.sel.split.V$RRG[2])
      )
    }
    Unit <- c("GHz", "mT", "mT", "mT", "Unitless",
              "Unitless", "mW", "s", "s", "s", "s",
              "K", "Unitless")
    Unit <- Unit %>% `if`(isTRUE(temperature.check),.,Unit[-12])
  }
  #
  ## Create a "parameter" data frame ('[2]' means second string in line / couple):
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
        as.character(str.dsc.sel.split.Ch$OPER[2]),
        as.character(str.dsc.sel.split.Ch$DATE[2]),
        as.character(str.dsc.sel.split.Ch$TIME[2]),
        as.character(str.dsc.sel.split.Ch$CMNT[2]),
        as.character(str.dsc.sel.split.Ch$SAMP[2])
      )
    )
  }
  if (origin == "winepr") {
    data.instrument.Ch <- data.frame(
      ParameterCh <- c("Operator", "Date", "Recording Time", "Comment"),
      Information <- c(
        as.character(str.dsc.sel.split.Ch$JON[2]),
        as.character(str.dsc.sel.split.Ch$JDA[2]),
        as.character(str.dsc.sel.split.Ch$JTM[2]),
        as.character(str.dsc.sel.split.Ch$JCO[2])
      )
    )
  }
  names(data.instrument.V) <- c("Parameter", "Value", "Unit")
  names(data.instrument.Ch) <- c("Parameter", "Information")
  #
  tab.list <- list(params = data.instrument.V, info = data.instrument.Ch)
  #
  ## interactive table by `DT` pkg.
  if (is.null(interact)){
    return(tab.list)
  } else {
    if (interact == "params"){
      params.values <- DT::datatable(tab.list$params)
      return(params.values)
    }
    if (interact == "info"){
      info.character <- DT::datatable(tab.list$info)
      return(info.character)
    }
    #
  }

  #
}
