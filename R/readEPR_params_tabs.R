#
#' Read the EPR Instrumental Parameters and Experiment Information
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Taking the instrumental parameters from \code{.DSC/.dsc} or \code{.par} files, applied
#'   to record the EPR Spectra and transfering them into list of \code{Tables/Data Frames} incl.
#'   parameter values as well as character/string information about the measurement see also
#'   \code{\link{readEPR_param_slct}}.
#'
#'
#' @param path_to_dsc_par Character string, path (also provided by \code{\link[base]{file.path}})
#'   to \code{.DSC/.dsc} or \code{.par} (depending on \code{origin} parameter)
#'   \code{text} files including instrumental parameters and provided by the EPR machine.
#' @param origin Character string, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER/MAGNETTECH spectrometers, because the files are slightly different depending on whether they
#'   were recorded by the "WinEpr",\code{origin = "winepr"} softw. or by the "Xenon"
#'   (\strong{default}: \code{origin = "xenon"}) one or by the "Magnettech" softw. (\code{origin = "magnetech"}).
#' @param interact Character string, whether or not to display interactive tables by \code{\link[DT]{datatable}}.
#'   \strong{Default}: \code{interact = NULL}. Interactive table with parameters can be displayed by
#'   \code{interact = "params"} or to display the additional information table:
#'   \code{interact = "info"}.
#'
#'
#' @return List of data frames/tables containing:
#'  \describe{
#'  \item{params}{Instrumental parameters with their numeric values and units.}
#'  \item{info}{Information character string, e.g. like date, operator, comment...etc.}
#'  }
#'  Both data frames may be depicted in the form of interactive tables
#'  by \code{interact} function argument.
#'
#'
#' @examples
#' ## loading built-in example file =>
#' ## "AcridineDeriv_Irrad_365nm.dsc" by `Magnettech`
#' ## spectrometer software
#' AcridinRad.data.file <-
#'   load_data_example("AcridineDeriv_Irrad_365nm.dsc")
#' ## reading nd displaying parameters as data frame
#' AcridinRad.params.data <-
#'   readEPR_params_tabs(AcridinRad.data.file,
#'                       origin = "magnettech")
#' #
#' ## parameters preview
#' AcridinRad.params.data$params
#' #
#' ## info preview
#' AcridinRad.params.data$info
#' #
#' ## built-in example file => "TMPD_specelchem_accu_b.par"
#' ## by the `WinEPR` spectrometer software
#' tmpd.params.file <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.par")
#' ## reading and displaying parameters as data frame
#' tmpd.params.tab <-
#'   readEPR_params_tabs(tmpd.params.file,
#'                       origin = "winepr")
#' #
#' ## preview
#' tmpd.params.tab$params
#' ##
#' ## the same data frame, now in interactive table form
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
  ## origin strings vectors to define "origin" conditions =>
  winepr.string <- c("winepr","Winepr","WinEpr","WINEPR","WinEPR","winEPR")
  xenon.string <- c("xenon","Xenon","XENON")
  magnettech.string <- c("magnettech","Magnettech","MagnetTech","magnetTech","MAGNETECH")
  #
  ## condition for switching between xenon and magnettech
  xen.magnet.cond <- function(origin){
    if (any(grepl(paste(xenon.string,collapse = "|"),origin))){
      return(0)
    }
    if (any(grepl(paste(magnettech.string,collapse = "|"),origin))){
      return(1)
    }
  }
  #
  ## condition for checking the temperature because
  ## "STMP"/"TE" (at the beginning of the line) is sometimes missing + basic quantities
  ## character vector
  ## XENON AND MAGNETTECH
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))){
    temperature.check <- isTRUE(any(grepl("^STMP",readLines(path_to_dsc_par))))
    #
    ## xenon
    str.epr.Instr.params.V.x <- c(
      "MWFQ", "QValue", "A1CT", "A1SW", "B0MA",
      "AVGS", "NbScansDone", "NbScansToDo", "A1RS",
      "MWPW", "SPTP", "RCTC", "RCAG", "STMP", "B0MF", "ConvFact"
    )
    ## magnetech
    str.epr.Instr.params.V.m <- str.epr.Instr.params.V.x[-c(7,8,12,13,16)]
    ## switch between
    str.epr.Instr.params.V <- switch(2-xen.magnet.cond(origin = origin),
                                     str.epr.Instr.params.V.m,
                                     str.epr.Instr.params.V.x)

  }
  ## WINEPR
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))){
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
  ## XENON AND MAGNETTECH
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))) {
    #
    str.epr.Instr.params.V <- str.epr.Instr.params.V %>%
      `if`(isTRUE(temperature.check),.,
           str.epr.Instr.params.V[!(str.epr.Instr.params.V == "STMP")]) ## corresp. to value
    #
    ## xenon
    str.epr.Instr.params.Ch.x <- c("OPER", "DATE", "TIME", "CMNT", "SAMP") ## corresp. to character
    ## magnetech
    str.epr.Instr.params.Ch.m <- str.epr.Instr.params.Ch.x[-5]
    ## switch between
    str.epr.Instr.params.Ch <- switch(2-xen.magnet.cond(origin = origin),
                                      str.epr.Instr.params.Ch.m,
                                      str.epr.Instr.params.Ch.x)
  }
  ## WINEPR
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
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
  str.dsc.sel.split.V <-
    lapply(str.dsc.sel.V, function(y) unlist(stringr::str_split(y, "\\s+", n = 2)))
  #
  str.dsc.sel.split.Ch <-
    lapply(str.dsc.sel.Ch, function(z) unlist(stringr::str_split(z, "\\s+", n = 2)))
  #
  ## Parameters, Values and Units Definitions:
  ## based upon `temperature.check` (TRUE or FALSE) condition =>
  ## XENON AND MAGNETTECH
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))){
    #
    ## general parameter string for xenon + magnetech => next step select
    ParameterV <- c(
      "Frequency", "QValue", "Central Field", "Sweep Width", "Modulation Amplitude",
      "Num. of Scans", "Num. of Scans Done", "Num. of Scans ToDo", "Number of Points",
      "Power", "Conversion Time", "Sweep Time", "Time Constant", "Receiver Gain",
      "Temperature","Modulation Frequency", "Conversion Factor"
    )
    #
    ## xenon excl. temperature
    if (any(grepl(paste(xenon.string,collapse = "|"),origin))){
      ParameterV <- ParameterV %>% `if`(isTRUE(temperature.check),.,ParameterV[-15])
    }
    ## magnettech excl. temperature
    if (any(grepl(paste(magnettech.string,collapse = "|"),origin))){
      ParameterV <- switch(2-isTRUE(temperature.check),
                           ParameterV[-c(7,8,13,14,17)],
                           ParameterV[-c(7,8,13,14,15,17)])
    }
    #
    ## general parameter values xenon => next step select + temperature
    if (any(grepl(paste(xenon.string,collapse = "|"),origin))){
      #
      Value.x <- c(
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
      #
      Value <- Value.x %>% `if`(isTRUE(temperature.check),
                                append(Value.x,
                                       as.numeric(str.dsc.sel.split.V$STMP[2]),
                                       after = 14),
                                .)
    }
    ## general parameter values magnettech => next step select + temperature
    if (any(grepl(paste(magnettech.string,collapse = "|"),origin))){
      #
      Value.m <- c(
        as.numeric(str.dsc.sel.split.V$MWFQ[2]) * 1e-9,
        as.numeric(str.dsc.sel.split.V$QValue[2]),
        as.numeric(str.dsc.sel.split.V$A1CT[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$A1SW[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$B0MA[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$AVGS[2]),
        as.numeric(str.dsc.sel.split.V$A1RS[2]),
        as.numeric(str.dsc.sel.split.V$MWPW[2]) * 1e+3,
        as.numeric(str.dsc.sel.split.V$SPTP[2]),
        as.numeric(str.dsc.sel.split.V$SPTP[2]) *
          as.numeric(str.dsc.sel.split.V$A1RS[2]),
        as.numeric(str.dsc.sel.split.V$B0MF[2]) * 1e-3
      )
      #
      Value <- Value.m %>% `if`(isTRUE(temperature.check),
                                append(Value.m,
                                       as.numeric(str.dsc.sel.split.V$STMP[2]),
                                       after = 10),
                                .)
    }
    #
    ## Units depending on xenon or magnettech (similarly as `ParameterV` see above)
    Unit <- c(
      "GHz", "Unitless", "mT", "mT", "mT", "Unitless", "Unitless", "Unitless",
      "Unitless", "mW", "s", "s", "s", "dB","K", "KHz", "Unitless"
    )
    ## xenon excl. temperature
    if (any(grepl(paste(xenon.string,collapse = "|"),origin))){
      Unit <- Unit %>% `if`(isTRUE(temperature.check),.,Unit[-15])
    }
    ## magnettech excl. temperature
    if (any(grepl(paste(magnettech.string,collapse = "|"),origin))){
      Unit <- switch(2-isTRUE(temperature.check),
                     Unit[-c(7,8,13,14,17)],
                     Unit[-c(7,8,13,14,15,17)])
    }
    #
  }
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))){
    ParameterV <- c(
      "Frequency", "Central Field", "Sweep Width", "Modulation Amplitude",
      "Number of Scans", "Number of Points", "Power", "Conversion Time",
      "Sweep Time", "Acquire Time", "Time Constant", "Temperature", "Receiver Gain"
    )
    ParameterV <- ParameterV %>% `if`(isTRUE(temperature.check),.,ParameterV[-12])
    #
    ## general definition of value vector => next step select +- temperature
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
    #
    Value <- Value %>% `if`(isTRUE(temperature.check),
                            append(Value,
                                   as.numeric(str.dsc.sel.split.V$TE[2]),
                                   after = 11),
                            .)
    #
    Unit <- c("GHz", "mT", "mT", "mT", "Unitless",
              "Unitless", "mW", "s", "s", "s", "s",
              "K", "Unitless")
    Unit <- Unit %>% `if`(isTRUE(temperature.check),.,Unit[-12])
  }
  #
  ## Create a "parameter" data frame:
  ## data frame from values
  data.instrument.V <- data.frame(
    ParameterV,Value,Unit
  )
  #
  ## data frame from characters Xenon + Magnettech
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))){
    #
    ## general character params. vectors => next step +-  `sample` depending on origin
    ## xenon
    ParameterCh.x <- c("Operator", "Date", "Recording Time", "Comment", "Sample")
    ## magnettech
    ParameterCh.m <- ParameterCh.x[-5]
    ## switch between
    ParameterCh <- switch(2-xen.magnet.cond(origin = origin),
                          ParameterCh.m,
                          ParameterCh.x)
    #
    ## similarly
    Information.m <- c(
      as.character(str.dsc.sel.split.Ch$OPER[2]),
      as.character(str.dsc.sel.split.Ch$DATE[2]),
      as.character(str.dsc.sel.split.Ch$TIME[2]),
      as.character(str.dsc.sel.split.Ch$CMNT[2])
      # as.character(str.dsc.sel.split.Ch$SAMP[2])
    )
    Information <- switch(2-xen.magnet.cond(origin = origin),
                          Information.m,
                          append(Information.m,as.character(str.dsc.sel.split.Ch$SAMP[2]))
                          )
    #
    ## the entire data frame =>
    data.instrument.Ch <- data.frame(ParameterCh,Information)
  }
  ## data frame from characters WinEpr
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
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
