#
#' Read the EPR Instrumental Parameters and Information for Tabular Outputs
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Extraction of the instrumental parameters from the \code{.DSC/.dsc} or \code{.par} files, applied
#'   to record the EPR Spectra, and transferring them into list of \code{Tables/Data Frames}.
#'   They include either parameter values and their units or character/string information about the measurement,
#'   see also the \code{\link{readEPR_param_slct}} function.
#'
#'
#' @param path_to_dsc_par Character string, path (also provided by \code{\link[base]{file.path}})
#'   to \code{.DSC/.dsc} or \code{.par} (either related to \code{origin = "xenon"}/\code{origin = "magnettech"}
#'   or to \code{origin = "winepr"}, respectively) \code{text} files, including instrumental parameters of the recorded
#'   spectra and provided by the EPR machine.
#' @param origin Character string, corresponding to software used to acquire EPR spectra.
#'   The files are slightly different depending on whether
#'   they were recorded by the "WinEpr",\code{origin = "winepr"}, "Xenon"
#'   (\strong{default}: \code{origin = "xenon"}) or by the "Magnettech" (ESR5000 [11-0422], \code{origin = "magnettech"}).
#' @param interact Character string, whether to display interactive tables by the \code{\link[DT]{datatable}}.
#'   \strong{Default}: \code{interact = NULL}. Interactive table with parameters can be displayed by
#'   \code{interact = "params"} or to display the additional information table:
#'   \code{interact = "info"}.
#'
#'
#' @return List of data frames/tables containing:
#'  \describe{
#'  \item{params}{Instrumental parameters with their numeric values and units.}
#'  \item{info}{Information character string, such as date, operator, comment...etc.}
#'  }
#'  Both data frames may be depicted in the form of interactive tables
#'  by the \code{interact} function argument.
#'
#'
#' @examples
#' ## loading built-in example file =>
#' ## "AcridineDeriv_Irrad_365nm.dsc" by `Magnettech`
#' ## spectrometer software
#' AcridinRad.data.file <-
#'   load_data_example("AcridineDeriv_Irrad_365nm.dsc")
#' ## reading and displaying parameters as data frame
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
  winepr.string <- c(
    "winepr","Winepr","WinEpr","WINEPR","WinEPR","winEPR",
    "SPC/PAR","spc/par","Spc/Par"
  )
  xenon.string <- c("xenon","Xenon","XENON")
  magnettech.string <- c(
    "magnettech","Magnettech","MagnetTech",
    "magnetTech","MAGNETTECH","magnetech",
    "Magnetech","MAGNETECH"
  )
  ## previous strings also with single "t"/"T" excepting mistakes :-)
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
  ## "STMP"/"TE" (at the beginning of the line) is sometimes
  ## missing + basic quantities character vector
  #
  ## ------------------ XENON AND MAGNETTECH ----------------------------
  #
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))){
    #
    # conditions to check the presence of key parameters
    temperature.check <-
      isTRUE(any(grepl("^STMP",readLines(path_to_dsc_par))))
    qValue.check <-
      isTRUE(any(grepl("^QValue",readLines(path_to_dsc_par))))
    y2ndVar.check <-
      isTRUE(any(grepl("^(YUNI|YPTS|YNAM|YWID|A2RS)",readLines(path_to_dsc_par))))
    #
    ## definition of character/string parameter vector `xenon`
    str.epr.Instr.params.Ch.x <-
      c(
        "OPER", "DATE", "TIME", "CMNT",
        "SAMP", # NOT in MAGNETTECH `.DSC`
        "TITL", "XUNI","YNAM","IRNAM",
        "YUNI"
      ) ## corresp. to character
    ## magnettech
    str.epr.Instr.params.Ch.m <- str.epr.Instr.params.Ch.x[-5]
    ## switch between
    str.epr.Instr.params.Ch <-
      switch(2-xen.magnet.cond(origin = origin),
             str.epr.Instr.params.Ch.m,
             str.epr.Instr.params.Ch.x)
    #
    ## xenon parameter values
    str.epr.Instr.params.V.x <- c(
      "XMIN","XWID",
      "MWFQ","QValue",
      "A1SW","B0MA","AVGS",
      "NbScansDone", # NOT in MAGNETTECH `.DSC`
      "NbScansToDo", # NOT in MAGNETTECH `.DSC`
      "XPTS","MWPW", "SPTP",
      "RCTC", # NOT in MAGNETTECH `.DSC`
      "RCAG", # NOT in MAGNETTECH `.DSC`
      "STMP", "B0MF",
      "ConvFact", # NOT in MAGNETTECH `.DSC`
      "YPTS", "YWID","YMIN","A2RS"
    )
    ## magnettech
    str.epr.Instr.params.V.m <- str.epr.Instr.params.V.x[-c(8,9,13,14,17)]
    ## switch between
    str.epr.Instr.params.V <- switch(2-xen.magnet.cond(origin = origin),
                                     str.epr.Instr.params.V.m,
                                     str.epr.Instr.params.V.x)
    #
  }
  ## ------------------------- WINEPR ------------------------------
  #
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))){
    #
    # conditions to check the presence of key parameters
    temperature.check <-
      isTRUE(any(grepl("^TE",readLines(path_to_dsc_par))))
    y2ndVar.check <-
      isTRUE(any(grepl("^(SSY|REY|JEY)",readLines(path_to_dsc_par))))
    #
    ## definition of character/string parameter vector `xenon`
    str.epr.Instr.params.Ch <-
      c("JON", "JDA", "JTM", "JCO",
        "JEX","JUN","XXUN","JEY") ## corresp. to character
    #
    ## winepr parameter values
    str.epr.Instr.params.V <- c(
      "MF", "HCF", "HSW", "RMA", "JSD","XXWI",
      "RES", "MP", "RCT", "RTC", "TE", "XXLB",
      "RRG","REY","XYLB","XYWI","GST","GSI"
    )
  }
  # -------------------- ALL STRINGS - PROCESSING --------------------
  #
  ## select all corresponding lines (which contain string pattern)
  ##from 'DSC'/'dsc' or 'par' file:
  ## reading parameters and the correspond. values
  ## at the beginning of the line => +"^"
  str.dsc.sel.V <- sapply(
    str.epr.Instr.params.V,
    function(x) {grep(
      paste0("^",x),
      readLines(path_to_dsc_par),
      value = TRUE
    )}
  )
  #
  str.dsc.sel.Ch <- sapply(
    str.epr.Instr.params.Ch,
    function(w) {grep(
      paste0("^",w),
      readLines(path_to_dsc_par),
      value = TRUE
    )}
  )
  #
  ## split these strings into string couples (n=2) by space "  " ("\\s+")
  str.dsc.sel.split.V <-
    lapply(
      str.dsc.sel.V,
      function(y) {
        unlist(stringr::str_split(y, "\\s+", n = 2))
      }
    )
  #
  str.dsc.sel.split.Ch <-
    lapply(
      str.dsc.sel.Ch,
      function(z) {
        unlist(stringr::str_split(z, "\\s+", n = 2))
      }
    )
  #
  ## Parameters, Values and Units Definitions:
  ## based upon `temperature.check` (TRUE or FALSE) condition =>
  #
  ## ------------------ XENON AND MAGNETTECH ------------------------
  #
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))){
    #
    ## general character params. vectors =>
    ## next step +-  `sample` depending on origin xenon
    ParameterCh <-
      c(
        "Operator", "Date", "Recording Time", "Comment",
        "Experiment Title", "X Var. (e.g. B/RF) Unit",
        "Sample", # NOT in MAGNETTECH `.DSC`
        "Y 2nd Var. Series","Y 2nd Var. Series Unit"
      )
    ## magnettech
    # ParameterCh.m <- ParameterCh.x[-7]
    # ## switch between
    # ParameterCh <- switch(2-xen.magnet.cond(origin = origin),
    #                       ParameterCh.m,
    #                       ParameterCh.x)
    #
    ## general parameter Information Xenon
    Information <- c(
      # 1. OPERATOR:
      as.character(str.dsc.sel.split.Ch$OPER[2]),
      # 2. DATE:
      as.character(str.dsc.sel.split.Ch$DATE[2]),
      # 3. RECORDING TIME:
      as.character(str.dsc.sel.split.Ch$TIME[2]),
      # 4. COMMENT:
      as.character(str.dsc.sel.split.Ch$CMNT[2]),
      # 5. EXPERIMENT TITLE (why `gsub` see below `Unit.x`):
      gsub("'","",as.character(str.dsc.sel.split.Ch$TITL[2])),
      # 6. X VARIABLE UNIT:
      gsub("'","",as.character(str.dsc.sel.split.Ch$XUNI[2])),
      # 7. SAMPLE:
      switch(
        2 - xen.magnet.cond(origin = origin),
        NA,
        as.character(str.dsc.sel.split.Ch$SAMP[2])
      ),
      # 8. Y 2nd VARIABLE SERIES:
      switch(
        2 - y2ndVar.check,
        gsub("'","",as.character(str.dsc.sel.split.Ch$YNAM[2])),
        NA
      ),
      # 9. Y 2nd VARIABLE SERIES UNIT:
      switch(
        2 - y2ndVar.check,
        gsub("'","",as.character(str.dsc.sel.split.Ch$YUNI[2])),
        NA
      )
    )
    #
    # ## magnettech
    # Information.m <- Information.x[-7]
    # ## switch between
    # Information <- switch(2-xen.magnet.cond(origin = origin),
    #                       Information.m,
    #                       Information.x)
    #
    ## general parameter  value string for xenon + magnettech => next step select
    ParameterV <- c(
      "Frequency", "QValue", "Central Field", "Sweep Width",
      "Modulation Amplitude", "Num. of Scans",
      "Num. of Scans Done","Num. of Scans ToDo", "Number of Points",
      "Power", "Conversion Time","Sweep Time", "Time Constant",
      "Receiver Gain","Temperature","Modulation Frequency",
      "Conversion Factor", "Y 2nd Var. Series Resolution",
      "Y 2nd Var. Series Start",
      "Y 2nd Var. Series End"
    )
    ## magnetech
    # ParameterV.m <- ParameterV.x[-c(7,8,13,14,17)]
    # ParameterV <- switch(2-xen.magnet.cond(origin = origin),
    #                       ParameterV.m,
    #                       ParameterV.x)
    #
    ## general parameter values Xenon
    Value <- c(
      # 1. FREQUENCY in GHz:
      as.numeric(str.dsc.sel.split.V$MWFQ[2]) * 1e-9,
      # 2. Q VALUE UNITLESS:
      switch(
        2 - qValue.check,
        as.numeric(str.dsc.sel.split.V$QValue[2]),
        NA
      ),
      # 3. ACTUAL CENTRAL FIELD (B) (ALSO IF TESLAMETER ON):
      # 3. UNITS -> see `Information[6]`
      (as.numeric(str.dsc.sel.split.V$XMIN[2]) +
         (as.numeric(str.dsc.sel.split.V$XWID[2]) / 2)),
      # 4. ACTUAL SWEEP WIDTH (B) (ALSO IF TESLAMETER ON):
      # 4. UNITS -> see `Information[6]`
      as.numeric(str.dsc.sel.split.V$XWID[2]),
      # 5. MODULATION AMPLITUDE mT:
      as.numeric(str.dsc.sel.split.V$B0MA[2]) * 1e+3,
      # 6. NUMBER OF SCANS UNITLESS:
      as.numeric(str.dsc.sel.split.V$AVGS[2]),
      # 7. NUMBER OF SCANS DONE UNITLESS:
      switch(
        2 - xen.magnet.cond(origin = origin),
        NA,
        as.numeric(str.dsc.sel.split.V$NbScansDone[2])
      ),
      # 8. NUMBER OF SCANS TODO UNITLESS:
      switch(
        2 - xen.magnet.cond(origin = origin),
        NA,
        as.numeric(str.dsc.sel.split.V$NbScansToDo[2])
      ),
      # 9. ACTUAL NUMBER OF POINTS/RESOLUTION UNITLESS:
      as.numeric(str.dsc.sel.split.V$XPTS[2]),
      # 10. POWER mW:
      as.numeric(str.dsc.sel.split.V$MWPW[2]) * 1e+3,
      # 11. CONVERSION TIME s:
      as.numeric(str.dsc.sel.split.V$SPTP[2]),
      # 12. SWEEP TIME s:
      as.numeric(str.dsc.sel.split.V$SPTP[2]) *
        as.numeric(str.dsc.sel.split.V$XPTS[2]),
      # 13. TIME CONSTANT s:
      switch(
        2 - xen.magnet.cond(origin = origin),
        NA,
        as.numeric(str.dsc.sel.split.V$RCTC[2])
      ),
      # 14. RECEIVER GAIN dB:
      switch(
        2 - xen.magnet.cond(origin = origin),
        NA,
        as.numeric(str.dsc.sel.split.V$RCAG[2])
      ),
      # 15. TEMPERATURE K:
      switch(
        2 - temperature.check,
        as.numeric(str.dsc.sel.split.V$STMP[2]),
        NA
      ),
      # 16. MODULATION FREQUENCY kHz:
      as.numeric(str.dsc.sel.split.V$B0MF[2]) * 1e-3,
      # 17. CONVERSION FACTOR UNITLESS:
      switch(
        2 - xen.magnet.cond(origin = origin),
        NA,
        as.numeric(str.dsc.sel.split.V$ConvFact[2])
      ),
      # 18. Y 2nd VARIABLE SERIES RESOLUTION:
      switch(
        2 - y2ndVar.check,
        as.numeric(str.dsc.sel.split.V$YPTS[2]),
        NA
      ),
      # 19. Y 2nd VARIABLE SERIES START:
      switch(
        2 - y2ndVar.check,
        as.numeric(str.dsc.sel.split.V$YMIN[2]),
        NA
      ),
      # 20. Y 2nd VARIABLE SERIES END:
      switch(
        2 - y2ndVar.check,
        as.numeric(str.dsc.sel.split.V$YWID[2]),
        NA
      )
    )
    #
    ## general parameter values magnettech
    # Value.m <- Value.x[-c(7,8,13,14,17)]
    # Value <- switch(2-xen.magnet.cond(origin = origin),
    #                      Value.m,
    #                      Value.x)
    #
    ## Corresponding units for the Values Xenon
    ## `gsub` because the `information[x]` returns
    ## string with ` '' ` like ` "'G'" `
    Unit <- c(
      "GHz","Unitless",gsub("'","",Information[6]),
      gsub("'","",Information[6]),
      "mT","Unitless","Unitless","Unitless","Unitless",
      "mW","s","s","s","dB","K","kHz","Unitless",
      "Unitless",gsub("'","",Information[9]),
      gsub("'","",Information[9])
    )
    #
    ## Corresponding units for the Values magnettech
    # Unit.m <- Unit.x[-c(7,8,13,14,17)]
    # Unit <- switch(2-xen.magnet.cond(origin = origin),
    #                 Unit.m,
    #                 Unit.x)
  }
  #
  ## ------------------ WINEPR ------------------------
  #
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))){
    #
    ## general character params. vectors ->
    ParameterCh <-
      c(
        "Operator", "Date", "Recording Time", "Comment",
        "Experiment Title", "X Var. (e.g. B/RF) Unit",
        "Y 2nd Var. Series"
      )
    #
    ## general parameter Information
    Information <- c(
      # 1. OPERATOR:
      as.character(str.dsc.sel.split.Ch$JON[2]),
      # 2. DATE:
      as.character(str.dsc.sel.split.Ch$JDA[2]),
      # 3. RECORDING TIME:
      as.character(str.dsc.sel.split.Ch$JTM[2]),
      # 4. COMMENT:
      as.character(str.dsc.sel.split.Ch$JCO[2]),
      # 5. EXPERIMENT TITLE:
      as.character(str.dsc.sel.split.Ch$JEX[2]),
      # 6. X VARIABLE UNIT:
      switch(
        2 - y2ndVar.check,
        as.character(str.dsc.sel.split.Ch$XXUN[2]),
        as.character(str.dsc.sel.split.Ch$JUN[2])
      ),
      # 7. Y 2nd VARIABLE SERIES:
      switch(
        2 - y2ndVar.check,
        as.character(str.dsc.sel.split.Ch$JEY[2]),
        NA
      )
    )
    #
    ## general parameter value string for Winepr
    ParameterV <- c(
      "Frequency", "Central Field", "Sweep Width",
      "Modulation Amplitude","Number of Scans", "Number of Points",
      "Power", "Conversion Time","Sweep Time", "Acquire Time",
      "Time Constant", "Temperature", "Receiver Gain",
      "Y 2nd Var. Series Resolution","Y 2nd Var. Series Start",
      "Y 2nd Var. Series End"
    )
    #
    ## general parameter values Winepr
    Value <- c(
      # 1. FREQUENCY in GHz:
      as.numeric(str.dsc.sel.split.V$MF[2]),
      # 2. ACTUAL CENTRAL FIELD (B)
      # 2. UNITS -> see `Information[6]`
      as.numeric(str.dsc.sel.split.V$HCF[2]),
      # 3. ACTUAL SWEEP WIDTH (B)
      # 3. UNITS -> see `Information[6]`
      as.numeric(str.dsc.sel.split.V$HSW[2]),
      # 4. MODULATION AMPLITUDE
      # 4. UNITS -> see `Information[6]`
      as.numeric(str.dsc.sel.split.V$RMA[2]),
      # 5. NUMBER OF SCANS UNITLESS
      as.numeric(str.dsc.sel.split.V$JSD[2]),
      # 6. NUMBER OF POINTS UNITLESS
      as.numeric(str.dsc.sel.split.V$RES[2]),
      # 7. POWER mW
      as.numeric(str.dsc.sel.split.V$MP[2]),
      # 8. CONVERSION TIME s
      as.numeric(str.dsc.sel.split.V$RCT[2]) * 1e-3,
      # 9. SWEEP TIME s
      as.numeric(str.dsc.sel.split.V$RCT[2]) * 1e-3 *
        as.numeric(str.dsc.sel.split.V$RES[2]),
      # 10. ACQUIRE TIME s
      as.numeric(str.dsc.sel.split.V$RCT[2]) * 1e-3 *
        as.numeric(str.dsc.sel.split.V$RES[2]) *
        as.numeric(str.dsc.sel.split.V$JSD[2]),
      # 11. TIME CONSTATNT s
      as.numeric(str.dsc.sel.split.V$RTC[2]) * 1e-3,
      # 12. TEMPERATURE K
      switch(
        2 - temperature.check,
        as.numeric(str.dsc.sel.split.V$TE[2]),
        NA
      ),
      # 13. RECEIVER GAIN UNITLESS
      as.numeric(str.dsc.sel.split.V$RRG[2]),
      # 14. Y 2nd VARIABLE SERIES RESOLUTION:
      switch(
        2 - y2ndVar.check,
        as.numeric(str.dsc.sel.split.V$REY[2]),
        NA
      ),
      # 15. Y 2nd VARIABLE SERIES START:
      switch(
        2 - y2ndVar.check,
        as.numeric(str.dsc.sel.split.V$XYLB[2]),
        NA
      ),
      # 16. Y 2nd VARIABLE SERIES END:
      switch(
        2 - y2ndVar.check,
        as.numeric(str.dsc.sel.split.V$XYWI[2]),
        NA
      )
    )
    #
    ## Corresponding units for the Values Winepr
    Unit <- c(
      "GHz",Information[6],Information[6],Information[6],
      "Unitless","Unitless","mW","s","s","s","s",
      "K","Unitless","Unitless (Slices)","Unitless (Slice)",
      "Unitless (Slice)"
    )
  }
  #
  ## ================= ENTIRE DATA FRAMES AND RESULTS ==================
  #
  ## data frame definitions =>
  data.instrument.Ch <- data.frame(
    Parameter = ParameterCh,
    Information = Information
  )
  ## filter NA information
  data.instrument.Ch <-
    data.instrument.Ch %>%
    dplyr::filter(!is.na(Information))
  #
  data.instrument.V <- data.frame(
    Parameter = ParameterV,
    Value = Value,
    Unit = Unit
  )
  ## filter NA information
  data.instrument.V <-
    data.instrument.V %>%
    dplyr::filter(!is.na(Value))
  #
  tab.list <-
    list(params = data.instrument.V, info = data.instrument.Ch)
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
