#
#' Read and Process CW EPR Time Series Experiments
#'
#'
#' @family Data Reading
#'
#'
#' @description Reading the continuous wave (CW) EPR time series spectral data (recorded by e.g. \code{2D_Field_Delay}
#'  experiment in "Xenon" acquisition/processing software). Function is based on the \code{\link{readEPR_Exp_Specs}}
#'  and includes automatic time correction for CW EPR \code{time.series} experiments
#'  (see also the \code{\link{correct_time_Exp_Specs}} description and documentation). If the time series EPR spectra
#'  are stored individually (one file per one spectrum, e.g. for \code{origin = "Magnettech"}, ESR5000 [11-0422]),
#'  such time series needs to be loaded by \code{\link{readEPR_Exp_Specs_multif}}. For \code{origin = "WinEpr"}
#'  the \code{time_s} column usually possesses the form of spectrum slices, i.e. an integer number (0,1,2,...) is assigned
#'  to each recorded spectrum. Therefore, for a radical kinetic analysis it has to be converted to \code{time_s}
#'  (see the argument \code{time.delta.slice}).
#'
#'
#' @inheritParams readEPR_Exp_Specs
#' @param path_to_file Character string, path to any spectrometer/instrumental file,
#'   having one the following extensions: \code{.txt}, \code{.csv}, \code{.asc}, \code{.DTA} or \code{.spc},
#'   including the 2D-experimental (i.e. \eqn{Intensity} vs \eqn{B} vs \eqn{time}) EPR data. The path can be also defined
#'   by the \code{\link[base]{file.path}} function.
#' @param time.unit Character string, specifying the \code{"s"},\code{"min"}, \code{"h"}
#'   or \code{time.unit = "unitless"} (if \code{time.delta.slice.s} is different from \code{NULL}).
#'   \strong{Default}: \code{time.unit = "s"}
#' @param time.delta.slice Numeric, time interval in \code{time.unit} between \code{slices},
#'   in the case of \code{origin = "winepr"}. \strong{Default}: \code{time.delta.slice = NULL} (actually,
#'   corresponding to \code{1 time.unit}).
#' @param col.names Character string vector, corresponding to desired column/variable names/headers of the returned
#'   data frame/table. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its unit, \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"} or \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum \eqn{x}-axis). \strong{Default}: \code{col.names = c("index","B_G","time_s","dIepr_over_dB")}
#'   (if \code{orgin = "xenon"}). For the \strong{spectral 2D-time series} \code{col.names} \strong{must include character string}
#'   (\strong{such as} \code{"time_s"}) \strong{in order to identify the time at which the EPR spectra were recorded}.
#' @param var2nd.series.id Numeric index related to \code{col.names} vector and pointing to column
#'   for the EPR spectral 2D- time series variable. \strong{Default}: \code{var2nd.series.id = 3}
#'   (for "Xenon" time series experiment, corresponding to "time" column).
#' @param qValue Numeric, Q value (quality or sensitivity factor number) displayed at specific \code{dB} by the spectrometer,
#'   in case of \emph{Xenon} or \emph{new Magnettech} software it is returned automatically, however in the case of \emph{WinEPR},
#'   it must be provided by the user. \strong{default}: \code{qValue = NULL}, actually corresponding to value \code{1}.
#' @param ... additional arguments specified, see also the \code{\link{readEPR_Exp_Specs}}
#'   and \code{\link[data.table]{fread}}.
#'
#' @return List of EPR spectrum time series data in tidy long table format (\code{df}) + corrected
#'    time vector (\code{time}). For the \code{origon = "winepr"} "time" slices/indices must be already converted
#'    into time domain by \code{time.delta.slice} (see arguments and examples).
#'
#'
#' @examples
#' ## loading the built-in package example to demonstrate
#' ## the reading of time series EPR spectra/kinetics:
#' triarylam.decay.series.dsc.path <-
#'   load_data_example(file =
#'     "Triarylamine_radCat_decay_series.DSC")
#' triarylam.decay.series.ygf.path <-
#'   load_data_example(file =
#'     "Triarylamine_radCat_decay_series.YGF")
#' triarylam.decay.series.asc.path <-
#'   load_data_example(file =
#'     "Triarylamine_radCat_decay_series.zip")
#' unzip(triarylam.decay.series.asc.path,exdir = tempdir())
#' #
#' ## loading the kinetics:
#' triarylam.decay.series.data <-
#'   readEPR_Exp_Specs_kin(path_to_file = paste0(
#'                           tempdir(),
#'                           "/Triarylamine_radCat_decay_series.txt"
#'                         ),
#'                         path_to_dsc_par =
#'                           triarylam.decay.series.dsc.path,
#'                         path_to_ygf =
#'                           triarylam.decay.series.ygf.path
#'                        )
#' #
#' ## data preview
#' head(triarylam.decay.series.data$df)
#' #
#' ## preview of corrected time vector
#' ## (the uncorrected one actually starts from `0`)
#' triarylam.decay.series.data$time
#' #
#' ## reading the EPR time series
#' ## returned by the "WinEPR" software
#' tmpd.se.cv.b.bin.path <-
#'   load_data_example("TMPD_specelchem_CV_b.spc")
#' tmpd.se.cv.b.par.path <-
#'   load_ddata_example("TMPD_specelchem_CV_b.par")
#' tmpd.se.cv.b.dat <-
#'   readEPR_Exp_Specs_kin(
#'     path_to_file = tmpd.se.cv.b.bin.path,
#'     path_to_dsc_par = tmpd.se.cv.b.par.path,
#'     col.names = c(
#'       "B_G",
#'       "Slice",
#'       "dIepr_over_dB"
#'      ),
#'   var2nd.series.id = 2,
#'   time.delta.slice = 18,
#'   origin = "winepr"
#'   )
#' #
#' ## data preview
#' head(tmpd.se.cv.b.dat)
#'
#'
#' @export
#'
#'
readEPR_Exp_Specs_kin <- function(path_to_file,
                                  path_to_dsc_par = NULL,
                                  path_to_ygf = NULL,
                                  time.unit = "s",
                                  time.delta.slice = NULL,
                                  col.names = c(
                                    "index",
                                    "B_G",
                                    "time_s",
                                    "dIepr_over_dB"
                                  ),
                                  x.unit = "G",
                                  var2nd.series.id = 3,
                                  origin = "xenon",
                                  qValue = NULL,
                                  ...) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## ================= Reading Files & Parameters ==================
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
  ## general condition to read `.id`s and co...(origin condition)
  origin.cond.all <- function(origin){
    if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
      return(0)
    }
    if (any(grepl(paste(magnettech.string,collapse = "|"),origin))){
      return(1)
    }
    if (any(grepl(paste(xenon.string,collapse = "|"),origin))){
      return(2)
    }
  }
  #
  ## condition for switching between xenon and magnettech
  if (origin.cond.all(origin = origin) == 2 ||
      origin.cond.all(origin = origin) == 1) {
    xen.magnet.cond <- function(origin){
      if (any(grepl(paste(xenon.string,collapse = "|"),origin))){
        return(0)
      }
      if (any(grepl(paste(magnettech.string,collapse = "|"),origin))){
        return(1)
      }
    }
  }
  #
  ## checking the path string, whether it points to ASCII or BINARY
  ## + checking the corresponding origin
  # ascii.cond <- grepl(".*\\.(txt|asc|csv)$",path_to_file)
  # binary.cond <- grepl(".*\\.(DTA|spc)$",path_to_file)
  # if (grepl(".*\\.spc$",path_to_file)) {
  #   if (origin.cond.all(origin = origin) == 2 ||
  #       origin.cond.all(origin = origin) == 1) {
  #     stop(" Reading of the '.spc' file requires origin = 'winepr' !! ")
  #   }
  # }
  # if (grepl(".*\\.DTA$",path_to_file)) {
  #   if (origin.cond.all(origin = origin) == 0) {
  #     stop(" Reading of the '.DTA' file requires\n
  #          origin = 'xenon' or origin = 'magnettech' !! ")
  #   }
  # }
  #
  # ------------------------- XENON & MAGNETTECH -----------------------------
  if (origin.cond.all(origin = origin) == 2 ||
      origin.cond.all(origin = origin) == 1) {
    #
    ## expecting that the `.DSC`/`.dsc` file is in the same folder
    ## as the `.DTA` one and possesses the same name
    path.to.dsc <- gsub(
      "\\.DTA$",
      switch(2 - xen.magnet.cond(origin = origin),".dsc",".DSC"),
      path_to_file
    )
    #
    ## check the existence of this previous file in the current working dir.
    if (!file.exists(path.to.dsc)) {
      stop(" Please provide the file path for the `.DSC`/`.dsc` file,\n
             refer to definition of the `path_to_dsc_par` argument !! ")
    } else {
      path_to_dsc_par <-
        path_to_dsc_par %>% `if`(is.null(path_to_dsc_par), path.to.dsc, .)
    }
    #
    ## Qvalue
    qValue.obtain <-
      readEPR_param_slct(
        path_to_dsc_par,
        string = "QValue",
        origin = origin
      )
    #
    ## condition for the Q-Value
    if (is.null(qValue.obtain) || is.na(qValue.obtain)) {
      qValue.obtain <- qValue %>% `if`(is.null(qValue), 1, .)
      warning("Sensitivity factor or Q-Value WAS NOT FOUND in the `.DSC`/`.dsc`\n
              file !! It is automatically switched to `1`, unless you won't define\n
              the `qValue` argument !! ")
    }
  }
  #  ------------------------ WINEPR --------------------------
  if (origin.cond.all(origin = origin) == 0) {
    #
    ## expecting that the `.par` file is in the same folder
    ## as the `.spc` one and possesses the same name
    path.to.par <- gsub(
      "\\.spc$",
      ".par",
      path_to_file
    )
    #
    ## check the existence of this previous file in the current working dir.
    if (!file.exists(path.to.par)) {
      stop(" Please provide the file path for the `.par` file,\n
             refer to definition of the `path_to_dsc_par` argument !! ")
    } else {
      path_to_dsc_par <-
        path_to_dsc_par %>% `if`(is.null(path_to_dsc_par), path.to.par, .)
    }
    #
    ## Qvalue definition
    if (is.null(qValue) || is.na(qValue)) {
      warning("Sensitivity factor or Q-Value WAS NOT DEFINED !!\n
              It is automatically switched to `1`, unless you won't define\n
              the `qValue` argument !! ")
    }
    qValue.obtain <- qValue %>% `if`(is.null(qValue), 1, .)
  }
  #
  ## ------------------ 'Kinetic' instrum. params --------------------
  instrument.params.kinet <-
    readEPR_params_slct_kin(path_to_dsc_par, origin = origin)
  #
  ## ================= Reading Data & Processing ==================
  #
  ## ------ conditions and definitions for "automatic" `.id`s ------------
  ## `x.id`
  if (exists("x.id")) {
    x.id <- switch(
      3 - origin.cond.all(origin = origin),
      x.id %>% `if`(x.id != 2, 2, .), ## check xenon
      x.id %>% `if`(x.id != 1 || x.id != 2,
                    switch(2 - binary.cond, 2, 1), .), ## magnettech
      x.id %>% `if`(x.id != 1, 1, .) ## check winepr
    )
  } else {
    x.id <- switch(
      3 - origin.cond.all(origin = origin),
      2,
      switch(
        2 - binary.cond,
        2,
        stop("Column for `x`/`B` is not defined ! \n
             Please, specify the `x.id` of the column/variable,\n
             or create it to proceed !! ")
      ),
      1
    )
  }
  ## `Intensity.id`
  if (exists("Intensity.id")) {
    Intensity.id <- switch(
      3 - origin.cond.all(origin = origin),
      Intensity.id %>% `if`(Intensity.id != 4, 4, .), ## check xenon
      switch(2 - binary.cond, 4, Intensity.id), ## magnettech
      Intensity.id %>% `if`(Intensity.id != 3, 3, .) ## check winepr
    )
  } else {
    Intensity.id <- switch(
      3 - origin.cond.all(origin = origin),
      4,
      switch(
        2 - binary.cond,
        4,
        stop("Column for `Intensity` is not defined ! \n
             Please, specify the `Intensity.id` of the column/variable,\n
             or create it to proceed !! ")
      ),
      3
    )
  }
  ## `var2nd.series.id`
  var2nd.series.id <- switch(
    3 - origin.cond.all(origin = origin),
    var2nd.series.id %>%
      `if`(var2nd.series.id != 3 || is.null(var2nd.series.id), 3, .),
    stop(" For the recorded EPR spectral series on `Magnettech`\n
             please use the `readEPR_Exp_Specs_multif` function, instead.\n
             Spectra are saved/stored individually on such machine !! "),
    var2nd.series.id %>%
      `if`(var2nd.series.id != 2 || is.null(var2nd.series.id), 2, .)
  )
  #
  ## ----------------------------------------------------
  #
  ## `Intensity` variable string
  IntensityString <- col.names[Intensity.id]
  #
  ## `time` variable string
  timeString <- col.names[var2nd.series.id]
  #
  ## Load spectral data
  data.spectra.time <-
    readEPR_Exp_Specs(
      path_to_file = path_to_file,
      path_to_dsc_par = path_to_dsc_par,
      path_to_ygf = path_to_ygf,
      col.names = col.names,
      x.id = x.id,
      Intensity.id = Intensity.id,
      var2nd.series.id = var2nd.series.id,
      qValue = qValue.obtain,
      origin = origin,
      ...
   )
  #
  ## recalculate  the time
  ## time var for `data.spectra.time`
  times <- data.spectra.time[[timeString]]
  #
  if (origin.cond.all(origin = origin) == 0){
    ## Re-definition for `time.delta.slice`
    ## ASSUMING USER CAN MAKE MISTAKES :-)
    time.delta.slice <-
      time.delta.slice %>%
      `if`(is.null(time.delta.slice),1, .)
  }
  #
  if (time.unit == "s" || time.unit == "unitless" || time.unit == "Unitless") {
    times <- times
    #
    if (origin.cond.all(origin = origin) == 0){
      time.delta.slice <- time.delta.slice
    }
  }
  if (time.unit == "min") {
    times <- times * 60
    ## rename column
    colnames(data.spectra.time)[colnames(data.spectra.time) == timeString] <- "time_s"
    #
    ## time interval
    if (origin.cond.all(origin = origin) == 0){
      time.delta.slice <- time.delta.slice * 60
    }
  }
  if (time.unit == "h") {
    times <- times * 3600
    ## rename column
    colnames(data.spectra.time)[colnames(data.spectra.time) == timeString] <- "time_s"
    #
    ## time interval
    if (origin.cond.all(origin = origin) == 0){
      time.delta.slice <- time.delta.slice * 3600
    }
  }
  ## Re-definition for `time.delta.slice`
  if (origin.cond.all(origin = origin) == 0){
    #
    times <- times * time.delta.slice.s
    #
  }
  #
  data.spectra.time[[timeString]] <-
    correct_time_Exp_Specs(
      time.s = times,
      Nscans = instrument.params.kinet$Nscans,
      sweep.time.s = instrument.params.kinet$swTime
    )
  #
  ## corrected time df
  time.corrected <- data.spectra.time %>%
    dplyr::group_by(.data[[timeString]]) %>%
    dplyr::group_keys()
  #
  data.all.spectra <-
    list(df = data.spectra.time, time = time.corrected[[timeString]])
  #
  return(data.all.spectra)
  #
}
