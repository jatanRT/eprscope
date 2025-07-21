#
#' Read and Process CW EPR Time Series Experiments
#'
#'
#' @family Data Reading
#'
#'
#' @description Reading the continuous wave (CW) EPR time series spectral data (recorded by e.g. \code{2D_Field_Delay}
#'  experiment in "Xenon" acquisition/processing software). Function (based on \code{\link{readEPR_Exp_Specs}}) includes
#'  automatic time correction for CW EPR \code{time.series} experiments (see also \code{\link{correct_time_Exp_Specs}}).
#'
#'
#' @inheritParams readEPR_Exp_Specs
#' @param path_to_file Character string, path to any spectrometer/instrumental file,
#'   having one the following extensions: \code{.txt}, \code{.csv}, \code{.asc}, \code{.DTA} or \code{.spc},
#'   including the 2D-experimental (i.e. \eqn{Intensity} vs \eqn{B} vs \eqn{time}) EPR data. The path can be also defined
#'   by the \code{\link[base]{file.path}} function.
#' @param path_to_dsc_par Character string, path (also provided by \code{\link[base]{file.path}})
#'   to \code{.DSC/.dsc} (\code{origin = "xenon"}/\code{origin = "magnettech"}) or \code{.par} (\code{origin = "winepr"})
#'   ASCII \code{text} file, including instrumental parameters of the recorded spectra
#'   (corresponding to the previous argument) and provided by the EPR machine.
#'   \strong{Default}: \code{path_to_dsc_par = NULL}. The latter assignment actually means that the argument
#'   automatically inherits the \code{path_to_file}, however with the appropriate extension
#'   (\code{.DSC/.dsc} or \code{.par}). In other words, the function is looking for the same
#'   filename like in \code{path_to_file} in the working directory. If the file does not exist, it will ask
#'   to provide/define the right file path.
#' @param path_to_ygf Character string, path (also provided by \code{\link[base]{file.path}})
#'   to binary \code{.YGF} file (\code{origin = "xenon"}/\code{origin = "magnettech"}), storing the values of the 2nd
#'   independent variable in the spectral time series, see also the \code{var2nd.series.id} argument description.
#'   \strong{Default}: \code{path_to_ygf = NULL}. The latter assignment actually means that
#'   the argument automatically inherits the \code{path_to_file}, however with the appropriate extension \code{.YGF}.
#'   In other words, the function is looking for the same file name like in \code{path_to_file} in the working directory.
#'   If the file does not exist, it automatically grabs those values based on the information provided by the \code{.DSC/.dsc}
#'   (\code{origin = "xenon"}/\code{origin = "magnettech"}) or \code{.par} (\code{origin = "winepr"}) files (see the argument
#'   \code{path_to_dsc_par} description).
#' @param time.unit Character string, specifying the \code{"s"},\code{"min"}, \code{"h"}
#'   or \code{time.unit = "unitless"} (if \code{time.delta.slice.s} is different from \code{NULL}).
#'   \strong{Default}: \code{time.unit = "s"}
#' @param time.delta.slice.s Numeric, time interval in seconds between \code{slices},
#'   in the case if \code{origin = "winepr"}. \strong{Default}: \code{time.delta.slice = NULL} (actually,
#'   corresponding to \code{1 s}).
#' @param col.names Character string vector, corresponding to desired column/variable names/headers of the returned
#'   data frame/table. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its unit, \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"} or \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum \eqn{x}-axis). \strong{Default}: \code{col.names = c("index","B_G",dIepr_over_dB)}
#'   (if \code{orgin = "xenon"}). For the spectral 2D-time series \code{col.names} must include character string
#'   (such as \code{"time_s"}) in order to identify the time at which the EPR spectra were recorded.
#' @param var2nd.series.id Numeric index related to \code{col.names} vector and pointing to column
#'   for the EPR spectral 2D- time series variable. \strong{Default}: \code{var2nd.series.id = 3}
#'   (for "Xenon" time series experiment, corresponding to "time" column).
#' @param qValue Numeric, Q value (quality or sensitivity factor number) displayed at specific \code{dB} by the spectrometer,
#'   in case of \emph{Xenon} or \emph{new Magnettech} software it is returned automatically, however in the case of \emph{WinEPR},
#'   it must be provided by the user. \strong{default}: \code{qValue = NULL}, actually corresponding to value \code{1}.
#' @param ... additional arguments specified, see also the \code{\link{readEPR_Exp_Specs}}
#'   and \code{\link[data.table]{fread}}.
#'
#' @return List of EPR spectrum data (including time) in tidy long table format (\code{df}) + corrected
#'    time vector (\code{time}). For the \code{origon = "winepr"} "time" slices/indices must be already converted
#'    into time domain by \code{time.delta.slice.s} (see arguments and examples).
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
#'     tmpd.se.cv.b.bin.path,
#'     tmpd.se.cv.b.par.path,
#'     col.names = c(
#'       "B_G",
#'       "Slice",
#'       "dIepr_over_dB"
#'      ),
#'   var2nd.series.id = 2,
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
                                  time.delta.slice.s = NULL,
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
  ## + chicking the corresponding origin
  ascii.cond <- grepl(".*\\.(txt|asc|csv)$",path_to_file)
  binary.cond <- grepl(".*\\.(DTA|spc)$",path_to_file)
  if (grepl(".*\\.DTA$",path_to_file)) {
    if (origin.cond.all(origin = origin) != 2 ||
        origin.cond.all(origin = origin) != 1) {
      stop(" Reading of the '.DTA' file requires\n
           origin = 'xenon' or origin = 'magnettech' !! ")
    }
  }
  if (grepl(".*\\.spc$",path_to_file)) {
    if (origin.cond.all(origin = origin) != 0) {
      stop(" Reading of the '.spc' file requires origin = 'winepr'!! ")
    }
  }
  #
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
    ## check the existence of this previous file in the working dir.
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
    ## check the existence of this previous file in the working dir.
    if (!file.exists(path.to.par)) {
      stop(" Please provide the file path for the `.par` file,\n
             refer to definition of the `path_to_dsc_par` argument !! ")
    } else {
      path_to_dsc_par <-
        path_to_dsc_par %>% `if`(is.null(path_to_dsc_par), path.to.par, .)
    }
    #
    ## Qvalue definition
    qValue.obtain <- qValue %>% `if`(is.null(qValue), 1, .)
  }
  #
  ## 'Kinetic' instrum. params
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
      switch(2 - binary.cond,2,x.id), ## magnettech
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
      switch(2 - binary.cond,4,Intensity.id), ## magnettech
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
    switch(2 - binary.cond,3,var2nd.series.id),
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
   ) %>%
     dplyr::filter(.data[[IntensityString]] != 0)
     ## because only non-zero intens. selected
  #
  ## recalculate  the time
  ## time var for `data.spectra.time`
  times <- data.spectra.time[[timeString]]
  #
  if (time.unit == "min") {
    times <- times * 60
    ## rename column
    colnames(data.spectra.time)[colnames(data.spectra.time) == timeString] <- "time_s"
  }
  if (time.unit == "h") {
    times <- times * 3600
    ## rename column
    colnames(data.spectra.time)[colnames(data.spectra.time) == timeString] <- "time_s"
  }
  ## Re-definition for `time.delta.slice.s`
  if (origin.cond.all(origin = origin) == 0){
    ## ASSUMING USER CAN MAKE MISTAKES :-)
    time.delta.slice.s <-
      time.delta.slice.s %>%
      `if`(is.null(time.delta.slice.s),1, .)
    #
    ## `time` if spectra are recorded as `slices` series
    if (grepl("s",time.unit) & !is.null(time.delta.slice.s)) {
      times <- times * time.delta.slice.s
    } else {
      stop(" `time.unit` must be either 's' or `unitless` !! ")
    }
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
