#
#' Read and Process Spectral Data of Time Dependent CW EPR Experiments
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
#' @param name.root Character string, corresponding to entire \strong{file name without extension}.
#' @param dir_ASC Character string, path (can be also defined by \code{\link[base]{file.path}})
#'   to directory where the \code{ASCII} spectral data is stored.
#' @param dir_dsc_par Character string, path (can be also defined by \code{\link[base]{file.path}})
#'   to directory where the \code{.DSC}/\code{.dsc} or \code{.par} parameter file is stored
#'   (to calculate \eqn{g}-value and/or normalize intensities).
#' @param time.unit Character string, specifying \code{"s"},\code{"min"}, \code{"h"}
#'   or \code{time.unit = "unitless"} (if \code{time.delta.slice.s} is different from \code{NULL}).
#'   \strong{Default}: \code{time.unit = "s"}
#' @param time.delta.slice.s Numeric, time span/interval in seconds between \code{slices},
#'   in the case if \code{origin = "winepr"}. \strong{Default}: \code{time.delta.slice = NULL} (actually,
#'   corresponding to \code{1 s}).
#' @param col.names Character/String vector inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its units, \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"}, \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum abscissa)...etc, \strong{default}: \code{col.names = c("index","B_G","dIepr_over_dB")}.
#' @param time.series.id Numeric index related to \code{col.names} pointing to \code{time} column for time series
#'   EPR spectra changing upon time. If data contains simple relationship like \eqn{Area} vs \eqn{time}
#'   use \code{x} and \code{x.unit} parameters/arguments instead. This parameter/argument is dedicated
#'   to kinetic-like experiments. \strong{Default}: \code{time.series.id = 3}.
#' @param qValue Numeric, Q value (quality factor, number) displayed at specific \code{dB} by spectrometer.
#'   In case of "Xenon" software the parameter is included in \code{.DSC} file, therefore \strong{default}:
#'   \code{qValue = NULL} (actually corresponding to value \code{1}). If EPR spectra were acquired
#'   by the "Winepr" software Q value must be defined like \code{qValue = 3400}.
#' @param norm.vec.add Numeric vector, additional normalization constant in the form of vector, involving
#'   all additional (in addition to \code{qValue}) normalization(s) such as concentration, powder sample
#'   weight, number of scans, ...etc (e.g. \code{norm.vec.add = c(2000,0.5,2)}). \strong{Default}:
#'   \code{norm.vec.add = NULL} (actually corresponding to value \code{1}).
#' @param ... additional arguments specified, see also \code{\link{readEPR_Exp_Specs}}
#'   and \code{\link[data.table]{fread}}.
#'
#' @return List of EPR spectrum data (including time) in tidy long table format (\code{df}) + corrected
#'    time vector (\code{time}). For \code{origon = "winepr"} "time" slices/indices must be already converted
#'    into time domain by \code{time.delta.slice.s} (see arguments and examples).
#'
#'
#' @examples
#' ## loading the built-in package example to demonstrate
#' ## the reading of time series EPR spectra/kinetics:
#' triarylam.decay.series.dsc.path <-
#'   load_data_example(file = "Triarylamine_radCat_decay_series.DSC")
#' triarylam.decay.series.asc.path <-
#'   load_data_example(file = "Triarylamine_radCat_decay_series.zip")
#' unzip(triarylam.decay.series.asc.path,exdir = tempdir())
#' #
#' ## loading the kinetics:
#' triarylam.decay.series.data <-
#'   readEPR_Exp_Specs_kin(name.root = "Triarylamine_radCat_decay_series",
#'                         dir_ASC = tempdir(),
#'                         dir_dsc_par =
#'                           system.file("extdata",
#'                                       package = "eprscope")
#'                        )
#' #
#' ## data preview
#' head(triarylam.decay.series.data$df)
#' #
#' ## preview of corrected time vector
#' ## (the uncorrected one actually starts from `0`)
#' triarylam.decay.series.data$time
#' #
#' \dontrun{
#' ## reading by the "WinEPR" software
#' readEPR_Exp_Specs_kin("Sample_spectra_irradiation",
#'                       file.path(".","ASCII_data_dir"),
#'                       file.path(".","dsc_data_dir"),
#'                       time.unit = "s",
#'                       time.delta.slice.s = 24.1,
#'                       col.names = c("B_G",
#'                                     "Slice",
#'                                     "Intensity"),
#'                       x.unit = "G",
#'                       qValue = 2900,
#'                       origin = "winepr")
#'
#' }
#'
#'
#' @export
#'
#'
readEPR_Exp_Specs_kin <- function(name.root,
                                  dir_ASC,
                                  dir_dsc_par,
                                  time.unit = "s",
                                  time.delta.slice.s = NULL,
                                  col.names = c(
                                    "index",
                                    "B_G",
                                    "time_s",
                                    "dIepr_over_dB"
                                  ),
                                  x.id = 2,
                                  x.unit = "G",
                                  Intensity.id = 4,
                                  time.series.id = 3,
                                  convertB.unit = TRUE,
                                  qValue = NULL,
                                  norm.vec.add = NULL,
                                  origin = "xenon",
                                  ...) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## file name.root which has to be the same for `ASC`+`DSC`/`dsc`
  ## or `.spc` and `.par`and corresponds to file name without extension
  #
  ## ================= Reading Files & Parameters ==================
  #
  ## origin strings vectors to define "origin" conditions =>
  winepr.string <- c("winepr","Winepr","WinEpr","WINEPR","WinEPR","winEPR")
  xenon.string <- c("xenon","Xenon","XENON")
  magnettech.string <- c("magnettech","Magnettech","MagnetTech",
                         "magnetTech","MAGNETECH")
  #
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))) {
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
    ## path to `asc` file
    path.to.asc <- file.path(
      dir_ASC,
      paste0(name.root,
             switch(2-xen.magnet.cond(origin = origin),
                    ".csv",
                    ".txt"
                    )
             ) ## `txt` for xenon & `csv` for magnettech
    )
    #
    ## path to `DSC` or `dsc`
    path.to.dsc.par <-
      list.files(path = dir_dsc_par,
                 pattern = paste0("^",name.root, "\\.(DSC|dsc)$"),
                 full.names = TRUE
      )
    #
    ## Qvalue
    qValue.obtain <-
      readEPR_param_slct(path.to.dsc.par,
                         string = "QValue",
                         origin = origin
    )
  }
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
    ## path to asc
    path.to.asc <- file.path(
      dir_ASC,
      paste0(name.root, ".asc")
    )
    #
    ## path to `par`
    path.to.dsc.par <- file.path(
      dir_dsc_par,
      paste0(name.root, ".par")
    )
    #
    ## Qvalue definition
    qValue.obtain <- qValue %>% `if`(is.null(qValue), 1, .)
  }
  #
  ## 'Kinetic' instrum. params
  instrument.params.kinet <-
    readEPR_params_slct_kin(path.to.dsc.par, origin = origin)
  #
  ## ================= Reading Data & Processing ==================
  #
  ## `Intensity` variable string
  IntensityString <- col.names[Intensity.id]
  #
  ## `time` variable string
  timeString <- col.names[time.series.id]
  #
  ## Load spectral data
  data.spectra.time <- readEPR_Exp_Specs(path.to.asc,
    col.names = col.names,
    x.id = x.id,
    Intensity.id = Intensity.id,
    time.series.id = time.series.id,
    convertB.unit = convertB.unit,
    qValue = qValue.obtain,
    norm.vec.add = norm.vec.add,
    origin = origin,
    ...
  ) %>%
    dplyr::filter(.data[[IntensityString]] != 0) ## only non-zero intens. selected
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
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))){
    ## ASSUMING USER CAN MAKE MISTAKES :-)
    time.delta.slice.s <- time.delta.slice.s %>% `if`(is.null(time.delta.slice.s),1, .)
    #
    ## `time` if spectra are recorded as `slices` series
    if (grepl("s",time.unit) & !is.null(time.delta.slice.s)) {
      times <- times * time.delta.slice.s
    } else {
      stop(" `time.unit` must be either 's' or `unitless` !! ")
    }
  }
  #
  data.spectra.time[[timeString]] <- correct_time_Exp_Specs(
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
