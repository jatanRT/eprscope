#
#' Read and Process Spectral Data of Time Dependent CW EPR Experiments (e.g. like Kinetics)
#'
#'
#' @family Data Reading
#'
#'
#' @description The function reads (based on \code{\link{readEPR_Exp_Specs}}) the continuous wave (CW)
#'  EPR time series spectral data (recorded by e.g. `2D_Field_Delay Experiment` in "Xenon"
#'  acquisition/processing software). Function includes automatic time correction for CW EPR
#'  \code{time.series} experiments (see also \code{\link{correct_time_Exp_Specs}}).
#'
#'
#' @param name_root Character string corresponding to entire `file name` without `extension`.
#' @param dir_ASC Character string, path (can be defined by \code{\link[base]{file.path}}, String/Character)
#'   to directory where the `ASCII` file is stored.
#' @param dir_dsc_par Character string, path (can be defined by \code{\link[base]{file.path}} character string)
#'   to directory where the file (`.DSC`/`.dsc` or `.par`) with instrumental parameters (to calculate \eqn{g}-value
#'   or normalize intensities) is stored.
#' @param time.unit Character/String \strong{time unit} defined by \code{"s"},\code{"min"} or \code{"h"}.
#'   \strong{Default}: \code{time.unit = "s"}
#' @param time.delta.slice.s Numeric, time span/interval in seconds between `slices`...TBC,
#'   e.g. in case if \code{origin = "winepr"}. \strong{Default}: \code{time.delta.slice = NULL}.
#' @param col.names Character/String vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its units, \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"}, \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum abscissa)...etc, \strong{default}: \code{col.names = c("index","B_G",dIepr_over_dB)}.
#' @param colClasses List, inherited from \code{\link[data.table]{fread}}... TBC
#'   e.g. like \code{colClasses = list(numeric = 1)} or by character string like \code{colClasses = c(V1 = "numeric")}
#'   or \code{colClasses = list(numeric = "V1")} where in all cases `1` corresponds, to column index.
#'   \strong{Default}: \code{colClasses = NULL}.
#' @param x Numeric index related to \code{col.names} pointing to independent variable, which corresponds
#'   to abscissa (\eqn{x}-axis) in spectra or other plots.
#' @param x.unit Character/String ...TBC only "mT" and "G" are available
#' @param Intensity Numeric index related to \code{col.names} pointing to `general` intensity,
#'   like derivative intensity (`dIepr_over_dB`), integral one (e.g. `single_Integ`), double or sigmoid
#'   integral (e.g. `Area`)...etc. This corresponds to column/vector which should be presented like
#'   \eqn{y}-axis in spectra or other plots.
#' @param time.series Numeric index related to \code{col.names} pointing to `time` column for time series
#'   EPR spectra changing upon time. If data contains simple relationship like \eqn{Area} vs \eqn{time}
#'   use \code{x} and \code{x.unit} parameters/arguments instead. This parameter/argument is dedicated
#'   to kinetic-like experiments. \strong{Default}: \code{time.series = 3}.
#' @param convertB.unit Logical (\strong{default}: \code{convertB.unit = TRUE}) description...
#'   convert \eqn{B} in Gauss <=> millitesla...
#' @param qValue Numeric, Q value (quality factor, number) displayed at specific \code{dB} by spectrometer.
#'   In case of "Xenon" software the parameter is included in \code{.DSC} file, therefore \strong{default}:
#'   \code{qValue = NULL}. If EPR spectra were acquired by the "Winepr" software Q value must be defined
#'   like \code{qValue = 3400}
#' @param norm.vec.add Numeric vector. Additional normalization constant in form of vector involving
#'   all additional (in addition to \code{qValue}) normalization(s) like e.g. concentration, powder sample
#'   weight, number of scans, ...etc (\code{norm.vec.add = c(2000,0.5,2)}). \strong{Default}:
#'   \code{norm.vec.add = NULL}.
#' @param origin String/Character corresponding to \strong{software} used to acquire the EPR spectra
#'   on BRUKER spectrometers, i.e. whether they were recorded by the windows based softw. ("WinEpr",
#'   \code{origin = "winepr"}) or by the Linux one ("Xenon"), \strong{default}: \code{origin = "xenon"}
#'   Only the two above-mentioned  characters/strings are available due to reading parameter files.
#'
#'
#' @return List of spectral data (incl. time) in tidy long table format (\code{df}) + corrected
#'    time vector (\code{time}).
#'
#'
#' @examples
#' \dontrun{
#' ## Reading by the "Xenon" software
#' readEPR_Exp_Specs_kin("Sample_spectra_irradiation",
#'                       file.path(".","ASCII_data_dir"),
#'                       file.path(".","dsc_data_dir")
#'                       )
#'
#' ## Reading by the "WinEPR" software
#' readEPR_Exp_Specs_kin("Sample_spectra_irradiation",
#'                       file.path(".","ASCII_data_dir"),
#'                       file.path(".","dsc_data_dir"),
#'                       time.unit = "s",
#'                       time.delta.slice.s = 24.1,
#'                       col.names = c("index",
#'                                     "B_G",
#'                                     "Slice",
#'                                     "Intensity"),
#'                       colClasses = NULL,
#'                       x = 2,
#'                       x.unit = "G",
#'                       Intensity = 4,
#'                       time.series = 2,
#'                       qValue = 2900,
#'                       origin = "winepr")
#'
#' }
#'
#'
#' @export
#'
#'
readEPR_Exp_Specs_kin <- function(name_root,
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
                                  colClasses = NULL,
                                  x = 2,
                                  x.unit = "G",
                                  Intensity = 4,
                                  time.series = 3,
                                  convertB.unit = TRUE,
                                  qValue = NULL,
                                  norm.vec.add = NULL,
                                  origin = "xenon") {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## file name_root which has to be the same for `ASC`+`DSC`/`dsc`
  ## or `.spc` and `.par`and corresponds to file name without extension
  #
  ## ================= Reading Files & Parameters ==================
  if (origin == "xenon") {
    ## path to `asc` file
    path.to.asc <- file.path(
      dir_ASC,
      paste0(name_root, ".txt")
    )
    #
    ## path to `DSC` or `par`
    path.to.dsc.par <- file.path(
      dir_dsc_par,
      paste0(name_root, "\\.(DSC|dsc)")
    )
    #
    ## Qvalue
    qValue.obtain <- readEPR_param_slct(path.to.dsc.par, string = "QValue")
  }
  if (origin == "winepr") {
    ## path to asc
    path.to.asc <- file.path(
      dir_ASC,
      paste0(name_root, ".asc")
    )
    #
    ## path to `par`
    path.to.dsc.par <- file.path(
      dir_dsc_par,
      paste0(name_root, ".par")
    )
    #
    ## Qvalue definition
    qValue.obtain <- qValue %>% `if`(is.null(qValue), 1, .)
  }
  #
  ## 'Kinetic' instrum. params
  instrument.params.kinet <- readEPR_params_slct_kin(path.to.dsc.par, origin = origin)
  #
  ## ================= Reading Data & Processing ==================
  #
  ## `Intensity` variable string
  IntensityString <- col.names[Intensity]
  #
  ## `time` variable string
  timeString <- col.names[time.series]
  #
  ## Load spectral data
  data.spectra.time <- readEPR_Exp_Specs(path.to.asc,
    col.names = col.names,
    colClasses = colClasses,
    x = x,
    Intensity = Intensity,
    time.series = time.series,
    convertB.unit = convertB.unit,
    qValue = qValue.obtain,
    norm.vec.add = norm.vec.add,
    origin = origin
  ) %>%
    dplyr::filter(.data[[IntensityString]] != 0) ## only non-zero intensities selected
  #
  ## recalculate  the time
  ## time var for `data.spectra.time`
  times <- data.spectra.time[[timeString]]
  #
  if (time.unit == "min") {
    times <- times * 60
  }
  if (time.unit == "h") {
    times <- times * 3600
  }
  ## Definition for `time.delta.slice.s`
  time.delta.slice.s <- time.delta.slice.s %>% `if`(is.null(time.delta.slice.s),1, .)
  #
  ## `time` if spectra are recorded as `slices` series
  if (time.unit == "unitless" & !is.null(time.delta.slice.s)) {
    times <- times * time.delta.slice.s
  }
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
  data.all.spectra <- list(df = data.spectra.time, time = time.corrected[[timeString]])
  #
  return(data.all.spectra)
  #
}
