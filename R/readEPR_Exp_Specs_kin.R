#
#' Read Spectral Data of Time Dependent Experiments (e.g. like Kinetics)
#'
#'
#' @description The function reads the EPR time series spectral data (recorded by e.g. `2D_Field_Delay Experiment`
#'  in "Xenon" acquisition/processing software)
#'
#'
#' @param file.rootname String/Character corresponding to `file name` without `extension`
#' @param dir_ASC path (defined by \code{\link[base]{file.path}}, String/Character) to directory where
#'  the `ascii` file is stored
#' @param dir_DSC_or_par path (defined by \code{\link[base]{file.path}} String/Character) to directory
#'  where the file (`.DSC` or `.par`) with instrumental parameters (to calculate \eqn{g}-value
#'  or normalize intensities) is stored
#' @param origin String/Character corresponding to \strong{software} used to acquire the EPR spectra
#'   on BRUKER spectrometers, i.e. whether they were recorded by the windows based softw. ("WinEpr",
#'   \code{origin = "winepr"}) or by the Linux one ("Xenon"), \strong{default}: \code{origin = "xenon"}
#' @param qValue Numeric, Q value (quality factor, number) displayed at specific \code{dB} by spectrometer.
#'   In case of "Xenon" software the parameter is included in \code{.DSC} file, therefore \strong{default}:
#'   \code{qValue = NULL}. If EPR spectra were acquired by the "Winepr" software Q value must be defined
#'   like \code{qValue = 3400}
#'
#'
#' @return List of spectral data (incl. time) in tidy long table format (`data`) + corrected time vector (`time`)
#'
#'
#' @examples
#' \dontrun{
#' ## Reading by the "Xenon" software
#' readEPR_Exp_Specs_kin("Sample_spectra_irradiation",
#'                       file.path(".","ASCII_data_dir"),
#'                       file.path(".","DSC_data_dir"),
#'                       origin = "xenon")
#'
#' ## Reading by the "WinEPR" software
#' readEPR_Exp_Specs_kin("Sample_spectra_irradiation",
#'                       file.path(".","ASCII_data_dir"),
#'                       file.path(".","DSC_data_dir"),
#'                       origin = "winepr",
#'                       qValue = 2900)
#'
#' }
#'
#'
#' @export
#'
#'
readEPR_Exp_Specs_kin <- function(file.rootname,
                                  dir_ASC,
                                  dir_DSC_or_par,
                                  origin = "xenon",
                                  qValue = NULL){
  #
  ## file rootname which has to be the same for `ASC`+`DSC`
  ## or `.spc` and `.par`and corresponds to file name without extension
  #
  if (origin == "xenon"){
    ## path to `asc` file
    path.to.asc <- file.path(dir_ASC,
                             paste0(file.rootname,".txt"))
    #
    ## path to `DSC` or `par`
    path.to.dsc.par <- file.path(dir_DSC_or_par,
                                 paste0(file.rootname,".DSC"))
    #
    ## Qvalue
    qValue.obtain <- readEPR_param_slct(path.to.dsc.par,string = "QValue")
  }
  if (origin == "winepr"){
    ## path to asc
    path.to.asc <- file.path(dir_ASC,
                             paste0(file.rootname,".asc"))
    #
    ## path to `par`
    path.to.dsc.par <- file.path(dir_DSC_or_par,
                                 paste0(file.rootname,".par"))
    #
    ## to obtain `QValue` run the following
    if (is.null(qValue)){
      stop(" 'qValue' is not provided. Please, define! ")
    } else{
      qValue.obtain <- qValue
    }
  }
  #
  ## 'Kinetic' instrum. params
  instrument.params.kinet <- readEPR_params_slct_kin(path.to.dsc.par,origin = origin)
  #
  ## Load spectral data
  data.spectra.time <- readEPR_Exp_Specs(path.to.asc,
                                         qValue = qValue.obtain,
                                         time.series = T,
                                         origin = origin) %>%
    dplyr::filter(.data$dIepr_over_dB != 0) ## only non-zero intensities selected
    #
    ## recalculate  time
    data.spectra.time$time_s = time_correct_EPR_Specs(time.s = data.spectra.time$time_s,
                                                      Nscans = instrument.params.kinet$Nscans,
                                                      sweep.time.s = instrument.params.kinet$sweepTime)
  #
  ## corrected time
  time.corrected <- data.spectra.time %>%
    dplyr::group_by(.data$time_s) %>%
    dplyr::group_keys()
  #
  data.all.spectra <- list(data = data.spectra.time,time = time.corrected$time_s)
  #
  return(data.all.spectra)
  #
}
