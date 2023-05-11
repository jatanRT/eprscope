#
#' Read the \strong{Selected} Instrumental Parameters Relevant to \strong{Time Series} Experiment
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'  Function takes the \strong{selected} instrumental parameters relevant to \strong{time series ("kinetic")}
#'  experiment from \code{.DSC} or \code{.par} file of an EPR Spectrum (written by the `Xenon`
#'  or `WinEpr` software, respectively). These parameters are required for the time correction of EPR
#'  spectra, see \code{\link{correct_time_Exp_Specs}}
#'
#'
#' @param path_to_DSC_or_par String, path to \code{.DSC} or \code{.par} file including all instrumental
#'   parameters provided by the EPR machine
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the files are slightly different depending on whether they were recorded
#'   by the windows based softw. ("WinEpr",\code{origin = "winepr"}) or by the Linux one ("Xenon"),
#'   \strong{default}: \code{origin = "xenon"}
#'
#'
#' @return List containing number of scans (\code{Nscans}) and sweep time (\code{sweepTime}) required
#'   for time correction during the \code{2D_Field_Delay} experiment ("kinetic" time series)
#'
#'
#' @examples
#' \dontrun{
#' readEPR_params_slct_kin(path_to_DSC_or_par)
#' readEPR_params_slct_kin(file.path(".",
#'                                   "dir_par",
#'                                   "EPR_spectrum.par"),
#'                         origin = "winepr")
#' }
#'
#' @export
#'
#'
readEPR_params_slct_kin <- function(path_to_DSC_or_par,origin = "xenon"){
  ## Load all required parameters from `.DSC` or `.par`
  if (origin == "xenon"){
    resol <- readEPR_param_slct(path_to_DSC_or_par,
                                string = "A1RS",
                                origin = origin)
    convTime <- readEPR_param_slct(path_to_DSC_or_par,
                                   string = "SPTP",
                                   origin = origin)
    NScans <- readEPR_param_slct(path_to_DSC_or_par,
                                 string = "NbScansToDo",
                                 origin = origin)
    ## for kinetic measurements "AVGS" doesn't work, therefore select "NbScansToDo"
  }
  if (origin == "winepr"){
    resol <- readEPR_param_slct(path_to_DSC_or_par,
                                string = "RES",
                                origin = origin)
    convTime <- readEPR_param_slct(path_to_DSC_or_par,
                                   string = "RCT",
                                   origin = origin)
    NScans <- readEPR_param_slct(path_to_DSC_or_par,
                                 string = "JSD",
                                 origin = origin)
  }
  sweeptime <- resol*convTime
  #
  return(list(Nscans = NScans,sweepTime = sweeptime,Npoints = resol))
  #
}
