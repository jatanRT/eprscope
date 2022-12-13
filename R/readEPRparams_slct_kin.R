#
#' Read the \strong{selected} Instrumental Parameters from \code{.DSC} or \code{.par} to Record
#'   the EPR Spectra (within the \code{Xenon} or \code{WinEpr} Software, respectively), \strong{
#'   required for time correction} (\code{\link{time_correct_EPRspecs}}) of the "kinetic" series
#'
#'
#' @description
#' tbc
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
#'   for time correction during the \code{2D_Field_Delay} experiment ("kinetic" series)
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#' @export
#'
#'
readEPRparams_slct_kin <- function(path_to_DSC_or_par,origin = "xenon"){
  ## Load all required parameters from `.DSC` or `.par`
  if (origin == "xenon"){
    resol <- readEPRparam_slct(path_to_DSC_or_par,string = "A1RS")
    convTime <- readEPRparam_slct(path_to_DSC_or_par,string = "SPTP")
    NScans <- readEPRparam_slct(path_to_DSC_or_par,string = "NbScansToDo")
    ## for kinetic measurements "AVGS" doesn't work
  }
  if (origin == "winepr"){
    resol <- readEPRparam_slct(path_to_DSC_or_par,string = "RES")
    convTime <- readEPRparam_slct(path_to_DSC_or_par,string = "RCT")
    NScans <- readEPRparam_slct(path_to_DSC_or_par,string = "JSD")
  }
  sweeptime <- resol*convTime
  #
  return(list(Nscans = NScans,sweepTime = sweeptime,Npoints = resol))
  #
}
