#
#' Correction for EPR spectral Time Series
#'
#'
#' @description
#' Providing the Accurate Time for EPR Spectral Line Appearance
#'
#'  The time is recorded according to the following scheme:
#'
#'  \tabular{cccccc}{
#'
#'            \tab     EPR Spectr.     \tab                   \tab     \tab             \tab   EPR Spectr. \cr
#'
#'  \code{t[1]} \tab  ------^v------> \tab \code{t[2]}-delay \tab ---->  \tab \code{t[2]} \tab ------^v-------> ... \cr
#'
#'         \tab  \code{N_scans}*\code{swt}  \tab          \tab \code{delay} \tab        \tab \code{N_scans}*\code{swt}
#' }
#' The recorded times are: \code{t[1]},\code{t[2]},\code{t[3]},...
#' and \code{N_scans} corresponds to \code{number of scans} and \code{swt} to \code{sweep time} for individual scan
#'
#'
#' @param time.s Vector/Column in \code{data frame} corresponding to \code{time} (in \code{s}) at which
#'   the individual EPR spectra were recorded
#' @param Nscans Numeric, number of accumulation (number of scans \code{AVGS}) for each spectrum
#'   in EPR time series
#' @param sweep.time.s Numeric, time (in \code{s}) for recording of one EPR spectrum \eqn{=} one accumulation
#'
#' @return Numeric value/vector corresponding to accurate time at which the EPR spectra were recorded during
#'   the kinetic measurements (e.g. like radical stability, electrochemical and/or photochemical measurements)
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#'
#' @export
#'
#'
## when the spectrum is recorded, this can be estimated by the time
## of the spectrum middle point. The time (`time.s`) however also depends
## on the number of scans (`N_scans`) and sweep time (`sweep.time.s`) for each
## spectrum in the kinetic series. Time converted into seconds
time_correct_EPR_Specs  <-  function(time.s,Nscans,sweep.time.s){
  #
  if(Nscans == 0){
    #
    return(round(time.s + sweep.time.s*Nscans + sweep.time.s/2))
    #
  } else{
    #
    return(round(time.s + sweep.time.s*(Nscans - 1) + sweep.time.s/2))
    #
  }
}
