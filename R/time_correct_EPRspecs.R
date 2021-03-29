#
#' @title Correction for EPR spectral Time Series Providing the Accurate Time for EPR Spectral Line Appearance
#'
#'
#' @description TODO
#'
#'  The time is recorded according to the following scheme:
#'
#'  \tabular{cccccc}{
#'
#'            \tab     EPR Spectr.     \tab                   \tab     \tab             \tab   EPR Spectr. \cr
#'
#'  \code{t[1]} \tab  ------^v------> \tab \code{t[2]}-delay \tab ---->  \tab \code{t[2]} \tab ------^v-------> ... \cr
#'
#'         \tab  \code{N_scans}x\code{swt}  \tab          \tab \code{delay} \tab        \tab \code{N_scans}x\code{swt}
#' }
#' The recorded times are: \code{t[1]},\code{t[2]},\code{t[3]},...
#' and \code{N_scans} corresponds to \code{number of scans} and \code{swt} to \code{sweep time} for individual scan
#'
#'
#' @param time Vector/Column in \code{data frame} corresponding to \code{time} at which
#'   the individual EPR spectra were recorded
#' @param N_scans Numeric, number of accumulation (number of scans \code{AVGS}) for each spectrum
#'   in EPR time series
#' @param sweep.time Numeric, time for recording of one EPR spectrum \eqn{=} one accumulation
#'
#' @return Numeric value/vector corresponding to accurate time at which the EPR spectra were recorded during
#'   the kinetic measurements (e.g. like radical stability, electrochemical and/or photochemical measurements)
#'
#'
#' @examples
#' TODO
#' TODO
#'
#'
#' @export
#'
#'
## when the spectrum is recorded, this can be estimated by the time
## of the spectrum middle point. The time (`time`) however also depends
## on the number of scans (`N_scans`) and sweep time (`sweep.time`) for each
## spectrum in the kinetic series. Time converted into seconds
time_correct_EPRspecs  <-  function(time,N_scans,sweep.time){
  #
  if(N_scans == 0){
    #
    return(round(time + sweep.time*N_scans + sweep.time/2))
    #
  } else{
    #
    return(round(Time + sweep.time*(N_scans - 1) + sweep.time/2))
    #
  }
}
