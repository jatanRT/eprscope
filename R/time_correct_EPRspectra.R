#
#' @title Correction for EPR spectral Time Series Providing the Accurate Time for EPR Spectral Line Appearance
#'
#'
#' @description TODO
#'
#'  The time is recorded according to the following scheme:
#'
#'      EPR Spectrum                        EPR Spectrum                        EPR Spectrum
#'  t[1] ----^v----> t[2]-delay ------> t[2] ----^v----> t[3]-delay ------> t[3] ----^v----> ...
#'       N_scans*swt            +delay       N_scans*swt            +delay       N_scans*swt
#'
#' The recorded times are: t[1],t[2],t[3],...
#' and `N_scans` corresponds to `number of scans` and `swt` to `sweep time` for individual scan
#'
#'
#' @param time Vector/Column in \code{data frame} corresponding to \code{time} at which
#'   the individual EPR spectra were recorded
#' @param N_scans Numeric, number of accumulation (number of scans \code{AVGS}) for each spectrum
#'   in EPR time series
#' @param sweep.time Numeric, time for recording of one EPR spectrum \eqn{=} one accumulation
#'
#' @return TODO
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
time_correct_EPRspectra  <-  function(time,N_scans,sweep.time){
  if(N_scans == 0){
    return(round(time + sweep.time*N_scans + sweep.time/2))
  } else{
    return(round(Time + sweep.time*(N_scans - 1) + sweep.time/2))
  }
}
