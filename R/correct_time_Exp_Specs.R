#
#' Time Correction for the Experimental CW EPR Spectral Time Series.
#'
#'
#' @family Conversions and Corrections
#'
#'
#' @description
#'  Providing the accurate time for EPR spectral line/spectrum appearance, i.e usually the middle
#'  \eqn{B} of the EPR spectrum which is is set up as the CF (\code{central field}) for the spectrum sweep.
#'
#'
#' @details
#'  The actual time at the middle/crossing point
#'  is different from that recorder by the EPR acquisition software, see below. This is especially important
#'  in determining the kinetics of radical generation or decay.
#'  The time is recorded according to the following scheme where "^v" in the scheme denotes the derivative form
#'  of an EPR spectrum:
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
#' and the \code{N_scans} corresponds to \code{number of scans}, \code{swt} to \code{sweep time} for the individual scan.
#' These parameters can be obtained by the \code{\link{readEPR_params_slct_kin}} or other functions which can read
#' the instrumental parameters.
#'
#'
#' @param time.s Numeric value/vector/column in \code{data frame} corresponding to \code{time} (in \code{s})
#'   at which the individual EPR spectra were recorded (supplied by the EPR acquisition software).
#' @param Nscans Numeric, number of accumulations (number of scans usually denoted as \code{AVGS})
#'   for each spectrum in EPR time series.
#' @param sweep.time.s Numeric, time (in \code{s}) for recording individual EPR spectrum \eqn{=}
#'   one "accumulation".
#'
#'
#' @return Numeric value/vector corresponding to accurate time at which the middle (on \eqn{B} abscissa) EPR spectra
#'   were recorded during the kinetic measurements (e.g. like radical formation, stability, electrochemical
#'   and/or photochemical measurements).
#'
#'
#' @examples
#' ## 12 s recorded by spectrometer, 6 accumulations by the sweep time of 6 s
#' correct_time_Exp_Specs(12,Nscans = 6,6)
#'
#'
#' @export
#'
#'
## when a CW EPR spectrum is recorded, this can be estimated by the time
## of the spectrum middle point. Usually, the middle of an EPR signal
## is set as a `central field` (CF). The time (`time.s`) however also depends
## on the number of scans (`N_scans`) and sweep time (`sweep.time.s`) for each
## spectrum in the kinetic series. Time converted into seconds
correct_time_Exp_Specs <- function(time.s,
                                   Nscans,
                                   sweep.time.s) {
  #
  if (Nscans == 0) {
    #
    return(round(time.s + sweep.time.s * Nscans + sweep.time.s / 2))
    ## It corresponds to `time.s + sweep.time.s/2`, however
    ## the formula is written in such form in order to be consistent
    ## with that presented below
    #
  } else {
    #
    return(round(time.s + sweep.time.s * (Nscans - 1) + sweep.time.s / 2))
    #
  }
}
