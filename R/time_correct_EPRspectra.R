#
#' @title TODO
#'
#'
#' @description TODO
#'
#'
#' @param time TODO
#' @param N_scans TODO
#' @param sweep.time TODO
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
