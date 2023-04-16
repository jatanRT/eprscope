#'
#' Convert Time into Variable Linearly Changing on Time
#'
#'
#' @description
#' Conversion of time into variable \code{var} which is linearly changed upon time like
#' \deqn{var = var0 + rate\times t}
#' This is especially suitable for time conversion of EPR time series experiments
#' (see e.g. \code{\link{readEPR_Exp_Specs_kin}}) simultaneously performed either during
#' electrochemical/voltammetric or variable temperature experiment.
#'
#'
#' @param time Numeric, value or vector corresponding to time (points) upon which the variable \code{var}
#'   is changed.
#' @param time.unit Character/String \strong{time unit} defined by \code{"s"},\code{"min"} or \code{"h"}.
#'   \strong{Default}: \code{time.unit = "s"}
#' @param var0 Numeric, the initial/starting value (\strong{INCL. ALSO NEGATIVE SIGN, if required}, e.g. like
#'   negative electrochemical potential) of variable (\code{var}) from which the linear \code{var} change
#'   is taking place.
#' @param var.switch Numeric, the switching value, when a linear \strong{cyclic change} (or 'triangular ramp')
#'   of \code{var} upon time is applied/happening (e.g. like cyclic voltammetry).
#'   \strong{Default}: \code{var.switch = NULL} (in case there is no cyclic change).
#' @param var.rate Numeric, corresponding to rate of linear \code{var} change (\strong{INCL. ALSO NEGATIVE SIGN,
#'   if required}, e.g. like in case of electrochemical reduction or sample cooling).
#'
#'
#' @return Numeric value or vector of variable (e.g. like electrochem. potential or temperature...etc)
#'   linearly dependent upon time.
#'
#'
#' @examples
#' ## Calculate Potential after 30 s, starting from 200 mV
#' ## (vs Reference Electrode) into cathodic direction
#' ## (reduction) by 5 mV/s
#' convert_time2var(30,var0 = 0.2,var.rate = - 0.005)
#' #
#' ## Heating sample after 5 min starting from 293 K
#' ## by the temperature rate of 0.5 K/s
#' convert_time2var(5,
#'                  time.unit = "min",
#'                  var0 = 293,
#'                  var.rate = 0.5)
#'
#'
#' @export
#'
#'
convert_time2var <- function(time,
                             time.unit = "s",
                             var0,
                             var.switch = NULL,
                             var.rate) {
  #
  # convert time to `s` depending on `time.unit`
  if (time.unit == "min") {
    time <- time * 60
  }
  if (time.unit == "h") {
    time <- time * 3600
  }
  #
  ## If there is no cyclic change of `var` (variable)
  ## or there is cyclic change like cyclic voltammetry
  ## or cyclic change of temperature...etc
  if (is.null(var.switch)) {
    var <- var0 + var.rate * time
  } else {
    #
    ## first calculate the switching time
    t.switch <- (var.switch - var0) / var.rate
    ## finding the `t.switch` in input data `time`
    t.switch.data <- which.min(abs(time - t.switch))
    ## `var` calcul. depends whether the `time` < `t.switch.data`
    ## or `time` > `t.switch.data` =>
    if (time < t.switch.data) {
      var <- var0 + var.rate * time
    }
    if (time > t.switch.data) {
      var <- var0 - var.rate * time
    }
  }
  #
  return(var)
  #
}
