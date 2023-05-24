#'
#' Convert Time (\eqn{t}) into Variable Linearly Depending on \eqn{t}
#'
#'
#' @family Conversions and Corrections
#'
#'
#' @description
#'   Conversion of time into variable \code{var} which is linearly changed upon time like
#'   \deqn{var = var0 + rate\times t}
#'   This is especially suitable for time conversion of EPR time series experiments
#'   (see e.g. \code{\link{readEPR_Exp_Specs_kin}}) simultaneously performed either during
#'   electrochemical/voltammetric or variable temperature experiment. If cyclic series experiment is performed
#'   (e.g. like cyclic voltammetry)...tbc
#'
#'
#' @param time Numeric, value or vector corresponding to time (points) where the variable \code{var}
#'   is changed.
#' @param time.unit Character string \strong{time unit} defined by \code{"s"},\code{"min"} or \code{"h"}.
#'   \strong{Default}: \code{time.unit = "s"}.
#' @param var0 Numeric, the initial/starting value (\strong{INCL. ALSO NEGATIVE SIGN, if required}, e.g. like
#'   negative electrochemical potential) of the variable (\code{var}).
#' @param var.switch Numeric, the switching point \code{var} value, when a linear \strong{cyclic change}
#'   (or 'triangular ramp') of \code{var} upon time is applied (e.g. like cyclic voltammetry).
#'   \strong{Default}: \code{var.switch = NULL} (in case there is no such cyclic change).
#' @param var.rate Numeric, corresponding to rate of linear \code{var} change (\strong{INCL. ALSO NEGATIVE SIGN,
#'   if required}, e.g. like in case of electrochemical reduction or sample cooling).
#' @param var.rate.unit Character string ...tbc...defined by \code{"s^{-1}"},\code{"min^{-1}"} or \code{"h^{-1}"}
#'
#'
#' @return Numeric value or vector of variable (e.g. like electrochem. potential or temperature...etc)
#'   linearly changing upon time.
#'
#'
#' @examples
#' ## Calculate Potential after 30 s, starting from 200 mV
#' ## into cathodic direction (reduction) by 5 mV s^{-1}
#' convert_time2var(30,var0 = 0.2,var.rate = - 0.005)
#' #
#' ## Heating sample after 5 min starting from 293 K
#' ## by the temperature rate of 4 K min^{-1}
#' convert_time2var(5,
#'                  time.unit = "min",
#'                  var0 = 293,
#'                  var.rate = 4,
#'                  var.rate.unit = "min^{-1}")
#' #
#' ## Create/Evaluate vector containing the applied cell potential (in V)
#' ## from the simultaneously performed electrochemical oxidation
#' ## experiment (e.g. cyclic voltammetry from -0.1V to 0.45V and back to -0.1V).
#' ## Time series vector is labeled as "time_s"
#' time_s <- seq(0,360,by = 18)
#' E_V <- convert_time2var(time = time_s,
#'                         var0 = -0.1,
#'                         var.switch = 0.45,
#'                         var.rate = 0.003)
#' as.matrix(E_V)
#'
#'
#'
#' @export
#'
#'
convert_time2var <- function(time,
                             time.unit = "s",
                             var0,
                             var.switch = NULL,
                             var.rate,
                             var.rate.unit = "s^{-1}") {
  #
  # convert time to `s` depending on `time.unit`
  if (time.unit == "min") {
    time <- time * 60
  }
  if (time.unit == "h") {
    time <- time * 3600
  }
  #
  ## convert rate if other than s^{-1}
  if (var.rate.unit == "min^{-1}"){
    var.rate <- var.rate / 60
  }
  if (var.rate.unit == "h^{-1}"){
    var.rate <- var.rate / 3600
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
    ## `var` calcul. depends whether the `time` < `t.switch`
    ## or `time` > `t.switch`(for the entire vector/column) =>
    #
    ## only one value
    if (length(time) == 1){
      if (time <= t.switch) {
        var <- var0 + var.rate * time
      }
      if (time > t.switch) {
        var <- var.switch - var.rate * (time-t.switch)
      }
    }
    # time is a vector
    if (length(time) > 1){
      var <- c()
      for (t in seq(time)){
        if (time[t] <= t.switch) {
          var[t] <- var0 + var.rate * time[t]
        }
        if (time[t] > t.switch) {
          var[t] <- var.switch - var.rate * (time[t]-t.switch)
        }
      }
    }
  }
  #
  return(round(var,digits = 3))
  #
}
