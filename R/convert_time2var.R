#'
#' Convert Time (\eqn{t}) into Variable Linearly Depending on \eqn{t}
#'
#'
#' @family Conversions and Corrections
#'
#'
#' @description
#'   Conversion of time (\eqn{t}) into variable \code{var} (\eqn{var}) which is linearly changed upon time.
#'
#'
#' @details
#'  The linear time change of \eqn{var} can be expressed like
#'  \deqn{var = var0 + rate~ t}
#'  This is especially suitable for time conversion of EPR time series experiments
#'  (see e.g. \code{\link{readEPR_Exp_Specs_kin}}) simultaneously performed either during
#'  electrochemical/voltammetric or variable temperature experiment. When cyclic series experiment
#'  is performed (e.g. like cyclic voltammetry), that \eqn{var} value depends on switching one,
#'  like =>
#'  \deqn{var = var0 + rate~ t ~~ \text{for} ~~ t \leq t_{\text{switch}}}
#'  \deqn{var = var_{\text{switch}} - rate\, (t - t_{\text{switch}}) ~~ \text{for} ~~ t \geq t_{\text{switch}}}
#'  where the \eqn{t_{\text{switch}}} corresponding to \eqn{var_{\text{switch}}} are the quantities
#'  at the turning point and denoted by \code{t.switch} and \code{var.switch}, respectively.
#'
#'
#'
#' @param time.vals Numeric value or vector corresponding to time (points) where the variable \code{var}
#'   is changed.
#' @param time.unit Character string \strong{time unit} defined by `s`,`min` or `h`.
#'   \strong{Default}: \code{time.unit = "s"}.
#' @param var0 Numeric, the initial/starting value (\strong{INCL. ALSO NEGATIVE SIGN, if required}, e.g. like
#'   negative electrochemical potential) of the variable (\code{var}).
#' @param var.switch Numeric, the switching point \code{var} value, when a linear \strong{cyclic change}
#'   (or 'triangular ramp') of \code{var} upon time is applied (e.g. like cyclic voltammetry).
#'   \strong{Default}: \code{var.switch = NULL} (in case there is no such cyclic change).
#' @param var.rate Numeric, corresponding to rate of linear \code{var} change (\strong{INCL. ALSO NEGATIVE SIGN,
#'   if required}, e.g. like in case of electrochemical reduction or sample cooling).
#' @param var.rate.unit Character string corresponding to \code{var.rate} unit defined
#'   by following strings \code{"s^{-1}"} \eqn{\equiv \text{s}^{-1}},
#'   \code{"min^{-1}"} \eqn{\equiv \text{min}^{-1}} or \code{"h^{-1}"} \eqn{\equiv \text{h}^{-1}}.
#'   \strong{Default}: \code{var.rate.unit = "s^{-1}"}.
#'
#'
#' @return Numeric value or vector of the variable like e.g. electrochem. potential or temperature,
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
#' ## Create/Evaluate vector containing the applied
#' ## cell potential (in V) from the simultaneously
#' ## performed electrochemical oxidation experiment
#' ## (e.g. cyclic voltammetry from -0.1V to 0.45V and back
#' ## to -0.1V). Time series vector is labeled as "time_s".
#' time_s <- seq(0,360,by = 18)
#' E_V <- convert_time2var(time.vals = time_s,
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
convert_time2var <- function(time.vals,
                             time.unit = "s",
                             var0,
                             var.switch = NULL,
                             var.rate,
                             var.rate.unit = "s^{-1}") {
  #
  # convert time to `s` depending on `time.unit`
  if (time.unit == "min") {
    time.vals <- time.vals * 60
  }
  if (time.unit == "h") {
    time.vals <- time.vals * 3600
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
  ## Is there a cyclic change like potential in cyclic voltammetry
  ## or cyclic change of temperature...etc ?
  if (is.null(var.switch)) {
    ## If there is no cyclic change of `var` (variable)
    var <- var0 + var.rate * time.vals
  } else {
    #
    ## first calculate the switching time
    t.switch <- (var.switch - var0) / var.rate
    ## `var` calcul. depends whether the `time.vals` < `t.switch`
    ## or `time.vals` > `t.switch`(for the entire vector/column) =>
    #
    ## only one value
    if (length(time.vals) == 1){
      if (time.vals <= t.switch) {
        var <- var0 + var.rate * time.vals
      }
      if (time.vals > t.switch) {
        var <- var.switch - var.rate * (time.vals - t.switch)
      }
    }
    # time is a vector
    if (length(time.vals) > 1){
      ## start to create a vector
      var <- c()
      ## checking all the elements in a loop
      for (t in seq(time.vals)){
        if (time.vals[t] <= t.switch) {
          var[t] <- var0 + var.rate * time.vals[t]
        }
        if (time.vals[t] > t.switch) {
          var[t] <- var.switch - var.rate * (time.vals[t]-t.switch)
        }
      }
    }
  }
  #
  return(round(var,digits = 3))
  #
}
