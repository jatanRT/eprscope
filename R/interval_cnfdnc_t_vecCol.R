#
#' Calculate Confidence Interval of a Vector/Data Frame-Column According to Sudent's t-Distribution
#'
#'
#' @description tbc
#'
#'
#' @param data.vec.col tbc
#' @param level.cnfdnc tbc
#' @param lw.tail tbc
#' @param separate tbc
#'
#' @return tbc
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#' @export
#'
#' @importFrom stats qt sd
#' @importFrom errors set_errors
interval_cnfdnc_t_vecCol <- function(data.vec.col,
                                     level.cnfdnc = 0.95,
                                     lw.tail = TRUE,
                                     separate = FALSE){
  #
  ## alpha (significance level)
  level.sgnfcn <- 1-level.cnfdnc
  #
  ## number of experiments
  n.data <- length(data.vec.col)
  #
  ## `qt` value
  if (isTRUE(lw.tail)){
    qt.data <- stats::qt(p = 1-level.sgnfcn/2,df = n.data - 1,lower.tail = lw.tail)
  } else{
    qt.data <- stats::qt(p = level.sgnfcn/2,df = n.data - 1,lower.tail = lw.tail)
  }
  #
  ## uncertainty
  uncrt.data <- qt.data*(stats::sd(data.vec.col)/sqrt(n.data))
  #
  ## calculation
  if (isTRUE(separate)){
    value.cnfdnc <- list(value = mean(data.vec.col),uncertainty = uncrt.data)
  } else{
    value.cnfdnc <- errors::set_errors(mean(data.vec.col),uncrt.data)
  }
  return(value.cnfdnc)
  #
}
