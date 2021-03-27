## Calculate from DFT Gaussian Output

#' @title Calculation of \eqn{g}-factor from Quantum Chemical Computational Output (e.g. from Gaussian)
#'
#' @description TODO
#'
#' @param delta_g_vec Numeric vector of principal \code{g-value differences in 'ppm'}
#'   from \eqn{g_e} within e.g. \strong{Gaussian} or \strong{ORCA} output
#' @param mean Boolean, whether to calculated the \code{mean value} of \code{delta_g_vec} elements,
#'   \strong{default}: \code{mean = TRUE}, or save the entire vector with all principal components
#'
#' @return TODO
#'
#'
#' @examples
#' gValue_QCHcomp(c(-806.1,1099.9,1417.1))
#' gValue_QCHcomp(c(-543.1,832.8,1089.5),mean = FALSE)
#'
#'
#' @export
#'
#'
gValue_QCHcomp <- function(delta_g_vec,mean = TRUE){
  ## g-factor for free electron (g.e) from `constants` package
  ## round the g.e to 6 decimal places
  g.e <- round(-constants::syms$gem,digits = 6)
  ## g-vector from shifts (`deltas`) and g.e
  delta_g_vec <- as.vector(delta_g_vec)
  g_vec <- g.e + delta_g_vec*1e-6
  ## whether to calculate the mean value or get the g-vector
  ## as it is
  if (isTRUE(mean)){
    gValue <-  mean(g_vec)
  } else{
    gValue <- g_vec
  }
  return(gValue)
}
