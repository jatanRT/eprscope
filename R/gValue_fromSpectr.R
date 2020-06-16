#
#' @title
#'
#' @description
#'
#' @param spectrum.data
#' @param nu
#' @param B.reg.start
#' @param B.reg.end
#'
#' @return
#' @export
#'
#' @examples
gValue_fromSpectr <- function(spectrum.data,nu,B.reg.start,B.reg.end){
  ## B at minimum of dIepr_over_dB:
  B.min <- spectrum.data %>% dplyr::filter(dplyr::between(.data$B_mT,B.reg.start,B.reg.end)) %>%
    dplyr::filter(.data$dIepr_over_dB == min(.data$dIepr_over_dB)) %>% dplyr::pull(.data$B_mT)
  ## B at maximum of dIepr_over_dB:
  B.max <- spectrum.data %>% dplyr::filter(dplyr::between(.data$B_mT,B.reg.start,B.reg.end)) %>%
    dplyr::filter(.data$dIepr_over_dB == max(.data$dIepr_over_dB)) %>% dplyr::pull(.data$B_mT)
  ## B between both of them:
  B.center <- (B.min+B.max)/2
  ## g -value calculation:
  Planck.const <- constants::syms$h
  Bohr.magnet <- constants::syms$muB
  g <- (Planck.const*nu*1e+9)/(Bohr.magnet*B.center*0.001)
  return(round(g,digits = 5))
}
