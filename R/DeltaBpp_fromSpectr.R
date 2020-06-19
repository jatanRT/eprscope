#
#' @title Claculation of
#'
#'
#' @description Blah
#'
#'
#' @param spectrum.data
#' @param B.reg.start
#' @param B.reg.end
#'
#'
#' @return
#'
#'
#' @examples
#'
#'
#' @export
#'
#'
DeltaBpp_fromSpectr <- function(spectrum.data,B.reg.start,B.reg.end){
  ## B corresponding to minimum and maximum derivative intensities
  ## in the selected B region ('.reg.'):
  B.min <- spectrum.data %>% filter(between(.data$B_mT,B.reg.start,B.reg.end)) %>%
    filter(.data$dIepr_over_dB == min(.data$dIepr_over_dB)) %>% pull(.data$B_mT)
  B.max <- spectrum.data %>% filter(between(.data$B_mT,B.reg.start,B.reg.end)) %>%
    filter(.data$dIepr_over_dB == max(.data$dIepr_over_dB)) %>% pull(.data$B_mT)
  ## Delta_B calculation:
  DeltaB_pp <- abs(B.min - B.max)
  return(round(DeltaB_pp,digits = 2))
}
