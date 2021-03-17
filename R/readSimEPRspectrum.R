#
#' @title Read the Simulated ASCII (\code{.txt}) EPR Spectrum from from \emph{MATLAB} and Transfers it into Data Frame
#'
#'
#' @description TODO
#'
#'
#' @param path_to_ASC String, path to ASCII file/table (\code{.txt})
#'   with spectral data (\eqn{Intensity vs B}(Field) obtained from \emph{MATLAB})
#'
#' @return TODO
#'
#'
#' @examples
#' TODO
#' TODO
#'
#' @export
#'
#'
readSimEPRspectrum <- function(path_to_ASC){
  spectrum.data <- data.table::fread(path_to_ASC,sep = "auto",
                                     col.names = c("B_mT_Sim","dIepr_over_dB_Sim")) %>%
    dplyr::mutate(B_G_Sim = .data$B_mT_Sim*10)
  return(spectrum.data)
}
