#
#' Read the Simulated ASCII (\code{.txt}) EPR Spectrum from \emph{MATLAB} and Transfer it into Data Frame
#'
#'
#' @description
#' tbc
#'
#'
#' @param path_to_ASC String, path to ASCII file/table (\code{.txt})
#'   with spectral data (\eqn{Intensity vs B}(Field) obtained from \emph{MATLAB})
#'
#' @return tbc
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#' @export
#'
#'
readEPR_Sim_Spec <- function(path_to_ASC){
  spectrum.data <- data.table::fread(path_to_ASC,sep = "auto",
                                     col.names = c("B_mT_Sim","dIepr_over_dB_Sim")) %>%
    dplyr::mutate(B_G_Sim = .data$B_mT_Sim*10)
  #
  return(spectrum.data)
 #
}
