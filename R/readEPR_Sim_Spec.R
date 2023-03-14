#
#' Read the Simulated ASCII EPR Spectrum from \emph{MATLAB}
#'
#'
#' @description
#' (\code{.txt}) and Transfer it into Data Frame
#'
#'
#' @param path_to_ASC String, path to ASCII file/table (\code{.txt})
#'   with spectral data (\eqn{Intensity vs B}(Field) obtained from \emph{MATLAB}).
#'   The path can be also defined by \code{\link[base]{file.path}}
#' @param B.unit Character/String pointing to unit of quantity (coming from original ASCII data, see also
#'   \code{column.names} parameter) which is to be presented on \eqn{B} abscissa of the EPR spectrum,
#'   like \code{"G"} (`Gauss`), \code{"mT"} (`millitesla`). \strong{Default}: \code{B.unit = "mT"}
#' @param col.names Character/String vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its units, \code{Quantity_Unit} like \code{"Bsim_G"} or \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum abscissa)...etc, \strong{default}: \code{col.names = c("Bsim_mT","dIeprSim_over_dB")}.
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
readEPR_Sim_Spec <- function(path_to_ASC,
                             B.unit = "mT",
                             col.names = c("Bsim_mT","dIeprSim_over_dB")){
  #
  spectrum.data <- data.table::fread(path_to_ASC,
                                     sep = "auto",
                                     col.names = col.names)
  #
  ## x for spectrum data
  x = col.names[1]
  #
  if (B.unit == "mT"){
    spectrum.data <- spectrum.data %>%
      dplyr::mutate(Bsim_G = .data[[x]]*10)
  }
  if (B.unit == "G"){
    spectrum.data <- spectrum.data %>%
      dplyr::mutate(Bsim_mT = .data[[x]]/10)
  }
  #
  return(spectrum.data)
 #
}
