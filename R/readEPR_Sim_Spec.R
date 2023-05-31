#
#' Read the Simulated ASCII EPR Spectrum from \emph{MATLAB/EasySpin}
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Loading the raw `.txt` data of a simulated EPR spectrum from \emph{MATLAB/EasySpin}. The `ASCII` data (\code{.txt})
#'   is automatically converted into data frame.
#'
#'
#' @param path_to_ASC Character string, path to ASCII file/table (\code{.txt})
#'   with spectral data (\eqn{Intensity vs B}(Field) obtained from \emph{MATLAB}).
#'   The path can be also defined by \code{\link[base]{file.path}}
#' @param B.unit Character string pointing to unit of magnetic flux density (coming from original data, see also
#'   \code{column.names} parameter) which is to be presented on the \eqn{B} abscissa of an EPR spectrum,
#'   like \code{"G"} (`Gauss`), \code{"mT"} (`millitesla`). \strong{Default}: \code{B.unit = "mT"}
#' @param col.names Character string vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its unit => \code{Quantity_Unit}, e.g. like \code{"Bsim_G"} or \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum abscissa). \strong{Default}: \code{col.names = c("Bsim_mT","dIeprSim_over_dB")}.
#'   \strong{Though one can choose an arbitrary intensity column name of the simulated spectrum} (the \strong{default} name
#'   is \code{dIeprSim_over_dB}), the additional \strong{processing of the data/spectrum
#'   either by} \code{\link{presentEPR_Sim_Spec}} \strong{or by} \code{\link{quantify_EPR_sim}} \strong{require
#'   that the corresponding names have to be changed accordingly}.
#'
#' @return Data frame (consisting of magnetic flux density and intensity variables/columns) of a simulated
#'   spectrum from \emph{MATLAB/EasySpin}.
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#'
#' @export
#'
#'
readEPR_Sim_Spec <- function(path_to_ASC,
                             B.unit = "mT",
                             col.names = c(
                               "Bsim_mT",
                               "dIeprSim_over_dB"
                             )) {
  #
  ## 'Temporary' processing variables
  Bsim_G <- NULL
  Bsim_mT <- NULL
  #
  spectrum.data <- data.table::fread(file = path_to_ASC,
    sep = "auto",
    col.names = col.names
  )
  #
  ## x for spectrum data
  x <- col.names[1]
  #
  if (B.unit == "mT") {
    spectrum.data <- spectrum.data %>%
      dplyr::mutate(Bsim_G = .data[[x]] * 10) %>%
      ## reordering columns
      dplyr::select(Bsim_G, .data[[col.names[1]]], .data[[col.names[2]]])
  }
  if (B.unit == "G") {
    spectrum.data <- spectrum.data %>%
      dplyr::mutate(Bsim_mT = .data[[x]] / 10) %>%
      ## reordering columns
      dplyr::select(.data[[col.names[1]]], Bsim_mT, .data[[col.names[2]]])
  }
  #
  return(spectrum.data)
  #
}
