#
#' Read the Simulated ASCII EPR Spectrum
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Loading the raw `.txt`,`.asc` or `.csv` data of a simulated EPR spectrum from different sources like
#'   `EasySpin` (Matlab), `Xenon` (EPR spectrometer), `SimFonia` (WinEPR system) or `csv` (universal format
#'   or MS Excel). The `ASCII` data is automatically converted into data frame.
#'
#'
#' @param path_to_ASC Character string, path to ASCII file/table
#'   with simulated spectral data (\eqn{Intensity vs B}(Field) obtained from various sources.
#'   The path can be also defined by \code{\link[base]{file.path}}.
#' @param B.unit Character string pointing to unit of magnetic flux density (\strong{coming from the original data},
#'   see also \code{column.names} parameter) which is to be presented on the \eqn{B} abscissa of an EPR spectrum,
#'   like \code{"G"} (`Gauss`), \code{"mT"} (`millitesla`). \strong{Default}: \code{B.unit = "mT"}.
#' @param Intensity.sim Character string
#' @param sim.origin Character string vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its unit => \code{Quantity_Unit}, e.g. like \code{"Bsim_G"} or \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum abscissa). \strong{Default}: \code{col.names = c("Bsim_mT","dIeprSim_over_dB")}.
#'   \strong{Though one can choose an arbitrary intensity column name of the simulated spectrum} (the \strong{default} name
#'   is \code{dIeprSim_over_dB}), the additional \strong{processing of the data/spectrum
#'   either by} \code{\link{presentEPR_Sim_Spec}} \strong{or by} \code{\link{quantify_EPR_sim}} \strong{require
#'   that the corresponding names have to be changed accordingly}.
#'
#' @return Data frame consisting of magnetic flux density and intensity variables/columns corresponding
#'   simulated to simulated EPR spectrum.
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
                             Intensity.sim = "dIeprSim_over_dB",
                             sim.origin = "easyspin", ## add "xenon" and "simfonia" as well as "csv"
                             ) {
  #
  ## 'Temporary' processing variables
  Bsim_G <- NULL
  Bsim_mT <- NULL
  #
  ## x-axis/column for B + y-axis/column for intensity
  x.col.string <- paste0("Bsim_",B.unit)
  y.col.string <- Intensity.sim
  #
  ## Conditions depending on "origin"
  if (sim.origin == "easyspin"){
    spectrum.data <- data.table::fread(file = path_to_ASC,
                                       sep = "auto",
                                       col.names = c(x.col.string,
                                                     y.col.string)
    )
  }
  if (sim.origin == "xenon"){
    spectrum.data <- data.table::fread(file = path_to_ASC,
                                       sep = "auto",
                                       header = FALSE,
                                       select = c(2,3),
                                       col.names = c(x.col.string,
                                                     y.col.string))
  }
  if (sim.origin == "simfonia"){
    ## There are two file types 'txt' and 'asc' therefore
    ## they have to differentiated
    ## patterns
    simf.data.file <- readLines(path_to_ASC)
    simf.data.pattern.read.01 <- unlist(stringr::str_split(simf.data.file[5],
                                                           pattern = "[[:space:]]+",
                                                           n = 5))
    simf.data.pattern.read.02 <- unlist(stringr::str_split(simf.data.file[6],
                                                           pattern = "---"))
    ## conditions
    simf.data.condition.01 <- grepl("Data",simf.data.pattern.read.01[2])
    simf.data.condition.02 <- grepl("--",simf.data.pattern.read.02[length(simf.data.pattern.read.02)])
    #
    ## the `sim.data.file` is not required anymore
    rm(simf.data.file)
    #
    if (isTRUE(simf.data.condition.01) & isTRUE(simf.data.condition.02)){
      ## reading simfonia '.txt'
      spectrum.data <- data.table::fread(file = path_to_ASC,
                                         sep = "auto",
                                         header = FALSE,
                                         select = c(2,3),
                                         col.names = c(x.col.string,
                                                       y.col.string))
    } else{
      ## reading simfonia '.asc'
      spectrum.data <- data.table::fread(file = path_to_ASC,
                                         sep = "auto",
                                         header = TRUE,
                                         col.names = c(x.col.string,
                                                       y.col.string))
    }
   #
  }
  #
  if (sim.origin == "csv"){
    spectrum.data <- data.table::fread(file = path_to_ASC,
                                       sep = "auto",
                                       header = TRUE,
                                       col.names = c(x.col.string,
                                                     y.col.string))
  }
  ## Adding additional `B` column with corresponding units depending
  ## on `B.unit`
  if (B.unit == "mT") {
    spectrum.data <- spectrum.data %>%
      dplyr::mutate(Bsim_G = .data[[x.col.string]] * 10) %>%
      ## reordering columns
      dplyr::select(Bsim_G, .data[[x.col.string]], .data[[y.col.string]])
  }
  if (B.unit == "G") {
    spectrum.data <- spectrum.data %>%
      dplyr::mutate(Bsim_mT = .data[[x.col.string]] / 10) %>%
      ## reordering columns
      dplyr::select(Bsim_mT, .data[[x.col.string]], .data[[y.col.string]])
  }
  #
  return(spectrum.data)
  #
}
