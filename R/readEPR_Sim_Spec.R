#
#' Read the Simulated ASCII EPR Spectrum
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Loading the ASCII data like \code{.txt},\code{.asc} or \code{.csv}, related to simulated EPR spectrum from different
#'   sources like "EasySpin" (\emph{Matlab}), "Xenon" (EPR spectrometer), "SimFonia" (WinEPR system)
#'   or "csv" (comma separated values, universal format or MS Excel). Finally, they are automatically converted
#'   into data frame by the \code{\link[data.table]{fread}}.
#'
#'
#' @param path_to_ASC Character string, path to ASCII file/table
#'   with simulated spectral data (\eqn{Intensity\,\,vs\,\,B}(Field) obtained from various sources.
#'   Path can be alternatively defined by the \code{\link[base]{file.path}} function.
#' @param B.unit Character string pointing to unit of magnetic flux density coming from the original data
#'   which is to be presented on the \eqn{B}-axis of an EPR spectrum,
#'   like \code{"G"} ("Gauss"), \code{"mT"} ("millitesla"). \strong{Default}: \code{B.unit = "G"}.
#' @param Intensity.sim Character string pointing to intensity data frame column corresponding
#'   to simulated EPR spectrum. If used together with quantification of radicals,
#'   this argument must be equal to that of the \code{\link{quantify_EPR_Sim_series}}.
#' @param sim.origin Character string referring to "origin" of the simulated ASCII data.
#'   There are four possibilities \eqn{\Rightarrow} \code{sim.orimgin = "easyspin"} (\strong{default}),
#'   \code{"xenon"}, \code{"simfonia"} as well as universal \code{"csv"}.
#'
#' @return Data frame consisting of magnetic flux density (\code{Bsim_mT} or \code{Bsim_G}) and intensity
#'   variable/column (depending on \code{Intensity.sim} argument) corresponding to simulated EPR spectrum.
#'
#'
#' @examples
#' \dontrun{
#' readEPR_Sim_Spec(path_to_ASC =
#'                   "./Simulations/TEMPO_simulation.txt",
#'                  sim.origin = "xenon")
#' #
#' readEPR_Sim_Spec("Cu_complex_simulation.txt",
#'                  B.unit = "mT",
#'                  sim.origin = "easyspin")
#' }
#'
#'
#'
#' @export
#'
#'
readEPR_Sim_Spec <- function(path_to_ASC,
                             B.unit = "G",
                             Intensity.sim = "dIeprSim_over_dB",
                             sim.origin = "easyspin"
                             ) {
  #
## 'Temporary' processing variables
Bsim_G <- NULL
Bsim_mT <- NULL
#
## x-axis/column for B + y-axis/column for intensity
x.col.string <- paste0("Bsim_", B.unit)
y.col.string <- Intensity.sim
#
## Conditions depending on "origin"
if (sim.origin == "easyspin") {
  spectrum.data <- data.table::fread(
    file = path_to_ASC,
    sep = "auto",
    col.names = c(
      x.col.string,
      y.col.string
    )
  )
}
if (sim.origin == "xenon") {
  spectrum.data <- data.table::fread(
    file = path_to_ASC,
    sep = "auto",
    header = FALSE,
    select = c(2, 3),
    col.names = c(
      x.col.string,
      y.col.string
    )
  )
}
if (sim.origin == "simfonia") {
  ## There are two file types 'txt' and 'asc' therefore
  ## they have to be differentiated by the corresponding pattern
  simf.data.file <- readLines(path_to_ASC)
  simf.data.pattern.read.01 <-
    unlist(stringr::str_split(simf.data.file[5],
                              pattern = "[[:space:]]+",
                              n = 5
    )
  )
  simf.data.pattern.read.02 <-
    unlist(stringr::str_split(simf.data.file[6],
                              pattern = "---"
    )
  )
  ## conditions
  simf.data.condition.01 <- grepl("Data", simf.data.pattern.read.01[2])
  simf.data.condition.02 <-
    grepl("--", simf.data.pattern.read.02[length(simf.data.pattern.read.02)])
  #
  ## the `sim.data.file` is not required anymore
  rm(simf.data.file)
  #
  if (isTRUE(simf.data.condition.01) & isTRUE(simf.data.condition.02)) {
    ## reading simfonia '.txt'
    spectrum.data <- data.table::fread(
      file = path_to_ASC,
      sep = "auto",
      header = FALSE,
      select = c(2, 3),
      col.names = c(
        x.col.string,
        y.col.string
      )
    )
  } else {
    ## reading simfonia '.asc'
    spectrum.data <- data.table::fread(
      file = path_to_ASC,
      sep = "auto",
      header = TRUE,
      col.names = c(
        x.col.string,
        y.col.string
      )
    )
  }
  #
}
#
if (sim.origin == "csv") {
  spectrum.data <- data.table::fread(
    file = path_to_ASC,
    sep = "auto",
    header = TRUE,
    col.names = c(
      x.col.string,
      y.col.string
    )
  )
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
