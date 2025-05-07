#
#' Read the ASCII Data of a Simulated EPR Spectrum
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Loading the ASCII data like \code{.txt},\code{.asc} or \code{.csv}, related to simulated EPR spectrum from different
#'   sources like "EasySpin" (\emph{Matlab}), "Xenon" (EPR spectrometer), "SimFonia" (WinEPR system)
#'   or "csv" (comma separated values, universal format or MS Excel). Finally, they are automatically converted
#'   into data frames by the \code{\link[data.table]{fread}}.
#'
#'
#' @param path_to_ASC Character string, path to ASCII file/table
#'   with simulated spectral data (\eqn{Intensity\,\,vs\,\,B}(Field) obtained from various sources.
#'   Path can be alternatively defined by the \code{\link[base]{file.path}} function.
#' @param B.unit Character string, pointing to unit of magnetic flux density coming from the original data
#'   which is to be presented on the \eqn{B}-axis of an EPR spectrum,
#'   like \code{"G"} ("Gauss"), \code{"mT"} ("millitesla"). \strong{Default}: \code{B.unit = "G"}.
#' @param col.names.sim Character string vector, pointing to column names/headers of the original ASCII data
#'   (refer also to the \code{path_to_ASC} argument and description).
#'   \strong{Default}: \code{col.names.sim = c("Bsim_G","dIeprSim_over_dB")},
#'   corresponding to the \code{origin.sim = "easyspin"}.
#' @param x.sim.id Numeric index related to the \code{col.names.sim} vector pointing to independent variable (like "B" or "Bsim"
#'   - magnetic flux density), which corresponds to \eqn{x}-axis in the simulated spectra (refer also to the original ASCII data
#'   \code{path_to_ASC} of the simulated spectrum). \strong{Default}: \code{x.sim.id = 1} (see also default of the \code{col.names.sim}).
#' @param Intensity.sim.id Numeric index related to the \code{col.names.sim} vector pointing to simulated EPR intensity
#'   column name/header in the original ASCII data (refer also to the \code{path_to_ASC} argument and description).
#'   If used together with the quantification of radicals, this argument must be equal to that
#'   of the \code{\link{quantify_EPR_Sim_series}}. \strong{Default}: \code{x.sim.id = 2} (see also default of the \code{col.names.sim}).
#' @param origin.sim Character string, referring to the "origin" of a simulated spectrum ASCII data.
#'   There are four possibilities \eqn{\Rightarrow} \code{sim.orimgin = "easyspin"} (\strong{default}),
#'   \code{"xenon"}, \code{"simfonia"} as well as universal \code{"csv"}.
#'
#' @return Data frame, consisting of magnetic flux density and intensity variable/column
#'   (depending on the \code{col.names.sim} and \code{.id} arguments), corresponding to simulated EPR spectrum.
#'
#'
#' @examples
#' \dontrun{
#' readEPR_Sim_Spec(path_to_ASC =
#'                   "./Simulations/TEMPO_simulation.txt",
#'                  origin.sim = "xenon")
#' #
#' readEPR_Sim_Spec("Cu_complex_simulation.txt",
#'                  B.unit = "mT",
#'                  col.names.sim = c("Bsim_G","dIeprSim_over_dB"),
#'                  x.sim.id = 1,
#'                  Intensity.sim.id = 2,
#'                  origin.sim = "easyspin")
#' }
#'
#'
#'
#' @export
#'
#'
readEPR_Sim_Spec <- function(path_to_ASC,
                             B.unit = "G",
                             col.names.sim = c("Bsim_G","dIeprSim_over_dB"),
                             x.sim.id = 1,
                             Intensity.sim.id = 2,
                             origin.sim = "easyspin"
                             ) {
  #
## 'Temporary' processing variables
Bsim_G <- NULL
Bsim_mT <- NULL
#
## x-axis/column for B + y-axis/column for intensity
x.col.string <- col.names.sim[x.sim.id]
y.col.string <- col.names.sim[Intensity.sim.id]
#
## Conditions depending on "origin"
if (origin.sim == "easyspin"|| origin.sim == "Easyspin" ||
    origin.sim == "EasySpin" || origin.sim == "EASYSPIN") {
  spectrum.data <- data.table::fread(
    file = path_to_ASC,
    sep = "auto",
    col.names = col.names.sim
  )
}
if (origin.sim == "xenon" || origin.sim == "Xenon" ||
    origin.sim == "XENON") {
  spectrum.data <- data.table::fread(
    file = path_to_ASC,
    sep = "auto",
    header = FALSE,
    select = c(2, 3),
    col.names = col.names.sim
  )
}
if (origin.sim == "simfonia" || origin.sim == "Simfonia" ||
    origin.sim == "SIMFONIA") {
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
      col.names = col.names.sim
    )
  } else {
    ## reading simfonia '.asc'
    spectrum.data <- data.table::fread(
      file = path_to_ASC,
      sep = "auto",
      header = TRUE,
      col.names = col.names.sim
    )
  }
  #
}
#
if (origin.sim == "csv") {
  spectrum.data <- data.table::fread(
    file = path_to_ASC,
    sep = "auto",
    header = TRUE,
    col.names = col.names.sim
  )
}
## Adding additional `B` column with corresponding units depending
## on `B.unit`
if (B.unit == "mT" & !any(grepl("G",col.names.sim))) {
  spectrum.data <- spectrum.data %>%
    dplyr::mutate(Bsim_G = .data[[x.col.string]] * 10) %>%
    ## reordering columns
    dplyr::select(Bsim_G, .data[[x.col.string]], .data[[y.col.string]])
}
if (B.unit == "G" & !any(grepl("mT",col.names.sim))) {
  spectrum.data <- spectrum.data %>%
    dplyr::mutate(Bsim_mT = .data[[x.col.string]] / 10) %>%
    ## reordering columns
    dplyr::select(Bsim_mT, .data[[x.col.string]], .data[[y.col.string]])
}
#
return(spectrum.data)
#
}
