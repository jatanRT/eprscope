#'
#' Read the \strong{Selected} Instrumental Parameters Relevant to \strong{Simulation} of EPR Spectra
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Reading the \code{.DSC} or \code{.par} file to extract the important parameters like
#'   "sweep width", "central field", "number of points" as well as "microwave frequency"
#'   which are are required for the simulations of EPR spectra (see \code{\link{eval_sim_EPR_iso}}).
#'
#'
#'
#' @param path_to_DSC_or_par Character string, path (also provided by \code{\link[base]{file.path}})
#'   to \code{.DSC} or \code{.par} (depending on OS, see \code{origin} parameter)
#'   \code{text} files including all instrumental parameters and provided by the EPR machine.
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the files are slightly different depending on whether they
#'   were recorded by the windows based softw. ("WinEpr",\code{origin = "winepr"}) or by the Linux
#'   one ("Xenon"). \strong{Default}: \code{origin = "xenon"}.
#' @param B.unit Character string pointing to unit of magnetic flux density which is the output
#'   `unit`, \code{"G"} (`Gauss`) or \code{"mT"} (`millitesla`), for \code{"sweep width"}
#'   and \code{"central field"} (see \code{\link{eval_sim_EPR_iso}}).
#'   \strong{Default}: \code{B.unit = "G"}.
#'
#'
#' @return Named numeric vector consisting of \code{"cf"} ("central field" value in `B.unit`),
#'   \code{"sw"} ("sweep width" value in `B.unit`), \code{"points"} ("number of points" value)
#'   and \code{"mwGHz"} ("microwave frequency" value in `GHz`).
#'
#'
#' @examples
#' \dontrun{
#' TODO
#' TODO
#' }
#'
#'
#' @export
#'
#'
## function to read instrumental parameters from `.DSC` or `.par`
## required for simulation
readEPR_params_slct_sim <- function(path_to_DSC_or_par,
                                    origin = "xenon",
                                    B.unit = "G"){
  #
  ## reading the table and extracting values form table
  params.df <- readEPR_params_tabs(path_to_DSC_or_par,origin = origin)$params
  #
  B.CF <- params.df %>%
    dplyr::filter(.data$Parameter == "Central Field") %>%
    dplyr::pull(dplyr::all_of(c("Value"))) %>% convert_B(B.unit = "mT",B.2unit = B.unit)
  B.CF <- round(B.CF,digits = 3)
  #
  B.SW <- params.df %>%
    dplyr::filter(.data$Parameter == "Sweep Width") %>%
    dplyr::pull(dplyr::all_of(c("Value"))) %>% convert_B(B.unit = "mT",B.2unit = B.unit)
  B.SW <- round(B.SW,digits = 3)
  #
  Npoints <- params.df %>%
    dplyr::filter(.data$Parameter == "Number of Points") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  Npoints <- round(Npoints)
  #
  nu.GHz <- params.df %>%
    dplyr::filter(.data$Parameter == "Frequency") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  nu.GHz <- round(nu.GHz,digits = 7)
  ## not required anymore =>
  rm(params.df)
  #
  named.params.vec <- c("cf" = B.CF,"sw" = B.SW,"points" = Npoints,"mwGHz" = nu.GHz)
  return(named.params.vec)
}
