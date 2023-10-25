#'
#' Read the \strong{Selected} Instrumental Parameters Relevant to EPR Quantitative Analysis
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Reading the \code{.DSC} or \code{.par} file to extract the important parameters like
#'   "modulation amplitude", "temperature", "microwave power" as well as "microwave frequency"
#'   which are are required for absolute quantitative analysis of the EPR spectra (\eqn{\equiv}
#'   radical or paramagnetic species number determination).
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
#'
#'
#' @return List consisting of \code{"BmmT"} ("modulation amplitude" value  in `mT`),
#'   \code{"PmW"} ("microwave source power" value in `mW`), \code{"TK"} ("temperature" value in `K`)
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
## required for quantification
readEPR_params_slct_quant <- function(path_to_DSC_or_par,
                                      origin = "xenon"){
  #
  ## reading the table and extracting values form table
  params.df <- readEPR_params_tabs(path_to_DSC_or_par,origin = origin)$params
  #
  Bm.mT <- params.df %>%
    dplyr::filter(.data$Parameter == "Modulation Amplitude") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  Bm.mT <- round(Bm.mT,digits = 3)
  #
  P.mW <- params.df %>%
    dplyr::filter(.data$Parameter == "Power") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  P.mW <- round(P.mW,digits = 2)
  #
  Temp.K <- params.df %>%
    dplyr::filter(.data$Parameter == "Temperature") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  Temp.K <- round(Temp.K)
  #
  nu.GHz <- params.df %>%
    dplyr::filter(.data$Parameter == "Frequency") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  nu.GHz <- round(nu.GHz,digits = 7)
  ## not required anymore =>
  rm(params.df)
  #
  named.params.list <- list(BmmT = Bm.mT,PmW = P.mW,TK = Temp.K,mwGHz = nu.GHz)
  return(named.params.list)
}
