#
#' @title Read the Experimental ASCII Data of EPR/ENDOR Spectra
#'
#' @description Reads experimental EPR/ENDOR spectra recorded by BRUKER spectrometers
#'   in ASCII format (\code{.txt}, \code{.csv} or \code{.asc}) and transforms it into \code{data frame},
#'   which can be easily processed by other R packages (e.g. by \pkg{tidyverse} system), afterwards.
#'   Spectral data (intensities) are normalized by the common experimental parameters like Q-factor, concentration...etc.
#'   ASCII files/tables depend on the software used to acquire the EPR spectra. This is mirrored by \code{origin}
#'   parameter. To distinguish between EPR and ENDOR spectrum a general \code{type}-parameter is defined
#'   \code{type = "epr"} or \code{type = "endor"}. Time series (time evolution of EPR spectra) can be read
#'   by the \code{time.series = TRUE/FALSE} parameter.
#'
#'
#' @param path_to_ASC String, path to ASCII file/table (e.g. \code{.txt}, \code{.csv} or \code{.asc})
#'   with spectral data (\eqn{Intensity vs B}(Field) with additional 'index' and/or 'time' variables),
#'   it can be also defined by \code{\link[base]{file.path}}
#' @param type String/Character pointing either to \code{"epr"} (\strong{default}, having the `x = "B_G"`
#'  in the spectrum) or to \code{"endor"} spectra (having the x = `x = "RF_MHz"` in the spectrum)
#' @param qValue Numeric, Q value (quality factor, number) displayed at specific \code{dB} by spectrometer.
#'   In case of "Xenon" software the parameter is included in \code{.DSC} file, \strong{default = 1}
#' @param Nscans Number of scans/sweeps per spectrum, in the case of "Xenon" software, the parameter
#'   is already included for the intensity normalization, \strong{default = 1}
#' @param m.mg Numeric, weight of the powder sample in\code{mg}, \strong{default = 1}
#' @param c.M Numeric, Concentration of the analyte (e.g. radical) in solution (sample) in mol*dm^{-3},
#'   \strong{default = 1}
#' @param time.series Boolean, whether the input ASCII spectrum comes from the time series experiment
#'   with the additional \code{time} column, \strong{default = FALSE}
#' @param origin String/Character corresponding to \strong{software} used to acquire the EPR spectra
#'   on BRUKER spectrometers, i.e. whether they were recorded by the windows based softw. ("WinEpr",
#'   \code{origin = "winepr"}) or by the Linux one ("Xenon"), \strong{default}: \code{origin = "xenon"}
#'
#' @return Data frame/table consisting of the magnetic flux density column \code{B_mT}
#'   in millitesla (as well as \code{B_G} in gauss) or \code{RF_MHz} (in case of ENDOR spectrum) and the derivative
#'   intensity column (\code{dIepr_over_dB}) in \code{procedure defined unit}
#'   (see \href{http://www.iupac.org/divisions/VII/VII.C.1/C-NPU_Uppsala_081023_25_minutes_confirmed.pdf}{p.d.u.}),
#'   which is normalized by the above-described parameters and finally the \code{index} and/or a \code{time}
#'   (in the case of time series experiment) columns are displayed as well.
#'
#' @examples
#' \dontrun{
#' readEPR_Exp_Specs(path_to_ASC,qValue = 3500) ## for the spectrum acquired by "xenon"
#'
#' ## for the spectrum acquired by "winepr" (and 20 scans) on a 10 mg powder sample:
#' readEPR_Exp_Specs(path_to_ASC,qValue = 2000,Nscans = 20,m.mg = 10,origin = "winepr")
#'
#' ## if no parameter intensity normalization is required and spectrum
#' ## was recorded by "xenon" software:
#' readEPR_Exp_Specs(path_to_ASC = file.path(".","ASCII_Folder","EPR_spectrum.txt"))
#'
#' ## for the ENDOR spectrum recorded by "xenon"
#' readEPR_Exp_Specs(path_to_ASC,type = "endor",Nscans = 40)
#'
#' ## Example for time series experiment (evolution of EPR spectra in time, e.g. in case of
#' ## EPR spectroelectrochemistry or photochemistry):
#' readEPR_Exp_Specs(path_to_ASC,type = "epr",qValue = 2800,time.series = T)
#' }
#'
#'
#' @export
#'
#'
#' @importFrom rlang .data
readEPR_Exp_Specs <- function(path_to_ASC,
                            type = "epr", ## only two 'values' "epr" or "endor"
                            qValue = 1,
                            Nscans = 1,
                            c.M = 1,
                            m.mg = 1,
                            time.series = FALSE,
                            origin = "xenon"){
  #
  ## 'Temporary' processing variables
  B_G <- NULL
  #
  if (origin == "xenon"){
    if (isFALSE(time.series)){
      if (type == "epr"){
        spectrum.data <- data.table::fread(path_to_ASC,
                                           sep = "auto",
                                           header = F,
                                           skip = 1,
                                           col.names = c("index","B_G","Intensity")) %>%
          dplyr::mutate(B_mT = B_G/10,
                        dIepr_over_dB = .data$Intensity/(qValue*Nscans*m.mg*c.M)) %>%
          dplyr::select(-.data$Intensity)
      }
      if (type == "endor"){
        spectrum.data <- data.table::fread(path_to_ASC,
                                           sep = "auto",
                                           header = F,
                                           skip = 1,
                                           col.names = c("index","RF_MHz","Intensity")) %>%
          dplyr::mutate(dIepr_over_dB = .data$Intensity/Nscans) %>%
          dplyr::select(-.data$Intensity)
      }
    #
    ## to add pipe operator '%>%' to the whole package one must run:
    ## 1. 'usethis::use_pipe()' 2. 'devtools::document()'
    } else{
      spectrum.data <- data.table::fread(path_to_ASC,
                                         sep = "auto",
                                         header = F,
                                         skip = 1,
                                         col.names = c("index","B_G","time_s","Intensity")) %>%
        dplyr::mutate(B_mT = B_G/10,
                      dIepr_over_dB = .data$Intensity/(qValue*Nscans*m.mg*c.M)) %>%
        dplyr::select(-.data$Intensity)
    }
  }
  if (origin == "winepr"){
    if (isFALSE(time.series)){
      if (type == "epr"){
        spectrum.data <- data.table::fread(path_to_ASC,
                                           sep = "auto",
                                           header = F,
                                           skip = 3,
                                           col.names = c("B_G","Intensity")) %>%
          dplyr::mutate(B_mT = B_G/10,
                        dIepr_over_dB = .data$Intensity/(qValue*Nscans*m.mg*c.M),
                        index = seq_len(length(.data$Intensity))) %>%
          dplyr::select(-.data$Intensity)
      }
      if (type == "endor"){
        spectrum.data <- data.table::fread(path_to_ASC,
                                           sep = "auto",
                                           header = F,
                                           skip = 3,
                                           col.names = c("RF_MHz","Intensity")) %>%
          dplyr::mutate(dIepr_over_dB = .data$Intensity/Nscans,
                        index = seq_len(length(.data$Intensity))) %>%
          dplyr::select(-.data$Intensity)
      }
    } else{
      spectrum.data <- data.table::fread(path_to_ASC,
                                         sep = "auto",
                                         header = F,
                                         skip = 4,
                                         col.names = c("B_G","time_s","Intensity")) %>%
        dplyr::mutate(B_mT = B_G/10,
                      dIepr_over_dB = .data$Intensity/(qValue*Nscans*m.mg*c.M),
                      index = seq_len(length(.data$Intensity))) %>%
        dplyr::select(-.data$Intensity)
    }
  }
  #
  return(spectrum.data)
  #
}
