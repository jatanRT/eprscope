#
#' @title Read the Experimental ASCII (\code{.txt}, \code{.csv} or \code{.asc}) EPR/ENDOR Spectrum and Transfer it into Data Frame
#'
#' @description Function reads/loads the experimental EPR/ENDOR spectrum recorded by BRUKER spectrometers
#'   in ASCII format by the \code{\link[data.table]{fread}} function and transforms it into \code{data frame},
#'   which can be easily processed by other R packages (e.g. by \pkg{tidyverse} system), afterwards.
#'   Spectral data are normalized by the common experimental parameters in order to qualitatively compare
#'   the intensities of several spectra. ASCII files/tables depend on the software used to record the EPR spectra
#'   on BRUKER spectrometers and are slightly different. This is mirrored by \code{origin} parameter (with \code{"xenon"}
#'   or \code{"winepr"}). To distinguish between EPR and ENDOR spectrum a general \code{x}-axis quantity with two \code{x}s
#'   \code{x = "B_G"} or \code{x = "RF_MHz"} is available. Time series spectra (time evolution of EPR spectra) can be read
#'   by the \code{time.series = TRUE/FALSE} parameter (ONLY IN CASE of \code{"xenon"} software).
#'
#'
#' @param path_to_ASC String, path to ASCII file/table (e.g. \code{.txt}, \code{.csv} or \code{.asc})
#'   with spectral data (\eqn{Intensity vs B}(Field) with additional 'index' and/or 'time' variables)
#' @param x String related to recorded "EPR" or "ENDOR" spectrum. \strong{Only two 'values' are allowed:}
#'   \code{x = "B_G"} or  \code{x = "RF_MHz"}
#' @param qValue Numeric, Q value (quality factor, number) displayed at specific \code{dB} by spectrometer.
#'   In case of "Xenon" software the parameter is included in \code{.DSC} file, \strong{default = 1}
#' @param Nscans Number of scans/sweeps per spectrum, in the case of "Xenon" software, the parameter
#'   is already included for the intensity normalization, \strong{default = 1}
#' @param m.mg Numeric, weight of the powder sample in\code{mg}, \strong{default = 1}
#' @param c.M Numeric, Concentration of the analyte (e.g. radical) in solution (sample) in mol*dm^{-3},
#'   \strong{default = 1}
#' @param time.series Boolean, whether the input ASCII spectrum comes from the time series experiment
#'   with the additional \code{time} column (ONLY IN CASE of "Xenon" software), \strong{default = FALSE}
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the files are slightly different depending on whether they were recorded
#'   by the windows based softw. ("WinEpr",\code{origin = "winepr"}) or by the Linux one ("Xenon"),
#'   \strong{default}: \code{origin = "xenon"}
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
#' readEPR_Exp_Specs(path_to_ASCII_file,qValue = 3500) ## for the spectrum acquired by "xenon"
#'
#' ## for the spectrum acquired by "winepr" (and 20 scans) on a 10 mg powder sample:
#' readEPR_Exp_Specs(path_to_ASCII_file,qValue = 2000,Nscans = 20,m.mg = 10,origin = "winepr")
#'
#' ## if no parameter intensity normalization is required and spectrum
#' ## was recorded by "xenon" software:
#' readEPR_Exp_Specs(path_to_ASCII_file)
#'
#' ## for the ENDOR spectrum recorded by "xenon"
#' readEPR_Exp_Specs(path_to_ASCII_file,x = "RF_MHz",Nscans = 40)
#'
#' ## Example for time series experiment (evolution of EPR spectra in time, e.g. in case of
#' ## EPR spectroelectrochemistry or photochemistry):
#' readEPR_Exp_Specs(path_to_ASC_file,x = "B_G",qValue = 2800,time.series = T)
#' }
#'
#'
#' @export
#'
#'
#' @importFrom rlang .data
readEPR_Exp_Specs <- function(path_to_ASC,
                            x = "B_G", ## only two 'values' "B_G" or "RF_MHz"
                            qValue = 1,
                            Nscans = 1,
                            c.M = 1,
                            m.mg = 1,
                            time.series = FALSE,
                            origin = "xenon"){
  if (origin == "xenon"){
    if (isFALSE(time.series)){
    spectrum.data <- data.table::fread(path_to_ASC,
                                       sep = "auto",
                                       header = F,
                                       skip = 1,
                                       col.names = c("index",x,"Intensity")) %>%
      {if(x == "B_G") dplyr::mutate(B_mT = .data$x/10,
                                    dIepr_over_dB = .data$Intensity/(qValue*Nscans*m.mg*c.M))
        else dplyr::mutate(dIepr_over_dB = .data$Intensity/Nscans)
          } %>%
      dplyr::select(-.data$Intensity) ## presence of both `B_mT` and `B_G` is required
    #
    ## to add pipe operator '%>%' to the whole package one must run:
    ## 1. 'usethis::use_pipe()' 2. 'devtools::document()'
    } else{
      spectrum.data <- data.table::fread(path_to_ASC,
                                         sep = "auto",
                                         header = F,
                                         skip = 1,
                                         col.names = c("index","B_G","time_s","Intensity")) %>%
        dplyr::mutate(B_mT = .data$B_G/10,
                      dIepr_over_dB = .data$Intensity/(qValue*Nscans*m.mg*c.M)) %>%
        dplyr::select(-.data$Intensity)
    }
  }
  if (origin == "winepr"){
    spectrum.data <- data.table::fread(path_to_ASC,
                                       sep = "auto",
                                       header = F,
                                       skip = 3,
                                       col.names = c(x,"Intensity")) %>%
      {if(x == "B_G") dplyr::mutate(B_mT = .data$B_G/10,
                                    dIepr_over_dB = .data$Intensity/(qValue*Nscans*m.mg*c.M),
                                    index = seq_len(nrow(spectrum.data)))
        else dplyr::mutate(dIepr_over_dB = .data$Intensity/Nscans,
                           index = seq_len(nrow(spectrum.data)))
          } %>%
      dplyr::select(-.data$Intensity)
  }
  #
  return(spectrum.data)
  #
}
