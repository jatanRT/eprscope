#
#' @title Read the Experimental ASCII (\code{.txt}, \code{.csv} or \code{.asc}) EPR Spectrum and Transfer it into Data Frame
#'
#' @description Function reads/loads the experimental EPR spectrum recorded by BRUKER spectrometers
#'   in ASCII format by the \code{\link[data.table]{fread}} function and transforms it into \code{data frame},
#'   which can be easily processed by other R \code{data science} packages (e.g. by \code{\pkg{tidyverse}} system),
#'    afterwards. For this purpose a \code{pipe} operator from \code{\pkg{magrittr}} is applied.
#'   Spectral data are normalized by the common experimental parameters in order to qualitatively compare
#'   the intensities of several spectra. ASCII files/tables depend on the software used to record the EPR spectra
#'   on BRUKER spectrometers and are slightly different. This is mirrored by \code{origin} parameter (with \code{"xenon"}
#'   or \code{"winepr"}).Time series spectra (time evolution of EPR spectra) can be read
#'   by the \code{time.series = TRUE/FALSE} parameter (ONLY IN CASE of \code{"xenon"} software).
#'
#'
#' @param path_to_ASC String, path to ASCII file/table (e.g. \code{.txt}, \code{.csv} or \code{.asc})
#'   with spectral data (\eqn{Intensity vs B}(Field) with additional 'index' and/or 'time' variables)
#' @param qfactor Numeric, Q value (quality factor, number) displayed at specific \code{dB} by spectrometer.
#'   In case of "Xenon" software the parameter is included in \code{.DSC} file, \strong{default = 1}
#' @param Ns Number of scans/sweeps per spectrum, in the case of "Xenon" software, the parameter
#'   is already included for the intensity normalization, \strong{default = 1}
#' @param m Numeric, weight of the powder sample in mg, \strong{default = 1}
#' @param cM Numeric, Concentration of the analyte (e.g. radical) in solution (sample) in mol*dm^{-3},
#'   \strong{default = 1}
#' @param time.series Boolean, whether the input ASCII spectrum comes from the time series experiment
#'   with the additional \code{time} column (ONLY IN CASE of "Xenon" software), \strong{default = FALSE}
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the ASCII files/tables are slightly different depending
#'   on whether they were recorded by the windows based softw. ("WinEpr") or by the Linux
#'   one ("Xenon"), \strong{default = "Xenon"}
#'
#' @return data frame/table consisting of the magnetic flux density column \code{B_mT}
#'   in millitesla (as well as \code{B_G} in gauss) and the derivative intensity column (\code{dIepr_over_dB})
#'   in \code{procedure defined unit}
#'   (see \href{http://www.iupac.org/divisions/VII/VII.C.1/C-NPU_Uppsala_081023_25_minutes_confirmed.pdf}{p.d.u.}),
#'   which is normalized by the above-described parameters and finally the \code{index} and/or a \code{time}
#'   (in the case of time series experiment) columns are displayed as well.
#'
#' @examples
#' \dontrun{
#' readExpEPRspectr(path_to_ASCII_file,qfactor = 3500) ## for the spectrum acquired by "xenon"
#' ## for the spectrum acquired by "winepr" (and 20 scans) on a 10 mg powder sample:
#' readExpEPRspectr(path_to_ASCII_file,qfactor = 2000,Ns = 20,m = 10,origin = "winepr")
#' ## if no parameter intensity normalization is required and spectrum
#' ## was recorded by "xenon" software:
#' readExpEPRspectr(path_to_ASCII_file)
#' ## Example for time series experiment (evolution of EPR spectra in time, e.g. in case of
#' ## EPR spectroelectrochemistry or photochemistry):
#' readExpEPRspectr(path_to_ASC_file,2800,time.series = T)
#' }
#'
#'
#' @export
#'
#'
#' @importFrom rlang .data
readExpEPRspectra <- function(path_to_ASC,qfactor = 1,Ns = 1,cM = 1,m = 1,time.series = FALSE,origin = "xenon"){
  if (origin == "xenon"){
    if (isFALSE(time.series)){
    spectrum.data <- data.table::fread(path_to_ASC,sep = "auto",header = F,
                                       skip = 1,col.names = c("index","B_G","Intensity")) %>%
      dplyr::mutate(B_mT = .data$B_G/10,dIepr_over_dB = .data$Intensity/(qfactor*Ns*m*cM)) %>%
      dplyr::select(-.data$Intensity) ## presence of both `B_mT` and `B_G` is required
    ## to add pipe operator '%>%' to the whole package one must run:
    ## 1. 'usethis::use_pipe()' 2. 'devtools::document()'
    } else{
      spectrum.data <- data.table::fread(path_to_ASC,sep = "auto",
                                         header = F,skip = 1,col.names = c("index","B_G","time_s","Intensity")) %>%
        dplyr::mutate(B_mT = .data$B_G/10,dIepr_over_dB = .data$Intensity/(qfactor*Ns*m*cM)) %>%
        dplyr::select(-.data$Intensity)
    }
  }
  if (origin == "winepr"){
    spectrum.data <- data.table::fread(path_to_ASC,sep = "auto",
                                       header = F,skip = 3,col.names = c("B_G","Intensity")) %>%
      dplyr::mutate(B_mT = .data$B_G/10,dIepr_over_dB = .data$Intensity/(qfactor*Ns*m*cM),
                    index = seq_len(nrow(spectrum.data))) %>%
      dplyr::select(-.data$Intensity)
  }
  return(spectrum.data)
}
