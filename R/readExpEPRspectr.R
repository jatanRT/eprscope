#
#' @title Read the Experimental ASCII EPR Spectrum
#'
#' @description Function reads/loads the experimental EPR spectrum recorded by BRUKER spectrometers
#'   in ASCII format by \code{\link[data.table]{fread}} and transforms it into \code{data frame},
#'   which can be then easily processed by other R 'data science' packages (e.g. by \code{tidyverse} system).
#'   For this purpose a 'pipe' operator from \code{\link{magrittr}} and heavily used in \code{tidyverse}
#'   is applied. Spectral data are normalized by common experimental parameters in order to qualitatively compare
#'   the intensities of several spectra.
#'
#'
#' @param path_to_ASC Path to ASCII file/table (e.g. \code{.txt} or \code{.csv})
#'   with spectral data (Intensity vs B(Field))
#' @param qfactor Q value (quality factor ) displayed at specific \code{dB} by spectrometer.
#'   In case of "Xenon" software the parameter is included in \code{.DSC} file, default = 1
#' @param Ns Number of scans per sweep, In the case of "Xenon" software, the parameter
#'   is already included for the intensity normalization, default = 1
#' @param m Weight of the powder sample in mg, default = 1
#' @param cM Concentration of the analyte (e.g. radical) in solution (sample) in mol*dm^{-3},
#'   default = 1
#' @param origin Corresponds to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the ASCII files/tables are slightly different depending
#'   on whether they were recorded by windows based softw. ("winepr") or by the Linux
#'   one ("xenon"), default = "xenon"
#'
#' @return data frame with magnetic flux density column (\code{B_mT}) in millitesla;
#'   derivative intensity column (\code{dIepr_over_dB}) in 'procedure defined unit'
#'   (see \href{http://www.iupac.org/divisions/VII/VII.C.1/C-NPU_Uppsala_081023_25_minutes_confirmed.pdf}{p.d.u.}),
#'   which is normalized by the above-described parameters and finally the \code{index} column are displayed
#'
#' @examples
#' \dontrun{
#' readExpEPRspectr(path_to_ASCII_file,qfactor = 3500) ## for the spectrum acquired by "xenon"
#' readExpEPRspectr(path_to_ASCII_file,qfactor = 2000,Ns = 20,m = 10,origin = "winepr")
#' ## for the spectrum acquired by "xenon" (and 20 scans) on a 10 mg powder sample
#' }
#'
#' @export
#'
#' @importFrom rlang .data
NULL
readExpEPRspectr <- function(path_to_ASC,qfactor=1,Ns=1,cM=1,m=1,origin="xenon"){
  if (origin == "xenon"){
    spectrum.data <- data.table::fread(path_to_ASC,sep = "auto",header = F,skip = 1,col.names = c("index","B_G","Intensity")) %>%
      dplyr::mutate(B_mT = .data$B_G/10,dIepr_over_dB = .data$Intensity/(qfactor*Ns*m*cM)) %>%
      dplyr::select(-c(.data$B_G,.data$Intensity))
    ## to add pipe operator '%>%' to the whole package one must run:
    ## 1. 'usethis::use_pipe()' 2. 'devtools::document()'
    return(spectrum.data)
  }
  if (origin == "winepr"){
    spectrum.data <- data.table::fread(path_to_ASC,sep = "auto",header = F,skip = 3,col.names = c("B_G","Intensity")) %>%
      dplyr::mutate(B_mT = .data$B_G/10,dIepr_over_dB = .data$Intensity/(qfactor*Ns*m*cM),index = seq_len(nrow(spectrum.data))) %>%
      dplyr::select(-c(.data$B_G,.data$Intensity))
    return(spectrum.data)
  }
}
