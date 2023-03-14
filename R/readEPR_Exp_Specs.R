#
#' Read the Experimental ASCII Data of EPR/ENDOR Spectra
#'
#' @description Reads experimental EPR/ENDOR spectra recorded by BRUKER spectrometers
#'   in ASCII format (\code{.txt}, \code{.csv} or \code{.asc}) and transforms it into \code{data frame},
#'   which can be easily processed by other R packages (e.g. by \pkg{tidyverse} system), afterwards.
#'   Spectral data (intensities) are normalized by the common experimental parameters like Q-factor, concentration...etc.
#'   ASCII files/tables depend on the origin/software used to acquire the EPR spectra. This is mirrored by \code{origin}
#'   parameter. Time series (time evolution of EPR spectra) can be read by the \code{time.series = TRUE/FALSE} parameter.
#'
#'
#' @param path_to_ASC String, path to ASCII file/table (e.g. \code{.txt}, \code{.csv} or \code{.asc})
#'   with spectral data (\eqn{Intensity vs B}(Field) with additional 'index' and/or 'time' variables).
#'   The path can be also defined by \code{\link[base]{file.path}}.
#' @param sep Character/String inherited from \code{\link[data.table]{fread}}, separator between columns,
#'   like \code{","},\code{"\t"}...etc \strong{default}: \code{sep = "auto"}.
#' @param skip Number, inherited from \code{\link[data.table]{fread}}, representing the number of ASCII data rows
#'   which do not contain data/rows or columns names. It automatically avoids irregular header information
#'   before the column names row/line, \strong{default}: \code{skip = 1}.
#' @param x.unit Character/String pointing to unit of quantity (coming from original ASCII data, see also
#'   \code{column.names} parameter) which is to be presented on \eqn{x} abscissa of the EPR spectrum,
#'   like \code{"G"} (`Gauss`), \code{"mT"} (`millitesla`), \code{"MHz"} (`megahertz` in case of ENDOR spectra)
#'   or \code{"Unitless"} in case of \eqn{g}-values, \strong{default}: \code{x.unit = "G"}.
#' @param col.names Character/String vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its units, \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"}, \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum abscissa)...etc, \strong{default}: \code{col.names = c("index","B_G",dIepr_over_dB)}.
#'   The \code{sep}, \code{skip} as well as \code{col.names} parameters strongly depends on the origin
#'   of ASCII input data file (see also \code{origin} parameter), however these parameters are present
#'   to make a `read` function flexible for any kind of ASCII data format.
#' @param qValue Numeric, Q value (quality factor, number) displayed at specific \code{dB} by spectrometer,
#'   in case of ` "Xenon" ` software the parameter is included in \code{.DSC} file, \strong{default}:
#'   \code{qValue = 1}
#' @param Nscans Number of scans/sweeps per spectrum, in the case of ` "Xenon" ` software, the parameter
#'   is already included for the intensity normalization, \strong{default}: \code{Nscans = 1}
#' @param c.M Numeric, Concentration of the analyte (e.g. radical) in solution (sample) in mol*dm^{-3},
#'   \strong{default}: \code{c.M = 1}
#' @param m.mg Numeric, weight of the powder sample in\code{mg}, \strong{default}: \code{m.mg = 1}
#' @param time.series Boolean, whether the input ASCII data come from the time series experiment
#'   with the additional \code{time} column, \strong{default}: \code{time.series = FALSE}
#' @param origin String/Character corresponding to \strong{origin} of the ASCII data, like from
#'   BRUKER spectrometers (from which are data loaded automatically using the default parameters).
#'   Options are summarized in the following table =>
#'   \tabular{rl}{
#'   \strong{String} \tab \strong{Description} \cr
#'   "xenon" \tab \strong{default} automatically loads from BRUKER `Xenon` softw with dafault params. \cr
#'   "winepr" \tab automatically loads from BRUKER `WinEpr` softw. \cr
#'   "csv" \tab general, to read `csv` ASCII data (parameters, see above must be adjusted accordingly) \cr
#'   "txt" \tab general, to read `txt` ASCII data (parameters, see above must be adjusted accordingly) \cr
#'   }
#'
#' @return Data frame/table consisting of the unitless \code{g-factor} or the magnetic flux density
#'   column \code{B_mT} in millitesla (as well as \code{B_G} in gauss) or \code{RF_MHz}
#'   (in case of ENDOR spectrum) and the derivative intensity column (\code{dIepr_over_dB})
#'   or any other intensities (like in integrated spectral form) in \code{procedure defined unit}
#'   (see \href{http://www.iupac.org/divisions/VII/VII.C.1/C-NPU_Uppsala_081023_25_minutes_confirmed.pdf}{p.d.u.}),
#'   which is normalized by the above-described parameters and finally the \code{index} and/or a \code{time}
#'   (in the case of time series experiment) columns are displayed as well.
#'
#' @examples
#' \dontrun{
#' ## for the spectrum acquired by "xenon"
#' readEPR_Exp_Specs(path_to_ASC,
#'                   qValue = 3500)
#'
#' ## for the spectrum acquired by "winepr"
#' ## (and 20 scans) on a 10 mg powder sample:
#' readEPR_Exp_Specs(path_to_ASC,
#'                   qValue = 2000,
#'                   Nscans = 20,
#'                   m.mg = 10,
#'                   origin = "winepr")
#'
#' ## if no parameter intensity normalization
#' ## is required and spectrum
#' ## was recorded by "xenon" software:
#' readEPR_Exp_Specs(path_to_ASC = file.path(".",
#'                                           "ASCII_Folder",
#'                                           "EPR_spectrum.txt"))
#'
#' ## for the ENDOR spectrum recorded by "xenon"
#' readEPR_Exp_Specs("./Data/ENDOR_spectrum.txt",
#'                   x.unit = "MHz",
#'                   col.names = c("index",
#'                                 "RF_MHz",
#'                                 "dIepr_over_dB"),
#'                   Nscans = 40)
#'
#' ## Example for time series experiment (evolution of EPR spectra
#' ##  in time, e.g. in case of
#' ## EPR spectroelectrochemistry or photochemistry):
#' readEPR_Exp_Specs(path_to_ASC,
#'                   x.unit = "G",
#'                   col.names = c("index",
#'                                 "B_G",
#'                                 "time_s",
#'                                 "dIepr_over_dB"),
#'                   qValue = 2800,
#'                   time.series = T)
#'
#' ## General example to read "csv" data,
#' ## including column "B_mT" and "Intensity"
#' readEPR_Exp_Specs("./Data/EPR_spectrum.csv",
#'                   sep = ",", ## or "auto"
#'                   skip = 2,
#'                   x.unit = "mT",
#'                   col.names = c("B_mT","Intensity"),
#'                   origin = "csv")
#' }
#'
#'
#' @export
#'
#'
#' @importFrom rlang .data quo_name :=
readEPR_Exp_Specs <- function(path_to_ASC,
                              sep = "auto",
                              skip = 1,
                              x.unit = "G",
                              col.names = c("index",
                                            "B_G",
                                            "dIepr_over_dB"),
                              qValue = 1,
                              Nscans = 1,
                              c.M = 1,
                              m.mg = 1,
                              time.series = FALSE,
                              origin = "xenon"){
  ## 'Temporary' processing variables
  Norm_Intensity <- NULL
  #
  ## basic data frame by `fread`, it is processed/mutated bellow
  ## and transfered into `spectrum.data`
  if (origin == "xenon" || origin == "txt" || origin == "csv"){
    spectrum.data.origin <- data.table::fread(path_to_ASC,
                                              sep = sep,
                                              header = FALSE,
                                              skip = skip,
                                              col.names = col.names)
  }
  if (origin == "winepr"){
    if (isFALSE(time.series)){
      spectrum.data.origin <- data.table::fread(path_to_ASC,
                                                sep = "auto",
                                                header = FALSE,
                                                skip = 3,
                                                col.names = col.names)
    } else{
      spectrum.data.origin <- data.table::fread(path_to_ASC,
                                                sep = "auto",
                                                header = FALSE,
                                                skip = 4,
                                                col.names = col.names)
    }
  }
  #
  ## select column character string from col.names corresponding
  ## to `x (B)` and `rf (MHz)` and/or `time`
  if (isFALSE(time.series)){
    if (x.unit == "Unitless"){
      x = grep("g",col.names,value = TRUE)
    }
    if (x.unit == "G" || x.unit == "mT"){
      x = grep("B|G|mT",col.names,value = TRUE)[[1]]
      ## `[[1]]` selection is due to fact that intensity
      ## could be `dIepr_over_dB` which also has `B` within
      ## the character string
      ## `|` \equiv `or`
    }
    if (x.unit == "MHz"){
      x = grep("rf|RF|MHz|radio|Radio",col.names,value = TRUE)
    }
  } else{
    if (x.unit == "Unitless"){
      x = grep("g",col.names,value = TRUE)
      time = grep("time|Time|tim|Tim",col.names,value = TRUE)
    }
    if (x.unit == "G" || x.unit == "mT"){
      x = grep("B|G|mT",col.names,value = TRUE)[[1]]
      time = grep("time|Time",col.names,value = TRUE)
    }
    if (x.unit == "MHz"){
      x = grep("rf|RF|MHz|radio|Radio",col.names,value = TRUE)
      time = grep("time|Time",col.names,value = TRUE)
    }
  }
  #
  ## select intesity column character string
  Intensity = grep("I|Intens|intens",col.names,value = TRUE)
  #
  ##
  if (origin == "xenon" || origin == "txt" || origin == "csv"){
    if (x.unit == "G"){
      ## For intesity 1.) Create new normalized intensity column
      ## 2.) Delete the 'old' intensity column
      ## 3.) Rename the new column by the 'old' name (incl. in col.names),
      ##     however, first off all it must be unquoted
      ##     therefore, !!rlang::quo_name(Intensity) := Norm_Intensity
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(B_mT = .data[[x]]/10,
                      Norm_Intensity = .data[[Intensity]]/(qValue*Nscans*m.mg*c.M)) %>%
        dplyr::select(-.data[[Intensity]]) %>%
        dplyr::rename(!!rlang::quo_name(Intensity) := Norm_Intensity)
    }
    if (x.unit == "MHz"){
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(Norm_Intensity = .data[[Intensity]]/Nscans) %>%
        dplyr::select(-.data[[Intensity]]) %>%
        dplyr::rename(!!rlang::quo_name(Intensity) := Norm_Intensity)
    }
    if (x.unit == "mT" || x.unit == "Unitless"){
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(Norm_Intensity = .data[[Intensity]]/(qValue*Nscans*m.mg*c.M)) %>%
        dplyr::select(-.data[[Intensity]]) %>%
        dplyr::rename(!!rlang::quo_name(Intensity) := Norm_Intensity)
    }
  }
  if (origin == "winepr"){
    if (x.unit == "G"){
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(B_mT = .data[[x]]/10,
                      Norm_Intensity = .data[[Intensity]]/(qValue*Nscans*m.mg*c.M),
                      index = seq_len(length(.data[[Intensity]]))) %>%
        dplyr::select(-.data[[Intensity]]) %>%
        dplyr::rename(!!rlang::quo_name(Intensity) := Norm_Intensity)
    }
    if (x.unit == "MHz"){
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(Norm_Intensity = .data[[Intensity]]/Nscans,
                      index = seq_len(length(.data[[Intensity]]))) %>%
        dplyr::select(-.data[[Intensity]]) %>%
        dplyr::rename(!!rlang::quo_name(Intensity) := Norm_Intensity)
    }
    if (x.unit == "mT" || x.unit == "Unitless"){
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(Norm_Intensity = .data[[Intensity]]/(qValue*Nscans*m.mg*c.M),
                      index = seq_len(length(.data[[Intensity]]))) %>%
        dplyr::select(-.data[[Intensity]]) %>%
        dplyr::rename(!!rlang::quo_name(Intensity) := Norm_Intensity)
    }
  }
  #
  return(spectrum.data)
  #
}
