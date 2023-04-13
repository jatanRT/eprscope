#
#' Read the Experimental ASCII or other Text-Based Data Related to EPR/ENDOR Spectra
#'
#' @description The function is based on \code{\link[data.table]{fread}} with the purpose to read
#'   experimental EPR/ENDOR spectra or other original related (preprocessed) data from EPR spectrometers
#'   in ASCII format (e.g. like \code{.txt}, \code{.csv} or \code{.asc}) and transforms it into \code{data frame},
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
#' @param header Character/String or Logical, inherited from \code{\link[data.table]{fread}}...TBC
#' @param na.strings Character vector, inherited from \code{\link[data.table]{fread}}...TBC
#' @param select Character or numeric vector, inherited from \code{\link[data.table]{fread}}...TBC
#' @param drop Character or numeric vector, inherited from \code{\link[data.table]{fread}}...TBC
#' @param encoding Character/String, inherited from \code{\link[data.table]{fread}}...TBC
#' @param fill Logical, inherited from \code{\link[data.table]{fread}}...TBC
#' @param blank.lines.skip Logical, inherited from \code{\link[data.table]{fread}}...TBC
#' @param col.names Character/String vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its units, \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"}, \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum abscissa)...etc, \strong{default}: \code{col.names = c("index","B_G",dIepr_over_dB)}.
#' @param col.char2num Logical, description...converting character column to numeric format... TBC
#' @param x Numeric index related to \code{col.names} pointing to independent variable, which corresponds
#'   to abscissa (\eqn{x}-axis) in spectra or other plots.
#' @param Intensity Numeric index related to \code{col.names} pointing to `general` intensity,
#'   like derivative intensity (`dIepr_over_dB`), integral one (e.g. `single_Integ`), double or sigmoid
#'   integral (e.g. `Area`)...etc. This corresponds to column/vector which should be presented like
#'   \eqn{y}-axis in spectra or other plots.
#' @param convert_B_G2mT Logical (\strong{default}: \code{convert_B_G2mT}) description... convert \eqn{B}
#'   in Gauss to millitesla
#' @param qValue Numeric, Q value (quality factor, number) displayed at specific \code{dB} by spectrometer,
#'   in case of ` "Xenon" ` software the parameter is included in \code{.DSC} file, \strong{default}:
#'   \code{qValue = 1}
#' @param norm.vec.add Numeric vector. Additional normalization constant in form of vector involving
#'   all additional (in addition to \code{qValue}) normalization(s) like e.g. concentration, powder sample
#'   weight, number of scans, ...etc (\code{norm.vec.add = c(2000,0.5,2)}). \strong{Default}:
#'   \code{norm.vec.add = NULL}.
#' @param time.series Boolean, whether the input ASCII data come from the time series experiment
#'   with the additional \code{time} column, \strong{default}: \code{time.series = FALSE}
#' @param origin String/Character corresponding to \strong{origin} of the ASCII data, like from
#'   most common spectrometers (from which are data loaded automatically using the default parameters).
#'   Options are summarized in the following table (Any other specific `origin` may be added later) =>
#'   \tabular{rl}{
#'   \strong{String} \tab \strong{Description} \cr
#'   "xenon" \tab \strong{default} automatically loads data from `Xenon` software with dafault params. \cr
#'   "winepr" \tab automatically loads data from `WinEpr` software \cr
#'   "other" (arbitrary string) \tab general, loads any other `origin` data ( like `csv`, `txt`, `asc`) incl.
#'   data from other instrumental/spectrometer software. \strong{In such case all the parameters/arguments for}
#'   \code{readEPR_Exp_Specs} \strong{have to be set up accordingly}.
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
#'                   skip = 3,
#'                   qValue = 2000,
#'                   Nscans = 20,
#'                   m.mg = 10,
#'                   origin = "winepr")
#'
#' ## series of spectra (e.g. like time series) acquired by "winepr"
#' readEPR_Exp_Specs(path_to_ASC,
#'                   skip = 4,
#'                   col.names = c("B_G",
#'                                 "Slice",
#'                                 "dIepr_over_dB"),
#'                   time.series = TRUE,
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
#' @importFrom data.frame fread
#' @importFrom rlang .data quo_name :=
#' @importFrom stats na.omit
readEPR_Exp_Specs <- function(path_to_ASC,
                              sep = "auto",
                              skip = 1,
                              header = FALSE,
                              na.strings = NULL,
                              select = NULL,
                              drop = NULL,
                              encoding = "unknown",
                              fill = FALSE,
                              blank.lines.skip = FALSE,
                              col.names = c(
                                "index",
                                "B_G",
                                "dIepr_over_dB"
                              ),
                              col.char2num = FALSE,
                              x = 2,
                              Intensity = 3,
                              convert_B_G2mT = TRUE,
                              qValue = 1,
                              norm.vec.add = NULL,
                              time.series = FALSE,
                              origin = "xenon") {
  ## 'Temporary' processing variables
  B_G <- NULL
  B_mT <- NULL
  . <- NULL
  #
  ## general normalization
  norm.vec.add <- norm.vec.add %>% `if`(is.null(norm.vec.add),1, .)
  norm.const <- prod(sapply(norm.vec.add, function(n) 1/n))
  #
  ## basic `fread` parameters to read the spectral data
  if (origin == "winepr") {
    if (isFALSE(time.series)) {
      ## parameter definition
      sep <- sep %>% `if`(sep != "auto", "auto", .)
      header <- header %>% `if`(isTRUE(header),FALSE, .)
      skip <- skip %>% `if`(skip != 3,3, .)
      na.strings <- NULL
      select <- NULL
      drop <- NULL
      encoding <- encoding %>% `if`(encoding != "unknown", "unknown", .)
      fill <- fill %>% `if`(isTRUE(fill),FALSE, .)
      blank.lines.skip <- blank.lines.skip %>% `if`(isTRUE(blank.lines.skip), FALSE, .)

      #
    } else {
      ## parameter definition
      sep <- sep %>% `if`(sep != "auto", "auto", .)
      header <- header %>% `if`(isTRUE(header),FALSE, .)
      skip <- skip %>% `if`(skip != 4,4, .)
      fill <- fill %>% `if`(isFALSE(fill),TRUE, .)
      blank.lines.skip <- blank.lines.skip %>% `if`(isFALSE(blank.lines.skip),TRUE, .)
      na.strings <- na.strings %>% `if`(is.null(na.strings),
                                        c("Intensity","X [G]","Y []"), .)
      select <- NULL
      drop <- NULL
      encoding <- encoding %>% `if`(encoding != "unknown", "unknown", .)
      #
    }
  }
  if (origin == "xenon"){
    ## parameter definition
    sep <- sep %>% `if`(sep != "auto", "auto", .)
    header <- header %>% `if`(isTRUE(header),FALSE, .)
    skip <- skip %>% `if`(skip != 1,1, .)
    na.strings <- NULL
    select <- NULL
    drop <- NULL
    encoding <- encoding %>% `if`(encoding != "unknown", "unknown", .)
    fill <- fill %>% `if`(isTRUE(fill),FALSE, .)
    blank.lines.skip <- blank.lines.skip %>% `if`(isTRUE(blank.lines.skip), FALSE, .)
  }
  ## change any other `origin` accordingly
  if (origin != "winepr" & origin != "xenon"){
    sep = sep
    header = header
    skip = skip
    na.strings = na.strings
    select = select
    drop = drop
    col.names = col.names
    encoding = encoding
    fill = fill
    blank.lines.skip = blank.lines.skip
  }
  #
  ## basic data frame by `fread` incl. the above defined parameters
  spectrum.data.origin <- data.table::fread(path_to_ASC,
                                            sep = sep,
                                            header = header,
                                            skip = skip,
                                            na.strings = na.strings,
                                            select = select,
                                            drop = drop,
                                            col.names = col.names,
                                            encoding = encoding,
                                            fill = fill,
                                            blank.lines.skip = blank.lines.skip
  ) %>%
    `if`(origin == "winepr" & isTRUE(time.series),
         dplyr::filter(grepl("Slice",.data[[1]])), .) %>%
    `if`(origin == "winepr" & isTRUE(time.series),
         stats::na.omit(), .)
  #
  ## Condition to convert any character column to numeric format
  if (isTRUE(col.char2num)){
    for (i in seq(ncol(spectrum.data.origin))){
      if (class(spectrum.data.origin[[i]]) == "character"){
        spectrum.data.origin[[i]] <- as.double(spectrum.data.origin[[i]])
      }
    }
  }
  #

  ## TO BE COMPLETED !!

  ## TO BE COMPLETED !!

  if (isFALSE(time.series)) {
    if (x.unit == "Unitless") {
      x <- grep("g", col.names, value = TRUE)
    }
    if (x.unit == "G" || x.unit == "mT") {
      x <- grep("B|G|mT|BField|Field", col.names, value = TRUE)[[1]]
      ## `[[1]]` selection is due to fact that intensity
      ## could be `dIepr_over_dB` which also has `B` within
      ## the character string
      ## `|` \equiv `or`
    }
    if (x.unit == "MHz") {
      x <- grep("rf|RF|MHz|radio|Radio|freq|Freq", col.names, value = TRUE)
    }
  } else {
    if (x.unit == "Unitless") {
      x <- grep("g", col.names, value = TRUE)
    }
    if (x.unit == "G" || x.unit == "mT") {
      x <- grep("B|G|mT", col.names, value = TRUE)[[1]]
    }
    if (x.unit == "MHz") {
      x <- grep("rf|RF|MHz|radio|Radio|freq|Freq", col.names, value = TRUE)
    }
    time <- grep("time|Time|tim|Tim", col.names, value = TRUE)
  }
  #
  ## select intesity column character string
  Intensity <- grep("I|Intens|intens|MW_Abs|MW_Intens", col.names, value = TRUE)
  #
  ##
  if (origin == "xenon" || origin == "txt" || origin == "csv") {
    if (x.unit == "G") {
      ## For intesity 1.) Normalized intensity
      ## 2.) Overwrite the 'old' Intensity col
      ##     however, first off all it must be unquoted
      ##     therefore, !!rlang::quo_name(Intensity) := Norm_Intensity
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(
          B_mT = .data[[x]] / 10,
          !!rlang::quo_name(Intensity) := .data[[Intensity]] / (qValue * Nscans * m.mg * c.M)
        )
    }
    if (x.unit == "MHz") {
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(!!rlang::quo_name(Intensity) := .data[[Intensity]] / Nscans)
    }
    if (x.unit == "mT" || x.unit == "Unitless") {
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(!!rlang::quo_name(Intensity) := .data[[Intensity]] / (qValue * Nscans * m.mg * c.M))
    }
  }
  if (origin == "winepr") {
    if (x.unit == "G") {
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(
          B_mT = .data[[x]] / 10,
          !!rlang::quo_name(Intensity) := .data[[Intensity]] / (qValue * Nscans * m.mg * c.M),
          index = seq_len(length(.data[[Intensity]]))
        )
    }
    if (x.unit == "MHz") {
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(
          !!rlang::quo_name(Intensity) := .data[[Intensity]] / Nscans,
          index = seq_len(length(.data[[Intensity]]))
        )
    }
    if (x.unit == "mT" || x.unit == "Unitless") {
      spectrum.data <- spectrum.data.origin %>%
        dplyr::mutate(
          !!rlang::quo_name(Intensity) := .data[[Intensity]] / (qValue * Nscans * m.mg * c.M),
          index = seq_len(length(.data[[Intensity]]))
        )
    }
  }
  #
  return(spectrum.data)
  #
}
