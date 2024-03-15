#
#' Read the EPR Experimental ASCII or other Text-Based Data
#'
#'
#' @family Data Reading
#'
#'
#' @description The function is based on \code{\link[data.table]{fread}} with the purpose to read
#'   experimental EPR/ENDOR spectra or other original related (pre-processed) data from EPR spectrometers
#'   in ASCII format (e.g. like \code{.txt}, \code{.csv} or \code{.asc}).
#'
#'
#' @details
#'   Data are transformed into \code{data frames}, which can be easily processed by other R packages
#'   (e.g. by \pkg{tidyverse} system), afterwards. Spectral data (intensities) are normalized by the common
#'   experimental parameters like Q-factor, concentration...etc. ASCII files/tables depend on the origin/software
#'   used to acquire the EPR spectra. This is mirrored by \code{origin} parameter. Time series (time evolution
#'   of EPR spectra) can be read by the \code{time.series} parameter.
#'
#'
#' @inheritParams data.table::fread
#' @param path_to_ASC String, path to ASCII file/table (e.g. \code{.txt}, \code{.csv} or \code{.asc})
#'   with spectral data (\eqn{Intensity vs B}(Field) with additional 'index' and/or 'time' variables).
#'   The path can be also defined by \code{\link[base]{file.path}}.
#' @param col.names Character/String vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its units, \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"}, \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum abscissa)...etc, \strong{default}: \code{col.names = c("index","B_G",dIepr_over_dB)}.
#'   The default (for original \code{\link[data.table]{fread}}) is to use the header column if present or detected,
#'   or if not `"V"` followed by the column number.
#' @param x Numeric index related to \code{col.names} pointing to independent variable, which corresponds
#'   to abscissa (\eqn{x}-axis) in spectra or other plots.
#' @param x.unit Character/String ...TBC
#' @param Intensity Numeric index related to \code{col.names} pointing to `general` intensity,
#'   like derivative intensity (`dIepr_over_dB`), integral one (e.g. `single_Integ`), double or sigmoid
#'   integral (e.g. `Area`)...etc. This corresponds to column/vector which should be presented like
#'   \eqn{y}-axis in spectra or other plots.
#' @param time.series Numeric index related to \code{col.names} pointing to `time` column for time series
#'   EPR spectra changing upon time. If data contains simple relationship like \eqn{Area} vs \eqn{time}
#'   use \code{x} and \code{x.unit} parameters/arguments instead. This parameter/argument is dedicated
#'   to kinetic-like experiments. \strong{Default}: \code{time.series = NULL}.
#' @param convertB.unit Logical (\strong{default}: \code{convertB.unit = TRUE}) description...
#'   convert \eqn{B} in Gauss <=> millitesla...
#' @param qValue Numeric, Q value (quality factor, number) displayed at specific \code{dB} by spectrometer,
#'   in case of ` "Xenon" ` software the parameter is included in \code{.DSC} file, \strong{default}:
#'   \code{qValue = NULL}
#' @param norm.vec.add Numeric vector. Additional normalization constant in form of vector involving
#'   all additional (in addition to \code{qValue}) normalization(s) like e.g. concentration, powder sample
#'   weight, number of scans, ...etc (\code{norm.vec.add = c(2000,0.5,2)}). \strong{Default}:
#'   \code{norm.vec.add = NULL}.
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
#' @param ... additional arguments specified (see also \code{\link[data.table]{fread}}).
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
#' ## simple EPR spectrum acquired by "xenon"
#' ## and with `B` conversion "G" <=> mT
#' ## Loading the data
#' aminoxyl.data.path <-
#' load_data_example(file = "Aminoxyl_radical_a.txt")
#' aminoxyl.data.01 <- readEPR_Exp_Specs(aminoxyl.data.path,
#'                                       qValue = 2100)
#' ## preview
#' head(aminoxyl.data.01)
#' #
#' # simple EPR spectrum acquired by "xenon"
#' ## and without `B` conversion "G" <=> mT
#' aminoxyl.data.02 <- readEPR_Exp_Specs(aminoxyl.data.path,
#'                                       convertB.unit = FALSE,
#'                                       qValue = 2100)
#' ## preview
#' head(aminoxyl.data.02)
#' #
#' ## the simple spectrum acquired by "winepr"
#' ## (and 20 scans) on a 1 mM sample concentration:
#' ## Loading the data
#' TMPD.data.path <-
#' load_data_example(file = "TMPDA_specelchem_accu_b.asc")
#' TMPD.data <- readEPR_Exp_Specs(TMPD.data.path,
#'                                col.names = c("B_G","dIepr_over_dB"),
#'                                x = 1,
#'                                Intensity = 2,
#'                                qValue = 3500,
#'                                norm.vec.add = c(20,0.001),
#'                                origin = "winepr")
#' ## preview
#' head(TMPDA.data)
#' #
#' ## the ENDOR spectrum recorded by "xenon"
#' ## and 8 accumulation sweeps
#' ## loading the data
#' PNT.ENDOR.data.path <-
#' load_data_example(file = "PNT_ENDOR_a.txt")
#' PNT.ENDOR.data <-
#' readEPR_Exp_Specs(PNT.ENDOR.data.path,
#'                   col.names = c("index",
#'                                 "RF_MHz",
#'                                 "dIepr_over_dB"),
#'                   x = 2,
#'                   x.unit = "MHz",
#'                   Intensity = 3,
#'                   norm.vec.add = 8)
#' ## preview
#' head(PNT.ENDOR.data)
#' #
#' \dontrun{
#' ## EPR time series acquired by "winepr"
#' readEPR_Exp_Specs(path_to_ASC,
#'                   col.names = c("B_G",
#'                                 "Slice",
#'                                 "dIepr_over_dB"),
#'                   x = 1,
#'                   Intensity = 3,
#'                   time.series = 2,
#'                   origin = "winepr")
#' #
#' ## if no parameter intensity normalization
#' ## is required and a simple spectrum
#' ## was recorded by "xenon" software:
#' readEPR_Exp_Specs(path_to_ASC = file.path(".",
#'                                           "ASCII_Folder",
#'                                           "EPR_spectrum.txt"))
#' #
#' ## example for "xenon" time series experiment
#' ## (evolution of EPR spectra in time, e.g. in case of
#' ## EPR spectroelectrochemistry or photochemistry):
#' ## together with `B` conversion "G" <=> mT
#' ## and intensity normalized against `qValue`
#' readEPR_Exp_Specs(path_to_ASC,
#'                   col.names = c("index",
#'                                 "B_G",
#'                                 "time_s",
#'                                 "dIepr_over_dB"),
#'                   x = 2,
#'                   Intensity = 4,
#'                   qValue = 2800,
#'                   time.series = 3)
#' #
#' ## reading simple spectrum from the new "magnettech"
#' ## acquisition software
#' readEPR_Exp_Specs("./Data/EPR_spectrum.csv",
#'                   skip = 88,
#'                   col.names = c("B_mT","dIepr_over_dB"),
#'                   x = 1,
#'                   x.unit = "mT",
#'                   Intensity = 2,
#'                   origin = "magnettech")
#' #
#' ## reading file data from (and pre-processed) by "xenon" software
#' ## corresponding to kinetics where `Area` and `time` are
#' ## data columns, however there are many columns and one should
#' ## select only the two mentioned above + normalize `Area`
#' ## against the `qValue`
#' readEPR_Exp_Specs("./EPR_ASCII/Quant_kinet_a.txt",
#'                   header = T,
#'                   fill = T,
#'                   select = c(3,7),
#'                   col.names = c("time_s","Area"),
#'                   x.unit = "s",
#'                   x = 1,
#'                   Intensity = 2,
#'                   qValue = 1700)
#' }
#'
#'
#' @export
#'
#'
#' @importFrom data.table fread
#' @importFrom rlang .data quo_name :=
#' @importFrom stats na.omit
readEPR_Exp_Specs <- function(path_to_ASC,
                              sep = "auto",
                              skip = 1,
                              header = FALSE,
                              col.names = c(
                                "index",
                                "B_G",
                                "dIepr_over_dB"
                              ),
                              x = 2,
                              x.unit = "G",
                              Intensity = 3,
                              time.series = NULL,
                              convertB.unit = TRUE,
                              qValue = NULL,
                              norm.vec.add = NULL,
                              origin = "xenon",
                              ...) {
  ## 'Temporary' processing variables
  B_G <- NULL
  B_mT <- NULL
  . <- NULL

  #
  ## general normalization
  qValue <- qValue %>% `if`(is.null(qValue), 1, .)
  norm.vec.add <- norm.vec.add %>% `if`(is.null(norm.vec.add), 1, .)
  norm.multiply.const <- prod(sapply(norm.vec.add, function(n) 1 / n))
  norm.multiply.qValue <- 1 / qValue
  #
  ## Ellipsis argument list definition
  # args <- list(...)
  #
  ## basic `fread` parameters to read the spectral data
  ## additional arguments see `?data.table::fread`
  if (origin == "winepr") {
    if (is.null(time.series)) {
      ## parameter definition
      sep <- sep %>% `if`(sep != "auto", "auto", .)
      header <- header %>% `if`(isTRUE(header), FALSE, .)
      skip <- skip %>% `if`(skip != 3, 3, .)
      #
      ## following defined by `...`
      #
      # na.strings <- NULL
      # select <- NULL
      # drop <- NULL
      # encoding <- "unknown"
      # fill <- FALSE
      # blank.lines.skip <- FALSE
      # colClasses <- NULL

      #
    } else {
      ## parameter definition
      sep <- sep %>% `if`(sep != "auto", "auto", .)
      header <- header %>% `if`(isTRUE(header), FALSE, .)
      skip <- skip %>% `if`(skip != 4, 4, .)
      #
      ## following defined by `...`
      #
      # fill <- TRUE
      # blank.lines.skip <- TRUE
      # na.strings <- c("Intensity", "X [G]", "Y []")
      # select <- NULL
      # drop <- NULL
      # encoding <- "unknown"
      # colClasses <- NULL
      #
    }
  }
  if (origin == "xenon") {
    ## parameter definition
    sep <- sep %>% `if`(sep != "auto", "auto", .)
    header <- header
    skip <- skip
    #
    ## following defined by `...`
    #
    # na.strings <- NULL
    # select <- NULL
    # drop <- NULL
    # encoding <- "unknown"
    # fill <- FALSE
    # blank.lines.skip <- FALSE
    # colClasses <- NULL
  }
  ## change any other `origin` accordingly
  if (origin != "winepr" & origin != "xenon") {
    sep <- sep
    header <- header
    skip <- skip

  }
  #
  ## basic data frame by `fread` incl. the above defined parameters
  #
  spectrum.data.origin <-
    data.table::fread(file = path_to_ASC,
                      sep = sep,
                      header = header,
                      skip = skip,
                      col.names = col.names,
                      ...
  )
  ## condition for `winepr`
  if (origin == "winepr" & !is.null(time.series)) {
    spectrum.data.origin <- spectrum.data.origin %>%
      dplyr::filter(!grepl("Slice", .data[[colnames(spectrum.data.origin)[1]]])) %>%
      stats::na.omit()
    ## in order to be sure that this column will appear as numeric
    spectrum.data.origin[[1]] <- as.double(spectrum.data.origin[[1]])
  }
  #
  ## Condition to convert any character column to numeric format
  ## to check character => `inherits(x,"character")`
  ## THIS IS REPLACED BY `fread` `colClasses`
  # if (isTRUE(col.char2num)) {
  #   for (i in seq(ncol(spectrum.data.origin))) {
  #     if (inherits(spectrum.data.origin[[i]],"character")) {
  #       spectrum.data.origin[[i]] <- as.double(spectrum.data.origin[[i]])
  #     }
  #   }
  # }
  #
  ## `Intensity` and `x` as well as `time` column
  ## character string definitions
  # if (isTRUE(header)) {
  #   col.names <- colnames(spectrum.data.origin)
  # }
  IntensityString <- col.names[Intensity] ## `Intensity` string
  xString <- col.names[x] ## `x` string
  # timeAxis <- col.names[time.series]
  ## new `spectra.data`
  #
  ## Common EPR Spectra
  ## Unit condition
  G.unit.cond <- if (x.unit == "G") TRUE else FALSE
  ## intial `B`/`Field` Character string condition
  if (grepl("B|BF",xString)){
    xString.init <- "B_"
  }
  if (grepl("Fiel|fiel",xString)){
    xString.init <- "Field_"
  }
  #
  if (x.unit == "G" || x.unit == "mT") {
    if (isTRUE(convertB.unit)){
      spectra.data <- spectrum.data.origin %>%
        dplyr::mutate(!!rlang::quo_name(paste0(xString.init,
                                               switch(2-isTRUE(G.unit.cond),"mT","G"))) :=
                        .data[[xString]] * switch(2 - isTRUE(G.unit.cond),
                               1 / 10,
                               10
                        )) %>%
        dplyr::mutate(!!rlang::quo_name(IntensityString) := .data[[IntensityString]] *
                        norm.multiply.qValue * norm.multiply.const)
    } else{
      spectra.data <- spectrum.data.origin %>%
        dplyr::mutate(!!rlang::quo_name(IntensityString) := .data[[IntensityString]] *
                        norm.multiply.qValue * norm.multiply.const)
    }
  }
  ## Any other Spectra like ENDOR or with g-Value or Intensity/Area vs time
  ## or Intensity vs power relationship
  if (x.unit != "G" & x.unit != "mT") {
    spectra.data <- spectrum.data.origin %>%
      dplyr::mutate(!!rlang::quo_name(IntensityString) := .data[[IntensityString]] *
        norm.multiply.qValue * norm.multiply.const)
  }
  #
  ## TO BE COMPLETED !! for `winepr` system and time.series => convert `Slice` into time
  #
  return(spectra.data)
  #
}
