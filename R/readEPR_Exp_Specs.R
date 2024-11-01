#
#' Read the Experimental ASCII or other Text-Based EPR Data.
#'
#'
#' @family Data Reading
#'
#'
#' @description The function is based on the \code{\link[data.table]{fread}} with the purpose to read
#'   the experimental EPR/ENDOR spectra or other original (pre-processed) data from the EPR spectrometers
#'   in tabular ASCII format (such as \code{.txt}, \code{.csv} or \code{.asc}). Default argument values correspond
#'   to data reading from \emph{Xenon} files (see the argument \code{origin}).
#'
#'
#' @details
#'   ASCII data are transformed into \emph{R} data frames, which can be then easily processed by the actual or other
#'   R packages, e.g. \href{https://dplyr.tidyverse.org/}{dplyr}), afterwards. Spectral intensities are automatically
#'   normalized by the common experimental parameters like Q-factor, concentration, weight...etc.
#'   These are defined by the two arguments:
#'   \code{qValue} and \code{norm.vec.add}. The latter actually corresponds to values of the above-mentioned
#'   quantities represented by the vector. If \code{qValue = NULL} (actually corresponding to \code{1}),
#'   the Q-value can be also defined as a component of the \code{norm.vec.add}. Finally, the normalized intensity
#'   is calculated by the following expression (depending \code{qValue} and/or \code{norm.vec.add}):
#'   \deqn{dI_{EPR} / dB = Original~Intensity \, (1/qValue)}
#'   or
#'   \deqn{dI_{EPR} / dB = Original~Intensity \, (1/qValue) \, \prod_{k} 1/(norm.vec.add[k])}
#'   where \eqn{k} is iterating through all components of the \code{norm.vec.add}.
#'   The structure of ASCII files/tables depends on the origin/software used to acquire the EPR spectra.
#'   This is mirrored mainly by the \code{origin} and \code{data.structure} arguments. Default arguments
#'   are set to read the data from \emph{Xenon} acquisition/processing software. However, additional
#'   \code{origins} can be set like \code{origin = "winepr"} or \code{origin = "magnettech"} or even
#'   any arbitrary string e.g. \code{origin = "csv"} (see also the \code{origin} argument). For the latter,
#'   all arguments must be set accordingly, as already demonstrated in \code{Examples}.
#'   Time series (time evolution of EPR spectra/kinetics) is defined by the \code{time.series.id} argument.
#'   In such case the ASCII data table also contains additional column either with recorded
#'   time (see also \code{\link{correct_time_Exp_Specs}}) or with slice number for each spectrum.
#'
#'
#' @inheritParams data.table::fread
#' @param path_to_ASC Character string, path to ASCII file/table (e.g. in \code{.txt}, \code{.csv}
#'   or \code{.asc} format) with the spectral data (\eqn{Intensity} vs \eqn{B}, Field) including additional
#'   \code{index} and/or \code{time} variables). The path can be also defined by the \code{\link[base]{file.path}} function.
#' @param col.names Character string vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its unit, \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"}, \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum \eqn{x}-axis)...etc, \strong{default}: \code{col.names = c("index","B_G",dIepr_over_dB)}.
#'   For spectral time series \code{col.names} must include \code{"T(t)ime"} or \code{"S(s)lice"} character
#'   string in order to identify the corresponding time column/variable in the original ASCII file.
#'   The default (for the original \code{\link[data.table]{fread}}) is to use the header column
#'   if present or detected, or if not the name is denoted as \code{"V"} followed by the column number.
#' @param x.id Numeric index related to \code{col.names} vector pointing to independent variable, which corresponds
#'   to \eqn{x}-axis in the spectra or other plots.
#' @param x.unit Character string corresponding to original \code{x} variable/column unit, such as \code{"G"},
#'   \code{"mT"} or \code{"MHz"}.
#' @param Intensity.id Numeric index related to \code{col.names} vector pointing to \code{general} intensity,
#'   like derivative intensity (\code{dIepr_over_dB}), integral one (e.g. \code{single_Integ}), double or sigmoid
#'   integral (e.g. \code{Area})...etc. This corresponds to column/vector which should be presented like
#'   \eqn{y}-axis in the EPR spectra or other plots.
#' @param time.series.id Numeric index related to \code{col.names} vector and pointing to \code{time} column for time series
#'   EPR spectra. If data contains simple relationship like \eqn{Area} vs \eqn{time}
#'   use \code{x} and \code{x.unit} parameters/arguments instead (see also examples). This argument is dedicated
#'   to kinetic-like experiments. \strong{Default}: \code{time.series.id = NULL} (see also \code{data.structure}
#'   argument).
#' @param convertB.unit Logical (\strong{default}: \code{convertB.unit = TRUE}) whether upon reading an automatic
#'   conversion between \code{G} and \code{mT} should be performed. If default is chosen, a new column/variable
#'   \eqn{B} in \code{mT}/\code{G} is created.
#' @param qValue Numeric, Q value (quality factor, number) displayed at specific \code{dB} by the spectrometer,
#'   in case of \emph{Xenon} or \emph{new Magnettech} software the parameter is included in \code{.DSC}/\code{.dsc} file,
#'   \strong{default}: \code{qValue = NULL}, which actually corresponds to value \code{1}.
#' @param norm.vec.add Numeric vector. Additional normalization constant in the form of vector, including
#'   all additional (in addition to \code{qValue}) normalization(s) like concentration, powder sample
#'   weight, number of scans, ...etc. (e.g. \code{norm.vec.add = c(2000,0.5,2)}). \strong{Default}:
#'   \code{norm.vec.add = NULL}, which actually corresponds to value \code{1}. If \code{qValue = NULL},
#'   the Q-factor/value might be also included in the \code{norm.vec.add}.
#' @param origin Character string corresponding to \strong{origin} of the ASCII data, like from
#'   most common spectrometers (from which the data are loaded automatically using the default parameters).
#'   Options are summarized in the following table (any other specific \code{origin} may be added later) =>
#'   \tabular{rl}{
#'   \strong{String} \tab \strong{Description} \cr
#'   "xenon" \tab \strong{default} automatically loads data from the "Xenon" software with default params. \cr
#'   "winepr" \tab automatically loads data from the "WinEpr" software. \cr
#'   "magnettech" \tab automatically loads data from the new "Magnettech" software (ESR5000 [11-0422]). \cr
#'   "other" (arbitrary string, e.g. "csv") \tab general, loads any other original
#'   data like \code{csv}, \code{txt}, \code{asc} incl. also data from other instrumental/spectrometer software.
#'   \strong{In such case, all the arguments for} \code{readEPR_Exp_Specs} \strong{have to be set up accordingly}. \cr
#'   }
#' @param data.structure Character string referring to structure of the ASCII data. Common spectral data files
#'   with \eqn{Intensity} vs. \eqn{x(B,g,RF(\text{MHz}))} and/or \eqn{time} columns (including the spectral
#'   time series) correspond to \code{data.structure = "spectra"} (\strong{default}). For more complex
#'   ASCII data structure (such as spectral series processed by the acquisition spectrometer software,
#'   see \code{Examples}, or any other data) put \code{data.structure = "others"}. \strong{In such case, all the arguments for}
#'   the \code{readEPR_Exp_Specs} \strong{have to be set up accordingly}. The \code{data.structure} argument
#'   (assuming \code{time.series.id = NULL}) is helping to simplify the reading of \code{"spectra"} by the predefined
#'   \code{origin} argument.
#' @param ... additional arguments specified (see also \code{\link[data.table]{fread}}).
#'
#'
#' @return Data frame/table consisting of the magnetic flux density
#'   column \code{B_mT} in millitesla (as well as \code{B_G} in gauss) or \code{RF_MHz} (in case of ENDOR spectrum)
#'   or unitless \code{g-factor} and of the derivative intensity column (\code{dIepr_over_dB}) or any other
#'   intensities (like integrated spectral form) in \code{procedure defined unit}
#'   (see \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6803776/}{p.d.u.}),
#'   which is normalized by the above-described parameters
#'   and finally the \code{index} and/or a \code{time} (in the case of time series experiment) columns
#'   are displayed as well.
#'
#'
#' @examples
#' ## simple EPR spectrum acquired by "xenon"
#' ## and with `B` conversion "G" <=> "mT"
#' ## Loading the data
#' aminoxyl.data.path <-
#'   load_data_example(file = "Aminoxyl_radical_a.txt")
#' aminoxyl.data.01 <-
#'   readEPR_Exp_Specs(aminoxyl.data.path,
#'                     qValue = 2100)
#' ## preview
#' head(aminoxyl.data.01)
#' #
#' # simple EPR spectrum acquired by "xenon"
#' ## and without `B` conversion "G" <=> "mT"
#' aminoxyl.data.02 <-
#'   readEPR_Exp_Specs(aminoxyl.data.path,
#'                     convertB.unit = FALSE,
#'                     qValue = 2100)
#' ## preview
#' head(aminoxyl.data.02)
#' #
#' ## the simple spectrum acquired by "winepr"
#' ## (and 20 scans) on a 1 mM sample concentration:
#' ## Loading the data
#' TMPD.data.path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.asc")
#' TMPD.data <-
#'   readEPR_Exp_Specs(TMPD.data.path,
#'                     col.names = c("B_G","dIepr_over_dB"),
#'                     qValue = 3500,
#'                     norm.vec.add = c(20,0.001),
#'                     origin = "winepr")
#' ## preview
#' head(TMPD.data)
#' #
#' ## the ENDOR spectrum recorded by "xenon"
#' ## and 8 accumulation sweeps
#' ## loading the data
#' PNT.ENDOR.data.path <-
#'   load_data_example(file = "PNT_ENDOR_a.txt")
#' PNT.ENDOR.data <-
#'   readEPR_Exp_Specs(PNT.ENDOR.data.path,
#'                     col.names = c("index",
#'                                   "RF_MHz",
#'                                   "dIepr_over_dB"),
#'                     x.unit = "MHz",
#'                     norm.vec.add = 8)
#' ## preview
#' head(PNT.ENDOR.data)
#' #
#' ## reading the (pre-processed) data file
#' ## (data.structure = "mixed") from (by) the "Xenon" software
#' ## corresponding to kinetics with `Area` and `time`
#' ## columns/variables , these two have to be selected
#' ## from several others + normalize `Area`
#' ## against the `qValue` (first of all load the path
#' ## of package example file)
#' triarylamine.rc.decay.path <-
#'   load_data_example("Triarylamine_radCat_decay_a.txt")
#' ## data
#' triarylamine.rc.decay.data <-
#'   readEPR_Exp_Specs(path_to_ASC = triarylamine.rc.decay.path,
#'                     header = TRUE,
#'                     fill = TRUE,
#'                     select = c(3,7),
#'                     col.names = c("time_s","Area"),
#'                     x.unit = "s",
#'                     x.id = 1,
#'                     Intensity.id = 2,
#'                     qValue = 1700,
#'                     data.structure = "others") %>%
#'     na.omit()
#' ## preview
#' head(triarylamine.rc.decay.data)
#' #
#' ## reading the "magnettech" file example,
#' ## first of all load the path of package example file
#' acridineRad.data.path <-
#'   load_data_example("AcridineDeriv_Irrad_365nm.csv.zip")
#' ## unzip
#' acridineRad.data <-
#'   unzip(acridineRad.data.path,
#'         files = c("AcridineDeriv_Irrad_365nm.csv"),
#'         exdir = tempdir())
#' ## reading
#' acridineRad.data <-
#'   readEPR_Exp_Specs(acridineRad.data,
#'                     col.names = c("B_mT","dIepr_over_dB"),
#'                     x.unit = "mT",
#'                     origin = "magnettech")
#' ## preview
#' head(acridineRad.data)
#' #
#' \dontrun{
#' ## EPR time series acquired by "Winepr"/"WinEpr"
#' readEPR_Exp_Specs(path_to_ASC,
#'                   col.names = c("B_G",
#'                                 "Slice",
#'                                 "dIepr_over_dB"),
#'                   origin = "Winepr")
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
#'                   qValue = 2800)
#' #
#' ## read `.csv` file which is an output from online
#' ## converter:
#' ## https://www.spectra.tools/bin/controller.pl?body=Xepr2gfac
#' readEPR_Exp_Specs("data.csv",
#'                   skip = 0,
#'                   col.names = c("B_G",
#'                                 "g_Value",
#'                                 "dIepr_over_dB"),
#'                   x.id = 1,
#'                   Intensity.id = 3,
#'                   origin = "csv")
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
                              x.id = 2,
                              x.unit = "G",
                              Intensity.id = 3,
                              time.series.id = NULL,
                              convertB.unit = TRUE,
                              qValue = NULL,
                              norm.vec.add = NULL,
                              origin = "xenon",
                              data.structure = "spectra",
                              ...) {
  ## 'Temporary' processing variables
  B_G <- NULL
  B_mT <- NULL
  . <- NULL
  #
  ## setting up the number of digits, general definition for all variables =>
  options(digits = 8)
  #
  ## general normalization
  qValue <- qValue %>% `if`(is.null(qValue), 1, .)
  norm.vec.add <- norm.vec.add %>% `if`(is.null(norm.vec.add), 1, .)
  norm.multiply.const <- prod(sapply(norm.vec.add, function(n) 1 / n))
  norm.multiply.qValue <- 1 / qValue
  #
  ## Ellipsis argument list definition => JUST A NOTE
  # args <- list(...)
  #
  ## origin strings vectors to define "origin" conditions =>
  winepr.string <- c("winepr","Winepr","WinEpr","WINEPR","WinEPR","winEPR")
  xenon.string <- c("xenon","Xenon","XENON")
  magnettech.string <- c("magnettech","Magnettech","MagnetTech",
                         "magnetTech","MAGNETTECH","magnetech",
                         "Magnetech","MAGNETECH")
  ## previous strings also with single "t"/"T" excepting mistakes :-)
  #
  ## basic `fread` parameters to read the spectral data
  ## additional arguments see `?data.table::fread`
  #
  ## condition for time series spectra
  time.series.cond <- any(grepl("Time|time|Slice|slice",col.names))
  #
  ## string to stop the reading if `data structure == "mixed"`
  ## and `time.series.id != NULL` =>
  stop.reading.structure.time <-
    " For `data.structure = others` the `time.series.id` is not defined !\n
      Please put `time.series.id = NULL` with the `x.id` \n
      and `x.unit` corresponding to `time` column (if present) ! \n
      Additional arguments must be set up, accordingly ! "
  #
  # DEFAULT ARGUMENTS FOR INDIVIDUAL ORIGINS
  ## --------------------- WINEPR -----------------------------
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
    if (data.structure == "spectra") {
      ## parameter definition (automatize value assignments)
      ## ASSUMING USER CAN MAKE MISTAKES :-)
      if (isFALSE(time.series.cond)) {
        sep <- sep %>% `if`(sep != "auto", "auto", .)
        header <- header %>% `if`(isTRUE(header), FALSE, .)
        skip <- skip %>% `if`(skip != 3, 3, .)
        x.id <- x.id %>% `if`(x.id != 1, 1, .)
        Intensity.id <- Intensity.id %>% `if`(Intensity.id != 2, 2, .)
        time.series.id <- time.series.id %>%
          `if`(!is.null(time.series.id), NULL, .)
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
        sep <- sep %>% `if`(sep != "auto", "auto", .)
        header <- header %>% `if`(isTRUE(header), FALSE, .)
        skip <- skip %>% `if`(skip != 4, 4, .)
        x.id <- x.id %>% `if`(x.id != 1, 1, .)
        Intensity.id <- Intensity.id %>%
          `if`(Intensity.id != 3, 3, .)
        time.series.id <- time.series.id %>%
          `if`(time.series.id != 2 || is.null(time.series.id), 2, .)
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
    } else {
      if (!is.null(time.series.id)) {
        stop(stop.reading.structure.time)
      } else {
        sep <- sep
        header <- header
        skip <- skip
        x.id <- x.id
        Intensity.id <- Intensity.id
      }
    }
    #
  }
  #
  ## ---------------------- XENON -------------------------------
  if (any(grepl(paste(xenon.string,collapse = "|"),origin))) {
    if (data.structure == "spectra") {
      ## parameter definition (automatize value assignments)
      ## ASSUMING USER CAN MAKE MISTAKES :-)
      sep <- sep %>% `if`(sep != "auto", "auto", .)
      header <- header
      skip <- skip
      if (isFALSE(time.series.cond)) {
        x.id <- x.id %>% `if`(x.id != 2, 2, .)
        Intensity.id <- Intensity.id %>%
          `if`(Intensity.id != 3, 3, .)
        time.series.id <- time.series.id %>%
          `if`(!is.null(time.series.id), NULL, .)
      } else {
        x.id <- x.id %>% `if`(x.id != 2, 2, .)
        Intensity.id <- Intensity.id %>%
          `if`(Intensity.id != 4, 4, .)
        time.series.id <- time.series.id %>%
          `if`(time.series.id != 3 || is.null(time.series.id), 3, .)
      }
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
    } else {
      if (!is.null(time.series.id)) {
        stop(stop.reading.structure.time)
      } else {
        sep <- sep
        header <- header
        skip <- skip
        x.id <- x.id
        Intensity.id <- Intensity.id
      }
    }
   #
  }
  #
  ## -------------------- MAGNETTECH ---------------------------
  if (any(grepl(paste(magnettech.string,collapse = "|"),origin))){
    if (data.structure == "spectra") {
      ## parameter definition (automatize value assignments)
      ## ASSUMING USER CAN MAKE MISTAKES :-)
      sep <- sep %>% `if`(sep != "auto", "auto", .)
      header <- header %>% `if`(isTRUE(header), FALSE, .)
      skip <- skip %>% `if`(skip != 88, 88, .)
      #
      if (isFALSE(time.series.cond)) {
        x.id <- x.id %>% `if`(x.id != 1, 1, .)
        Intensity.id <- Intensity.id %>%
          `if`(Intensity.id != 2, 2, .)
        time.series.id <- time.series.id %>%
          `if`(!is.null(time.series.id), NULL, .)
      } else {
        if (is.null(time.series.id)) {
          stop("Time series column is not defined ! \n
             Please, specify the `id` of the column/variable,\n
             or create it to proceed !! ")
        } else {
          x.id <- x.id
          Intensity.id <- Intensity.id
          time.series.id <- time.series.id
        }
      }
    } else {
      if (!is.null(time.series.id)) {
        stop(stop.reading.structure.time)
      } else {
        sep <- sep
        header <- header
        skip <- skip
        x.id <- x.id
        Intensity.id <- Intensity.id
      }
    }
   #
  }
  #
  ## ------------------------ OTHERS ---------------------------
  ## change any other `origin` accordingly
  if (!any(grepl(paste(winepr.string,collapse = "|"),origin)) &
      !any(grepl(paste(xenon.string,collapse = "|"),origin)) &
      !any(grepl(paste(magnettech.string,collapse = "|"),origin))) {
    #
    ## condition for data structure
    data.structure.cond <- ifelse(data.structure == "spectra",TRUE,FALSE)
    #
    sep <- sep
    header <- header
    skip <- skip
    x.id <- x.id
    Intensity.id <- Intensity.id
    time.series.id <- switch(2-data.structure.cond,
                             time.series.id,
                             NULL)

  }
  #
  ## ============ SPECTRA/DATA READING AND NORMALIZING THE INTENSITY ===============
  #
  ## condition for special case `winepr` and `time.series`
  if (any(grepl(paste(winepr.string,collapse = "|"),origin)) &
      !is.null(time.series.id)) {
    spectrum.data.origin <-
      data.table::fread(file = path_to_ASC,
                        sep = sep,
                        header = header,
                        skip = skip,
                        col.names = col.names,
                        fill = T,
                        blank.lines.skip = T,
                        na.strings = c("Intensity","X [G]","Y []")) %>%
      ## filter out all rows containing "Slice|slice" in "B|Field" column
      dplyr::filter(!grepl("Slice|slice", .data[[grep(pattern = "BG|B_G|B-G|Field",
                                                col.names,value = TRUE)]])) %>%
      stats::na.omit()
    ## in order to be sure that this column will appear as numeric
    spectrum.data.origin[[grep(pattern = "BG|B_G|B-G|Field",col.names,value = TRUE)]] <-
      as.double(spectrum.data.origin[[grep(pattern = "BG|B_G|B-G|Field",col.names,value = TRUE)]])
  } else {
    ## basic data frame by general function (`fread`) incl. the above defined parameters
    spectrum.data.origin <-
      data.table::fread(file = path_to_ASC,
                        sep = sep,
                        header = header,
                        skip = skip,
                        col.names = col.names,
                        ...
      )
  }
  #
  ## Condition to convert any character column to numeric format
  ## to check character => `inherits(x,"character")` => JUST A NOTE
  #
  IntensityString <- col.names[Intensity.id] ## `Intensity` string
  xString <- col.names[x.id] ## `x` string
  # timeAxis <- col.names[time.series.id]
  ## new `spectra.data`
  #
  ## Common EPR Spectra
  ## Unit condition
  G.unit.cond <- if (x.unit == "G") TRUE else FALSE
  ## intial `B`/`Field` Character string condition
  if (grepl("B|BF|BG|B_G|B-G",xString)){
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
