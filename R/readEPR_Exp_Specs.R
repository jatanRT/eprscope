#
#' Reading of the Experimental ASCII or Binary EPR Spectra/Data.
#'
#'
#' @family Data Reading
#'
#'
#' @description Based on the \code{\link[data.table]{fread}} or \code{\link[base]{readBin}} R functions,
#'   the experimental EPR/ENDOR spectra (referred to as 1D- or 2D-Experiments) or other original (pre-processed)
#'   data from the EPR spectrometers are transformed into data frames (tables). The function can read several data formats
#'   such as \code{.txt}, \code{.csv}, \code{.asc}, \code{.DTA} as well as \code{.spc}. The latter two extensions,
#'   corresponding to binary files, require information (instrumental parameters of the acquired spectra/data)
#'   provided by the \code{.DSC}/\code{.dsc} or \code{.par} files, respectively (see the \code{path_to_dsc_par}
#'   as well as \code{path_to_ygf} arguments description). Because the original file structure depends
#'   on the EPR spectrometer acquisition software or data processing, the \code{origin} argument is necessary
#'   to specify the data/file source. Default function arguments are related to \code{origin ="xenon"}.
#'
#'
#' @details
#'   Right after the instrumental or pre-processed data/files are transformed into data frames,
#'   they can be easily handled by the actual or additional R packages, e.g. by \href{https://dplyr.tidyverse.org/}{dplyr}),
#'   afterwards. Spectral intensities are normalized by the common experimental parameters like Q-factor, concentration,
#'   weight...etc. These are defined by the two arguments:
#'   \code{qValue} and \code{norm.vec.add}. The latter actually corresponds to values of the above-mentioned
#'   quantities represented by the vector. If \code{qValue = NULL} (actually corresponding to \code{1}),
#'   it can be also defined as a component of the \code{norm.vec.add}. Finally, the normalized intensity
#'   is calculated by the following expression (depending on the \code{qValue} and/or \code{norm.vec.add}):
#'   \deqn{dI_{EPR} / dB = Original~Intensity \, (1/qValue)}
#'   or
#'   \deqn{dI_{EPR} / dB = Original~Intensity \, (1/qValue) \, \prod_{k} 1/(norm.vec.add[k])}
#'   or
#'   \deqn{dI_{EPR} / dB = Original~Intensity \, \prod_{k} 1/(norm.vec.add[k])}
#'   where the \eqn{k} is iterating through all components of the \code{norm.vec.add}.
#'   The structure of files depends on the origin/software used to acquire the EPR spectra.
#'   This is mainly mirrored by the \code{origin} and \code{data.structure} arguments. Default arguments
#'   are set to read the data from \emph{Xenon} acquisition/processing software. However, additional
#'   \code{origins} can be set like \code{origin = "winepr"} or \code{origin = "magnettech"} or even
#'   any arbitrary string e.g. \code{origin = "csv"} (see also description of the \code{origin} argument).
#'   For the latter, all arguments must be set accordingly, as already demonstrated in \code{Examples}.
#'   When reading the spectrometer files, any 2D-experiment (e.g. time/temperature/microwave power series)
#'   can be loaded as well. For such purpose, the reading/loading of the data
#'   must be activated by the \code{var2nd.series.id} argument (which is \code{NULL} by default
#'   to load the 1D-experiments), pointing to \code{col.names} element index in order to define relevant
#'   column of the returned data frame. For example, if the second variable series corresponds
#'   to time (in seconds) column: e.g. \code{col.names = c("index","B_G","time_s","dIepr_over_dB")},
#'   the \code{id} must be defined as \code{var2nd.series.id = 3}.
#'
#'
#' @inheritParams data.table::fread
#' @param path_to_file Character string, path to any spectrometer/instrumental file,
#'   having one the following extensions: \code{.txt}, \code{.csv}, \code{.asc}, \code{.DTA} or \code{.spc},
#'   including the 1D- (e.g. \eqn{Intensity} vs \eqn{B}, Field) or 2D-experimental
#'   (e.g. \eqn{Intensity} vs \eqn{B} vs \eqn{time}) EPR data. The path can be also defined
#'   by the \code{\link[base]{file.path}} function.
#' @param path_to_dsc_par Character string, path (also provided by \code{\link[base]{file.path}})
#'   to \code{.DSC/.dsc} (\code{origin = "xenon"}/\code{origin = "magnettech"}) or \code{.par} (\code{origin = "winepr"})
#'   ASCII \code{text} file, including instrumental parameters of the recorded spectra
#'   (corresponding to the previous argument) and provided by the EPR machine.
#'   \strong{Default}: \code{path_to_dsc_par = NULL}. The latter assignment actually means that the argument
#'   automatically inherits the \code{path_to_file}, however with the appropriate extension
#'   (\code{.DSC/.dsc} or \code{.par}). In other words, the function is looking for the same
#'   file name like in \code{path_to_file} in the working directory. If the file does not exist, it will ask
#'   to provide/define the right file path.
#' @param path_to_ygf Character string, path (also provided by \code{\link[base]{file.path}})
#'   to binary \code{.YGF} file (\code{origin = "xenon"}/\code{origin = "magnettech"}), storing the values of the 2nd
#'   independent variable in the spectral series like time, temperature, microwave power, ...etc (different from magnetic
#'   flux density and EPR intensity, see also \code{Details} and/or the \code{var2nd.series.id} argument description
#'   for 2D experiments). The latter assignment actually means that the argument automatically inherits
#'   the \code{path_to_file}, however with the appropriate extension \code{.YGF}. In other words, the function is looking
#'   for the same file name like in \code{path_to_file} in the working directory. If the file does not exist, the function
#'   automatically grabs those values based on the information provided by the \code{.DSC/.dsc}
#'   (\code{origin = "xenon"}/\code{origin = "magnettech"}) or \code{.par} (\code{origin = "winepr"}) files (see the argument
#'   \code{path_to_dsc_par} description).
#' @param col.names Character string vector, corresponding to desired column/variable names/headers of the returned
#'   data frame/table. A safe rule of thumb is to use column names incl. physical quantity notation
#'   with its unit, \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"}, \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum \eqn{x}-axis). \strong{Default}: \code{col.names = c("index","B_G",dIepr_over_dB)}.
#'   For spectral 2D-series \code{col.names} must include character string (such as \code{"time_s"} or \code{"T_K"})
#'   in order to identify the corresponding quantity for the series in the original file (please refer also
#'   to the \code{var2nd.series.id}). Additional \code{\link[data.table]{fread}} documentation might be helpful
#'   to read the ASCII text files.
#' @param x.id Numeric index related to \code{col.names} vector, pointing to the independent variable, which corresponds
#'   to \eqn{x}-axis in the spectra or other plots (e.g. \eqn{B} or \eqn{\nu_{\text{RF}}}).
#'   \strong{Default}: \code{x.id = 2} (for \emph{Xenon}).
#' @param x.unit Character string, corresponding to original \code{x} variable/column unit, such as \code{"G"},
#'   \code{"mT"} or \code{"MHz"}.
#' @param Intensity.id Numeric index related to \code{col.names} vector, pointing to general intensity of EPR spectrum,
#'   like derivative intensity (\code{dIepr_over_dB}), integral one (e.g. \code{single_Integ}), double or sigmoid
#'   integral (e.g. \code{Area})...etc. This corresponds to column/vector which should be presented on the
#'   \eqn{y}-axis in the EPR spectra or other plots. \strong{Default}: \code{Intensity.id = 3} (for \emph{Xenon}).
#' @param var2nd.series.id Numeric index related to \code{col.names} vector and pointing to column
#'   for the EPR spectral series variable like time, temperature or microwave power. If data contains simple relation
#'   like \eqn{Area} vs \eqn{time}, use the \code{x} and \code{x.unit} parameters/arguments instead (see also \code{Examples}).
#'   This argument is dedicated to kinetic-like experiments. \strong{Default}: \code{var2nd.series.id = NULL}
#'   (see also the \code{data.structure} argument).
#' @param convertB.unit Logical (\strong{default}: \code{convertB.unit = TRUE}), whether upon reading, an automatic
#'   conversion between \code{G} and \code{mT} should be performed. If default is chosen, a new column/variable
#'   \eqn{B} in \code{mT}/\code{G} is created.
#' @param qValue Numeric, Q value (quality factor, number) displayed at specific \code{dB} by the spectrometer,
#'   in case of \emph{Xenon} or \emph{new Magnettech} software the parameter is included in \code{.DSC}/\code{.dsc} file,
#'   \strong{default}: \code{qValue = NULL}, which actually corresponds to value \code{1}.
#' @param norm.vec.add Numeric vector. Additional normalization constants in the form of vector, including
#'   all (in addition to \code{qValue}) normalization(s) like concentration, powder sample
#'   weight, number of scans, ...etc. (e.g. \code{norm.vec.add = c(2000,0.5,2)}). \strong{Default}:
#'   \code{norm.vec.add = NULL}, which actually corresponds to value \code{1}. If \code{qValue = NULL},
#'   the Q-factor/value might be also included in the \code{norm.vec.add}.
#' @param origin Character string, corresponding to \strong{origin} of the data and related to EPR acquisition software
#'   at the spectrometer (from which the data are loaded automatically using the default parameters).
#'   Options are summarized in the following table (any other specific \code{origin} may be added later) =>
#'   \tabular{rl}{
#'   \strong{String} \tab \strong{Description} \cr
#'   "xenon" \tab \strong{default} automatically loads data from the "Xenon" software with the default parameters. \cr
#'   "winepr" \tab automatically loads data from the "WinEpr" software. \cr
#'   "magnettech" \tab automatically loads data from the new "Magnettech" software (ESR5000 [11-0422]). \cr
#'   "other" (arbitrary string, e.g. "csv") \tab general, loads any other original
#'   data like \code{csv}, \code{txt}, \code{asc} incl. also data from other instrumental/spectrometer software.
#'   \strong{In such case, all the arguments for} \code{readEPR_Exp_Specs} \strong{have to be set up accordingly}. \cr
#'   }
#' @param data.structure Character string, referring to structure of the ASCII data. Common spectral data files
#'   with \eqn{Intensity} vs. \eqn{x(B,g,RF(\text{MHz}))} and/or \eqn{time} columns (including the spectral
#'   time series) correspond to \code{data.structure = "spectra"} (\strong{default}). For more complex
#'   ASCII data structure (such as spectral series processed by the acquisition spectrometer software,
#'   see \code{Examples}, or any other data) put \code{data.structure = "others"}. \strong{In such case, all the arguments for}
#'   the \code{readEPR_Exp_Specs} \strong{have to be set up accordingly}. The \code{data.structure} argument
#'   (assuming \code{var2nd.series.id = NULL}) is helping to simplify the reading of \code{"spectra"} by the predefined
#'   \code{origin} argument.
#' @param ... additional arguments specified (see also the \code{\link[data.table]{fread}} function).
#'
#'
#' @return Data frame/table consisting of the magnetic flux density
#'   column \code{B_mT} in millitesla (as well as \code{B_G} in gauss) or \code{RF_MHz} (in case of ENDOR spectrum)
#'   or unitless \code{g-factor} and of the derivative intensity column (\code{dIepr_over_dB}) or any other
#'   intensities (like integrated spectral form) in \code{procedure defined unit}
#'   (see \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6803776/}{p.d.u.}),
#'   which is normalized by the above-described parameters/function arguments.
#'   For 2D experiments (spectral series) an additional column with the 2nd independent variable (like time, temperature
#'   or microwave power, ...etc. ) is created. The actual data frame is return
#'   in the \href{https://r4ds.hadley.nz/data-tidy.html#sec-tidy-data}{tidy/long table format}.
#'
#'
#' @examples
#' ## simple ASCII EPR spectrum acquired by "xenon"
#' ## and with `B` conversion "G" <=> "mT"
#' ## Loading the data
#' aminoxyl.ascii.data.path <-
#'   load_data_example(file = "Aminoxyl_radical_a.txt")
#' aminoxyl.data.01 <-
#'   readEPR_Exp_Specs(
#'     aminoxyl.ascii.data.path,
#'     qValue = 2100
#'   )
#' ## preview
#' head(aminoxyl.data.01)
#' #
#' ## the same EPR spectrum, reading from
#' ## the original binary file
#' ## Loading the data
#' aminoxyl.bin.data.path <-
#'   load_data_example(file = "Aminoxyl_radical_a.DTA")
#' aminoxyl.data.02 <-
#'   readEPR_Exp_Specs(
#'     aminoxyl.bin.data.path,
#'     qValue = 2100
#'   )
#' ## preview
#' head(aminoxyl.data.02)
#' #
#' # simple EPR spectrum acquired by "xenon"
#' ## and without `B` conversion "G" <=> "mT"
#' aminoxyl.data.03 <-
#'   readEPR_Exp_Specs(aminoxyl.bin.data.path,
#'                     convertB.unit = FALSE,
#'                     qValue = 2100)
#' ## preview
#' head(aminoxyl.data.03)
#' #
#' ## the simple spectrum acquired by "winepr"
#' ## (and 20 scans) on a 1 mM sample concentration:
#' ## Loading the data
#' TMPD.ascii.data.path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.asc")
#' TMPD.data.01 <-
#'   readEPR_Exp_Specs(
#'     TMPD.ascii.data.path,
#'     col.names = c("B_G","dIepr_over_dB"),
#'     qValue = 3500,
#'     norm.vec.add = c(20,0.001),
#'     origin = "winepr"
#'   )
#' ## preview
#' head(TMPD.data.01)
#' #
#' ## the same EPR spectrum, reading from
#' ## the original binary file
#' ## Loading the data
#' TMPD.bin.data.path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.spc")
#' TMPD.data.02 <-
#'   readEPR_Exp_Specs(
#'     TMPD.bin.data.path,
#'     col.names = c("B_G","dIepr_over_dB"),
#'     qValue = 3500,
#'     norm.vec.add = c(20,0.001),
#'     origin = "winepr"
#'   )
#' ## preview
#' head(TMPD.data.02)
#' #
#' ## TODO below !!!
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
#' ## first of all load the package example file
#' acridineRad.data.path <-
#'   load_data_example("AcridineDeriv_Irrad_365nm.csv.zip")
#' ## unzip
#' acridineRad.data <-
#'   unzip(acridineRad.data.path,
#'         files = c("AcridineDeriv_Irrad_365nm.csv"),
#'         exdir = tempdir())
#' ## reading "magnettech"
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
readEPR_Exp_Specs <- function(path_to_file,
                              path_to_dsc_par = NULL,
                              path_to_ygf = NULL,
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
                              var2nd.series.id = NULL,
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
  winepr.string <- c(
    "winepr","Winepr","WinEpr","WINEPR","WinEPR","winEPR",
    "SPC/PAR","spc/par","Spc/Par"
  )
  xenon.string <- c("xenon","Xenon","XENON")
  magnettech.string <- c(
    "magnettech","Magnettech","MagnetTech",
    "magnetTech","MAGNETTECH","magnetech",
    "Magnetech","MAGNETECH"
  )
  ## previous strings also with single "t"/"T" excepting mistakes :-)
  #
  ## general condition to read `.id`s and co...(origin condition)
  origin.cond.all <- function(origin){
    if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
      return(0)
    }
    if (any(grepl(paste(magnettech.string,collapse = "|"),origin))){
      return(1)
    }
    if (any(grepl(paste(xenon.string,collapse = "|"),origin))){
      return(2)
    }
  }
  #
  ## condition for switching between xenon and magnettech
  if (origin.cond.all(origin = origin) == 2 ||
      origin.cond.all(origin = origin) == 1) {
    xen.magnet.cond <- function(origin){
      if (any(grepl(paste(xenon.string,collapse = "|"),origin))){
        return(0)
      }
      if (any(grepl(paste(magnettech.string,collapse = "|"),origin))){
        return(1)
      }
    }
  }
  #
  ## basic `fread` parameters to read the spectral data
  ## additional arguments see `?data.table::fread`
  #
  ## condition for the 2nd series spectra variable (must be TRUE if present)
  var2nd.series.cond <- isFALSE(is.null(var2nd.series.id))
  if (isTRUE(var2nd.series.cond) & length(col.names) <= 2){
    stop(" It doesn't look that your column names already include\n
         the column name correspondig to the 2nd variable series (e.g. time).\n
         Please, refer to the `col.names` and `var2nd.series.id` arguments !!")
  }
  #
  ## string to stop the reading if `data structure == "mixed"`
  ## and `var2nd.series.id != NULL` =>
  stop.reading.structure.time <-
    " For `data.structure = others` the `var2nd.series.id` is not defined !\n
      Please put `var2nd.series.id = NULL` with the `x.id` \n
      and `x.unit` corresponding to `time` (2nd var.) column (if present) ! \n
      Additional arguments must be set up, accordingly ! "
  #
  ## checking the path string, whether it points to ASCII or BINARY
  ## + chicking the corresponding origin
  ascii.cond <- grepl(".*\\.(txt|asc|csv)$",path_to_file)
  binary.cond <- grepl(".*\\.(DTA|spc)$",path_to_file)
  if (grepl(".*\\.DTA$",path_to_file)) {
    if (origin.cond.all(origin = origin) != 2 ||
        origin.cond.all(origin = origin) != 1) {
      stop(" Reading of the '.DTA' file requires\n
           origin = 'xenon' or origin = 'magnettech' !! ")
    }
  }
  if (grepl(".*\\.spc$",path_to_file)) {
    if (origin.cond.all(origin = origin) != 0) {
      stop(" Reading of the '.spc' file requires origin = 'winepr'!! ")
    }
  }
  #
  ## ==================== READING BINARIES ====================
  #
  if (isTRUE(binary.cond)) {
    #
    ## binary file length (to read the highest possible `n`)
    bin.file.length <- readBin(path_to_file,what = "raw",n = 1e+10) %>% length()
    #
    ## function to process loaded binary of time(2nd variable)-series experiments:
    process.bin.series.fn <-
      function(
    vec.intensity, ## intensity num. vector just after the binary load/conversion
    res.x, ## resolution/number of points X (e.g. B)
    unit.x, ## character string -> X-unit
    unit.y, ## character string -> Y-unit
    name.x, ## character string -> variable X-name
    name.y, ## character string -> variable Y-name
    vec.x, ## the entire X-vector (e.g. B)
    vec.y, ## the entire Y-vector (e.g. Time)
    origin = origin ## character string -> origin
      ) {
        #
        ## split intensity vector into list of individual intensity spectra
        whole.intens.list <-
          split(
            vec.intensity,
            ceiling(seq_along(vec.intensity) / res.x)
          )
        #
        ## create starting list of data frames
        whole.intens.df.list <-
          lapply(
            whole.intens.list,
            function(s) {
              data.frame(Intensity = s)
            }
          )
        #
        ## create time + Field(B) columns for each spectrum
        whole.intens.df.final.list <-
          Map(
            function(u,v){
              u <- u %>% dplyr::mutate(
                !!rlang::quo_name(paste0(name.y,unit.y)) :=
                  rep(v,times = dim(u)[1]),
                !!rlang::quo_name(paste0(name.x,unit.x)) := vec.x
              )
            },
            whole.intens.df.list,
            vec.y
          )
        #
        ## remove the previous 1st + 2nd list
        rm(whole.intens.list,whole.intens.df.list)
        #
        ## create "tidy" data frame (long format) +
        ## add index
        df.original <-
          dplyr::bind_rows(whole.intens.df.final.list)
        #
        if (origin.cond.all(origin = origin) == 2 ||
            origin.cond.all(origin = origin) == 1) {
          df.original$index <- 1:dim(df.original)[1]
          #
          ## reordering columns
          df.original <- df.original %>% dplyr::select(4,3,2,1)
        }
        if (origin.cond.all(origin = origin) == 0) {
          #
          ## just reordering columns
          df.original <- df.original %>% dplyr::select(3,2,1)
        }
        #
        # remove the final list in order to free memory:-)
        rm(whole.intens.df.final.list)
        #
        return(df.original)
        #
      }
    #
    ##  ------------------ loading Xenon and Magnettech BINARY files ------------------
    #
    if (origin.cond.all(origin = origin) == 2 ||
        origin.cond.all(origin = origin) == 1) {
      #
      ## first of all one must look for the `.DSC`/`.dsc` file
      ## which is essential to read the binaries
      #
      ## expecting that the `.DSC`/`.dsc` file is in the same folder
      ## as the `.DTA` one and possesses the same name
      path.to.dsc <- gsub(
        "\\.DTA$",
        switch(2 - xen.magnet.cond(origin = origin),".dsc",".DSC"),
        path_to_file
      )
      #
      ## check the existence of this previous file in the working dir.
      if (!file.exists(path.to.dsc)) {
        stop(" Please provide the file path for the `.DSC`/`.dsc` file,\n
             refer to definition of the `path_to_dsc_par` argument !! ")
      } else {
        path_to_dsc_par <-
          path_to_dsc_par %>% `if`(is.null(path_to_dsc_par), path.to.dsc, .)
      }
      #
      ## (time/2nd var. series) experimental series
      if (isTRUE(var2nd.series.cond)) {
        #
        ## condition for 2nd variable series -->
        if (length(col.names) == 3) {
          stop(" In order to load the 'Xenon' experiment with\n
               2nd var. series (e.g. time), the `col.names` arument\n
               must be defined like `c('index','B_G','time_s','dIepr_over_dB')`\n
               see `Examples` in the documentation !! ")
        }
        #
        ##  ------------------- COMMENT ---------------------------------|
        ## first of all one must look for the `.YGF` file                |
        ## which is essential to read the time/2nd var. series binaries, |
        ## in such binary file, all recorded times at the beginning      |
        ## of each spectrum scan are available, if this file does not    |
        ## exist -> the 2nd var. series vector can be computed by using  |
        ## the `.DSC`/`.dsc` file -> the difference between these two    |
        ## falls into the order of ms                                    |
        ## --------------------------------------------------------------|
        #
        ## detect the Y-unit (2nd var. series)
        spectr.Yunit <- readEPR_param_slct(
          path_to_dsc_par = path_to_dsc_par,
          string = "YUNI"
        )
        spectr.Yunit <- gsub("'","",spectr.Yunit)
        #
        ## expecting that the `.YGF` file is in the same folder
        ## as the `.DTA` one and possesses the same name
        path.to.ygf <- gsub("\\.DTA$",".YGF",path_to_file)
        #
        ## check the existence of this previous file in the working dir.
        if (!file.exists(path.to.ygf)) {
          #
          ## we need additional info
          spectr.Yparams <- readEPR_param_slct(
            path_to_dsc_par = path_to_dsc_par,
            string = c("YMIN","YWID","YPTS"),
            origin = origin
          )
          #
          ## therefore the Y/time vector
          spectr.Yvec <- seq(
            spectr.Yparams$YMIN,
            spectr.Yparams$YMIN + spectr.Yparams$YWID,
            length.out = spectr.Yparams$YPTS
          )
          #
        } else {
          path_to_ygf <-
            path_to_ygf %>% `if`(is.null(path_to_ygf), path.to.ygf, .)
          #
          ## load the binary `.YGF` file
          bin.ygf.file.length <-
            readBin(path_to_ygf,what = "raw",n = 1e+10) %>% length()
          #
          spectr.Yvec <- readBin(
            path_to_ygf,
            what = "numeric",
            size = 8,
            n = bin.ygf.file.length / 8,
            signed = TRUE,
            endian = switch(2 - xen.magnet.cond(origin = origin),"little","big")
          )
        }
      }
      #
      ## definitions EPR spectrum abscissa with help of `.DSC`
      spectr.Xparams <- readEPR_param_slct(
        path_to_dsc_par = path_to_dsc_par,
        string = c("XPTS","XMIN","XWID","XUNI"),
        origin = origin
      )
      #
      ## detect the X-unit
      spectr.Xunit <- spectr.Xparams$XUNI
      spectr.Xunit <- gsub("'","",spectr.Xunit)
      #
      ## therefore the entire X vector
      spectr.Xvec <-
        seq(
          spectr.Xparams$XMIN,
          spectr.Xparams$XMIN + spectr.Xparams$XWID,
          length.out = spectr.Xparams$XPTS
        )
      #
      ## Intensity vector
      Intensity.vec <- readBin(
        path_to_file,
        what = "numeric",
        size = 8, # blocks/elements with 8-bytes
        n = bin.file.length / 8,
        signed = TRUE,
        endian = switch(2 - xen.magnet.cond(origin = origin),"little","big")
      )
      #
      if (isTRUE(var2nd.series.cond)) {
        #
        ## redefinition of `.id`s corresp. to `col.names`
        ## expecting mistakes :-)
        x.id <- x.id %>% `if`(x.id != 2, 2, .)
        Intensity.id <- Intensity.id %>%
          `if`(Intensity.id != 4, 4, .)
        var2nd.series.id <- var2nd.series.id %>%
          `if`(var2nd.series.id != 3 || is.null(var2nd.series.id), 3, .)
        #
        ## use the `process.bin.series.fn` function defined above
        spectrum.data.origin <-
          process.bin.series.fn(
            vec.intensity = Intensity.vec,
            res.x = spectr.Xparams$XPTS,
            unit.x = spectr.Xunit,
            unit.y = spectr.Yunit,
            name.x = "X_",
            name.y = "Yvar_",
            vec.x = spectr.Xvec,
            vec.y = spectr.Yvec,
            origin = origin
          )
        #
        ## final column names
        colnames(spectrum.data.origin) <- c(
          "index",
          col.names[x.id],
          col.names[var2nd.series.id],
          col.names[Intensity.id]
        )
        #
      } else {
        #
        ## redefinition of `.id`s corresp. to `col.names`
        ## expecting mistakes :-)
        x.id <- x.id %>% `if`(x.id != 2, 2, .)
        Intensity.id <- Intensity.id %>%
          `if`(Intensity.id != 3, 3, .)
        var2nd.series.id <- var2nd.series.id %>%
          `if`(!is.null(var2nd.series.id), NULL, .)
        #
        ## final step in reading, if no series
        spectrum.data.origin <- data.frame(
          index = 1:length(spectr.Xvec),
          stats::setNames(list(spectr.Xvec),paste0("X_",spectr.Xunit)),
          Intensity = Intensity.vec
        )
        #
        ## redefinition of the actual column names
        colnames(spectrum.data.origin) <- c(
          "index",
          col.names[x.id],
          col.names[Intensity.id]
        )
      }
    }
    ## ------------------ loading Binary WinEPR files ------------------
    #
    if (origin.cond.all(origin = origin) == 0) {
      #
      ## first of all one must look for the `.par` file
      ## which is essential to read the binaries
      #
      ## expecting that the `.par` file is in the same folder
      ## as the `.spc` one and possesses the same name
      path.to.par <- gsub(
        "\\.spc$",
        ".par",
        path_to_file
      )
      #
      ## check the existence of this previous file in the working dir.
      if (!file.exists(path.to.par)) {
        stop(" Please provide the file path for the `.par` file,\n
             refer to definition of the `path_to_dsc_par` argument !! ")
      } else {
        path_to_dsc_par <-
          path_to_dsc_par %>% `if`(is.null(path_to_dsc_par), path.to.par, .)
      }
      #
      ## 2nd variable (time) experimental series
      if (isTRUE(var2nd.series.cond)) {
        #
        ## condition for 2nd variable series -->
        if (length(col.names) == 2) {
          stop(" In order to load the 'WinEPR' experiment with\n
               2nd var. series (e.g. time), the `col.names` arument\n
               must be defined like `c('B_G','Slice','dIepr_over_dB')`\n
               see `Examples` in the documentation !! ")
        }
        #
        ##  Y-unit is unitless because the data
        ## corresp. to the slices/spectra
        #
        ## additional info/parameters for the time series
        spectr.Yparams <- readEPR_param_slct(
          path_to_dsc_par = path_to_dsc_par,
          string = c("XYLB","XYWI","REY"),
          origin = origin
        )
        #
        ## therefore the Slice vector
        spectr.Yvec <- seq(
          spectr.Yparams$XYLB,
          spectr.Yparams$XYLB + spectr.Yparams$XYWI,
          length.out = spectr.Yparams$REY
        )
      }
      #
      ## definitions EPR spectrum abscissa with help of `.par`
      spectr.Xparams <- readEPR_param_slct(
        path_to_dsc_par = path_to_dsc_par,
        string = switch(
          2 - var2nd.series.cond,
          c("RES","XXLB","XXWI","XXUN"),
          c("RES","GST","GSI","JUN")
        ),
        origin = origin
      )
      #
      ## detect X-unit
      spectr.Xunit <- spectr.Xparams[[4]]
      #
      ## therefore the entire X vector
      spectr.Xvec <-
        seq(
          spectr.Xparams[[2]],
          spectr.Xparams[[2]] + spectr.Xparams[[3]],
          length.out = spectr.Xparams$RES
        )
      #
      ## Intensity vector
      Intensity.vec <- readBin(
        path_to_file,
        what = "numeric",
        size = 4, # blocks/elements with 4-bytes
        n = bin.file.length / 4,
        signed = TRUE,
        endian = "little"
      )
      #
      if (isTRUE(var2nd.series.cond)) {
        #
        ## redefinition of `.id`s corresp. to `col.names`
        ## expecting mistakes :-)
        x.id <- x.id %>% `if`(x.id != 1, 1, .)
        Intensity.id <- Intensity.id %>%
          `if`(Intensity.id != 3, 3, .)
        var2nd.series.id <- var2nd.series.id %>%
          `if`(var2nd.series.id != 2 || is.null(var2nd.series.id), 2, .)
        #
        ##  Y-unit is unitless because the data
        ## corresp. to the slices/spectra -> `unit.y = ""`
        #
        ## use the `process.bin.series.fn` function defined above
        spectrum.data.origin <-
          process.bin.series.fn(
            vec.intensity = Intensity.vec,
            res.x = spectr.Xparams$RES,
            unit.x = spectr.Xunit,
            unit.y = "",
            name.x = "X_",
            name.y = "slice",
            vec.x = spectr.Xvec,
            vec.y = spectr.Yvec,
            origin = origin
          )
        #
        ## final column names
        colnames(spectrum.data.origin) <- c(
          col.names[x.id],
          col.names[var2nd.series.id],
          col.names[Intensity.id]
        )
        #
      } else {
        #
        ## redefinition of `.id`s corresp. to `col.names`
        ## expeccting mistakes :-)
        x.id <- x.id %>% `if`(x.id != 1, 1, .)
        Intensity.id <- Intensity.id %>% `if`(Intensity.id != 2, 2, .)
        var2nd.series.id <- var2nd.series.id %>%
          `if`(!is.null(var2nd.series.id), NULL, .)
        #
        ## final step in reading, if no series
        spectrum.data.origin <- data.frame(
          stats::setNames(list(spectr.Xvec),paste0("X_",spectr.Xunit)),
          Intensity = Intensity.vec
        )
        #
        ## redefinition of the actual column names
        colnames(spectrum.data.origin) <- c(
          col.names[x.id],
          col.names[Intensity.id]
        )
      }
    }
  }
  #
  ## ==================== READING ASCII TEXT ====================
  #
  if (isTRUE(ascii.cond)) {
    #
    ## --------------------- WINEPR -----------------------------
    #
    if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
      if (data.structure == "spectra") {
        ## parameter definition (automatize value assignments)
        ## ASSUMING USER CAN MAKE MISTAKES :-)
        if (isFALSE(var2nd.series.cond)) {
          sep <- sep %>% `if`(sep != "auto", "auto", .)
          header <- header %>% `if`(isTRUE(header), FALSE, .)
          skip <- skip %>% `if`(skip != 3, 3, .)
          x.id <- x.id %>% `if`(x.id != 1, 1, .)
          Intensity.id <- Intensity.id %>% `if`(Intensity.id != 2, 2, .)
          var2nd.series.id <- var2nd.series.id %>%
            `if`(!is.null(var2nd.series.id), NULL, .)
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
          #
          ## condition for 2nd variable series -->
          if (isTRUE(var2nd.series.cond) & length(col.names) == 2) {
            stop(" In order to load the 'WinEPR' experiment with\n
               2nd var. series (e.g. time), the `col.names` arument\n
               must be defined like `c('B_G','Slice','dIepr_over_dB')`\n
               see `Examples` in the documentation !! ")
          }
          #
          sep <- sep %>% `if`(sep != "auto", "auto", .)
          header <- header %>% `if`(isTRUE(header), FALSE, .)
          skip <- skip %>% `if`(skip != 4, 4, .)
          x.id <- x.id %>% `if`(x.id != 1, 1, .)
          Intensity.id <- Intensity.id %>%
            `if`(Intensity.id != 3, 3, .)
          var2nd.series.id <- var2nd.series.id %>%
            `if`(var2nd.series.id != 2 || is.null(var2nd.series.id), 2, .)
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
        if (!is.null(var2nd.series.id)) {
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
    #
    if (any(grepl(paste(xenon.string,collapse = "|"),origin))) {
      if (data.structure == "spectra") {
        ## parameter definition (automatize value assignments)
        ## ASSUMING USER CAN MAKE MISTAKES :-)
        sep <- sep %>% `if`(sep != "auto", "auto", .)
        header <- header
        skip <- skip
        if (isFALSE(var2nd.series.cond)) {
          x.id <- x.id %>% `if`(x.id != 2, 2, .)
          Intensity.id <- Intensity.id %>%
            `if`(Intensity.id != 3, 3, .)
          var2nd.series.id <- var2nd.series.id %>%
            `if`(!is.null(var2nd.series.id), NULL, .)
        } else {
          #
          # condition for 2nd variable series -->
          if (isTRUE(var2nd.series.cond) & length(col.names) == 3) {
            stop(" In order to load the 'Xenon' experiment with\n
               2nd var. series (e.g. time), the `col.names` arument\n
               must be defined like `c('index','B_G','time_s','dIepr_over_dB')`\n
               see `Examples` in the documentation !! ")
          }
          #
          x.id <- x.id %>% `if`(x.id != 2, 2, .)
          Intensity.id <- Intensity.id %>%
            `if`(Intensity.id != 4, 4, .)
          var2nd.series.id <- var2nd.series.id %>%
            `if`(var2nd.series.id != 3 || is.null(var2nd.series.id), 3, .)
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
        if (!is.null(var2nd.series.id)) {
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
    #
    if (any(grepl(paste(magnettech.string,collapse = "|"),origin))){
      if (data.structure == "spectra") {
        ## parameter definition (automatize value assignments)
        ## ASSUMING USER CAN MAKE MISTAKES :-)
        sep <- sep %>% `if`(sep != "auto", "auto", .)
        header <- header %>% `if`(isTRUE(header), FALSE, .)
        skip <- skip %>% `if`(skip != 88, 88, .)
        #
        if (isFALSE(var2nd.series.cond)) {
          x.id <- x.id %>% `if`(x.id != 1, 1, .)
          Intensity.id <- Intensity.id %>%
            `if`(Intensity.id != 2, 2, .)
          var2nd.series.id <- var2nd.series.id %>%
            `if`(!is.null(var2nd.series.id), NULL, .)
        } else {
          if (is.null(var2nd.series.id)) {
            stop("Time series column in the ASCII is not defined ! \n
             Please, specify the `var2nd.series.id` of the column/variable,\n
             or create it to proceed !! ")
          } else {
            x.id <- x.id
            Intensity.id <- Intensity.id
            var2nd.series.id <- var2nd.series.id
          }
        }
      } else {
        if (!is.null(var2nd.series.id)) {
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
      var2nd.series.id <- switch(2-data.structure.cond,
                                 var2nd.series.id,
                                 NULL)

    }
    #
    ## ============ SPECTRA/DATA READING AND NORMALIZING THE INTENSITY ===============
    #
    ## condition for special case `winepr` and `time.series`
    if (any(grepl(paste(winepr.string,collapse = "|"),origin)) &
        !is.null(var2nd.series.id)) {
      spectrum.data.origin <-
        data.table::fread(file = path_to_file,
                          sep = sep,
                          header = header,
                          skip = skip,
                          col.names = col.names,
                          fill = T,
                          blank.lines.skip = T,
                          na.strings = c("Intensity","X [G]","Y []")) %>%
        ## filter out all rows containing "Slice|slice"
        ## in "B|Field" column
        dplyr::filter(
          !grepl(
            "Slice|slice",
            .data[[grep(pattern = "BG|B_G|B-G|Field|X-G|X_G",
                        col.names,value = TRUE)]]
          )
        ) %>%
        stats::na.omit()
      ## in order to be sure that this column will appear as numeric
      spectrum.data.origin[[
        grep(
          pattern = "BG|B_G|B-G|Field|X-G|X_G",
          col.names,
          value = TRUE
        )
      ]] <-
        spectrum.data.origin[[
          grep(
            pattern = "BG|B_G|B-G|Field|X-G|X_G",
            col.names,
            value = TRUE
          )
        ]] %>%
        as.double()
    } else {
      ## basic data frame by general function (`fread`)
      ## incl. the above defined parameters
      spectrum.data.origin <-
        data.table::fread(
          file = path_to_file,
          sep = sep,
          header = header,
          skip = skip,
          col.names = col.names,
          ...
        )
    }
  }
  #
  ## Condition to convert any character column to numeric format
  ## to check character => `inherits(x,"character")` => JUST A NOTE
  #
  IntensityString <- col.names[Intensity.id] ## `Intensity` string
  xString <- col.names[x.id] ## `x` string
  # timeAxis <- col.names[var2nd.series.id]
  ## new `spectra.data`
  #
  ## Common EPR Spectra
  ## Unit condition
  G.unit.cond <- if (x.unit == "G") TRUE else FALSE
  ## intial `B`/`Field` Character string condition
  if (grepl("B|BF|BG|B_G|B-G|X_G|X-G",xString)){
    xString.init <- "B_"
  }
  if (grepl("Fiel|fiel",xString)){
    xString.init <- "Field_"
  }
  #
  if (x.unit == "G" || x.unit == "mT") {
    if (isTRUE(convertB.unit)){
      spectra.data <- spectrum.data.origin %>%
        dplyr::mutate(
          !!rlang::quo_name(
            paste0(xString.init,switch(2-isTRUE(G.unit.cond),"mT","G"))
          ) :=
            .data[[xString]] * switch(
              2 - isTRUE(G.unit.cond),
              1 / 10,
              10
            )
        ) %>%
        dplyr::mutate(
          !!rlang::quo_name(IntensityString) := .data[[IntensityString]] *
            norm.multiply.qValue * norm.multiply.const
        )
      #
    } else {
      #
      spectra.data <- spectrum.data.origin %>%
        dplyr::mutate(
          !!rlang::quo_name(IntensityString) := .data[[IntensityString]] *
            norm.multiply.qValue * norm.multiply.const
        )
    }
  }
  ## Any other Spectra like ENDOR or with g-Value or Intensity/Area vs time
  ## or Intensity vs power relationship
  if (x.unit != "G" & x.unit != "mT") {
    spectra.data <- spectrum.data.origin %>%
      dplyr::mutate(
        !!rlang::quo_name(IntensityString) := .data[[IntensityString]] *
          norm.multiply.qValue * norm.multiply.const
      )
  }
  #
  return(spectra.data)
  #
}
