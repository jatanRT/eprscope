#
#' Load Several/Multiple EPR Spectral Data Files with Parameters
#'
#'
#' @description Loads EPR spectra from several/multiple `ASCII`/`text` files and from those incl. instrumental
#'  parameters (`DSC` or `par`) at once and transforms it into a database list of data frames. Function is based
#'  on the \code{\link[base]{list.files}} and \code{\link{readEPR_Exp_Specs}} into one list/database.
#'  According to  experiment quantity (e.g. temperature,microwave power,recording time...etc),
#'  `names` and `var2nd` (in case of `tidy = T`) parameters have to be provided. If intensity normalization
#'  by e.g. like concentration, sample weight...etc is required, it can be performed afterwards
#'  (generally, except the Q values, it is not included in the transformation process).
#'
#'
#' @param pattern String/Character ('specimen'), inherited from \code{\link[base]{list.files}}, which appear
#'  at the beginning of a file name.
#' @param dir_ASC path (defined by \code{\link[base]{file.path}}, String/Character) to directory where
#'  the `ascii` files are stored
#' @param dir_DSC_or_par path (defined by \code{\link[base]{file.path}} String/Character) to directory
#'  where the files (`.DSC` or `.par`) with instrumental parameters (to calculate \eqn{g}-value
#'  or normalize intensities) are stored
#' @param col.names Character/String vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names \strong{for individual file} (see also \code{\link{readEPR_Exp_Specs}}).
#'   A safe rule of thumb is to use column names incl. physical quantity notation with its units,
#'   \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"}, \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum abscissa)...etc, \strong{default}: \code{col.names = c("index","B_G",dIepr_over_dB)}.
#' @param colClasses List, inherited from \code{\link[data.table]{fread}}... TBC
#'   e.g. like \code{colClasses = list(numeric = 1)} or by character string like \code{colClasses = c(V1 = "numeric")}
#'   or \code{colClasses = list(numeric = "V1")} where in all cases `1` corresponds, to column index.
#'   \strong{Default}: \code{colClasses = NULL}.
#' @param x Numeric index related to \code{col.names} pointing to independent variable, which corresponds
#'   to abscissa (\eqn{x}-axis) in spectra or other plots.
#' @param x.unit Character/String pointing to unit of quantity (coming from original ASCII data, see also
#'   \code{column.names} parameter) which is to be presented on \eqn{x} abscissa of the EPR spectrum,
#'   like \code{"G"} (`Gauss`), \code{"mT"} (`millitesla`), \code{"MHz"} (`megahertz` in case of ENDOR spectra)
#'   or \code{"Unitless"} in case of \eqn{g}-values, \strong{default}: \code{x.unit = "G"}.
#' @param Intensity Numeric index related to \code{col.names} pointing to `general` intensity,
#'   like derivative intensity (`dIepr_over_dB`), integral one (e.g. `single_Integ`), double or sigmoid
#'   integral (e.g. `Area`)...etc. This corresponds to column/vector which should be presented like
#'   \eqn{y}-axis in spectra or other plots.
#' @param convertB.unit Logical (\strong{default}: \code{convertB.unit = TRUE}) description...
#'   convert \eqn{B} in Gauss <=> millitesla...
#' @param qValues Numeric Vector, `Q Value` (sensitivity factors to normalize EPR intensity) either loaded from
#'  files incl. parameters (`.DSC` or `.par`) by this function/R.script (therefore \code{qValues = NULL},
#'  \strong{default}) or in case of \code{origin = "winepr"} it have to provided by the spectrometer operator.
#' @param norm.list.add Numeric list of vectors. Additional normalization constants in form of vectors involving
#'   all additional (in addition to \code{qValue}) normalization(s) like e.g. concentration, powder sample
#'   weight, number of scans, ...etc (\code{norm.vec.add = c(2000,0.5,2)}). \strong{Default}:
#'   \code{norm.list.add = NULL}.
#' @param names String/Character Vector corresponding to values of \strong{additional quantity}
#'  (e.g. temperature,microwave power...etc) being varied by the individual experiments
#' @param tidy Logical, whether to transform the list of data frames into long table (`tidy`) format,
#'  \strong{default}: \code{tidy = F}
#' @param var2nd.series String/Character, if \code{tidy = T} (see `tidy` parameter/argument) it is referred to name
#'  of the variable/quantity (e.g. like `time`,`Temperature`,`Electrochemical Potential`,`Microwave Power`...etc)
#'  altered upon individual experiments as a second variable (\code{var2nd}) and related to spectra/data.
#' @param origin String/Character corresponding to \strong{software} used to acquire the EPR spectra
#'   on BRUKER spectrometers, i.e. whether they were recorded by the windows based softw. ("WinEpr",
#'   \code{origin = "winepr"}) or by the Linux one ("Xenon"), \strong{default}: \code{origin = "xenon"}
#'   Only the two above-mentioned  characters/strings are available due to reading parameter files.
#'
#'
#' @return List of Data Frames (or `long table` format) corresponding to multiple spectral data files/database
#'
#'
#' @examples
#' \dontrun{
#' ## Multiple ENDOR spectra at different temperatures recorded by "Xenon" software
#' ## reading and transforming into `longtable`
#' readEPR_Exp_Specs_multif(pattern = "Sample_VT_",
#'                          file.path(".","ASCII_data_dir"),
#'                          file.path(".","DSC_data_dir"),
#'                          col.names = c("index",
#'                                        "RF_MHz",
#'                                        "Intensity"),
#'                          x = 2,
#'                          x.unit = "MHz",
#'                          Intensity = 3,
#'                          names = c("210","220","230","240"),
#'                          tidy = TRUE,
#'                          var2nd.series = "Temperature_K")
#'
#' ## Multiple EPR spectra recorded at different temperatures
#' ## by "WinEPR" software. Experiment performed with powder
#' ## sample (m = 10 mg) and each spectrum acquired
#' ## as 7 accumulations. The resulting database as list of data frames
#' readEPR_Exp_Specs_multif("Sample_VT_",
#'                          file.path(".","ASCII_data_dir"),
#'                          file.path(".","DSC_data_dir"),
#'                          col.names = c("B_G","dIepr_over_dB"),
#'                          colClasses = list(numeric = 1),
#'                          x = 1,
#'                          x.unit = "G",
#'                          Intensity = 2,
#'                          names = c("210","220","230","240"),
#'                          qValues =c(3400,3501,3600,2800),
#'                          norm.list.add = list(rep(c(10,7),times = 4)),
#'                          origin = "winepr")
#' }
#'
#'
#' @export
#'
#'
#' @importFrom rlang quo_name :=
readEPR_Exp_Specs_multif <- function(pattern,
                                     dir_ASC,
                                     dir_DSC_or_par,
                                     col.names = c(
                                       "index",
                                       "B_G",
                                       "dIepr_over_dB"
                                     ),
                                     colClasses = NULL,
                                     x = 2,
                                     x.unit = "G",
                                     Intensity = 3,
                                     convertB.unit = TRUE,
                                     qValues = NULL,
                                     norm.list.add = NULL,
                                     names,
                                     tidy = FALSE,
                                     var2nd.series = NULL,
                                     origin = "xenon") {
  #
  ## 'Temporary' processing variables
  . <- NULL
  g_Value <- NULL
  index <- NULL
  #
  ## =========================== FILES AND PARAMETERS ==============================
  #
  ## file name pattern which has to be the same for `txt`+`DSC`
  ## or `.asc` and `.par`
  file.name.pattern.asc <- paste0("^",pattern,".*\\.(txt|asc|csv)")
  file.name.pattern.params <- paste0("^",pattern,".*\\.(DSC|par)")
  #
  ## path to all `asc` files
  files.asc <- list.files(
    path = dir_ASC,
    pattern = file.name.pattern.asc,
    full.names = TRUE
  )
  #
  ## path to `.DSC`  or  `.par`  files
  files.params <- list.files(
    path = dir_DSC_or_par,
    pattern = file.name.pattern.params,
    full.names = TRUE
  )
  #
  if (origin == "xenon") {
    ## to obtain `QValues` (from all `.DSC` files) run the following
    qValues.from.files <- sapply(
      files.params,
      function(x) readEPR_param_slct(x, string = "QValue")
    )
    ## to obtain microwave frequencies `MWFQ` (from all `.DSC` files),
    ## required for g value calculations
    mwfq.string <- "MWFQ"
  }
  if (origin == "winepr") {
    ## to define `QValues` run the following
    qValues.from.files <- qValues %>% `if`(is.null(qValues),
                                           rep(1,times = length(names)), .)
    ## to obtain microwave frequencies `MWFQ` (from all `.par` files),
    ## required for g value calculations
    mwfq.string <- "MW"
  }
  #
  ## all frequencies
  mwfreq.from.files <- sapply(
    files.params,
    function(y) {
      readEPR_param_slct(y,
        string = mwfq.string,
        origin = origin
      )
    }
  )
  if (origin == "winepr") {
    ## conversion from "GHz" to "Hz"
    mwfreq.from.files <- mwfreq.from.files * 1e9
  }
  #
  ## `norm.list.add` definition
  norm.list.add <- norm.list.add %>% `if`(is.null(norm.list.add),
                                          lapply(rep(1,times = length(names)),
                                                 function(g) g), .)
  #
  ## =========================== SPECTRAL DATA READING ==========================
  #
  ## the all spectra with intensity correction
  ## to `qValue` + new column with `g`-factors
  ## in order to check whether there is some spectral position
  ## shift/drift
  #
  ## However prior to the operation above `x`/`B` has to be defined
  xString <- col.names[x]
  #
  spectra.datab.from.files <-
    Map(
      function(r, s, t, u) {
        readEPR_Exp_Specs(r,
          col.names = col.names,
          colClasses = colClasses,
          x = x,
          x.unit = x.unit,
          Intensity = Intensity,
          convertB.unit = convertB.unit,
          qValue = s,
          norm.vec.add = t,
          origin = origin
        ) %>%
          `if`(x.unit == "G" & x.unit == "mT",
          dplyr::mutate(g_Value = eval_gFactor(
            nu = u,
            nu.unit = "Hz",
            B = .data[[xString]],
            B.unit = x.unit
          )), .)
      },
      files.asc,
      qValues.from.files,
      norm.list.add,
      mwfreq.from.files
    )
  #
  ## rename spectra according to desired parameter/quantity/...etc. dependency
  ## see params. above
  names(spectra.datab.from.files) <- names
  #
  ## Weather to create a long table format (`tidy`) or not
  if (isFALSE(tidy)) {
    #
    return(spectra.datab.from.files)
    #
  } else {
    if (is.null(var2nd.series)) {
      stop(" 'var2nd.series' string is not specified. Please, define! ")
    } else {
      ## apply `bind_rows` to merge all spectral data from the list
      spectra.datab.from.files <-
        dplyr::bind_rows(spectra.datab.from.files, .id = var2nd.series)
      ## remove index if present
      if (any(grepl("index",colnames(spectra.datab.from.files)))){
        spectra.datab.from.files$index <- NULL
      }
      ## recalculate `var2nd.series` column
      spectra.datab.from.files <- spectra.datab.from.files %>%
        dplyr::mutate(!!rlang::quo_name(var2nd.series) := as.factor(as.numeric(.data[[var2nd.series]])))
      #
      return(spectra.datab.from.files)
      #
    }
  }
}
