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
#' @param pattern String/Character, inherited from \code{\link[base]{list.files}}, which appear in the file name
#'  as a 'specimen'.
#' @param dir_ASC path (defined by \code{\link[base]{file.path}}, String/Character) to directory where
#'  the `ascii` files are stored
#' @param dir_DSC_or_par path (defined by \code{\link[base]{file.path}} String/Character) to directory
#'  where the files (`.DSC` or `.par`) with instrumental parameters (to calculate \eqn{g}-value
#'  or normalize intensities) are stored
#' @param x.unit Character/String pointing to unit of quantity (coming from original ASCII data, see also
#'   \code{column.names} parameter) which is to be presented on \eqn{x} abscissa of the EPR spectrum,
#'   like \code{"G"} (`Gauss`), \code{"mT"} (`millitesla`), \code{"MHz"} (`megahertz` in case of ENDOR spectra)
#'   or \code{"Unitless"} in case of \eqn{g}-values, \strong{default}: \code{x.unit = "G"}.
#' @param col.names Character/String vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names \strong{for individual file} (see also \code{\link{readEPR_Exp_Specs}}).
#'   A safe rule of thumb is to use column names incl. physical quantity notation with its units,
#'   \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"}, \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum abscissa)...etc, \strong{default}: \code{col.names = c("index","B_G",dIepr_over_dB)}.
#' @param origin String/Character corresponding to \strong{software} used to acquire the EPR spectra
#'   on BRUKER spectrometers, i.e. whether they were recorded by the windows based softw. ("WinEpr",
#'   \code{origin = "winepr"}) or by the Linux one ("Xenon"), \strong{default}: \code{origin = "xenon"}
#'   Only the two above-mentioned  characters/strings are available due to reading parameter files.
#' @param qValues Numeric Vector, `Q Value` (sensitivity factors to normalize EPR intensity) either loaded from
#'  files incl. parameters (`.DSC` or `.par`) by this function/R.script (therefore \code{qValues = NULL},
#'  \strong{default}) or in case of \code{origin = "winepr"} it have to provided by the operator of a spectrometer
#' @param names String/Character Vector corresponding to values of \strong{additional quantity}
#'  (e.g. temperature,microwave power...etc) being varied by the individual experiments
#' @param tidy Boolean, whether to transform the list of data frames into long table (`tidy`) format,
#'  \strong{default}: \code{tidy = F}
#' @param var2nd String/Character, if \code{tidy = T} (see `tidy` parameter) it is referred to name
#'  of the variable/quantity (e.g. like `time`,`Temperature`,`Electrochemical Potential`,`Microwave Power`...etc)
#'  altered upon individual experiments as a second variable (\code{var2nd}) and related to spectra/data.
#'
#'
#' @return List of Data Frames (or `long table` format) corresponding to multiple spectral data files/database
#'
#'
#' @examples
#' \dontrun{
#' ## Multiple ENDOR spectra recorded by "Xenon" software
#' ## reading and transforming into `longtable`
#' readEPR_Exp_Specs_multif("Sample_VT_",
#'                          file.path(".","ASCII_data_dir"),
#'                          file.path(".","DSC_data_dir"),
#'                          x.unit = "MHz",
#'                          col.names = c("index",
#'                                        "RF_MHz",
#'                                        "Intensity")
#'                          names = c("210","220","230","240"),
#'                          tidy = T,
#'                          var2nd = "Temperature_K")
#'
#' ## Multiple EPR spectra recorded by "WinEPR" sofware
#' readEPR_Exp_Specs_multif("Sample_VT_",
#'                          file.path(".","ASCII_data_dir"),
#'                          file.path(".","DSC_data_dir"),
#'                          col.names = c("B_G","dIepr_over_dB"),
#'                          origin = "winepr",
#'                          names = c("210","220","230","240"),
#'                          qValues =c(3400,3501,3600,2800))
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
                                     x.unit = "G",
                                     col.names = c(
                                       "index",
                                       "B_G",
                                       "dIepr_over_dB"
                                     ),
                                     origin = "xenon",
                                     qValues = NULL,
                                     names,
                                     tidy = FALSE,
                                     var2nd = NULL) {
  #
  ## 'Temporary' processing variables
  new_variable <- NULL
  g_Value <- NULL
  index <- NULL
  #
  ## --------------------------- FILES AND PARAMETERS ------------------------------
  #
  ## file name pattern which has to be the same for `ASC`+`DSC`
  ## or `.spc` and `.par`
  file.name.pattern <- pattern
  #
  ## path to all `asc` files
  files.asc <- list.files(
    path = dir_ASC,
    pattern = file.name.pattern,
    full.names = TRUE
  )
  #
  ## path to all `raw` (`.DSC`+`.DTA` or `.par`+`.spc`) files
  files.raw <- list.files(
    path = dir_DSC_or_par,
    pattern = file.name.pattern,
    full.names = TRUE
  )
  #
  if (origin == "xenon") {
    ## select only `.DSC` files
    files.params <- grep(
      pattern = ".DSC",
      files.raw,
      value = TRUE
    )
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
    ## select only `.par` files
    files.params <- grep(
      pattern = ".par",
      files.raw,
      value = TRUE
    )
    ## to obtain `QValues` run the following
    if (is.null(qValues)) {
      stop(" 'qValues' vector is not provided. Please, define! ")
    } else {
      qValues.from.files <- qValues
    }
    ## to obtain microwave frequencies `MWFQ` (from all `.par` files),
    ## required for g value calculations
    mwfq.string <- "MW"
  }
  #
  ## delete `files.raw` which is not required anymore
  files.raw <- NULL
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
  ## --------------------------- SPECTRAL DATA READING  ------------------------------
  #
  if (x.unit == "G" || x.unit == "mT") {
    ## the all spectra with intensity correction
    ## to `qValue` + new column with `g`-factors
    #
    ## However prior to the operation above `x`/`B` has to be defined
    x <- grep("B|G|mT", col.names, value = TRUE)[[1]]
    #
    spectra.datab.from.files <-
      Map(
        function(s, t, u) {
          readEPR_Exp_Specs(s,
            qValue = t,
            x.unit = x.unit,
            col.names = col.names,
            origin = origin
          ) %>%
            dplyr::mutate(g_Value = eval_gFactor(
              nu = u,
              nu.unit = "Hz",
              B = .data[[x]],
              B.unit = x.unit
            ))
        },
        files.asc,
        qValues.from.files,
        mwfreq.from.files
      )
  }
  #
  if (x.unit == "Unitless") {
    ## the all spectra with intensity correction
    #
    spectra.datab.from.files <-
      Map(
        function(s, t) {
          readEPR_Exp_Specs(s,
            qValue = t,
            x.unit = x.unit,
            col.names = col.names,
            origin = origin
          )
        },
        files.asc,
        qValues.from.files
      )
  }
  #
  if (x.unit == "MHz") {
    ## the entire database of all spectra with intensity correction to `qValue`

    #
    spectra.datab.from.files <-
      Map(
        function(s, t) {
          readEPR_Exp_Specs(s,
            qValue = t,
            x.unit = x.unit,
            col.names = col.names,
            origin = origin
          )
        },
        files.asc,
        qValues.from.files
      )
  }
  #
  ## rename spectra according to desired parameter/quantity/...etc dependecy
  ## see params above
  names(spectra.datab.from.files) <- names
  #
  ## Weather to create a long table format (`tidy`) or not
  if (isFALSE(tidy)) {
    #
    return(spectra.datab.from.files)
    #
  } else {
    if (is.null(var2nd)) {
      stop(" 'var2nd' string is not provided. Please, define! ")
    } else {
      ## apply `bind_rows` to merge all spectral data from the list
      spectra.datab.from.files <-
        dplyr::bind_rows(spectra.datab.from.files, .id = var2nd) %>%
        dplyr::select(-.data$index) %>% ## remove index
        dplyr::mutate(new_variable = as.numeric(.data[[var2nd]])) %>% ## new column
        dplyr::mutate(new_variable = as.factor(new_variable)) %>%
        dplyr::select(-.data[[var2nd]]) %>% ## delete old col.
        dplyr::rename(!!quo_name(var2nd) := new_variable) ## rename new col. by the name of the old one
      ## the the latter can be expressed also by `dplyr::rename(!!var2nd := new_variable)`
      ## or by `dplyr::rename(!!rlang::sym(var2nd) := new_variable)`
      #
      return(spectra.datab.from.files)
      #
    }
  }
}
