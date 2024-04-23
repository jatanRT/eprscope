#
#' Load Several/Multiple EPR Spectral Data Files with Parameters
#'
#'
#' @family Data Reading
#'
#'
#' @description Loads the EPR spectra from several/multiple \code{ASCII}/\code{text} files and from those incl. instrumental
#'  parameters (\code{.DSC}/\code{.dsc} or \code{.par}) at once and transforms it into a database list of data frames.
#'
#'
#' @details
#'   Function is based on the \code{\link[base]{list.files}} and \code{\link{readEPR_Exp_Specs}} into
#'   one list/database. According to  experiment quantity (e.g. temperature,microwave power,recording time...etc),
#'  \code{names} and \code{var2nd} (in case of \code{tidy = TRUE}) parameters have to be provided.
#'  If intensity normalization by e.g. like concentration, sample weight...etc is required,
#'  it can be performed afterwards (generally, except the Q values, it is not included
#'  in the transformation process).
#'
#'
#' @inheritParams readEPR_Exp_Specs
#' @param name_pattern Character string ('specimen'), inherited from \code{\link[base]{list.files}}. A pattern
#'  from name which might not necessarily appear at the beginning of the file name.
#' @param dir_ASC path (defined by \code{\link[base]{file.path}}, String/Character) to directory where
#'  the `ascii` files are stored
#' @param dir_dsc_par path (defined by \code{\link[base]{file.path}} String/Character) to directory
#'  where the files (\code{.DSC}/\code{.dsc} or \code{.par}) with instrumental parameters (to calculate \eqn{g}-value
#'  or normalize intensities) are stored
#' @param col.names Character/String vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names \strong{for individual file} (see also \code{\link{readEPR_Exp_Specs}}).
#'   A safe rule of thumb is to use column names incl. physical quantity notation with its units,
#'   \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"}, \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum abscissa)...etc, \strong{default}: \code{col.names = c("index","B_G",dIepr_over_dB)}.
#' @param x.id Numeric index related to \code{col.names} pointing to independent variable, which corresponds
#'   to abscissa (\eqn{x}-axis) in spectra or other plots.
#' @param x.unit Character/String pointing to unit of quantity (coming from original ASCII data, see also
#'   \code{column.names} parameter) which is to be presented on \eqn{x} abscissa of the EPR spectrum,
#'   like \code{"G"} ("Gauss"), \code{"mT"} ("millitesla"), \code{"MHz"} ("megahertz" in case of ENDOR spectra)
#'   or \code{"Unitless"} in case of \eqn{g}-values, \strong{default}: \code{x.unit = "G"}.
#' @param Intensity.id Numeric index related to \code{col.names} pointing to "general" intensity,
#'   like derivative intensity (\code{dIepr_over_dB}), integral one (e.g. \code{single_Integ}), double or sigmoid
#'   integral (e.g. \code{Area})...etc. This corresponds to column/vector which should be presented like
#'   \eqn{y}-axis in spectra or other plots.
#' @param convertB.unit Logical (\strong{default}: \code{convertB.unit = TRUE}) description...
#'   convert \eqn{B} in Gauss <=> millitesla...
#' @param qValues Numeric Vector, Q-value (sensitivity factors to normalize EPR intensity) either loaded from
#'  files incl. parameters (\code{.DSC} or \code{.par}) by this function/R.script (therefore \code{qValues = NULL},
#'  \strong{default}) or in case of \code{origin = "winepr"} they have to be provided by the spectrometer operator.
#' @param norm.list.add Numeric list of vectors. Additional normalization constants in form of vectors involving
#'   all additional (in addition to \code{qValue}) normalization(s) like e.g. concentration, powder sample
#'   weight, number of scans, ...etc (\code{norm.list.add = list(c(2000,0.5,2),c(1500,1,3))}). \strong{Default}:
#'   \code{norm.list.add = NULL}.
#' @param names String/Character Vector corresponding to values of \strong{additional quantity}
#'  (e.g. temperature,microwave power...etc) being varied by the individual experiments
#' @param tidy Logical, whether to transform the list of data frames into long table (\code{tidy}) format,
#'  \strong{default}: \code{tidy = FALSE}.
#' @param var2nd.series String/Character, if \code{tidy = TRUE} (see \code{tidy} parameter/argument)
#'  it is referred to name of the variable/quantity (e.g. like "time","Temperature","Electrochemical Potential",
#'  "Microwave Power"...etc) altered upon individual experiments as a second variable (\code{var2nd})
#'  and related to spectra/data.
#' @param var2nd.series.factor Logical, description ...TBC ... factorize \code{var2nd.series}, useful for plotting
#'   the overlay spectra. \strong{Default}: \code{var2nd.series.factor = FALSE}, the case to visualize
#'   the EPR spectra by \code{plot}-functions.
#' @param ... additional arguments specified, see also\code{\link{readEPR_Exp_Specs}}
#'   and \code{\link[data.table]{fread}}.
#'
#'
#' @return List of Data Frames (or long table \code{tidy} format) corresponding to multiple spectral data files/data sets.
#'
#'
#' @examples
#' \dontrun{
#' ## Multiple ENDOR spectra at different temperatures recorded by "Xenon" software
#' ## reading and transforming into `longtable`. Prepared for plotting the overlay
#' ## EPR spectra => `var2nd.series.factor = FALSE` (default).
#' readEPR_Exp_Specs_multif(name_pattern = "Sample_VT_",
#'                          file.path(".","ASCII_data_dir"),
#'                          file.path(".","DSC_data_dir"),
#'                          col.names = c("index",
#'                                        "RF_MHz",
#'                                        "Intensity"),
#'                          x.id = 2,
#'                          x.unit = "MHz",
#'                          Intensity.id = 3,
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
#'                          x.id = 1,
#'                          x.unit = "G",
#'                          Intensity.id = 2,
#'                          names = c("210","220","230","240"),
#'                          qValues = c(3400,3501,3600,2800),
#'                          norm.list.add = list(rep(c(10,7),times = 4)),
#'                          origin = "winepr")
#' }
#'
#'
#' @export
#'
#'
#' @importFrom rlang quo_name :=
readEPR_Exp_Specs_multif <- function(name_pattern,
                                     dir_ASC,
                                     dir_dsc_par,
                                     col.names = c(
                                       "index",
                                       "B_G",
                                       "dIepr_over_dB"
                                     ),
                                     x.id = 2,
                                     x.unit = "G",
                                     Intensity.id = 3,
                                     convertB.unit = TRUE,
                                     qValues = NULL,
                                     norm.list.add = NULL,
                                     names,
                                     tidy = FALSE,
                                     var2nd.series = NULL,
                                     var2nd.series.factor = FALSE,
                                     origin = "xenon",
                                     ...) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  g_Value <- NULL
  index <- NULL
  #
  ## origin strings vectors to define "origin" conditions =>
  winepr.string <- c("winepr","Winepr","WinEpr","WINEPR","WinEPR","winEPR")
  xenon.string <- c("xenon","Xenon","XENON")
  magnettech.string <- c("magnettech","Magnettech","MagnetTech","magnetTech","MAGNETECH")
  #
  ## =========================== FILES AND PARAMETERS ==============================
  #
  ## file name pattern which has to be the same for `txt`+`DSC`/`.dsc`
  ## or `.asc` and `.par`
  file.name.pattern.asc <- paste0(name_pattern,".*\\.(txt|asc|csv)$")
  file.name.pattern.params <- paste0(name_pattern,".*\\.(DSC|dsc|par)$")
  #
  ## path to all `asc` files
  files.asc <- list.files(
    path = dir_ASC,
    pattern = file.name.pattern.asc,
    full.names = TRUE
  )
  #
  ## path to `.DSC`/`.dsc`  or  `.par`  files
  files.params <- list.files(
    path = dir_dsc_par,
    pattern = file.name.pattern.params,
    full.names = TRUE
  )
  #
  ## xenon or magnettech
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))) {
    ## to obtain `QValues` (from all `.DSC`/`.dsc` files) run the following
    qValues.from.files <- sapply(
      files.params,
      function(x) readEPR_param_slct(x, string = "QValue",origin = origin)
    )
    ## to obtain microwave frequencies `MWFQ` (from all `.DSC`/`.dsc` files),
    ## required for g value calculations
    mwfq.string <- "MWFQ"
  }
  ## winepr
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
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
  ## winepr
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
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
  xString <- col.names[x.id]
  #
  if (x.unit == "G" || x.unit == "mT"){
    spectra.datab.from.files <-
      Map(
        function(r, s, t, u) {
          readEPR_Exp_Specs(r,
                            col.names = col.names,
                            x.id = x.id,
                            x.unit = x.unit,
                            Intensity.id = Intensity.id,
                            convertB.unit = convertB.unit,
                            qValue = s,
                            norm.vec.add = t,
                            origin = origin,
                            ...
          ) %>%
            dplyr::mutate(g_Value = eval_gFactor(
              nu.val = u,
              nu.unit = "Hz",
              B.val = .data[[xString]],
              B.unit = x.unit
            ))
        },
        files.asc,
        qValues.from.files,
        norm.list.add,
        mwfreq.from.files
      )
  } else {
    spectra.datab.from.files <-
      Map(
        function(r, s, t) {
          readEPR_Exp_Specs(r,
                            col.names = col.names,
                            x.id = x.id,
                            x.unit = x.unit,
                            Intensity.id = Intensity.id,
                            convertB.unit = convertB.unit,
                            qValue = s,
                            norm.vec.add = t,
                            origin = origin,
                            ...
          )
        },
        files.asc,
        qValues.from.files,
        norm.list.add
      )
  }
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
      ## condition to factorize the `var2nd.series`
      cond.var2nd.factor <- ifelse(var2nd.series.factor,TRUE,FALSE)
      #
      spectra.datab.from.files <-
        switch(2-cond.var2nd.factor,
               spectra.datab.from.files %>%
                 dplyr::mutate(!!rlang::quo_name(var2nd.series) :=
                                 as.factor(as.numeric(.data[[var2nd.series]]))),
               spectra.datab.from.files %>%
                 dplyr::mutate(!!rlang::quo_name(var2nd.series) :=
                                 as.numeric(.data[[var2nd.series]])) %>%
                 dplyr::arrange(.data[[var2nd.series]])
               )
      #
      return(spectra.datab.from.files)
      #
    }
  }
}
