#
#' Load Several/Multiple EPR Data/Spectra Files Simultaneously
#'
#'
#' @family Data Reading
#'
#'
#' @description Loading the EPR spectra from several/multiple files (including the instrumental
#'   parameters in \code{.DSC}/\code{.dsc} or \code{.par} format) at once. Finally, the data are transformed
#'   either into a list of data frames (individual spectra) or into
#'   a \href{https://r4ds.hadley.nz/data-tidy.html#sec-tidy-data}{tidy/long table format}.
#'   According to experimental quantity (e.g. temperature, microwave power, recording time...etc),
#'   \code{names} and \code{var2nd.series} (in the case of \code{tidy = TRUE}) arguments have to be specified.
#'
#'
#' @inheritParams readEPR_Exp_Specs
#' @param name.pattern Character string ('specimen'), inherited from \code{\link[base]{list.files}}. A pattern
#'   from file name which may not necessarily appear at the beginning of the file name. One might also consult
#'   how to \href{https://r4ds.hadley.nz/regexps}{use regular expressions in R}. THE SAME NAME AND \code{name.pattern}
#'   MUST BE USED FOR ALL FILE NAMES WITHIN THE SERIES.
#' @param dir_asc_bin Path (defined by \code{\link[base]{file.path}} or by character string) to directory where
#'   the \code{ASCII} or \code{binary} (\code{.DTA} or \code{.spc}) files are stored. If both ASCII and binary files
#'   are located in the same directory, please refer to the \code{read.ascii} argument below.
#' @param dir_dsc_par Path (defined by \code{\link[base]{file.path}} or by character string) to directory
#'   where the \code{.DSC}/\code{.dsc} or \code{.par} files,including instrumental parameters, are stored.
#'   They can be also stored/located within the same \code{dir_asc_bin}.
#' @param read.ascii Logical, whether to read the ASCII EPR data files with the extensions \code{.csv}, \code{.txt}
#'   or \code{.asc} (\code{read.ascii = TRUE}, \strong{default}) or binary files like \code{.DTA} and \code{.spc}
#'   (\code{read.ascii = FALSE}).
#' @param col.names Character string vector, inherited from \code{\link[data.table]{fread}}, corresponding to
#'   column/variable names \strong{for individual file} (see also \code{\link{readEPR_Exp_Specs}}).
#'   A safe rule of thumb is to use column names including the physical quantity notation with its units,
#'   \code{Quantity_Unit} like \code{"B_G"}, \code{"RF_MHz"}, \code{"Bsim_mT"} (e.g. pointing
#'   to simulated EPR spectrum \eqn{x}-axis)...etc, \strong{default}: \code{col.names = c("index","B_G",dIepr_over_dB)}
#'   referring to column names coming from \emph{Xenon} software.
#' @param x.unit Character string pointing to unit of quantity (coming from the original data, see also
#'   \code{col.names} argument) which is to be presented on \eqn{x}-axis of the EPR spectrum.
#'   Units like \code{"G"} ("Gauss"), \code{"mT"} ("millitesla"), \code{"MHz"} ("megahertz" in case of ENDOR spectra)
#'   or \code{"Unitless"} in case of \eqn{g}-values can be used. \strong{Default}: \code{x.unit = "G"}.
#' @param qValues Numeric vector of Q-values (sensitivity factors to normalize EPR intensities) either loaded from
#'   the instrumental parameters (\code{.DSC} or \code{.par}), therefore \code{qValues = NULL} (\strong{default}),
#'   or in case of the \code{origin = "winepr"}, they have to be provided by the spectrometer operator.
#' @param norm.list.add Numeric list of vectors. Additional normalization constants in the form of vectors involving
#'   all (i.e. in addition to \code{qValue}) normalization(s) like e.g. concentration, powder sample
#'   weight, number of scans, ...etc (e.g. \code{norm.list.add = list(c(2000,0.5,2),c(1500,1,3))}). \strong{Default}:
#'   \code{norm.list.add = NULL}.
#' @param names Character string vector, corresponding either to values of \strong{additional - 2nd variable/quantity}
#'   (e.g. temperature,microwave power...etc, \code{c("240","250","260","270")}) or to \strong{general sample coding}
#'   by alpha character (e.g. \code{c("a","b","c","d")}) being varied by the individual experiments.
#' @param tidy Logical, whether to transform the list of data frames into the long table (\code{tidy}) format,
#'   \strong{default}: \code{tidy = FALSE}.
#' @param var2nd.series Character string, if \code{tidy = TRUE} (see also the \code{tidy} argument)
#'   it is referred to name of the 2nd variable/quantity (such as "time","Temperature","Electrochemical Potential",
#'   "Microwave Power"...etc) altered during individual experiments as a second variable series (\code{var2nd.series})
#'   and related to the spectral data.
#' @param var2nd.series.factor Logical, whether to factorize \code{var2nd.series} column vector which is useful
#'   for plotting the spectra in overlay form. \strong{Default}: \code{var2nd.series.factor = FALSE}, which is the case
#'   to visualize EPR spectra by \code{plot}-functions.
#' @param ... additional arguments specified, see also\code{\link{readEPR_Exp_Specs}}
#'   and \code{\link[data.table]{fread}}.
#'
#'
#' @return List of Data Frames (or long table \code{tidy} format) corresponding
#'   to multiple spectral data files/data sets. g-Value column (if \code{x.unit = "mT"} or \code{"G"})
#'   is automatically calculated during the processing and it is included in the data frame list/database as well.
#'
#'
#' @examples
#' \dontrun{
#' ## multiple ENDOR spectra at different temperatures recorded
#' ## by `Xenon` software read and transformed into `longtable`,
#' ## ready to plot the overlay EPR spectra
#' ## => `var2nd.series.factor = FALSE` (default).
#' readEPR_Exp_Specs_multif(
#'   name.pattern = "^.*_sample_VT_",
#'   file.path(".","ASCII_data_dir"),
#'   file.path(".","DSC_data_dir"),
#'   col.names = c("index",
#'                 "RF_MHz",
#'                 "Intensity"),
#'   x.unit = "MHz",
#'   names = c("210","220","230","240"),
#'   tidy = TRUE,
#'   var2nd.series = "Temperature_K"
#' )
#' #
#' ## multiple EPR spectra recorded at different temperatures
#' ## by `WinEPR` software, experiments performed with a powder
#' ## sample (m = 10 mg) and each spectrum acquired
#' ## as 7 accumulations, the resulting database
#' ## corresponds to list of data frames
#' readEPR_Exp_Specs_multif(
#'   "^Sample_VT_",
#'   dir_asc_bin = "./RAW_EPR_data_dir",
#'   dir_dsc_par = "./DSC_EPR_data_dir",
#'   read.ascii = FALSE,
#'   col.names = c("B_G","dIepr_over_dB"),
#'   x.unit = "G",
#'   names = c("210","220","230","240"),
#'   qValues = c(3400,3501,3600,2800),
#'   norm.list.add = rep(list(c(10,7)),times = 4),
#'   origin = "winepr"
#'  )
#' #
#' ## multiple `Xenon` EPR spectra related to one powder
#' ## sample (m = 8 mg) where several instrumental parameters
#' ## are changed at once, the file names (files are stored
#' ## in the actual directory) start with the "R5228_AV_powder_",
#' ## function returns all spectral data in `tidy` (long)
#' ## table format
#' readEPR_Exp_Specs_multif(
#'   name.pattern = "R5228_AV_powder_",
#'   dir_asc_bin = ".",
#'   dir_dsc_par = ".",
#'   names = c("a","b","c","d"),
#'   tidy = TRUE,
#'   var2nd.series = "sample",
#'   norm.list.add = rep(list(8),4))
#' #
#' ## time series coming from `Magnettech` as individual EPR spectra
#' ## (represented by binary files `.DTA`) recorded at different
#' ## time in seconds -> see `names` argument
#' readEPR_Exp_Specs_multif(
#'   name.pattern = "Kinetics_EPR_Spectra_",
#'   dir_asc_bin = "./Input_EPR_Data",
#'   dir_dsc_par = "./Input_EPR_Data",
#'   read.ascii = FALSE,
#'   names = c("20","40","60","80","100","120"),
#'   tidy = TRUE,
#'   var2nd.series = "time_s",
#'   origin = "magnettech"
#' )
#' #
#' ## time series coming from `Magnettech` as individual EPR spectra
#' ## (represented by ASCII files `.csv`) recorded at different
#' ## time in seconds -> see `names` argument
#' data.time.series.mngtech <-
#'   readEPR_Exp_Specs_multif(
#'     name.pattern = "Kinetics_EPR_Spectra_",
#'     dir_asc_bin = "./Input_EPR_Data",
#'     dir_dsc_par = "./Input_EPR_Data",
#'     col.names = c("B_mT","dIepr_over_dB"),
#'     x.unit = "mT",
#'     names = as.character(c(1:12)), # total number of spectra: 12
#'     tidy = TRUE,
#'     var2nd.series = "Slice",
#'     origin = "magnettech"
#'   )
#' #
#' ## convert the `Slice` column (of the previous data) into `time_s`,
#' ## using the `dplyr` package, time interval between recording
#' ## of the two consecutive Slices/EPR spectra corresponds to 32 s:
#' data.time.series.mngtech <-
#'   data.time.series.mngtech %>%
#'     mutate(time_s = (Slice - 1) * 32) # 32 s time interval
#' ## correct time, if the middle of an individual spectrum
#' ## appears at the sweep time half
#' data.time.series.mngtech$time_s <-
#'   correct_time_Exp_Specs(
#'     time.s = data.time.series.mngtech$time_s,
#'     # how many accumulations per individual EPR spectrum ? :
#'     Nscans = 1,
#'     # experimental sweep time (for one EPR spectrum) in seconds:
#'     sweep.time.s = 26
#'    )
#' #
#' }
#'
#'
#' @export
#'
#'
#' @importFrom rlang quo_name :=
readEPR_Exp_Specs_multif <- function(name.pattern,
                                     dir_asc_bin,
                                     dir_dsc_par,
                                     read.ascii = TRUE,
                                     col.names = c(
                                       "index",
                                       "B_G",
                                       "dIepr_over_dB"
                                     ),
                                     x.unit = "G",
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
  ## =========================== FILES AND PARAMETERS ==============================
  #
  ## file name pattern depending on binary/ascii +`DSC`/`.dsc`/`.par`
  if (isTRUE(read.ascii)) {
    #
    file.name.pattern <- paste0(name.pattern,".*\\.(txt|asc|csv)$")
    message(" Please note, you're reading multiple ASCII `.csv`/`.txt`/`.asc` files ! ")
    #
  } else {
    #
    file.name.pattern <- paste0(name.pattern,".*\\.(DTA|spc)$")
    message(" Please note, you're reading multiple BINARY `.spc`/`.DTA` files ! ")
    #
  }
  #
  file.name.pattern.params <- paste0(name.pattern,".*\\.(DSC|dsc|par)$")
  #
  ## path to all ascii or binary files
  files.specs <- list.files(
    path = dir_asc_bin,
    pattern = file.name.pattern,
    full.names = TRUE
  )
  #
  ## checking the path string, whether it points to ASCII or BINARY
  ## + checking the corresponding origin
  ascii.cond <- any(grepl(".*\\.(txt|asc|csv)$",files.specs))
  binary.cond <- any(grepl(".*\\.(DTA|spc)$",files.specs))
  #
  if (any(grepl(".*\\.spc$",files.specs))) {
    if (origin.cond.all(origin = origin) == 2 ||
        origin.cond.all(origin = origin) == 1) {
      stop(" Reading of the `.spc` file requires origin = 'winepr' !! ")
    }
  }
  if (any(grepl(".*\\.DTA$",files.specs))) {
    if (origin.cond.all(origin = origin) == 0) {
      stop(" Reading of the `.DTA` file requires\n
           origin = 'xenon' or origin = 'magnettech' !! ")
    }
  }
  #
  ## path to `.DSC`/`.dsc`  or  `.par`  files
  files.params <- list.files(
    path = dir_dsc_par,
    pattern = file.name.pattern.params,
    full.names = TRUE
  )
  #
  ## xenon or magnettech
  if (origin.cond.all(origin = origin) == 2 ||
      origin.cond.all(origin = origin) == 1) {
    ## to obtain `QValues` (from all `.DSC`/`.dsc` files) run the following
    if (is.null(qValues)) {
      ## automatic
      qValues.from.files <- sapply(
        files.params,
        function(x) readEPR_param_slct(x, string = "QValue",origin = origin)
      )
      #
      ## check the values of the previous vector
      if (any(is.null(qValues.from.files)) || any(is.na(qValues.from.files))) {
        stop(" One or more Q-Values in the parameter `.dsc`/`.DSC` file(s)\n
             is(are) missing !! Intensities cannot be normalized !!\n
             Please, specify the `qValues` vector/argument accordingly !! ")
      } else {
        qValues.from.files <- qValues.from.files
      }
    } else {
      qValues.from.files <- qValues
    }
    #
    ## to obtain microwave frequencies `MWFQ` (from all `.DSC`/`.dsc` files),
    ## required for g value calculations
    mwfq.string <- "MWFQ"
  }
  ## winepr
  if (origin.cond.all(origin = origin) == 0) {
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
  if (origin.cond.all(origin = origin) == 0) {
    ## conversion from "GHz" to "Hz"
    mwfreq.from.files <- mwfreq.from.files * 1e+9
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
  ## ------ conditions and definitions for `.id`s ------------
  ## ---------- in order to simplify the function ------------
  ## `x.id`
  if (exists("x.id")) {
    x.id <- switch(
      3 - origin.cond.all(origin = origin),
      x.id %>% `if`(x.id != 2, 2, .), ## check xenon
      x.id %>% `if`(x.id != 1 || x.id != 2,
                    switch(2 - binary.cond, 2, 1), .), ## magnettech
      x.id %>% `if`(x.id != 1, 1, .) ## check winepr
    )
  } else {
    x.id <- switch(
      3 - origin.cond.all(origin = origin),
      2,
      switch(2 - binary.cond, 2, 1),
      1
    )
  }
  ## `Intensity.id`
  if (exists("Intensity.id")) {
    Intensity.id <- switch(
      3 - origin.cond.all(origin = origin),
      Intensity.id %>% `if`(Intensity.id != 3, 3, .), ## check xenon
      Intensity.id %>% `if`(Intensity.id != 2 || Intensity.id != 3,
                            switch(2 - binary.cond, 3, 2), .), ## magnettech
      Intensity.id %>% `if`(Intensity.id != 2, 2, .) ## check winepr
    )
  } else {
    Intensity.id <- switch(
      3 - origin.cond.all(origin = origin),
      3,
      switch(2 - binary.cond, 3, 2),
      2
    )
  }
  #
  ## However prior to the operation above `x`/`B` has to be defined
  xString <- col.names[x.id]
  #
  if (x.unit == "G" || x.unit == "mT"){
    spectra.datab.from.files <-
      Map(
        function(p, r, s, t, u) {
          readEPR_Exp_Specs(path_to_file = p,
                            path_to_dsc_par = r,
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
        files.specs,
        switch(2 - binary.cond,files.params,NULL),
        qValues.from.files,
        norm.list.add,
        mwfreq.from.files
      )
  } else {
    spectra.datab.from.files <-
      Map(
        function(p, r, s, t) {
          readEPR_Exp_Specs(path_to_file = p,
                            path_to_dsc_par = r,
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
        switch(2 - binary.cond,files.params,NULL),
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
      stop(" The 'var2nd.series' string argument is not specified.\n
           Please, define the corresponding column name for the tidy data ! ")
    } else {
      #
      ## condition whether the `names` contains only numeric values
      ## i.e. no characters ("alpha")
      names.num.cond <- ifelse(any(!grepl("[[:alpha:]]",names)),TRUE,FALSE)
      #
      ## apply `bind_rows` to merge all spectral data from the list
      spectra.datab.from.files <-
        dplyr::bind_rows(spectra.datab.from.files, .id = var2nd.series)
      ## remove index if present
      if (any(grepl("index",colnames(spectra.datab.from.files)))){
        spectra.datab.from.files$index <- NULL
      }
      #
      ## based on conditions for the numeric values (see `names.num.cond` above)
      if (isFALSE(names.num.cond)){
        spectra.datab.from.files <- spectra.datab.from.files
      } else {
        ## recalculate `var2nd.series` column
        ## condition to factorize the `var2nd.series`
        cond.var2nd.factor <- ifelse(var2nd.series.factor,TRUE,FALSE)
        #
        spectra.datab.from.files <-
          switch(2 - cond.var2nd.factor,
                 spectra.datab.from.files %>%
                   dplyr::mutate(!!rlang::quo_name(var2nd.series) :=
                                   as.factor(as.numeric(.data[[var2nd.series]]))),
                 spectra.datab.from.files %>%
                   dplyr::mutate(!!rlang::quo_name(var2nd.series) :=
                                   as.numeric(.data[[var2nd.series]])) %>%
                   dplyr::arrange(.data[[var2nd.series]])
          )
      }
      #
      return(spectra.datab.from.files)
      #
    }
  }
}
