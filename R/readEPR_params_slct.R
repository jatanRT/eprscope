#' Read the Selected EPR Instrumental Parameters and Information
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'  Taking selected instrumental parameters or information
#'  from the \code{.DSC/.dsc} or \code{.par} file of an EPR spectrum (written by the \code{Xenon}/\code{Magnettech}
#'  or \code{WinEpr} software, respectively).
#'
#'
#' @param path_to_dsc_par Character string, path to \code{.DSC/.dsc} or \code{.par} file including
#'   the instrumental parameters provided by the EPR machine. File path can be also defined
#'   by the \code{\link[base]{file.path}} function.
#' @param string Character (vector) string (appeared at the line beginning) within
#'   the \code{.DSC/.dsc} or \code{.par} file corresponding to instrumental parameter or information.
#'  Following \strong{strings are defined for all three main acquisition softwares described-above}
#'   (\strong{in parenthesis for the "winepr" origin}):
#'  \tabular{ll}{
#'   \strong{String} \tab \strong{Instrumental Parameter} \cr
#'    "OPER" ("JON") \tab operator (of the EPR instrument) \cr
#'    "CMNT" ("JCO") \tab comment (in order to describe the measurement) \cr
#'    "DATE" ("JDA") \tab date (when the EPR spectrum was recorded) \cr
#'    "TIME" ("JTM") \tab time (when the EPR spectrum was recorded) \cr
#'    "SAMP" \tab name/description of the sample, not available in "magnettech" \code{.dsc} \cr
#'    "TITL" ("JEX") \tab experiment title \cr
#'    "YNAM" ("JEY") \tab name/title for the second variable/dimension of the experiment \cr
#'    "B0MF" \tab modulation frequency in \code{Hz} \cr
#'    "MWFQ" ("MF") \tab microwave frequency in \code{Hz} (\code{GHz}) \cr
#'    "QValue" \tab recorded quality-Factor (required for intensity normalization) \code{unitless} \cr
#'    "A1CT" ("HCF") \tab central field (B) in \code{T} (\code{G}) \cr
#'    "A1SW" ("HSW") \tab sweep width in \code{T} (\code{G}) \cr
#'    "STMP" ("TE")  \tab temperature in \code{K} \cr
#'    "B0MA" ("RMA") \tab modulation amplitude in \code{T} (\code{G}) \cr
#'    "AVGS" ("JSD") \tab number of accumulations for each spectrum \cr
#'    "XPTS"/"A1RS" ("RES"/"SSX") \tab number of points/resolution \cr
#'    "XUNI" ("JUN"/"XXUN") \tab spectrum abscissa unit \cr
#'    "YUNI" ("XYUN") \tab unit corresponding to second variable (e.g. time) in spectral series \cr
#'    "XMIN" ("GST"/"XXLB") \tab actual \emph{x}/\emph{B} starting point (especially
#'    if teslameter in ON state) \cr
#'    "YPTS"/"A2RS" ("REY"/"SSY") \tab \emph{y}-resolution, corresponding to second
#'    variable (e.g. time) in spectral series \cr
#'    "XWID" ("GSI"/"XXWI") \tab actual \emph{x}/\emph{B} width (entire range, especially
#'    if teslameter in ON state) \cr
#'    "MWPW" ("MP") \tab microwave power in \code{W} (\code{mW}) \cr
#'    "SPTP" ("RCT") \tab conversion time in \code{s} (\code{ms}) \cr
#'    "RCTC" ("RTC") \tab time constant in \code{s} (ms),
#'    not available in "magnettech" \code{.dsc} \cr
#'    "RCAG" ("RRG") \tab signal receiver gain in \code{dB} (unitless),
#'    not available in "magnettech" \code{.dsc} \cr
#'    "ConvFact" \tab conversion factor/instr. calibration constant for quantitative
#'    analysis \code{unitless}, not available in "magnettech" \code{.dsc} \cr
#'  }
#' @param origin Character string, corresponding to software used to acquire EPR spectra.
#'   The files are slightly different depending on whether
#'   they were recorded by the "WinEpr",\code{origin = "winepr"}, "Xenon"
#'   (\strong{default}: \code{origin = "xenon"}) or by the "Magnettech" (ESR5000 [11-0422],
#'   \code{origin = "magnettech"}).
#'
#'
#' @return Numeric or character string (e.g. date or comment), corresponding to selected instrumental parameter(s)
#'   applied to record the EPR spectra. In case of \code{string} character vector, named list, containing
#'   either character and/or numeric values, is returned with the names corresponding to \code{string}.
#'
#'
#' @examples
#' ## loading `.DSC` (`Xenon`) parameter file example
#' triaryl_radCat_dsc_path <-
#'   load_data_example(file = "Triarylamine_radCat_decay_a.DSC")
#' #
#' ## reading modulation amplitude (in T) from the `Xenon` spectrometer file
#' readEPR_param_slct(triaryl_radCat_dsc_path,string = "B0MA")
#' #
#' ## reading Q-Value from the `Xenon` spectrometer file
#' readEPR_param_slct(triaryl_radCat_dsc_path,string = "QValue")
#' #
#' ## reading `CMNT` (comment) and `MWFQ` (microwave frequency in Hz)
#' ## from the `Xenon` spectrometer file
#' readEPR_param_slct(triaryl_radCat_dsc_path,
#'                    string = c("CMNT","MWFQ"))
#' #
#' ## loading `.par` (`WinEPR`) parameter file example
#' TMPD_radCat_par_path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.par")
#' #
#' ## reading `JDA` (date) from `WinEPR` spectrometer file
#' readEPR_param_slct(TMPD_radCat_par_path,
#'                    string = "JDA",
#'                    origin = "winepr")
#' #
#' ## reading `RMA` (modulation amplitude in G) and `TE`
#' ## (temperature in K) as well as `JCO` (comment)
#' ## from `WinEPR` spectrometer file
#' readEPR_param_slct(TMPD_radCat_par_path,
#'                    string = c("RMA","TE","JCO"),
#'                    origin = "WinEPR")
#' #
#' ## loading and reading the `.DSC` file from `Xenon`
#' ## corresponding to phenalenyl (PNT) CW ENDOR spectrum,
#' ## read expr. date (`TIME`), microwave frequency (`MWFQ`)
#' ## in Hz and the corresponding field for saturation (`B0VL`)
#' ## in Tesla:
#' pnt_endor_dsc_path <-
#'   load_data_example(file = "PNT_ENDOR_a.DSC")
#' readEPR_param_slct(pnt_endor_dsc_path,
#'                 string = c("TIME","MWFQ","B0VL")
#'                )
#'
#'
#'
#' @export
#'
#'
readEPR_param_slct <- function(path_to_dsc_par,
                               string,
                               origin = "xenon") {
  #
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
  ## conditions to couple `string` with origin
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))) {
    ## `winepr` vector check for `xenon`
    we.vector.check <- c(
      "JON","JCO","JDA","JTM","MF",
      "HCF","HSW","TE","RMA","JSD",
      "RES","MP","RCT","RTC","RRG",
      "GST","GSI","JEX","JUN","SSY",
      "JEY","XXLB","REY","XXWI","XYLB",
      "XYWI","XXUN"
    )
   #
    if (any(grepl(paste(we.vector.check,collapse = "|"),string))) {
      stop(" The `string` component(s) is (are) restricted to WinEPR.\n
           Please, provide `string(s)` for Xenon/Magnettech,\n
           refer to the `string` argument !! ")
    }
  }
  if (any(grepl(paste(magnettech.string,collapse = "|"),origin))) {
    if (any(string == "RCTC") ||
        any(string == "RCAG") ||
        any(string == "ConvFact") ||
        any(string == "SAMP")) {
      stop(" The `string` component(s) is (are) not available for Magnettech !!")
    }
  }
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
    #
    ## `xenon` vector check for `winepr`
    xen.vector.check <- c(
      "OPER","CMNT","DATE","TIME","SAMP",
      "B0MF","MWFQ","QValue","A1CT","A1SW",
      "STMP","B0MA","AVGS","A1RS","MWPW",
      "SPTP","RCTC","RCAG","ConvFact",
      "XPTS","XMIN","XWID","YPTS","YMIN",
      "YWID","TITL","IRNAM","XNAM","XUNI",
      "YNAM","YUNI","A2RS"
    )
    #
    if (any(grepl(paste(xen.vector.check,collapse = "|"),string))) {
      stop(" The WinEPR system does not provide defined string(s).\n
           Please, refer to the `string` argument for the available\n
           strings/parameters !! ")
    }
  }
  #
  ## path corresponds to file (`.DSC` or `.dsc`) from which the params. are read
  ## string is the selected 'string' pattern such as "QValue" or "MWFQ"
  ## if `string` = vector => iterate/read over all components
  sel.str.line <- lapply(
    string,
    function(s) {
      grep(
        paste0("^",s),
        readLines(path_to_dsc_par),
        value = TRUE
      )
    }
  )
  #
  ## such line is then separated (split) into two ('n = 2') string parts
  ## by 'str_split' comming from 'stringr' pckg.
  sel.str.split <- lapply(
    sel.str.line,
    function(l) {
      unlist(stringr::str_split(l,"[[:space:]]+",n = 2))
    }
  )
  #
  ## function to convert string vector into named list if the vector
  ## contains combined numbers and characters
  str_vec_conversion <- function(string.vec,names.vec){
    #
    ## check if each of these components can be converted
    ## into numeric value + suppress warnings
    suppressWarnings(
      check.char <- sapply(
        string.vec,
        function(l) is.na(as.double(l))
      )
    )
    #
    ## if the component cannot be converted into numeric
    ## then convert it into character, if one of the components
    ## <=> "NA" => suppress warnings
    suppressWarnings(
      result <- Map(
        function(i,j) {
          ifelse(isTRUE(i),as.character(j),as.double(j))
        },
        check.char,
        string.vec
      )
    )
    #
    ## rename the list + suppress warnings
    suppressWarnings(
      names(result) <- names.vec
      )
    #
    ## condition for the length of `names.vec` / `string.vec`,
    ## however, first of all check the length of both vectors
    if (length(string.vec) != length(names.vec)) {
      stop(" Vector of strings/parameters must be of the same length\n
           like the vector of their corresponding names !! ")
    } else {
      if (length(string.vec) > 1) {
        return(result)
      } else {
        return(unname(result[[1]]))
      }
    }
    #
  }
  #
  ## --------------------- XENON + MAGNETTECH ---------------------
  #
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))) {
    #
    if (any(grepl("SAMP",string)) &
        any(grepl(paste(xenon.string,collapse = "|"),origin))) {
      #
      param.slct <-
        sapply(sel.str.split, function(v) as.character(v[2]))
      #
      ## apply the above-defined function
      param.slct <-
        str_vec_conversion(string.vec = param.slct,names.vec = string)
      #
    } else if (any(grepl("SAMP",string)) &
               any(grepl(paste(magnettech.string,collapse = "|"),origin))) {
      #
      ## the `magnettech` parameter file does not contain
      ## the "SAMP" character string =>
      stop(" The required string is not available in `.dsc` file ! ")
      #
    } else {
      #
      param.slct <-
        sapply(sel.str.split, function(v) as.character(v[2]))
      #
      ## apply the above-defined function
      param.slct <-
        str_vec_conversion(
          string.vec = param.slct,
          names.vec = string
        )
    }
  }
  #
  ## ------------------------- WINEPR ----------------------------
  #
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
    #
    param.slct <-
      sapply(sel.str.split, function(v) as.character(v[2]))
    #
    ## apply the above-defined function
    param.slct <-
      str_vec_conversion(
        string.vec = param.slct,
        names.vec = string
      )
    #
  }
  #
  ## ======================== RESULT =========================
  #
  return(param.slct)
  #
}
#
#
#
#
#' Read the Selected Instrumental Parameters of EPR Time Series Experiment
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'  Function takes selected instrumental parameters relevant to \strong{time series ("kinetic")}
#'  experiment from the \code{.DSC/.dsc} or \code{.par} file of an EPR Spectrum, obtained from the "Xenon",
#'  "WinEpr" or "Magnettech" software. These parameters are required for time correction
#'  of the CW (continuous wave) EPR spectra, see the \code{\link{correct_time_Exp_Specs}}.
#'
#'
#' @inheritParams readEPR_param_slct
#' @param path_to_dsc_par String, path to \code{.DSC/.dsc} or \code{.par} file including all instrumental
#'   parameters provided by the EPR machine.
#'
#'
#' @return List containing:
#'   \describe{
#'   \item{Nscans}{Number of scans.}
#'   \item{swTime}{Sweep time in \code{s} required for time correction during the \code{2D_Field_Delay}
#'   (time series EPR experiment).}
#'   \item{Npoints}{Number of points (spectral resolution).}
#'   }
#'
#'
#' @examples
#' ## loading `.DSC` (`Xenon`) parameter file example
#' aminoxyl_dsc_path <-
#'   load_data_example(file = "Triarylamine_radCat_decay_series.DSC")
#' #
#' readEPR_params_slct_kin(aminoxyl_dsc_path)
#'
#'
#' @export
#'
#'
readEPR_params_slct_kin <- function(path_to_dsc_par, origin = "xenon") {
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
  ## condition for switching between xenon and magnettech
  xen.magnet.cond <- function(origin){
    if (any(grepl(paste(xenon.string,collapse = "|"),origin))){
      return(0)
    }
    if (any(grepl(paste(magnettech.string,collapse = "|"),origin))){
      return(1)
    }
  }
  #
  ## Load all required parameters from `.DSC`/`.dsc` or `.par`
  ## xenon or magnettech
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))) {
    #
    resol <- readEPR_param_slct(
      path_to_dsc_par,
      string = "XPTS",
      origin = origin
    )
    #
    convTime <- readEPR_param_slct(
      path_to_dsc_par,
      string = "SPTP",
      origin = origin
    )
    #
    NScans <- readEPR_param_slct(
      path_to_dsc_par,
      string =
        switch(
          2-xen.magnet.cond(origin = origin),
          "AVGS",
          "NbScansToDo"
        ),
      origin = origin
    )
    ## for kinetic measurements "AVGS" doesn't work,
    ## therefore select "NbScansToDo" for Xenon
  }
  ## winepr
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
    #
    resol <- readEPR_param_slct(
      path_to_dsc_par,
      string = "RES",
      origin = origin
    )
    #
    convTime <- readEPR_param_slct(
      path_to_dsc_par,
      string = "RCT",
      origin = origin
    )
    #
    NScans <- readEPR_param_slct(
      path_to_dsc_par,
      string = "JSD",
      origin = origin
    )
  }
  sweeptime <- resol * convTime
  #
  return(
    list(
      Nscans = NScans,
      swTime = sweeptime,
      Npoints = resol
    )
  )
  #
}
#
#
#
#
#' Read the Selected Instrumental Parameters Relevant to EPR Quantitative Analysis
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Reading the \code{.DSC/.dsc} or \code{.par} file to extract the important parameters like
#'   "modulation amplitude", "temperature", "microwave power" as well as "microwave frequency"
#'   which are are required for the absolute EPR quantitative analysis (\eqn{\equiv}
#'   radical or paramagnetic species number determination, see the \code{\link{quantify_EPR_Abs}} function).
#'
#'
#' @inheritParams readEPR_param_slct
#' @param path_to_dsc_par Character string, path (also provided by \code{\link[base]{file.path}})
#'   to \code{.DSC/.dsc} or \code{.par} (depending on OS, see the \code{origin} argument)
#'   \code{text} files including all instrumental parameters and provided by the EPR machine.
#'
#'
#' @return List consisting of:
#'   \describe{
#'   \item{BmmT}{Modulation amplitude value in \code{mT}.}
#'   \item{PmW}{Microwave source power in \code{mW}.}
#'   \item{TK}{Experimental temperature in \code{K}.}
#'   \item{mwGHz}{Microwave frequency value in \code{GHz}.}
#'   }
#'
#'
#' @examples
#' ## loading `.DSC` (`Xenon`) parameter file example
#' aminoxyl_dsc_path <-
#'   load_data_example(file = "Aminoxyl_radical_a.DSC")
#' #
#' readEPR_params_slct_quant(aminoxyl_dsc_path)
#'
#'
#' @export
#'
#'
## function to read instrumental parameters from `.DSC`/`.dsc` or `.par`
## required for quantification
readEPR_params_slct_quant <- function(path_to_dsc_par,
                                      origin = "xenon"){
  #
  ## reading the table and extracting values form table
  params.df <- readEPR_params_tabs(path_to_dsc_par,origin = origin)$params
  #
  ## modulation amplitude unit
  Bm.unit <- params.df %>%
    dplyr::filter(.data$Parameter == "Modulation Amplitude") %>%
    dplyr::pull(dplyr::all_of(c("Unit")))
  #
  ## modulation amplitude value and conversion depending on unit
  Bm <- params.df %>%
    dplyr::filter(.data$Parameter == "Modulation Amplitude") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  ## conversion + rounding
  Bm.mT <- convert_B(Bm,B.unit = Bm.unit,B.2unit = "mT") %>%
    round(digits = 3)
  #
  P.mW <- params.df %>%
    dplyr::filter(.data$Parameter == "Power") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  P.mW <- round(P.mW,digits = 2)
  #
  Temp.K <- params.df %>%
    dplyr::filter(.data$Parameter == "Temperature") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  Temp.K <- round(Temp.K)
  #
  nu.GHz <- params.df %>%
    dplyr::filter(.data$Parameter == "Frequency") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  nu.GHz <- round(nu.GHz,digits = 7)
  ## not required anymore =>
  rm(params.df)
  #
  named.params.list <- list(
    BmmT = Bm.mT,
    PmW = P.mW,
    TK = Temp.K,
    mwGHz = nu.GHz
  )
  #
  return(named.params.list)
}
#
#
#
#
#' Read the Selected Instrumental Parameters Required for EPR Simulations
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Reading the \code{.DSC/.dsc} or \code{.par} file to extract the important parameters like
#'   "sweep width", "central field", "number of points" as well as "microwave frequency"
#'   which are are required for the simulations of EPR spectra (see also
#'   the \code{\link{eval_sim_EPR_iso}} function).
#'
#'
#' @inheritParams readEPR_param_slct
#' @param path_to_dsc_par Character string, path (also provided by \code{\link[base]{file.path}})
#'   to \code{.DSC/.dsc} or \code{.par} (depending on OS, see the \code{origin} argument)
#'   \code{text} files including all instrumental parameters and provided by the EPR machine.
#' @param B.unit Character string, pointing to unit of magnetic flux density which is the output
#'   "unit", \code{"G"} ("Gauss") or \code{"mT"} ("millitesla"), for \code{"sweep width"}
#'   and \code{"central field"} (see also the \code{\link{eval_sim_EPR_iso}}).
#'   \strong{Default}: \code{B.unit = "G"}.
#'
#'
#' @return List consisting of:
#'   \describe{
#'   \item{Bcf}{Central field (magnetic flux density, \emph{B}) value in \code{B.unit}.}
#'   \item{Bsw}{Sweep width (magnetic flux density, \emph{B}, experimental range) value in \code{B.unit}.}
#'   \item{Npoints}{Number of points (spectral resolution).}
#'   \item{mwGHz}{Microwave frequency value in \code{GHz}.}
#'   }
#'
#'
#' @examples
#' ## loading `.par` (`WinEPR`) parameter file example
#' TMPD_radCat_par_path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.par")
#' #
#' ## `B` parameters in `mT`
#' readEPR_params_slct_sim(TMPD_radCat_par_path,
#'                         origin = "winepr",
#'                         B.unit = "mT")
#' #
#' ## loading `.dsc` (`Magnettech`) parameter
#' ## file example
#' AcridineRad.params.path <-
#'   load_data_example("AcridineDeriv_Irrad_365nm.dsc")
#' readEPR_params_slct_sim(AcridineRad.params.path,
#'                         origin = "magnettech")
#'
#'
#' @export
#'
#'
## function to read instrumental parameters from `.DSC`/`.dsc` or `.par`
## required for simulation
readEPR_params_slct_sim <- function(path_to_dsc_par,
                                    origin = "xenon",
                                    B.unit = "G"){
  #
  ## reading the table and extracting values form table
  params.df <- readEPR_params_tabs(path_to_dsc_par,origin = origin)$params
  #
  ## central field unit
  B.CF.unit <- params.df %>%
    dplyr::filter(.data$Parameter == "Central Field") %>%
    dplyr::pull(dplyr::all_of(c("Unit")))
  #
  ## central field value
  B.CF <- params.df %>%
    dplyr::filter(.data$Parameter == "Central Field") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  ## conversion + rounding
  B.CF <- convert_B(B.CF,B.unit = B.CF.unit,B.2unit = B.unit) %>%
    round(digits = 3)
  #
  ## sweep width unit
  B.SW.unit <- params.df %>%
    dplyr::filter(.data$Parameter == "Sweep Width") %>%
    dplyr::pull(dplyr::all_of(c("Unit")))
  #
  ## sweep width value
  B.SW <- params.df %>%
    dplyr::filter(.data$Parameter == "Sweep Width") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  B.SW <- convert_B(B.SW,B.unit = B.SW.unit,B.2unit = B.unit) %>%
    round(digits = 3)
  #
  Npoints <- params.df %>%
    dplyr::filter(.data$Parameter == "Number of Points") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  Npoints <- round(Npoints)
  #
  nu.GHz <- params.df %>%
    dplyr::filter(.data$Parameter == "Frequency") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  nu.GHz <- round(nu.GHz,digits = 7)
  ## not required anymore =>
  rm(params.df)
  #
  named.params.list <- list(
    Bcf = B.CF,
    Bsw = B.SW,
    Npoints = Npoints,
    mwGHz = nu.GHz
  )
  ## `B` quantities are returned in `B.unit` values
  #
  return(named.params.list)
}
