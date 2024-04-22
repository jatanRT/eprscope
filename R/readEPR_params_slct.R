#' Read the \strong{Selected} EPR Instrumental Parameters and Information
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'  Function takes the \strong{selected} instrumental parameters or information
#'  from \code{.DSC/.dsc} or \code{.par} file of an EPR Spectrum (written by the \code{Xenon}/\code{Magnettech}
#'  or \code{WinEpr} Software, respectively)
#'
#'
#' @param path_to_dsc_par String, path to \code{.DSC/.dsc} or \code{.par} file including all instrumental
#'   parameters provided by the EPR machine, path can be provided by \code{\link[base]{file.path}}
#' @param string String, within the \code{.DSC/.dsc} or \code{.par} (at the line beginning) file
#'   corresponding to instrumental parameter,
#'  following \strong{strings are defined for all three main acquisition software described-above}
#'   (\strong{in parenthesis for "winepr" software}):
#'  \tabular{ll}{
#'   \strong{String} \tab \strong{Instrumental Parameter} \cr
#'    "OPER" ("JON") \tab  operator (of the EPR instrument) \cr
#'    "CMNT" ("JCO") \tab  comment (in order to describe the measurement) \cr
#'    "DATE" ("JDA") \tab  date (when the EPR spectrum was recorded) \cr
#'    "TIME" ("JTM") \tab  time (when the EPR spectrum was recorded) \cr
#'    "SAMP" \tab   name/decsript. of the sample,
#'    not available in "magnettech" \code{.dsc} \cr
#'    "B0MF" \tab  modulation frequency in \code{Hz} \cr
#'    "MWFQ"  ("MF") \tab microwave frequency in \code{Hz} (\code{GHz}) \cr
#'    "QValue" \tab  recorded quality-Factor (required for intensity norm.) \code{unitless} \cr
#'    "A1CT" ("HCF") \tab central field (B) in \code{T} (\code{G}) \cr
#'    "A1SW" ("HSW") \tab  sweep width in \code{T} (\code{G}) \cr
#'    "STMP" ("TE")  \tab  temperature in \code{K} \cr
#'    "B0MA" ("RMA") \tab  modulation amplitude in \code{T} (\code{G}) \cr
#'    "AVGS" ("JSD") \tab  number of accumulations for each spectrum \cr
#'    "A1RS" ("RES") \tab  number of points/resolution \cr
#'    "MWPW" ("MP") \tab microwave power in \code{W} (\code{mW}) \cr
#'    "SPTP" ("RCT") \tab  conversion time in \code{s} (\code{ms}) \cr
#'    "RCTC" ("RTC") \tab  time constant in \code{s} (ms),
#'    not available in "magnettech" \code{.dsc} \cr
#'    "RCAG" ("RRG") \tab signal receiver gain in \code{dB} (unitless),
#'    not available in "magnetech" \code{.dsc} \cr
#'    "ConvFact" \tab conversion factor/instr. calibration constant for quantitative
#'    analysis \code{unitless}, not available in "magnettech" \code{.dsc} \cr
#'  }
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the files are slightly different depending on whether
#'   they were recorded by the "WinEpr",\code{origin = "winepr"} softw. or by the "Xenon"
#'   ("Magnettech", \code{origin = "magnettech"}) one. \strong{Default}: \code{origin = "xenon"}.
#'
#' @return Numeric or character string (e.g. date or comment) corresponding to selected (\code{slct})
#'   instrumental parameter applied to record the EPR spectra.
#'
#'
#' @examples
#' \dontrun{
#' ## Reading modulation amplitude from 'Xenon' spectrometer file
#' readEPR_param_slct(path_to_dsc_par,
#'                    string = "B0MA")
#'
#' ## Reading Q Value from 'Xenon' spectrometer file
#' read_param_slct(file.path(".",
#'                           "dir_DSC",
#'                           "EPR_spectrum.DSC"),
#'                 string = "QValue")
#'
#' ## Reading `date` from 'WinEPR' spectrometer file
#' read_param_slct(file.path(".",
#'                           "dir_par",
#'                           "EPR_spectrum.par"),
#'                 string = "JDA",
#'                 origin = "winepr")
#' }
#'
#'
#' @export
#'
readEPR_param_slct <- function(path_to_dsc_par,
                               string,
                               origin = "xenon") {
  #
  ## path corresponds to file (`.DSC` or `.dsc`) from which the params. are read
  ## string is the selected 'string' pattern e.g. like "QValue" or "MWFQ"
  sel.str.line <- grep(paste0("^",string), readLines(path_to_dsc_par), value = TRUE)
  #
  ## such line is then separated (split) into two ('n = 2') string parts
  ## by 'str_split' comming from 'stringr' pckg.
  sel.str.split <- stringr::str_split(sel.str.line, "[[:space:]]+", n = 2)
  #
  ## the result is list, therefore select the second list element ('[[1]][2]'),
  ## therefore unlist the `sel.str.split`
  sel.str.split <- unlist(sel.str.split)
  #
  ## origin strings vectors to define "origin" conditions =>
  winepr.string <- c("winepr","Winepr","WinEpr","WINEPR","WinEPR","winEPR")
  xenon.string <- c("xenon","Xenon","XENON")
  magnettech.string <- c("magnettech","Magnettech","MagnetTech","magnetTech","MAGNETECH")
  #
  if (any(grepl(paste(xenon.string,collapse = "|"),origin)) ||
      any(grepl(paste(magnettech.string,collapse = "|"),origin))) {
    if (string == "OPER" || string == "CMNT" ||
        string == "DATE" || string == "TIME") {
      param.slct <- as.character(sel.str.split[2])
    } else {
      param.slct <- as.double(sel.str.split[2])
    }
  }
  #
  if (any(grepl(paste(xenon.string,collapse = "|"),origin))){
    if (string == "SAMP"){
      param.slct <- as.character(sel.str.split[2])
    }
  }
  #
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
    if (string == "JON" || string == "JCO" || string == "JDA" ||
        string == "JTM") {
      param.slct <- as.character(sel.str.split[2])
    } else {
      param.slct <- as.double(sel.str.split[2])
    }
  }
  #
  return(param.slct)
  #
}
#
#
#
#
#' Read the \strong{Selected} Instrumental Parameters Relevant to \strong{Time Series} Experiment
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'  Function takes the \strong{selected} instrumental parameters relevant to \strong{time series ("kinetic")}
#'  experiment from \code{.DSC/.dsc} or \code{.par} file of an EPR Spectrum (written by the `Xenon`
#'  or `WinEpr` software, respectively). These parameters are required for the time correction of EPR
#'  spectra, see \code{\link{correct_time_Exp_Specs}}
#'
#'
#' @param path_to_dsc_par String, path to \code{.DSC/.dsc} or \code{.par} file including all instrumental
#'   parameters provided by the EPR machine
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the files are slightly different depending on whether they were recorded
#'   by the windows based softw. ("WinEpr",\code{origin = "winepr"}) or by the Linux one ("Xenon"),
#'   \strong{default}: \code{origin = "xenon"}
#'
#'
#' @return List containing:
#'   \describe{
#'   \item{Nscans}{Number of scans.}
#'   \item{swTime}{Sweep time in `s` required for time correction during the `2D_Field_Delay`
#'   (time series EPR experiment).}
#'   \item{Npoints}{Number of points (spectral resolution).}
#'   }
#'
#'
#' @examples
#' \dontrun{
#' readEPR_params_slct_kin(path_to_dsc_par)
#' readEPR_params_slct_kin(file.path(".",
#'                                   "dir_par",
#'                                   "EPR_spectrum.par"),
#'                         origin = "winepr")
#' }
#'
#' @export
#'
#'
readEPR_params_slct_kin <- function(path_to_dsc_par, origin = "xenon") {
  #
  ## origin strings vectors to define "origin" conditions =>
  winepr.string <- c("winepr","Winepr","WinEpr","WINEPR","WinEPR","winEPR")
  xenon.string <- c("xenon","Xenon","XENON")
  magnettech.string <- c("magnettech","Magnettech","MagnetTech","magnetTech","MAGNETECH")
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
    resol <- readEPR_param_slct(path_to_dsc_par,
                                string = "A1RS",
                                origin = origin
    )
    convTime <- readEPR_param_slct(path_to_dsc_par,
                                   string = "SPTP",
                                   origin = origin
    )
    NScans <- readEPR_param_slct(path_to_dsc_par,
                                 string = switch(2-xen.magnet.cond(origin = origin),
                                                 "AVGS",
                                                 "NbScansToDo"),
                                 origin = origin
    )
    ## for kinetic measurements "AVGS" doesn't work, therefore select "NbScansToDo" for Xenon
  }
  ## winepr
  if (any(grepl(paste(winepr.string,collapse = "|"),origin))) {
    #
    resol <- readEPR_param_slct(path_to_dsc_par,
                                string = "RES",
                                origin = origin
    )
    convTime <- readEPR_param_slct(path_to_dsc_par,
                                   string = "RCT",
                                   origin = origin
    )
    NScans <- readEPR_param_slct(path_to_dsc_par,
                                 string = "JSD",
                                 origin = origin
    )
  }
  sweeptime <- resol * convTime
  #
  return(list(Nscans = NScans, swTime = sweeptime, Npoints = resol))
  #
}
#
#
#
#
#' Read the \strong{Selected} Instrumental Parameters Relevant to EPR Quantitative Analysis
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Reading the \code{.DSC/.dsc} or \code{.par} file to extract the important parameters like
#'   "modulation amplitude", "temperature", "microwave power" as well as "microwave frequency"
#'   which are are required for absolute quantitative analysis of the EPR spectra (\eqn{\equiv}
#'   radical or paramagnetic species number determination).
#'
#'
#'
#' @param path_to_dsc_par Character string, path (also provided by \code{\link[base]{file.path}})
#'   to \code{.DSC/.dsc} or \code{.par} (depending on OS, see \code{origin} parameter)
#'   \code{text} files including all instrumental parameters and provided by the EPR machine.
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the files are slightly different depending on whether they
#'   were recorded by the windows based softw. ("WinEpr",\code{origin = "winepr"}) or by the Linux
#'   one ("Xenon"). \strong{Default}: \code{origin = "xenon"}.
#'
#'
#' @return List consisting of:
#'   \describe{
#'   \item{BmmT}{Modulation amplitude value in `mT`.}
#'   \item{PmW}{Microwave source power in `mW`.}
#'   \item{TK}{Experimental temperature in `K`.}
#'   \item{mwGHz}{Microwave frequency value in `GHz`.}
#'   }
#'
#'
#' @examples
#' \dontrun{
#' TODO
#' TODO
#' }
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
  Bm.mT <- params.df %>%
    dplyr::filter(.data$Parameter == "Modulation Amplitude") %>%
    dplyr::pull(dplyr::all_of(c("Value")))
  Bm.mT <- round(Bm.mT,digits = 3)
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
  named.params.list <- list(BmmT = Bm.mT,PmW = P.mW,TK = Temp.K,mwGHz = nu.GHz)
  return(named.params.list)
}
#
#
#
#
#' Read the \strong{Selected} Instrumental Parameters Relevant to EPR \strong{Simulations}
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Reading the \code{.DSC/.dsc} or \code{.par} file to extract the important parameters like
#'   "sweep width", "central field", "number of points" as well as "microwave frequency"
#'   which are are required for the simulations of EPR spectra (see \code{\link{eval_sim_EPR_iso}}).
#'
#'
#'
#' @param path_to_dsc_par Character string, path (also provided by \code{\link[base]{file.path}})
#'   to \code{.DSC/.dsc} or \code{.par} (depending on OS, see \code{origin} parameter)
#'   \code{text} files including all instrumental parameters and provided by the EPR machine.
#' @param origin String, corresponding to software which was used to acquire the EPR spectra
#'   on BRUKER spectrometers, because the files are slightly different depending on whether they
#'   were recorded by the windows based softw. ("WinEpr",\code{origin = "winepr"}) or by the Linux
#'   one ("Xenon"). \strong{Default}: \code{origin = "xenon"}.
#' @param B.unit Character string pointing to unit of magnetic flux density which is the output
#'   `unit`, \code{"G"} (`Gauss`) or \code{"mT"} (`millitesla`), for \code{"sweep width"}
#'   and \code{"central field"} (see \code{\link{eval_sim_EPR_iso}}).
#'   \strong{Default}: \code{B.unit = "G"}.
#'
#'
#' @return List consisting of:
#'   \describe{
#'   \item{Bcf}{Central field (magnetic fux density, \emph{B}) value in \code{B.unit}.}
#'   \item{Bsw}{Sweep width (magnetic fux density, \emph{B}, experimental range) value in \code{B.unit}.}
#'   \item{Npoints}{Number of points (spectral resolution).}
#'   \item{mwGHz}{Microwave frequency value in `GHz`.}
#'   }
#'
#'
#' @examples
#' \dontrun{
#' TODO
#' TODO
#' }
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
  B.CF <- params.df %>%
    dplyr::filter(.data$Parameter == "Central Field") %>%
    dplyr::pull(dplyr::all_of(c("Value"))) %>% convert_B(B.unit = "mT",B.2unit = B.unit)
  B.CF <- round(B.CF,digits = 3)
  #
  B.SW <- params.df %>%
    dplyr::filter(.data$Parameter == "Sweep Width") %>%
    dplyr::pull(dplyr::all_of(c("Value"))) %>% convert_B(B.unit = "mT",B.2unit = B.unit)
  B.SW <- round(B.SW,digits = 3)
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
  named.params.list <- list(Bcf = B.CF,Bsw = B.SW,Npoints = Npoints,mwGHz = nu.GHz)
  return(named.params.list)
}
