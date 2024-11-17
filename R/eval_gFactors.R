#'
#' Basic Calculation of \eqn{g}-factor
#'
#'
#' @family Evaluations
#'
#'
#' @description Calculation of \eqn{g}-factor according to fundamental formula (see \code{Value}).
#'   The magnetic flux density (\code{B.val}) and microwave frequency (\code{nu.val},\eqn{\nu})
#'   can be defined, having the common units like \code{G} (Gauss) \code{mT}
#'   (millitesla) or \code{T} (tesla) as well as \code{GHz} or \code{Hz}.
#'
#'
#' @param nu.val Numeric, microwave Frequency value.
#' @param nu.unit Character string, frequency unit defined by \code{GHz} or \code{Hz},
#'   \strong{default}: \code{nu.unit = "GHz"}.
#' @param B.val Numeric, magnetic flux density value.
#' @param B.unit Character string, magnetic flux density unit in \code{G} or \code{mT} or \code{T},
#'   \strong{default}: \code{B.unit = "mT"}.
#'
#'
#' @return \eqn{g}-Value from \eqn{(\nu h)/(\mu_{\text{B}} B)}. Physical constants (\eqn{\mu_{\text{B}}}
#'   and \eqn{h}) are taken from \href{https://r-quantities.github.io/constants/}{constants}
#'   package by the \code{\link[constants]{syms}}.
#'
#'
#' @examples
#' eval_gFactor(9.8020458,
#'              nu.unit = "GHz",
#'              350.214,
#'              B.unit = "mT")
#' #
#' eval_gFactor(nu.val = 9.8020458e+9,
#'              nu.unit = "Hz",
#'              B.val = 3502.14,
#'              B.unit = "G")
#' #
#' eval_gFactor(9.5421,"GHz",0.333251,"T")
#'
#'
#' @export
#'
#'
eval_gFactor <- function(nu.val,
                         nu.unit = "GHz",
                         B.val,
                         B.unit = "mT") {
  #
  ## Fundamental Physical Constants from `{constants}` pkg.
  Planck.const <- constants::syms$h
  Bohr.magnet <- constants::syms$mub
  #
  ## Conditions to calculate `g`
  if (nu.unit == "GHz" & B.unit == "mT") {
    g <- (Planck.const * nu.val * 1e+9) / (Bohr.magnet * B.val * 0.001)
  } else if (nu.unit == "Hz" & B.unit == "mT") {
    g <- (Planck.const * nu.val) / (Bohr.magnet * B.val * 0.001)
  } else if (nu.unit == "Hz" & B.unit == "G") {
    g <- (Planck.const * nu.val) / (Bohr.magnet * B.val * 0.0001)
  } else if (nu.unit == "GHz" & B.unit == "G") {
    g <- (Planck.const * nu.val * 1e+9) / (Bohr.magnet * B.val * 0.0001)
  } else if (nu.unit == "GHz" & B.unit == "T") {
    g <- (Planck.const * nu.val * 1e+9) / (Bohr.magnet * B.val)
  } else if (nu.unit == "Hz" & B.unit == "T") {
    g <- (Planck.const * nu.val) / (Bohr.magnet * B.val)
  }
  #
  return(round(g, digits = 5))
  #
}
#
#
#
#
#' Calculation of \eqn{g}-factor ("Position") from EPR Spectrum/Data
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   Calculation of g-value according to fundamental formula, see \code{\link{eval_gFactor}}.
#'   \eqn{g}-related magnetic flux density (like \eqn{B_{\text{iso}}} or \eqn{B_{\text{center}}})
#'   is directly taken from the EPR spectrum. If positive and negative derivative intensities of the spectral
#'   line are similar and their distance from the middle point of the spectrum equals,
#'   the \eqn{B_{\text{iso}}} should be considered. Otherwise, the \eqn{B_{\text{center}}} must be taken
#'   into account. In case of integrated EPR spectrum/data, the \eqn{B_{\text{max}}} is used for
#'   the \eqn{g}-value calculation.
#'
#'
#' @param data.spectr Spectrum data frame object where the magnetic flux density (in \code{mT} or \code{G}
#'   or \code{T}) column can be labeled as \code{Field} or \code{B_G} and that of the derivative intensity
#'   as \code{dIepr_over_dB} or single integrated intensity like \code{Integrated_Intensity}
#'   (\code{index} column might be included as well).
#' @param nu.GHz Numeric value, microwave frequency in \code{GHz}.
#' @param B.unit Character string, denoting the magnetic flux density unit e.g. \code{B.unit = "G"}
#'   (gauss, \strong{default}) or \code{B.unit = "mT"}/\code{"T"} (millitesla/tesla).
#' @param B Character string, pointing to magnetic flux density \code{column} of the EPR spectrum data frame
#'   \code{data.spectr} either in "millitesla"/"tesla" or in "gauss", that is \code{B = "B_mT"} (\strong{default})
#'   or \code{B = "B_G"}/\code{B = "T"} or \code{B = "Bsim_G"} to include simulated EPR spectra as well.
#' @param Intensity Character string, pointing to \code{intensity column} if other than \code{dIepr_over_dB}
#'   name/label is used (e.g. for simulated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param lineSpecs.form Character string, describing either \code{"derivative"} (\strong{default}) or
#'   \code{"integrated"} (i.e. \code{"absorption"} which can be used as well) line form of the analyzed
#'   EPR spectrum/data.
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G}/\code{T}
#'   corresponding to lower and upper limit of the selected \eqn{B}-region,
#'   such as \code{Blim = c(3495.4,3595.4)}. \strong{Default}: \code{Blim = NULL} (corresponding to the entire
#'   \eqn{B}-range of the EPR spectrum).
#' @param iso Logical, whether to calculate the \eqn{g}-factor from the \eqn{B}-value corresponding to
#'   that between the \code{min.} and \code{max.} derivative intensities (\code{dIepr_over_dB},
#'   that is \eqn{g_{\text{iso}}} (this is the \strong{default} one: \code{iso = TRUE}), or by finding
#'   the \eqn{B}-value corresponding to \code{dIepr_over_dB = 0} (close to zero, which is \code{iso = FALSE}).
#'   For the \code{lineSpecs.form = "integrated"} (or \code{absorptiion}), the \code{iso} is related to magnetic
#'   flux density with \code{max.} intensity.
#'
#'
#' @return Numeric \eqn{g_{\text{iso}}}-value ('iso' = 'isotropic') or \eqn{g_{\text{center}}},
#'   from the EPR spectrum, according to \eqn{(h\,\nu)/(\mu_{\text{B}}\,B)}.
#'
#'
#' @examples
#' ## load package built-in EPR spectral data example:
#' data.file.path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.asc")
#' data.epr <-
#'   readEPR_Exp_Specs(path_to_ASC = data.file.path,
#'                     col.names = c("B_G",
#'                                   "dIepr_over_dB"),
#'                     qValue = 3500,
#'                     origin = "winepr")
#' #
#' ## g_iso calculation from EPR spectrum/data:
#' eval_gFactor_Spec(data.spectr = data.epr,
#'                   nu.GHz = 9.814155,
#'                   B.unit = "mT",
#'                   B = "B_mT",
#'                   Blim = c(349.677, 350.457))
#'
#'
#' @export
#'
#'
#' @importFrom dplyr filter select mutate pull between near
eval_gFactor_Spec <- function(data.spectr,
                              nu.GHz,
                              B.unit = "G",
                              B = "B_G",
                              Intensity = "dIepr_over_dB",
                              lineSpecs.form = "derivative",
                              Blim = NULL,
                              iso = TRUE) {
  ## 'Temporary' processing variables
  AbsIntens <- NULL
  . <- NULL
  #
  ## Field definition also for other functions
  # if (grepl("_|-",B,ignore.case = TRUE)){
  #   B.split <- unlist(stringr::str_split(B,pattern = "_|-"))
  # }
  #
  ## Define limits if `Blim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.B.region <- c(min(data.spectr[[B]]), max(data.spectr[[B]]))
  Blim <- Blim %>% `if`(is.null(Blim), data.B.region, .)
  #
  ## B minimum & maximum
  B.min <- data.spectr %>%
    dplyr::filter(dplyr::between(.data[[B]], Blim[1], Blim[2])) %>%
    dplyr::filter(.data[[Intensity]] == min(.data[[Intensity]])) %>%
    dplyr::pull(.data[[B]])
  #
  B.max <- data.spectr %>%
    dplyr::filter(dplyr::between(.data[[B]], Blim[1], Blim[2])) %>%
    dplyr::filter(.data[[Intensity]] == max(.data[[Intensity]])) %>%
    dplyr::pull(.data[[B]])
  ## B between minimum and maximum of dIepr_over_dB:
  if (isTRUE(iso)) {
    if (grepl("deriv|Deriv",lineSpecs.form)) {
      B.center <- (B.min + B.max) / 2
      ## B at dIepr_over_dB = 0 (near 0, see next comment on `B.center`):
    }
    if (grepl("integ|Integ|absorpt|Absorpt",lineSpecs.form)) {
      B.center <- B.max
    }
  } else {
    if (grepl("deriv|Deriv",lineSpecs.form)) {
      ## Find the value B, corresponding to Intensity very close to 0 (tolerance max(Intensity)/2)
      B.center <- data.spectr %>%
        dplyr::filter(dplyr::between(.data[[B]], B.max, B.min)) %>%
        dplyr::mutate(AbsIntens = abs(.data[[Intensity]])) %>%
        dplyr::filter(dplyr::near(AbsIntens, 0, tol = max(.data[[Intensity]]) / 2)) %>%
        dplyr::filter(AbsIntens == min(AbsIntens)) %>%
        dplyr::pull(.data[[B]])
    }
    if (grepl("integ|Integ|absorpt|Absorpt",lineSpecs.form)) {
      B.center <- B.max
    }
  }
  ## g -value calculation:
  Planck.const <- constants::syms$h
  Bohr.magnet <- constants::syms$mub
  g.precurs <- (Planck.const * nu.GHz * 1e+9) / (Bohr.magnet * B.center)
  #
  ## Conditions for B column, the name should contain ("B", "mT" or "G" or "T"):
  if (B.unit == "mT") {
    g <- g.precurs / 1e-3
  }
  if (B.unit == "G") {
    g <- g.precurs / 1e-4
  }
  if (B.unit == "T") {
    g <- g.precurs
  }
  #
  return(round(g, digits = 5))
  #
}
#
#
#
#
#' Calculation of \eqn{g}-factor from Quantum Chemical Computational Output
#'
#'
#' @family Evaluations and Quantum Chemistry
#'
#'
#' @description
#'   In the Gaussian and ORCA outputs, the \eqn{g}-value (its 3 principal components) is presented in the form
#'   of differences from the \eqn{g_e} (\eqn{g} of the free electron). Therefore, the function takes these values
#'   to calculate the entire \eqn{g}-factor components or parses the corresponding \eqn{g}-mean value
#'   from the outputs.
#'
#'
#' @param path_to_QCHoutput Character string, corresponding to path of "Gaussian" or "ORCA" output text files
#'   including all \eqn{g}-factors. Alternatively, the \code{\link[base]{file.path}} can be applied to get
#'   the full/relative path of that file.
#' @param mean Logical, whether to calculate the \code{mean value/iso} from the principal components,
#'   \strong{default}: \code{mean = TRUE}, or return the entire vector with the all 3 components.
#' @param origin Character string, pointing to origin of the EPR calculation parameters <=> which
#'   software package was used. Only two values are available => \code{"Gaussian"} (\strong{default})
#'   or \code{"ORCA"}.
#'
#'
#' @return Numeric mean \eqn{g}-factor value from the principal difference (from \eqn{g_e}) components
#'   calculated by the QCH method (e.g. by DFT) or numeric vector with the principal \eqn{g}-components
#'   if \code{mean = FALSE}.
#'
#'
#' @examples
#' ## built-in package file example and path:
#' gauss.file.path <-
#'   load_data_example(file = "TMPDAradCatEPRa.inp.log.zip")
#' gauss.file <- unzip(gauss.file.path)
#' ## g_iso-value calculation from Gaussian output file:
#' eval_gFactor_QCHcomp(gauss.file)
#'
#'
#' @export
#'
#'
eval_gFactor_QCHcomp <- function(path_to_QCHoutput,
                                 mean = TRUE,
                                 origin = "gaussian") {
  #
  ## reading the output files from GAUSSIAN or ORCA
  qchfile <- readLines(path_to_QCHoutput)
  #
  ## g-Value indicator based on `origin`
  if (any(grepl("Gauss|GAUSS|gauss",origin))) {
    gval.indicator <- "g shifts relative"
    start.read.line <- grep(gval.indicator, qchfile)
    qchfile.select.g <- qchfile[start.read.line + 1]
    #
    ## character line split into string elements
    qchfile.select.g <- stringr::str_split(qchfile.select.g,
                                           pattern = "[[:space:]]+"
    )
    #
    ## select only number strings
    ## `str_split` results in list therefore it has to be unlisted
    qchfile.select.g <- unlist(qchfile.select.g)
    #
    vector.string.dg <- c(
      qchfile.select.g[3],
      qchfile.select.g[5],
      qchfile.select.g[7]
    )
    #
    #
    ## numeric values (in `Gaussian` they are already presented in ppm)
    vector.dg <- as.numeric(as.character(vector.string.dg))
  }
  if (any(grepl("ORCA|orca|Orca",origin))) {
    gval.indicator <- "Delta-g"
    start.read.line <- grep(gval.indicator, qchfile)
    qchfile.select.g <- qchfile[start.read.line]
    #
    ## character line split into string elements
    qchfile.select.g <- stringr::str_split(qchfile.select.g,
                                           pattern = "[[:space:]]+"
    )
    #
    ## select only number strings
    qchfile.select.g <- unlist(qchfile.select.g) ## `str_split` results in list
    #
    vector.string.dg <- c(
      qchfile.select.g[3],
      qchfile.select.g[4],
      qchfile.select.g[5]
    )
    #
    ## numeric values (in `ORCA` they are in form 1e-6)
    vector.dg <- as.numeric(as.character(vector.string.dg)) * 1e6
  }
  #
  ## Delete `qchfile` => it is not required anymore
  rm(qchfile)
  #
  ## g-factor for free electron (g.e) from `{constants}` package
  ## round the g.e to 6 decimal places, the negative sign must
  ## cancel that coming from `constants::syms$gem`, which is negative
  g.e <- round(-constants::syms$gem, digits = 6)
  #
  ## g-vector from shifts (`deltas` or `d`) and g.e
  delta_g_vec <- vector.dg
  g_vec <- g.e + delta_g_vec * 1e-6
  #
  ## whether to calculate the mean value or get the g-vector
  ## as it is
  if (isTRUE(mean)) {
    gValue <- mean(g_vec)
  } else {
    gValue <- g_vec
  }
  #
  return(round(gValue,digits = 5))
  #
}
