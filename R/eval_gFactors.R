#'
#' Basic Calculation of \eqn{g}-factor
#'
#'
#' @family Evaluations
#'
#'
#' @description Calculation of {g}-factor according to fundamental formula.
#'   The magnetic flux density (\code{B}) and microwave frequency (\code{nu},\eqn{\nu})
#'   can be entered with common units like \code{G} (Gauss) \code{mT}
#'   (millitesla) or \code{T} (tesla) as well as \code{GHz} or \code{Hz}, respectively.
#'   The Planck constant (\eqn{h}) and Bohr magneton (\eqn{\mu_{B}}) are included
#'   in \code{\link[constants]{syms}} function and their values are taken by \code{syms$h}
#'   and \code{syms$mub} commands, respectively.
#'
#'
#' @param nu Numeric, microwave Frequency
#' @param nu.unit Character string, frequency unit defined by \code{"GHz"} or \code{"Hz"}, \strong{default}: \code{nu.unit = "GHz"}
#' @param B Numeric, magnetic flux density
#' @param B.unit Character string, magnetic flux density unit in \code{"G"} or \code{"mT"} or \code{"T"}, \strong{default}:
#'   \code{B.unit = "mT"}
#'
#'
#' @return g-value from \eqn{(\nu h)/(\mu_{B} B)}. For variables and constants =>
#'   see description above
#'
#'
#' @examples
#' eval_gFactor(9.8020458,
#'              nu.unit = "GHz",
#'              350.214,
#'              B.unit = "mT")
#' #
#' eval_gFactor(nu = 9.8020458e+9,
#'              nu.unit = "Hz",
#'              B = 3502.14,
#'              B.unit = "G")
#' #
#' eval_gFactor(9.5421,"GHz",0.333251,"T")
#'
#'
#' @export
#'
#'
eval_gFactor <- function(nu,
                         nu.unit = "GHz",
                         B,
                         B.unit = "mT") {
  #
  ## Fundamental Physical Constants
  Planck.const <- constants::syms$h
  Bohr.magnet <- constants::syms$mub
  #
  ## Conditions to calculate `g`
  if (nu.unit == "GHz" & B.unit == "mT") {
    g <- (Planck.const * nu * 1e+9) / (Bohr.magnet * B * 0.001)
  } else if (nu.unit == "Hz" & B.unit == "mT") {
    g <- (Planck.const * nu) / (Bohr.magnet * B * 0.001)
  } else if (nu.unit == "Hz" & B.unit == "G") {
    g <- (Planck.const * nu) / (Bohr.magnet * B * 0.0001)
  } else if (nu.unit == "GHz" & B.unit == "G") {
    g <- (Planck.const * nu * 1e+9) / (Bohr.magnet * B * 0.0001)
  } else if (nu.unit == "GHz" & B.unit == "T") {
    g <- (Planck.const * nu * 1e+9) / (Bohr.magnet * B)
  } else if (nu.unit == "Hz" & B.unit == "T") {
    g <- (Planck.const * nu) / (Bohr.magnet * B)
  }
  #
  return(round(g, digits = 5))
  #
}
#
#
#
#
#' Calculation of \eqn{g}-factor ("Position") from the EPR Spectrum/Data
#'
#'
#' @family Evaluations
#'
#'
#' @description Calculation of g-value according to fundamental formula (\code{\link{eval_gFactor}}).
#'   \eqn{g}-related magnetic flux density (like \eqn{B_{iso}} or \eqn{B_{center}}) is directly taken
#'   from the EPR spectrum. If positive and negative derivative intensities of the spectral line are similar
#'   and their distance from the middle of the spectrum equals, the \eqn{B_{iso}} should be be considered,
#'   otherwise the \eqn{B_{center}} must be taken into account. In case of integrated EPR spectrum/data
#'   the \eqn{B_{max}} is used for the \eqn{g}-value evaluation.
#'
#'
#' @param data.spectrum Spectrum data frame/table where the magnetic flux density (in \code{mT} or \code{G}) column
#'   can be labeled as \code{Field} or \code{B_G} and that of the derivative intensity as \code{dIepr_over_dB}
#'   or single integrated intensity like \code{Integrated_Intensity}, \code{index} column can be included as well.
#' @param nu.GHz Numeric, microwave frequency in \code{GHz}
#' @param B.unit description tbc
#' @param B Character string pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{data.spectrum} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} (\strong{default})
#'   or \code{B = "B_G"} or \code{B = "B_G_Sim"} to include simulated EPR spectra as well
#' @param Intensity Character string pointing to \code{intensity column} if other than \code{dIepr_over_dB}
#'   name/label is used (e.g. for simulated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param lineSpecs.form Character string describing either \code{"derivative"} (\strong{default}) or \code{"integrated"}
#'   (i.e. \code{"absorption"} which can be used as well) line form of the analyzed EPR spectrum/data.
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `Blim = c(3495.4,3595.4)`. \strong{Default}: \code{Blim = NULL} (corresponding
#'   to entire `B` range).
#' @param iso Logical, whether to calculate the \eqn{g}-factor from the \eqn{B} value corresponding to
#'   that between the \code{min.} and \code{max.} derivative intensities (\code{dIepr_over_dB}, that is \eqn{g_{iso}}
#'   (this is the \strong{default}: \code{iso = TRUE}), or by finding the the \eqn{B} value corresponding
#'   to \code{dIepr_over_dB = 0} (close/near zero, which is \code{iso = FALSE})
#'
#'
#' @return Numeric \eqn{g_{iso}}-value ('iso' = 'isotropic') according to \eqn{(\nu h)/(\mu_{B} B)}
#'
#'
#' @examples
#' ## load built-in EPR spectral data
#' data.file.path <- load_data_example(file = "TMPD_specelchem_accu_b.asc")
#' data.epr <- readEPR_Exp_Specs(path_to_ASC = data.file.path,
#'                                 col.names = c("B_G", "dIepr_over_dB"),
#'                                 x = 1,
#'                                 Intensity = 2,
#'                                 qValue = 3500,
#'                                 origin = "winepr")
#' ## g_iso calculation from EPR spectrum/data =>
#' eval_gFactor_Spec(data.spectrum = data.epr,
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
eval_gFactor_Spec <- function(data.spectrum,
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
  ## Define limits if `Blim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.B.region <- c(min(data.spectrum[[B]]), max(data.spectrum[[B]]))
  Blim <- Blim %>% `if`(is.null(Blim), data.B.region, .)
  #
  ## B minimum & maximum
  B.min <- data.spectrum %>%
    dplyr::filter(dplyr::between(.data[[B]], Blim[1], Blim[2])) %>%
    dplyr::filter(.data[[Intensity]] == min(.data[[Intensity]])) %>%
    dplyr::pull(.data[[B]])
  #
  B.max <- data.spectrum %>%
    dplyr::filter(dplyr::between(.data[[B]], Blim[1], Blim[2])) %>%
    dplyr::filter(.data[[Intensity]] == max(.data[[Intensity]])) %>%
    dplyr::pull(.data[[B]])
  ## B between minimum and maximum of dIepr_over_dB:
  if (isTRUE(iso)) {
    if (lineSpecs.form == "derivative") {
      B.center <- (B.min + B.max) / 2
      ## B at dIepr_over_dB = 0 (near 0, see next comment on `B.center`):
    }
    if (lineSpecs.form == "integrated" || lineSpecs.form == "absorption") {
      B.center <- B.max
    }
  } else {
    if (lineSpecs.form == "derivative") {
      ## Find the value B, corresponding to Intensity very close to 0 (tolerance max(Intensity)/100)
      B.center <- data.spectrum %>%
        dplyr::filter(dplyr::between(.data[[B]], B.max, B.min)) %>%
        dplyr::mutate(AbsIntens = abs(.data[[Intensity]])) %>%
        dplyr::filter(dplyr::near(AbsIntens, 0, tol = max(.data[[Intensity]]) / 2)) %>%
        dplyr::filter(AbsIntens == min(AbsIntens)) %>%
        dplyr::pull(.data[[B]])
    }
    if (lineSpecs.form == "integrated" || lineSpecs.form == "absorption") {
      B.center <- B.max
    }
  }
  ## g -value calculation:
  Planck.const <- constants::syms$h
  Bohr.magnet <- constants::syms$mub
  g.precurs <- (Planck.const * nu.GHz * 1e+9) / (Bohr.magnet * B.center)
  #
  ## Conditions for B column, the name should contain ("B", "mT" or "G"):
  if (B.unit == "mT") {
    g <- g.precurs / 1e-3
  }
  if (B.unit == "G") {
    g <- g.precurs / 1e-4
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
#' @description \eqn{g}-Values (3 principal ones) are presented in a form of differences from the \eqn{g_e}.
#' Therefore the function takes these values and calculates the entire \eqn{g}-values or parses
#' the corresponding mean value from `Gaussian` or `ORCA` output.
#'
#'
#' @param path_to_QCHoutput Character string corresponding to path of `Gaussian` or `ORCA` output text files
#'   incl. all \eqn{g}-values. \code{\link[base]{file.path}} can be applied to get the full/relative path.
#' @param mean Logical, whether to calculate the \code{mean value/iso} from principal components,
#'   \strong{default}: \code{mean = TRUE}, or save the entire vector with all the components.
#' @param origin Character string pointing to origin of DFT EPR calculation parameters <=> which
#'   software package was used. Only two values are available => \code{"gaussian"} (\strong{default})
#'   or \code{"orca"}.
#'
#'
#' @return Numeric mean \eqn{g}-factor value from principal difference (from \eqn{g_e}) components
#'   calculated by QCH methods (e.g. by DFT) or numeric vector with principal \eqn{g}-components
#'   if \code{mean = FALSE}
#'
#'
#' @examples
#' ## built-in file and path
#' gauss.file.path <- load_data_example(file = "TMPDAradCatEPRa.inp.log.zip")
#' gauss.file <- unzip(gauss.file.path)
#' ## g_iso-value calculation from Gaussian output file
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
  if (origin == "gaussian") {
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
  if (origin == "orca") {
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
  ## g-factor for free electron (g.e) from `constants` package
  ## round the g.e to 6 decimal places
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
