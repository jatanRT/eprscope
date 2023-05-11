#'
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
#'   \strong{default}: \code{mean = TRUE}, or save the entire vector with all the components
#' @param origin Character string pointing to origin of DFT EPR calculation parameters <=> which
#'   software package was used. Only two values are available => \code{"gaussian"} (\strong{default})
#'   or \code{"orca"}
#'
#'
#' @return Numeric mean \eqn{g}-factor value from principal difference (from \eqn{g_e}) components
#'   calculated by QCH methods (e.g. by DFT) or numeric vector with principal \eqn{g}-components
#'   if \code{mean = FALSE}
#'
#'
#' @examples
#' \dontrun{
#' eval_gFactor_QCHcomp("./InputData/DFTcomputs/RadicalDFToutput.inp.log")
#' eval_gFactor_QCHcomp(file.path(".","DFT_calculations","EPRorcaRadical.out"),
#'                     mean = FALSE,
#'                     origin = "orca")
#' }
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
  return(gValue)
  #
}
