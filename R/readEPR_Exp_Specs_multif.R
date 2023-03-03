#
#' Load Several/Multiple EPR Spectral Data Files into one List/Database.
#'
#'
#' @description Function the load EPR spectra from several/multiple files. tbc
#'
#'
#' @param pattern tbc
#' @param path_to_ASC tbc
#' @param path_to_DSC_or_par tbc
#' @param origin tbc
#' @param qValues tbc
#' @param names tbc
#'
#'
#' @return List of Data Frames Corresponding to Multiple Spectral Data Files
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#'
#' @export
#'
#'
readEPR_Exp_Specs_multif <- function(pattern,
                                     path_to_ASC,
                                     path_to_DSC_or_par,
                                     origin = "xenon",
                                     qValues = NULL,
                                     names){
  #
  ## file name pattern which has to be the same for `ASC`+`DSC`
  ## or `.spc` and `.par`
  file.name.pattern <- pattern
  #
  ## path to all `asc` files
  files.asc <- list.files(path = path_to_ASC,
                          pattern = file.name.pattern,
                          full.names = TRUE)
  #
  ## path to all `raw` (`.DSC`+`.DTA` or `.par`+`.spc`) files
  files.raw <- list.files(path = path_to_DSC_or_par,
                          pattern = file.name.pattern,
                          full.names = TRUE)
  #
  if (origin == "xenon"){
    ## select only `.DSC` files
    files.params <- grep(pattern = ".DSC",
                         files.raw,
                         value = TRUE)
    ## to obtain `QValues` (from all `.DSC` files) run the following
    qValues.from.files <- sapply(files.params,
                                 function(x) readEPR_param_slct(x,string = "QValue"))
    ## to obtain microwave frequencies `MWFQ` (from all `.DSC` files),
    ## required for g value calculations
    mwfq.string <- "MWFQ"

  }
  if (origin == "winepr"){
    ## select only `.par` files
    files.params <- grep(pattern = ".par",
                         files.raw,
                         value = TRUE)
    ## to obtain `QValues` run the following
    if (is.null(qValues)){
      cat(" 'qValues' vector is not provided. Please, define! ")
    } else{
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
  mwfreq.from.files <- sapply(files.params,
                              function(y) readEPR_param_slct(y,
                                                             string = mwfq.string,
                                                             origin = origin))

  if (origin == "winepr"){
    ## conversion from "GHz" to "Hz"
    mwfreq.from.files <- mwfreq.from.files*1e9
  }
  #
  ## the entire database of all spectra with intensity correction
  ## to `qValue` + new column with `g`-factors
  spectra.datab.from.files <-
    Map(function(s,t,u) readEPR_Exp_Specs(s,qValue = t,origin = origin) %>%
          dplyr::mutate(g_Value = gValue(nu = u,nu_unit = "Hz",B = .data[["B_mT"]])),
        files.asc,
        qValues.from.files,
        mwfreq.from.files)
  #
  ## rename spectra according to desired parameter/quantity/...etc dependecy
  ## see params above
  names(spectra.datab.from.files) <- names
  #
  return(spectra.datab.from.files)
  #
}
