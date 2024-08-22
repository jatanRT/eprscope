#'
#' Reading Solvent Properties from \code{solvents_ds} Data Frame (Database)
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Gathering the solvent properties from \code{\link{solvents_ds}} in order to pick-up
#'   specific \code{solvent} and its corresponding properties (\code{prop} argument).
#'   See also \code{vignette("datasets")}.
#'
#'
#' @param solvent Character string pointing to solvent name (or any string from the solvent name/abbreviation),
#'   such as \code{solvent = "DMSO"},\code{solvent = "acetone"}, \code{solvent = "xylene"}.
#'   If more than one rows/observations are being returned (e.g. in case of \code{solvent = "xylene"}) => additional
#'   solvent specification must be provided e.g. \code{solvent = "o-xylene"}.
#' @param prop Character string related to a data frame variable/column/property e.g. \code{"boiling"},
#'   \code{"formula"}, \code{"dens"}, \code{"dielectric"}...etc (see also \code{\link{solvents_ds}}).
#'
#'
#' @return Data frame with row/rows of selected solvent(s) and the corresponding properties. If a specific property
#'   is called like, e.g. \code{code = "melting"} (\eqn{\equiv} melting point in °C), the function returns
#'   \code{value/character}.
#'
#'
#' @examples
#' ## Properties of `DMSO`:
#' solvent_01 <- readEPR_solvent_props("DMSO")
#' head(solvent_01)
#' #
#' ## All `xylene` solvent specifications:
#' solvent_02 <- readEPR_solvent_props(solvent = "xylene")
#' head(solvent_02)
#' #
#' ## Boiling point of `o-xylene`:
#' readEPR_solvent_props(solvent = "o-xylene",
#'                       prop = "boiling")
#'
#'
#'
#' @export
#'
#'
#' @importFrom dplyr contains
readEPR_solvent_props <- function(solvent, prop = NULL) {
  #
  ## Select only specific `solvent` row/observation
  solvent_slct <- eprscope::solvents_ds %>%
    dplyr::filter(grepl(solvent, .data$Solvent))
  #
  if (is.null(prop)) {
    #
    ## one-row data frame
    solvent_slct <- solvent_slct
    #
  } else {
    #
    if (nrow(solvent_slct) == 1) {
      ## selected column/variable value
      solvent_slct <- solvent_slct %>%
        dplyr::pull(dplyr::contains(prop))
      ## or like
      # solvent_slct %>%
      #   dplyr::pull(.data[[grep(prop,names(eprscope::solvents_ds),
      #                           ignore.case = TRUE,
      #                           value = TRUE)]])
    } else {
      stop(" Please, specify the solvent explicitly ! ")
    }
    #
  }
  #
  return(solvent_slct)
  ## if number of rows > 1 solvent should
  ## be clearly specified
  if (nrow(solvent_slct) > 1 & is.null(prop)) {
    print(" Please, specify the solvent explicitly ! ")
  }
  #
}
