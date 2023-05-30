#'
#' Reading Solvent Properties from `solvents_ds` Data Frame (Database)
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   A short description...tbc...
#'
#'
#'
#' @param solvent Character string pointing to solvent name (or any string from solvent name/abbreviation),
#'   e.g. like \code{solvent = "DMSO"},\code{solvent = "acetone"}, \code{solvent = "xylene"}.
#'   If more than one rows/observations are being returned (e.g. in case of \code{solvent = "xylene"}) => additional
#'   solvent specification must be provided like e.g. \code{solvent = "o-xylene"}.
#' @param prop Character string...tbc...
#'
#'
#' @return description...tbc...
#'
#'
#' @examples
#' ## Properties of `DMSO`
#' print(tibble::as_tibble(readEPR_solvent_props("DMSO")),
#'       width = 80,n = 40)
#' #
#' ## All `xylene` solvent specifications
#' print(tibble::as_tibble(readEPR_solvent_props(solvent = "xylene")),
#'       width = 80,n = 40)
#' #
#' ## Boiling point of `o-xylene`
#' readEPR_solvent_props(solvent = "o-xylene",
#'                       prop = "boiling")
#'
#'
#'
#' @export
#'
#'
#' @importFrom dplyr contains
readEPR_solvent_props <- function(solvent,prop = NULL){
  #
  ## Select only specific `solvent` row/observation
  solvent_slct <- eprscope::solvents_ds %>%
    dplyr::filter(grepl(solvent,.data$Solvent))
  #
  if (is.null(prop)){
    #
    ## one-row data frame
    olvent_slct <- solvent_slct
    #
  } else {
    #
    if (nrow(solvent_slct) == 1){
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
  if (nrow(solvent_slct) > 1 & is.null(prop)){
    print(" Please, specify the solvent explicitly ! ")
  }
  #
}
