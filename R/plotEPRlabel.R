#
#' @title
#'
#'
#' @description
#'
#'
#' @param quantity
#' @param unit
#' @param user.defined
#'
#'
#' @return
#'
#'
#' @examples
#'
#'
#' @export
#'
#'
#' @importFrom rlang enquo
plotEPRlabel <- function(quantity,unit,user.defined = F){
  ## in 'bquote' bang-bang ('!!') operator does not work
  ## use '.()' instead
  quantity <- enquo(quantity)
  unit <- enquo(unit)
  if (isFALSE(user.defined)){
    l <- bquote(italic(.(quantity))~~"("~.(unit)~")")
  } else{
    l <- bquote(italic(.(quantity))~~.(unit))
  }
  return(l)
}
