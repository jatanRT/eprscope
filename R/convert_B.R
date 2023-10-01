#' Conversion of Magnetic Flux Density According to Input-Output Units
#'
#'
#' @family Conversions and Corrections
#'
#'
#' @description
#' Conversion of magnetic flux density/field (\eqn{B}) values depending on the input and the required
#' output units.
#'
#'
#'
#' @param B Numeric value/vector corresponding to input value(s) of magnetic flux density.
#' @param B.unit Character string referring to input magnetic flux density units.
#'   Usually \code{"T"}, \code{"mT"} or \code{"G"} are used.
#' @param B.2unit Character string referring to input magnetic flux density units.
#'   Usually \code{"T"}, \code{"mT"} or \code{"G"} are used.
#'
#'
#' @return Numeric value or vector as a result of the B conversion.
#'
#'
#' @examples
#' ## simple conversion
#' convert_B(3500,B.unit = "G",B.2unit = "T")
#' #
#' ## conversion of B vector with Sweep Width = 100 G
#' ## and central filed 3496 G
#' B <- seq(3496-100/2,3496+100/2,length.out = 1024)
#' Bnew <- convert_B(B,B.unit = "G",B.2unit = "mT")
#' head(as.matrix(Bnew),n = 20)
#'
#'
#' @export
#'
#'
convert_B <- function(B,
                      B.unit,
                      B.2unit){
  if (B.unit == "G" & B.2unit == "T"){
    B <- B * 1e-4
  }
  if (B.unit == "mT" & B.2unit == "T"){
    B <- B * 1e-3
  }
  if (B.unit == "G" & B.2unit == "mT"){
    B <- B * 0.1
  }
  if (B.unit == "mT" & B.2unit == "G"){
    B <- B * 10
  }
  if (B.unit == "T" & B.2unit == "mT"){
    B <- B * 1e+3
  }
  if (B.unit == "T" & B.2unit == "G"){
    B <- B * 1e+4
  }
  if ((B.unit == "G" & B.2unit == "G") ||
      (B.unit == "mT" & B.2unit == "mT") ||
      (B.unit == "T" & B.2unit == "T")){
    B <- B
  }
  #
  return(B)
}

