#' Conversion of Magnetic Flux Density
#'
#'
#' @family Conversions and Corrections
#'
#'
#' @description
#'   Conversion of magnetic flux density/field (\emph{B}) values depending on the input and the required
#'   output units.
#'
#'
#'
#' @param B.val Numeric value/vector corresponding to input value(s) of magnetic flux density.
#' @param B.unit Character string referring to input magnetic flux density units.
#'   Usually `T`, `mT` or `G` are used.
#' @param B.2unit Character string referring to output magnetic flux density units.
#'   Usually `T`, `mT` or `G` are used.
#'
#'
#' @return Numeric value or vector as a result of the B conversion.
#'
#'
#' @examples
#' ## simple conversion
#' convert_B(B.val = 3500,B.unit = "G",B.2unit = "T")
#' #
#' ## conversion of B.seq vector with Sweep Width = 100 G
#' ## and central filed 3496 G
#' B.seq <- seq(3496-100/2,3496+100/2,length.out = 1024)
#' Bnew <- convert_B(B.seq,B.unit = "G",B.2unit = "mT")
#' head(as.matrix(Bnew),n = 20)
#'
#'
#' @export
#'
#'
convert_B <- function(B.val,
                      B.unit,
                      B.2unit){
  if (B.unit == "G" & B.2unit == "T"){
    B.val <- B.val * 1e-4
  }
  if (B.unit == "mT" & B.2unit == "T"){
    B.val <- B.val * 1e-3
  }
  if (B.unit == "G" & B.2unit == "mT"){
    B.val <- B.val * 0.1
  }
  if (B.unit == "mT" & B.2unit == "G"){
    B.val <- B.val * 10
  }
  if (B.unit == "T" & B.2unit == "mT"){
    B.val <- B.val * 1e+3
  }
  if (B.unit == "T" & B.2unit == "G"){
    B.val <- B.val * 1e+4
  }
  if ((B.unit == "G" & B.2unit == "G") ||
      (B.unit == "mT" & B.2unit == "mT") ||
      (B.unit == "T" & B.2unit == "T")){
    B.val <- B.val
  }
  #
  return(B.val)
}

