#
#' @title Basic Calculation of \eqn{g}-factor
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
#' @param nu_unit String, frequency unit defined by \code{"GHz"} or \code{"Hz"}, \strong{default}: \code{nu_unit = "GHz"}
#' @param B Numeric, magnetic flux density
#' @param B_unit String, magnetic flux density unit in \code{"G"} or \code{"mT"} or \code{"T"}, \strong{default}:
#'   \code{B_unit = "mT"}
#'
#' @return g-value from \eqn{(\nu h)/(\mu_{B} B)}. For variables and constants =>
#'   see description above
#'
#' @examples
#' gValue(9.8020458,nu_unit = "GHz",350.214,B_unit = "mT")
#' gValue(nu = 9.8020458e+9,nu_unit = "Hz",B = 3502.14,B_unit = "G")
#' gValue(9.5421,"GHz",0.333251,"T")
#'
#' @export
gValue <- function(nu,nu_unit = "GHz",B,B_unit = "mT"){
  Planck.const <- constants::syms$h
  Bohr.magnet <- constants::syms$mub
  if (nu_unit == "GHz" & B_unit == "mT"){
    g <- (Planck.const*nu*1e+9)/(Bohr.magnet*B*0.001)
  } else if (nu_unit == "Hz" & B_unit == "mT"){
    g <- (Planck.const*nu)/(Bohr.magnet*B*0.001)
  } else if (nu_unit == "Hz" & B_unit == "G"){
    g <- (Planck.const*nu)/(Bohr.magnet*B*0.0001)
  } else if (nu_unit == "GHz" & B_unit == "G"){
    g <- (Planck.const*nu*1e+9)/(Bohr.magnet*B*0.0001)
  } else if (nu_unit == "GHz" & B_unit == "T"){
    g <- (Planck.const*nu*1e+9)/(Bohr.magnet*B)
  } else if (nu_unit == "Hz" & B_unit == "T"){
    g <- (Planck.const*nu)/(Bohr.magnet*B)
  }
  return(round(g,digits = 5))
}
