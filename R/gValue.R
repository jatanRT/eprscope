#
#' @title Basic Calculation of g-Factor
#'
#' @description Calculation of g-factor from the basic formula (see below).
#'   The magnetic flux density (\code{B}) and frequency (\code{nu}) can be entered
#'   with common units like \code{G} (Gauss) \code{mT} (millitesla) or \code{T} (tesla)
#'   as well as \code{GHz} or \code{Hz}, respectively.The Planck constant
#'   and Bohr magneton are included in \code{\link{constants}} package
#'   and their values are taken by \code{syms$h} and \code{syms$muB} commands,
#'   respectively.
#'
#'
#' @param nu Frequency (number)
#' @param unit_nu Frequency unit defined by "GHz" or "Hz"
#' @param B Magnetic flux density (number)
#' @param unit_B Magnetic flux density unit in "G" or "mT" or "T"
#'
#' @return g-value from \eqn{(\nu * h)/(\mu_B * B)}
#'
#' @examples
#' gValue(9.8020458,unit_nu = "GHz",350.214,unit_B = "mT")
#' gValue(9.8020458e+9,unit_nu = "Hz",3502.14,unit_B = "G")
#'
#'
#' @export
gValue <- function(nu,unit_nu = "GHz",B,unit_B = "mT"){
  Planck.const <- constants::syms$h
  Bohr.magnet <- constants::syms$muB
  if (unit_nu == "GHz" & unit_B == "mT"){
    g <- (Planck.const*nu*1e+9)/(Bohr.magnet*B*0.001)
  } else if (unit_nu == "Hz" & unit_B == "mT"){
    g <- (Planck.const*nu)/(Bohr.magnet*B*0.001)
  } else if (unit_nu == "Hz" & unit_B == "G"){
    g <- (Planck.const*nu)/(Bohr.magnet*B*0.0001)
  } else if (unit_nu == "GHz" & unit_B == "G"){
    g <- (Planck.const*nu*1e+9)/(Bohr.magnet*B*0.0001)
  } else if (unit_nu == "GHz" & unit_B == "T"){
    g <- (Planck.const*nu*1e+9)/(Bohr.magnet*B)
  } else if (unit_nu == "Hz" & unit_B == "T"){
    g <- (Planck.const*nu)/(Bohr.magnet*B)
  }
  return(round(g,digits = 5))
}
