#' Solvent Properties Data Frame (Database) for EPR/ENDOR
#'
#' Data frame summarizing the ...tbc
#'
#' @format A data frame with 46 rows and 9 variables/columns:
#' \describe{
#'   \item{No_Proton}{Numeric, proton number.}
#'   \item{Isotope}{Character string ponting to isotope in format like "14N".}
#'   \item{Stability}{Character string pointing either to stable, "STB", or radio-active, "RA", isotope.}
#'   \item{Name}{Character string corresponding to isotope name.}
#'   \item{Spin}{Numeric, denoting the spin quantum number.}
#'   \item{g_Nuclear}{Numeric, corresponding to nuclear \eqn{g}-factor (\eqn{g_{\text{n}}^{}}).}
#'   \item{Abund_Natur_Percent}{Numeric, pointing to natural abundance of an isotope in \eqn{\%}.}
#'   \item{Q_Barn}{Numeric, corresponding to nuclear quadrupolar moment in \eqn{10^{-28}\,\text{m}^2}.}
#'   \item{nu_ENDOR_MHz_035T}{Numeric, specific Larmor/ENDOR frequency (\eqn{\nu_{\text{ENDOR}}^{}})
#'         at \eqn{0.35\,\text{T}}.}
#' }
#' @source \url{https://easyspin.org/easyspin/documentation/isotopetable.html}
"solvent_db"
