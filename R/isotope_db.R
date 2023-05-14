#' Nuclear Isotope Data Frame (Database) with ENDOR Frequencies
#'
#' Data frame summarizing the essential characteristics of nuclei in EPR spectroscopy. This database
#' was taken form \href{https://easyspin.org/easyspin/documentation/isotopetable.html}{`EasySpin` toolbox}
#' and only it's format was slightly modified. Therefore, IT CONTAINS THE ENTIRE INFORMATION
#' LIKE THE ORIGINAL DATABASE (see the SOURCE BELOW). For better orientation in ENDOR spectra,
#' column with the Larmor/ENDOR frequencies (in MHz) at 0.35 T was added according to =>
#' \deqn{\nu_{\text{ENDOR}}^{} = - \frac{1}{h}\,\mu_{\text{N}}^{}\,g_{\text{n}}^{}\,B}
#' where \eqn{h} is the Planck's constant, \eqn{\mu_{\text{N}}^{}} is the nuclear magneton
#' available from \pkg{constants} package (\code{constants::syms$mun}), \eqn{g_{\text{n}}^{}},
#' is the nuclear \eqn{g}-factor of the specific nucleus (reported in the data frame as \code{g_Nuclear})
#' and finally, the \eqn{B = 0.35\,\text{T}} denotes the magnetic flux density.
#'
#' @format A data frame with 351 rows and 9 variables/columns:
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
"isotope_db"
