#' Solvent Properties Data Frame (Dataset) for EPR/ENDOR
#'
#'
#' Data frame summarizing the most important solvent properties for EPR/ENDOR which are required
#' for variable temperature (VT) experiments and EPR spectroelectrochemistry.
#'
#'
#' @details
#'  The main properties were collected from the \href{https://organicchemistrydata.org/solvents/}{Division of Organic Chemistry
#'  of the ACS}
#'  and the \href{https://www.sigmaaldrich.com/deepweb/assets/sigmaaldrich/marketing/global/documents/614/456/labbasics_pg144.pdf}{Sigma-Aldrich}.
#'  Additional resources (e.g. for viscosities) are \href{https://pubchem.ncbi.nlm.nih.gov/}{PubChem NCBI}
#'  and \href{https://www.accudynetest.com/visc_table.html}{ACCU DYNE TEST}. Besides that, the polarity of solvents
#'  (expressed by the relative permitivity \code{Dielectric_Const}) is important parameter to decide which tube/cell
#'  has to be used for an experiment at specific temperature (unless the measurements performed directly in liquid \eqn{\text{N}_2})
#'  => for the polar solvents use capillaries or special "flat" cells (e.g. for EPR spectroelectrochemistry),
#'  while for the less polar solvents, common quartz tubes (with the i.d. of \eqn{(2-4)\,\text{mm}}) can be applied.
#'  See also \code{vignette("datasets")}.
#'
#'
#'
#' @family Built-In Datasets
#'
#'
#' @format A data frame with 46 rows and 10 variables/columns:
#' \describe{
#'   \item{Solvent}{Character, solvent name.}
#'   \item{Formula}{Character, ponting to molecular formula.}
#'   \item{MW}{Numeric, pointing to relative molecular weight.}
#'   \item{Boiling_Point_oC}{Numeric, corresponding to \strong{boling} point in \strong{°C}.}
#'   \item{Melting_Point_oC}{Numeric, corresponding \strong{melting} point in \strong{°C}.}
#'   \item{Density_gmL}{Numeric, corresponding to density in \eqn{\text{g}\,\text{mL}^{-1}}.}
#'   \item{Solubility_g100gW}{Character, pointing to solubility in water expressed
#'         in \eqn{\text{g}\,(100\,\text{g}~\text{of}~\text{H}_2\text{O})^{-1}}. 2. Solubility of THF in water is rather complex.}
#'   \item{Dielectric_Const}{Character, corresponding to relative permittivity.}
#'   \item{Flash_Point_oC}{Numeric, pointing to flash point in \strong{°C}.}
#'   \item{Viscosity_cp}{Character, corresponding to solvent dynamic viscosity
#'         in \eqn{\text{cp}\equiv 1\,\text{mPa}\,\text{s}}. The values were collected from
#'         \href{https://www.sigmaaldrich.com/deepweb/assets/sigmaaldrich/marketing/global/documents/614/456/labbasics_pg144.pdf}{Sigma-Aldrich}
#'         for 20°C;
#'         \href{https://pubchem.ncbi.nlm.nih.gov/}{PubChem NCBI} for 20°C and 25°C
#'         and from \href{https://www.accudynetest.com/visc_table.html}{ACCU DYNE TEST} for 20°C, 25°C or 30°C.}
#' }
#' @source \url{https://organicchemistrydata.org/solvents/}
#' @source \url{https://www.sigmaaldrich.com/deepweb/assets/sigmaaldrich/marketing/global/documents/614/456/labbasics_pg144.pdf}
#' @source \url{https://pubchem.ncbi.nlm.nih.gov/}
#' @source \url{https://www.accudynetest.com/visc_table.html}
"solvents_ds"
