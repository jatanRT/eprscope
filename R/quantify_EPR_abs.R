#
#' Absolute Quantification of Radicals/Spins
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'   A short description...TBC...in the Most Common EPR Cavities
#'
#'
#'
#' @details
#'   There are two approaches how to quantify the number of paramagnetic species/radicals/spins.
#'   The \strong{relative} one needs a standard sample with a known number and can be evaluated
#'   by the sigmoid integral ratio of the sample under study and that of the standard.
#'   Whereas the \strong{absolute} method do not need the reference sample however, it requires
#'   a precise cavity signal calibration as well as standardized cell geometry. Both are provided
#'   by the EPR instrument and lab-glass manufacturers (see e.g.
#'   \href{https://hirschmann-laborgeraete.de/en/artikelgruppe/96001}{Hirschmann Capillaries}).
#'   In case of absolute quantitative EPR analysis the sigmoid integral (its maximum value),
#'   \eqn{I_{\text{sigmoid}}},can be expressed as follows =>
#'   \deqn{I_{\text{sigmoid}} = (c/f(B_1,B_{\text{m}}))\,(G_{\text{R}}\,t_{\text{C}}\,N_{\text{Scans}})\,
#'   [\sqrt{P_{\text{MW}}}\,B_{\text{m}}\,Q\,n_{\text{B}}\,S(S+1)]\,N_{\text{Spins}}}
#'   where  \eqn{c} is the point sample calibration factor (supplied by the spectrometer manufacturer);
#'   \eqn{f(B_1,B_{\text{m}})} is the function depicting the spatial distribution of \eqn{B_1}
#'   and \eqn{B_{\text{m}}} and actually it corresponds to integrated intensity distribution within
#'   the cavity/probehead for different sample length and positions. Such intensity distribution
#'   is expressed by polynomial and is supplied by the manufacturer as well. Additional quantities
#'   are the following => \eqn{G_{\text{R}}} is receiver gain; \eqn{t_{\text{C}}} corresponds to
#'   conversion time; \eqn{N_{\text{Scans}}} is the number of scans/accumulations. Because the receiver
#'   gain is expressed in \eqn{\text{dB}}...tbc.
#'
#'
#' @references{
#'   \insertRef{eatonQepr2010}{eprscope}
#'
#'   \insertRef{hirschCapill2023}{eprscope}
#' }
#'
#'
#' @importFrom Rdpack reprompt
#'
#'
#' @inheritParams eval_sim_EPR_iso
#' @param integ.sigmoid.max Numeric value of entire EPR spectrum sigmoid integral.
#' @param instrum.params Named numeric vector containing instrumental parameters required
#'   for the quantification =>
#'   \tabular{ll}{
#'   \code{PmW} \tab power of the MW source in mW \cr
#'   \code{BmmT} \tab modulation amplitude (magnetic flux density modulation,
#'   \eqn{B_{\text{m}}}) in mT \cr
#'   \code{TK} \tab temperature in K \cr
#'   \code{mwGHz} \tab applied microwave frequency in `GHz` to record the continuous wave (CW)
#'   EPR spectrum \cr
#'   }
#'   \strong{Default}: \code{instrum.params = NULL} because they are primarily extracted
#'   from the \code{path_to_dsc_par} based on the \code{origin}.
#' @param qValue Numeric value of the sensitivity `Q` factor. For the processed EPR spectra by
#'   the `{eprscope}` package the \code{integ.sigmoid.max} is usually normalized by the `Q` value.
#'   Therefore, \strong{default}: \code{qValue = NULL}.
#' @param tube.sample.id.mm Numeric value equal to internal diameter (in `mm`) of the tube/cell used
#'   for the quantitative EPR experiment.
#' @param fill.sample.h.mm Numeric value equal to sample height (in `mm`) within the tube/cell.
#' @param Norm.const Numeric value corresponding to normalization constant (see
#'   \code{\link{quantify_EPR_Norm_const}}). \strong{Default}: \code{Norm.const = NULL} in case
#'   when the EPR spectrum was normalized by that constant either upon measurement or during processing.
#' @param Temp.K Numeric
#' @param S Numeric, ...tbc...
#' @param microW.cavity Character string, ...tbc... \strong{Default}:
#'   \code{microW.cavity = "rectangular"}.
#'
#'
#' @return List of the following quantities:
#'
#'   * \code{Ncm}, number of spins per effective centimeter. It is defined
#'     as the cm around the maximum, \eqn{\pm 5\,\text{mm}}, of the intensity
#'     distribution curve within the cavity \eqn{f(B_1,B_{\text{m}})} from
#'     the equation above shown in details.
#'
#'   * \code{Ncm3}, corresponding to number of spins per \eqn{\text{cm}^3}
#'
#'   * \code{cM}, denotes the concentration in \eqn{\text{mol}\,\text{dm}^{-3}}
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#'
#' @export
#'
#'
quantify_EPR_Abs <- function(integ.sigmoid.max,
                             instrum.params = NULL, ##  otherwise c(mwGHz,PmW,BmmT,TK = 298)
                             path_to_dsc_par,
                             origin = "xenon",
                             qValue = NULL,
                             tube.sample.id.mm,
                             fill.sample.h.mm,
                             Norm.const = NULL,
                             Temp.K = NULL,
                             S = 0.5,
                             microW.cavity = "rectangular") {
  #
  ## 'Temporary' processing variables
  . <- NULL
  ## Physical Constants:
  Planck.const <- constants::syms$h
  Boltzmann.const <- constants::syms$k
  Avogadro.No <- constants::syms$na
  #
  ## Definition for `qValue` and `Norm.const`
  qValue <- qValue %>% `if`(is.null(qValue), 1, .)
  Norm.const <- Norm.const %>% `if`(is.null(Norm.const), 1, .)
  #
  ## Additional instrumental parameters
  ## Extracting instrumental parameter values from `.DSC`/`.dsc` or `.par` files
  ## or from named numeric vector above
  if (is.null(path_to_dsc_par)){
    if (is.null(instrum.params)){
      stop(" Please, define `instrum.params` like modulation amplitude, MW freq.,... ! ")
    } else {
      Bm.mT <- unname(instrum.params["BmmT"])
      P.mW <- unname(instrum.params["PmW"])
      Temper.K <- unname(instrum.params["TK"])
      nu.GHz <- unname(instrum.params["mwGHz"])
    }
  } else{
    if (!is.null(instrum.params)){
      stop(" Parameters are extracted from file, please define `instrum.params = NULL` ! ")
    } else{
      if (is.null(origin)){
        stop(" Please provide `origin` of the `.DSC`/`.dsc` or `.par` file ! ")
      } else{
        ## reading the table and extracting values form table
        instrum.params.list <- readEPR_params_slct_quant(path_to_dsc_par,origin = origin)
        Bm.mT <- instrum.params.list$BmmT
        P.mW <- instrum.params.list$PmW
        Temper.K <- instrum.params.list$TK
        nu.GHz <- instrum.params.list$mwGHz
      }
    }
  }
  ## Two step conditions for the definition of temperature !
  ## 1st check against `Temp.K`
  Temper.K <- Temper.K %>% `if`(is.null(Temper.K), Temp.K, .)
  ## 2nd check if `Temper.K` was NULL
  Temper.K <- Temper.K %>% `if`(is.null(Temper.K), 298, .)
  #
  ## Boltzmann factor:
  n.B <- (Planck.const * nu.GHz * 1e+9) / (2 * Boltzmann.const * Temper.K)
  ## `Third` quantification factor in definition:
  third.quant.factor <- sqrt(P.mW * 1e-3) * Bm.mT * 1e-3 * qValue * n.B * S * (S + 1)
  ## Tube volume:
  tube.volume.m3 <- (fill.sample.h.mm * 1e-3) * pi * ((tube.sample.id.mm / 2) * 1e-3)^2
  #
  if (microW.cavity == "rectangular") {
    #
    ## Cavity constants/characteristics:
    point.sample.c.factor <- 8.51e-09 # unitless
    ## difference between the cavity center and the sample center position:
    centr.sample.h.mm <- 61 ## in mm
    h.cavity.length <- 23 # in mm
    #
    ## Polynomial function to characterize intensity distribution within the cavity:
    ## `y` corresponds to distance from cavity center in mm:
    intensity.poly.function <- function(y) {
      1.00179 - 0.00307086 * y - 0.0265409 * y^2 +
        0.000297603 * y^3 + 0.000223277 * y^4 - 4.53833e-06 * y^5 - 4.1451e-07 * y^6 +
        1.89417e-08 * y^7 - 1.48241e-09 * y^8
    }
  }
  if (microW.cavity == "highsensitive") {
    #
    ## Cavity constants/characteristics:
    point.sample.c.factor <- 9.271e-09 # unitless
    ## difference between the cavity center and the sample center position:
    centr.sample.h.mm <- 62.5 ## in mm
    h.cavity.length <- 40 # in mm
    #
    ## Polynomial function to characterize intensity distribution within the cavity:
    ## `y` corresponds to distance from cavity center in mm:
    intensity.poly.function <- function(y) {
      0.99652 + 0.00737177 * y - 0.00559614 * y^2 -
        2.88221e-05 * y^3 + 1.00404e-05 * y^4 + 3.43695e-08 * y^5 - 5.0404e-09 * y^6 -
        1.4783e-11 * y^7 - 1.29132e-12 * y^8
    }
  }
  #
  ## CALCULATIONS depending on the sample height =>
  if (fill.sample.h.mm >= h.cavity.length) {
    #
    ## Integration of the polynomial function
    integral.poly.list <- stats::integrate(intensity.poly.function,
                                           lower = (-h.cavity.length / 2),
                                           upper = (h.cavity.length / 2)
    )
    integral.poly <- integral.poly.list[[1]]
    #
    ## Own quantification:
    ## Number of species:
    No.paramag.spc <- integ.sigmoid.max / ((point.sample.c.factor / integral.poly) *
                                             Norm.const * third.quant.factor)
    ## Number of species per effective cm
    No.paramag.cm.spc <- (No.paramag.spc / h.cavity.length) * 10
    ## Number of species in cm^3:
    No.paramag.V.spc <- No.paramag.spc / ((h.cavity.length * 1e-3) * pi *
                                            ((tube.sample.id.mm / 2) * 1e-3)^2 / 1e6)
    ## Number od species => concentration mol*dm^{-3}
    No.paramag.c.spc <- (No.paramag.spc / Avogadro.No) / ((h.cavity.length * 1e-3) * pi *
                                                            ((tube.sample.id.mm / 2) * 1e-3)^2 / 1e3)
  }
  if (fill.sample.h.mm < h.cavity.length) {
    #
    ## Integration of the polynomial function
    integral.poly.list <- stats::integrate(intensity.poly.function,
                                           lower = (-(fill.sample.h.mm / 2)),
                                           upper = ((fill.sample.h.mm / 2))
    )
    integral.poly <- integral.poly.list[[1]]
    #
    ## Own quantification:
    ## Number of species:
    No.paramag.spc <- integ.sigmoid.max / ((point.sample.c.factor / integral.poly) *
                                             Norm.const * third.quant.factor)
    ## Number of species per effective cm
    No.paramag.cm.spc <- (No.paramag.spc / fill.sample.h.mm) * 10
    ## NUmber of species in cm^3:
    No.paramag.V.spc <- No.paramag.spc / (tube.volume.m3 / 1e+6)
    ## Number od species => concentration mol*dm^{-3}
    No.paramag.c.spc <- (No.paramag.spc / Avogadro.No) / (tube.volume.m3 / 1e+3)
  }
  #
  ## RESULT:
  No_paramagSpecies <- list(
    Ncm = No.paramag.cm.spc,
    Ncm3 = No.paramag.V.spc,
    cM = No.paramag.c.spc
  )
  #
  return(No_paramagSpecies)
  #
}
