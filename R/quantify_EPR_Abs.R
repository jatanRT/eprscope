#
#' Absolute Quantification of Radicals/Spins
#'
#'
#' @family Evaluations and Quantification
#'
#'
#' @description
#'   Estimate the number or concentration of "spins"/radicals/paramagnetic species using the absolute
#'   quantitative method by sigmoid integral as well as by instrumental parameters without the need
#'   for a standard sample with known concentration/amount of radicals/"spins". The calculation assumes
#'   that the sample height middle point, within an EPR tube, matches the cavity/resonator center.
#'
#'
#' @details
#'   There are two approaches how to quantify the number of paramagnetic species/radicals/spins.
#'   The \strong{relative} one needs a standard sample with a known spin number and can be evaluated
#'   by the sigmoid integral ratio of the sample under study and that of the standard.
#'   While the \strong{absolute} method do not need the reference sample, it requires
#'   a precise cavity signal calibration as well as standardized cell geometry. Both are provided
#'   by the EPR instrument and lab-glass manufacturers (see e.g. Hirschmann-Laborgeräte (2023), \code{References}).
#'   In case of absolute quantitative EPR analysis the sigmoid integral (its maximum value),
#'   \eqn{I_{\text{sigmoid}}},can be used to calculate the number of "spins"/radicals/paramagnetic species,
#'   \eqn{N_{\text{Spins}}} (see also \code{References}) =>
#'   \deqn{N_{\text{Spins}} = I_{\text{sigmoid}}\,/\,((c/f(B_1,B_{\text{m}}))\,(G_{\text{R}}\,t_{\text{C}}
#'   \,N_{\text{Scans}})\,[\sqrt{P_{\text{MW}}}\,B_{\text{m}}\,Q\,n_{\text{B}}\,S(S+1)])}
#'   where the quantity notations possess the following meaning (whether it is instrumental or sample
#'   dependent it is presented in parentheses):
#'   \tabular{ll}{
#'   \strong{Quantity Symbol} \tab \strong{Meaning/Short Desription} \cr
#'   \eqn{c}\tab Point sample calibration factor (instrumental). \cr
#'   \eqn{f(B_1,B_\text{m})} \tab Spatial distribution of the microwave \eqn{B_1} and modulation
#'   amplitude within the cavity/probehead/resonator (instrumental). \cr
#'   \eqn{G_{\text{R}}} \tab Receiver gain (commonly in \eqn{\text{dB}} units (instrumental)). \cr
#'   \eqn{t_{\text{C}}} \tab Conversion time (commonly in \eqn{\text{ms}}) which is an analogy
#'   to integration time in other spectroscopies (instrumental). \cr
#'   \eqn{N_{\text{Scans}}} \tab Number of scans/accumulations during the experiment (instrumental). \cr
#'   \eqn{P_{\text{MW}}} \tab Microwave power (instrumental). \cr
#'   \eqn{B_{\text{m}}} \tab Modulation amplitude. \cr
#'   \eqn{Q} \tab \emph{Q}-Value or \emph{Q}-Factor characterizing the resonator/cavity/probehead
#'   sensitivity (unitless and instrumental). \cr
#'   \eqn{n_{\text{B}}} \tab Boltzmann factor for temperature dependence (instrumental-sample). \cr
#'   \eqn{S} \tab Total electronic spin quantum number (sample). Commonly, for radicals \eqn{S = 1/2}. \cr
#'   }
#'   Almost all summarized quantities are instrument-dependent. Most of them correspond to the essential
#'   parameters for the experiment and can be easily acquired from the \code{.DSC}/\code{.dsc}/\code{.par} file(s).
#'   The Boltzmann factor describes the population of spin states
#'   by \eqn{\exp{(\Delta \varepsilon)\,/\,(k_{\text{B}}\,T)}}, where \eqn{\Delta \varepsilon} denotes
#'   the energy difference between the basic spin states, \eqn{k_{\text{B}}} is the Boltzmann constant
#'   (available from \code{\link[constants]{syms}}) and the \eqn{T} represents the temperature in \eqn{\text{K}}.
#'   For temperatures \eqn{\geq 4\,\text{K}} and continuous wave experiments where
#'   the \eqn{\Delta \varepsilon = h\,\nu_{\text{MW}}^{}} is constant, this factor may be very well estimated
#'   by the following formula
#'   \deqn{n_{\text{B}} = h\,\nu_{\text{MW}}^{}\,/\,(2\,k_{\text{B}}\,T)}
#'   The term \eqn{(G_{\text{R}}\,t_{\text{C}}\,N_{\text{Scans}})} actually corresponds to normalization constant
#'   which is available from \code{\link{quantify_EPR_Norm_const}}.
#'   Besides the above-described parameters which can be easily estimated there are however characteristics
#'   that requires precise calibration and usually are provided by the spectrometer manufacturers.
#'   The spatial distribution of the microwave, \eqn{B_1}, and modulation
#'   amplitude, \eqn{B_\text{m}}, influences the intensity of the sample predominantly along the \eqn{y}-axis direction
#'   (i.e. when moving the sample tube up or down within the cavity). Such intensity distribution,
#'   depending on the cavity/probehead for different sample length and positions, can be approximated by a polynomial
#'   (see the \code{fn.B1.Bm.fit.y} argument) that is supplied by the manufacturer as well (the coefficients
#'   of the polynomial can be sometimes found in \code{.DSC}/\code{.dsc}/\code{.par} file(s)). For quantitative
#'   purposes, such polynomial is integrated over the length of the sample.
#'
#' @references
#'   Eaton GR, Eaton SS, Barr DP, Weber RT (2010). \emph{Quantitative EPR}. Springer Science and Business Media.
#'   ISBN 978-3-211-92947-6, \url{https://link.springer.com/book/10.1007/978-3-211-92948-3}.
#'
#'   Weber RT (2011). \emph{Xenon User's Guide}. Bruker BioSpin Manual Version 1.3, Software Version 1.1b50.
#'
#'   Hirschmann-Laborgeräte (2023). “Ringcaps.” \url{https://hirschmannlab.de/en/produkt/ringcaps/}.
#'
#'   Mazúr M, Valko M, Morris H (2000). “Analysis of the Radial and Longitudinal Effect in a Double TE104
#'   and a Single TE102 Rectangular Cavity.” \emph{J. Magn. Reson.}, \strong{142}(1), 37–56. ISSN 1090-7807,
#'   \url{https://doi.org/10.1006/jmre.1999.1915}.
#'
#'   Portis AM (1953). “Electronic Structure ofF Centers: Saturation of the Electron Spin Resonance.”
#'   \emph{Phys. Rev.}, \strong{91}(5), 1071–1078, \url{https://doi.org/10.1103/PhysRev.91.1071}.
#'
#'   Mailer C, Sarna T, Swartz HM, Hyde JS (1977). “Quantitative Studies of Free Radicals in Biology:
#'   Corrections to ESR Saturation Data.” \emph{J. Magn. Reson.} (1969), \strong{25}(1), 205–210, ISSN 0022-2364,
#'   \url{https://doi.org/10.1016/0022-2364(77)90133-0}.
#'
#'
#' @inheritParams eval_sim_EPR_iso
#' @param integ.sigmoid.max Numeric value or vector of the entire EPR spectrum sigmoid integral.
#' @param instrum.params Named numeric vector containing instrumental parameters required
#'   for the quantification =>
#'   \tabular{ll}{
#'   \code{PmW} \tab power of the MW source in mW \cr
#'   \code{BmmT} \tab modulation amplitude (magnetic flux density modulation,
#'   \eqn{B_{\text{m}}}) in mT \cr
#'   \code{TK} \tab temperature in K \cr
#'   \code{mwGHz} \tab applied microwave frequency in \code{GHz} to record the continuous wave (CW)
#'   EPR spectrum \cr
#'   }
#'   \strong{Default}: \code{instrum.params = NULL} because they are primarily extracted
#'   from the \code{path_to_dsc_par} based on the \code{origin}.
#' @param qValue Numeric value of the sensitivity \code{Q} factor. For the processed EPR spectra by
#'   the \code{{eprscope}} package the \code{integ.sigmoid.max} is usually normalized by the \code{Q} value.
#'   Therefore, \strong{default}: \code{qValue = NULL}.
#' @param point.sample.factor Numeric value corresponding to point sample correction factor, provided by the
#'   cavity/probehead manufacturer. Value for the standard Bruker rectangular cavity is set as \strong{default}.
#' @param tube.sample.id.mm Numeric value, which equals to internal diameter (in \code{mm}) of the tube/cell used
#'   for the quantitative EPR experiment.
#' @param fill.sample.h.mm Numeric value, referring to sample height (in \code{mm}) within the tube/cell.
#' @param eff.cavity.h.mm Numeric value, which equals to effective cavity/probehead height/length,
#'   usually provided by the probehead manufacturer.
#' @param fn.B1.Bm.fit.y Numeric vector (coefficients) of the polynomial degree from 5 to 12.
#'   Coefficients for the standard Bruker rectangular cavity are set as \strong{default}.
#' @param fn.B1.Bm.fit.y.max Numeric value corresponding to maximum value of the polynomial fit.
#'   Value for the standard Bruker rectangular cavity is set as \strong{default}.
#' @param Norm.const Numeric value corresponding to normalization constant (see
#'   \code{\link{quantify_EPR_Norm_const}}). \strong{Default}: \code{Norm.const = NULL} in case
#'   if the EPR spectrum was normalized by such constant either upon measurement or during processing.
#'   Otherwise it must be provided by the \code{\link{quantify_EPR_Norm_const}}.
#' @param Temp.K Numeric value, temperature value in \code{K}. Because the \code{instrum.params} also contains temperature
#'   input one may choose which definition (\code{Temp.K} or \code{TK}) is taken for the calculation.
#'   Either \code{Temp.K} or \code{TK} CAN BE ALSO \code{NULL} but NOT BOTH !! In the latter case, default value
#'   \code{298 K} is considered.
#' @param S Numeric value, total spin sample quantum number. For radicals \code{S = 0.5}
#'   (\strong{default}).
#'
#'
#' @return List of the following quantities:
#'   \describe{
#'   \item{N.cm}{Number of spins per effective centimeter. It is defined
#'     as the cm around the maximum, \eqn{\pm 5\,\text{mm}}, of the intensity
#'     distribution curve/polynomial fit within the cavity \eqn{f(B_1,B_{\text{m}})} from
#'     the equation above shown in details.}
#'    \item{N.cm3}{Number of spins per \eqn{\text{cm}^3}.}
#'    \item{c.M}{Concentration of spins/radicals in \eqn{\text{mol}\,\text{dm}^{-3}}.}
#'   }
#'
#'
#' @examples
#' \dontrun{
#' ## quantitative analysis (determining the
#' ## radical concentration `c.M`) of a sample measured
#' ## at different temperatures
#' ## all data summarized in `data.tidy.integ`
#' data.quant <- mapply(function(x,y)
#'   {quantify_EPR_Abs(x,
#'     instrum.params = c(PmW = 2.518,
#'                        BmmT = 5.4e-02,
#'                        TK = NULL, ## 298 K
#'                        mwGHz = 9.530265),
#'     path_to_dsc_par = NULL,
#'     origin = NULL,
#'     tube.sample.id.mm = 2.86,
#'     fill.sample.h.mm = 23,
#'     Temp.K = y)$c.M
#'     },
#'   data.tidy.integ$Area,
#'   data.tidy.integ$Temperature_K
#'   )
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
                             point.sample.factor = 8.51e-09,
                             fill.sample.h.mm,
                             eff.cavity.h.mm = 23,
                             fn.B1.Bm.fit.y = c(1.00179,-3.07086e-3,-2.65409e-2,
                                              2.97603e-4,2.23277e-4,-4.53833e-06,
                                              -4.1451e-07,1.89417e-08,-1.48241e-09),
                             fn.B1.Bm.fit.y.max = 0.25, # (1/4)
                             Norm.const = NULL,
                             Temp.K = NULL,
                             S = 0.5) {
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
  Temper.K <- Temper.K %>% `if`(is.null(Temper.K) || is.na(Temper.K), Temp.K, .)
  ## 2nd check if `Temper.K` was NULL
  Temper.K <- Temper.K %>% `if`(is.null(Temper.K) || is.na(Temper.K), 298, .)
  #
  ## Boltzmann factor:
  n.B <- (Planck.const * nu.GHz * 1e+9) / (2 * Boltzmann.const * Temper.K)
  ## `Third` quantification factor in definition:
  third.quant.factor <- sqrt(P.mW * 1e-3) * Bm.mT * 1e-3 * qValue * n.B * S * (S + 1)
  #
  ## polynomial fitting function to integrate (in case if no theoretical description
  ## is considered) <==> `fn.B1.Bm.fit.y` != "theoretical"
  ## function to characterize the relative intensity distribution within
  ## the cavity: see also https://doi.org/10.1016/0022-2364(77)90133-0
  ## and https://doi.org/10.1006/jmre.1997.1248
  ## `y` corresponds to distance from cavity center in mm,
  ##
  ## see also https://doi.org/10.1016/0022-2364(77)90133-0 +
  ## https://doi.org/10.1103/PhysRev.91.1071
  if (length(fn.B1.Bm.fit.y) > 1){
    fn.fit.poly <- function(y,length){
      if (length == 5){
        return(
          (fn.B1.Bm.fit.y.max) * (fn.B1.Bm.fit.y[1] +
                                  (fn.B1.Bm.fit.y[2] * y) +
                                  (fn.B1.Bm.fit.y[3] * y^2) +
                                  (fn.B1.Bm.fit.y[4] * y^3) +
                                    (fn.B1.Bm.fit.y[5] * y^4)
                                  )
        )
        ## the following simplification do not work, IT MUST BE EXPRESSED EXPLICITELY !!
        # (1/(2*pi)) * sum(sapply(seq(fn.B1.Bm.fit.y), function(j) fn.B1.Bm.fit.y[j] * y^(j-1L)))
      }
      if (length == 6){
        return(
          (fn.B1.Bm.fit.y.max) * (fn.B1.Bm.fit.y[1] +
                                  (fn.B1.Bm.fit.y[2] * y) +
                                  (fn.B1.Bm.fit.y[3] * y^2) +
                                  (fn.B1.Bm.fit.y[4] * y^3) +
                                  (fn.B1.Bm.fit.y[5] * y^4) +
                                  (fn.B1.Bm.fit.y[6] * y^5)
                                  )
          )
      }
      if (length == 7){
        return(
          (fn.B1.Bm.fit.y.max) * (fn.B1.Bm.fit.y[1] +
                                  (fn.B1.Bm.fit.y[2] * y) +
                                  (fn.B1.Bm.fit.y[3] * y^2) +
                                  (fn.B1.Bm.fit.y[4] * y^3) +
                                  (fn.B1.Bm.fit.y[5] * y^4) +
                                  (fn.B1.Bm.fit.y[6] * y^5) +
                                  (fn.B1.Bm.fit.y[7] * y^6)
                                  )
          )
      }
      if (length == 8){
        return(
          (fn.B1.Bm.fit.y.max) * (fn.B1.Bm.fit.y[1] +
                                 (fn.B1.Bm.fit.y[2] * y) +
                                 (fn.B1.Bm.fit.y[3] * y^2) +
                                 (fn.B1.Bm.fit.y[4] * y^3) +
                                 (fn.B1.Bm.fit.y[5] * y^4) +
                                 (fn.B1.Bm.fit.y[6] * y^5) +
                                 (fn.B1.Bm.fit.y[7] * y^6) +
                                 (fn.B1.Bm.fit.y[8] * y^7)
                                 )
          )
      }
      if (length == 9){
        return(
          (fn.B1.Bm.fit.y.max) * (fn.B1.Bm.fit.y[1] +
                                  (fn.B1.Bm.fit.y[2] * y) +
                                  (fn.B1.Bm.fit.y[3] * y^2) +
                                  (fn.B1.Bm.fit.y[4] * y^3) +
                                  (fn.B1.Bm.fit.y[5] * y^4) +
                                  (fn.B1.Bm.fit.y[6] * y^5) +
                                  (fn.B1.Bm.fit.y[7] * y^6) +
                                  (fn.B1.Bm.fit.y[8] * y^7) +
                                  (fn.B1.Bm.fit.y[9] * y^8)
                                  )
               )
      }
      if (length == 10){
        return(
          (fn.B1.Bm.fit.y.max) * (fn.B1.Bm.fit.y[1] +
                                  (fn.B1.Bm.fit.y[2] * y) +
                                  (fn.B1.Bm.fit.y[3] * y^2) +
                                  (fn.B1.Bm.fit.y[4] * y^3) +
                                  (fn.B1.Bm.fit.y[5] * y^4) +
                                  (fn.B1.Bm.fit.y[6] * y^5) +
                                  (fn.B1.Bm.fit.y[7] * y^6) +
                                  (fn.B1.Bm.fit.y[8] * y^7) +
                                  (fn.B1.Bm.fit.y[9] * y^8) +
                                  (fn.B1.Bm.fit.y[10] * y^9)
                                  )
               )
      }
      if (length == 11){
        return(
          (fn.B1.Bm.fit.y.max) * (fn.B1.Bm.fit.y[1] +
                                  (fn.B1.Bm.fit.y[2] * y) +
                                  (fn.B1.Bm.fit.y[3] * y^2) +
                                  (fn.B1.Bm.fit.y[4] * y^3) +
                                  (fn.B1.Bm.fit.y[5] * y^4) +
                                  (fn.B1.Bm.fit.y[6] * y^5) +
                                  (fn.B1.Bm.fit.y[7] * y^6) +
                                  (fn.B1.Bm.fit.y[8] * y^7) +
                                  (fn.B1.Bm.fit.y[9] * y^8) +
                                  (fn.B1.Bm.fit.y[10] * y^9) +
                                  (fn.B1.Bm.fit.y[11] * y^10)
                                  )
               )
      }
      if (length == 12){
        return(
          (fn.B1.Bm.fit.y.max) * (fn.B1.Bm.fit.y[1] +
                                  (fn.B1.Bm.fit.y[2] * y) +
                                  (fn.B1.Bm.fit.y[3] * y^2) +
                                  (fn.B1.Bm.fit.y[4] * y^3) +
                                  (fn.B1.Bm.fit.y[5] * y^4) +
                                  (fn.B1.Bm.fit.y[6] * y^5) +
                                  (fn.B1.Bm.fit.y[7] * y^6) +
                                  (fn.B1.Bm.fit.y[8] * y^7) +
                                  (fn.B1.Bm.fit.y[9] * y^8) +
                                  (fn.B1.Bm.fit.y[10] * y^9) +
                                  (fn.B1.Bm.fit.y[11] * y^10) +
                                  (fn.B1.Bm.fit.y[12] * y^11)
                                  )
        )
      }
    }
    #
    ## and the final function for the integration
    fn.fit.poly.q <- function(y){fn.fit.poly(y,length = length(fn.B1.Bm.fit.y))}
  } else {
    stop(" The `fn.B1.Bm.fit.y` polynomial of degree between <5,12> \n
         must be defined to fit the intensity distribution within the cavity !! ")
  }
  #
  if (fill.sample.h.mm <= eff.cavity.h.mm){
    #
    ## tube volume in m^3
    tube.volume.m3 <- (fill.sample.h.mm * 1e-3) * pi * ((tube.sample.id.mm / 2) * 1e-3)^2
    #
    ## The theoretical distribution will be implemented later
    #
    if (length(fn.B1.Bm.fit.y) > 1){
      #
      ## Integration of the polynomial function,
      ## resolution for the integration
      integral.poly.resolv = fill.sample.h.mm * 5 ## resolution 0.2 mm
      integral.poly.list <- stats::integrate(fn.fit.poly.q,
                                             lower =  -(fill.sample.h.mm / 2),
                                             upper = (fill.sample.h.mm / 2),
                                             subdivisions = integral.poly.resolv)
    } else {
      stop(" The `fn.B1.Bm.fit.y` polynomial of degree between <5,12> \n
         must be defined to fit the intensity distribution within the cavity !! ")
    }
    #
  }
  if (fill.sample.h.mm > eff.cavity.h.mm){
    #
    ## tube volume in m^3
    tube.volume.m3 <- (eff.cavity.h.mm * 1e-3) * pi * ((eff.cavity.h.mm / 2) * 1e-3)^2
    #
    # if (length(fn.B1.Bm.fit.y) == 1){
    #   stop(" Theoretical description/fit of `f(B1,Bm)` function for condition\n
    #        `fill.sample.h.mm` > `eff.cavity.h.mm` is not available. Please,\n
    #        use polynomial fit instead ! ")
    # }
    if (length(fn.B1.Bm.fit.y) > 1){
      #
      ## Integration of the polynomial function,
      ## resolution for the integration
      integral.poly.resolv = eff.cavity.h.mm * 5 ## resolution 0.2 mm
      integral.poly.list <- stats::integrate(fn.fit.poly.q,
                                             lower =  - (eff.cavity.h.mm / 2),
                                             upper = (eff.cavity.h.mm / 2),
                                             subdivisions = integral.poly.resolv)
    } else {
      stop(" The `fn.B1.Bm.fit.y` polynomial of degree between <5,12> \n
         must be defined to fit the intensity distribution within the cavity !! ")
    }
  }
  #
  ## because the integration result is list & the integral corresponds to `[[1]]` `value`
  integral.poly <- integral.poly.list[[1]] ## corresponds to `point.sample.c.factor` units
  #
  ## Own quantification:
  ## Number of species:
  No.paramag.spc <- integ.sigmoid.max / ((point.sample.factor / integral.poly) *
                                           Norm.const * third.quant.factor)
  #
  ## `point.sample.factor` and `integral.poly` must possess the same units
  #
  ## Number of species per effective cm (around the `f(B1,Bm)` maximum)
  No.paramag.cm.spc <- (No.paramag.spc / eff.cavity.h.mm) * 10
  #
  ## Number of species in cm^3:
  No.paramag.V.spc <- No.paramag.spc / (tube.volume.m3 * 1e+6)
  ## Number od species => concentration mol*dm^{-3}
  No.paramag.c.spc <- (No.paramag.spc / Avogadro.No) / (tube.volume.m3 * 1e+3)
  #
  ## RESULT:
  No_paramagSpecies <- list(
    N.cm = No.paramag.cm.spc,
    N.cm3 = No.paramag.V.spc,
    c.M = No.paramag.c.spc
  )
  #
  return(No_paramagSpecies)
  #
}
