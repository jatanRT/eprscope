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
#'   for a standard sample with known concentration/amount of radicals/"spins".
#'
#'
#' @details
#'   There are two approaches how to quantify the number of paramagnetic species/radicals/spins.
#'   The \strong{relative} one needs a standard sample with a known spin number and can be evaluated
#'   by the sigmoid integral ratio of the sample under study and that of the standard.
#'   While the \strong{absolute} method do not need the reference sample however, it requires
#'   a precise cavity signal calibration as well as standardized cell geometry. Both are provided
#'   by the EPR instrument and lab-glass manufacturers (see e.g. \insertCite{hirschCapill2023}{eprscope}).
#'   In case of absolute quantitative EPR analysis the sigmoid integral (its maximum value),
#'   \eqn{I_{\text{sigmoid}}},can be used to calculate the number of "spins"/radicals/paramagnetic species,
#'   \eqn{N_{\text{Spins}}} =>
#'   \deqn{N_{\text{Spins}} = I_{\text{sigmoid}}/((c/f(B_1,B_{\text{m}}))\,(G_{\text{R}}\,t_{\text{C}}
#'   \,N_{\text{Scans}})\,[\sqrt{P_{\text{MW}}}\,B_{\text{m}}\,Q\,n_{\text{B}}\,S(S+1)]\,N_{\text{Spins}})}
#'   where  the quantity notations possess the following meaning:
#'   \tabular{ll}{
#'   ------------------------ \tab --------------------------------- \cr
#'   \strong{Quantity Symbol} \tab \strong{Meaning/Short Desription} \cr
#'   ------------------------ \tab --------------------------------- \cr
#'   \eqn{c}\tab Point sample calibration factor (instrumental) \cr
#'   \eqn{f(B_1,B_\text{m})} \tab Spatial distribution of the microwave \eqn{B_1} and modulation
#'   amplitude within the cavity/probehead/resonator (instrumental/theoretical) \cr
#'   \eqn{G_{\text{R}}} \tab Receiver gain (commonly in \eqn{\text{dB}} units (instrumental)) \cr
#'   \eqn{t_{\text{C}}} \tab Conversion time (commonly in \eqn{\text{ms}}) which is an analogy
#'   to integration time in other spectroscopies (instrumental) \cr
#'   \eqn{N_{\text{Scans}}} \tab Number of scans/accumulations during the experiment (instrumental) \cr
#'   ------------------------ \tab --------------------------------- \cr
#'   }
#'    ...and actually it corresponds to integrated intensity distribution within
#'   the cavity/probehead for different sample length and positions. Such intensity distribution
#'   is expressed by polynomial and is supplied by the manufacturer as well.
#'
#'
#' @references
#'   \insertRef{eatonQepr2010}{eprscope}
#'
#'   \insertRef{RWeberXenon2011}{eprscope}
#'
#'   \insertRef{hirschCapill2023}{eprscope}
#'
#'   \insertRef{mazurAnalysis2000}{eprscope}
#'
#'   \insertRef{portisElectro1953}{eprscope}
#'
#'   \insertRef{mailerQuant1977}{eprscope}
#'
#'
#' @importFrom Rdpack reprompt
#'
#'
#' @inheritParams eval_sim_EPR_iso
#' @param integ.sigmoid.max Numeric value or vectoe of entire EPR spectrum sigmoid integral.
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
#' @param point.sample.factor Numeric value ... tbc ...
#' @param tube.sample.id.mm Numeric value equal to internal diameter (in `mm`) of the tube/cell used
#'   for the quantitative EPR experiment.
#' @param fill.sample.h.mm Numeric value equal to sample height (in `mm`) within the tube/cell.
#' @param eff.cavity.h.mm Numeric value equal to effective cavity/probehead height/length,
#'   usually provided by the probehead manufacturer.
#' @param fn.B1.Bm.fit Numeric vector (coefficients) of the polynomial degree from 6 to 11
#'   or character string ("theoretical").
#' @param Norm.const Numeric value corresponding to normalization constant (see
#'   \code{\link{quantify_EPR_Norm_const}}). \strong{Default}: \code{Norm.const = NULL} in case
#'   if the EPR spectrum was normalized by such constant either upon measurement or during processing.
#'   Otherwise it must be provided by \code{\link{quantify_EPR_Norm_const}}.
#' @param Temp.K Numeric temperature value in `K`. Because the \code{instrum.params} also contains temperature
#'   input one may choose which definition (\code{Temp.K} or \code{TK}) is taken for calculation.
#'   Either \code{Temp.K} or \code{TK} CAN BE ALSO `NULL` but NOT BOTH !! In the latter case default value `298 K`
#'   is considered.
#' @param S Numeric value, total spin sample quantum number. For radicals \code{S = 0.5}
#'   (\strong{default}).
#'
#'
#' @return List of the following quantities:
#'   \describe{
#'   \item{N.cm}{Number of spins per effective centimeter. It is defined
#'     as the cm around the maximum, \eqn{\pm 5\,\text{mm}}, of the intensity
#'     distribution curve within the cavity \eqn{f(B_1,B_{\text{m}})} from
#'     the equation above shown in details.}
#'    \item{N.cm3}{Number of spins per \eqn{\text{cm}^3}.}
#'    \item{c.M}{Concentration of spins/radicals in \eqn{\text{mol}\,\text{dm}^{-3}}.}
#'   }
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
                             point.sample.factor = 8.51e-09,
                             fill.sample.h.mm,
                             eff.cavity.h.mm = 23,
                             fn.B1.Bm.fit = c(1.00179,-3.07086e-3,-2.65409e-2,
                                              2.97603e-4,2.23277e-4,-4.53833e-06,
                                              -4.1451e-07,1.89417e-08,-1.48241e-09),
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
  ## is considered) <==> `fn.B1.Bm.fit` != "theoretical"
  if (length(fn.B1.Bm.fit) > 1){
    fn.fit.poly <- function(y,length){
      if (length == 6){
        return(
          (1/pi) * (fn.B1.Bm.fit[1] + (fn.B1.Bm.fit[2] * y) + (fn.B1.Bm.fit[3] * y^2) +
                    (fn.B1.Bm.fit[4] * y^3) + (fn.B1.Bm.fit[5] * y^4) +
                    (fn.B1.Bm.fit[6] * y^5))
          )
        ## the following simplification do not work, IT MUST BE EXPRESSED EXPLICITELY !!
        # (1/pi) * sum(sapply(seq(fn.B1.Bm.fit), function(j) fn.B1.Bm.fit[j] * y^(j-1L)))
      }
      if (length == 7){
        return(
          (1/pi) * (fn.B1.Bm.fit[1] + (fn.B1.Bm.fit[2] * y) + (fn.B1.Bm.fit[3] * y^2) +
                    (fn.B1.Bm.fit[4] * y^3) + (fn.B1.Bm.fit[5] * y^4) +
                    (fn.B1.Bm.fit[6] * y^5) + (fn.B1.Bm.fit[7] * y^6))
          )
      }
      if (length == 8){
        return(
          (1/pi) * (fn.B1.Bm.fit[1] + (fn.B1.Bm.fit[2] * y) + (fn.B1.Bm.fit[3] * y^2) +
                    (fn.B1.Bm.fit[4] * y^3) + (fn.B1.Bm.fit[5] * y^4) +
                    (fn.B1.Bm.fit[6] * y^5) + (fn.B1.Bm.fit[7] * y^6) +
                    (fn.B1.Bm.fit[8] * y^7))
          )
      }
      if (length == 9){
        return(
          (1/pi) * (fn.B1.Bm.fit[1] + (fn.B1.Bm.fit[2] * y) + (fn.B1.Bm.fit[3] * y^2) +
                    (fn.B1.Bm.fit[4] * y^3) + (fn.B1.Bm.fit[5] * y^4) +
                    (fn.B1.Bm.fit[6] * y^5) + (fn.B1.Bm.fit[7] * y^6) +
                    (fn.B1.Bm.fit[8] * y^7) + (fn.B1.Bm.fit[9] * y^8))
               )
      }
      if (length == 10){
        return(
          (1/pi) * (fn.B1.Bm.fit[1] + (fn.B1.Bm.fit[2] * y) + (fn.B1.Bm.fit[3] * y^2) +
                    (fn.B1.Bm.fit[4] * y^3) + (fn.B1.Bm.fit[5] * y^4) +
                    (fn.B1.Bm.fit[6] * y^5) + (fn.B1.Bm.fit[7] * y^6) +
                    (fn.B1.Bm.fit[8] * y^7) + (fn.B1.Bm.fit[9] * y^8) +
                    (fn.B1.Bm.fit[10] * y^9))
               )
      }
      if (length == 11){
        return(
          (1/pi) * (fn.B1.Bm.fit[1] + (fn.B1.Bm.fit[2] * y) + (fn.B1.Bm.fit[3] * y^2) +
                    (fn.B1.Bm.fit[4] * y^3) + (fn.B1.Bm.fit[5] * y^4) +
                    (fn.B1.Bm.fit[6] * y^5) + (fn.B1.Bm.fit[7] * y^6) +
                    (fn.B1.Bm.fit[8] * y^7) + (fn.B1.Bm.fit[9] * y^8) +
                    (fn.B1.Bm.fit[10] * y^9) + (fn.B1.Bm.fit[11] * y^10))
               )
      }
    }
    #
    ## and the final function
    fn.fit.poly.q <- function(y){fn.fit.poly(y,length = length(fn.B1.Bm.fit))}
  }
  #
  if (fill.sample.h.mm <= eff.cavity.h.mm){
    #
    ## tube volume in m^3
    tube.volume.m3 <- (fill.sample.h.mm * 1e-3) * pi * ((tube.sample.id.mm / 2) * 1e-3)^2
    #
    if (length(fn.B1.Bm.fit) == 1){
      ## function to characterize the relative intensity distribution within
      ## the cavity: see also https://doi.org/10.1016/0022-2364(77)90133-0
      ## and https://doi.org/10.1006/jmre.1997.1248
      ## `y` corresponds to distance from cavity center in mm:
      ## It must be normalized per half of the standing wave cycle
      ## (sample is positioned in the cavity-center) => therefore
      ## multiplied by `(1/pi)`,
      ## see also https://doi.org/10.1016/0022-2364(77)90133-0 +
      ## https://doi.org/10.1103/PhysRev.91.1071
      fn.fit.theo <- function(y,q){
        (1/pi) * ((q * (cospi(y/eff.cavity.h.mm))^3) / sqrt(1 + (q^2 * (cospi(y/eff.cavity.h.mm))^2)))
      }
      ## coeff. `q` depends on `fill.sample.h.mm`, see below
      #
      if (fill.sample.h.mm > (0.1 * eff.cavity.h.mm)){
        ## function
        fn.fit.theo.q1 <- function(y){fn.fit.theo(y,q = sqrt(4.3))}
        #
        ## Integration of the polynomial function,
        ## resolution for the integration
        integral.poly.resolv = fill.sample.h.mm * 5 ## resolution 0.2 mm can be easily measured
        integral.poly.list <- stats::integrate(fn.fit.theo.q1,
                                               lower = -(fill.sample.h.mm / 2),
                                               upper = (fill.sample.h.mm / 2),
                                               subdivisions = integral.poly.resolv)
      }
      if (fill.sample.h.mm == (0.1 * eff.cavity.h.mm)){
        ## function
        fn.fit.theo.q2 <- function(y){fn.fit.theo(y,q = sqrt(3.1))}
        #
        ## Integration of the polynomial function,
        ## resolution for the integration
        integral.poly.resolv = fill.sample.h.mm * 5 ## resolution of 0.2 mm selected
        integral.poly.list <- stats::integrate(fn.fit.theo.q2,
                                               lower = -(fill.sample.h.mm / 2),
                                               upper = (fill.sample.h.mm / 2),
                                               subdivisions = integral.poly.resolv)
      }
    }
    if (length(fn.B1.Bm.fit) > 1){
      #
      ## Integration of the polynomial function,
      ## resolution for the integration
      integral.poly.resolv = fill.sample.h.mm * 5 ## resolution 0.2 mm
      integral.poly.list <- stats::integrate(fn.fit.poly.q,
                                             lower =  -(fill.sample.h.mm / 2),
                                             upper = (fill.sample.h.mm / 2),
                                             subdivisions = integral.poly.resolv)
    }
    #
  }
  if (fill.sample.h.mm > eff.cavity.h.mm){
    #
    ## tube volume in m^3
    tube.volume.m3 <- (eff.cavity.h.mm * 1e-3) * pi * ((eff.cavity.h.mm / 2) * 1e-3)^2
    #
    if (length(fn.B1.Bm.fit) == 1){
      stop(" Theoretical description/fit of `f(B1,Bm)` function for condition\n
           `fill.sample.h.mm` > `eff.cavity.h.mm` is not available. Please,\n
           use polynomial fit instead ! ")
    }
    if (length(fn.B1.Bm.fit) > 1){
      #
      ## Integration of the polynomial function,
      ## resolution for the integration
      integral.poly.resolv = eff.cavity.h.mm * 5 ## resolution 0.2 mm
      integral.poly.list <- stats::integrate(fn.fit.poly.q,
                                             lower =  - (eff.cavity.h.mm / 2),
                                             upper = (eff.cavity.h.mm / 2),
                                             subdivisions = integral.poly.resolv)
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
