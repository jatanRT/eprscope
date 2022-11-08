#
#' TBC (Absolute EPR Quantity)
#'
#'
#' @description
#' tbc
#'
#'
#' @param double.integ tbc
#' @param nu.GHz tbc
#' @param power.mW tbc
#' @param modul.amp.mT tbc
#' @param qValue tbc
#' @param tube.sample.id.mm tbc
#' @param fill.sample.h.mm tbc
#' @param Norm.constant tbc
#' @param Temp.K tbc
#' @param S tbc
#' @param centr.sample.h.mm tbc
#' @param microW.cavity tbc
#'
#'
#' @return tbc
#'
#'
#' @examples
#' tbc
#' tbc
#'
#'
#' @export
#'
#'
quantitativ_EPR_abs <- function(double.integ,
                                nu.GHz,
                                power.mW,
                                modul.amp.mT,
                                qValue,
                                tube.sample.id.mm,
                                fill.sample.h.mm,
                                Norm.constant = 1,
                                Temp.K = 298,
                                S = 0.5,
                                centr.sample.h.mm = 61,
                                microW.cavity = "rectangular"){
  #
  ## Physical Constants:
  Planck.const <- constants::syms$h
  Boltzmann.const <- constants::syms$k
  Avogadro.No <- constants::syms$na
  #
  ## Boltzmann factor:
  n.B <- (Planck.const*nu.GHz*1e+9)/(2*Boltzmann.const*Temp.K)
  ## `Third` quantification factor in definition:
  third.quant.factor <- sqrt(power.mW*1e-3)*modul.amp.mT*1e-3*qValue*n.B*S*(S + 1)
  ## Tube volume:
  tube.volume.m3 <- (fill.sample.h.mm*1e-3)*pi*((tube.sample.id.mm/2)*1e-3)^2
  #
  if (microW.cavity == "rectangular"){
    #
    ## Cavity constants/characteristics:
    point.sample.c.factor <- 8.51e-09 # unitless
    ## difference between the cavity center and the sample center position:
    h.center.diff <- 61 - centr.sample.h.mm  ## in mm
    h.cavity.length <- 23 # in mm
    #
    ## Polynomial function to characterize intensity distribution within the cavity:
    ## `y` corresponds to distance from cavity center in mm:
      intensity.poly.function <- function(y) {1.00179 - 0.00307086*y - 0.0265409*y^2 +
        0.000297603*y^3 + 0.000223277*y^4 - 4.53833e-06*y^5 - 4.1451e-07*y^6 +
        1.89417e-08*y^7 - 1.48241e-09*y^8
      }
    if (abs(h.center.diff) > 0 & abs(h.center.diff) < ((h.cavity.length-fill.sample.h.mm)/2)){
      #
      ## Integration of the polynomial function
      integral.poly.list <- stats::integrate(intensity.poly.function,
                                             lower = (h.center.diff - (fill.sample.h.mm/2)),
                                             upper = (h.center.diff + (fill.sample.h.mm/2)))
      integral.poly <- integral.poly.list[[1]]
      #
      ## Own quantification:
      ## Number of species:
      No.paramag.spc <- double.integ/((point.sample.c.factor/integral.poly)*Norm.constant*third.quant.factor)
      ## Number of species per effective cm
      No.paramag.cm.spc <- (No.paramag.spc/fill.sample.h.mm)*10
      ## NUmber of species in cm^3:
      No.paramag.V.spc <- No.paramag.spc/(tube.volume.m3/1e+6)
      ## Number od species => concentration mol*dm^{-3}
      No.paramag.c.spc <- (No.paramag.spc/Avogadro.No)/(tube.volume.m3/1e+3)
      #
      ## Result:
      No_paramagSpecies <- list(N_cm = No.paramag.cm.spc,N_cm3 = No.paramag.V.spc,c_M = No.paramag.c.spc)
    }
    if (abs(h.center.diff) > 0 & abs(h.center.diff) == h.cavity.length/2){
      #
      if (h.center.diff > 0){

        ## Integration of the polynomial function
        integral.poly.list <- stats::integrate(intensity.poly.function,
                                               lower = (h.center.diff - (fill.sample.h.mm/2)),
                                               upper = (h.center.diff))
        integral.poly <- integral.poly.list[[1]]
      }
      if (h.center.diff < 0){
        ## Integration of the polynomial function
        integral.poly.list <- stats::integrate(intensity.poly.function,
                                               lower = (h.center.diff),
                                               upper = (h.center.diff + (fill.sample.h.mm/2)))
        integral.poly <- integral.poly.list[[1]]
      }
      #
      ## Own quantification:
      ## Number of species:
      No.paramag.spc <- double.integ/((point.sample.c.factor/integral.poly)*Norm.constant*third.quant.factor)
      ## Number of species per effective cm
      No.paramag.cm.spc <- (No.paramag.spc/fill.sample.h.mm/2)*10
      ## NUmber of species in cm^3:
      No.paramag.V.spc <- No.paramag.spc/(tube.volume.m3/2/1e+6)
      ## Number od species => concentration mol*dm^{-3}
      No.paramag.c.spc <- (No.paramag.spc/Avogadro.No)/(tube.volume.m3/2/1e+3)
      #
      ## Result:
      No_paramagSpecies <- list(N_cm = No.paramag.cm.spc,N_cm3 = No.paramag.V.spc,c_M = No.paramag.c.spc)
    }
    if (abs(h.center.diff) > 0 & abs(h.center.diff) > h.cavity.length/2){
      No_paramagSpecies <- "Accurate number of paramagnetic species is not available"
    }
    if (abs(h.center.diff) == 0 & fill.sample.h.mm >= h.cavity.length){
      #
      ## Integration of the polynomial function
      integral.poly.list <- stats::integrate(intensity.poly.function,
                                             lower = (-h.cavity.length/2),
                                             upper = (h.cavity.length/2))
      integral.poly <- integral.poly.list[[1]]
      #
      ## Own quantification:
      ## Number of species:
      No.paramag.spc <- double.integ/((point.sample.c.factor/integral.poly)*Norm.constant*third.quant.factor)
      ## Number of species per effective cm
      No.paramag.cm.spc <- (No.paramag.spc/h.cavity.length)*10
      ## Number of species in cm^3:
      No.paramag.V.spc <- No.paramag.spc/((h.cavity.length*1e-3)*pi*((tube.sample.id.mm/2)*1e-3)^2/1e6)
      ## Number od species => concentration mol*dm^{-3}
      No.paramag.c.spc <- (No.paramag.spc/Avogadro.No)/((h.cavity.length*1e-3)*pi*((tube.sample.id.mm/2)*1e-3)^2/1e3)
      #
      ## Result:
      No_paramagSpecies <- list(N_cm = No.paramag.cm.spc,N_cm3 = No.paramag.V.spc,c_M = No.paramag.c.spc)
    }
    if (abs(h.center.diff) == 0 & fill.sample.h.mm < h.cavity.length){
      #
      ## Integration of the polynomial function
      integral.poly.list <- stats::integrate(intensity.poly.function,
                                             lower = (-(fill.sample.h.mm/2)),
                                             upper = ((fill.sample.h.mm/2)))
      integral.poly <- integral.poly.list[[1]]
      #
      ## Own quantification:
      ## Number of species:
      No.paramag.spc <- double.integ/((point.sample.c.factor/integral.poly)*Norm.constant*third.quant.factor)
      ## Number of species per effective cm
      No.paramag.cm.spc <- (No.paramag.spc/fill.sample.h.mm)*10
      ## NUmber of species in cm^3:
      No.paramag.V.spc <- No.paramag.spc/(tube.volume.m3/1e+6)
      ## Number od species => concentration mol*dm^{-3}
      No.paramag.c.spc <- (No.paramag.spc/Avogadro.No)/(tube.volume.m3/1e+3)
      #
      ## Result:
      No_paramagSpecies <- list(N_cm = No.paramag.cm.spc,N_cm3 = No.paramag.V.spc,c_M = No.paramag.c.spc)
    }
  }
  if (microW.cavity == "highsensitive"){
    #
    ## Cavity constants/characteristics:
    point.sample.c.factor <- 9.271e-09 # unitless
    ## difference between the cavity center and the sample center position:
    h.center.diff <- 62.5 - centr.sample.h.mm  ## in mm
    h.cavity.length <- 40 # in mm
    #
    ## Polynomial function to characterize intensity distribution within the cavity:
    ## `y` corresponds to distance from cavity center in mm:
      intensity.poly.function <- function(y) {0.99652 + 0.00737177*y - 0.00559614*y^2 -
          2.88221e-05*y^3 + 1.00404e-05*y^4 + 3.43695e-08*y^5 - 5.0404e-09*y^6 -
          1.4783e-11*y^7 - 1.29132e-12*y^8
      }
    if (abs(h.center.diff) > 0 & abs(h.center.diff) < ((h.cavity.length-fill.sample.h.mm)/2)){
      #
      ## Integration of the polynomial function
      integral.poly.list <- stats::integrate(intensity.poly.function,
                                             lower = (h.center.diff - (fill.sample.h.mm/2)),
                                             upper = (h.center.diff + (fill.sample.h.mm/2)))
      integral.poly <- integral.poly.list[[1]]
      #
      ## Own quantification:
      ## Number of species:
      No.paramag.spc <- double.integ/((point.sample.c.factor/integral.poly)*Norm.constant*third.quant.factor)
      ## Number of species per effective cm
      No.paramag.cm.spc <- (No.paramag.spc/fill.sample.h.mm)*10
      ## NUmber of species in cm^3:
      No.paramag.V.spc <- No.paramag.spc/(tube.volume.m3/1e+6)
      ## Number od species => concentration mol*dm^{-3}
      No.paramag.c.spc <- (No.paramag.spc/Avogadro.No)/(tube.volume.m3/1e+3)
      #
      ## Result:
      No_paramagSpecies <- list(N_cm = No.paramag.cm.spc,N_cm3 = No.paramag.V.spc,c_M = No.paramag.c.spc)
    }
    if (abs(h.center.diff) > 0 & abs(h.center.diff) == h.cavity.length/2){
      #
      if (h.center.diff > 0){

        ## Integration of the polynomial function
        integral.poly.list <- stats::integrate(intensity.poly.function,
                                               lower = (h.center.diff - (fill.sample.h.mm/2)),
                                               upper = (h.center.diff))
        integral.poly <- integral.poly.list[[1]]
      }
      if (h.center.diff < 0){
        ## Integration of the polynomial function
        integral.poly.list <- stats::integrate(intensity.poly.function,
                                               lower = (h.center.diff),
                                               upper = (h.center.diff + (fill.sample.h.mm/2)))
        integral.poly <- integral.poly.list[[1]]
      }
      #
      ## Own quantification:
      ## Number of species:
      No.paramag.spc <- double.integ/((point.sample.c.factor/integral.poly)*Norm.constant*third.quant.factor)
      ## Number of species per effective cm
      No.paramag.cm.spc <- (No.paramag.spc/fill.sample.h.mm/2)*10
      ## NUmber of species in cm^3:
      No.paramag.V.spc <- No.paramag.spc/(tube.volume.m3/2/1e+6)
      ## Number od species => concentration mol*dm^{-3}
      No.paramag.c.spc <- (No.paramag.spc/Avogadro.No)/(tube.volume.m3/2/1e+3)
      #
      ## Result:
      No_paramagSpecies <- list(N_cm = No.paramag.cm.spc,N_cm3 = No.paramag.V.spc,c_M = No.paramag.c.spc)
    }
    if (abs(h.center.diff) > 0 & abs(h.center.diff) > h.cavity.length/2){
      No_paramagSpecies <- "Accurate number of paramagnetic species is not available"
    }
    if (abs(h.center.diff) == 0 & fill.sample.h.mm >= h.cavity.length){
      #
      ## Integration of the polynomial function
      integral.poly.list <- stats::integrate(intensity.poly.function,
                                             lower = (-h.cavity.length/2),
                                             upper = (h.cavity.length/2))
      integral.poly <- integral.poly.list[[1]]
      #
      ## Own quantification:
      ## Number of species:
      No.paramag.spc <- double.integ/((point.sample.c.factor/integral.poly)*Norm.constant*third.quant.factor)
      ## Number of species per effective cm
      No.paramag.cm.spc <- (No.paramag.spc/h.cavity.length)*10
      ## Number of species in cm^3:
      No.paramag.V.spc <- No.paramag.spc/((h.cavity.length*1e-3)*pi*((tube.sample.id.mm/2)*1e-3)^2/1e6)
      ## Number od species => concentration mol*dm^{-3}
      No.paramag.c.spc <- (No.paramag.spc/Avogadro.No)/((h.cavity.length*1e-3)*pi*((tube.sample.id.mm/2)*1e-3)^2/1e3)
      #
      ## Result:
      No_paramagSpecies <- list(N_cm = No.paramag.cm.spc,N_cm3 = No.paramag.V.spc,c_M = No.paramag.c.spc)
    }
    if (abs(h.center.diff) == 0 & fill.sample.h.mm < h.cavity.length){
      #
      ## Integration of the polynomial function
      integral.poly.list <- stats::integrate(intensity.poly.function,
                                             lower = (-(fill.sample.h.mm/2)),
                                             upper = ((fill.sample.h.mm/2)))
      integral.poly <- integral.poly.list[[1]]
      #
      ## Own quantification:
      ## Number of species:
      No.paramag.spc <- double.integ/((point.sample.c.factor/integral.poly)*Norm.constant*third.quant.factor)
      ## Number of species per effective cm
      No.paramag.cm.spc <- (No.paramag.spc/fill.sample.h.mm)*10
      ## NUmber of species in cm^3:
      No.paramag.V.spc <- No.paramag.spc/(tube.volume.m3/1e+6)
      ## Number od species => concentration mol*dm^{-3}
      No.paramag.c.spc <- (No.paramag.spc/Avogadro.No)/(tube.volume.m3/1e+3)
      #
      ## Result:
      No_paramagSpecies <- list(N_cm = No.paramag.cm.spc,N_cm3 = No.paramag.V.spc,c_M = No.paramag.c.spc)
    }
  }
   #
  return(No_paramagSpecies)
  #
}
