#
quantitativ_EPR_abs <- function(double.integ,
                                tube.id.mm,
                                tube.h.mm,
                                MWFQ.GHz,
                                MWPW.mW,
                                B0MA.mT,
                                QValue,
                                AVGS,
                                RCAG.dB,
                                SPTP.ms,
                                Temp.K = 298,
                                S.quantum.No = 0.5,
                                microW.cavity = "rectangular"){
  #
  ## Physical Constants:
  Planck.const <- constants::syms$h
  Boltzmann.const <- constants::syms$k
  Avogadro.No <- constants::syms$na
  #
  ## Boltzmann factor:
  n.B <- (Planck.const*MWFQ.GHz*1e+9)/(2*Boltzmann.const*Temp.K)
  ## Normalization constant Norm.const:
  Norm.const <- SPTP.ms*AVGS*20*10^(RCAG.dB/20)
  ## `Third` quant. factor in definition:
  third.quant.factor <- sqrt(MWPW.mW*1e-3)*B0MA.mT*1e-3*QValue*n.B*S.quantum.No*(S.quantum.No + 1)
  ## Tube volume:
  tube.volume.m3 <- (tube.h.mm*1e-3)*pi*((tube.id.mm/2)*1e-3)^2
  #
  if (microW.cavity == "rectangular"){
    #
    ## Cavity constants/characteristics:
    point.sample.c.factor <- 8.51e-09 # unitless
    h.cavity.center <- 61 # in mm
    h.cavity.length <- 23 # in mm
    #
    ## Polynomial function to characterize intensity distribution within the cavity:
      intensity.poly.function <- function(y) {1.00179 - 0.00307086*y - 0.0265409*y^2 +
        0.000297603*y^3 + 0.000223277*y^4 - 4.53833e-06*y^5 - 4.1451e-07*y^6 +
        1.89417e-08*y^7 - 1.48241e-09*y^8
      }
    if (tube.h.mm < h.cavity.length){
      #
      ## Integration of the polynomial function
      integral.poly.list <- stats::integrate(intensity.poly.function,
                                        lower = (h.cavity.center - tube.h.mm/2)*1e-3,
                                        upper = (h.cavity.center + tube.h.mm/2)*1e-3)
      integral.poly <- integral.poly.list[[1]]
      #
      ## Own quantification:
      ## Number of species:
      No.paramag.spc <- double.integ/((point.sample.c.factor/integral.poly)*Norm.const*third.quant.factor)
      ## Number of species per effective cm
      No.paramag.cm.spc <- No.paramag.spc/tube.h.mm/10
      ## Number od species => concentration mol*dm^{-3}
      No.paramag.c <- (No.paramag.spc/Avogadro.No)/(tube.volume.m3/1e3)
      ## Result:
      Result <- list(N_cm = No.paramag.cm.spc,c_N_mol_dm3 = No.paramag.c)
    }
    if (tube.h.mm >= h.cavity.length){
      #
      ## Integration of the polynomial function
      integral.poly.list <- stats::integrate(intensity.poly.function,
                                             lower = (h.cavity.center - h.cavity.length/2)*1e-3,
                                             upper = (h.cavity.center + h.cavity.length/2)*1e-3)
      integral.poly <- integral.poly.list[[1]]
      #
      ## Own quantification:
      ## Number of species:
      No.paramag.spc <- double.integ/((point.sample.c.factor/integral.poly)*Norm.const*third.quant.factor)
      ## Number of species per effective cm
      No.paramag.cm.spc <- No.paramag.spc/h.cavity.length/10
      ## Number od species => concentration mol*dm^{-3}
      No.paramag.c <- (No.paramag.spc/Avogadro.No)/((h.cavity.length*1e-3)*pi*((tube.id.mm/2)*1e-3)^2/1e3)
      ## Result:
      Result <- list(N_cm = No.paramag.cm.spc,c_N_mol_dm3 = No.paramag.c)
    }
  }


}
