#
## ================= Testing Quantification of Integrals of EPR Sim. Series =================
## --------------------------- One EPR spectral component -----------------------------------
#
## load ASCII text file from Xenon software
triaryl.radCat.path <-
  load_data_example(file = "Triarylamine_radCat_decay_a.txt")
#
## load instrumental parameters file
triaryl.radCat.path.params <-
  load_data_example(file = "Triarylamine_radCat_decay_a.DSC")
## load data params. for kinetics
triaryl.radCat.data.params.df <-
  readEPR_params_tabs(triaryl.radCat.path.params)$params
#
## corresponding data (double integrated EPR
## spectrum = `Area` vs `time`)
triaryl.radCat.data.xenon <-
  readEPR_Exp_Specs(
    triaryl.radCat.path,
    header = TRUE,
    fill = TRUE,
    select = c(3,7),
    col.names = c("time_s","Area"),
    x.unit = "s",
    x.id = 1,
    Intensity.id = 2,
    qValue = 1700,
    data.structure = "others"
  ) %>%
  na.omit() %>%
  ## time correction
  dplyr::mutate(
    time_s = correct_time_Exp_Specs(
      time_s,
      Nscans = triaryl.radCat.data.params.df[6,2],
      sweep.time.s = triaryl.radCat.data.params.df[12,2]
    )
  )
#
## now load the whole time series (kinetics) and perform
## the simulation fitting for the EPR spectra in that series
triarylamine.decay.series.bin.path <-
load_data_example(file =
        "Triarylamine_radCat_decay_series.DTA")
## loading the kinetics:
triarylamine.decay.series.data <-
  readEPR_Exp_Specs_kin(
    path_to_file =
      triarylamine.decay.series.bin.path
   )
#
quant.sim.triarylamine.df <-
 quantify_EPR_Sim_series(
    data.spectra.series =
      triarylamine.decay.series.data$df,
    # this is an `{eprscope}` pkg. dir:
    dir_ASC_sim = system.file(
      "extdata",
      package = "eprscope",
      mustWork = TRUE
    ),
    optim.method = "cobyla", ## similar results with 'sbplx' or 'lbfgs' or `pswarm` with `Nmax.evals = 768`
    # name pattern for `csv` EPR sim. data:
    name.pattern.sim =
      "Triarylamine_radCat_decay_9thSpec",
    col.names.sim = c(
      "Bsim_G",
      "B_G",
      "dIeprSim_over_dB",
      "dIepr_over_dB",
      "Norm_dIeprSim_over_dB"
    ),
    x.sim.id = 1,
    Intensity.sim.id = 3,
    optim.params.init = c(0,0.4),
    output.area.stat = TRUE,
    msg.optim.progress = FALSE
)
#
test_that(" Data coming from 'Xenon' matches that of the 'Quant.Sim.Fit' (a small error determined). ", {
  #
  ## comparison of time vectors in the range of (6 - 1477) s (resolution 1 s)
  #
  expect_equal(
    quant.sim.triarylamine.df$time_s,
    triaryl.radCat.data.xenon$time_s,
    tolerance = 1.1e-3 ## in s (i.e. milliseconds)
  )
  #
  expect_true(
    all(
      abs(
        quant.sim.triarylamine.df$Area_Sim_A -
          triaryl.radCat.data.xenon$Area
      ) < 2.2e-3
    )
  )
  #
})
#
## --------------------- Perform the Kinetic Model Check -------------------------
## ------------------------ for radical recombination ----------------------------
#
test_that(" Evaluated kinetic model for 'Xenon' data matches that of the 'Quant.Sim.Fit' (a small error determined). ",{
#
triarylamine.rc.decay.model.xenon <-
  eval_kinR_EPR_modelFit(
    data.qt.expr = triaryl.radCat.data.xenon,
    model.react = "(r=2)R --> [k1] B",
    qvarR = "Area",
    elementary.react = TRUE,
    params.guess = c(
      qvar0R = 0.018,
      k1 = 0.04
    )
  )
#
## the same for fitted and simulated EPR spectral series
triarylamine.rc.decay.model.quant <-
  eval_kinR_EPR_modelFit(
    data.qt.expr = quant.sim.triarylamine.df,
    model.react = "(r=2)R --> [k1] B",
    qvarR = "Area_Sim_A",
    elementary.react = TRUE,
    params.guess = c(
      qvar0R = 0.018,
      k1 = 0.04
    )
  )
#
## kinetics must be the same, therefore rate constants must equal
## within an experimental error (difference should be low)
k1.xenon <-
  triarylamine.rc.decay.model.xenon$df.coeffs[2,1] ## k1 = 0.0507 \pm 0.0002
k1.quant.sim <-
  triarylamine.rc.decay.model.quant$df.coeffs[2,1] ## k1 = 0.0578 \pm 0.0002
#
qvar0.xenon <-
  triarylamine.rc.decay.model.xenon$df.coeffs[1,1] ## qvar0 = 0.01850 \pm 0.00005
qvar0.quant.sim <-
  triarylamine.rc.decay.model.quant$df.coeffs[1,1] ## qvar0 = 0.01636 \pm 0.00003
#
expect_lt(abs(k1.quant.sim - k1.xenon),7.5e-3)
expect_lt(abs(qvar0.quant.sim - qvar0.xenon),2.5e-3)
#
})
