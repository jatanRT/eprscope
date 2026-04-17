#
## ============ Test to extract g(iso)-Value/g(iso)-Factor from the EPR Spectra ===============
#
## -------------------------- Single line simulated spectrum -------------------------------
#
test_that("g-Value of the derivative (simulated) EPR spectrum properly extracted.", {
  #
  ## predefined g-Value and MW frequency
  mw.freq.GHz <- 9.8943
  g.val <- 1.9804
  #
  ## simulation
  sim.deriv.single.line <-
    eval_sim_EPR_iso(g.iso = g.val,
      instrum.params = c(Bcf = 3490,
                         Bsw = 200,
                         Npoints = 1600,
                         mwGHz = mw.freq.GHz),
      lineGL.DeltaB = list(3.2,NULL)
    )
  #
  ## g-Value from the spectrum
  g.deriv.single.line <-
    eval_gFactor_Spec(
      sim.deriv.single.line$df,
      nu.GHz = mw.freq.GHz,
      Intensity = "dIeprSim_over_dB",
      B = "Bsim_G"
     )
  #
  ## comparison
  expect_equal(g.val,g.deriv.single.line,tolerance = 1e-4)
  #
})
#
## -------------------- Integrated simulated spectrum with HF structure ---------------------
#
test_that("g-Value of the integrated (simulated) EPR spectrum (with HF structure) properly extracted.",{
  #
  ## predefined values
  g.val.hf <- 2.0027
  mw.freq.GHz.hf <- 9.8
  #
  ## PNT EPR spectrum in integrated form,
  ## first generate the simulated spectrum/data
  pnt.sim.integ.iso <-
    eval_sim_EPR_iso(g.iso = g.val.hf,
      instrum.params = c(Bcf = 3500, # central field
                         Bsw = 100, # sweep width
                         Npoints = 4096,
                         mwGHz = mw.freq.GHz.hf), # MW Freq. in GHz
      B.unit = "G",
      nuclear.system = list(
        list("1H",3,5.09), # 3 x A(1H) = 5.09 MHz
        list("1H",6,17.67) # 6 x A(1H) = 17.67 MHz
       ),
      lineSpecs.form = "integrated",
      lineGL.DeltaB = list(0.54,NULL), # Gauss. FWHM in G
      Intensity.sim = "Integ_Intensity"
    )
  #
  g.integ.hf.spec <-
    eval_gFactor_Spec(
      pnt.sim.integ.iso$df,
      nu.GHz = mw.freq.GHz.hf,
      Intensity = "Integ_Intensity",
      B = "Bsim_G",
      lineSpecs.form = "absorption"
    )
  #
  expect_equal(g.val.hf,g.integ.hf.spec,tolerance = 1e-4)
  #
})
#
