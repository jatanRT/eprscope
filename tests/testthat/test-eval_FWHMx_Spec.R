#
## ============ Test to find out FWHM from the Integrated EPR Spectra ===============
#
## ------------------------- Single line simulated spectrum -------------------------
#
test_that("FWHM of the single line integrated EPR spectrum matches that of the simulated one !", {
  #
  single.fwhm.Gauss <- 4.20 # G
  single.fwhm.Gauss.mT <- 0.420
  #
  single.line.iso.sim <-
    eval_sim_EPR_iso(
      g.iso = 1.9804,
      instrum.params = c(Bcf = 3490,
                         Bsw = 200,
                         Npoints = 3200,
                         mwGHz = 9.8943),
      lineGL.DeltaB = list(single.fwhm.Gauss,NULL),
      lineSpecs.form = "integrated"
    )
  #
  single.line.iso.sim.B <-
    eval_sim_EPR_iso(
      g.iso = 1.9804,
      instrum.params = c(Bcf = 349,
                         Bsw = 20,
                         Npoints = 3200,
                         mwGHz = 9.8943),
      B.unit = "mT",
      lineGL.DeltaB = list(single.fwhm.Gauss.mT,NULL),
      lineSpecs.form = "integrated"
    )
  #
  single.line.iso.sim.fwhm <-
    eval_FWHMx_Spec(
      data.spectr.integ = single.line.iso.sim$df,
      x = "Bsim_G",
      Intensity = "Intensity_Sim",
      xlim = c(3550,3600)
  )
  #
  single.line.iso.sim.fwhm.B <-
    eval_FWHMx_Spec(
      data.spectr.integ = single.line.iso.sim.B$df,
      x = "Bsim_mT",
      Intensity = "Intensity_Sim",
      xlim = c(355,360)
    )
  #
  expect_equal(single.line.iso.sim.fwhm,single.fwhm.Gauss,tolerance = 5e-2) # 0.05 G
  expect_equal(single.line.iso.sim.fwhm.B,single.fwhm.Gauss.mT,tolerance = 1.5e-2) # mT
})
#
## ------------------------- Simulated spectrum with HF structure-------------------------
#
test_that("FWHM of the integrated spectrum with HF structure matches that of the simulated one !",{
  #
  hf.spectrum.iso.sim <-
    eval_sim_EPR_iso(
      g.iso = 2.003,
      instrum.params = c(Bcf = 3490,
                         Bsw = 200,
                         Npoints = 2600, # different number of points
                         mwGHz = 9.8),
      nuclear.system = list("1H",3,15),
      lineGL.DeltaB = list(1.6,NULL),
      lineSpecs.form = "integrated"
    )
  #
  ## loop for all 4 HFS lines
  xlim.list <- list(
    c(3485,3490.1),
    c(3490.1,3495.7),
    c(3495.7,3501.1),
    c(3501.1,3507)
  )
  #
  hf.spectrum.iso.sim.fwhm <-
    sapply(
      xlim.list,
      function(x) {
        eval_FWHMx_Spec(
          data.spectr.integ = hf.spectrum.iso.sim$df,
          x = "Bsim_G",
          Intensity = "Intensity_Sim",
          xlim = x
        )
      }
    )
  #
  expect_equal(rep(1.6,times = 4),hf.spectrum.iso.sim.fwhm,tolerance = 0.05) # 0.05 G
  #
})
