#
## For the scope of testing the simulation fit procedure, the same experimental
## EPR spectrum of the aminoxyl radical will be used as in the case
## of `test-eval_sim_EPR_iso`
#
# Find out the following values from the EPR spectrum simulation fit
#
#         <-> DeltaB1.     <-> DeltaB2.     <-> DeltaB3   overall DeltaB from non-linear fit
#         g[1]              g[2] = g.iso    g[3]
#
#        /|               /|               /|
#   ----/ | /------------/ | /------------/ | /------
#         |/               |/               |/
#                 a1/A1           a2/A2                   overall A from non-linear fit
#         <----------------><--------------->
#
## first of all load the experimental file example
#
## load the Qvalue and microwave frequency
## first of all load the parameter file =>
aminoxyl.rad.path.params.a <-
  load_data_example(file = "Aminoxyl_radical_a.DSC")
#
Qvalue <-
  readEPR_param_slct(aminoxyl.rad.path.params.a, string = "QValue")
mw.freq.GHz <-
  readEPR_param_slct(aminoxyl.rad.path.params.a, string = "MWFQ") * 1e-9
#
## load the experimental EPR spectrum
aminoxyl.rad.path.data.a <-
  load_data_example(file = "Aminoxyl_radical_a.txt")
aminoxyl.rad.data.a <-
  readEPR_Exp_Specs(aminoxyl.rad.path.data.a, qValue = Qvalue)
#
test_that("The parameters determined from the expr. EPR spectrum
          as well as from the EPR simulation fit equals.", {
  #
  ## To evaluate the Aiso and DeltaB from the experimental spectrum
  ## use the same code like in `test-eval_sim_EPR_iso` =>
  #
  ## from the expr. spectrum, the following regions may be obtained =>
  B.regions.mT <- list(c(346.5, 348.5), c(348.5, 350.5), c(350.5, 352.5))
  #
  ## evaluating g
  g <- sapply(B.regions.mT, function(i) {
    eval_gFactor_Spec(
      data.spectr = aminoxyl.rad.data.a,
      nu.GHz = mw.freq.GHz,
      B.unit = "mT",
      B = "B_mT",
      Blim = i
    )
  })
  #
  ## Determination of the A_iso from the experimental EPR spectrum =>
  ## first of all all `B`s with maximal intensity should be evaluated =>
  ## the same `B` regions as above can be used
  B.max <- sapply(B.regions.mT, function(k) {
    eval_extremeX_Spec(
      data.spectr = aminoxyl.rad.data.a,
      xlim = k
    )
  })
  #
  ## now calculate both `B.max[2] - B.max[1]` as well as `B.max[3] - B.max[2]`
  ## as also depicted in the scheme above (at the beginning of this test script)
  ## the g.iso value corresponds to g[2] above
  A.iso.expr <- c() ## in MHz
  for (l in 1:(length(B.max) - 1)) {
    A.iso.expr[l] <-
      convert_a_mT_2A(B.max[l + 1] - B.max[l],
        g.val = g[2]
      )
  }
  ## ... and the mean value
  mean.A.iso.expr <- round(mean(A.iso.expr), 2) ## the mean value is 52.349 MHz
  #
  ## To simulate that EPR spectrum also the line width is required =>
  DeltaB.expr <- c() ## in mT
  for (m in seq(B.regions.mT)) {
    DeltaB.expr[m] <-
      eval_DeltaXpp_Spec(
        data.spectr = aminoxyl.rad.data.a,
        xlim = B.regions.mT[[m]]
      )
  }
  ## ...and the mean value
  mean.DeltaB.expr <- round(mean(DeltaB.expr), 2) ## the mean value equals to 0.541 mT
  #
  ## EPR Spectrum Simulation Fit, the instrumental parameters are already
  ## defined above and are taken from the `.DSC` file
  ## In this case combination of Gaussian (G) and Lorentzian (L) lineshape
  ## was applied => `lineG.cont` * G + (1 - `lineG.cont`) * L
  lineG.cont <- 0.97 # this close to simulation in `test-eval_sim_EPR_iso()`
  aminoxyl.rad.data.simFit.a <-
    eval_sim_EPR_isoFit(
      data.spectr.expr = aminoxyl.rad.data.a,
      nu.GHz = mw.freq.GHz,
      B.unit = "mT",
      lineG.content = lineG.cont,
      optim.method = "neldermead",
      nuclear.system.noA = list("14N", 1),
      baseline.correct = "constant",
      optim.params.init = c(
        g[2],
        mean.DeltaB.expr,
        mean.DeltaB.expr,
        0,
        1e-4,
        mean.A.iso.expr
      ),
      sim.check = F
    )
  #
  ## extracting A.iso from the simulation 6th index in `optim.params.init`
  ## as well as within the `best.fit.params`
  A.iso.sim.fit <-
    round(aminoxyl.rad.data.simFit.a$best.fit.params[[1]][6], digits = 2)
  #
  ## extracting the sum of the residual squares
  sum.LSQ.min.sim <-
    aminoxyl.rad.data.simFit.a$sum.LSQ.min
  #
  ## weighted value of the Gaussian and Lorentzian linewidths
  weight.DeltaB.sim.fit <-
    round(
      (lineG.cont * aminoxyl.rad.data.simFit.a$best.fit.params[[1]][2] +
        (1 - lineG.cont) * aminoxyl.rad.data.simFit.a$best.fit.params[[1]][3]),
      digits = 2
    )
  #
  ## g-value
  g.iso.sim.fit <-
    aminoxyl.rad.data.simFit.a$best.fit.params[[1]][1]
  #
  # ========= FITTING THE SIMULATED EPR SPECTRUM ONTO
  #           THE EXPERIMENTAL ONE NICELY WORKS ==========
  #
  # Checking the values
  # parameter length
  # The expected differences are processed by `expect_lt(e)` because of fitting
  expect_length(aminoxyl.rad.data.simFit.a$best.fit.params[[1]], 6)
  ## A MHz, the same tol. as before =>
  expect_lt(abs(A.iso.sim.fit - mean.A.iso.expr), 4e-1) # 0.4 MHz \approx 0.14 G should be OK
  ## sum of the residual squares (lower than ...) =>
  expect_lt(aminoxyl.rad.data.simFit.a$sum.LSQ.min[[1]], 1.3e-8)
  ## Linewidths => expr. + sim. fit. comparison (difference lower than 0.03)  =>
  expect_lte(abs(weight.DeltaB.sim.fit - mean.DeltaB.expr), 0.03) # in mT
  ## g-Values => expr. + sim. fit. comparison
  expect_lte(abs(g.iso.sim.fit - g[2]),2.4e-4) # 2e-4 is the usual experimental uncertainty
  #
})
