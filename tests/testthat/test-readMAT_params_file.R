#
## ============== Tests to Read MATLAB `.mat` WORKSPACE FILES ===============
#
## load aminoxyl corresponding EPR spectrum `.mat` file
aminoxyl.sim.fit.mat.path <-
  load_data_example("Aminoxyl_radical_a.mat")
#
## interacting nucleus for the aminoxyl radical
aminoxyl.nucs.string <- "14N"
aminoxyl.nucs.number <- 1
#
## load tetramethyl-phenylenediamine (TMPD) corresponding EPR spectrum `.mat` file
tmpd.sim.fit.mat.path <-
  load_data_example("TMPD_specelchem_accu_b.mat")
#
## interacting nucleus for the TMPD^*+
tmpd.nucs.string <- "14N,1H,1H"
tmpd.nucs.number <- c(2,4,12)
#
test_that("Reading of MATLAB-EasySpin simulation parameters works !", {
  #
  ## read simulation parameters/arguments for the aminoxyl radical
  aminoxyl.sim.fit.mat <- readMAT_params_file(
    path_to_MAT = aminoxyl.sim.fit.mat.path,
    str.var = "Sim1"
  )
  #
  ## read simulation parameters/arguments for the TMPD radical cation
  tmpd.sim.fit.mat <- readMAT_params_file(
    path_to_MAT = tmpd.sim.fit.mat.path,
    str.var = "Sys"
  )
  #
  ## checking
  expect_equal(aminoxyl.sim.fit.mat$Nucs,aminoxyl.nucs.string)
  expect_equal(aminoxyl.sim.fit.mat$n,aminoxyl.nucs.number)
  expect_equal(tmpd.sim.fit.mat$Nucs,tmpd.nucs.string)
  expect_equal(tmpd.sim.fit.mat$n,tmpd.nucs.number)
  #
})
#
test_that("Reading of MATLAB-EasySpin fitting parameters works !",{
  #
  ## fitted (`fit`) scaled EPR intensity (vector)
  tmpd.fit.Intensity.mat <- readMAT_params_file(
    path_to_MAT =  tmpd.sim.fit.mat.path,
    str.var = "fit1",
    field.var = "fit"
  )
  #
  ## magnetic flux density (vector)
  tmpd.fit.B.mat <- readMAT_params_file(
    path_to_MAT = tmpd.sim.fit.mat.path,
    str.var = "B"
  )
  #
  ## the length of both vectors must be the same
  expect_equal(
    length(tmpd.fit.Intensity.mat),
    length(tmpd.fit.B.mat)
  )
  #
})

