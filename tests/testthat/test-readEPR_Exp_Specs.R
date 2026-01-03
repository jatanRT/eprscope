#
## load the EPR spectrum of aminoxyl ASCII
aminoxyl.data.ascii.file <-
  load_data_example(file = "Aminoxyl_radical_a.txt")
#
## load EPR spectrum of aminoxyl binary
aminoxyl.data.binary.file <-
  load_data_example(file = "Aminoxyl_radical_a.DTA")
#
## load EPR params. req. to load aminoxyl binary
aminoxyl.params.file <-
  load_data_example(file = "Aminoxyl_radical_a.DSC")
#
## load aminoxyl data, later (below), the output
## should have the same column names like for magnettech
aminoxyl.data.ascii <-
  readEPR_Exp_Specs(
    path_to_file = aminoxyl.data.ascii.file,
    qValue = 2100
  )
#
#
test_that("Do both aminoxyl Xenon data frames (ASCII & Binary) differ ?", {
  #
  aminoxyl.data.binary <-
    readEPR_Exp_Specs(
      path_to_file = aminoxyl.data.binary.file,
      path_to_dsc_par = aminoxyl.params.file,
      qValue = 2100
    )
  #
  ## comparisons
  index.compar <-
    isFALSE(all.equal(aminoxyl.data.ascii$index,aminoxyl.data.binary$index))
  BG.compar <-
    isFALSE(all.equal(aminoxyl.data.ascii$B_G,aminoxyl.data.binary$B_G))
  intensity.compar <-
    isFALSE(
      all.equal(
        aminoxyl.data.ascii$dIepr_over_dB,
        aminoxyl.data.binary$dIeor_over_dB
      )
    )
  #
  ## all must be false
  expect_false(index.compar)
  expect_false(BG.compar)
  expect_false(intensity.compar)
  #
})
#
## load ASCII 2D - time series of TMPD - spectroelectrochemistry
TMPD.2Data.ascii.file <-
  load_data_example(file = "TMPD_specelchem_CV_b.asc")
#
## load Binary 2D - time series -||-
TMPD.2Data.binary.file <-
  load_data_example(file = "TMPD_specelchem_CV_b.spc")
#
## ... and finally load the corresponding parameter file
TMPD.2Data.params.file <-
  load_data_example(file = "TMPD_specelchem_CV_b.par")
#
test_that(" Do the TMPD^+* (2D - WinEPR time series) data frames (ASCII + Bin) have the same structure ?",{
  #
  TMPD.2Data.ascii <-
    readEPR_Exp_Specs(
      path_to_file = TMPD.2Data.ascii.file,
      col.names = c(
        "B_G",
        "Slice",
        "dIepr_over_dB"
      ),
      var2nd.series.id = 2,
      origin = "winepr"
    )
  #
  TMPD.2Data.binary <-
    readEPR_Exp_Specs(
      path_to_file = TMPD.2Data.binary.file,
      path_to_dsc_par = TMPD.2Data.params.file,
      col.names = c(
        "B_G",
        "Slice",
        "dIepr_over_dB"
      ),
      var2nd.series.id = 2,
      origin = "winepr"
    )
  #
  ## comparison
  str.compar <-
    identical(
      str(data.2D.winepr.asc),
      str(data.2D.winepr.bin)
    )
  #
  expect_true(str.compar)
})
#
## load binary magnettech file
acrid.data.magnettech.bin.file <-
  load_file_example(file = "AcridineDeriv_Irrad_365nm.DTA")
#
## load the corresponding parameters
acrid.data.magnettech.params.file <-
  load_file_example(file = "AcridineDeriv_Irrad_365nm.dsc")
#
test_that(" The column names/headers of loaded Xenon & Magnettech files do math ! ",{
  #
  acrid.data.magnettech <-
    readEPR_Expe_Specs(
      path_to_file = acrid.data.magnettech.bin.file,
      path_to_dsc_par = acrid.data.magnettech.params.file,
      qValue = 1829,
      origin = "magnetech"
    )
  #
  ## now check the column names/headers
  acrid.data.magnettech.cols <- colnames(acrid.data.magnettech)
  acrid.data.xenon.cols <- colnames(aminoxyl.data.ascii)
  #
  condition.headers.match <-
    identical(
      acrid.data.magnettech.cols,
      acrid.data.xenon.cols
    )
  #
  expect_true(condition.headers.match)
  #
})
#
