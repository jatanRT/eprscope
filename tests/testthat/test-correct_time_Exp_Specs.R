
#
## ========================== Corrected Time for CW EPR Kinetics/Time Series ============================
#
## load parameter file for kinetics path
triarylam.decay.series.dsc.path <-
  load_data_example(file =
    "Triarylamine_radCat_decay_series.DSC")
#
## load EPR spectra/data path
triarylam.decay.series.dta.path <-
  load_data_example(file =
    "Triarylamine_radCat_decay_series.DTA")
#
## load YGF file with original time included
triarylam.decay.series.ygf.path <-
  load_data_example(file =
    "Triarylamine_radCat_decay_series.YGF")
#
Qval <- readEPR_param_slct(triarylam.decay.series.dsc.path,string = "QValue")
nscans <- readEPR_param_slct(triarylam.decay.series.dsc.path,string = "AVGS")
sweeptimes <- readEPR_param_slct(triarylam.decay.series.dsc.path,string = "SPTP") *
  readEPR_param_slct(triarylam.decay.series.dsc.path,string = "A1RS")
#
## load original data
triarylam.decay.series.orig.df <-
  readEPR_Exp_Specs(
    path_to_file = triarylam.decay.series.dta.path,
    col.names = c(
      "index",
      "B_G",
      "time_s",
      "dIepr_over_dB"
    ),
    var2nd.series.id = 3,
    qValue = Qval
  )
#
## load kinetics with corrected time
triarylam.decay.series.corr <-
  readEPR_Exp_Specs_kin(
    path_to_file = triarylam.decay.series.dta.path
  )
#
## length of the YGF file
triarylam.decay.series.ygf.len <-
  readBin(
    con = triarylam.decay.series.ygf.path,
    what = "raw",
    n = 1e+4
  ) %>% length()
#
## load `.YGF` file
triarylam.decay.series.orig.ygf <-
  readBin(
    con = triarylam.decay.series.ygf.path,
    what = "numeric",
    size = 8, ## Xenon
    n = triarylam.decay.series.ygf.len / 8, ## Xenon
    signed = TRUE,
    endian = "big" ## Xenon
  )
#
time.from.ygf <- triarylam.decay.series.orig.ygf
#
test_that("Instrumental (Xenon) parameters do match. ", {
  #
  expect_equal(Qval,1700)
  expect_equal(nscans,1)
  expect_equal(sweeptimes,0.00509 * 2400)
  #
})
#
test_that("Original time vectors from `.DTA` and `.YGF` do match. ",{
  #
  time.from.dta <-
    triarylam.decay.series.orig.df %>%
    dplyr::group_by(time_s) %>%
    dplyr::group_keys()
  #
  time.from.dta <- time.from.dta[["time_s"]]
  #
  expect_equal(time.from.dta,time.from.ygf)
  #
})
#
test_that("Time from 'EPR loaded' kinetics equals to the corrected one. ",{
  #
  correct.time <- correct_time_Exp_Specs(
    time.s = time.from.ygf,
    Nscans = nscans,
    sweep.time.s = sweeptimes
  )
  #
  ## ...or from the raw code/function
  time.from.raw.code.01 <-
    time.from.ygf + (nscans - 1) * sweeptimes + sweeptimes / 2
  #
  expect_equal(round(time.from.raw.code.01),triarylam.decay.series.corr$time)
  #
  nscans <- nscans - 1
  #
  time.from.raw.code.02 <-
    time.from.ygf + nscans * sweeptimes + sweeptimes / 2
  #
  expect_equal(round(time.from.raw.code.02),triarylam.decay.series.corr$time)
  #
  expect_equal(correct.time,triarylam.decay.series.corr$time)
  #
})
