#
## ========================== Time to Variable Conversion Tests ============================
#
# ------------------------------ EPR Spectroelectrochemistry -------------------------------
#
## load series of EPR spectra of TMPD^*+ (`origin = winepr`)
TMPD.2Dascii.datapath <- load_data_example(
  file = "TMPD_specelchem_CV_b.asc"
)
TMPD.2D.data.slice <- readEPR_Exp_Specs(
  path_to_file = TMPD.2Dascii.datapath,
  col.names = c("B_G","Slice","dIepr_over_dB"),
  var2nd.series.id = 2,
  origin = "winepr"
)
#
test_that("0.352 V is the potential with max. EPR intensity !", {
  #
  # time correction -- 18 seconds betwween
  # 2 recordingd of EPR spectra
  TMPD.2D.data.slice$time_s <-
    TMPD.2D.data.slice$Slice * 18
  #
  # time vector df
  time.s.vector.df <- TMPD.2D.data.slice %>%
    dplyr::group_by(time_s) %>%
    dplyr::group_keys()
  #
  time.compar <- all.equal(
    time.s.vector.df$time_s,
    seq(0,360,by = 18)
  )
  # create `E_V` (potential in Volts) column
  TMPD.2D.data.slice$E_V <-
    convert_time2var(
      TMPD.2D.data.slice$time_s,
      var0 = -0.1, # V (starting potential)
      var.switch = 0.45, # V
      var.rate = 0.003 # V/s (oxidation)
    )
  #
  # which potential correspond to max intensity
  # should be 0.352 V vs. Ag-pseudoref.
  max.intensity.E.V <-
    TMPD.2D.data.slice %>%
    dplyr::filter(dIepr_over_dB == max(dIepr_over_dB)) %>%
    dplyr::pull(E_V)
  #
  expect_true(time.compar)
  expect_equal(max.intensity.E.V,0.352)
})
#
# -------------------------- Variable temperature experiment ---------------------
#
test_that("Variable temperature cooling cycle works !",{
  #
  # create time vector
  time.s <- seq(0,960,by = 60)
  #
  # cooling from 293 K to 253 K and back
  # with the cooling rate 5 K/min
  temperature.vec <-
    convert_time2var(
      time.vals = time.s,
      var0 = 293, # K
      var.switch = 253, # K
      var.rate = - 5, # K/min
      var.rate.unit = "min^{-1}"
    )
  #
  # create hypothetical temperature vector
  t.K.down <- rev(seq(253,293,by = 5))
  t.K.up <- seq(258,293,by = 5)
  t.K <- c(t.K.down,t.K.up)
  #
  expect_equal(t.K,temperature.vec)
})
#
