#
## ========================== Integration Tests ============================
#
## load series of EPR spectra of TMPD^*+ (`origin = winepr`)
TMPD.2Dbin.datapath <- load_data_example(
  file = "TMPD_specelchem_CV_b.spc"
)
TMPD.2Dpar.datapath <- load_data_example(
  file = "TMPD_specelchem_CV_b.par"
)
TMPD.2D.data.slice <- readEPR_Exp_Specs(
  path_to_file = TMPD.2Dbin.datapath,
  path_to_dsc_par = TMPD.2Dpar.datapath,
  col.names = c("B_G","Slice","dIepr_over_dB"),
  var2nd.series.id = 2,
  origin = "winepr"
)
#
# -------------------- Test for the First Integration ---------------------
#
test_that("Single integration works regardless of the applied method ! ", {
  #
  # simple uncorrected integrals in the series
  integs.1st.tmpd.specelchem.a <-
    TMPD.2D.data.slice %>%
    dplyr::group_by(Slice) %>%
    dplyr::mutate(
      single_Integ = eval_integ_EPR_Spec(
        dplyr::pick(B_G,dIepr_over_dB),
        vectorize = TRUE
      )
    )
  #
  # ...the same can be achieved by
  integs.1st.tmpd.specelchem.b <-
    TMPD.2D.data.slice %>%
    dplyr::group_by(Slice) %>%
    eval_integ_EPR_Spec()
  #
  # are both data frames same
  df.comparison <-
    identical(
      integs.1st.tmpd.specelchem.a,
      integs.1st.tmpd.specelchem.b
    )
  expect_true(df.comparison)
})
#
test_that("Data frame of corrected single ingrals is created !",{
  #
  # single corrected integral data frame
  integs.1st.tmpd.specelchem.df <-
    TMPD.2D.data.slice %>%
    dplyr::group_by(Slice) %>%
    eval_integ_EPR_Spec(
      correct.integ = TRUE,
      BpeaKlim = c(3456.72,3544.22),
      poly.degree = 5
    )
  #
  # column name must be equal to those of previous df
  integs.1st.tmpd.specelchem.df.cn <-
    c("B_G","Slice","dIepr_over_dB","B_mT",
      "single_Integ","baseline_Integ_fit",
      "single_Integ_correct")
  #
  column.names.compar <-
    all.equal(
      integs.1st.tmpd.specelchem.df.cn,
      colnames(integs.1st.tmpd.specelchem.df)
    )
  #
  # maximum of the `baseline_Integ_fit`
  integs.1st.tmpd.specelchem.bm <-
    integs.1st.tmpd.specelchem.df %>%
    dplyr::group_by(Slice) %>%
    dplyr::summarize(Max_baseline = max(baseline_Integ_fit))
  #
  # check vectors:
  max.baseline.compar <- all.equal(
    integs.1st.tmpd.specelchem.bm$Max_baseline,
    rep(79988.282,times = 21)
  )
  #
  expect_true(column.names.compar)
  expect_true(max.baseline.compar)
})
#
test_that("Single integral maxima of series of EPR spectra do match !",{
  #
  # single corrected integrals and their maximum values
  integs.1st.tmpd.specelchem.max <-
    TMPD.2D.data.slice %>%
    dplyr::group_by(Slice) %>%
    eval_integ_EPR_Spec(
      correct.integ = TRUE,
      BpeaKlim = c(3456.72,3544.22),
      poly.degree = 3
    ) %>%
    dplyr::group_by(Slice) %>%
    dplyr::summarize(Max = max(single_Integ_correct))
  #
  # this output should be the same like the following data frame
  integs.1st.df.specelchem.max <-
    data.frame(
      Slice = 0:20,
      Max = c(1.8558597,1.6018651,1.8190508,3.0369030,1.2538464,
              1.7565024,1.6034727,3.6923065,6.7953867,15.1131175,
              26.1367771,35.3320559,35.5394192,32.8155442,24.5111776,
              16.8591808,13.2491926,11.8787895,9.7518394,
              8.1541803,7.1738379) * 1e+5
    )
  #
  # are both intensity vectors the same data frames the same ?
  Max.comparison <- all.equal(
    integs.1st.tmpd.specelchem.max$Max,
    integs.1st.df.specelchem.max$Max
  )
  # are both `Slices` the same?
  Slice.comparison <- all.equal(
    integs.1st.df.specelchem.max$Slice,
    integs.1st.tmpd.specelchem.max$Slice
  )
  #
  expect_true(Max.comparison)
  expect_true(Slice.comparison)
})
#
# -------------------- Test for the Sigmoid Integrals ---------------------
#
test_that(" 'Sigmoid' integration works regardless of the applied method !",{
  #
  integs.2nd.tmpd.specelchem.max.a <-
    TMPD.2D.data.slice %>%
    dplyr::group_by(Slice) %>%
    dplyr::mutate(
      sigmoid_Integ =
        eval_integ_EPR_Spec(
          data.spectr = dplyr::pick(B_G,dIepr_over_dB),
          correct.integ = TRUE,
          BpeaKlim = c(3456.72,3544.22),
          poly.degree = 4,
          sigmoid.integ = TRUE,
          vectorize = TRUE
        )$sigmoid
    ) %>%
    dplyr::summarize(Max_sigmoid = max(sigmoid_Integ))
  #
  # the same method, now with `pracma::cumtrapz()`
  # using the previously created single integral series
  integs.2nd.tmpd.specelchem.max.b <-
    TMPD.2D.data.slice %>%
    dplyr::group_by(Slice) %>%
    eval_integ_EPR_Spec(
      correct.integ = TRUE,
      BpeaKlim = c(3456.72,3544.22),
      poly.degree = 4
    ) %>%
    dplyr::group_by(Slice) %>%
    dplyr::mutate(sigmoid_Integ =
                    pracma::cumtrapz(B_G,single_Integ_correct)[,1]) %>%
    dplyr::mutate(sigmoid_Integ_correct =
                    abs(min(sigmoid_Integ) - sigmoid_Integ)) %>%
    dplyr::group_by(Slice) %>%
    dplyr::summarize(Max_sigmoid = max(sigmoid_Integ_correct))
  #
  compar.double.integs <-
    all.equal(
      integs.2nd.tmpd.specelchem.max.a$Max_sigmoid,
      integs.2nd.tmpd.specelchem.max.b$Max_sigmoid,
      tolerance = 0.16 # Sigmoid integrals are in the order of 1e+7
    )
  #
  expect_true(compar.double.integs)
})
#
