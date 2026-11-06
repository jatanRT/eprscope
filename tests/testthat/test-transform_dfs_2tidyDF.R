#
## ========================== Test Transformation of Data Frame Objests to a Tidy one ============================
#
## load TMPD*+ EPR spectrum
tmpd.data.path <-
  load_data_example(file = "TMPD_specelchem_accu_b.asc")
tmpd.data.df <-
  readEPR_Exp_Specs(
    tmpd.data.path,
    col.names = c("B_G","dIepr_over_dB"),
    qValue = 3500,
    origin = "winepr"
   ) %>%
  dplyr::mutate(index = 1:nrow(.))
#
## load Aminoxyl EPR Spectrum
aminoxyl.data.path <-
  load_data_example(file = "Aminoxyl_radical_a.txt")
aminoxyl.data.df <-
  readEPR_Exp_Specs(
    aminoxyl.data.path,
    qValue = 2100
  )
#
## both data frames should have the same column
intensity.string <- "dIepr_over_dB"
#
## names for both data frames
names.df <- c("TMPD Radical", "Aminoxyl")
#
## vector for normalization
normalization.vector <- c(5400,1e-5)
#
test_that("Both 'Aminoxyl' and 'TMPD' data frames have the same number of columns. ", {
  #
  expect_equal(ncol(aminoxyl.data.df),4)
  expect_equal(ncol(tmpd.data.df),4)
  #
})
#
test_that("Both 'Aminoxyl' and 'TMPD' data frames have the same intesity column. ", {
  #
  expect_true(
    intensity.string %in% colnames(aminoxyl.data.df) &&
      intensity.string %in% colnames(tmpd.data.df)
  )
  #
})
#
test_that("Both EPR spectra are correctly normalized in the final TIDY data frame. ", {
  #
  spectra.data.tidy.df <-
    transform_dfs_2tidyDF(
      tmpd.data.df,
      aminoxyl.data.df,
      df.names = names.df,
      norm.vec = normalization.vector,
      var2nd.series = "Radical"
    )
  #
  ## generating list
  argumn.dfs.list <- list(tmpd.data.df,aminoxyl.data.df)
  names(argumn.dfs.list) <- names.df
  #
  ## row code for intensity normalization
  argumn.dfs.list.new <-
    Map(
      function(u,v) {
        u <- u %>%
          dplyr::mutate(!!rlang::quo_name(intensity.string) :=
                          .data[[intensity.string]] / v)
      },
      argumn.dfs.list,
      normalization.vector
    )
  #
  tmpd.intensity <- spectra.data.tidy.df %>%
    dplyr::filter(Radical == names.df[1]) %>%
    dplyr::pull(dplyr::all_of(c(intensity.string)))
  #
  aminoxyl.intensity <- spectra.data.tidy.df %>%
    dplyr::filter(Radical == names.df[2]) %>%
    dplyr::pull(dplyr::all_of(c(intensity.string)))
  #
  expect_equal(
    tmpd.intensity,
    tmpd.data.df[[intensity.string]] / normalization.vector[1]
  )
  expect_equal(
    aminoxyl.intensity,
    aminoxyl.data.df[[intensity.string]] / normalization.vector[2]
  )
  #
  expect_equal(
    tmpd.intensity,argumn.dfs.list.new[[1]][[intensity.string]]
  )
  #
  expect_equal(
    aminoxyl.intensity,argumn.dfs.list.new[[2]][[intensity.string]]
  )
  #
})
