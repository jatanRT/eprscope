test_that("Hyperfine Coupling(A)/Splitting(a) Constants, for the TMPD Radical Cation, Matches.", {
  #
  ## load the data
  gauss.file.path <-
    load_data_example(file = "TMPDAradCatEPRa.inp.log.zip")
  gauss.file <- unzip(gauss.file.path)
  #
  ## list of selected nuclei (numbers)
  nucs.slct <-
    list(c(7, 8), ## 2 x 14N
         c(13, 14, 15, 16), ## 4 x 1H (aromatic)
         c(17, 18, 19, 20,
           21, 22, 23, 24,
           25, 26, 27, 28) ## 12 x 1H (methyl groups)
    )
  #
  ## a/A extraction from file + rearrangement
  symmetry.aAs.df <-
    rearrange_aAiso_QCHorgau(
      gauss.file,
      nuclei.list.slct = nucs.slct
    )
  #
  ## remove the `log` (not required anymore)
  file.remove("TMPDAradCatEPRa.inp.log")
  #
  ## check the selected nuclei list from the output `df`
  ## with that in the original list
  expect_equal(
    (nchar(symmetry.aAs.df$NuclearGroup[1]) - 21)/2, ## should be 12
    length(nucs.slct[[3]]) ## should be 12
  )
  expect_equal(
    (nchar(symmetry.aAs.df$NuclearGroup[3]) - 12)/2, ## should be 4
    length(nucs.slct[[2]]) ## shold be 4
  )
  #
  ## check the columns of the `symmetry.aAs.df`
  expect_equal(
    symmetry.aAs.df$Aiso_MHz,
    c(20.25,17.52,-5.24)
  )
  #
  expect_equal(
    symmetry.aAs.df$aiso_mT,
    c(0.72,0.63,-0.19)
  )
  #
})
