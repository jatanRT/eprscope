test_that("Magnetic flux density (B) conversion works.", {
  #
  ## initial value =>
  B.init <- 0.34901024 # in Tesla
  ## convert to Gauss
  B.init.G <- convert_B(B.val = B.init,
                        B.unit = "T",
                        B.2unit = "G")
  #
  ## convert into Tesla in order to check decimal places
  B.init.T <- convert_B(B.val = B.init,
                        B.unit = "T",
                        B.2unit = "T")
  ## check the values
  expect_equal(B.init.G,3490.102)
  expect_equal(B.init.T,0.3490102)
  #
  ## number of decimal places
  B.init.G.ndecim <- nchar(strsplit(as.character(B.init.G),"\\.")[[1]][2])
  B.init.T.ndecim <- nchar(strsplit(as.character(B.init.T),"\\.")[[1]][2])
  #
  ## check the number of decimal places
  expect_identical(B.init.G.ndecim,3L)
  expect_identical(B.init.T.ndecim,7L)
  #
  ## B vector in order to check the dimension/length
  Bcf <- 349.78235 ## central field in mT
  Bsw <- 25 ## sweep width in mT
  Npoints <- 1024 ## number of points
  B.init.vec.mT <- seq(Bcf - (Bsw/2),
                       Bcf + (Bsw/2),
                       length.out = Npoints)
  #
  ## previous vector conversion
  B.init.vec.G <- convert_B(B.val = B.init.vec.mT,
                            B.unit = "mT",
                            B.2unit = "G")
  # Check the length
  expect_length(B.init.vec.G,Npoints)
  #
})
