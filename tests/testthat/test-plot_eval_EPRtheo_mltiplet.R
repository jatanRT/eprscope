test_that("Theoretical evaluation of EPR intensity multiplets works.", {
  #
  ## let's assume one of the two 14N nuclei (2 x) groups
  ## like for verdazyl radical (or two equivalent 14N in MV*+)
  intensity.coeffs.01 <-
    plot_eval_EPRtheo_mltiplet(
      nucle_us_i = "14N",
      N.nuclei = 2
    )
  #
  ## ...similarly, now via the "I"
  ## (nuclear spin quantum number) definition
  intensity.coeffs.02 <-
    plot_eval_EPRtheo_mltiplet(
      I = 1,
      N.nuclei = 2
    )
  #
  ## do they equal ? (vectors not plots) :-)
  expect_equal(
    intensity.coeffs.01$intensity.vec,
    intensity.coeffs.02$intensity.vec
  )
  #
  ## similar test now with 4 and 5 equivalent "14N" nuclei
  intensity.coeffs.14N.4x <-
    plot_eval_EPRtheo_mltiplet(
      nucle_us_i = "14N",
      N.nuclei = 4
    )
  #
  intensity.coeffs.14N.5x <-
    plot_eval_EPRtheo_mltiplet(
      nucle_us_i = "14N",
      N.nuclei = 5
    )
  #
  ## like before, test intensity coefficients
  ## see also e.g.:
  ## https://escholarship.org/content/qt2zx4r4mj/qt2zx4r4mj_noSplash_5d2d1d4b77f4141726ffc92bb0ac2be1.pdf
  ## and Brustolons: https://onlinelibrary.wiley.com/doi/book/10.1002/9780470432235 page 123
  ## however (in that book) for 5 equivalent 14N (I=1) the 4th and 8th coeff. must be 30 (not 20) !!
  expect_equal(
    unname(intensity.coeffs.14N.4x$intensity.vec),
    c(1,4,10,16,19,16,10,4,1)
  )
  ## check also their names/total spin quantum numbers
  ## from -N*I....N*I
  expect_equal(
    names(intensity.coeffs.14N.4x$intensity.vec),
    c("-4","-3","-2","-1","0","1","2","3","4")
  )
  ## the last test :-)
  expect_equal(
    unname(intensity.coeffs.14N.5x$intensity.vec),
    c(1,5,15,30,45,51,45,30,15,5,1)
  )
  #
})
