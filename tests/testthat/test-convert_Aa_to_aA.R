test_that("The A <--> a conversion works as expected.", {
  #
  ## expected g-value =>
  g_iso = 2.0027
  #
  ## the following splitting/cpoupling constants are taken
  ## from the EPR spectrum of phenylalenyl radical,
  ## see also https://pubs.rsc.org/en/content/articlelanding/2006/CS/b500509b
  a.lower.G <- convert_A_MHz_2a(5.09,g.val = g_iso) * 10
  A.higher.MHz <- convert_a_mT_2A(0.63,g.val = g_iso)
  #
  ## checking =>
  expect_equal(a.lower.G,1.8)
  expect_equal(A.higher.MHz,17.659)
  #
})
