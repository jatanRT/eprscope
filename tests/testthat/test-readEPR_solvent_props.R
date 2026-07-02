#
## ========================== Solvent Properties Tests ============================
#
# ----------------------------- CH3CN/ACN Properties ---------------------------
#
test_that("Properties of acatonitrile do match.", {
  #
  solv <- "acetonitrile"
  #
  # read properties of acetonitrile
  ACN.boiling.01 <-
    readEPR_solvent_props(solvent = solv) %>%
    dplyr::pull(Boiling_Point_oC)
  #
  ACN.boiling.02 <-
    readEPR_solvent_props(
      solvent = "acetonitrile",
      prop = "boiling"
    )
  #
  ACN.dielectric.01 <-
    readEPR_solvent_props(solvent = solv) %>%
    dplyr::pull(Dielectric_Const)
  #
  ACN.dielectric.02 <-
    readEPR_solvent_props(
      solvent = solv,
      prop = "dielectric"
    )
  # tests
  expect_equal(ACN.boiling.01,ACN.boiling.02)
  expect_equal(ACN.dielectric.01,ACN.dielectric.02)
})
#
# ---------------------------- Xylene Solvents ----------------------------
#
test_that("All xylene solvents are recognized.",{
  #
  solvx <- "xylene"
  #
  xylene.df <- readEPR_solvent_props("xylene")
  #
  # tests
  expect_equal(dim(xylene.df),c(3,10)) # dimension
  expect_equal(
    xylene.df$Solvent,
    c(paste0("o-",solvx),"m-xylene","p-xylene")
  ) # xylene names
  expect_equal(
    xylene.df$Formula,
    rep("C8H10",times = 3)
  ) # xylene formula
  expect_equal(
    xylene.df$Density_gmL,
    c(0.897,0.868,0.861)
  ) # xylene density
})
