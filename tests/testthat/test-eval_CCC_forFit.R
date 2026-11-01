#
## ========================== Test for Concordance Correlations ============================
#
## Simulation Fit of the Experimental Aminoxyl EPR Spectrum
#
## load experimental EPR spectrum of an aminoxyl radical
aminoxyl.rad.data.path <-
  load_data_example(file = "Aminoxyl_radical_a.DTA")
#
aminoxyl.rad.data <- readEPR_Exp_Specs(
       path_to_file = aminoxyl.rad.data.path,
       qValue = 2100
    )
#
## own simulation fit
tempo.test.sim.fit <-
  eval_sim_EPR_isoFit(
    data.spectr.expr = aminoxyl.rad.data,
    nu.GHz = 9.806769,
    lineG.content = 0.75,
    optim.method = "sbplx",
    nuclear.system.noA = list("14N",1),
    baseline.correct = "constant",
    optim.params.init = c(2.0062,4.8,4.8,0,8e-3,52.68),
    optim.params.lower = c(2.0048,4.6,4.6,-1e-4,5e-3,52.68),
    optim.params.upper = c(2.0068,5.0,5.0,1e-4,1.5e-2,52.68),
    Nmax.evals = 128,
    msg.optim.progress = FALSE
  )
#
## vectors for comparison
x.vec <- tempo.test.sim.fit$ra$df$Experiment
y.vec <- tempo.test.sim.fit$ra$df$Simulation
#
## setting up the number of digits, general
## definition for all variables =>
options(digits = 9)
#
## check both x,y vectors, having their counterpart,
## only `complete.cases` are evaluated
ok <- stats::complete.cases(x.vec, y.vec)
x.vec <- x.vec[ok]
y.vec <- y.vec[ok]
#
## vector of King's `delta` exponent
delta <- c(1,1.5, 2)
#
## calculation of `CCC` (King, Chinchilli & Vallejos)
## https://doi.org/10.1002/env.70117
## https://doi.org/10.1081/BIP-100107651
## https://www.jstor.org/stable/2532051
## for the entire `delta` vector the last `CCC`
## should actually correspond to Lin's one,
## see below
rho.CCC <- sapply(
  delta,
  function(d) {
    #
    ## calculate `E_I` cross-pairs
    E_I <- abs(outer(x.vec,y.vec,"-"))^d %>% mean()
    #
    ## calculate `E_F` matched pairs
    E_F <- abs(x.vec - y.vec)^d %>% mean()
    #
    ## calculate CCC
    1 - (E_F / E_I)
  }
)
#
## calculation of the Lin's CCC (also incl. Pearson)
lin.CCC <- function(a = x.vec,b = y.vec) {
  #
  mx <- mean(a)
  my <- mean(b)
  sx2 <- mean((a - mx)^2)
  sy2 <- mean((b - my)^2)
  sxy <- mean((a - mx) * (b - my))
  #
  ## Pearson Correlation
  r <- sxy / (sqrt(sx2) * sqrt(sy2))
  #
  ## Lin's Correlation
  ccc <- 2 * sxy / (sx2 + sy2 + (mx - my)^2)
  #
  result.list <- list(
    CCC = ccc,
    Cb = ccc / r ## bias correction factor (see below)
  )
  #
  return(result.list)
  #
}
#
## generating the final data frame
df.CCC <- data.frame(
  d1 = rho.CCC[1],
  d15 = rho.CCC[2],
  d2 = rho.CCC[3],
  lin = lin.CCC()$CCC
)
## renaming the columns
colnames(df.CCC) <- c(
  "King's exponent \u03B4 = 1",
  "King's exponent \u03B4 = 1.5",
  "King's exponent \u03B4 = 2 (Approaches Lin's CCC)",
  "Lin's CCC"
)
#
test_that("Lin's concordance coefficients do match regardles of method. ", {
  #
  ## third and fourth value should be the same
  expect_equal(df.CCC[1,3],df.CCC[["Lin's CCC"]])
  #
  ## the first two values should be higher than 0.9
  expect_gt(df.CCC[["King's exponent \u03B4 = 1"]],0.9)
  expect_gt(df.CCC[1,2],0.9)
  #
})
#
test_that("Pearson's coerrelation coefficients do match. ",{
  #
  ## Pearson's correlation from fit (Simulation - 2nd column)
  r <- lin.CCC()$CCC / lin.CCC()$Cb
  r.from.df <- tempo.test.sim.fit$cor.df()[1,2]
  expect_equal(r,r.from.df)
  #
})
#
test_that("No Bland-Altman plot is displayed/provided by default. ",{
  #
  list.ccc.from.func <- eval_CCC_forFit(x.vec = x.vec,y.vec = y.vec)
  #
  expect_null(list.ccc.from.func$baplot)
  #
})
