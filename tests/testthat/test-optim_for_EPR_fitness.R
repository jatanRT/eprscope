#
## =================== Tests for GENERIC OPTIMIZATION FUNCTION =================
#
## This function can be applied not only on EPR spectra, however for optimization
## in general, e.g. on UV-Vis (Gaussian) spectrum band/peak as evaluated below
#
#                    500 nm
#                    -|-
#                   /   \
#                  /     \
#                 /       \
#  400 nm        /         \         600 nm
# --|-----------/           \---------|----
#
## ---------------------- `Synthesis` of a Gaussian Peak ---------------------
#
## ## generate Gaussian peak UV-Vis spectrum
A.init <- 2
mu.init <- 500
sigma.init <- 15
#
set.seed(42)
wl <- seq(400, 600, by = 2) ## Wavelength
y  <- A.init * exp(- (wl - mu.init)^2/(2 * sigma.init^2)) +
  rnorm(length(wl), sd = 0.05) ## Intensity
#
## ...and the corresponding data frame
df.model.expr <- data.frame(
  Wavelength_nm = wl,
  Intensity = y
)
#
## ----------------------- Parametrization of Curves ------------------------
#
## While `{nloptr}` methods use `x0` parameter, `levenmarq` and the `pswarm`
## use the `par` parameter vector
#
## parametrize the fitting function
## `x` and `Intensity.fit` are column headers
Gaussian_Fit_x0 <- ## for the `x0` parameter vector
  function(data,x,Intensity.fit,x0) {
    data[[Intensity.fit]] <-
      x0[1] * exp(- (data[[x]] - x0[2])^2 / (2 * x0[3]^2))
    return(data[[Intensity.fit]])
  }
Gaussian_Fit_par <- ## for the `par` parameter vector
  function(data,x,Intensity.fit,par) {
    data[[Intensity.fit]] <-
      par[1] * exp(- (data[[x]] - par[2])^2 / (2 * par[3]^2))
    return(data[[Intensity.fit]])
  }
#
## ------------------------- `Fitness` Functions ------------------------------
## -------------------- Minimize the (squares) Residuals ----------------------
#
## fitness function for all nloptr functions
min_residuals_x0_nloptr <-
  function(data,x,Intensity.fit,x0) {
    sum(
      ## `"Intensity"`, see `df.model.expr`:
      (data[["Intensity"]] -
         Gaussian_Fit_x0(data,x,Intensity.fit,x0))^2
    )
  }
#
## fitness for "levenmarq"
min_residuals_par_levenmarq <-
  function(data,x,Intensity.fit,par) {
    data[["Intensity"]] -
      Gaussian_Fit_par(data,x,Intensity.fit,par)
  }
#
## fitness for particle swarm algorithm/method `pswarm`
min_residuals_par_pswarm <-
  function(data,x,Intensity.fit,par) {
    sum(
      ## `"Intensity"`, see `df.model.expr`:
      (data[["Intensity"]] -
         Gaussian_Fit_par(data,x,Intensity.fit,par))^2
    )
  }
#
## ----------------- Lists/Evaluations by Different Methods --------------------
#
## collect all `{nloptr}` methods
nloptr.methods <-
  c("slsqp","cobyla","lbfgs","neldermead","crs2lm","sbplx")
#
## fit/optimize in one loop (complex list) for all `{nloptr}`
optim.fit.nloptr.list <-
  lapply(
    nloptr.methods,
    function(l) {
      optim_for_EPR_fitness(
        method = l,
        x.0 = c(1.4,480,17),
        fn = min_residuals_x0_nloptr,
        lower = c(1.3,470,13),
        upper = c(2.2,520,18),
        data = df.model.expr,
        x = "Wavelength_nm",
        Intensity.fit = "Fit",
        Nmax.evals = 256
      )
    }
  )
#
## fit for the `pswarm` method
optim.fit.pswarm.list <-
  optim_for_EPR_fitness(
    method = "pswarm",
    x.0 = c(1.4,480,17),
    fn = min_residuals_par_pswarm,
    lower = c(1.3,470,13),
    upper = c(2.2,520,18),
    data = df.model.expr,
    x = "Wavelength_nm",
    Intensity.fit = "Fit",
    Nmax.evals = 512
  )
#
## fit for the `levenmarq` method
optim.fit.levenmarq.list <-
  optim_for_EPR_fitness(
    method = "levenmarq",
    x.0 = c(1.4,480,17),
    fn = min_residuals_par_levenmarq,
    lower = c(1.3,470,13),
    upper = c(2.2,520,18),
    data = df.model.expr,
    x = "Wavelength_nm",
    Intensity.fit = "Fit",
    Nmax.evals = 1024 ## maximum number for "levenmarq"
  )
#
## compare all with the essential `nls()` fit
## which, by default, applies `Gauss-Newton` optim. algorithm
optim.fit.nls <-
  stats::nls(
    Intensity ~ A * exp(- (Wavelength_nm - mu)^2/(2 * sigma^2)),
    data = df.model.expr,
    start = list(A = 1.4, mu = 480, sigma = 17)
  )
#
test_that("Optimized parameters of the Gaussian UV-Vis spectrum peak correspond to initial model ! ", {
  #
  ## best optimized parameters for all `{nloptr}` methods
  nloptr.all.best.params <- lapply(
    seq(optim.fit.nloptr.list),
    function(x) {
      optim.fit.nloptr.list[[x]]$par
    }
  )
  ## best optimized parameters parameters for the `pswarm`
  pswarm.best.params <- optim.fit.pswarm.list$par
  #
  ## best optimized parameters for the `levenmarq`
  levenmarq.best.params <- optim.fit.levenmarq.list$par
  #
  ## ...compared with the optimized/best by `nls()`
  nls.best.params <- unname(coef(optim.fit.nls))
  #
  #
  ## create data frame for all parameters and methods
  params.data.frame.all <-
    data.frame(
      A = c(
        sapply(seq(nloptr.all.best.params),
               function(i) {nloptr.all.best.params[[i]][1]}),
        pswarm.best.params[1],
        levenmarq.best.params[1],
        nls.best.params[1]
      ),
      mu = c(
        sapply(seq(nloptr.all.best.params),
               function(i) {nloptr.all.best.params[[i]][2]}),
        pswarm.best.params[2],
        levenmarq.best.params[2],
        nls.best.params[2]
      ),
      sigma = c(
        sapply(seq(nloptr.all.best.params),
               function(i) {nloptr.all.best.params[[i]][3]}),
        pswarm.best.params[3],
        levenmarq.best.params[3],
        nls.best.params[3]
      )
    )
  #
  ## Values for individual parameters (95 % confidence interval)
  A.param <-
    eval_interval_cnfd_tVec(params.data.frame.all$A)
  #
  mu.param  <-
    eval_interval_cnfd_tVec(params.data.frame.all$mu)
  #
  sigma.param <-
    eval_interval_cnfd_tVec(params.data.frame.all$sigma)
  #
  expect_equal(A.param[["value"]],A.init,tolerance = 0.1)
  expect_equal(mu.param[["value"]],mu.init,tolerance = 0.1)
  expect_equal(sigma.param[["value"]],sigma.init,tolerance = 0.1)
  #
})
#
## ------------------- Checking the returned valuescorresp. to min. RSS ----------------------
#
test_that("Returned list `values`, upon UV-Vis spectrum fitting, correspond to minimal RSS ! ",{
  #
  ## function fit values for the `{nloptr}` methods
  nloptr.all.best.values <- lapply(
    seq(optim.fit.nloptr.list),
    function(x) {
      optim.fit.nloptr.list[[x]]$value
    }
  )
  ## vector of all values
  nloptr.all.best.values <- sapply(
    seq(nloptr.all.best.values),
    function(x) { nloptr.all.best.values[[x]] }
  )
  ## function fit value for the `pswarm`
  pswarm.best.value <- optim.fit.pswarm.list$value
  #
  ## function fit value for the `levenmarq`
  levenmarq.best.value <- optim.fit.levenmarq.list$deviance
  #
  ## 95% confidence interval for `{nloptr}` and `pswarm`
  nloptr.value <-
    eval_interval_cnfd_tVec(
      c(nloptr.all.best.values,pswarm.best.value)
    )
  #
  expect_equal(nloptr.value[["value"]],0.29,tolerance = 0.1)
  expect_equal(levenmarq.best.value,1.22,tolerance = 0.1)
  #
})
#
## ------------------------- Checking the returned message -------------------------------
#
test_that(" Returned `message`, upon UV-Vis spectrum fitting by `NLOPTR`,
          contains strings characteristic for the succesful fitting
          process termination ! ",{
  #
  ## messages for the `{nloptr}`
  nloptr.all.messages <- lapply(
    seq(optim.fit.nloptr.list),
    function(x) {
      optim.fit.nloptr.list[[x]]$message
    }
  )
  ## check message content in all vectors
  check.message <-
    grepl("Optimization stopped.*(maxeval|xtol_).*reached",nloptr.all.messages)
  #
  expect_false(isFALSE(check.message)) ## FALSE because all are TRUE
  #
})
