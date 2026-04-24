
#
## ============ Tests Radical (R) Kinetic Models using ODE by `deSolve` 📦 ===============
#
## ------------------ Consrvation of mass for the consecutive reactions ------------------
#
test_that("Conservation of mass for the consecutive reactions does work !", {
  #
  ## initial quantitative variables for "A" and "R" (+ "B")
  qvar.init.A <- 0.02 # A0
  qvar.init.R <- qvar.init.A / 10 # R0
  qvar.init.B <- 0 # B0
  #
  kin.test.consecutive <-
    eval_kinR_ODE_model(
      model.react = "(a=1)A<==>[k1][k4](r=1)R<==>[k2][k3](b=1)B",
      kin.params = c(
        k1 = 0.025,
        k2 = 0.01,
        k3 = 0,
        k4 = 0,
        qvar0A = qvar.init.A,
        qvar0R = qvar.init.R,
        qvar0B = qvar.init.B
      )
    )
  #
  ## for all time points conservation of mass must be fulfilled
  ## A0 + R0 = A + R + B in the case of 1:1:1 stoichiometry,
  ## at first create vector of total concentration/quantitative variable
  vec.total.qvar.init <-
    rep(
      (qvar.init.A + qvar.init.R),
      times = nrow(kin.test.consecutive$df)
    )
  #
  ## total qvar/conc for all time points/rows of data frame
  ## 'rowwise' sum
  vec.total.qvar.df <- rowSums(kin.test.consecutive$df[,c("A","R","B")])
  #
  ## comparison
  expect_equal(vec.total.qvar.df,vec.total.qvar.init)
  #
})
#
## ------------------- Stoichimetric Coefficients ----------------------
#
test_that("Stoichiometric coefficient of the elementar reaction do match ! ",{
  #
  ## function - extract stoichiometric coeffs.
  stoichiom_coeff <- function(expression, coeff = "r") {
    if (coeff == "r") {
      rabc.string <- stringr::str_extract(expression, pattern = "\\(r=[[:digit:]]\\)")
    }
    if (coeff == "a") {
      rabc.string <- stringr::str_extract(expression, pattern = "\\(a=[[:digit:]]\\)")
    }
    if (coeff == "b"){
      rabc.string <- stringr::str_extract(expression, pattern = "\\(b=[[:digit:]]\\)")
    }
    model.react.rabc <- stringr::str_extract(rabc.string, pattern = "[[:digit:]]")
    model.react.rabc <- as.numeric(model.react.rabc)
    #
    return(model.react.rabc)
    #
  }
  #
  ## model reaction (character string) and the corresponding coeffs.
  model.react <- "(r=2)R <==> [k1] [k2] (b=1)B"
  b <- stoichiom_coeff(expression = model.react,coeff = "b")
  r <- stoichiom_coeff(expression = model.react,coeff = "r")
  #
  ## comparison
  expect_equal(2 * b,r)
  #
})
#
## ------------------- Analytical vs Numeric ODE Solution -----------------
#
test_that("Numeric ODE solution of 2nd-order kinetic model profile matches that of the analytical one !",{
  #
  ##
  kin.test.2nd.order <-
    eval_kinR_ODE_model(
      model.react = "(r=2)R --> [k1] B",
      kin.params = c(k1 = 0.012,
                     qvar0R = 0.08)
    )
  #
  kin.test.2nd.order.df <- kin.test.2nd.order$df
  kin.test.2nd.order.df <-
    kin.test.2nd.order.df %>%
    ## qvarR = qvar0R * (1 / (2 * qvar0R * k1 * t + 1))
    dplyr::mutate(R_anal = 0.08 * (1 / (2 * 0.08 * 0.012 * time + 1)))
  #
  expect_equal(
    kin.test.2nd.order.df$R,
    kin.test.2nd.order.df$R_anal,
    tolerance = 1e-7
  )
  #
})
