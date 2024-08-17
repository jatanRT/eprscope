## constants required for the calculations
Planck.const <- constants::syms$h
nuclear.mu <- constants::syms$mun ## Nuclear magneton
Bohr.mu <- constants::syms$mub ## Bohr magneton
#
# ======================== EXPERIMENT ==========================
#
## Obtaining all three `B`s and `g`s for an experimental EPR spectrum
## of a nitroxide (aminoxyl radical) derivative. The `B.max` values,
## corresponding to `B`s with maximal intensity, are required for the
## evaluation of the hyperfine coupling/splitting (A_iso,a_iso, respectively)
#
# Find out the following values from the experimental EPR spectrum
#
# .      B.max[1]         B.max[2].        B.max[3]
#
#        /|               /|               /|
#   ----/ | /------------/ | /------------/ | /------
#         |/               |/               |/
#
#     B[1],g[1]       B[2],g[2]        B[3],g[3]
#
## first of all load the experimental file example
## to read the parameters
aminoxyl.rad.path.params.a <-
  load_data_example(file = "Aminoxyl_radical_a.DSC")
aminoxyl.rad.data.params.a <-
  readEPR_params_tabs(aminoxyl.rad.path.params.a)$params
## microwave frequency in GHz
mw.freq.GHz <- aminoxyl.rad.data.params.a %>%
  dplyr::filter(Parameter == "Frequency") %>%
  dplyr::pull(Value)
## central field in mT
B.cf.mT <- aminoxyl.rad.data.params.a %>%
  dplyr::filter(Parameter == "Central Field") %>%
  dplyr::pull(Value)
## sweep width in mT
B.sw.mT <- aminoxyl.rad.data.params.a %>%
  dplyr::filter(Parameter == "Sweep Width") %>%
  dplyr::pull(Value)
## number of points
No.points <- aminoxyl.rad.data.params.a %>%
  dplyr::filter(Parameter == "Number of Points") %>%
  dplyr::pull(Value)
## Q-value, sensitivity factor
Qvalue <- aminoxyl.rad.data.params.a %>%
  dplyr::filter(Parameter == "QValue") %>%
  dplyr::pull(Value)
#
## load the experimental data file
aminoxyl.rad.path.data.a <-
  load_data_example(file = "Aminoxyl_radical_a.txt")
aminoxyl.rad.data.a <-
  readEPR_Exp_Specs(aminoxyl.rad.path.data.a, qValue = Qvalue)
#
## evaluation of B, g
## the regions to get B,g can be obtained from the interactive EPR spectrum like =>
## `plot_EPR_Specs2D_interact(aminoxyl.rad.data.a)`
## from that spectrum, the following regions may be obtained =>
B.regions.mT <- list(c(346.5, 348.5), c(348.5, 350.5), c(350.5, 352.5))
## evaluating g
g <- sapply(B.regions.mT, function(i) {
  eval_gFactor_Spec(
    data.spectr = aminoxyl.rad.data.a,
    nu.GHz = mw.freq.GHz,
    B.unit = "mT",
    B = "B_mT",
    Blim = i
  )
})
# ...and the corresponding B in mT
B <- sapply(g, function(j) {
  ## B = h nu / mu_B g
  (Planck.const * mw.freq.GHz * 1e+9) / (j * Bohr.mu)
}) * 1e+3 ## in mT
#
g.iso <- g[2]
instrum.params <- c(
  Bcf = B.cf.mT, ## all these values are
  Bsw = B.sw.mT, ## already defined above
  Npoints = No.points,
  mwGHz = mw.freq.GHz
)
test_that("The `B` calculated by the 'Breit-Rabi' formula/function
          for an EPR simulation corresponds to experimental EPR spectrum.", {
  #
  # ======================== SIMULATION ==========================
  # ========= FIND THE CORRESPONDING `B`s by Breit-Rabi ==========
  # ====== AFTERWARDS, THEY MUST EQUAL TO EXPERIMENTAL `B` ======
  #
  ## The following code is taken from the `eval_sim_EPR_iso()`
  ## in order to test whether the same `B` values are obtained
  #
  B.unit <- "mT"
  ## the A_iso = 52.349 MHz from the expr. spectrum (see the `mean.A.iso.expr` below)
  nuclear.system <- list("14N", 1, 52.349)
  ## `nuclear.system` definition if `nuclear.system != NULL`
  if (!is.null(nuclear.system)) {
    ## check if the list is nested (several groups) or simple (only one group)
    nested_list <- any(sapply(nuclear.system, is.list))
    if (isFALSE(nested_list)) {
      ## redefinition of `nuclear.system` list to calculate the spectra without
      ## any additional conditions just by simple =>
      nuclear.system <- list(nuclear.system)
    }
    ## reordering the `nuclear.system` from the highest A_iso to the lowest one
    nuclear.system <- nuclear.system[order(sapply(nuclear.system, "[[", 3), decreasing = TRUE)]
    #
    ## extract list components and put them into vectors
    nucle_us_i <- sapply(1:length(nuclear.system), function(e) nuclear.system[[e]][[1]])
    N_nuclei <- sapply(1:length(nuclear.system), function(e) nuclear.system[[e]][[2]])
    A_iso_MHz <- sapply(1:length(nuclear.system), function(e) nuclear.system[[e]][[3]])
    ## take only absolute values of `A_iso_MHz` (common EPR experiments
    ## cannot determine the `A_iso_MHz` sign)
    A_iso_MHz <- abs(A_iso_MHz)
    #
    ## condition for the `A_iso` < nu.GHz(mwGHz)
    if (any((A_iso_MHz) * 1e-3 >= unname(instrum.params["mwGHz"]))) {
      stop(" Any of the `A_{iso}` values is higher than the MW Frequency !\n
           The isotropic EPR spectrum cannot be simulated ! ")
    }
  }
  ## Extracting instrumental parameter values from `.DSC` or `.par` files
  ## or from named numeric vector above
  if (is.null(instrum.params)) {
    stop(" Please, define `instrum.params` like central field, MW freq.,... ! ")
  } else {
    B.CF <- unname(instrum.params["Bcf"])
    B.SW <- unname(instrum.params["Bsw"])
    Npoints <- unname(instrum.params["Npoints"])
    nu.GHz <- unname(instrum.params["mwGHz"])
  }
  #
  ## Creating data frame (`B` + `g`) for the simulated B region
  B.g.sim.df <- data.frame(B = seq(B.CF - (B.SW / 2),
    B.CF + (B.SW / 2),
    length.out = Npoints
  ))
  colnames(B.g.sim.df) <- paste0("B_", B.unit)
  B.g.sim.df <- B.g.sim.df %>%
    dplyr::mutate(g = eval_gFactor(
      nu.val = nu.GHz,
      nu.unit = "GHz",
      B.val = .data[[paste0("B_", B.unit)]],
      B.unit = B.unit
    ))
  #
  ## Conversions (adding columns) for `B.g.sim.df`
  B.CF <- convert_B(B.CF, B.unit = B.unit, B.2unit = T)
  B.SW <- convert_B(B.SW, B.unit = B.unit, B.2unit = T)
  B.g.sim.df[["B_T"]] <- convert_B(B.g.sim.df[[paste0("B_", B.unit)]],
    B.unit = B.unit,
    B.2unit = "T"
  )
  B.g.sim.df[["B_mT"]] <- convert_B(B.g.sim.df[[paste0("B_", B.unit)]],
    B.unit = B.unit,
    B.2unit = "mT"
  )
  B.g.sim.df[["B_G"]] <- convert_B(B.g.sim.df[[paste0("B_", B.unit)]],
    B.unit = B.unit,
    B.2unit = "G"
  )
  #
  ## additional `nuclear.system` definition if `nuclear.system != NULL`
  if (!is.null(nuclear.system)) {
    ## Look up for Nuclei (for several groups of equivalent nuclei)
    ## nuclear spin
    spin_nuclear <- sapply(
      nucle_us_i,
      function(y) {
        eprscope::isotopes_ds %>%
          dplyr::filter(.data$Isotope == y) %>%
          dplyr::pull(.data$Spin)
      }
    )
    spin_nuclear <- unname(spin_nuclear)
    ## nuclear g
    g_nuclear <- sapply(
      nucle_us_i,
      function(y) {
        eprscope::isotopes_ds %>%
          dplyr::filter(.data$Isotope == y) %>%
          dplyr::pull(.data$g_Nuclear)
      }
    )
    g_nuclear <- unname(g_nuclear)
    ## abundance
    abund_nuclear <- sapply(
      nucle_us_i,
      function(y) {
        eprscope::isotopes_ds %>%
          dplyr::filter(.data$Isotope == y) %>%
          dplyr::pull(.data$Abund_Natur_Percent)
      }
    )
    abund_nuclear <- unname(abund_nuclear)
    abund_nuclear <- abund_nuclear / 100
    #
    ## magnetic spin quantum numbers of all nuclear groups in one list
    ## this is required to calculate Breit-Rabi energies/frequencies/Bs (see below)
    ## (2*N*I + 1) values going from  - N*I ...to...+ N*I
    m_spin_values <- Map(
      function(z, w) {
        sapply(1:(2 * z * w + 1), function(q) -(z * w) + q - 1)
      },
      N_nuclei,
      spin_nuclear
    )
  }
  #
  ## QM function to calculate the delta spin delta energies / frequencies (in MHz) / B (in T)
  ## according to Breit-Rabi => see J. Magn. Reson. https://doi.org/10.1016/0022-2364(71)90049-7,
  ## WEIL, J. A. and J. Magn. Reson. https://doi.org/10.1016/j.jmr.2005.08.013, STOLL, S.
  ## formula corresponding to hyperfine interaction with one unpaired electron,
  ## where the nucleus-nucleus interaction cross-terms are neglected.
  ## According to above-referenced theory the following condition must be fulfilled
  if (all((spin_nuclear + 0.5) * A_iso_MHz >= 200 * nu.GHz)){
    stop(" The Breit-Rabi Energy/Frequency/B calculations\n
         cannot be used to predict the EPR spectra ! ")
  }
  fun_breit_rabi <- function(A_iso, ## in energy units NOT in MHz <-> convert into energy (A * h)
                             B.0, ## in Tesla
                             g_nuclear,
                             g_e_iso,
                             spin_nuclear,
                             m_spin_nuclear) {
    #
    ## definition of variable alpha
    alpha <- ((g_e_iso * Bohr.mu * B.0) + (g_nuclear * nuclear.mu * B.0)) /
      (A_iso * (spin_nuclear + 0.5))
    ## Energies depending on lower (1) or higher state (2)
    E1 <- A_iso / 4 - (g_nuclear * nuclear.mu * B.0) * (m_spin_nuclear - 0.5) -
      (spin_nuclear + 0.5) * (A_iso / 2) *
        sqrt(1 + ((2 * (m_spin_nuclear - 0.5)) / (spin_nuclear + 0.5)) * alpha + alpha^2)
    #
    E2 <- A_iso / 4 - (g_nuclear * nuclear.mu * B.0) * (m_spin_nuclear + 0.5) +
      (spin_nuclear + 0.5) * (A_iso / 2) *
        sqrt(1 + ((2 * (m_spin_nuclear + 0.5)) / (spin_nuclear + 0.5)) * alpha + alpha^2)
    #
    ## Delta E for individual line
    DeltaE <- E2 - E1 ## in J
    #
    Freq_corresp <- round((DeltaE / Planck.const) * 1e-6, digits = 3) ## in MHz
    #
    ## The corresponding `B` (to `DeltaE`) cannot be evaluated analytically,
    ## because we do not know the corresponding line g-value => therefore the `B`
    ## will be evaluated by an iterative manner as already shown in
    ## https://doi.org/10.1016/0022-2364(71)90049-7 (WEIL, J. A.) as well as
    ## https://doi.org/10.1016/j.jmr.2005.08.013 (STOLL, S) where after 2-4 iterations
    ## value converges to the resonant field
    #
    ## We start from the new `B.mI` vector and in the first approximation,
    ## the corresponding `B` (to `DeltaE`) can be evaluated as =>
    B.mI <- c()
    B.mI[1] <- round(DeltaE / (g_e_iso * Bohr.mu), digits = 7) ## in T
    ## cycle through 4 iterations =>
    xaj <- c()
    gamma <- (Bohr.mu * g_e_iso) + (nuclear.mu * g_nuclear)
    for (i in 1:4){
      xaj[i] <- (A_iso / (2 * Planck.const)) / ((nu.GHz * 1e+9) +
                (nuclear.mu * g_nuclear * (B.mI[i] / Planck.const)))
      B.mI[i+1] <- (A_iso / (gamma * (1 - xaj[i]^2))) *
        (- m_spin_nuclear + sqrt(m_spin_nuclear^2 + (1 - xaj[i]^2) *
        ((1 / (4 * xaj[i]^2)) - (spin_nuclear + 0.5)^2)))
    }
    #
    ## all three quantities into one list (B as a last value from the iterations)
    return(list(D_E = DeltaE, nu = Freq_corresp, B = B.mI[length(B.mI)]))
  }
  #
  ## LOOKING FOR g_iso & B_iso within SIM. DATA FRAME (`B.g.sim.df`)
  #
  ## condition to find g_iso
  if (is.null(g.iso)) {
    near_g_iso <- which.min(abs(B.g.sim.df$B_T - B.CF)) ## find index/row
  } else {
    near_g_iso <- which.min(abs(B.g.sim.df$g - g.iso))
  }
  near_g_iso_df <- B.g.sim.df[near_g_iso, ] ## data.frame with the corresponding index
  ## and finally
  g_iso <- near_g_iso_df %>% dplyr::pull(.data$g)
  ## corresponding actual `B_iso`
  B_iso <- near_g_iso_df %>% dplyr::pull(.data$B_T)
  #
  ## `B` for the biggest A_iso
  B_for_m_spin_values1 <-
    sapply(1:length(m_spin_values[[1]]), function(f) {
      fun_breit_rabi(
        A_iso = A_iso_MHz[1] * 1e+6 * Planck.const,
        B.0 = B_iso,
        g_nuclear = g_nuclear[1],
        g_e_iso = g_iso,
        spin_nuclear = spin_nuclear[1],
        m_spin_nuclear = m_spin_values[[1]][f]
      )$B
    })
  ## finding the indices in simulation df (data frame) => `B.g.sim.df` corresp. to B values
  near_row_for_m_spin_values1 <-
    sapply(
      1:length(B_for_m_spin_values1),
      function(l) {
        which.min(abs(B.g.sim.df$B_T - B_for_m_spin_values1[l]))
      }
    )
  ## selecting rows based on previous indices
  near_B_for_m_spin_values1 <-
    B.g.sim.df %>%
    dplyr::slice(near_row_for_m_spin_values1)
  #
  # ====================== END OF `eval_sim_EPR_iso()` CODE ======================
  #
  #
  # ========= COMPARISON BETWEEN BREIT-RABI and EXPERIMENTAL `B`,`g` =============
  #
  expect_equal(rev(B), near_B_for_m_spin_values1$B_mT, tolerance = 1e-2) ## `B` in mT
  expect_equal(rev(g), near_B_for_m_spin_values1$g, tolerance = 1e-4) ## `g`
})
#
# ===================================================================================
#
## additional tests linewidth (Delta_B) and A evaluation from the simulated spectrum
#
test_that("The isotropic hyperfine coupling constants determined
          from the experimental and simulated spectrum equal.", {
  #
  ## Determination of the A_iso from the experimental EPR spectrum =>
  ## first of all all `B`s with maximal intensity should be evaluated =>
  ## the same `B` regions as above can be used
  B.max <- sapply(B.regions.mT, function(k) {
    eval_extremeX_Spec(
      data.spectr = aminoxyl.rad.data.a,
      xlim = k
    )
  })
  #
  ## now calculate both `B.max[2] - B.max[1]` as well as `B.max[3] - B.max[2]`
  ## as also depicted in the scheme above (at the beginning of this test script)
  ## the g.iso value corresponds to g[2] above
  A.iso.expr <- c() ## in MHz
  for (l in 1:(length(B.max) - 1)) {
    A.iso.expr[l] <-
      convert_a_mT_2A(B.max[l + 1] - B.max[l],
        g.val = g[2]
      )
  }
  ## ... and the mean value
  mean.A.iso.expr <- round(mean(A.iso.expr), 2) ## the mean value is 52.349 MHz
  #
  ## To simulate that EPR spectrum also the line width is required =>
  DeltaB.expr <- c() ## in mT
  for (m in seq(B.regions.mT)) {
    DeltaB.expr[m] <-
      eval_DeltaXpp_Spec(
        data.spectr = aminoxyl.rad.data.a,
        xlim = B.regions.mT[[m]]
      )
  }
  ## ...and the mean value
  mean.DeltaB.expr <- round(mean(DeltaB.expr), 2) ## the mean value equals to 0.541 mT
  #
  ## EPR Spectrum Simulation, the instrumental parameters are already
  ## defined above
  aminoxyl.rad.data.sim.a <-
    eval_sim_EPR_iso(
      g.iso = g[2],
      instrum.params = instrum.params,
      B.unit = "mT",
      nuclear.system = list("14N", 1, 52.349),
      lineGL.DeltaB = list(mean.DeltaB.expr, NULL)
    )$df
  #
  ## Determination of the A_iso from the simulated EPR spectrum =>
  ## first of all all `B`s with maximal intensity should be evaluated =>
  ## the same `B` regions as above can be used
  Bsim.max <- sapply(B.regions.mT, function(k) {
    eval_extremeX_Spec(
      data.spectr = aminoxyl.rad.data.sim.a,
      x = "Bsim_mT",
      Intensity = "dIeprSim_over_dB",
      xlim = k
    )
  })
  #
  ## now calculate both `Bsim.max[2] - Bsim.max[1]` as well as `Bsim.max[3] - Bsim.max[2]`
  ## as also depicted in the scheme above (at the beginning of this test script)
  ## the g.iso value corresponds to g[2] above
  A.iso.sim <- c() ## in MHz
  for (l in 1:(length(Bsim.max) - 1)) {
    A.iso.sim[l] <-
      convert_a_mT_2A(Bsim.max[l + 1] - Bsim.max[l],
        g.val = g[2]
      )
  }
  ## ... and the mean value
  mean.A.iso.sim <- round(mean(A.iso.sim), 2) ## the mean value is 52.208 MHz
  #
  ## The linewidth from the simulated spectrum
  DeltaB.sim <- c() ## in mT
  for (m in seq(B.regions.mT)) {
    DeltaB.sim[m] <-
      eval_DeltaXpp_Spec(
        data.spectr = aminoxyl.rad.data.sim.a,
        x = "Bsim_mT",
        Intensity = "dIeprSim_over_dB",
        xlim = B.regions.mT[[m]]
      )
  }
  ## ...and the mean value
  mean.DeltaB.sim <- round(mean(DeltaB.sim), 2) ## the mean value equals to 0.560 mT
  #
  # ====== COMPARISON BETWEEN SIMULATED and EXPERIMENTAL `DeltaB`,`A.iso` ======
  #
  expect_equal(abs(mean.DeltaB.sim - mean.DeltaB.expr), 0.02, tolerance = 1e-2) # `B` diff. in mT
  expect_equal(mean.A.iso.sim,mean.A.iso.expr,tolerance = 1e-1) ## `A.iso` in MHz
  #
  ## the DeltaB` can be also obtained from the simulation fit,
  ## see `test-eval_sim_EPR_isoFit`
})
#
# ===================================================================================
#
## Simulation of phenalenyl (or PNT) radical in order to check the simulation
## intensity pattern, the EPR spectrum can be also compared with that published
## elsewhere https://pubs.rsc.org/en/content/articlelanding/2006/CS/b500509b
#
## Schematically, the "first half" of the EPR spectrum intensities theoretically
## should look like the following one =>
#
# ----- 1:3:3:1 -- 6:18:18:6 -- 15:45:45:15 -- 20:60:60:20 -- ...second spectrum "half"
#
## the remaining pattern corresponds to =>
# 15:45:45:15 -- 6:18:18:6 -- 1:3:3:1 ------
## ...because there is an interaction of the unpaired electron with =>
## 3 x 1H (5.09 MHz / 1.8 G) and 6 x 1H (17.67 MHz / 6.3 G) =>
## total number of lines ((2 * 3 * 0.5) + 1) * ((2 * 6 * 0.5) + 1) = 28
#
test_that("The isotropic intensity pattern for the simulated EPR spectrum
          of phenylalenyl radical matches the theoretical one.", {
  #
  ## obtain the simulation corresponding data frame
  phenylalenyl.rad.data.sim.a <-
    eval_sim_EPR_iso(
      g.iso = 2.0027,
      B.unit = "G",
      nuclear.system = list(
        list("1H", 3, 5.09),
        list("1H", 6, 17.67)
      ),
      lineGL.DeltaB = list(0.42, NULL)
    )$df
  #
  ## Similarly like in the case of nitroxide => the following `B` regions
  ## for individual lines are selected (from the interactive spectrum plot),
  ## corresponding to first three patterns from the intensity scheme above =>
  B.regions.pnt.G <-
    list(
      c(3473.669, 3475.232), c(3475.232, 3477.284), c(3477.284, 3478.945), c(3478.945, 3480.410),
      c(3480.410, 3481.681), c(3481.681, 3483.537), c(3483.537, 3485.296), c(3485.296, 3486.761),
      c(3486.761, 3487.934), c(3487.934, 3489.790), c(3489.790, 3491.646), c(3491.646, 3493.112)
    )
  #
  ## Obtaining of all the maxima =>
  pnt.rad.data.maxima.sim.a <- c()
  for (n in seq(B.regions.pnt.G)) {
    pnt.rad.data.maxima.sim.a[n] <-
      phenylalenyl.rad.data.sim.a %>%
      dplyr::filter(dplyr::between(
        Bsim_G,
        B.regions.pnt.G[[n]][1],
        B.regions.pnt.G[[n]][2]
      )) %>%
      dplyr::filter(dIeprSim_over_dB == max(dIeprSim_over_dB)) %>%
      dplyr::pull(dIeprSim_over_dB)
  }
  #
  ## Obtaining of all the minima =>
  pnt.rad.data.minima.sim.a <- c()
  for (n in seq(B.regions.pnt.G)) {
    pnt.rad.data.minima.sim.a[n] <-
      phenylalenyl.rad.data.sim.a %>%
      dplyr::filter(dplyr::between(
        Bsim_G,
        B.regions.pnt.G[[n]][1],
        B.regions.pnt.G[[n]][2]
      )) %>%
      dplyr::filter(dIeprSim_over_dB == min(dIeprSim_over_dB)) %>%
      dplyr::pull(dIeprSim_over_dB)
  }
  #
  ## Because the linewidth and the form are the same for all lines
  ## => in the first approach the entire individual intensities
  ## can be calculated as =>
  pnt.rad.data.diffMaxMin.sim.a <-
    pnt.rad.data.maxima.sim.a - pnt.rad.data.minima.sim.a
  #
  ## Now we can divide all the intensities by the first value to obtain
  ## the pattern
  pnt.rad.data.intPattern.sim.a <-
    pnt.rad.data.diffMaxMin.sim.a / pnt.rad.data.diffMaxMin.sim.a[1]
  #
  ## ...and in ideal case such pattern should correspond to the theoretical one
  pnt.rad.data.intPattern.theo.a <-
    c(1, 3, 3, 1, 6, 18, 18, 6, 15, 45, 45, 15)
  #
  ## However, the partial overlay of the lines at the margin of each 1:3:3:1 pattern
  ## causes that the maximum residual is 0.7053877 and 0.2840122 between
  ## the 2nd and 3rd pattern and 0.2679887 at the end of the 3rd 1:3:3:1 pattern.
  ## In all other cases the residuals fall into the range of (0,8.05e-2).
  #
  pnt.rad.data.intCheck.sim.a <-
    abs(pnt.rad.data.intPattern.theo.a - pnt.rad.data.intPattern.sim.a)
  ##
  # ===== THE DERIV. SIM. INTESITY PATTERN ALMOST MATCHES THE THEORETICAL ONE =======
  #
  ## Therefore, checking the max. residual value and the relative errors in '%' =>
  expect_true(all(pnt.rad.data.intCheck.sim.a < 0.706)) # residuals
  expect_equal((0.706 / pnt.rad.data.intPattern.theo.a[8]) * 100,
    11.77, # rel.error in %
    tolerance = 1e-2
  )
  expect_equal((0.29 / pnt.rad.data.intPattern.theo.a[9]) * 100,
    1.93, # rel.error in %
    tolerance = 1e-2
  )
  expect_equal((0.27 / pnt.rad.data.intPattern.theo.a[12]) * 100,
    1.8, # rel.error in %
    tolerance = 1e-2
  )
  #
  ## Adding similar test for the INTEGRATED INTENSITIES
  phenylalenyl.rad.data.sim.b <-
    eval_sim_EPR_iso(
      g.iso = 2.0027,
      B.unit = "G",
      nuclear.system = list(
        list("1H", 3, 5.09),
        list("1H", 6, 17.67)
      ),
      lineSpecs.form = "integrated",
      Intensity.sim = "IeprSim",
      lineGL.DeltaB = list(0.42, NULL)
    )$df
  #
  ## Obtaining of all the maxima =>
  pnt.rad.data.maxima.sim.b <- c()
  for (n in seq(B.regions.pnt.G)) {
    pnt.rad.data.maxima.sim.b[n] <-
      phenylalenyl.rad.data.sim.b %>%
      dplyr::filter(dplyr::between(
        Bsim_G,
        B.regions.pnt.G[[n]][1],
        B.regions.pnt.G[[n]][2]
      )) %>%
      dplyr::filter(IeprSim == max(IeprSim)) %>%
      dplyr::pull(IeprSim)
  }
  #
  ## Now we can divide all the intensities by the first value to obtain
  ## the pattern
  pnt.rad.data.intPattern.sim.b <-
    pnt.rad.data.maxima.sim.b / pnt.rad.data.maxima.sim.b[1]
  #
  ## Residuals
  pnt.rad.data.intCheck.sim.b <-
    abs(pnt.rad.data.intPattern.theo.a - pnt.rad.data.intPattern.sim.b)
  #
  # ===== THE INTEGRATED SIM. INTESITY PATTERN MATCHES THE THEORETICAL ONE =======
  #
  ## all residuals are lower that 1.005e-3 and the highest relative error in '%' =>
  expect_true(all(pnt.rad.data.intCheck.sim.b < 1.005e-3)) # residuals
  expect_equal(
    (1.005e-3 / pnt.rad.data.intPattern.theo.a[8]) * 100,
    0.01675 # rel. error in %
  )
  #
})
