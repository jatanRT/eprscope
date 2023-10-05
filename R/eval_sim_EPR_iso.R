#
#' Simulation of Isotropic EPR Spectra
#'
#'
#' @family Evaluations and Quantum Chemistry
#'
#'
#' @description
#' A short description...tbc...of the EPR simulation
#'
#'
#'
#' @param g.iso Numeric value, guess of the isotropic \eqn{g}-factor. It may also possess a `NULL`
#'   (\code{g.iso = NULL}) in case of the `B.CF` corresponding \eqn{g} is equal to `g.iso`. \strong{Default}:
#'   \code{g.iso = 2.00232} (the approximate \eqn{g} of the free electron).
#' @param B.CF Numeric value of the "\strong{c}entral \strong{f}ield" i.e. the central position
#'   of magnetic flux density \eqn{B} within the spectral sweep => see also argument \code{B.SW}.
#'   \code{B.CF} UNIT (the corresp. B.CF value) MUST BE EQUAL TO \code{B.unit} !
#' @param B.SW Numeric value equal to the \eqn{B} region (\eqn{B.CF - B.SW / 2}, eqn{B.CF + B.SW / 2})
#'   of the spectral "\strong{s}weep \strong{w}idth". \code{B.SW} UNIT (the corresp. B.CF value) MUST
#'   BE EQUAL TO \code{B.unit} !
#' @param B.unit Character string pointing to unit of magnetic flux density which is to be presented
#'   on \eqn{B} abscissa of the EPR spectrum, like \code{"G"} (`Gauss`) or \code{"mT"} (`millitesla`),
#'   \strong{default}: \code{B.unit = "G"}. THE UNIT MUST BE SHARED ACROSS ALL B ARGUMENTS like \code{B.CF},
#'   \code{B.SW} AND \code{lineGL.DeltaBpp} !
#' @param Npoints Numeric value corresponding to number of points of the entire \code{B.SW}
#'   (\eqn{\equiv } resolution).
#' @param nu.GHz Numeric value equal to either hypothetical or applied microwave frequency in \strong{GHz}
#'   to record/simulate the EPR spectrum. \strong{Default}: \code{nu.GHz = 9.8} (usually applied X-band frequency).
#' @param nuclear.system List containing the information about groups of equivalent nuclei
#'   interacting with the unpaired electron, like \code{nuclear.system} ...tbc...if only one,
#'   it could be either nested or simple.
#' @param natur.abund ...tbc...
#' @param lineGL.DeltaBpp ...tbc...
#' @param lineGL.content ...tbc...
#' @param Intensity.sim ...tbc...
#' @param plot.sim.interact description ...tbc...
#'
#'
#' @return ...list...tbc or interactive simulated `plotly` spectrum
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#'
#' @export
#'
#'
eval_sim_EPR_iso <- function(g.iso = 2.00232,
                             B.CF,
                             B.SW,
                             B.unit = "G",
                             Npoints,
                             nu.GHz = 9.8,
                             nuclear.system = NULL,
                             natur.abund = FALSE,
                             lineGL.DeltaBpp = list(1,NULL),
                             lineGL.content = list(1,NULL),
                             Intensity.sim = "dIeprSim_over_dB",
                             plot.sim.interact = FALSE){
  #
  ## Constants
  Planck.const <- constants::syms$h
  nuclear.mu <- constants::syms$mun ## Nuclear magneton
  Bohr.mu <- constants::syms$mub ## Bohr magneton
  #
  ## `nuclear.system` definition if `nuclear.system != NULL`
  if (!is.null(nuclear.system)){
    ## check if the list is nested (several groups) or simple (only one group)
    nested_list <- any(sapply(nuclear.system, is.list))
    if (isFALSE(nested_list)){
      ## redefinition of `nuclear.system` list to calculate the spectra without
      ## any additional conditions
      nuclear.system <- list(nuclear.system)
    }
    ## reordering the `nuclear.system` from the highest A_iso to the lowest one
    nuclear.system <- nuclear.system[order(sapply(nuclear.system,"[[",3),decreasing = TRUE)]
    #
    ## similarly like for simple list:
    nucle_us_i <- sapply(1:length(nuclear.system), function(e) nuclear.system[[e]][[1]])
    N_nuclei <- sapply(1:length(nuclear.system), function(e) nuclear.system[[e]][[2]])
    A_iso_MHz <- sapply(1:length(nuclear.system), function(e) nuclear.system[[e]][[3]])
  }
  #
  ## Data frame (`B` + `g`) for the simulated B region
  B.g.sim.df <- data.frame(B = seq(B.CF - B.SW / 2,B.CF + B.SW / 2,length.out = Npoints))
  colnames(B.g.sim.df) <- paste0("B_",B.unit)
  B.g.sim.df <- B.g.sim.df %>%
    dplyr::mutate(g = eval_gFactor(nu = nu.GHz,
                                   nu.unit = "GHz",
                                   B = .data[[paste0("B_",B.unit)]],
                                   B.unit = B.unit))
  #
  ## Conversions (adding columns) for `B.g.sim.df`
  B.CF <- convert_B(B.CF,B.unit = B.unit,B.2unit = T)
  B.SW <- convert_B(B.SW,B.unit = B.unit,B.2unit = T)
  B.g.sim.df[["B_T"]] <- convert_B(B.g.sim.df[[paste0("B_",B.unit)]],
                                   B.unit = B.unit,
                                   B.2unit = "T")
  B.g.sim.df[["B_mT"]] <- convert_B(B.g.sim.df[[paste0("B_",B.unit)]],
                                    B.unit = B.unit,
                                    B.2unit = "mT")
  B.g.sim.df[["B_G"]] <- convert_B(B.g.sim.df[[paste0("B_",B.unit)]],
                                   B.unit = B.unit,
                                   B.2unit = "G")
  #
  ## additional `nuclear.system` definition if `nuclear.system != NULL`
  if (!is.null(nuclear.system)){
    ## Look up for Nuclei (for several groups of equivalent nuclei)
    ## nuclear spin
    spin_nuclear <- sapply(nucle_us_i,
                           function(y)
                             eprscope::isotopes_ds %>%
                             dplyr::filter(.data$Isotope == y) %>%
                             dplyr::pull(.data$Spin))
    spin_nuclear <- unname(spin_nuclear)
    ## nuclear g
    g_nuclear <- sapply(nucle_us_i,
                        function(y)
                          eprscope::isotopes_ds %>%
                          dplyr::filter(.data$Isotope == y) %>%
                          dplyr::pull(.data$g_Nuclear))
    g_nuclear <- unname(g_nuclear)
    ## abundance
    abund_nuclear <- sapply(nucle_us_i,
                            function(y)
                              eprscope::isotopes_ds %>%
                              dplyr::filter(.data$Isotope == y) %>%
                              dplyr::pull(.data$Abund_Natur_Percent))
    abund_nuclear <- unname(abund_nuclear)
    abund_nuclear <- abund_nuclear / 100
    #
    ## magnetic spin quantum numbers of all nuclear groups in one list
    ## this required to calculate Breit-Rabi energies/frequencies/Bs (see below)
    m_spin_values <- Map(function(z,w)
    {sapply(1:(2 * z * w + 1), function(q) - (z * w) + q - 1)},
    N_nuclei,
    spin_nuclear
    )
    #
    ## QM function to calculate the delta spin energies / frequencies (in GHz) / B (in T)
    ## according to Breit-Rabi (see J. Magnet. Reson. https://doi.org/10.1016/0022-2364(71)90049-7)
    ## formula corresponding to hyperfine interaction with one unpaired electron
    nuB_breit_rabi <- function(A_iso, ## in energy units NOT in MHz <-> convert
                               B.0, ## in Tesla
                               g_nuclear,
                               g_iso_electronic,
                               spin_nuclear,
                               m_spin_nuclear,
                               B.output = FALSE){
      #
      ## definition of variable alpha
      alpha <- ((g_iso_electronic * Bohr.mu * B.0) + (g_nuclear * nuclear.mu * B.0)) /
        (A_iso * (spin_nuclear + 0.5))
      ## Energies depending on lower (1) or higher state (2)
      E1 <- A_iso/4 - (g_nuclear * nuclear.mu * B.0) * (m_spin_nuclear - 0.5) -
        (spin_nuclear + 0.5) * (A_iso / 2) *
        sqrt(1 + ((2 * (m_spin_nuclear - 0.5))/(spin_nuclear + 0.5)) * alpha + alpha^2)
      #
      E2 <- A_iso/4 - (g_nuclear * nuclear.mu * B.0) * (m_spin_nuclear + 0.5) +
        (spin_nuclear + 0.5) * (A_iso / 2) *
        sqrt(1 + ((2 * (m_spin_nuclear + 0.5))/(spin_nuclear + 0.5)) * alpha + alpha^2)
      #
      ## set up decimal places (later for frequency and magnetic flux density)
      options(digits = 8)
      ## Delta E
      DeltaE <- E2 - E1
      ## Convert energy into `nu` or `B`
      if (isFALSE(B.output)){
        Frequency <- round((DeltaE / Planck.const) * 1e-9,digits = 8) ## in GHz
        return(Frequency)
      } else {
        B_corresp_nu <- round(DeltaE / (g_iso * Bohr.mu),digits = 7)
        return(B_corresp_nu)
      }
    }
    #
    ## Intensity pattern function for nuclear spin quantum number (I)
    ## and number of nuclei (h)
    intensity_pattern <- function(I,h){
      if (I == 0.5){
        ## binomial coeff. from the last row of Pascal triangle
        intens_pattern_v <- sapply(0:h, function(i) choose(h,i))
        return(intens_pattern_v)
      }
      ## Intensity pattern for I = 1 (e.g. 14N,D,6Li,...)
      if (I == 1 & h == 0){
        return(1)
      }
      if (I == 1 & h == 1){
        return(c(1,1,1))
      }
      if (I == 1 & h == 2){
        return(c(1,2,3,2,1))
      }
      if (I == 1 & h == 3){
        return(c(1,3,6,7,6,3,1))
      }
      if (I == 1 & h == 4){
        return(c(1,4,10,16,19,16,10,4,1))
      }
      if (I == 1 & h == 5){
        return(c(1,5,15,30,45,51,45,30,15,5,1))
      }
      if (I == 1 & h == 6){
        return(c(1,6,21,50,90,126,141,126,90,50,21,6,1))
      }
      # if (I == 1 & h == 7){
      #   return(c(1,7,28,77,161,266,357,393,357,266,161,77,28,7,1))
      # }
      # if (I == 1 & h == 8){
      #   return(c(1,8,36,112,266,504,784,1016,1107,1016,784,504,266,112,36,8,1))
      # }
      ## Intensity pattern for I = 3/2 (e.g. 11B, 35Cl, 37Cl...)
      if (I == 1.5 & h == 0){
        return(1)
      }
      if (I == 1.5 & h == 1){
        return(c(1,1,1,1))
      }
      if (I == 1.5 & h == 2){
        return(c(1,2,3,4,3,2,1))
      }
      if (I == 1.5 & h == 3){
        return(c(1,3,6,10,12,12,10,6,3,1))
      }
      if (I == 1.5 & h == 4){
        return(c(1,4,10,20,31,40,44,40,31,20,10,4,1))
      }
      if (I == 1.5 & h == 5){
        return(c(1,5,15,35,65,101,135,155,
                 155,135,101,65,35,15,5,1))
      }
      if (I == 1.5 & h == 6){
        return(c(1,6,20,56,120,216,336,456,546,580,
                 546,456,336,216,120,56,20,6,1))
      }
      ## There are no stable isotopes with I = 2 =>
      ## Intensity pattern for I = 5/2 (e.g. 47Ti, 55Mn, 127I ,27Al, 99Ru, 17O,101Ru...)
      if (I == 2.5 & h == 0){
        return(1)
      }
      if (I == 2.5 & h == 1){
        return(c(1,1,1,1,1,1))
      }
      if (I == 2.5 & h == 2){
        return(c(1,2,3,4,5,6,5,4,3,2,1))
      }
      if (I == 2.5 & h == 3){
        return(c(1,3,6,10,15,21,25,27,27,25,21,15,10,6,3,1))
      }
      if (I == 2.5 & h == 4){
        return(c(1,4,10,20,35,56,80,104,125,140,146,
                 140,125,104,80,56,35,20,10,4,1))
      }
      ## Intensity pattern for I = 3 (e.g. 10B)
      if (I == 3 & h == 0){
        return(1)
      }
      if (I == 3 & h == 1){
        return(c(1,1,1,1,1,1,1))
      }
      if (I == 3 & h == 2){
        return(c(1,2,3,4,5,6,7,6,5,4,3,2,1))
      }
      if (I == 3 & h == 3){
        return(c(1,3,6,10,15,21,28,33,36,37,36,33,28,21,15,10,6,3,1))
      }
      if (I == 3 & h == 4){
        return(c(1,4,10,20,35,56,84,116,149,180,206,224,231,
                 224,206,180,149,116,84,56,35,20,10,4,1))
      }
      #
    }
    #
    ## intensity pattern list for all nuclei by the previous function
    intensity_pattern_nuclei <- Map(function(d,c)
      intensity_pattern(d,c),
      spin_nuclear,
      N_nuclei)
    #
    ## Function to calculate the entire intensity pattern by the multiplication
    ## of adjacent levels e.g. like
    ##          1           1           1 |       level_01 (e.g. 1 x 14N), highest Aiso
    ##      1   2   1 |  ...                      level_02 (e.g. 2 x 1H)
    ##     1 1 | ...                              level_03 (e.g. 1 x 1H), lowest Aiso
    ## therefore => 1 1 x 1, 1 1 x 2, 1 1 x 1.....etc <=> level_03 x each component of level_02
    ## then the result must be multiplied by each component of level_01 pattern
    ## the entire number of lines = (2 * 0.5 + 1) x (2 * 2 * 0.5 + 1) x (2 * 1 + 1) = 2 x 3 x 3
    ## If natural abundances have to be considered => each level must be also multiplied
    ## by the corresponding isotope natur. abundance however only once !
    intensity_level_pattern_multiply <- function(intensity.nuclei.pattern,
                                                 natur.abund = FALSE,
                                                 nuclear.abund){
      ## `intensity.nuclei.pattern` corresponds to list
      ## for all (see N_levels) nuclear groups
      ## `nuclear.abund` corresponds to vector of all (see N_levels)
      ## abundances, the entire number of levels
      N_levels <- length(intensity.nuclei.pattern)
      ## iterating through all patterns
      intensity_pattern_N_Nminus <- c()
      if (isFALSE(natur.abund)){
        nuclear.abund <- NULL
        if (N_levels >= 2){
          intensity_pattern_N_Nminus[[N_levels-1]] <-
            lapply(1:length(intensity.nuclei.pattern[[N_levels-1]]),
                   function(m)
                     intensity.nuclei.pattern[[N_levels]] *
                     intensity.nuclei.pattern[[N_levels-1]][m])
          ## unlist
          intensity_pattern_N_Nminus[[N_levels-1]] <-
            unlist(intensity_pattern_N_Nminus[[N_levels-1]],
                   use.names = FALSE)
        }
        if (N_levels >= 3){
          for (j in 2:(N_levels-1)){
            intensity_pattern_N_Nminus[[N_levels - j]] <-
              lapply(1:length(intensity.nuclei.pattern[[N_levels - j]]),
                     function(o)
                       intensity_pattern_N_Nminus[[N_levels - (j-1)]] *
                       intensity.nuclei.pattern[[N_levels - j]][o])
            ## unlist
            intensity_pattern_N_Nminus[[N_levels - j]] <-
              unlist(intensity_pattern_N_Nminus[[N_levels - j]],
                     use.names = FALSE)
          }
        }
      } else{
        if (N_levels >= 2){
          intensity_pattern_N_Nminus[[N_levels-1]] <-
            lapply(1:length(intensity.nuclei.pattern[[N_levels-1]]),
                   function(m)
                     intensity.nuclei.pattern[[N_levels]] *
                     nuclear.abund[N_levels] *
                     intensity.nuclei.pattern[[N_levels-1]][m] *
                     nuclear.abund[N_levels-1])
          ## unlist
          intensity_pattern_N_Nminus[[N_levels-1]] <-
            unlist(intensity_pattern_N_Nminus[[N_levels-1]],
                   use.names = FALSE)
        }
        if (N_levels >= 3){
          for (j in 2:(N_levels-1)){
            intensity_pattern_N_Nminus[[N_levels - j]] <-
              lapply(1:length(intensity.nuclei.pattern[[N_levels - j]]),
                     function(o)
                       intensity_pattern_N_Nminus[[N_levels - (j-1)]] *
                       intensity.nuclei.pattern[[N_levels - j]][o] *
                       nuclear.abund[N_levels - j])
            ## unlist
            intensity_pattern_N_Nminus[[N_levels - j]] <-
              unlist(intensity_pattern_N_Nminus[[N_levels - j]],
                     use.names = FALSE)
          }
        }
      }
      #
      ## because the list goes from N-1,N-2...to [[1]],
      ## the "last" (`[[1]]`) element coincides with
      ## the entire pattern
      return(intensity_pattern_N_Nminus[[1]])
      #
    }
    #
  }
  #
  ## condition to find g_iso
  if (is.null(g.iso)){
    near_g_iso <- which.min(abs(B.g.sim.df$B_T - B.CF)) ## find index/row
  } else{
    near_g_iso <- which.min(abs(B.g.sim.df$g - g.iso))
  }
  near_g_iso_df <- B.g.sim.df[near_g_iso,] ## data.frame with index
  ## and finally
  g_iso <- near_g_iso_df %>% dplyr::pull(.data$g)
  ## corresponding actual `B_iso`
  B_iso <- near_g_iso_df %>% dplyr::pull(.data$B_T)
  #
  ## Definition for the Derivative EPR Spectral Line Forms
  ## Pseudo-Voight is only required because =>
  ## x*Gaussian(derivative) + y*Lorentzian(derivative)
  deriv_line_form <- function(B,
                              B.0,
                              g.x = lineGL.content[[1]],
                              l.y = lineGL.content[[2]],
                              gDeltaBpp = lineGL.DeltaBpp[[1]],
                              lDeltaBpp = lineGL.DeltaBpp[[2]]){
    #
    ## B.0 line center
    ## DeltaBpp linewidth
    #
    ## condition for the coefficients & line-width
    if (is.null(lDeltaBpp) & is.null(l.y)){
      ## Gaussian
      intens_deriv <- g.x * (- 4 * sqrt(2/pi) * (1/gDeltaBpp^2) * ((B - B.0)/gDeltaBpp) *
                               exp(- 2 * ((B - B.0)/gDeltaBpp)^2))
    }
    if (is.null(gDeltaBpp) & is.null(g.x)){
      ## Lorentzian
      intens_deriv <- l.y * (- 16 * (1/(pi * 3 * sqrt(3))) * ((B - B.0)/lDeltaBpp^3) *
                               (1 + 4/3 * ((B - B.0)/lDeltaBpp)^2)^(-2))
    }
    if (!is.null(g.x) & !is.null(l.y) & !is.null(gDeltaBpp) & !is.null(lDeltaBpp)){
      ## x*Gaussian(derivative) + y*Lorentzian(derivative) <=> pseudo Voightian
      intens_deriv <- g.x * (- 4 * sqrt(2/pi) * (1/gDeltaBpp^2) * ((B - B.0)/gDeltaBpp) *
                               exp(- 2 * ((B - B.0)/gDeltaBpp)^2)) +
        l.y * (- 16 * (1/(pi * 3 * sqrt(3))) * ((B - B.0)/lDeltaBpp^3) *
                 (1 + 4/3 * ((B - B.0)/lDeltaBpp)^2)^(-2))
    }
    #
    return(intens_deriv)
  }
  #
  #
  ## CALCULATING ENERGIES/Bs, INTENSITY PATTERNS and EPR SPECTRA for ALL e-N INTERACTIONS
  if (is.null(nuclear.system)){
    ## Simulated derivative EPR spectrum if `nuclear.system = NULL` (single line, no HF structure)
    B.g.sim.df[[Intensity.sim]] <- deriv_line_form(B = B.g.sim.df[[paste0("B_",B.unit)]],
                                                   B.0 = convert_B(B_iso,B.unit = "T",B.2unit = B.unit))
  } else{
    ## Simulated derivative EPR spectrum if `nuclear.system != NULL`
    ## Frequency/B + spectra calculations depending on number of nuclear groups
    #
    ## (1) NUMBER of NUCLEAR GROUPS >= 1
    #
    if (length(nuclear.system) >= 1){
      ## frequency for the biggest A_iso
      B_for_m_spin_values1 <-
        sapply(1:length(m_spin_values[[1]]), function(f)
          nuB_breit_rabi(A_iso = A_iso_MHz[1] * 1e+6 * Planck.const,
                         B.0 = B_iso,
                         g_nuclear = g_nuclear[1],
                         g_iso_electronic = g_iso,
                         spin_nuclear = spin_nuclear[1],
                         m_spin_nuclear = m_spin_values[[1]][f],
                         B.output = TRUE)
        )
      ## finding the indices in simulation df => `B.g.sim.df` corresponding to B values
      near_row_for_m_spin_values1 <-
        sapply(1:length(B_for_m_spin_values1),
               function(l)
                 which.min(abs(B.g.sim.df$B_T - B_for_m_spin_values1[l])))
      ## selecting rows based on previous indices
      near_B_for_m_spin_values1 <-
        B.g.sim.df %>%
        dplyr::slice(near_row_for_m_spin_values1)
      #
      if (length(nuclear.system) == 1){
        ## Spectral line intensity in `B.unit`s depending on natur. abund
        if (isFALSE(natur.abund)){
          abund_nuclear1 <- 1
        } else {
          abund_nuclear1 <- abund_nuclear[1]
        }
        Sim_Intensity <-
          Map(function(u,v)
          {u * abund_nuclear1 * deriv_line_form(B = B.g.sim.df[[paste0("B_",B.unit)]],B.0 = v)},
          intensity_pattern_nuclei[[1]],
          near_B_for_m_spin_values1[[paste0("B_",B.unit)]]
          )
        ## Sum of the spectral line intensities
        B.g.sim.df[[Intensity.sim]] <- Reduce("+",Sim_Intensity)
        ## List `Sim_Intensity` is not required anymore
        rm(Sim_Intensity)
      }
    }
    #
    ## (2) NUMBER of NUCLEAR GROUPS >= 2
    #
    if (length(nuclear.system) >= 2){
      ## for all Bs from the previous 1 corresponding Bs ot the multiplet =>
      B_for_m_spin_values2 <- c()
      for (e in seq(near_B_for_m_spin_values1$B_T)){
        B_for_m_spin_values2[[e]] <-
          sapply(1:length(m_spin_values[[2]]), function(f)
            nuB_breit_rabi(A_iso = A_iso_MHz[2] * 1e+6 * Planck.const,
                           B.0 = near_B_for_m_spin_values1$B_T[e],
                           g_nuclear = g_nuclear[2],
                           g_iso_electronic = g_iso,
                           spin_nuclear = spin_nuclear[2],
                           m_spin_nuclear = m_spin_values[[2]][f],
                           B.output = TRUE)
          )
      }
      ## finding the indices in simulation df => `B.g.sim.df` corresponding to B values
      near_row_for_m_spin_values2 <- c()
      for (l in seq(B_for_m_spin_values2)){
        near_row_for_m_spin_values2[[l]] <-
          sapply(1:length(B_for_m_spin_values2[[l]]),
                 function(x)
                   which.min(abs(B.g.sim.df$B_T - B_for_m_spin_values2[[l]][x]))
          )
      }
      ## summarizing in the actual 2 vector list and deleting the previous list
      near_row_for_m_spin_values2 <- unlist(near_row_for_m_spin_values2,use.names = FALSE)
      ## selecting the rows based on previous indices
      near_B_for_m_spin_values2 <-
        B.g.sim.df %>%
        dplyr::slice(near_row_for_m_spin_values2)
      #
      if (length(nuclear.system) == 2){
        ## entire line intensities including the natural abundance
        intensity_pattern_nuclei_total <-
          intensity_level_pattern_multiply(
            intensity.nuclei.pattern = intensity_pattern_nuclei,
            natur.abund = natur.abund,
            nuclear.abund = abund_nuclear)
        ## Spectral line intensities
        Sim_Intensity <-
          Map(function(u,v)
          {u * deriv_line_form(B = B.g.sim.df[[paste0("B_",B.unit)]],B.0 = v)},
          intensity_pattern_nuclei_total,
          near_B_for_m_spin_values2[[paste0("B_",B.unit)]]
          )
        ## Sum of the spectral line intensities
        B.g.sim.df[[Intensity.sim]] <- Reduce("+",Sim_Intensity)
        ## List `Sim_Intensity` is not required anymore
        rm(Sim_Intensity)
      }
    }
    #
    ## (3) NUMBER of NUCLEAR GROUPS >= 3
    #
    if (length(nuclear.system) >= 3){
      ## for all Bs from the previous 2 corresponding Bs of the multiplet =>
      B_for_m_spin_values3 <- c()
      for (e in seq(near_B_for_m_spin_values2$B_T)){
        B_for_m_spin_values3[[e]] <-
          sapply(1:length(m_spin_values[[3]]), function(f)
            nuB_breit_rabi(A_iso = A_iso_MHz[3] * 1e+6 * Planck.const,
                           B.0 = near_B_for_m_spin_values2$B_T[e],
                           g_nuclear = g_nuclear[3],
                           g_iso_electronic = g_iso,
                           spin_nuclear = spin_nuclear[3],
                           m_spin_nuclear = m_spin_values[[3]][f],
                           B.output = TRUE)
          )
      }
      ## finding the indices in simulation df => `B.g.sim.df` corresponding to B values
      near_row_for_m_spin_values3 <- c()
      for (l in seq(B_for_m_spin_values3)){
        near_row_for_m_spin_values3[[l]] <-
          sapply(1:length(B_for_m_spin_values3[[l]]),
                 function(x)
                   which.min(abs(B.g.sim.df$B_T - B_for_m_spin_values3[[l]][x]))
          )
      }
      ## summarizing in the actual 3 vector list and deleting the previous list
      near_row_for_m_spin_values3 <- unlist(near_row_for_m_spin_values3,use.names = FALSE)
      ## selecting the rows based on previous indices
      near_B_for_m_spin_values3 <-
        B.g.sim.df %>%
        dplyr::slice(near_row_for_m_spin_values3)
      #
      if (length(nuclear.system) == 3){
        ## entire line intensities including the natural abundance
        intensity_pattern_nuclei_total <-
          intensity_level_pattern_multiply(
            intensity.nuclei.pattern = intensity_pattern_nuclei,
            natur.abund = natur.abund,
            nuclear.abund = abund_nuclear)
        ## Spectral line intensities
        Sim_Intensity <-
          Map(function(u,v)
          {u * deriv_line_form(B = B.g.sim.df[[paste0("B_",B.unit)]],B.0 = v)},
          intensity_pattern_nuclei_total,
          near_B_for_m_spin_values3[[paste0("B_",B.unit)]]
          )
        ## Sum of the spectral line intensities
        B.g.sim.df[[Intensity.sim]] <- Reduce("+",Sim_Intensity)
        ## List `Sim_Intensity` is not required anymore
        rm(Sim_Intensity)
      }
    }
    #
    ## (4) NUMBER of NUCLEAR GROUPS >= 4
    #
    if (length(nuclear.system) >= 4){
      ## for all Bs from the previous 3 corresponding Bs of the multiplet =>
      B_for_m_spin_values4 <- c()
      for (e in seq(near_B_for_m_spin_values3$B_T)){
        B_for_m_spin_values4[[e]] <-
          sapply(1:length(m_spin_values[[4]]), function(f)
            nuB_breit_rabi(A_iso = A_iso_MHz[4] * 1e+6 * Planck.const,
                           B.0 = near_B_for_m_spin_values3$B_T[e],
                           g_nuclear = g_nuclear[4],
                           g_iso_electronic = g_iso,
                           spin_nuclear = spin_nuclear[4],
                           m_spin_nuclear = m_spin_values[[4]][f],
                           B.output = TRUE)
          )
      }
      ## finding the indices in simulation df => `B.g.sim.df` corresponding to B values
      near_row_for_m_spin_values4 <- c()
      for (l in seq(B_for_m_spin_values4)){
        near_row_for_m_spin_values4[[l]] <-
          sapply(1:length(B_for_m_spin_values4[[l]]),
                 function(x)
                   which.min(abs(B.g.sim.df$B_T - B_for_m_spin_values4[[l]][x]))
          )
      }
      ## summarizing in the actual 4 vector list and deleting the previous list
      near_row_for_m_spin_values4 <- unlist(near_row_for_m_spin_values4,use.names = FALSE)
      ## selecting the rows based on previous indices
      near_B_for_m_spin_values4 <-
        B.g.sim.df %>%
        dplyr::slice(near_row_for_m_spin_values4)
      if (length(nuclear.system) == 4){
        ## entire line intensities including the natural abundance
        intensity_pattern_nuclei_total <-
          intensity_level_pattern_multiply(
            intensity.nuclei.pattern = intensity_pattern_nuclei,
            natur.abund = natur.abund,
            nuclear.abund = abund_nuclear)
        ## Spectral line intensities
        Sim_Intensity <-
          Map(function(u,v)
          {u * deriv_line_form(B = B.g.sim.df[[paste0("B_",B.unit)]],B.0 = v)},
          intensity_pattern_nuclei_total,
          near_B_for_m_spin_values4[[paste0("B_",B.unit)]]
          )
        ## Sum of the spectral line intensities
        B.g.sim.df[[Intensity.sim]] <- Reduce("+",Sim_Intensity)
        ## List `Sim_Intensity` is not required anymore
        rm(Sim_Intensity)
      }
    }
    #
    ## (5) NUMBER of NUCLEAR GROUPS = 5 (number of groups can be extended in the future)
    #
    if (length(nuclear.system) == 5){
      ## for all Bs from the previous 4 corresponding Bs of the multiplet =>
      B_for_m_spin_values5 <- c()
      for (e in seq(near_B_for_m_spin_values4$B_T)){
        B_for_m_spin_values5[[e]] <-
          sapply(1:length(m_spin_values[[5]]), function(f)
            nuB_breit_rabi(A_iso = A_iso_MHz[5] * 1e+6 * Planck.const,
                           B.0 = near_B_for_m_spin_values4$B_T[e],
                           g_nuclear = g_nuclear[5],
                           g_iso_electronic = g_iso,
                           spin_nuclear = spin_nuclear[5],
                           m_spin_nuclear = m_spin_values[[5]][f],
                           B.output = TRUE)
          )
      }
      ## finding the indices in simulation df => `B.g.sim.df` corresponding to B values
      near_row_for_m_spin_values5 <- c()
      for (l in seq(B_for_m_spin_values5)){
        near_row_for_m_spin_values5[[l]] <-
          sapply(1:length(B_for_m_spin_values5[[l]]),
                 function(x)
                   which.min(abs(B.g.sim.df$B_T - B_for_m_spin_values5[[l]][x]))
          )
      }
      ## summarizing in the actual 5 vector list and deleting the previous list
      near_row_for_m_spin_values5 <- unlist(near_row_for_m_spin_values5,use.names = FALSE)
      ## selecting the rows based on previous indices
      near_B_for_m_spin_values5 <-
        B.g.sim.df %>%
        dplyr::slice(near_row_for_m_spin_values5)
      ## entire line intensities including the natural abundance
      intensity_pattern_nuclei_total <-
        intensity_level_pattern_multiply(
          intensity.nuclei.pattern = intensity_pattern_nuclei,
          natur.abund = natur.abund,
          nuclear.abund = abund_nuclear)
      ## Spectral line intensities
      Sim_Intensity <-
        Map(function(u,v)
        {u * deriv_line_form(B = B.g.sim.df[[paste0("B_",B.unit)]],B.0 = v)},
        intensity_pattern_nuclei_total,
        near_B_for_m_spin_values5[[paste0("B_",B.unit)]]
        )
      ## Sum of the spectral line intensities
      B.g.sim.df[[Intensity.sim]] <- Reduce("+",Sim_Intensity)
      ## List `Sim_Intensity` is not required anymore
      rm(Sim_Intensity)
      #
    }
    #
    ## number of groups can be extended in the future
    #
    #
  }
  #
  ## Reducing columns in the final data frame
  B.g.sim.df <- B.g.sim.df %>%
    dplyr::select(.data$B_G,.data$B_mT,.data[[Intensity.sim]])
  ## Plotting the EPR spectrum
  spectrum.sim.plot <-
    ggplot(data = B.g.sim.df,
           aes(x = .data[[paste0("B_",B.unit)]],
               y = .data[[Intensity.sim]])) +
    geom_line(color = "blue",linewidth = 0.75) +
    labs(x = bquote(italic(B) ~ "(" ~ .(B.unit) ~ ")"),
         y = bquote(d * italic(I)[EPR] ~ "/" ~ d * italic(B) ~ ~"(" ~ p.d.u. ~ ")")) +
    plot_theme_NoY_ticks() +
    scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL))
  #
  ## result list with data frame and plot
  if (isFALSE(plot.sim.interact)){
    return(list(plot = spectrum.sim.plot,df = B.g.sim.df))
  } else{
    return(plot_EPR_Specs2D_interact(data.spectra = B.g.sim.df,
                                     x = paste0("B_",B.unit),
                                     x.unit = B.unit,
                                     Intensity = Intensity.sim))
  }
  #
}
