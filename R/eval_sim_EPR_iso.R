#
#' Simulation of Isotropic EPR Spectra
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#'   EPR spectra often display hyperfine structure, where the spectral lines split into several other ones,
#'   as a consequence of the electron-nuclear interaction with nuclei close to the paramagnetic center.
#'   Sometimes, such splitting can be complicated and requires \strong{simulations}, i.e. computational "synthesis"
#'   of spectral structure based on quantum chemistry as well as on mathematical description of spectral lineshapes
#'   (see also \code{References}). The actual function helps to analyze the hyperfine structure (HFS)
#'   of the isotropic EPR spectra by simulations. For such purpose, properties like nuclear \eqn{g}-value,
#'   spin quantum number as well as natural abundance of the isotopes, related to interacting nuclei,
#'   must be known and are collected in the \code{\link{isotopes_ds}}. EPR spectra can be simulated
#'   for the derivative as well as for the integrated line forms (see the argument \code{lineSpec.form}).
#'
#'
#' @details
#'   Theoretical predictions or computations of a spectrum (and its comparison with the experiment) represent
#'   an important step in the analysis of EPR spectra. However, such step requires an iterative process with modelling
#'   the above-described \strong{electron-nuclear system} followed by a numerical simulations of EPR spectra
#'   to match the experimental ones (see also \code{\link{eval_sim_EPR_isoFit}}). Commonly, quantum chemical calculations
#'   (usually DFT, see also \code{vignette("functionality")} are involved in this process.
#'   EPR simulations in the isotropic regime assume that the molecules tumble/move extremely fast causing a total averaging
#'   of any anisotropic properties out. An EPR spectrum corresponding to the latter consists of a series of symmetric
#'   lines with equal widths. In such case the spectrum is characterized by the isotropic HF coupling/splitting
#'   constants \eqn{A_{\text{iso}}/a_{\text{iso}}} (see also the \code{nuclear.system} argument) as well as
#'   by the above-mentioned linewidth. Many organic radicals exhibit such EPR spectra at room temperature
#'   and in solutions of low viscosities (Gerson F, Huber W (2003), see \code{References}).
#'
#'   In the first step the \eqn{B}-region (magnetic flux density) and the resolution must be defined
#'   by the \code{instrum.params} argument or can be directly acquired from the parameter file using
#'   the \code{path_to_dsc_par} argument. Position of the spectrum (within the desired \eqn{B}-region)
#'   as well as those of HFS-lines are evaluated by the resonance condition (see also the \code{\link{eval_gFactor}})
#'   and by the Breit-Rabi analytical expression for energy levels of the interacting nuclei.
#'   The related \eqn{B}s are computed by the fixed-point iterations,
#'   because the corresponding \eqn{g}-value for each of the HFS-lines is not known (Weil JA (1971)
#'   and Stoll A, Schweiger A (2006)). The shape of spectral lines are calculated by the analytical formula
#'   of linear combination of the Gaussian and the Lorentzian line-shapes (also referred to as pseudo-Voigt,
#'   Weil JA, Bolton JR (2007) and Stoll S (2024)).  The linear coefficients
#'   are defined by the \code{lineG.content} argument, actually, corresponding to Gaussian line content (the Lorentzian one
#'   is computed as 1-\code{lineG.content}, accordingly). The linewidth, from that linear combination,
#'   is defined individually for the Gaussian and the Lorentzian (please, refer to the \code{lineGL.DeltaB} argument).
#'   The multiplicities (relative intensity ratios) are computed by the binomial/multinomial coefficients
#'   taking into account the spin quantum numbers of the interacting nuclei, in each of the equivalent groups,
#'   as well as their natural abundance (if \code{natur.abund = TRUE}).
#'
#'
#'
#' @references
#'   Weil JA, Bolton JR (2007). \emph{Electron paramagnetic resonance: elementary theory and practical applications},
#'   2nd edition. John Wiley and Sons. ISBN 978-0-471-75496-1,
#'   \url{https://onlinelibrary.wiley.com/doi/book/10.1002/0470084987}.
#'
#'   Weil JA (1971). “The Analysis of Large Hyperfine Splitting in Paramagnetic Resonance Spectroscopy.”
#'   \emph{J. Magn. Reson.} (1969), \strong{4}(3), 394–399, \url{https://doi.org/10.1016/0022-2364(71)90049-7}.
#'
#'   Stoll S, Schweiger A (2006). “EasySpin, A Comprehensive Software Package for Spectral Simulation
#'   and Analysis in EPR.” \emph{J. Magn. Reson.}, \strong{178}(1), 42–55,
#'   \url{https://doi.org/10.1016/j.jmr.2005.08.013}.
#'
#'   Stoll S (2024). “EasySpin Documentation - Line Shapes.”,
#'   \url{https://easyspin.org/easyspin/documentation/lineshapes.html}.
#'
#'   Gerson F, Huber W (2003). \emph{Electron Spin Resonance Spectroscopy of Organic Radicals},
#'   Biotechnology Series, Wiley-VCH, ISBN 978-3-527-30275-8, \url{https://books.google.cz/books?id=SEPeNjG3IvYC}.
#'
#'
#'
#' @param g.iso Numeric value, guess of the isotropic \eqn{g}-factor. It may also possess a \code{NULL}
#'   value if the \eqn{g} corresponding to "central field" is equal to \code{g.iso}.
#'   \strong{Default}: \code{g.iso = 2.00232} (the approximate \eqn{g} of the free electron).
#' @param instrum.params Named numeric vector, containing instrumental parameters required
#'   for the simulation =>
#'   \tabular{ll}{
#'   \code{Bcf} \tab "central field" (magnetic flux density, \eqn{B_{\text{CF}}}) \cr
#'   \code{Bsw} \tab "sweep width" (magnetic flux density recording region,
#'   \eqn{B_{\text{SW}}}) \cr
#'   \code{Npoints} \tab number of spectral points (corresponding to resolution) within
#'   the "sweep width" \cr
#'   \code{mwGHz} \tab applied microwave frequency in \code{GHz} to record the continuous wave (CW)
#'   EPR spectrum \cr
#'   }
#'   \strong{Default} values are chosen to cover the EPR spectra of common organic radicals.
#'   If \code{instrum.params = NULL} then parameters must be provided by the \code{path_to_dsc_par}
#'   as well as by \code{origin} arguments.
#' @param path_to_dsc_par Character string, path (can be also acquired by the \code{\link[base]{file.path}})
#'   to \code{.DSC/.dsc} or \code{.par} (depending on the OS, see \code{origin} argument)
#'   \code{text} files including all instrumental parameters from the EPR machine.
#'   \strong{Default}: \code{path_to_dsc_par = NULL} in case if the \code{instrum.params}
#'   is already defined. IF the \code{instrum.params = NULL} then BOTH the \code{path_to_dsc_par}
#'   AS WELL AS the \code{origin} MUST BE DEFINED !
#' @param origin Character string, corresponding to software which was used to obtain the EPR spectra
#'   on spectrometers, because the files are slightly different, whether they
#'   were recorded by the "WinEpr" (\code{origin = "winepr"}) or by the "Xenon".
#'   \strong{Default}: \code{origin = NULL} in case no file is used to extract
#'   the parameters (i.e. exactly if \code{path_to_dsc_par = NULL}).
#' @param B.unit Character string, pointing to unit of magnetic flux density which is to be presented
#'   on \eqn{B}-axis of the EPR spectrum, like \code{"G"} (Gauss) or \code{"mT"} (millitesla),
#'   \strong{default}: \code{B.unit = "G"}. THE UNIT MUST BE SHARED ACROSS ALL RELEVANT B-ARGUMENTS
#'   like \code{cf} and \code{sw} within the \code{instrum.params} AS WELL AS within THOSE IN \code{lineGL.DeltaB} !
#' @param nuclear.system List, containing the information about groups of equivalent nuclei
#'   interacting with the unpaired electron like \code{nuclear.system = list("14N",1,45)}.
#'   This corresponds to one group of "14N" interacting nuclei where \strong{the second number}
#'   denotes \strong{the number of nuclei} within the group and \strong{the third number}
#'   is the \strong{guess of the hyperfine coupling constant in MHz}. Therefore, in summary
#'   it refers to \eqn{A(1\times ^{14}\text{N}) = 45~\text{MHz}}. If more complex interaction
#'   is considered, e.g. \eqn{A(3\times ^{1}\text{H}) = 5.06~\text{MHz} +
#'   A(6\times ^{1}\text{H}) = 17.64~\text{MHz}}, such system must be defined by nested lists like
#'   \code{nuclear.system = list(list("1H",3,5.06),list("1H",6,17.64))}. The number of \code{lists}
#'   is not limited and therefore, any number of equivalent nuclei groups can be used to simulate
#'   the EPR spectra. \strong{Default}: \code{nuclear.system = NULL} in case
#'   if no interaction with the unpaired electron surrounding nuclei is considered and only single
#'   line EPR spectrum is expected.
#' @param natur.abund Logical, whether the natural abundance of the interacting nuclei
#'   is taken into the calculation of intensity pattern of the simulated EPR spectrum.
#'   \strong{Default}: \code{natur.abund = TRUE}. For a single-line
#'   EPR spectrum without hyperfine splitting(HFS) it is automatically switched to \code{natur.abund = FALSE}.
#' @param lineSpecs.form Character string, describing either \code{"derivative"} (\strong{default})
#'   or \code{"integrated"} (i.e. \code{"absorption"} which can be used as well) line form
#'   of the analyzed EPR spectrum/data.
#' @param lineGL.DeltaB List of two values referred to as \emph{Gaussian} (G) and \emph{Lorentzian} (L)
#'   spectral line-forms, respectively. For the "pure" \emph{Gaussian} only the first value is numeric
#'   and the second one is \code{NULL} => e.g. \code{lineGL.DeltaB = list(1,NULL)} (\strong{default}).
#'   For the "pure" \emph{Lorentzian} the opposite expression must be used => e.g.
#'   \code{lineGL.DeltaB = list(NULL,0.5)}. If the linear combination of both
#'   line forms is taken into account (see \code{lineG.content}), that is so called \emph{pseudo-Voigt},
#'   then both values are numeric (e.g. \code{lineGL.DeltaB = list(0.5,0.5)}) and are related
#'   to \emph{Gaussian} and \emph{Lorentzian} forms, respectively. The \code{DeltaB} corresponds either to
#'   \eqn{\Delta B_{\text{pp}}} (if \code{lineSpecs.form = "derivative"}) or to \eqn{FWHM}
#'   (if \code{lineSpecs.form = "integrated"} or if \code{lineSpecs.form = "absorption"}).
#'   The unit of values must coincide with those used in \code{instrum.params} as well as with \code{B.unit}.
#' @param lineG.content Numeric value between \code{0} and \code{1} referring to content of \emph{Gaussian} line form.
#'   If \code{lineG.content = 1} (\strong{default}) it corresponds to "pure" \emph{Gaussian} line form
#'   and if \code{lineG.content = 0} it corresponds to \emph{Lorentzian} one. The value from (0,1)
#'   (e.g. \code{lineG.content = 0.5}) represents the linear combination (for the example above
#'   with the coefficients 0.5 and 0.5) of both line forms => so called \emph{pseudo-Voigt}.
#' @param Intensity.sim Character string, pointing to column of simulated EPR intensity within the related output
#'   data frame. \strong{Default}: \code{Intensity.sim = "dIeprSim_over_dB"}.
#' @param plot.sim.interact Logical, whether to display the simulated spectrum by interactive \code{plotly} graph
#'   (see also \code{\link{plot_EPR_Specs2D_interact}}). If \code{plot.sim.interact = FALSE} (\strong{dafault}),
#'   then the output contains the data frame as well as \code{ggplot2} based plot of the simulated EPR spectrum
#'   within a list.
#'
#'
#' @return If \code{plot.sim.interact = TRUE}, function returns an interactive plot object with the simulated EPR spectrum.
#'   Otherwise (if \code{plot.sim.interact = FALSE}), the output is represented by the \code{list} with the following
#'   elements:
#'   \describe{
#'   \item{plot}{\code{ggplot2} static object showing the simulated EPR spectrum.}
#'
#'   \item{df}{Data frame/table object related to the simulated spectrum.}
#'   }
#'
#'
#' @examples
#' ## simulation of simple EPR spectrum (without hyperfine
#' ## structure) with g(iso) = 1.9804 and the linewidth
#' ## ∆Bpp = 3.2 G, only Gaussian lineform is considered:
#' sim.simple.a <-
#'   eval_sim_EPR_iso(g.iso = 1.9804,
#'     instrum.params = c(Bcf = 3490,
#'                        Bsw = 200,
#'                        Npoints = 1600,
#'                        mwGHz = 9.8943),
#'     lineGL.DeltaB = list(3.2,NULL)
#'   )
#' ## simulation preview:
#' sim.simple.a$plot
#' #
#' ## simulation of luteolin radical anion with
#' ## the following four hyperfine coupling constants
#' ## A(1 x 1H) = 3.1 MHz, A(1 x 1H) = 2.8 MHz,
#' ## A(1 x 1H) = 8.0 MHz and A(1 x 1H) = 4.1 MHz,
#' ## one may check out the simulation
#' ## at https://doi.org/10.1016/j.electacta.2013.06.136
#' ## (see Fig. 6 in that article):
#' sim.luteol <-
#'   eval_sim_EPR_iso(g.iso = 2.00495,
#'     instrum.params = c(Bcf = 339.367,
#'                        Bsw = 5.9,
#'                        Npoints = 2048,
#'                        mwGHz = 9.5294),
#'     nuclear.system = list(list("1H",1,3.1),
#'                           list("1H",1,2.8),
#'                           list("1H",1,8.0),
#'                           list("1H",1,4.1)),
#'     lineGL.DeltaB = list(0.034,0.034),
#'     lineG.content = 0.6,
#'     B.unit = "mT"
#'  )
#' #
#' ## simulated spectrum preview within
#' ## the B = (338-341) mT region:
#' sim.luteol$plot +
#'   ggplot2::coord_cartesian(xlim = c(338,341))
#' #
#' ## ...and the corresponding data frame:
#' head(sim.luteol$df)
#' #
#' ## simulation of phenalenyl/perinaphthenyl
#' ## (PNT) radical in the integrated form:
#' eval_sim_EPR_iso(g = 2.0027,
#'   instrum.params = c(Bcf = 3500, # central field
#'                      Bsw = 100, # sweep width
#'                      Npoints = 4096,
#'                      mwGHz = 9.8), # MW Freq. in GHz
#'   B.unit = "G",
#'   nuclear.system = list(
#'       list("1H",3,5.09), # 3 x A(1H) = 5.09 MHz
#'       list("1H",6,17.67) # 6 x A(1H) = 17.67 MHz
#'    ),
#'   lineSpecs.form = "integrated",
#'   lineGL.DeltaB = list(0.54,NULL), # Gauss. FWHM in G
#'   Intensity.sim = "single_Integ",
#'   plot.sim.interact = TRUE
#'  )
#'
#'
#'
#' @export
#'
#'
#' @importFrom dplyr all_of any_of
eval_sim_EPR_iso <- function(g.iso = 2.00232,
                             instrum.params = c(
                               Bcf = 3500,
                               Bsw = 200,
                               Npoints = 2048,
                               mwGHz = 9.8
                             ),
                             path_to_dsc_par = NULL,
                             origin = NULL,
                             B.unit = "G",
                             nuclear.system = NULL,
                             natur.abund = TRUE,
                             lineSpecs.form = "derivative",
                             lineGL.DeltaB = list(1,NULL),
                             lineG.content = 1,
                             Intensity.sim = "dIeprSim_over_dB",
                             plot.sim.interact = FALSE){
  #
  ## Temporary processing variables
  . <- NULL
  ## Constants
  Planck.const <- constants::syms$h
  nuclear.mu <- constants::syms$mun ## Nuclear magneton
  Bohr.mu <- constants::syms$mub ## Bohr magneton
  #
  ## ================================= INPUT SYSTEM ======================================
  #
  ## `nuclear.system` definition if `nuclear.system != NULL`
  if (!is.null(nuclear.system)){
    ## check if the list is nested (several groups) or simple (only one group)
    nested_list <- any(sapply(nuclear.system, is.list))
    if (isFALSE(nested_list)){
      ## redefinition of `nuclear.system` list to calculate the spectra without
      ## any additional conditions just by simple =>
      nuclear.system <- list(nuclear.system)
    }
    ## reordering the `nuclear.system` from the highest A_iso to the lowest one
    nuclear.system <- nuclear.system[order(sapply(nuclear.system,"[[",3),decreasing = TRUE)]
    #
    ## extract list components and convert them into vectors
    nucle_us_i <- sapply(1:length(nuclear.system), function(e) nuclear.system[[e]][[1]])
    N_nuclei <- sapply(1:length(nuclear.system), function(e) nuclear.system[[e]][[2]])
    A_iso_MHz <- sapply(1:length(nuclear.system), function(e) nuclear.system[[e]][[3]])
    ## take only absolute values of `A_iso_MHz` (common EPR experiments
    ## cannot determine the `A_iso_MHz` sign)
    A_iso_MHz <- abs(A_iso_MHz)
    #
    ## condition for the `A_iso` < nu.GHz(mwGHz)
    if (any((A_iso_MHz) * 1e-3 >= unname(instrum.params["mwGHz"]))){
      stop(" Any of the `A_{iso}` values is higher than the MW Frequency !\n
           The isotropic EPR spectrum cannot be simulated by this function ! ")
    }
  }
  #
  ## Extracting instrumental parameter values from `.DSC` or `.par` files
  ## or from named numeric vector above
  if (is.null(path_to_dsc_par)){
    if (is.null(instrum.params)){
      stop(" Please, define `instrum.params` like central field, MW freq.,... ! ")
    } else {
      B.CF <- unname(instrum.params["Bcf"])
      B.SW <- unname(instrum.params["Bsw"])
      Npoints <- unname(instrum.params["Npoints"])
      nu.GHz <- unname(instrum.params["mwGHz"])
    }
  } else{
    if (!is.null(instrum.params)){
      stop(" Parameters are extracted from file, please write `instrum.params = NULL` ! ")
    } else{
      if (is.null(origin)){
        stop(" Please provide an `origin` of the `.DSC`/`.dsc` or `.par` file ! ")
      } else{
        ## reading the table and extracting values from it
        instr.params.list <-
          readEPR_params_slct_sim(path_to_dsc_par,origin = origin,B.unit = B.unit)
        B.CF <- instr.params.list$Bcf
        B.SW <- instr.params.list$Bsw
        Npoints <- instr.params.list$Npoints
        nu.GHz <- instr.params.list$mwGHz
      }
    }
  }
  #
  ## Create a data frame (`B` + `g`) for the simulated B region
  B.g.sim.df <- data.frame(B = seq(B.CF - (B.SW / 2),
                                   B.CF + (B.SW / 2),
                                   length.out = Npoints)
                           )
  colnames(B.g.sim.df) <- paste0("B_",B.unit)
  B.g.sim.df <- B.g.sim.df %>%
    dplyr::mutate(g = eval_gFactor(nu.val = nu.GHz,
                                   nu.unit = "GHz",
                                   B.val = .data[[paste0("B_",B.unit)]],
                                   B.unit = B.unit))
  #
  ## Conversions (adding columns) for `B.g.sim.df`, B in T/mT/G
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
    ## this is required to calculate Breit-Rabi energies/frequencies/Bs (see below)
    ## (2*N*I + 1) values going from  - N*I ...to...+ N*I
    m_spin_values <- Map(function(z,w)
    {sapply(1:(2 * z * w + 1), function(q) - (z * w) + q - 1)},
    N_nuclei,
    spin_nuclear
    )
    #
    ## ========================= DELTA_E,FREQ,B CALCULATION (BREIT-RABI) ============================
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
    fun_breit_rabi <- function(A_iso, ## in energy units NOT in MHz !! <-> convert into energy (A * h)
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
      E1 <- (A_iso / 4) - (g_nuclear * nuclear.mu * B.0) * (m_spin_nuclear - 0.5) -
        (spin_nuclear + 0.5) * (A_iso / 2) *
        sqrt(1 + ((2 * (m_spin_nuclear - 0.5)) / (spin_nuclear + 0.5)) * alpha + alpha^2)
      #
      E2 <- (A_iso / 4) - (g_nuclear * nuclear.mu * B.0) * (m_spin_nuclear + 0.5) +
        (spin_nuclear + 0.5) * (A_iso / 2) *
        sqrt(1 + ((2 * (m_spin_nuclear + 0.5)) / (spin_nuclear + 0.5)) * alpha + alpha^2)
      #
      ## Delta E for individual line
      DeltaE <- E2 - E1 ## in J
      #
      Freq_corresp <- round((DeltaE / Planck.const) * 1e-6, digits = 3) ## in MHz
      #
      ## The corresponding `B` (to `DeltaE`) cannot be evaluated analytically,
      ## because we do not know the corresponding line g-value !! => therefore the `B`
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
      gamma <- (Bohr.mu * g_e_iso) + (nuclear.mu * g_nuclear) ## constant variable
      for (i in 1:4){
        xaj[i] <- (A_iso / (2 * Planck.const)) / ((nu.GHz * 1e+9) +
                  (nuclear.mu * g_nuclear * (B.mI[i] / Planck.const)))
        B.mI[i+1] <- (A_iso / (gamma * (1 - xaj[i]^2))) *
                     (- m_spin_nuclear + sqrt(m_spin_nuclear^2 + (1 - xaj[i]^2) *
                     ((1 / (4 * xaj[i]^2)) - (spin_nuclear + 0.5)^2)))
      }
      #
      ## all three quantities into one list (B as a last value from those iterations)
      return(list(D_E = DeltaE, nu = Freq_corresp, B = B.mI[length(B.mI)]))
    }
    #
    ## ============================ LINE INTENSITIES ==================================
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
      if (I == 1 & h >= 7){
        stop(" The multimomial coefficients for such a high number\n
             of equivalent I = 1 nuclei are not defined !")
      }
      # will be fixed later on
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
      if (I == 1.5 & h >= 7){
        stop(" The multimomial coefficients for such a high number\n
             of equivalent I = 3/2 nuclei are not defined !")
      }
      # will be fixed later on
      ## There are no stable isotopes with I = 2 =>
      ## Intensity pattern for I = 5/2 (e.g. 47Ti, 55Mn, 127I, 27Al, 99Ru, 101Ru...)
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
      if (I == 2.5 & h >= 5){
        stop(" The multimomial coefficients for such a high number\n
             of equivalent I = 5/2 nuclei are not defined !")
      }
      # will be fixed later, on
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
      if (I == 3 & h >= 5){
        stop(" The multimomial coefficients for such a high number\n
             of equivalent I = 3 nuclei are not defined !")
      }
      #
      ## multinomial coefficients will be fixed later on
    }
    #
    ## intensity pattern list for all nuclei by the previous function
    intensity_pattern_nuclei <- Map(function(d,c)
      {intensity_pattern(d,c)},
      spin_nuclear,
      N_nuclei
      )
    #
    ## combinatorics if `natur.abund = TRUE` (`FALSE` included as well) =>
    ## intensity mutiplication coefficients:
    ## natur.abun^N_nuclei/sum(pattern intensities)
    combin_abund_coeff_int <-
      function(nucleus.abund,
               N_nuclei,
               natur.abund = natur.abund,
               intensity.nuclei.patern){
        #
        if (isTRUE(natur.abund)){
          coeff <- ((nucleus.abund)^(N_nuclei)) / sum(intensity.nuclei.patern)
        } else {
          coeff <- 1
        }
        #
        return(coeff)
      }

    # iterate through all coefficients into one variable (list)
    combin.abund.coeff.intens <-
      Map(function(p,r,s)
      {combin_abund_coeff_int(p,r,natur.abund = natur.abund,s)},
      abund_nuclear,
      N_nuclei,
      intensity_pattern_nuclei
      )

    #
    ## ---------------------------------- COMMENT !! -----------------------------------
    ## |                                                                                |
    ## | Function to calculate the entire intensity pattern by the multiplication       |
    ## | of the adjacent levels such as:                                                |
    ## |          1           1           1 |   level_01 (e.g. 1 x 14N), highest Aiso   |
    ## |      1   2   1 |  ...                  level_02 (e.g. 2 x 1H), mid Aiso        |
    ## |     1 1 | ...                          level_03 (e.g. 1 x 1H), lowest Aiso     |
    ## | therefore => 1 1 x 1, 1 1 x 2, 1 1 x 1.....etc <=> level_03 times each         |
    ## | component of the level_02.                                                     |
    ## | Afterwards the result must be multiplied by each component of the level_01     |
    ## | pattern. The entire number of lines =>                                         |
    ## | (2 * 1 * 1 + 1) x (2 * 2 * 0.5 + 1) x (2 * 1 * 0.5 + 1) = 3 x 3 x 2 = 18       |
    ## | If the natural abundances have to be considered => each level must be also     |
    ## | multiplied by the corresponding isotope natur. abundance intensity coefficient |
    ## | from combinatorics function above, however only once  !!!                      |
    ## | (only the the level corresponding to those nuclei not the other ones) !        |
    ## |                                                                                |
    ## ----------------------------------------------------------------------------------
    #
    intensity_level_pattern_multiply <- function(intensity.nuclei.pattern,
                                                 natur.abund = natur.abund,
                                                 nuclear.abund){ ## `nuclear.abund`
      ## corresponds to `combin.abund.coeff.intens`
      #
      ## `intensity.nuclei.pattern` corresponds to list
      ## for all (see N_levels below) nuclear groups
      ## `nuclear.abund` corresponds to vector of all (see N_levels)
      ## abundances, the entire number of levels
      N_levels <- length(intensity.nuclei.pattern)
      ## iterating through all patterns
      intensity_pattern_N_Nminus <- c()
      #
      ## with several groups
      if (N_levels >= 2){
        intensity_pattern_N_Nminus[[N_levels-1]] <-
          lapply(1:length(intensity.nuclei.pattern[[N_levels-1]]),
                 function(m)
                   intensity.nuclei.pattern[[N_levels]] *
                   nuclear.abund[[N_levels]] *
                   intensity.nuclei.pattern[[N_levels-1]][m] *
                   nuclear.abund[[N_levels-1]])
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
                     nuclear.abund[[N_levels - j]])
          ## unlist
          intensity_pattern_N_Nminus[[N_levels - j]] <-
            unlist(intensity_pattern_N_Nminus[[N_levels - j]],
                   use.names = FALSE)
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
  ## ========== LOOKING FOR g_iso & B_iso within SIM. DATA FRAME (`B.g.sim.df`) ===========
  #
  ## condition to find g_iso
  if (is.null(g.iso)){
    near_g_iso <- which.min(abs(B.g.sim.df$B_T - B.CF)) ## find index/row
  } else{
    near_g_iso <- which.min(abs(B.g.sim.df$g - g.iso))
  }
  near_g_iso_df <- B.g.sim.df[near_g_iso,] ## data.frame with the corresponding index
  ## and finally
  g_iso <- near_g_iso_df %>% dplyr::pull(.data$g)
  ## corresponding actual `B_iso`
  B_iso <- near_g_iso_df %>% dplyr::pull(.data$B_T)
  #
  ## ====================== DERIVATIVE/INTEGRATED LINE FORMS =========================
  #
  ## Definition for the Derivative as well as Integrated EPR Spectral Line Forms
  ## Pseudo-Voigt is only required because =>
  ## x*Gaussian(derivative) + y*Lorentzian(derivative)
  ## see also https://easyspin.org/easyspin/documentation/lineshapes.html or
  ## EPR Wertz and Bolton https://onlinelibrary.wiley.com/doi/book/10.1002/0470084987
  deriv_line_form <- function(B,
                              B.0,
                              g.x = lineG.content,
                              l.y = (1 - lineG.content),
                              gDeltaBpp = lineGL.DeltaB[[1]],
                              lDeltaBpp = lineGL.DeltaB[[2]]){
    #
    ## B.0 line center crossing `0` because of derivative form
    ## DeltaBpp linewidth
    #
    ## condition for the coefficients & line-width
    if ((is.null(lDeltaBpp) & l.y == 0) || (is.null(lDeltaBpp) & g.x == 1)){
      ## Gaussian
      intens_deriv <- g.x * (- 4 * sqrt(2/pi) * ((B - B.0)/(gDeltaBpp^3)) *
                               exp(- 2 * ((B - B.0)/gDeltaBpp)^2))
    }
    if ((is.null(gDeltaBpp) & g.x == 0) || (is.null(gDeltaBpp) & l.y == 1)){
      ## Lorentzian
      intens_deriv <- l.y * (- 16 * (1/(pi * 3 * sqrt(3))) * ((B - B.0)/lDeltaBpp^3) *
                               (1 + 4/3 * ((B - B.0)/lDeltaBpp)^2)^(-2))
    }
    if (g.x != 0 & l.y != 0 & g.x != 1 & l.y != 1 & !is.null(gDeltaBpp) & !is.null(lDeltaBpp)){
      ## x*Gaussian(derivative) + y*Lorentzian(derivative) <=> pseudo-Voigtian
      intens_deriv <- g.x * (- 4 * sqrt(2/pi) * ((B - B.0)/(gDeltaBpp^3)) *
                               exp(- 2 * ((B - B.0)/gDeltaBpp)^2)) +
        l.y * (- 16 * (1/(pi * 3 * sqrt(3))) * ((B - B.0)/lDeltaBpp^3) *
                 (1 + 4/3 * ((B - B.0)/lDeltaBpp)^2)^(-2))
    }
    #
    return(intens_deriv)
  }
  ## integral form of the spectral line
  integ_line_form <- function(B,
                              B.0,
                              g.x = lineG.content,
                              l.y = (1 - lineG.content),
                              gDeltaB = lineGL.DeltaB[[1]],
                              lDeltaB = lineGL.DeltaB[[2]]){
    #
    ## B.0 line center maximum because of integral form
    ## FWHM linewidth corresponding to `lineGL.DeltaB`
    #
    gGamma.deltaB <- gDeltaB / sqrt(2 * log(2)) ## `gDeltaB` = Gauss FWHM
    lGamma.deltaB <- lDeltaB / sqrt(3) ## `lDeltaB` = Lorentz FWHM
    ## condition for the coefficients & line-width
    if ((is.null(lDeltaB) & l.y == 0) || (is.null(lDeltaB) & g.x == 1)){
      ## Gaussian
      intens_integ <- g.x * (sqrt(2 / pi) * (1 / gGamma.deltaB) *
        exp(-2 * ((B - B.0) / gGamma.deltaB)^2))
    }
    if ((is.null(gDeltaB) & g.x == 0) || (is.null(gDeltaB) & l.y == 1)){
      ## Lorentzian
      intens_integ <- l.y * ((2 / (pi * sqrt(3))) * (1 / lGamma.deltaB) *
        (1 + (4/3) * ((B - B.0) / lGamma.deltaB)^2)^(-1))
    }
    if (g.x != 0 & l.y != 0 & g.x != 1 & l.y != 1 & !is.null(gDeltaB) & !is.null(lDeltaB)){
      ## x*Gaussian(integrated) + y*Lorentzian(integrated) <=> pseudo-Voigtian
      intens_integ <- g.x * (sqrt(2 / pi) * (1 / gGamma.deltaB) *
                               exp(-2 * ((B - B.0) / gGamma.deltaB)^2)) +
        l.y * ((2 / (pi * sqrt(3))) * (1 / lGamma.deltaB) *
                 (1 + (4/3) * ((B - B.0) / lGamma.deltaB)^2)^(-1))
    }
    #
    return(intens_integ)
  }
  #
  ## Condition for derivative or integrated line form spectrum
  line.form.cond <- grepl("deriv|Deriv",lineSpecs.form)
  #
  ## Function to calculate intensities based on pattern, magnetic flux density (from Breit-Rabi)
  ## as well as line form => either derivative or integrated one (see `length(nuclear.system) >= 1`
  ## below). This function is applied for `length(nuclear.system) >= 2`
  intensities <- function(data.frame.sim,
                          B.values.breit.rabi,
                          intensity.pattern,
                          line.form){
    #
    ## condition for derivative/integrated form
    line.form.condN <- grepl("deriv|Deriv",line.form)
    ## intensity (iterate over all Breit-Rabi values and intensity patterns) =>
    intens <-
      Map(function(u,v)
      {switch(2-line.form.condN,
              u * deriv_line_form(B = data.frame.sim[[paste0("B_",B.unit)]],B.0 = v),
              u * integ_line_form(B = data.frame.sim[[paste0("B_",B.unit)]],B.0 = v)
              )
      },
      intensity.pattern,
      B.values.breit.rabi
      )
    #
    return(intens)
    #
  }
  #
  ## ======= CALCULATING ENERGIES/Bs, INTENSITY PATTERNS + EPR SPECTRA for ALL e-N INTERACTIONS =========
  #
  if (is.null(nuclear.system)){
    ## Simulated derivative EPR spectrum if `nuclear.system = NULL` (single line, no HF structure)
    ## no natural abundance
     natur.abund <- natur.abund %>% `if`(isTRUE(natur.abund),FALSE,.)
     #
      B.g.sim.df[[Intensity.sim]] <-
        switch(2-line.form.cond,
               deriv_line_form(B = B.g.sim.df[[paste0("B_",B.unit)]],
                               B.0 = convert_B(B_iso,B.unit = "T",B.2unit = B.unit)),
               integ_line_form(B = B.g.sim.df[[paste0("B_",B.unit)]],
                               B.0 = convert_B(B_iso,B.unit = "T",B.2unit = B.unit))
      )

    #
  } else{
    #
    ## Simulated derivative EPR spectrum if `nuclear.system != NULL`
    ## Energy/Frequency/B + spectra calculations depending on number of nuclear groups
    ##
    ## --------------------------------------- COMMENT !! -------------------------------------
    ## |                                                                                       |
    ## | Similarly as for the line intensities ( see fun. `intensity_level_pattern_multiply`)  |
    ## | The B-R delta Energies/Corresponding Bs are calculated level-by-level from            |
    ## | the highest Aiso to the lowest one (depending on number of equivalent nuclei groups). |
    ## | Each level corresponds to one group. Each line from the upper level represents        |
    ## | the (B,g) center for the "spectrum"/lines from the lower one...etc.                   |
    ## | The maximum number of equivalent groups is not limited => any number of equiv. groups |
    ## |                                                                                       |
    # -----------------------------------------------------------------------------------------
    #
    ## --------------------------- (1) NUMBER of NUCLEAR GROUPS >= 1 --------------------------
    #
    ## BULDING LEVELS STEP BY STEP IT MUST BE >= 1 because
    ## the output from `length(nuclear.system >= 1)` corresponds to input
    ## for `length(nuclear.system >= 2)` !!!!
    if (length(nuclear.system) >= 1){
      ## frequency for the biggest A_iso
      B_for_m_spin_values1 <-
        sapply(1:length(m_spin_values[[1]]), function(f)
          fun_breit_rabi(A_iso = A_iso_MHz[1] * 1e+6 * Planck.const,
                         B.0 = B_iso,
                         g_nuclear = g_nuclear[1],
                         g_e_iso = g_iso,
                         spin_nuclear = spin_nuclear[1],
                         m_spin_nuclear = m_spin_values[[1]][f])$B
        )
      ## finding the indices in simulation df (data frame) => `B.g.sim.df` corresp. to B values
      near_row_for_m_spin_values1 <-
        sapply(1:length(B_for_m_spin_values1),
               function(l)
                 which.min(abs(B.g.sim.df$B_T - B_for_m_spin_values1[l])))
      ## selecting rows based on previous indices
      near_B_for_m_spin_values1 <-
        B.g.sim.df %>%
        dplyr::slice(near_row_for_m_spin_values1)
      #
      ## Spectral line intensity in `B.unit`s depending on natur. abund
      abund_nuclear1 <- combin.abund.coeff.intens[[1]] * intensity_pattern_nuclei[[1]]
      ## Simulated Spectra as a variable into nested lists
      Sim_Intensity <- c()
      #
      ## condition (`line.form.cond`) for derivative/integrated form see above
      #
      ## intensity =>
      Sim_Intensity[[1]] <-
        Map(function(u,v)
        {switch(2-line.form.cond,
                u * deriv_line_form(B = B.g.sim.df[[paste0("B_",B.unit)]],B.0 = v),
                u * integ_line_form(B = B.g.sim.df[[paste0("B_",B.unit)]],B.0 = v)
                )
          },
        abund_nuclear1,
        near_B_for_m_spin_values1[[paste0("B_",B.unit)]]
        )
      ## Sum of the spectral lines from `Sim_Intensity[[1]]` list into final sim. spectrum
      B.g.sim.df[[Intensity.sim]] <- Reduce("+",Sim_Intensity[[1]])
    }
    #
    ## --------------------------- (2) NUMBER of NUCLEAR GROUPS >= 2 ---------------------------
    #
    if (length(nuclear.system) >= 2) {
      #
      ## converting all relevant vars. to find `B` and indices into lists
      B_for_m_spin_values <- list()
      near_B_for_m_spin_values <- list()
      near_row_for_m_spin_values <- list()
      B_for_m_spin_values[[1]] <- B_for_m_spin_values1
      near_B_for_m_spin_values[[1]] <- near_B_for_m_spin_values1
      near_row_for_m_spin_values[[1]] <- near_row_for_m_spin_values1
      #
      for (n in 2:length(nuclear.system)) {
        #
        B_for_m_spin_values[[n]] <- list()
        for (e in seq(near_B_for_m_spin_values[[n - 1]]$B_T)) {
          #
          B_for_m_spin_values[[n]][[e]] <-
            sapply(1:length(m_spin_values[[n]]), function(f) {
              fun_breit_rabi(
                A_iso = A_iso_MHz[n] * 1e+6 * Planck.const,
                B.0 = near_B_for_m_spin_values[[n - 1]]$B_T[e],
                g_nuclear = g_nuclear[n],
                g_e_iso = near_B_for_m_spin_values[[n - 1]]$g[e],
                spin_nuclear = spin_nuclear[n],
                m_spin_nuclear = m_spin_values[[n]][f]
              )$B
            })
        }
        ## finding the indices in simulation df => `B.g.sim.df` corresponding to B values
        near_row_for_m_spin_values[[n]] <- list()
        for (l in seq(B_for_m_spin_values[[n]])) {
          near_row_for_m_spin_values[[n]][[l]] <-
            sapply(
              1:length(B_for_m_spin_values[[n]][[l]]),
              function(x) {
                which.min(abs(B.g.sim.df$B_T - B_for_m_spin_values[[n]][[l]][x]))
              }
            )
        }
        #
        ## summarizing the list into vector of indices
        near_row_for_m_spin_values[[n]] <-
          unlist(near_row_for_m_spin_values[[n]], use.names = FALSE)
        ## selecting the rows based on previous indices
        near_B_for_m_spin_values[[n]] <-
          B.g.sim.df %>%
          dplyr::slice(near_row_for_m_spin_values[[n]])
        #
      }
      #
      ## entire line intensities including the natural abundance
      abund_nuclearN <-
        intensity_level_pattern_multiply(
          intensity.nuclei.pattern = intensity_pattern_nuclei,
          natur.abund = natur.abund,
          nuclear.abund = combin.abund.coeff.intens
        )
      #
      ## Spectral line intensities for the last (corresp. to `length(nuclear.system)`)
      ## `near_B_for_m_spin_values` data frame
      ## see the `intensities()` function above
      Sim_Intensity[[2]] <-
        intensities(
          data.frame.sim = B.g.sim.df,
          B.values.breit.rabi = near_B_for_m_spin_values[[length(nuclear.system)]][[paste0("B_", B.unit)]],
          intensity.pattern = abund_nuclearN,
          line.form = lineSpecs.form
        )
      #
      ## Sum of the spectral lines from `Sim_Intensity[[2]]` list into final sim. spectrum
      ## done by function `Reduce()` with argument "+"
      B.g.sim.df[[Intensity.sim]] <- Reduce("+", Sim_Intensity[[2]])
    }
  }
  #
  ## ======================== DATA FRAME AND PLOTTING OF EPR SIM. SPECTRA ====================
  #
  ## Reducing columns in the final data frame
  B.g.sim.df <- B.g.sim.df %>%
    dplyr::select(dplyr::all_of(c("B_G","B_mT",Intensity.sim)))
    # dplyr::select(.data$B_G,.data$B_mT,.data[[Intensity.sim]])
  ## Plotting the EPR spectrum
  ## y-axis label depending on derivative or integrated line form
  ## First of all create title and caption character vector
  ## condition
  nucs.sys.cond <- if (is.null(nuclear.system)) TRUE else FALSE
  char.title <-
    switch(2-nucs.sys.cond,
           "Non-Interacting Paramagnetic Center/Radical",
           mapply(function(x,y,z)
           paste0("A(",x," x ",y,") = ",z," MHz"),
           N_nuclei,
           nucle_us_i,
           A_iso_MHz))
  if (!is.null(nuclear.system)){
    ## separate description into several lines
    if (length(char.title) <= 3){
        char.title <- paste(unname(char.title), collapse = ", ")
        char.title.title <- paste("EPR Spectrum Simulation with ",char.title,sep = "\n")
    }
    if (length(char.title) > 3){
      ## first line variable =>
      char.title1L <- paste(unname(char.title[1:3]), collapse = ", ") ## 1st Line
    }
    if (length(char.title) > 3 & length(char.title) <= 6){
      char.title2L <- paste(unname(char.title[4:length(char.title)]), collapse = ", ") ## 2nd Line
      char.title.title <- paste("EPR Spectrum Simulation with ",
                                char.title1L,
                                char.title2L,
                                sep = "\n")
    }
    if (length(char.title) > 6){
      ## second line variable =>
      char.title2L <- paste(unname(char.title[4:6]), collapse = ", ") ## 2nd Line
    }
    if (length(char.title) > 6 & length(char.title) <= 9){
      char.title3L <- paste(unname(char.title[7:length(char.title)]), collapse = ", ") ## 3rd Line
      char.title.title <- paste("EPR Spectrum Simulation with ",
                                char.title1L,
                                char.title2L,
                                char.title3L,
                                sep = "\n")
    }
    if (length(char.title) > 9 & length(char.title) <= 12){
      char.title3L <- paste(unname(char.title[7:9]), collapse = ", ") ## 3rd Line
      char.title4L <- paste(unname(char.title[10:length(char.title)]), collapse = ", ") ## 4th Line
      char.title.title <- paste("EPR Spectrum Simulation with ",
                                char.title1L,
                                char.title2L,
                                char.title3L,
                                char.title4L,
                                sep = "\n")
    }
    if (length(char.title) > 12){
      char.title.title <- paste("EPR Spectrum Simulation with ",
                                "> 12 Groups of Equivalent Nuclei.",
                                sep = "\n")
    }
   } else {
    char.title.title <- paste("EPR Spectrum Simulation of ",char.title,sep = "\n")
  }
  ## caption
  if (is.null(lineGL.DeltaB[[1]])){
    char.caption <- bquote(
      italic(g)[iso] == .(g.iso)~~~Delta~italic(B)[G] == 0~~.(B.unit)~~
        ~Delta~italic(B)[L] == .(lineGL.DeltaB[[2]])~~.(B.unit)
    )
  }
  if (is.null(lineGL.DeltaB[[2]])){
    char.caption <- bquote(
      italic(g)[iso] == .(g.iso)~~~Delta~italic(B)[G] == .(lineGL.DeltaB[[1]])~~.(B.unit)~~
        ~Delta~italic(B)[L] == 0~~.(B.unit)
    )
  }
  if (!is.null(lineGL.DeltaB[[1]]) & !is.null(lineGL.DeltaB[[2]])){
    char.caption <- bquote(
      italic(g)[iso] == .(g.iso)~~~Delta~italic(B)[G] == .(lineGL.DeltaB[[1]])~~.(B.unit)~~
        ~Delta~italic(B)[L] == .(lineGL.DeltaB[[2]])~~.(B.unit)
    )
  }
  #
  ylab <- switch(2-line.form.cond,
                 bquote(d * italic(I)[EPR] ~ "/" ~ d * italic(B) ~ ~"(" ~ p.d.u. ~ ")"),
                 bquote(italic(Intensity) ~ ~"(" ~ p.d.u. ~ ")")
                 )
  #
  spectrum.sim.plot <-
    ggplot(data = B.g.sim.df,
           aes(x = .data[[paste0("B_",B.unit)]],
               y = .data[[Intensity.sim]])) +
    geom_line(color = "darkviolet",linewidth = 0.75) +
    labs(title = char.title.title,
         caption = char.caption,
         x = bquote(italic(B) ~ "(" ~ .(B.unit) ~ ")"),
         y = ylab) +
    plot_theme_NoY_ticks() +
    theme(plot.title = element_text(hjust = 0.5,size = 15))
  #
  ## =============================== RESULTS ====================================
  #
  ## result list with data frame and plot
  if (isFALSE(plot.sim.interact)){
    ## B within the final data frame should be renamed to "Bsim_..."
    ## in order to be consistent with other `sim` functions
    B.g.sim.df <- B.g.sim.df %>%
      dplyr::rename_with(~ c("Bsim_G","Bsim_mT"),dplyr::all_of(c("B_G","B_mT")))
    #
    return(list(plot = spectrum.sim.plot,df = B.g.sim.df))
    #
  } else{
    #
    return(plot_EPR_Specs2D_interact(
      data.spectra = B.g.sim.df,
      x = paste0("B_", B.unit),
      x.unit = B.unit,
      Intensity = Intensity.sim,
      lineSpecs.form = lineSpecs.form
      )
    )
  }
  #
}
