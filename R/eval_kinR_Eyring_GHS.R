#'
#' Reaction Activation Parameters Obtained by Essential Transition State Theory
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   Finding the temperature-dependence of a rate constant (\eqn{k}) related to the elementary radical reaction, using the essential
#'   transition state theory (TST). The activation parameters, such as \eqn{\Delta^{\ddagger} S^o} and \eqn{\Delta^{\ddagger} H^o}
#'   are obtained either by the non-linear fit (see the general \code{\link[stats]{nls}} R function) or by the linear fit
#'   (using the \code{\link[stats]{lm}}) of the Eyring expression (or its logarithmic linear form) on the original
#'   \eqn{k} \emph{vs} \eqn{T} relation (please, refer to the \code{data.kvT} argument). The latter can be acquired
#'   by the \code{\link{eval_kinR_EPR_modelFit}} from sigmoid-integrals of the EPR spectra recorded at different temperatures.
#'   Finally, the activation Gibbs energy (\eqn{\Delta^{\ddagger} G^o}) is calculated, using the optimized \eqn{\Delta^{\ddagger} S^o}
#'   and \eqn{\Delta^{\ddagger} H^o}, for each temperature in the series.
#'
#'
#' @details
#'   The basic assumption of the Transition State Theory (TST) is the existence of activated state/complex, formed
#'   by the collision of reactant molecules, which does not actually lead to reaction products directly. The activated state (AS)
#'   is formed as highly energized, and therefore as an unstable intermediate, decomposing into products of the reaction.
#'   Accordingly, the reaction rate is given by the rate of its decomposition. Additional important assumption for TST
#'   is the presence of pre-equilibrium (characterized by the \eqn{K^{\ddagger}} constant) of the reactants with
#'   the activated complex (AC). Because the latter is not stable, it dissociates with motion along the corresponding
#'   bond-stretching coordinate. For this reason, the rate constant (\eqn{k}) must be related to the associated vibration
#'   frequency (\eqn{\nu}). Thus, every time, if the AC is formed, the \eqn{k} of AC-dissociation
#'   actually equals to \eqn{\nu}. Nevertheless, it is possible that the AC will revert back to reactants and therefore,
#'   only a fraction of ACs will lead to product(s). Such situation is reflected by the transmission coefficient \eqn{\kappa}
#'   (see also the argument \code{transmiss.coeff}), where \eqn{k = \kappa\,\nu}.
#'
#'   According to statistical thermodynamics, the equilibrium constant can be expressed by the partition function (\eqn{q})
#'   of the reactants and that of the AC. By definition, each \eqn{q} corresponds to ratio of total number of particles
#'   to the number of particles in the ground state. In essence, it is the measure of degree to which the particles
#'   are spread out (partitioned among) over the energy levels. Therefore, taking into account the energies of a harmonic quantum
#'   oscillator vibrating along the reaction coordinate as well as partition functions of the AC and those of the reactants,
#'   the rate constant can be expressed as follows (see e.g. Ptáček P, Šoukal F, Opravil T (2018) in the \code{References}):
#'   \deqn{k = \kappa\,(k_{\text{B}}\,T\,/\,h)\,K^{\ddagger}}
#'   where the \eqn{k_{\text{B}}} and \eqn{h} are the Boltzmann and Planck constants, respectively, \eqn{T} corresponds
#'   to temperature and finally, the \eqn{K^{\ddagger}} represents the equilibrium constant including the partition functions
#'   of reactants and that of the AC. In order to evaluate the AC partition function, its structure must be known.
#'   However, often, due to the lack of structural information, it is difficult (if not impossible) to evaluate
#'   the corresponding \eqn{q^{\ddagger}(\text{AC})}. Therefore, considering the equilibrium between the reactants and the AC,
#'   one may express the \eqn{K^{\ddagger}} in terms of \emph{Gibbs} activation energy (\eqn{\Delta^{\ddagger} G^o}),
#'   because \eqn{\Delta^{\ddagger} G^o = - R\,T\,ln K^{\ddagger}} and thus the Eyring equation reads:
#'   \deqn{k = \kappa\,(k_{\text{B}}\,T\,/\,h)\,exp[- (\Delta^{\ddagger} G^o)/(R\,T)] =
#'   \kappa\,(k_{\text{B}}\,T\,/\,h)\,exp[- (\Delta^{\ddagger} H^o)/(R\,T)]\,exp[\Delta^{\ddagger} S^o / R]}
#'   where \eqn{R\approx 8.31446\,\text{J}\,\text{mol}^{-1}\,\text{K}^{-1}} is the universal gas constant and the upper index \eqn{^o}
#'   denotes the standard molar state (see IUPAC (2019) in the \code{References}). Previous formula is applied
#'   as a model to fit the experimental \eqn{k\,\,vs\,\,T} (see the argument \code{data.kvT}) relation, where both
#'   the \eqn{\Delta^{\ddagger} S^o} and the \eqn{\Delta^{\ddagger} H^o} (in the graphical output, are also denoted as
#'   \eqn{\Delta^{active} S^o} and \eqn{\Delta^{active} H^o}, respectively) are optimized using the \code{fit.method}
#'   (by the \code{\link[stats]{nls}} function). In the first approach, both latter are considered as temperature independent
#'   within the selected temperature range. Often, the Eyring equation is not applied in the original form,
#'   however in the linear one,like
#'   \deqn{ln(k\,/\,T) = - (\Delta^{\ddagger} H^o)\,/\,(R\,T) + (\Delta^{\ddagger} S^o\,/\,R) + ln(\kappa\,k_{\text{B}}\,/\,h)}
#'   with the slope/coefficient \eqn{- (\Delta^{\ddagger} H^o)\,/\,R}
#'   and the \eqn{(\Delta^{\ddagger} S^o\,/\,R) + ln(\kappa\,k_{\text{B}}\,/\,h)}
#'   as intercept of the \eqn{ln(k\,/\,T)\,\,vs\,\,1/T} relation. Nevertheless, the latter is not recommended as a model for fitting
#'   the experimental \eqn{k(T)} (see also Lente G, Fábián I, Poë AJ (2005) in the \code{References}).
#'   The reason inherits in the misinterpretation of the extrapolation to \eqn{T\rightarrow \infty} (or \eqn{1/T\rightarrow 0})
#'   by which the \eqn{\Delta^{\ddagger} S^o} is obtained and thus it is unreliable. Accordingly, \strong{the original exponential
#'   Eyring form is recommended} as a model to fit the experimental \eqn{k(T)}. Contrary, it may happen that \eqn{k\,\,vs\,\,T}
#'   can vary in several (\eqn{\approx 2-3}) orders of magnitude within the studied temperature range and therefore, it is necessary
#'   to proportionally weight the \eqn{k} (see also Lente G, Fábián I, Poë AJ (2005) in the \code{References}). For the linear form,
#'   weighting does not make a difference because the transformed \eqn{ln(k\,/\,T)} span over a narrow range of values.
#'   Therefore, it is advisable to perform the original exponential fit as well as the linear one within the studied temperature range
#'   and compare both outcomes (both methods are involved in this function, see the argument \code{fit.method}).
#'   The \href{https://goldbook.iupac.org/terms/view/E02142}{\eqn{k}-unit depends on the molecularity of the reaction},
#'   please also refer to the \code{rate.const.unit} argument. Therefore, the left hand site of the Eyring equation above
#'   must be multiplied by the standard molar concentration \eqn{c^o = 1\,\text{mol}\,\text{dm}^{-3}}:
#'   \deqn{k\,(c^o)^{- \sum_i \nu_i^{\ddagger}}}
#'   where the \eqn{\sum_i \nu_i^{\ddagger}} goes through stoichiometric coefficients (including the negative sign for reactants)
#'   of the AC formation reaction (therefore the index \eqn{^{\ddagger}} is used), i.e. for the bi-molecular reaction,
#'   the sum results in \code{-1}, however for the mono-molecular one, the sum results in \code{0}.
#'
#'   While the transition state theory (TST) is a helpful tool to get information about the mechanism
#'   of an elementary reaction, it has some limitations, particularly for radical reactions. Couple of them are listed below.
#'   \enumerate{
#'   \item One should be very careful if applied to elementary steps in a multistep reaction kinetics (like
#'   consecutive reactions, example shown in \code{\link{eval_kinR_ODE_model}}). If the intermediate (e.g. in the consecutive
#'   reaction mechanism) possesses a short life-time, the TST probably fails.
#'
#'   \item For very fast reactions the assumed equilibrium between the reactants and the AC won't be reached.
#'   Therefore, the spin trapping reactions, which \eqn{k}s may actually fall into the order
#'   of \eqn{10^9\,\text{dm}^3\,\text{mol}^{-1}\,\text{s}^{-1}} (or oven higher, see Kemp TJ (1999) in the \code{References})
#'   should be taken with extreme caution in terms of TST.
#'
#'   \item Formation of AC in TST is based on classical mechanics, that is molecules/atoms will only collide,
#'   having enough energy (to form the AC), otherwise reaction does not occur. Whereas, taking into account the quantum
#'   mechanical principle, molecules/atoms with any finite energy may (with a certain probability) tunnel across
#'   the energy barrier. Such effect will be less probable for high energy barriers, however e.g. for radical-radical
#'   recombination, where the barriers are typically very low, the tunneling probability is high and TST may fail.
#'   In addition, such reactions proceed relatively fast and therefore the TST (Eyring fit) can also strongly bias
#'   the activation parameters. This type of reactions may also exhibit negative \eqn{k\,\,vs\,\,T} dependence
#'   (see Wardlaw DM and Marcus RA (1986) in the \code{References}).
#'   }
#'
#'
#' @references
#'   Engel T, Reid P (2013). \emph{Physical Chemistry, 3rd Edition}, Pearson Education, ISBN 978-0-321-81200-1,
#'   \url{https://elibrary.pearson.de/book/99.150005/9781292035444}.
#'
#'   Ptáček P, Šoukal F, Opravil T (2018). "Introduction to the Transition State Theory",
#'   \emph{InTech.}, \url{https://doi.org/10.5772/intechopen.78705}.
#'
#'   International Union of Pure and Applied Chemistry (IUPAC) (2019). “Transition State Theory”,
#'   \url{https://goldbook.iupac.org/terms/view/T06470}.
#'
#'   Anslyn EV, Dougherty DA (2006). \emph{Modern Physical Organic Chemistry}, University Science Books,
#'   ISBN 978-1-891-38931-3, \url{https://uscibooks.aip.org/books/modern-physical-organic-chemistry/}.
#'
#'   Lente G, Fábián I, Poë AJ (2005). "A common Misconception about the Eyring Equation",
#'   \emph{New J. Chem.}, \strong{29}(6), 759–760, \url{https://doi.org/10.1039/B501687H}.
#'
#'   Kemp TJ (1999), "Kinetic Aspects of Spin Trapping", \emph{Progress in Reaction Kinetics}, \strong{24}(4),
#'   287-358, \url{https://doi.org/10.3184/007967499103165102}.
#'
#'   Wardlaw DM and Marcus RA (1986), "Unimolecular reaction rate theory for transition states of any looseness.
#'   3. Application to methyl radical recombination", \emph{J. Phys. Chem.}, \strong{90}(21), 5383-5393,
#'   \url{https://doi.org/10.1021/j100412a098}.
#'
#'
#' @param data.kvT Data frame object, which must include two essential columns: rate constant (\eqn{k} of an elementary
#'   radical reaction) and the corresponding temperatures at which the \eqn{k} was acquired.
#' @param rate.const Character string, pointing to rate constant column header in the actual \code{data.kvT} data frame.
#' @param rate.const.unit Character string, referring to rate constant unit. This has to be specified using
#'   the \code{\link[grDevices]{plotmath}} notation, like \code{rate.const.unit = "M^{-1}~s^{-1}"}
#'   or \code{rate.const.unit = "s^{-1}"} (\strong{default}), because it is automatically applied as \eqn{y}-axis unit
#'   in the graphical output by the \code{{ggplot2}}.
#' @param Temp Character string, pointing to temperature column header within the original \code{data.kvT} data frame.
#' @param Temp.unit Character string, corresponding to temperature unit related to \code{Temp}. Temperature can be defined
#'   in the following units: \code{Temp.unit = "K"} (kelvin, \strong{default}), \code{Temp.unit = "oC"} (degree Celsius)
#'   or \code{Temp.unit = "oF"} (degree Fahrenheit). If other than \strong{default} specified, temperature values
#'   (column characterized by the \code{Temp} argument) are automatically converted into \code{"K"} (kelvins).
#' @param transmiss.coeff Numeric value, corresponding to probability that the activated complex is transformed into products.
#'   \strong{Default}: \code{transmiss.coeff = 1} (\eqn{100\,\%}).
#' @param fit.method Character string, corresponding to method applied to fit the theoretical Eyring relation
#'   (by optimizing the activation parameters, see \code{Details}) to the experimental \eqn{k\,\,vs\,\,T}
#'   dependence. For this purpose, either \code{fit.method = "linear"} (using the \code{\link[stats]{lm}}) or non-linear methods
#'   (available by the \code{\link[stats]{nls}} function) are applied. The latter includes \code{"default"}
#'   (corresponding to \href{https://journal.r-project.org/articles/RJ-2023-091/}{Gauss-Newton algorithm}),
#'   \code{"plinear"}, which is
#'   \href{https://geo-ant.github.io/blog/2020/variable-projection-part-1-fundamentals/}{Golub-Pereyra} algorithm
#'   or \code{"port"}
#'   (\href{https://ms.mcmaster.ca/\%7Ebolker/misc/port.pdf}{Fortran PORT, "portable" library for numerical computation}).
#'
#'
#' @return As a result of the Eyring-relation fit, list with the following components is available:
#'   \describe{
#'   \item{df}{Data frame, including the original \code{data.kvT} + the column of \eqn{\Delta^{\ddagger} G^o},
#'   with the name of \code{DeltaG_active_kJ_per_mol}, as well as \code{fitted}/predicted values of the rate constant
#'   and finally, the corresponding residuals. If \code{fit.method = "linear"} additional columns like \code{reciprocal_T}
#'   and \code{lnk_per_T} are available, corresponding to \eqn{1\,/\,T} and \eqn{ln(k\,/\,T)}, respectively.}
#'   \item{df.fit}{Data frame including temperature (in the same region like in the original \code{data.kvT},
#'   however with the resolution of 1024 points) and the corresponding \code{.fitted} \eqn{k}, according to
#'   Eyring model.}
#'   \item{plot}{Static ggplot2-based object/list, showing graphical representation of the (non-)linear fit,
#'   together with the Eyring equation.}
#'   \item{plot.ra}{GGplot2 object (related to simple \strong{r}esidual \strong{a}nalysis), including
#'   two main plots: Q-Q plot and residuals vs predicted/fitted \eqn{k} \emph{vs} \eqn{T} from the Eyring fit.}
#'   \item{df.coeffs.HS}{Data frame object, containing the optimized (best fit) parameter values (\code{Estimates}),
#'   their corresponding \code{standard errors}, \code{t-} as well as \code{p-values} for the corresponding Eyring model.}
#'   \item{df.model.HS}{Data frame object, contaning model characteristics.}
#'   \item{vec.HS.uncert}{Numeric vector, consisting of \eqn{\Delta^{\ddagger} S^o} as well as \eqn{\Delta^{\ddagger} H^o},
#'   together with their uncertainties, all in SI units like J / (mol(* K)). Calculation of uncertainties for linear model
#'   is performed by the error propagation, implemented
#'   in \href{https://r-quantities.github.io/errors/articles/rjournal.html}{\code{{errors}} R package}, using the first
#'   order Taylor series method.}
#'   \item{converg}{If \code{fit.method} IS DIFFERENT FROM \code{"linear"} a list, containing fitting/optimization
#'   characteristics like number of evaluations/iterations
#'   (\code{N.evals}); character denoting the (un)successful convergence (\code{message})
#'   and finally, standard deviation of the residuals (\code{residual.sd}), which is defined as:
#'   \deqn{\sqrt{\sum_i (y_i - y_{i,\text{fit/model}})^2\,/\,(N - k_{\text{pars}} - 1)}}
#'   where \eqn{N} is the number of observations and \eqn{k_{\text{pars}}} is the number of optimized parameters.
#'   Therefore, the smaller the \code{residual.sd}, the better the original Eyring-relation fit.}
#'   }
#'
#'
#' @examples
#' ## demonstration on raw data, presented
#' ## in https://www.rsc.org/suppdata/nj/b5/b501687h/b501687h.pdf
#' ## considering reaction H+ + (S2O6)2- <==> SO2 + (HSO4)-
#' kinet.test.data <-
#'   data.frame(k_per_M_per_s =
#'                c(9.54e-7,1.91e-6,3.76e-6,
#'                  7.33e-6,1.38e-5,2.56e-5,
#'                  4.71e-5,8.43e-5,1.47e-4),
#'              T_oC = c(50,55,60,65,70,
#'                       75,80,85,90)
#' )
#' #
#' ## original "Eyring" model
#' activ.kinet.test01.data <-
#'   eval_kinR_Eyring_GHS(
#'     data.kvT = kinet.test.data,
#'     rate.const = "k_per_M_per_s",
#'     rate.const.unit = "M^{-1}~s^{-1}",
#'     Temp = "T_oC",
#'     Temp.unit = "oC"
#'   )
#' #
#' ## preview of the original data
#' ## + ∆G (activated) + fitted + residuals
#' activ.kinet.test01.data$df
#' #
#' ## preview of the non-linear fit plot
#' activ.kinet.test01.data$plot
#' #
#' ## preview of the optimized (activated)
#' ## ∆S and ∆H parameters
#' activ.kinet.test01.data$df.coeffs.HS
#' #
#' ## compare values with those presented in
#' ## https://www.rsc.org/suppdata/nj/b5/b501687h/b501687h.pdf
#' ## ∆S = (7.2 +- 1.1) J/(mol*K) &
#' ## ∆H = (118.80 +- 0.41) kJ/mol
#' #
#' ## preview of the convergence measures
#' activ.kinet.test01.data$converg
#' #
#' ## this was an untransformed dataset,
#' ## when the k values span more than 2 orders
#' ## of magnitude => a proportional weighting
#' ## or linear logarithmic model might be applied
#' #
#' ## take the same above-defined dataset and assign
#' ## it to a new variable
#' kinet.test.data.new <-
#'   data.frame(k_per_M_per_s =
#'                c(9.54e-7,1.91e-6,3.76e-6,
#'                  7.33e-6,1.38e-5,2.56e-5,
#'                  4.71e-5,8.43e-5,1.47e-4),
#'              T_oC = c(50,55,60,65,70,
#'                       75,80,85,90)
#' )
#' #
#' ## linear logarithmic Eyring model
#' activ.kinet.test02.data <-
#'   eval_kinR_Eyring_GHS(
#'     data.kvT = kinet.test.data.new,
#'     rate.const = "k_per_M_per_s",
#'     rate.const.unit = "M^{-1}~s^{-1}",
#'     Temp = "T_oC",
#'     Temp.unit = "oC",
#'     fit.method = "linear"
#'   )
#' #
#' ## preview of the original data
#' ## + ∆G (activated) + fitted + residuals +
#' ## + additional variables of the linear model
#' activ.kinet.test02.data$df
#' #
#' ## preview of the linear fit plot
#' ## also with confidence interval (99,999999 %)
#' activ.kinet.test02.data$plot
#' #
#' ## preview of the optimized (activated)
#' ## ∆S and ∆H parameters together with
#' ## their uncertainties
#' activ.kinet.test02.data$vec.HS.uncert
#' #
#' ## compare values with those presented in
#' ## https://www.rsc.org/suppdata/nj/b5/b501687h/b501687h.pdf
#' ## ∆S = (11.33 +- 0.45) J/(mol*K) &
#' ## ∆H = (120.27 +- 0.15) kJ/mol
#' #
#' ## corresponding analysis of residuals
#' activ.kinet.test02.data$plot.ra
#'
#'
#' @export
#'
#'
#'
#' @importFrom broom tidy augment
#' @importFrom ggplot2 geom_ribbon annotate geom_smooth
eval_kinR_Eyring_GHS <- function(data.kvT,
                                 rate.const,
                                 rate.const.unit = "s^{-1}",
                                 Temp,
                                 Temp.unit = "K", ## "K" or "oC" or "oF"
                                 transmiss.coeff = 1, ## kappa
                                 fit.method = "default"){ ## "Gauss-Newton" +
                                 ## "plinear" (Golub-Pereyra) and "port" (all from `nls`) or "linear"
  #
  ## 'Temporary' processing variables
  T_K <- NULL
  DeltaG_active_kJ_per_mol <- NULL
  fitted <- NULL
  residuals <- NULL
  reciprocal_T <- NULL
  lnk_per_T <- NULL
  #
  ## ======================== TEMPERATURE + CONSTANTS ==========================
  #
  ## Pysical sonstants
  Boltzmann <- constants::syms$k ## uncertainty 0
  gas.const <- constants::syms$r ## uncertainty 0 from codata `{constants}` package
  Planck <- constants::syms$h ## uncertainty 0
  #
  ## check the units of k (rate.const)
  if (isFALSE(grepl("s-1|second|s\\^-1|s\\^{-1}|sec|per_sec_|per_s|per_second",rate.const.unit))) {
    stop(" The rate constant (k) unit must contain `seconds` ! \n
         ...such as `s^-1` or `M^{-1}~s^{-1}`...etc.\n
         Please, refer to description of the `rate.const.unit` argument !! ")
  }
  #
  ## checking whether the temperature is in K or oC or oF
  if (Temp.unit == "oC"){
    data.kvT <- data.kvT %>%
      dplyr::mutate(!!rlang::quo_name(Temp) := .data[[Temp]] + 273.15)
    #
    ## rename column, afterwards
    colnames(data.kvT)[colnames(data.kvT) == Temp] <- "T_K"

  } else if (Temp.unit == "K") {
    data.kvT <- data.kvT
    ## check the name of the temperature column and rename
    if (Temp != "T_K") {
      colnames(data.kvT)[colnames(data.kvT) == Temp] <- "T_K"
    }
    #
  } else if (Temp.unit == "oF") { ## Fahrenheit
    data.kvT <- data.kvT %>%
      dplyr::mutate(!!rlang::quo_name(Temp) := ((.data[[Temp]] - 32) * (5/9)) + 273.15)
    ## rename column afterwards
    colnames(data.kvT)[colnames(data.kvT) == Temp] <- "T_K"
  }
  #
  ## =========================== MODEL FIT ===========================
  #
  ## --------------------------- ORIGINAL (EXPONENTIAL) ----------------------------
  ## formula for the original Eyring theory
  ## ("theory of absolute reaction rates" :-))
  ## rate.const = ((kappa * k_B * T) / h) * exp((DeltaS / R) - (DeltaH / RT))
  origin.Eyring.formula.HS <-
    stats::as.formula(
      paste0(
        rate.const,
        "~",
        "(eval(transmiss.coeff) * eval(Boltzmann) * T_K/eval(Planck)) *
         exp((dS/eval(gas.const)) - (dH/(eval(gas.const) * T_K)))"
      )
    )
  #
  ## ------------------------- LINEAR --------------------------------
  ## ln(rate.const/T) = - (DeltaH / RT) + (DeltaS / R) + ln((kappa * k_B) / h)
  #
  linear.Eyring.formula.HS <-
    stats::as.formula(
      paste0(
        "log(",rate.const,"/T_K)","~", ## log = ln
        "I(- 1/(eval(gas.const) * T_K))"
      )
    )
  #
  ## -----------------------------------------------------------------
  #
  ## model to calculate H, S
  if (fit.method == "linear") {
    Eyring.model.HS <-
      stats::lm(linear.Eyring.formula.HS,
                 data = data.kvT)
  } else {
    Eyring.model.HS <-
      stats::nls(origin.Eyring.formula.HS,
                 data = data.kvT,
                 start = c(dS = 1,dH = 1),
                 algorithm = fit.method)
  }
  #
  ## parameters related to the model =>
  df.dSH.coeffs <- broom::tidy(Eyring.model.HS)
  #
  ## model summaries =>
  df.dSH.model.summar <- broom::glance(Eyring.model.HS) ## add to final output list
  #
  ## new data for the fit with 1024 points =>
  new.fit.data <- data.frame(T_K = seq(
    from = min(data.kvT$T_K), to = max(data.kvT$T_K),
    length.out = 1.024e3)
  )
  #
  new.fit.data <-
    broom::augment(Eyring.model.HS,
                   newdata = new.fit.data)
  #
  ## extraction of dS and dH depending on the model (with uncertainties)=>
  if (fit.method == "linear") {
    DeltaH_active <- df.dSH.coeffs$estimate[2]
    Intercept <- df.dSH.coeffs$estimate[1]
    DeltaS_active <- gas.const * (Intercept - log(transmiss.coeff * Boltzmann / Planck))
    #
    ## calculation of uncertainties by the error propagation implemented
    ## in `{errors}` R package (first order Taylor series method, TSM)
    ## see also https://r-quantities.github.io/errors/articles/rjournal.html:
    DeltaH_active_with_un <-
      errors::set_errors(DeltaH_active,df.dSH.coeffs$std.error[2]) ## J / mol
    #
    Intercept_with_un <- errors::set_errors(Intercept,df.dSH.coeffs$std.error[1])
    gasConst_with_un <- errors::set_errors(gas.const,0)
    transmissCoeff_with_un <- errors::set_errors(transmiss.coeff,0)
    Boltzmann_with_un <- errors::set_errors(Boltzmann,0)
    Planck_with_un <- errors::set_errors(Planck,0)
    #
    DeltaS_active_with_un <- ## J / (mol * K)
      gasConst_with_un *
      (Intercept_with_un - log(transmissCoeff_with_un * Boltzmann_with_un / Planck_with_un))
    #
  } else {
    DeltaS_active <- df.dSH.coeffs$estimate[1]
    DeltaH_active <- df.dSH.coeffs$estimate[2]
    #
    ## with uncertainties:
    DeltaH_active_with_un <-
      errors::set_errors(DeltaH_active,df.dSH.coeffs$std.error[2])
    DeltaS_active_with_un <-
      errors::set_errors(DeltaS_active,df.dSH.coeffs$std.error[1])
  }
  #
  ## Add `DeltaG_active` column:
  data.kvT <- data.kvT %>%
    dplyr::mutate(DeltaG_active_kJ_per_mol =
                    (DeltaH_active - (.data[["T_K"]] * DeltaS_active)) * 1e-3
    )
  #
  ## ================= SIMPLE RESIDUAL ANALYSIS/PLOTS ======================
  #
  ## add residuals and predicted/fitted values to `data.kvT`
  if (fit.method == "linear") {
    #
    ## new columns for linear model
    data.kvT$reciprocal_T <- 1 / data.kvT$T_K
    data.kvT <- data.kvT %>%
      dplyr::mutate(lnk_per_T = log(.data[[rate.const]] / .data$T_K))
  } else {
    data.kvT <- data.kvT
  }
  data.kvT$fitted <- stats::fitted(Eyring.model.HS)
  data.kvT$residuals <- stats::residuals(Eyring.model.HS)
  #
  ## residual plot (`{ggplot2}`)
  plot.resids <-
    ggplot(data.kvT,
           mapping = aes(
             x = fitted,y = residuals
           )
    ) +
    geom_point(size = 2.6,color = "darkblue") +
    geom_hline(yintercept = 0,color = "darkred") +
    labs(
      x = bquote(italic(Eyring~~Fit)),
      y = bquote(italic(Residuals)),
      title = "Residual Plot"
    ) +
    plot_theme_In_ticks()
  #
  ## q-q plot (`{ggplot2}`)
  plot.qq <-
    ggplot(data.kvT,
           mapping = aes(
             sample = residuals
           )
    ) +
    stat_qq(size = 2.6,color = "darkblue") +
    stat_qq_line(color = "darkred") +
    labs(
      x = bquote(italic(Theoretical~~Quantiles)),
      y = bquote(italic(Sample~~Quantiles)),
      title = "Normal Q-Q Plot of Residuals"
    ) +
    plot_theme_In_ticks()
  #
  ## optional histogram with density plot
  # ggplot(data = data.kvT,
  #   mapping = aes(
  #          x = residuals,
  #          after_stat(density)
  #        )
  # ) + geom_histogram(
  #   fill = "darkblue",
  #   alpha = 0.75,
  #   bins = 42
  # ) +
  #   geom_density(
  #     fill = "darkred",alpha = 0.42
  #   ) +
  #   labs(
  #     x = bquote(italic(Residuals)),
  #     y = bquote(italic(Density)),
  #     title = "Histogram and Density of Residuals"
  #   ) +
  #   plot_theme_In_ticks()
  #
  ## patchwork combination all both plots:
  plot.ra <-
    patchwork::wrap_plots(plot.resids,
                          plot.qq,
                          ncol = 1)
  #
  ## ===================== EYRING PLOT (GGPLOT2) ===========================
  #
  caption.string <-
    bquote(
      Delta^{active}*italic(H)^o ~ '=' ~ .(DeltaH_active*1e-3)~k*J~mol^-1~","
      ~Delta^{active}*italic(S)^o~ '=' ~ .(DeltaS_active)~~J~K^-1~mol^-1~","
      ~ kappa ~ '=' ~ .(transmiss.coeff) ~ "," ~ italic(c)^o ~ '=' ~ 1~~mol~dm^{-3}
    )
  #
  if (fit.method == "linear") {
    plot_Eyring <-
      ggplot(
        data = data.kvT,
        aes(x = reciprocal_T, y = lnk_per_T)
      ) +
      geom_point(
        color = "darkcyan",
        size = 3.2
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "magenta",
        fill = "darkgray",
        linewidth = 1.1,
        level = 0.99999999
      ) +
      labs(
        title = "Linear Eyring Fit",
        caption = caption.string,
        x = bquote(italic(1 / T)~~"("~K^-1~")"),
        y = bquote(italic(ln~"("~k/T~")")~~~~"("~p.d.u.~")")
      ) +
      annotate(
        geom = "text",
        x = 1.08 * min(data.kvT[["reciprocal_T"]]),
        y = 1.02 * max(data.kvT[["lnk_per_T"]]),
        parse = TRUE,
        size = 4,
        label = "ln(frac(italic(k)~(italic(c)^o)^{- sum(nu[i]^{active},i,)},italic(T))) ==
                 - frac(Delta^{active}*italic(H)^o,italic(R)~italic(T)) +
                 frac(Delta^{active}*italic(S)^o,italic(R)) +
                 ln(frac(kappa~italic(k)[B],italic(h)))"
      ) +
      plot_theme_In_ticks() +
      theme(plot.title = element_text(hjust = 0.5))
    #
  } else {
    plot_Eyring <-
      ggplot() +
      geom_point(data = data.kvT,
                 aes(x = T_K,
                     y = .data[[rate.const]]),
                 color = "darkcyan",size = 3.2) +
      geom_line(data = new.fit.data,
                aes(x = T_K,
                    y = .data[[".fitted"]]),
                color = "magenta",
                linewidth = 1.1) +
      #{if(interv.cnfd)geom_ribbon(aes(ymin = lwr,ymax = upr),fill = "steelblue2",alpha = 0.2)} +
      labs(title = "Eyring Fit",
           caption = caption.string,
           x = bquote(italic(T)~~"("~K~")"),
           y = bquote(italic(k)~~~"("~.(eval(parse(text = rate.const.unit)))~")")
      ) +
      plot_theme_In_ticks() +
      annotate(geom = "text",
               x = 1.04 * min(data.kvT[["T_K"]]),
               y = 0.8 * max(data.kvT[[rate.const]]),
               parse = TRUE,
               size = 4,
               label = "italic(k)~(italic(c)^o)^{- sum(nu[i]^{active},i,)} ==
                     frac(kappa~italic(k)[B]~italic(T),italic(h))~~
                     exp(scriptstyle(frac(Delta^{active}*italic(S)^o,italic(R))~~ -
                     ~~ frac(Delta^{active}*italic(H)^o,italic(R)~italic(T))))") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  #
  ## ============================== RESULTS =====================================
  #
  ## Results as list
  #
  result <- list(
    df = data.kvT, ## including DeltaG, residuals and fitted (variables for linear)
    df.fit = new.fit.data,
    plot = plot_Eyring,
    plot.ra = plot.ra,
    df.coeffs.HS = df.dSH.coeffs,
    df.model.HS = df.dSH.model.summar,
    vec.HS.uncert = c(DeltaH_active_with_un,DeltaS_active_with_un) ## both in SI (J / (mol (K)) )
  )
  #
  if (fit.method == "linear") {
    result <- result
  } else {
    result <- append(
      result,
      list(converg = list(N.evals = Eyring.model.HS[["convInfo"]]$finIter,
                     message = Eyring.model.HS[["convInfo"]]$stopMessage,
                     residual.sd = summary(Eyring.model.HS)$sigma
      )),
      after = length(result)
    )
  }
  #
  return(result)
  #
}
