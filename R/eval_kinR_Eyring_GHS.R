#'
#' Activation Parameters (Enthalpy, Entropy and Gibbs Energy) by Transition State Theory
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   Finding the temperature-dependence of a rate constant (\eqn{k}), related to elementary radical reaction, using the essential
#'   transition state theory (TST). The activation parameters, such as \eqn{\Delta^{\ddagger} S^o} and \eqn{\Delta^{\ddagger} H^o}
#'   are obtained by the non-linear fit (see the general \code{\link[stats]{nls}} R function) of the Eyring expression
#'   (its non-linear form, see \code{Details}) on the original \eqn{k} \emph{vs} \eqn{T} relation (please,
#'   refer to the \code{data.kvT} argument). The latter can be acquired by the \code{\link{eval_kinR_EPR_modelFit}}
#'   from sigmoid-integrals of the EPR spectra recorded at different temperatures. Finally, the activation Gibbs energy
#'   (\eqn{\Delta^{\ddagger} G^o}) is calculated, using the optimized \eqn{\Delta^{\ddagger} S^o} and \eqn{\Delta^{\ddagger} H^o},
#'   for each temperature in the series.
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
#'   are spread out (partitioned among) over the energy levels. Therefore, taking into account the energies of quantum
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
#'   where \eqn{R\approx 8.31446\,\text{J\,mol^{-1}\,K^{-1}}} is the universal gas constant and the upper index \eqn{^o}
#'   denotes the standard molar state (see IUPAC (2019) in the \code{References}). Previous formula is applied
#'   as a model to fit onto the experimental \eqn{k\,\,vs\,\,T} (see the argument \code{data.kvT}) relation, where both
#'   the \eqn{\Delta^{\ddagger} S^o} and the \eqn{\Delta^{\ddagger} H^o} (in the graphical output also denoted as
#'   \eqn{\Delta^{active} S^o} and \eqn{\Delta^{active} H^o}, respectively) are optimized using the \code{fit.method}
#'   (by the \code{\link[stats]{nls}} function). Often, the Eyring equation is not applied in the original form,
#'   however in the linearized one. Nevertheless, the latter is not recommended as a model for fitting the experimental \eqn{k(T)}
#'   (see also Lente G, Fábián I, Poë AJ (2005) in the \code{References}). The reason inherits in the misinterpretation
#'   of the extrapolation to \eqn{T\rightarrow \infty} (or \eqn{1/T\rightarrow 0}) by which the \eqn{\Delta^{\ddagger} S^o}
#'   is obtained and thus it is unreliable. Accordingly, \strong{the original exponential Eyring form is recommended}
#'   as a model to fit the experimental \eqn{k(T)}.
#'   The \href{https://goldbook.iupac.org/terms/view/E02142}{\eqn{k}-unit depends on the molecularity of the reaction},
#'   please also refer to the \code{rate.const.unit} argument. Therefore, the left hand site of the Eyring equation above
#'   must be multiplied by the standard molar concentration \eqn{c^o = 1\,\text{mol}\,\text{dm^{-3}}}:
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
#'   reaction mechanism) possesses a short life-time, the TST fails.
#'
#'   \item For very fast reactions the assumed equilibrium between the reactants and the AC won't be reached.
#'   Therefore, the spin trapping reactions, which \eqn{k}s may actually fall into the order
#'   of \eqn{10^9\,\text{dm}^3\,\text{mol}^{-1}\,\text{s}^{-1}} (or oven higher, see Kemp TJ (1999) in the \code{References})
#'   should be taken with caution in terms of TST.
#'
#'   \item Formation of AC in TST is based on classical mechanics, that is molecules/atoms will only collide,
#'   having enough energy (to form the AC), otherwise reaction does not occur. Whereas, taking into account the quantum
#'   mechanical principle, molecules/atoms with any finite energy may (with a certain probability) tunnel across
#'   the energy barrier. Such effect will be less probable for high energy barriers, however e.g. for radical-radical
#'   recombination, where the barriers are typically very low, the tunneling probability is high and TST may fail.
#'   In addition, such reactions proceed relatively fast and therefore the TST (Eyring fit) can also strongly bias
#'   the activation parameters. This type of reactions may also exhibit negative \eqn{k} \emph{vs} \eqn{T} dependence
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
#'   or \code{rate.const.unit = "s^{-1}"} (\strong{default}) because it is automatically applied as \eqn{y}-axis unit
#'   in the graphical output by the \code{{ggplot2}}.
#' @param Temp Character string, pointing to temperature column header within the original \code{data.kvT} data frame.
#' @param Temp.unit Character string, corresponding to temperature unit related to \code{Temp}. Temperature can be defined
#'   in the following units: \code{Temp.unit = "K"} (kelvin, \strong{default}), \code{Temp.unit = "oC"} (degree Celsius)
#'   or \code{Temp.unit = "oF"} (degree Fahrenheit). If other than \strong{default} specified, temperature values
#'   (column characterized by the \code{Temp} argument) are automatically converted into \code{"K"} (kelvins).
#' @param transmiss.coeff Numeric value, corresponding to probability that the activated complex is transformed into products.
#'   \strong{Default}: \code{transmiss.coeff = 1} (\eqn{100\,\%}).
#' @param fit.method Character string, corresponding to method applied to fit the theoretical Eyring relation
#'   (by optimizing the activation parameters, see \code{Details}) to the experimental \eqn{k} \emph{vs} \eqn{T}
#'   dependence. For this purpose, the \code{\link[stats]{nls}} function is used. Therefore, all the methods, defined
#'   under its \code{algorithm} argument, are available: \code{"default"}
#'   (corresponding to \href{https://journal.r-project.org/articles/RJ-2023-091/}{Gauss-Newton algorithm}),
#'   \code{"plinear} (which is
#'   \href{https://geo-ant.github.io/blog/2020/variable-projection-part-1-fundamentals/}{Golub-Pereyra} algorithm)
#'   or \code{"port"} (\href{https://ms.mcmaster.ca/%7Ebolker/misc/port.pdf}{Fortran PORT ("portable") library for numerical
#'   computation}).
#'
#'
#' @return As a result of the Eyring-relation fit, list with the following components is available:
#'   \describe{
#'   \item{df}{Data frame, including the original \code{data.kvT} and the column of \eqn{\Delta^{\ddagger} G^o}
#'   with the name of \code{DeltaG_active_kJ_per_mol}.}
#'   \item{df.fit}{Data frame including temperature (in the same region like in the original \code{data.kvT},
#'   however with the resolution of 1024 points) and the corresponding \code{.fitted} \eqn{k}, according to
#'   Eyring model.}
#'   \item{plot}{Static ggplot2-based object/list, showing graphical representation of the non-linear fit,
#'   together with the Eyring equation.}
#'   \item{plot.ra}{GGplot2 object (related to simple \strong{r}esidual \strong{a}nalysis), including
#'   two main plots: Q-Q plot and residuals vs predicted/fitted \eqn{k} \emph{vs} \eqn{T} from the Eyring fit.}
#'   \item{df.coeffs.HS}{Data frame object, containing the optimized (best fit) parameter values (\code{Estimates}),
#'   their corresponding \code{standard errors}, \code{t-} as well as \code{p-values} for both \eqn{\Delta^{\ddagger} H^o}
#'   and \eqn{\Delta^{\ddagger} S^o}.}
#'   \item{converg}{List, containing fitting/optimization characteristics like number of evaluations/iterations
#'   (\code{N.evals}); character denoting the (un)successful convergence (\code{message})
#'   and finally, standard deviation of the residuals (or the residual standard error, \code{residual.sd}),
#'   which is defined as:
#'   \deqn{\sqrt{(\sum_i (y_i - y_{i,\text{fit/model}})^2)\,/\,(N - k_{\text{pars}} - 1)}}
#'   where \eqn{N} is the number of observations and \eqn{k_{\text{pars}}} is the number of optimized parameters.
#'   Therefore, the smaller the \code{residual.sd}, the better the Eyring-relation fit.}
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
#' activ.kinet.test.data <-
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
#' activ.kinet.test.data$df
#' #
#' ## preview of the non-linear fit plot
#' activ.kinet.test.data$plot
#' #
#' ## preview of the optimized (activated)
#' ## ∆S and ∆H parameters
#' activ.kinet.test.data$df.coeffs.HS
#' #
#' ## compare values with those presented in
#' ## https://www.rsc.org/suppdata/nj/b5/b501687h/b501687h.pdf
#' ## ∆S = (7.2 +- 1.1) kJ/mol*K & ∆H = (118.80 +- 0.41) kJ/mol
#' #
#' ## preview of the new `.fitted` data frame
#' activ.kinet.test.data$df.fit
#' #
#' ## preview of the convergence measures
#' activ.kinet.test.data$converg
#'
#'
#' @export
#'
#'
#'
#' @importFrom broom tidy augment
#' @importFrom ggplot2 geom_ribbon
eval_kinR_Eyring_GHS <- function(data.kvT,
                                 rate.const,
                                 rate.const.unit = "s^{-1}",
                                 Temp,
                                 Temp.unit = "K", ## "K" or "oC" or "oF"
                                 transmiss.coeff = 1, ## kappa
                                 fit.method = "default"){ ## "Gauss-Newton" +
                                 ## "plinear" (Golub-Pereyra) and "port" (all from `nls`)
  #
  ## 'Temporary' processing variables
  T_K <- NULL
  DeltaG_active_kJ_per_mol <- NULL
  #
  ## ======================== TEMPERATURE + CONSTANTS ==========================
  #
  ## Pysical sonstants
  Boltzmann <- constants::syms$k
  gas.const <- constants::syms$r
  Planck <- constants::syms$h
  #
  ## check the units of k (rate.const)
  if (isFALSE(grepl("s-1|second|s\\^-1|s\\^{-1}|sec|per_sec_|per_s|per_second",rate.const.unit))) {
    stop(" The rate constant (k) unit must contain `seconds` ! \n
         ...such as `s^-1` or `M^{-1}~s^{-1}`...etc !! ")
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
  ## =========================== ORIGINAL MODEL FIT ===========================
  #
  ## formula for the original Eyring theory
  ## ("theory of absolute reaction rates" :-))
  ## rate.const = ((kappa * k_B * T) / h) * exp((DeltaS / R) - (DeltaH / RT))
  origin.Eyring.formula.HS <-
    as.formula(
      paste0(
        rate.const,
        "~",
        "(eval(transmiss.coeff) * eval(Boltzmann) * T_K/eval(Planck)) *
         exp((dS/eval(gas.const)) - (dH/(eval(gas.const) * T_K)))"
      )
    )
  #
  ## model to calculate H, S
  origin.Eyring.model.HS <-
    stats::nls(origin.Eyring.formula.HS,
               data = data.kvT,
               start = c(dS = 1,dH = 1),
               algorithm = fit.method)
  #
  ## parameters related to the model =>
  df.dSH.coeffs <- broom::tidy(origin.Eyring.model.HS)
  #
  ## new data for the fit with 1024 points =>
  new.fit.data <- data.frame(T_K = seq(
    from = min(data.kvT$T_K), to = max(data.kvT$T_K),
    length.out = 1.024e3)
  )
  #
  new.fit.data <-
    broom::augment(origin.Eyring.model.HS,
                   newdata = new.fit.data)
  #
  ## extraction of dS and dH =>
  DeltaS_active <- df.dSH.coeffs$estimate[1]
  DeltaH_active <- df.dSH.coeffs$estimate[2]
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
  data.kvT$fitted <- stats::fitted(origin.Eyring.model.HS)
  data.kvT$residuals <- stats::residuals(origin.Eyring.model.HS)
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
      x = bquote(italic(Eyring~~Fit)*","~~italic(k)),
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
  plot_Eyring <-
    ggplot() +
    geom_point(data = data.kvT,
               aes(x = T_K,
                   y = .data[[rate.const]]),
               color = "darkcyan",size = 3.2) +
    geom_line(data = new.fit.data,
              aes(x = T_K,
                  y = .fitted),
              color = "magenta",
              linewidth = 1.1) +
    #{if(interv.cnfd)geom_ribbon(aes(ymin = lwr,ymax = upr),fill = "steelblue2",alpha = 0.2)} +
    labs(title = "Eyring Fit",
         caption = bquote(Delta^{active}*italic(H)^o ~ '=' ~ .(DeltaH_active*1e-3)~k*J~mol^-1~","
                          ~Delta^{active}*italic(S)^o~ '=' ~ .(DeltaS_active)~~J~K^-1~mol^-1~","
                          ~ kappa ~ '=' ~ .(transmiss.coeff) ~ "," ~ italic(c)^o ~ '=' ~ 1~~mol~dm^{-3}),
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
  #
  ## ============================== RESULTS =====================================
  #
  ## Results as list
  result <- list(df = data.kvT, ## including DeltaG, residuals and fitted
                 df.fit = new.fit.data,
                 plot = plot_Eyring,
                 plot.ra = plot.ra,
                 df.coeffs.HS = df.dSH.coeffs,
                 converg = list(N.evals = origin.Eyring.model.HS[["convInfo"]]$finIter,
                                message = origin.Eyring.model.HS[["convInfo"]]$stopMessage,
                                residual.sd = summary(origin.Eyring.model.HS)$sigma
                 )
  )
  #
  return(result)
  #
}
