#'
#' Activation Parameters (Enthalpy, Entropy and Gibbs Energy) by Transition State Theory
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   Finding the temperature-dependence of a rate constant (\eqn{k}), related to elementary radical reaction, using the transition
#'   state theory (TST). The activation parameters, such as \eqn{\Delta^{\ddagger} S^o} and \eqn{\Delta^{\ddagger} H^o}
#'   are obtained by the non-linear fit (see the general \code{\link[stats]{nls}} R function) of the Eyring expression
#'   (its non-linear form, see \code{Details}) on the original \eqn{k} \emph{vs} \eqn{T} relation (please,
#'   refer to \code{data.kvT} argument). The latter can be acquired by the \code{\link{eval_kinR_EPR_modelFit}}
#'   from sigmoid-integrals of the EPR spectra recorded at different temperatures. Finally, the activation Gibbs energy
#'   (\eqn{\Delta^{\ddagger} G^o}) is calculated, using the optimized \eqn{\Delta^{\ddagger} S^o} and \eqn{\Delta^{\ddagger} H^o},
#'   for each temperature in the series.
#'
#'
#' @details
#'   The basic assumption of Transition State Theory (TST) is the existence of activated state/complex, formed
#'   by the collision of reactant molecules, which does not actually lead to reaction products directly. The activated state (AS)
#'   is formed as highly energized, and therefore as an unstable intermediate, decomposing into products of the reaction.
#'   Accordingly, the reaction rate is given by the rate of decomposition. Additional important assumption of TST
#'   is the presence of pre-equilibrium (characterized by the \eqn{K^{\ddagger}} constant) of reactants with
#'   the activated complex (AC). Because the latter is not stable, it dissociates with motion along the corresponding
#'   bond-stretching coordinate. For this reason, the rate constant (\eqn{k}) must be related to the associated vibration
#'   frequency (\eqn{\nu}). Thus, every time, if an AC is formed, the \eqn{k} of AC-dissociation
#'   actually equals to \eqn{\nu}. Nevertheless, it is possible that the AC will revert back to reactants and therefore,
#'   only a fraction of ACs will lead to product(s). Such situation is reflected by the transmission coefficient \eqn{\kappa}
#'   (see also the argument \code{transmiss.coeff}), where \eqn{k = \kappa\,\nu}.
#'
#'   According to statistical thermodynamics, the equilibrium constant can be expressed by the partition function,
#'   which corresponds, by definition, to ratio of total number of particles to the number of particles in the ground state.
#'   In essence, it is the measure of degree to which the particles are spread out (partitioned among) over the energy levels.
#'   ...go ahead...`in thech open`...partition function and vibrations...
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
#'
#' @param data.kvT Data frame object, which must include two essential columns: rate constant (\eqn{k} of an elementary
#'   radical reaction) and the corresponding temperatures at which the \eqn{k} was acquired.
#' @param rate.const Character string, pointing to rate constant column header in the actual \code{data.kvT} data frame.
#' @param rate.const.unit Character string, referring to rate constant unit. This has to be specified using
#'   the \code{\link[grDevices]{plotmath}} notation, like \code{rate.const.unit = "M^{-1}~s^{-1}"}
#'   or \code{rate.const.unit = "s^{-1}"} (\strong{default}).
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
#'   \item{df}{Data frame including the original \code{data.kvT} and the column of \eqn{\Delta^{\ddagger} G^o}
#'   with the name of \code{DeltaG_active_kJ_per_mol}.}
#'   \item{df.fit}{Data frame including temperature (in the same region like in the original \code{data.kvT},
#'   however with the resolution of 1000 points) and the corresponding \code{.fitted} \eqn{k}, according to
#'   Eyring model.}
#'   \item{plot}{Static ggplot2-based object/list, showing graphical representation of the non-linear fit,
#'   together with the Eyring equation.}
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
#' ## preview of the original data + ∆G (activated)
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
  ## --------------------------- ORIGINAL MODEL -------------------------------
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
  ## new data for the fit with 1000 points =>
  new.fit.data <- data.frame(T_K = seq(
    from = min(data.kvT$T_K), to = max(data.kvT$T_K),
    length.out = 1e3)
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
  ## Results as list
  result <- list(df = data.kvT, ## including DeltaG
                 df.fit = new.fit.data,
                 plot = plot_Eyring,
                 df.coeffs.HS = df.dSH.coeffs,
                 converg = list(N.evals = origin.Eyring.model.HS[["convInfo"]]$finIter,
                                message = origin.Eyring.model.HS[["convInfo"]]$stopMessage,
                                residual.sd = summary(origin.Eyring.model.HS)$sigma
                 )
  )
  #
  return(results)
  #
}
