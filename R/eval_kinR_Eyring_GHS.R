#'
#' Activation Parameters (Enthalpy, Entropy and Gibbs Energy) by Transition State Theory
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   A short description...
#'
#'
#' @details
#'   Additional details...
#'
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
#'   (column characterized by \code{Temp} argument) are automatically converted into \code{"K"} (kelvins).
#' @param transmiss.coeff Numeric, ... TBC ...
#' @param fit.method Character string, corresponding to method applied to fit the theoretical Eyring relation
#'   (by optimizing the activation parameters, see \code{Details}) to the experimental \eqn{k} \emph{vs} \eqn{T}
#'   dependence. For this purpose, the \code{\link[stats]{nls}} function is used. Therefore, all the methods, defined
#'   under its \code{algorithm} argument, are available: \code{"default"}
#'   (corresponding to \href{https://journal.r-project.org/articles/RJ-2023-091/}{Gauss-Newton algorithm}),
#'   \code{"plinear} (which is
#'   \href{https://geo-ant.github.io/blog/2020/variable-projection-part-1-fundamentals/}{Golub-Pereyra} algorithm)
#'
#'
#'
#' @return
#'
#'
#'
#' @examples
#'
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
                                 fit.method = "default"){ ## "Gauss-Newton" + "plinear" (Golub-Pereyra) and "port" (all from `nls`)
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
