#'
#' Simulation of Isotropic EPR Spectra Consisting of Several Components
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @description
#' A short description...tbc...
#'
#'
#' @inheritParams eval_sim_EPR_iso
#' @param g.iso.vec Numeric vector...tbc...
#' @param nuclear.system Nested list ...tbc...
#' @param natur.abund.vec Logical vector ...tbc ...
#' @param lineGL.DeltaB Nested list ...tbc ...
#' @param lineG.content.vec Numeric vector ...tbc...
#' @param Intensity.sim.coeffs.vec Numeric vector ...tbc...
#' @param plot.sim.interact Character string "components" or "sum" or NULL (\code{default})
#'
#'
#' @return List of the following data frames and plots in case of
#'   \code{plot.sim.interact = NULL} =>
#'    \describe{
#'    \item{df}{Long-format data frame with simulation components A, B, C, ...
#'     (e.g. representing the individual radicals) as categorical variable
#'     + magnetic flux density and intensity.}
#'     \item{df.areas}{Data frame with simulation components A, B, C ...and their
#'     corresponding double/single integrals (`areas`) and with their related ratios
#'     to overall integral sum (`weighted_areas`).}
#'     \item{df.sum}{Data frame with the overall intensity (+ magnetic flux density)
#'     as well as integral sum from all simulation components.}
#'     \item{plot.comps}{Overlay plot object with all simulated components with their
#'     corresponding intensities defined by \code{Intensity.sim.coeffs.vec}.}
#'     \item{plot.sum}{Plot object displaying the sum of all simulation components.}
#'    }
#'   If \code{plot.sim.interact} is activated (i.e. possesses either \code{"components"}
#'   or \code{"sum"} values) interactive plots (based on `plotly`) are presented either with
#'   all individual components or with the overall simulated sum EPR spectrum, respectively.
#'
#'
#' @examples
#' ## Simulation of EPR spectrum of TEMPO (nitroxyl) radical with 13C satellites
#' ## and hyperfine coupling constants A(1 x 14N) = 45 MHz and A(1 x 13C) = 18 MHz.
#' sim.tempo.13c <- eval_sim_EPR_iso_combo(g.iso.vec = c(2.0059,2.0059),
#'                                         nuclear.system = list(list("14N",1,45),
#'                                                               list(list("14N",1,45),
#'                                                                    list("13C",1,18))),
#'                                         natur.abund.vec = c(TRUE,TRUE),
#'                                         lineGL.DeltaB = list(list(1.2,NULL),
#'                                                              list(1.2,NULL)),
#'                                         lineG.content.vec = c(1,1),
#'                                         Intensity.sim.coeffs.vec = c(1,1))
#' sim.tempo.13c$plot.sum + ggplot2::coord_cartesian(xlim = c(3425,3550))
#' #
#' ## ...and the corresponding data frame =>
#' sim.tempo.13c$df.sum[1000:1005,]
#' #
#' ## The data frame with all components =>
#' sim.tempo.13c$df[1000:1005,]
#' #
#' ## The data frame with all integrals (areas)
#' sim.tempo.13c$df.areas
#'
#'
#' @export
#'
#'
eval_sim_EPR_iso_combo <- function(g.iso.vec, ## e.g. c(2.0027,1.9999,2.0059)
                                   instrum.params =c(Bcf = 3500,
                                                     Bsw = 200,
                                                     Npoints = 2048,
                                                     mwGHz = 9.8),
                                   B.unit = "G", ## default
                                   path_to_dsc_par = NULL, ## default
                                   origin = NULL, ## default
                                   nuclear.system, ## e.g. list(list("1H",2,24),
                                   ##           NULL,
                                   ##           list(list("14N",1,45),
                                   ##.               list("1H",4,15)))
                                   natur.abund.vec, ## e.g. c(TRUE,FALSE,TRUE)
                                   lineSpecs.form = "derivative", ## default additional see `sim` function
                                   lineGL.DeltaB, ## e.g. list(list(1,NULL),list(3,NULL),list(1,NULL))
                                   lineG.content.vec, ## e.g. c(1,1,1)
                                   Intensity.sim = "dIeprSim_over_dB", ## default
                                   Intensity.sim.coeffs.vec, ## e.g. c(2,10,0.2)
                                   plot.sim.interact = NULL){ ## default or "components" or "sum"
  #
  ## 'Temporary' processing variables
  Sim_sigmoid_Integs <- NULL
  weighted_Sim_areas <- NULL
  #
  ## all data frames of simulated spectra corresponding to each nuclear group in one list
  df.systems <-
    Map(
      function(o,p,q,r,s)
      {eval_sim_EPR_iso(g.iso = o,
                        instrum.params = instrum.params,
                        B.unit = B.unit,
                        path_to_dsc_par = path_to_dsc_par,
                        origin = origin,
                        nuclear.system = p,
                        natur.abund = q,
                        lineSpecs.form = lineSpecs.form,
                        lineGL.DeltaB = r,
                        lineG.content = s,
                        Intensity.sim = Intensity.sim)$df
      },
      g.iso.vec,
      nuclear.system,
      natur.abund.vec,
      lineGL.DeltaB,
      lineG.content.vec)
  #
  ## Multiplication of the individual data frame `Intensity.sim`
  ## by the corresponding components of coeffs.weight.vctr
  Intensity.sim.weight.vec <- Intensity.sim.coeffs.vec / sum(Intensity.sim.coeffs.vec)
  df.systems.weighted <-
    Map(function(u,v)
    { df.systems[[u]] <- df.systems[[u]] %>%
      dplyr::mutate(!!rlang::quo_name(Intensity.sim) := v * .data[[Intensity.sim]]) },
    seq(df.systems),
    Intensity.sim.weight.vec
    )
  #
  ## LONG-TABLE FORMAT FOR OVERLAY SPECTRA
  #
  ## `df.systems.weight` into long table format
  ## however, before generating the alphabet character vector in order to
  ## replace numbers by alphabet characters after the `bind_rows()`
  character.component.vec <- sapply(1:length(df.systems.weighted), function(k) LETTERS[k])
  df.systems.weighted.long <- dplyr::bind_rows(df.systems.weighted,
                                               .id = "Sim_Components")
  ## convert character numbers into alphabet characters defined by `character.component.vec`
  df.systems.weighted.long$Sim_Components <-
    factor(df.systems.weighted.long$Sim_Components,
           labels = character.component.vec)
  #
  ## Integration
  df.systems.weighted.integ.long <- df.systems.weighted.long %>%
    dplyr::group_by(.data$Sim_Components) %>%
    dplyr::mutate(Sim_sigmoid_Integs =
                    eval_integ_EPR_Spec(dplyr::pick(dplyr::all_of(c(paste0("Bsim_",B.unit),
                                                                    Intensity.sim))),
                                        B = paste0("Bsim_",B.unit),
                                        B.unit = B.unit,
                                        Intensity = Intensity.sim,
                                        lineSpecs.form = lineSpecs.form,
                                        sigmoid.integ = TRUE,
                                        output.vecs = TRUE)$sigmoid)
  ## Areas
  df.systems.areas <- df.systems.weighted.integ.long %>%
    dplyr::group_by(.data$Sim_Components) %>%
    dplyr::summarize(Sim_areas = max(.data$Sim_sigmoid_Integs))
  #
  ## WIDE-TABLE FORMAT FOR THE SIM. EPR SPECTRAL SUM rowwise by `rowSums`
  #
  df.systems.weighted.wide <- df.systems.weighted.long %>%
    tidyr::pivot_wider(names_from = dplyr::all_of(c("Sim_Components")),
                       values_from = dplyr::all_of(c(Intensity.sim))) %>%
    dplyr::mutate(!!rlang::quo_name(paste0(Intensity.sim,"_Sum")) :=
                    rowSums(dplyr::across(dplyr::matches("^[[:upper:]]$"))))
  #
  ## Overall Integration
  df.systems.weighted.wide$Sim_sigmoid_Integ <-
    eval_integ_EPR_Spec(df.systems.weighted.wide,
                        B = paste0("Bsim_",B.unit),
                        B.unit = B.unit,
                        Intensity = paste0(Intensity.sim,"_Sum"),
                        lineSpecs.form = lineSpecs.form,
                        sigmoid.integ = TRUE,
                        output.vecs = TRUE)$sigmoid
  ## delete/-select columns A,B,C,...etc
  df.systems.weighted.wide <-
    df.systems.weighted.wide %>%
    dplyr::select(dplyr::all_of(c("Bsim_mT",
                                  "Bsim_G",
                                  "Sim_sigmoid_Integ",
                                  paste0(Intensity.sim,"_Sum"))))
  ## instead of =>
  # dplyr::select(.data$B_mT,
  #               .data$B_G,
  #               .data$Sim_sigmoid_Integ,
  #               .data[[paste0(Intensity.sim,"_Sum")]])
  #
  ## Overall Sum
  systems.area <- max(df.systems.weighted.wide$Sim_sigmoid_Integ)
  #
  ## new weighted sum column in `df.systems.areas`
  df.systems.areas <- df.systems.areas %>%
    dplyr::mutate(weighted_Sim_areas = .data$Sim_areas / systems.area)
  #
  ## SUMMARIZING ALL THE RESULTS (AT THE END IT WILL BE LIST OF DFs and PLOTS)
  #
  ## overlay plot from the long table format
  ## y-axis label depending on derivative or integrated line form
  if (lineSpecs.form == "derivative"){
    ylab <- bquote(d * italic(I)[EPR] ~ "/" ~ d * italic(B) ~ ~"(" ~ p.d.u. ~ ")")
  }
  if (lineSpecs.form == "integrated" || lineSpecs.form == "absorption"){
    ylab <- bquote(italic(Intensity) ~ ~"(" ~ p.d.u. ~ ")")
  }
  #
  plot.sim.comps.overlay <-
    ggplot(data = df.systems.weighted.long,
           aes(x = .data[[paste0("Bsim_",B.unit)]],
               y = .data[[Intensity.sim]],
               color = .data$Sim_Components)) +
    geom_line(linewidth = 0.75) +
    labs(color = "Simulation\nComponents",
         x = bquote(italic(B) ~ "(" ~ .(B.unit) ~ ")"),
         y = ylab) +
    plot_theme_NoY_ticks() +
    scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    theme(legend.title = element_text(size = 14),
          legend.text = element_text(size = 13))
  #
  ## the above corresponding data frame (incl. integrals/areas)
  # `df.systems.weighted.integ.long`
  #
  ## data frame with component areas/integrals
  # see `df.systems.areas`
  #
  ## the overall/sum plot
  plot.sim.sum <-
    ggplot(data = df.systems.weighted.wide) +
    geom_line(aes(x = .data[[paste0("Bsim_",B.unit)]],
                  y = .data[[paste0(Intensity.sim,"_Sum")]]),
              linewidth = 0.75,
              color = "blue") +
    labs(x = bquote(italic(B) ~ "(" ~ .(B.unit) ~ ")"),
         y = ylab) +
    plot_theme_NoY_ticks() +
    scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL))
  #
  ## list of the all results and/or plots
  if (is.null(plot.sim.interact)){
    result.sim <- list(df = df.systems.weighted.integ.long,
                       df.areas = df.systems.areas,
                       df.sum = df.systems.weighted.wide,
                       plot.comps = plot.sim.comps.overlay,
                       plot.sum = plot.sim.sum)
  } else{
    if (plot.sim.interact == "components"){
      result.sim <- plot_EPR_Specs2D_interact(data.spectra = df.systems.weighted.long,
                                              x = paste0("Bsim_",B.unit),
                                              x.unit = B.unit,
                                              Intensity = Intensity.sim,
                                              var2nd.series = "Sim_Components",
                                              lineSpecs.form = lineSpecs.form,
                                              legend.title = "Sim. Components",
                                              legend.title.size = 14)
    }
    if (plot.sim.interact == "sum"){
      result.sim <- plot_EPR_Specs2D_interact(data.spectra = df.systems.weighted.wide,
                                              x = paste0("Bsim_",B.unit),
                                              x.unit = B.unit,
                                              Intensity = paste0(Intensity.sim,"_Sum"),
                                              lineSpecs.form = lineSpecs.form)
    }
  }
  return(result.sim)
}
