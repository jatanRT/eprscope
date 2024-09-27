#
#' A Comparison of EPR Spectra Between Experimental and Simulated Form
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#'   Presenting experimental as well as simulated/fitted EPR spectra together
#'   in one \href{https://ggplot2.tidyverse.org/}{ggplot2} object. They are visualized
#'   in two possible modes. Despite the fact that quite often two spectra are compared in overlay mode,
#'   sometimes it is rather difficult to figure out differences between the experimental
#'   and the simulated EPR spectrum by such manner (the details can be "hidden" in overlays).
#'   Therefore, there is also an option to offset the EPR simulated spectrum underneath the experimental
#'   one, see argument \code{Intensity.shift.ratio}. For the sake of presentation, the maximal
#'   and the minimal intensity difference, of the simulated spectrum, is automatically scaled onto
#'   the experimental one.
#'
#'
#' @param data.spectr.expr Data frame object related to experimental spectrum including the magnetic flux
#'   density (in \code{mT} or \code{G}) column, which can be labeled as \code{B_mT}
#'   in mT (or \code{B_G} in gauss), and the intensity column such as \code{dIepr_over_dB}, where
#'   the \code{index} column can be included as well.
#' @param data.spectr.sim Data frame object related to experimental spectrum including the magnetic flux
#'   density (in \code{mT} or \code{G}) column, which can be labeled as \code{Bsim_mT}
#'   in mT (or \code{Bsim_G} in gauss), and the intensity column such as \code{dIeprSim_over_dB}.
#'   These column names are acquired automatically if function like the \code{\link{readEPR_Sim_Spec}}
#'   or the \code{\link{eval_sim_EPR_iso}} is used to get the simulated spectrum data in ASCII.
#' @param B.unit Character string pointing to unit of magnetic flux density (coming from the original
#'   datasets) which is to be presented on \eqn{B}-axis of the EPR spectrum.
#'   Strings like \code{"G"} (`Gauss`) (\strong{default}) or \code{"mT"} (`millitesla`) can be used.
#' @param Intensity.expr Character string referring to intensity column name if other
#'   than \code{dIepr_over_dB} name/label is used (e.g. for integrated spectra),
#'   \strong{default}: \code{Intesity = "dIepr_over_dB"}.
#' @param Intensity.sim Character string referring to intensity column name of the simulated spectrum
#'   if other than \code{dIeprSim_over_dB} name/label is used (e.g. for integrated spectra),
#'   \strong{default}: \code{Intesity = "dIeprSim_over_dB"}, which is automatically set if
#'   the \code{\link{readEPR_Sim_Spec}} function is used to read the spectrum in ASCII.
#' @param Intensity.shift.ratio Numeric (\strong{CANNOT BE \code{0}}), showing how 'far' is the simulated
#'   EPR spectrum presented underneath the experimental one. The lower the ratio, the 'deeper' the simulated
#'   spectrum offset, \strong{default}: \code{Intensity.shift.ratio = 1.2}, other common
#'   values : \code{0.6},\code{0.8}, \code{1.2},\code{1.1}. If the \code{Intensity.shift.ratio = NULL},
#'   BOTH SPECTRA ARE PRESENTED IN OVERLAY MODE !
#' @param B.shift Numeric, difference between the \eqn{B_{center}} of simulated and experimental spectrum,
#'   that can be caused by switching ON the Teslameter. It refers to simulated spectrum, \strong{default}:
#'   \code{B.shift = 0} (\strong{NOTE}: It depends on the \code{B} parameter. If \code{B.unit = "mT"} =>
#'   \code{B.shift} must be in \code{mT}, or if \code{B.unit = "G"} then \code{B.shift} must be in \code{G}).
#' @param lineSpecs.form Character string describing either \code{"derivative"} (\strong{default})
#'   or \code{"integrated"} (i.e. \code{"absorption"} or sigmoid-integrated which can be used as well)
#'   line form of the analyzed EPR spectrum/data.
#' @param line.color.expr Character string, line color to plot simple EPR spectrum. All \code{ggplot2}
#'   compatible colors are allowed (see also \code{\link{plot_EPR_Specs}}), \strong{default}:
#'   \code{line.color = "red"}, should be different from \code{line.color.sim}.
#' @param line.color.sim Character string, line color to plot simple EPR spectrum. All \code{ggplot2}
#'   compatible colors are allowed, \strong{default}: \code{line.color = "blue"}, should be different
#'   from \code{line.color.expr}.
#' @param line.width Numeric, linewidth of the plot line in \code{mm}, \strong{default}:
#'   \code{line.width = 0.75}.
#' @param output.df Logical, whether the data frame, corresponding to graphic spectra comparison
#'   (this actually corresponds to merging of the above-mentioned data frames
#'   by \code{\link[dplyr:bind]{dplyr::bind_cols}}) should be returned as well (\code{output.df = TRUE}).
#'   In such case, the output is \code{list} containing plot (\code{list$plot}) as well as
#'   the data frame (\code{list$df}). However, the \strong{default} is \code{output.df = FALSE},
#'   i.e. only graphical representation is shown.
#'
#'
#' @return Plot object (list) of the experimental and the simulated EPR (in derivative or integrated form)
#'   spectrum or \code{list} consisting of \code{plot} and the corresponding data frame \code{df}.
#'   Any output plot corresponds to simple \code{ggplot2} object and can be combined with a desired \code{theme}
#'   (e.g. with \code{\link{plot_theme_NoY_ticks}}).
#'
#'
#' @examples
#' ## load package built-in EPR spectral
#' ## data example:
#' data.file.path <-
#'   load_data_example(file =
#'     "TMPD_specelchem_accu_b.asc")
#' data.spectrum.expr <-
#'   readEPR_Exp_Specs(path_to_ASC =
#'              data.file.path,
#'     col.names = c("B_G",
#'     "dIepr_over_dB"),
#'     qValue = 3500,
#'     origin = "winepr"
#'    )
#' #
#' ## instrumental parameters for the spectrum,
#' ## by the `WinEPR` spectrometer software
#' tmpd.params.file <-
#'   load_data_example(file =
#'     "TMPD_specelchem_accu_b.par")
#' #
#' ## simulation of the TMPD radical cation
#' ## EPR spectrum
#' data.spectrum.sim <-
#'   eval_sim_EPR_iso(g.iso = 2.00303,
#'     instrum.params = NULL,
#'     path_to_dsc_par = tmpd.params.file,
#'     origin = "winepr",
#'     nuclear.system = list(
#'       list("14N", 2, 19.3),
#'       list("1H", 4, 5.5),
#'       list("1H", 12, 19.7)
#'     ),
#'   lineGL.DeltaB = list(0.46, 0.55),
#'   lineG.content = 0.4,
#' )
#' #
#' ## comparison of both spectra
#' ## by the simulated spectrum offset
#' present_EPR_Sim_Spec(
#'   data.spectr.expr = data.spectrum.expr,
#'   data.spectr.sim = data.spectrum.sim$df
#' ) + plot_theme_NoY_ticks(legend.text =
#'            ggplot2::element_text(size = 13)
#'            )
#' #
#' ## comparison of both spectra
#' ## in overley mode
#' present_EPR_Sim_Spec(
#'   data.spectr.expr = data.spectrum.expr,
#'   data.spectr.sim = data.spectrum.sim$df,
#'   Intensity.shift.ratio = NULL,
#' ) + plot_theme_NoY_ticks(legend.text =
#'            ggplot2::element_text(size = 13)
#'            )
#'
#'
#' @export
#'
#'
#'
present_EPR_Sim_Spec <- function(data.spectr.expr,
                                 data.spectr.sim,
                                 B.unit = "G",
                                 Intensity.expr = "dIepr_over_dB",
                                 Intensity.sim = "dIeprSim_over_dB",
                                 Intensity.shift.ratio = 1.2, ## ca be also `NULL`
                                 B.shift = 0,
                                 lineSpecs.form = "derivative",
                                 line.color.expr = "red",
                                 line.color.sim = "blue",
                                 line.width = 0.75,
                                 output.df = FALSE) {
  #
  ## 'Temporary' processing variables
  B <- NULL
  #
  ## Join both tables/data frames
  both.spectr.data <- dplyr::bind_cols(data.spectr.expr,
                                       data.spectr.sim)
  #
  ## Differences in intensity extremes:
  diff_Intens_expr <-
    max(both.spectr.data[[Intensity.expr]]) -
    min(both.spectr.data[[Intensity.expr]])
  diff_Intens_sim <-
    max(both.spectr.data[[Intensity.sim]]) -
    min(both.spectr.data[[Intensity.sim]])
  #
  ## Ratio of both above:
  Intens_ratio <- diff_Intens_sim / diff_Intens_expr
  #
  ## scaling the Sim. EPR spectrum intensity to match
  ## the Exp. one (new column `Norm_...`):
  if (Intens_ratio != 1) {
    both.spectr.data[[paste0("Norm_", Intensity.sim)]] <-
      both.spectr.data[[Intensity.sim]] / Intens_ratio
    both.spectr.data[[paste0("Norm_", Intensity.sim)]] <-
      both.spectr.data[[paste0("Norm_", Intensity.sim)]]
  } else {
    both.spectr.data[[paste0("Norm_", Intensity.sim)]] <-
      both.spectr.data[[Intensity.sim]]
  }
  #
  ## Shift the Sim. spectrum below the Exp. one
  if (is.null(Intensity.shift.ratio)){
    both.spectr.data[[paste0("Norm_", Intensity.sim)]] <-
      both.spectr.data[[paste0("Norm_", Intensity.sim)]]
  } else {
    both.spectr.data[[paste0("Norm_", Intensity.sim)]] <-
      both.spectr.data[[paste0("Norm_", Intensity.sim)]] -
      diff_Intens_expr / Intensity.shift.ratio
  }
  #
  ## Shift the B of the simulated spectrum (B/g-factor
  ## can be slightly shifted in MATLAB output)
  both.spectr.data[[paste0("Bsim_", B.unit)]] <-
    both.spectr.data[[paste0("Bsim_", B.unit)]] + B.shift
  #
  ## New data frame with both spectra
  ## (select only required columns/variables)
  both.spectr.data <- both.spectr.data %>%
    dplyr::select(dplyr::all_of(c(paste0("Bsim_", B.unit),
                                  paste0("B_",B.unit),
                                  Intensity.sim,
                                  Intensity.expr,
                                  paste0("Norm_",Intensity.sim))))
    # dplyr::select(.data[[paste0("Bsim_", B.unit)]],
    #               .data[[paste0("B_",B.unit)]],
    #               .data[[Intensity.sim]],
    #               .data[[Intensity.expr]],
    #               .data[[paste0("Norm_",Intensity.sim)]])
  #
  ## B (x) label for the plot:
    xlab <- bquote(italic(B) ~ "(" ~ .(B.unit) ~ ")")
  #
  ## Intensity (y) label depending on intensity (derivative, integrated...)
  if (grepl("deriv|Deriv",lineSpecs.form)) {
    ylab <- bquote(d * italic(I)[EPR] ~ "/" ~ d * italic(B) ~ ~"(" ~ p.d.u. ~ ")")
  }
  if (grepl("integ|Integ|absorpt|Absorpt",lineSpecs.form)) {
    ylab <- bquote(italic(Intensity) ~ ~"(" ~ p.d.u. ~ ")")
  }
  #
  ## plot variable:
  simulation.plot <- both.spectr.data %>%
    ggplot() +
    geom_line(
      aes(
        x = .data[[paste0("B_", B.unit)]],
        y = .data[[Intensity.expr]],
        color = "Experiment"
      ),
      linewidth = line.width
    ) +
    geom_line(
      aes(
        x = .data[[paste0("Bsim_", B.unit)]],
        y = .data[[paste0("Norm_",Intensity.sim)]],
        color = "Simulation"
      ),
      linewidth = line.width
    ) +
    scale_color_manual(
      values = c(
        line.color.expr,
        line.color.sim
      ),
      breaks = c("Experiment", "Simulation")
    ) +
    labs(
      color = "",
      x = xlab,
      y = ylab
    )

  #
  ## if the entire table/table should be included
  if (isTRUE(output.df)) {
    SimPlotPlusTable <- list(plot = simulation.plot,
                             df = both.spectr.data)
  } else {
    SimPlotPlusTable <- simulation.plot
  }
  #
  return(SimPlotPlusTable)
  #
}
