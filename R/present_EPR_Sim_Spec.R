#
#' Comparison Between the Experimental and Simulated EPR Spectrum
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description TODO by Tabular and/or Graphic Representation
#'
#'
#' @param data.spectrum.expr \strong{Experimental} spectrum data frame incl. magnetic flux
#'   density (in \code{mT} or \code{G}) column which are labeled as \code{B_mT}
#'   in mT (or \code{B_G} in gauss) + it contains intensity column as \code{dIepr_over_dB},
#'   \code{index} column can be included as well.
#' @param data.spectrum.sim Data frame corresponding to \strong{simulated} spectrum incl. \strong{magnetic
#'   flux density} (in \code{mT} or \code{G}) like \code{Bsim_mT} in mT (or \code{Bsim_G} in gauss) +
#'   it contains intensity as well, column names are taken automatically
#'   if the \code{\link{readEPR_Sim_Spec}} function is used to read the spectrum in ASCII.
#' @param B.unit Character/String pointing to unit of magnetic flux density (coming from original data)
#'   which is to be presented on \eqn{B} abscissa of the EPR spectrum,
#'   like \code{"G"} (`Gauss`) or \code{"mT"} (`millitesla`), \strong{default}: \code{B.unit = "G"}.
#' @param Intensity.expr Character string pointing to \code{intensity column} if other
#'   than \code{dIepr_over_dB} name/label is used (e.g. for integrated spectra),
#'   \strong{default}: \code{Intesity = "dIepr_over_dB"}.
#' @param Intensity.sim Character string pointing to \code{intensity column} of the simulated spectrum
#'   if other than \code{dIeprSim_over_dB} name/label is used (e.g. for integrated spectra),
#'   \strong{default}: \code{Intesity = "dIeprSim_over_dB"}, this is automatic if
#'   the \code{\link{readEPR_Sim_Spec}} function is used to read the spectrum in ASCII.
#' @param Intensity.shift.ratio Numeric (\strong{CANNOT BE `0`}), showing how 'far' is the simulated
#'   EPR spectrum presented below the experimental one. The lower the ratio, the 'deeper' the simulated
#'   spectrum shift, \strong{default}: \code{Intensity.shift.ratio = 1.2}, other common
#'   values : \code{0.6},\code{0.8}, \code{1.2},\code{1.1}. IF the \code{Intensity.shift.ration = NULL}
#'   BOTH SPECTRA ARE PRESENTED IN OVERAY MODE !
#' @param B.shift Numeric, difference between the \eqn{B_{center}} of simulated and experimental spectrum,
#'   that can be caused by \emph{MATLAB}-output, it refers to simulated spectrum, \strong{default}:
#'   \code{B.shift = 0} (\strong{NOTE}: It depends on the \code{B} parameter. If \code{B.unit = "mT"} =>
#'   \code{B.shift} must be in \code{mT}, or if \code{B.unit = "G"} then \code{B.shift} must be in \code{G}).
#' @param lineSpecs.form Character string describing either \code{"derivative"} (\strong{default})
#'   or \code{"integrated"} (i.e. \code{"absorption"} or sigmoid-integrated which can be used as well)
#'   line form of the analyzed EPR spectrum/data.
#' @param line.color.expr String, line color to plot simple EPR spectrum. All \pkg{ggplot2} compatible
#'   colors are allowed, \strong{default}: \code{line.color = "red"}, should be different
#'   from \code{line.color.sim}.
#' @param line.color.sim String, line color to plot simple EPR spectrum. All \pkg{ggplot2} compatible
#'   colors are allowed, \strong{default}: \code{line.color = "blue"}, should be different
#'   from \code{line.color.expr}.
#' @param line.width Numeric, linewidth of the plot line in \code{pt}, \strong{default}:
#'   \code{line.size = 0.75}.
#' @param output.df Logical, whether the data frame, corresponding to graphic spectra comparison
#'   (this actually corresponds to joining the above-mentioned data frames
#'   by \code{\link[dplyr:bind]{dplyr::bind_cols}}) should be presented as well (\code{output.df = TRUE}),
#'   in such case the output is \code{list} containing plot (\code{list$plot},graphic representation)
#'   as well as containing the data frame (\code{list$df}, tabular representation), however
#'   the \strong{default} is \code{output.df = FALSE}, i.e. only graphical representation is shown.
#'
#'
#' @return \pkg{ggplot2} \code{plot} of experimental and simulated EPR (in derivative or integrated form)
#'   spectrum or \code{list} consisting of plot and the corresponding data frame \code{df}.
#'
#'
#' @examples
#' \dontrun{
#' TODO
#' TODO
#' TODO
#' }
#'
#'
#' @export
#'
#'
#'
present_EPR_Sim_Spec <- function(data.spectrum.expr,
                                 data.spectrum.sim,
                                 B.unit = "G",
                                 Intensity.expr = "dIepr_over_dB",
                                 Intensity.sim = "dIeprSim_over_dB",
                                 Intensity.shift.ratio = 1.2, ## ca be also NULL
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
  both.spectr.data <- dplyr::bind_cols(data.spectrum.expr, data.spectrum.sim)
  #
  ## Differences in intensity extremes:
  diff_Intens_expr <- max(both.spectr.data[[Intensity.expr]]) - min(both.spectr.data[[Intensity.expr]])
  diff_Intens_sim <- max(both.spectr.data[[Intensity.sim]]) - min(both.spectr.data[[Intensity.sim]])
  #
  ## Ratio of both above:
  Intens_ratio <- diff_Intens_sim / diff_Intens_expr
  #
  ## scaling the Sim. EPR spectrum intensity to match the Exp. one (new column `Norm_...`):
  ## in order to better scale the intensity for noisy spectra multiply the `sim` Intensity by 0.95
  if (Intens_ratio != 1) {
    both.spectr.data[[paste0("Norm_", Intensity.sim)]] <- both.spectr.data[[Intensity.sim]] / Intens_ratio
    both.spectr.data[[paste0("Norm_", Intensity.sim)]] <-
      both.spectr.data[[paste0("Norm_", Intensity.sim)]] * 0.95
  } else {
    both.spectr.data[[paste0("Norm_", Intensity.sim)]] <- both.spectr.data[[Intensity.sim]]
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
  ## Shift the B of the simulated spectrum (B/g-factor can be slightly shifted in MATLAB output)
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
  if (lineSpecs.form == "derivative") {
    ylab <- bquote(d * italic(I)[EPR] ~ "/" ~ d * italic(B) ~ ~"(" ~ p.d.u. ~ ")")
  }
  if (lineSpecs.form == "integrated" || lineSpecs.form == "absorption") {
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
    SimPlotPlusTable <- list(plot = simulation.plot, df = both.spectr.data)
  } else {
    SimPlotPlusTable <- simulation.plot
  }
  #
  return(SimPlotPlusTable)
  #
}
