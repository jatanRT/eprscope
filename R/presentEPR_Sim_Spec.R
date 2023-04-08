#
#' Comparison Between the Experimental and Simulated EPR Spectrum
#'
#'
#' @description TODO by Tabular and/or Graphic Representation
#'
#'
#' @param data.spectrum.exp \strong{Experimental} Spectrum data frame/table incl. magnetic flux
#'   density (in \code{mT} or \code{G}) column and labeled as \code{B_mT} in mT (or \code{B_G} in gauss) +
#'   it contains intensity column as \code{dIepr_over_dB}, \code{index} column can be included as well,
#' @param data.spectrum.sim Data frame/table corresponding to \strong{simulated} spectrum incl. \strong{magnetic
#'   flux density} (in \code{mT} or \code{G}) \strong{column must be labeled as} \code{Bsim_mT} in mT
#'   (or \code{Bsim_G} in gauss) + it contains intensity as well, this is automatic
#'   if the \code{\link{readEPR_Sim_Spec}} function is used to read the spectrum in ASCII
#' @param B.unit Character/String pointing to unit of magnetic flux density (coming from original data) which
#'   is to be presented on \eqn{B} abscissa of the EPR spectrum,
#'   like \code{"G"} (`Gauss`) or \code{"mT"} (`millitesla`), \strong{default}: \code{B.unit = "mT"}
#' @param Intensity.exp Character/String pointing to \code{intensity column} if other than \code{dIepr_over_dB}
#'   name/label is used (e.g. for integrated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param Intensity.sim Character/String pointing to \code{intensity column} of the simulated spectrum
#'   if other than \code{dIeprSim_over_dB} name/label is used (e.g. for integrated spectra),
#'   \strong{default}: \code{Intesity = "dIeprSim_over_dB"}, this is automatic if
#'   the \code{\link{readEPR_Sim_Spec}} function is used to read the spectrum in ASCII
#' @param Intensity.shift.ratio Numeric (\strong{cannot be 0}), showing how 'far' is the simulated EPR spectrum
#'   presented below the experimental one. The lower the ratio, the 'deeper' the simulated spectrum shift,
#'   \strong{default}: \code{Intensity.shift.ratio = 1.2}, other common values : \code{0.6},\code{0.8},
#'   \code{1.2},\code{1.1}
#' @param B.shift Numeric, difference between the \eqn{B_{center}} of simulated and experimental spectrum,
#'   that can be caused by \emph{MATLAB}-output, it refers to simulated spectrum, \strong{dafault}:
#'   \code{B.shift = 0} (\strong{NOTE}: It depends on the \code{B} parameter. If \code{B.unit = "mT"} =>
#'   \code{B.shift} must be in \code{mT}, or if \code{B.unit = "G"} then \code{B.shift} must be in \code{G})
#' @param line.color.exp String, line color to plot simple EPR spectrum. All \pkg{ggplot2} compatible
#'   colors are allowed, \strong{default}: \code{line.color = "red"}, should be different from \code{line.color.sim}
#' @param line.color.sim String, line color to plot simple EPR spectrum. All \pkg{ggplot2} compatible
#'   colors are allowed, \strong{default}: \code{line.color = "blue"}, should be different from \code{line.color.exp}
#' @param line.width Numeric, linewidth of the plot line in \code{pt}, \strong{default}: \code{line.size = 0.75}
#' @param output.table Boolean, whether the table/data frame, corresponding to graphic spectra comparison
#'   (this actually corresponds to joining the above-mentioned data frames by \code{\link[dplyr:bind]{dplyr::bind_cols}})
#'   should be presented as well (\code{output.table = TRUE}), in such case the output is \code{list}
#'   containing plot (\code{list$plot},graphic representation) as well as containing table
#'   (\code{list$table}, tabular representation), however the \strong{default} is \code{output.table = FALSE},
#'   i.e. only graphical representation is shown
#'
#'
#' @return \pkg{ggplot2} \code{plot} of experimental and simulated EPR (in derivative or integrated formm)
#'   spectrum or \code{list} consisting of mentioned-plot and the corresponding \code{data frame/table}
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
presentEPR_Sim_Spec <- function(data.spectrum.exp,
                                data.spectrum.sim,
                                B.unit = "mT",
                                Intensity.exp = "dIepr_over_dB",
                                Intensity.sim = "dIeprSim_over_dB",
                                Intensity.shift.ratio = 1.2,
                                B.shift = 0,
                                line.color.exp = "red",
                                line.color.sim = "blue",
                                line.width = 0.75,
                                output.table = FALSE) {
  #
  ## 'Temporary' processing variables
  mT <- NULL
  G <- NULL
  B <- NULL
  #
  ## Join both tables/data frames
  both.spectr.data <- dplyr::bind_cols(data.spectrum.exp, data.spectrum.sim)
  #
  ## Differences in intensity extremes:
  diff_Intens_exp <- max(both.spectr.data[[Intensity.exp]]) - min(both.spectr.data[[Intensity.exp]])
  diff_Intens_sim <- max(both.spectr.data[[Intensity.sim]]) - min(both.spectr.data[[Intensity.sim]])
  #
  ## Ratio of both above:
  Intens_ratio <- diff_Intens_sim / diff_Intens_exp
  #
  ## scaling the Sim. EPR spectrum intensity to match the Exp. one (new column `Norm_...`):
  if (Intens_ratio != 1) {
    both.spectr.data[[paste0("Norm_", Intensity.sim)]] <- both.spectr.data[[Intensity.sim]] / Intens_ratio
  } else {
    both.spectr.data[[paste0("Norm_", Intensity.sim)]] <- both.spectr.data[[Intensity.sim]]
  }
  #
  ## Shift the Sim. spectrum below the Exp. one
  both.spectr.data[[paste0("Norm_", Intensity.sim)]] <-
    both.spectr.data[[paste0("Norm_", Intensity.sim)]] - diff_Intens_exp / Intensity.shift.ratio
  #
  ## Shift the B of the simulated spectrum (B/g-factor can be slightly shifted in MATLAB output)
  both.spectr.data[[paste0("Bsim_", B.unit)]] <- both.spectr.data[[paste0("Bsim_", B.unit)]] + B.shift
  #
  ## B (x) label for the plot:
  if (B.unit == "mT") {
    xlab <- plot_labels_xyz(B, mT)
  }
  if (B.unit == "G") {
    xlab <- plot_labels_xyz(B, G)
  }
  #
  ## Labels based on `Intensity` conditions:
  ## Select labels by defining the corresponding vectors
  slct.vec.deriv.EPR.intens <- c(
    "dB", "_dB", "intens", "deriv", "Intens",
    "Deriv", "dIepr", "dIepr_over_dB", "dIepr_dB",
    "MW_Absorp"
  )
  #
  slct.vec.integ.EPR.intens <- c(
    "single", "Single", "SInteg", "sinteg", "s_integ",
    "single_", "singleinteg", "sintegral", "integral",
    "Integral", "sInteg_", "sInteg", "singleI", "integ", "Integ"
  )
  #
  slct.vec.Dinteg.EPR.intens <- c(
    "double", "Double", "Dinteg", "DInteg", "dinteg", "d_integ",
    "D_integ", "D_Integ", "double_", "Double_", "doubleinteg",
    "DoubleInteg", "Dintegral", "DIntegral", "dintegral",
    "di", "DI", "Second", "dInteg", "doubleI", "sigm", "Sigm"
  )
  #
  ## Intensity (y) label depending on intensity (derivative, integrated...)
  if (any(grepl(Intensity.exp,paste(slct.vec.deriv.EPR.intens,collapse = "|")))) {
    ylab <- bquote(d * italic(I)[EPR] ~ "/" ~ d * italic(B) ~ ~"(" ~ p.d.u. ~ ")")
  }
  if (any(grepl(Intensity.exp,paste(slct.vec.integ.EPR.intens,collapse = "|")))) {
    ylab <- bquote(italic(I)[EPR] ~ ~"(" ~ p.d.u. ~ ")")
  }
  if (any(grepl(Intensity.exp,paste(slct.vec.Dinteg.EPR.intens,collapse = "|")))) {
    ylab <- bquote(italic(DI)[EPR] ~ ~"(" ~ p.d.u. ~ ")")
  }
  #
  ## plot variable:
  simulation.plot <- both.spectr.data %>%
    ggplot() +
    geom_line(
      aes(
        x = .data[[paste0("B_", B.unit)]],
        y = .data[[Intensity.exp]],
        color = "Experiment"
      ),
      linewidth = line.width
    ) +
    geom_line(
      aes(
        x = .data[[paste0("Bsim_", B.unit)]],
        y = .data[[Intensity.sim]],
        color = "Simulation"
      ),
      linewidth = line.width
    ) +
    scale_color_manual(
      values = c(
        line.color.exp,
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
  if (isTRUE(output.table)) {
    SimPlotPlusTable <- list(plot = simulation.plot, table = both.spectr.data)
  } else {
    SimPlotPlusTable <- simulation.plot
  }
  #
  return(SimPlotPlusTable)
  #
}
