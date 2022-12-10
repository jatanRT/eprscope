#
#' Show Comparison Between the Experimental and Simulated EPR Spectrum by Tabular and/or Graphic Representation
#'
#'
#' @description TODO
#'
#'
#' @param data.spectrum.exp \strong{Experimental} Spectrum data frame/table where the magnetic flux
#'   density (in \code{mT}) column is labeled as \code{B_mT} in mT (or \code{B_G} in gauss)
#'   and that of the derivative intensity as \code{dIepr_over_dB}, \code{index} column can be included as well,
#'   this is automatic if the \code{\link{readExpEPRspecs}} function is used to read the spectrum in ASCII
#' @param data.spectrum.sim Data frame/table corresponding to \strong{simulated} spectrum where the magnetic
#'   flux density (in \code{mT}) column must be labeled as \code{B_mT_Sim} in mT (or \code{B_G_Sim} in gauss)
#'   and that of the derivative intensity as \code{dIepr_over_dB_Sim}, this is automatic
#'   if the \code{\link{readSimEPRspec}} function is used to read the spectrum in ASCII
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectra data frames
#'   either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} (\strong{default})
#'   or \code{B = "B_G"}. Previous labels refer to \strong{both simulated and experimental spectral data frames}
#' @param Intensity.shift.ratio Numeric (\strong{cannot be 0}), showing how 'far' is the simulated EPR spectrum
#'   presented below the experimental one. The lower the ratio, the 'deeper' the simulated spectrum shift,
#'   \strong{default}: \code{Intensity.shift.ratio = 1.2}, other common values : \code{0.6},\code{0.8},
#'   \code{1.2},\code{1.1}
#' @param B.shift Numeric, difference between the \eqn{B_{center}} of simulated and experimental spectrum,
#'   that can be caused by \emph{MATLAB}-output, it refers to simulated spectrum, \strong{dafault}:
#'   \code{B.shift = 0} (\strong{NOTE}: It depends on the \code{B} parameter. If \code{B = "B_mT"} => \code{B.shift}
#'   must be in \code{mT}, or if \code{B = "B_G"} then \code{B.shift} must be in \code{G})
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
#' @return \pkg{ggplot2} \code{plot} of experimental and simulated spectrum or \code{list} consisting of mentioned-plot
#'   and the corresponding \code{data frame/table}
#'
#'
#' @examples
#' TODO
#' TODO
#' TODO
#'
#'
#' @export
#'
presentSimEPRspec <- function(data.spectrum.exp,
                              data.spectrum.sim,
                              B = "B_mT",
                              Intensity.shift.ratio = 1.2,
                              B.shift = 0,
                              line.color.exp = "red",
                              line.color.sim = "blue",
                              line.width = 0.75,
                              output.table = FALSE){
  #
  ## 'Temporary' processing variables
  mT <- NULL
  G <- NULL
  #
  ## Join both tables/data frames
  both.spectr.data <- dplyr::bind_cols(data.spectrum.exp,data.spectrum.sim)
  #
  ## Differences in intensity extremes:
  diff_Intens_exp <- max(both.spectr.data$dIepr_over_dB) - min(both.spectr.data$dIepr_over_dB)
  diff_Intens_sim <- max(both.spectr.data$dIepr_over_dB_Sim) - min(both.spectr.data$dIepr_over_dB_Sim)
  #
  ## Ratio of both above:
  Intens_ratio <- diff_Intens_sim/diff_Intens_exp
  #
  ## scaling the Sim. EPR spectrum intensity to match the Exp. one (new column `Norm_...`):
  if (Intens_ratio != 1){
    both.spectr.data$Norm_dIepr_over_dB_Sim <- both.spectr.data$dIepr_over_dB_Sim/Intens_ratio
  } else{
    both.spectr.data$Norm_dIepr_over_dB_Sim <- both.spectr.data$dIepr_over_dB_Sim
  }
  #
  ## Shift the Sim. spectrum below the Exp. one
  both.spectr.data$Norm_dIepr_over_dB_Sim <-
    both.spectr.data$Norm_dIepr_over_dB_Sim - diff_Intens_exp/Intensity.shift.ratio
  #
  ## Shift the B of the simulated spectrum (B/g-factor can be slightly shifted in MATLAB output)
  both.spectr.data[[paste0(B,"_Sim")]] <- both.spectr.data[[paste0(B,"_Sim")]] + B.shift
  #
  ## B label for the plot:
  if (B == "B_mT"){
    x.label <- EPRphysChemSpec::plotLabelsXYZ(B,mT)
  }
  if (B == "B_G"){
    x.label <- EPRphysChemSpec::plotLabelsXYZ(B,G)
  }
  #
  ## plot variable:
  simulation.plot <- both.spectr.data %>%
    ggplot() +
    geom_line(aes(x = .data[[B]],y = .data$dIepr_over_dB,color = "Experiment"),linewidth = line.width) +
    geom_line(aes(x = .data[[paste0(B,"_Sim")]],y = .data$Norm_dIepr_over_dB_Sim,
                  color = "Simulation"),linewidth = line.width) +
    scale_color_manual(values = c(line.color.exp,line.color.sim),breaks = c("Experiment","Simulation")) +
    labs(color = "",x = x.label,
         y = plotLabelsXYZ("d"~italic(I)[EPR]~"/"~"d"~italic(B),"("~p.d.u.~")",user.defined = TRUE))
  #
  ## if the entire table/table should be included
  if (isTRUE(output.table)){
    SimPlotPlusTable <- list(plot = simulation.plot,table = both.spectr.data)
  } else{
    SimPlotPlusTable <- simulation.plot
  }
  #
  return(SimPlotPlusTable)
 #
}
