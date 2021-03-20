#
#' @title TODO
#'
#'
#' @description TODO
#'
#'
#' @param exp.spectrum.data TODO
#' @param sim.spectrum.data TODO
#' @param B TODO
#' @param Intensity.shift.ratio TODO
#' @param B.shift TODO
#' @param line.color.exp TODO
#' @param line.color.sim TODO
#' @param line.size TODO
#' @param output.table TODO
#'
#'
#' @return TODO
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
presentSimEPRspectrum <- function(exp.spectrum.data,
                                  sim.spectrum.data,
                                  B = "B_mT",
                                  Intensity.shift.ratio = 1.2,
                                  B.shift = 0,
                                  line.color.exp = "red",
                                  line.color.sim = "blue",
                                  line.size = 0.75,
                                  output.table = FALSE){
  ## Join both tables/data frames
  both.spectr.data <- dplyr::bind_cols(exp.spectrum.data,sim.spectrum.data)
  ## Differences in intensity extremes:
  diff_Intens_exp <- max(both.spectr.data$dIepr_over_dB) - min(both.spectr.data$dIepr_over_dB)
  diff_Intens_sim <- max(both.spectr.data$dIepr_over_dB_Sim) - min(both.spectr.data$dIepr_over_dB_Sim)
  ## Ratio of both above:
  Intens_ratio <- diff_Intens_sim/diff_Intens_exp
  ## scaling the Sim. EPR spectrum intensity to match the Exp. one (new column `Norm_...`):
  if (Intens_ratio != 1){
    both.spectr.data <- both.spectr.data %>%
      dplyr::mutate(Norm_dIepr_over_dB_Sim <- .data$dIepr_over_dB_Sim/Intens_ratio)
  } else{
    both.spectr.data <- both.spectr.data %>%
      dplyr::mutate(Norm_dIepr_over_dB_Sim <- .data$dIepr_over_dB_Sim)
  }
  ## Shift the Sim. spectrum below the Exp. one
  both.spectr.data$Norm_dIepr_over_dB_Sim <-
    both.spectr.data$Norm_dIepr_over_dB_Sim - diff_Intens_exp/Intensity.shift.ratio
  ## Shift the B of the simulated spectrum (B/g-factor can be slightly shifted in MATLAB output)
  both.spectr.data[[B]] <- both.spectr.data[[B]] + B.shift
  ## B label for the plot:
  if (B == "B_mT"){
    x.label <- plotEPRlabel(B,mT)
  }
  if (B == "B_G"){
    x.label <- plotEPRlabel(B,G)
  }
  ## plot variable:
  simulation.plot <- both.spectr.data %>%
    ggplot() +
    geom_line(aes(x = .data[[B]],y = .data$dIepr_over_dB,color = "Experiment"),size = line.size) +
    geom_line(aes(x = .data[[paste(B,"_Sim",sep = "")]],y = .data$dIepr_over_dB_Sim,
                  color = "Simulation"),size = line.size) +
    scale_color_manual(values = c(line.color.exp,line.color.sim),breaks = c("Experiment","Simulation")) +
    labs(x = x.label,
         y = plotEPRlabel("d"~italic(I)[EPR]~"/"~"d"~italic(B),"("~p.d.u.~")",user.defined = TRUE))
  ## if the entire table/table should be included
  if (isTRUE(output.table)){
    SimPlotPlusTable <- list(simulation.plot,both.spectr.data)
  } else{
    SimPlotPlusTable <- simulation.plot
  }

  return(SimPlotPlusTable)

}
