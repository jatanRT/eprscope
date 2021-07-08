## Interactive plotting Simple EPR spectrum by `plotly`
#
#' @title Interactive (incl. Zooming, Data Reading...etc) EPR Spectrum Plot
#'
#'
#' @description TODO
#'
#'
#' @param spectrum.data Spectrum data frame/table where the magnetic flux density (in \code{mT}) column
#'   must be labeled as \code{B_mT} in mT (or \code{B_G} in gauss) and that of the derivative
#'   intensity as \code{dIepr_over_dB}, \code{index} column can be included as well, integrated/simulated spectra
#'   (incl. other \code{Intensity} and \code{B} columns) can be read as well
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{spectrum.data} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} (\strong{default})
#'   or \code{B = "B_G"} or \code{B = "B_G_Sim"} to include simulated EPR spectra as well
#' @param Intensity Character/String pointing to \code{intensity column} if other than \code{dIepr_over_dB}
#'   name/label is used (e.g. for integrated or simulated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param line.size Numeric, linewidth of the plot line in \code{pt}, \strong{default}: \code{line.size = 0.75}
#' @param time.series Boolean, whether the input ASCII spectrum data comes from the time series experiment
#'   with the additional \code{time} (labeled as \code{time_s}) column (ONLY IN CASE of "Xenon" software)
#'
#'
#' @return Interactive EPR spectrum plot/graph based on \pkg{plotly}
#'
#'
#' @examples
#' TODO
#' TODO
#'
#'
#' @export
#'
#'
#' @importFrom plotly ggplotly
plotEPRspecs_interact <- function(spectrum.data,
                                      B = "B_mT",
                                      Intensity = "dIepr_over_dB",
                                      line.size = 0.75,
                                      time.series = FALSE){
  #
  ## labels based on `Intensity` and `B` (`B` must contain either "B" and "mT" or "B" and "G") conditions:
  if (sjmisc::str_contains(B,c("B","mT"),logic = "and",ignore.case = F)){
    xlabel = "B (mT)"
  }
  if (sjmisc::str_contains(B,c("B","G"),logic = "and",ignore.case = F)){
    xlabel = "B (G)"
  }
  if (sjmisc::str_contains(Intensity,c("dB",
                                       "_dB",
                                       "intens",
                                       "deriv",
                                       "Intens",
                                       "Deriv",
                                       "dIepr"),logic = "or",ignore.case = F)){
    ylabel = "dIepr / dB  (p.d.u.)"
  }
  if (sjmisc::str_contains(Intensity,
                           c("single",
                             "Single",
                             "SInteg",
                             "sinteg",
                             "s_integ",
                             "single_",
                             "singleinteg",
                             "sintegral",
                             "integral",
                             "Integral",
                             "sInteg_"),
                           logic = "or",ignore.case = F)){
    ylabel <- "Iepr  (p.d.u.)"
  }
  if (sjmisc::str_contains(Intensity,
                           c("double",
                             "Double",
                             "Dinteg",
                             "DInteg",
                             "dinteg",
                             "d_integ",
                             "D_integ",
                             "D_Integ",
                             "double_",
                             "Double_",
                             "doubleinteg",
                             "DoubleInteg",
                             "Dintegral",
                             "DIntegral",
                             "dintegral",
                             "di",
                             "DI",
                             "sec",
                             "second",
                             "Second"),
                           logic = "or",ignore.case = T,switch = F)){
    ylabel <- "DIepr  (p.d.u.)"
  }
  #
  ## plot precursor
  if (isTRUE(time.series)){
    simplePlot <- ggplot(spectrum.data,aes(x = .data[[B]], y = .data[[Intensity]],color = .data$time_s)) +
      geom_line(size = line.size,show.legend = TRUE) +
      labs(x = xlabel,y = ylabel)
  } else{
    simplePlot <- ggplot(spectrum.data,aes(x = .data[[B]], y = .data[[Intensity]])) +
      geom_line(size = line.size,color = "#6600CC",show.legend = FALSE) +
      labs(x = xlabel,y = ylabel)
  }
  #
  return(ggplotly(simplePlot))
  #
}
