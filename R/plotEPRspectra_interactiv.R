## Interactive plotting Simple EPR spectrum by `plotly`
#
#' @title TODO
#'
#'
#' @description TODO
#'
#'
#' @param spectrum.data Spectrum data frame/table where the magnetic flux density (in \code{mT}) column
#'   must be labeled as \code{B_mT} in mT (or \code{B_G} in gauss) and that of the derivative
#'   intensity as \code{dIepr_over_dB}, \code{index} column can be included as well
#' @param B Character/String pointing to magnetic flux density \code{column} of EPR spectrum data frame
#'   \code{spectrum.data} either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} (default)
#'   or \code{B = "B_G"}
#' @param line.size Numeric, linewidth of the plot line in \code{pt}, default: \code{line.size = 0.75}
#' @param time.series Boolean, whether the input ASCII spectrum data comes from the time series experiment
#'   with the additional \code{time} (labeled as \code{time_s}) column (ONLY IN CASE of "Xenon" software)
#'
#'
#' @return TODO
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
plotEPRspectra_interactiv <- function(spectrum.data,B = "B_mT",line.size = 0.75,time.series = FALSE){
  ## plot precursor
  if (isTRUE(time.series)){
    simplePlot <- ggplot(spectrum.data,aes(x = .data[[B]], y = .data$dIepr_over_dB,color = .data$times_s)) +
      geom_line(size = line.size,show.legend = TRUE)
  } else{
    simplePlot <- ggplot(spectrum.data,aes(x = .data[[B]], y = .data$dIepr_over_dB)) +
      geom_line(size = line.size,show.legend = FALSE)
  }
  ##
  return(ggplotly(simplePlot))
}
