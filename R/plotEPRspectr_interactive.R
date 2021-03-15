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
#' @param line.color String, line color to plot simple EPR spectrum. All \pkg{ggplot2} compatible
#'   colors are allowed, default: \code{line.color = "steelblue"}
#' @param line.size Numeric, linewidth of the plot line in \code{pt}, default: \code{line.size = 0.75}
#'
#' @return TODO
#'
#' @examples
#' TODO
#' TODO
#'
#' @export
#'
#'
#' @importFrom plotly ggplotly
plotEPRspectr_interactive <- function(spectrum.data,B = "B_mT",line.color = "steelblue",line.size = 0.75){
  ## plot precursor
  simplePlot <- ggplot(spectrum.data,aes(x = .data[[B]], y = .data$dIepr_over_dB)) +
    geom_line(size = line.size,color = line.color,show.legend = FALSE)
  ##
  return(ggplotly(simplePlot))
}
