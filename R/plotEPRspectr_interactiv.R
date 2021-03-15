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
plotEPRspectr_interactiv <- function(spectrum.data,B = "B_mT",line.color = "steelblue"){
  ## Labels for the x and y axis:
  if (B == "B_mT"){
    x.label <- bquote(italic(B)~"("~mT~")")
  }
  if (B == "B_G"){
    x.label <- bquote(italic(B)~"("~G~")")
  }
  y.label <- bquote("d"~italic(I)[EPR]~"/"~"d"~italic(B)~~"("~p.d.u.~")")
  ## Themes for the spectrum (ticks inside the graph)
  theme.ticks.in <- theme_gray() +
    theme(axis.ticks.length = unit(-6,"pt"),
          axis.text.x = element_text(margin = margin(10,8,6,8,unit = "pt"),size = 15),
          axis.text.y = element_text(margin = margin(8,10,8,0,unit = "pt"),size = 15),
          axis.title.y = element_text(margin = margin(2,12,2,6,unit = "pt"),size = 17),
          axis.title.x = element_text(margin = margin(2,6,2,6,unit = "pt"),size = 17),
          panel.border = element_rect(color = "black",fill = NA))
  ## own plot:
  simplePlot <- ggplot(spectrum.data) +
    geom_line(aes(x = .data[[B]], y = .data$dIepr_over_dB),
              size = 0.75,color = line.color,show.legend = FALSE) +
    labs(x = x.label,y = y.label) +
    theme.ticks.in +
    scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = "",labels = NULL))
  ##
  return(ggplotly(simplePlot))
}
