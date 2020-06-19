#
## Plotting simple EPR spectrum

#' @title Simple plotting of the EPR spectrum
#'
#' @description Graph/Plot of simple EPR spectrum based on \code{\link{ggplot2}}-functionality. Spectral data
#'   are in the form of data frame (must contain \code{dIepr_over_dB} and \code{B_mT}, i.e. derivative EPR
#'   intensity vs. magnetic flux density, respectively, columns). Theme of the graphic spectrum representation
#'   as well its line color can be varied like in \code{\link{ggplot2}} (see below). A theme
#'   for \code{publication ready} figures can be also applied based on \code{theme_linedraw()} with displayed
#'   or skipped \code{y} (\code{dIepr_over_dB} in 'procedure defined unit',
#'   see \href{http://www.iupac.org/divisions/VII/VII.C.1/C-NPU_Uppsala_081023_25_minutes_confirmed.pdf}{p.d.u.})
#'   ticks (this is common for presenting the EPR spectra). Function ca be additionally combined by \code{+} sign
#'   with other functions like in \code{\link{ggplot2}}.
#'
#'
#' @param spectrum.data Spectrum data frame/table where the magnetic flux density (in \code{mT}) column
#'   must be labeled as \code{B_mT} (in mT) and that of the derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column can be included as well
#' @param line.color String, line color to plot simple EPR spectrum. All \code{\link{ggplot2}} compatible
#'   colors are allowed
#' @param plot.theme String, which calls a ggplot theme. The following ones are defined:
#'   \itemize{
#'     \item \code{"theme_grey"} (default one) => gray background with white grid lines, default theme
#'     \item \code{"theme_bw"} => white background with thin gray grid lines
#'     \item \code{"theme_light"} => similar to \code{"theme_bw"} but without pronounced axis black lines
#'     \item \code{"theme_minimal"} => no axis lines are displayed (nor original neither opposite ones), only grid
#'     \item \code{"theme_classic"} => without grid, pronounced axis lines, however no opposite ones
#'     \item \code{"theme_pubready"} => pronounced axis lines (both for origin and opposite) without the grid,
#'     theme is proposed for publications.
#'   }
#'   Except the last one all above-described themes are standard in \code{\link{ggplot2}}.
#'   The last one (\code{"theme_pub"}) is modified \code{"theme_linedraw"} from \code{\link{ggplot2}}.
#' @param yTicks Boolean, whether to display the \code{y} (\code{dIepr_over_dB}) ticks and the corresponding text,
#'   which is usually skipped out in the EPR community, default TRUE
#'
#'
#' @return EPR spectrum graph/plot with key parameter (e.g. line-color and theme) variation
#'
#'
#' @examples
#' \dontrun{
#' plotEPRspectr(spectrum.data,"blue",plot.theme = "theme_pubready",yTicks = FALSE)
#' plotEPRspectr(spectrum.data,line.color = "steelblue","theme_bw")
#' plotEPRspectr(spectrum.data,"darkred")
#' }
#'
#'
#' @export
#'
#'
#' @importFrom ggplot2 ggplot geom_line theme aes labs coord_cartesian element_blank element_text unit margin theme_bw
#'   theme_light theme_minimal theme_classic theme_linedraw
plotEPRspectr <- function(spectrum.data,line.color,plot.theme = "theme_grey",yTicks = T){
  ## EPR spectrum borders for the visualization (see 'coord_cartesian')
  xB <- .data$B_mT ## this is the mask in order to assign variable correctly
  B.start <- min(spectrum.data$xB)
  B.end <- max(spectrum.data$xB)
  ## Labels for the x and y axis:
  x.label <- bquote(italic(B)~"("~mT~")")
  y.label <- bquote("d"~italic(I)[EPR]~"/"~"d"~italic(B)~~"("~p.d.u.~")")
  ## The plot depending on theme and whether the Y ticks are displayed or not.
  ## Therefore a theme variable is defined:
  NOyTicks.theme <- theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
  ## The lot function (5 G distance from the y-axis borders ('B.start-0.5','B.end+0.5')):
  simplePlot <- ggplot(spectrum.data) + geom_line(aes(x = xB, y = .data$dIepr_over_dB),size = 0.75,color = line.color) +
    labs(x = x.label,y = y.label) + coord_cartesian(xlim = c(B.start-0.5,B.end+0.5)) +
    theme(axis.ticks.length = unit(-6,"pt"),
          axis.text.x = element_text(margin = margin(10,10,4,10,unit = "pt")),
          axis.text.y = element_text(margin = margin(10,10,10,4,unit = "pt")),
          axis.title = element_text(margin = margin(2,2,6,6,unit = "pt"))
    ) ## theme in order to have ticks inside the graph
  if (plot.theme == "theme_grey"){
    if (isTRUE(yTicks)){
      p <- simplePlot
    } else{
      p <- simplePlot + NOyTicks.theme
    }

  } else if (plot.theme == "theme_bw"){
    if (isTRUE(yTicks)){
      p <- simplePlot + theme_bw()
    } else{
      p <- simplePlot + theme_bw() + NOyTicks.theme
    }

  } else if (plot.theme == "theme_light"){
    if (isTRUE(yTicks)){
      p <- simplePlot + theme_light()
    } else{
      p <- simplePlot + theme_light() + NOyTicks.theme
    }
  } else if (plot.theme == "theme_minimal"){
    if (isTRUE(yTicks)){
      p <- simplePlot + theme_minimal()
    } else{
      p <- simplePlot + theme_minimal() + NOyTicks.theme
    }
  } else if (plot.theme == "theme_classic"){
    if (isTRUE(yTicks)){
      p <- simplePlot + theme_classic()
    } else{
      p <- simplePlot + theme_classic() + NOyTicks.theme
    }
  } else if (plot.theme == "theme_pubready"){
    if (isTRUE(yTicks)){
      p <- simplePlot + theme_linedraw() + theme(panel.grid = element_blank())
    } else{
      p <- simplePlot + theme_linedraw() + theme(panel.grid = element_blank()) + NOyTicks.theme
    }
  }
  return(p)
}
