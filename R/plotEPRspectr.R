#
## Plotting simple EPR spectrum

#' @title
#'
#' @description
#'
#'
#' @param spectrum.data Spectrum data frame/table where the magnetic flux density (in \code{mT}) column
#'   must be labeled as \code{B_mT} and that of the derivative intensity as \code{dIepr_over_dB},
#'   \code{index} column can be included as well
#' @param line.color String, line color to plot simple EPR spectrum. All \code{\link{ggplot2}} compatible
#'   colors are allowed
#' @param plot.theme String, which calls a ggplot theme. The following ones are defined:
#'   \itemize{
#'     \item \code{"theme_grey"} (default one) => gray background with white grid lines
#'     \item \code{"theme_bw"} => white background with thin gray lines
#'     \item \code{"theme_light"} => the same as \code{"theme_bw"}
#'     \item \code{"theme_minimal"}
#'     \item \code{"theme_classic"}
#'     \item \code{"theme_pubready"}
#'   }
#' @param yTicks Boolean,
#'
#' @return
#'
#'
#' @examples
#'
#'
#' @export
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
          axis.text = element_text(margin = margin(10,10,10,10,unit = "pt")),
          axis.text.title = element_text(margin = margin(6,6,6,6,unit = "pt")),
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
