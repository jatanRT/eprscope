

## Plotting simple EPR spectrum
plotEPRspectr <- function(spectrum.data,line.color,plot.theme = "theme_grey",includeYticks = T){
  ## EPR spectrum borders for the visualization (see 'coord_cartesian')
  xB <- .data$B_mT ## this is the mask in order to assign variable correctly
  B.start <- min(spectrum.data$xB)
  B.end <- max(spectrum.data$xB)
  ## Labels for the x and y axis:
  x.label <- bquote(italic(B)~"("~mT~")")
  y.label <- bquote("d"~italic(I)[EPR]~"/"~"d"~italic(B)~~"("~p.d.u.~")")
  ## The plot depending on theme and whether the Y ticks are drawn or not:
  simplePlot <- ggplot(spectrum.data) + geom_line(aes(x = xB, y = .data$dIepr_over_dB),size = 0.75,color = line.color) +
    labs(x = x.label,y = y.label) + coord_cartesian(xlim = c(B.start-0.5,B.end+0.5)) ## 5 G from the borders
  if (plot.theme == "theme_grey"){
    if (includeYthicks = T){
      p <- simplePlot
    } else{
      p <- simplePlot + theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
    }

  } else if (plot.theme == "theme_bw"){
    # TODO
  }

}
