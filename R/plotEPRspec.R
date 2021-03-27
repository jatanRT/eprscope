#
## Plotting simple EPR spectrum

#' @title EPR Spectrum Simple Plot
#'
#' @description Graph/Plot of an EPR spectrum based on \code{\pkg{ggplot2}}-functionality. Spectral data
#'   are in the form of data frame, which must contain the \code{dIepr_over_dB} and \code{B_mT}/\code{B_G}
#'   (depending on units, data frame may include both of them) columns,
#'   i.e. derivative EPR intensity vs. magnetic flux density, respectively. Integrated spectra,
#'   if integral column is available, can be ploted as well, see examples below.
#'   Theme of the graphic spectrum representation as well its line color can be varied like
#'   in \code{\pkg{ggplot2}} (see below). Within a theme \code{y} ticks can be displayed
#'   or skipped \code{y} (\code{dIepr_over_dB} in 'procedure defined unit',
#'   see \href{http://www.iupac.org/divisions/VII/VII.C.1/C-NPU_Uppsala_081023_25_minutes_confirmed.pdf}{p.d.u.}),
#'   this is common for presenting the EPR spectra. Function can be additionally combined by \code{+} sign
#'   with other functions like in \code{\pkg{ggplot2}}, e.g. present or skip \code{grid} within the code.
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
#'   name/label is used (e.g. for simulated or integrated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param line.color String, line color to plot simple EPR spectrum. All \code{\pkg{ggplot2}} compatible
#'   colors are allowed, \strong{default}: \code{line.color = "steelblue"}
#' @param line.size Numeric, linewidth of the plot line in \code{pt}, \strong{default}: \code{line.size = 0.75}
#' @param basic.theme Character/String, which calls a ggplot theme base. The following ones are defined:
#'   \itemize{
#'     \item \code{"theme_gray"} (\strong{default} one) => the gray background with white grid lines
#'     \item \code{"theme_bw"} => the white background with thin gray grid lines
#'     \item \code{"theme_light"} => similar to \code{theme_bw()} but without the pronounced axis black lines
#'     \item \code{"theme_classic"} => without grid, pronounced axis lines, however no opposite ones
#'     \item \code{"theme_linedraw"} => pronounced axis lines (both for origin and opposite) as well as the grid-lines,
#'     theme is proposed \strong{for publications} (if the \code{grid} is set to \code{FALSE})
#'   }
#' @param axis.text.size Numeric, text size (in \code{pt}) for the axes units/descriptions,
#'   \strong{default}: \code{axis.text.size = 15}
#' @param axis.title.size Numeric, text size (in \code{pt}) for the axes title,
#'   \strong{default}: \code{axis.title.size = 17}
#' @param grid Boolean, whether to dislay the \code{grid} within the plot/graph, \strong{default}: \code{grid = TRUE}
#' @param yTicks Boolean, whether to display the \code{y} (\code{dIepr_over_dB}) ticks and the corresponding text
#'   (not the axis title!), which is usually skipped in the EPR community, \strong{default}: \code{yTicks = TRUE}
#'
#'
#' @return EPR simple spectrum graph/plot with key parameter (e.g. line-color and theme,grid...etc.) variation
#'
#'
#' @examples
#' \dontrun{
#' plotEPRspec(spectrum.data)
#' plotEPRspec(spectrum.data,"B_G",Intensity = "dIepr_over_dB_Sim")
#' plotEPRspec(spectrum.data,"Integral",B = "B_mT_Sim")
#' plotEPRspec(spectrum.data,"blue",basic.theme = theme_linedraw(),yTicks = FALSE)
#' plotEPRspec(spectrum.data,line.color = "steelblue",B = "B_G",theme_bw(),grid = TRUE)
#' plotEPRspec(spectrum.data,"B_mT","darkred",line.size = 1.2)
#' }
#'
#'
#' @export
#'
#'
#' @importFrom ggplot2 ggplot geom_line theme aes labs coord_cartesian scale_x_continuous scale_y_continuous
#'   scale_color_manual element_blank element_text element_rect dup_axis unit margin theme_bw theme_light theme_gray
#'   theme_minimal theme_classic theme_linedraw
plotEPRspec <- function(spectrum.data,
                          B = "B_mT",
                          Intensity = "dIepr_over_dB",
                          line.color = "steelblue",
                          line.size = 0.75,
                          basic.theme = "theme_gray",
                          axis.text.size = 15,
                          axis.title.size = 17,
                          grid = TRUE,
                          yTicks = TRUE){
  ## EPR spectrum borders for the visualization (see 'coord_cartesian')
  B.start <- min(spectrum.data[,B])
  B.end <- max(spectrum.data[,B])
  ## Labels based on `Intensity` and `B` (`B` must contain either "B" and "mT" or "B" and "G") conditions:
  if (sjmisc::str_contains(B,c("B","mT"),logic = "and",ignore.case = F)){
    x.label <- bquote(italic(B)~"("~mT~")")
  }
  if (sjmisc::str_contains(B,c("B","G"),logic = "and",ignore.case = F)){
    x.label <- bquote(italic(B)~"("~G~")")
  }
  if (sjmisc::str_contains(Intensity,c("dB","intens","deriv"),logic = "or",ignore.case = T)){
    y.label <- bquote("d"~italic(I)[EPR]~"/"~"d"~italic(B)~~"("~p.d.u.~")")
  }
  if (sjmisc::str_contains(Intensity,
                           c("single","sinteg","s_integ","single_","singleinteg","sintegral","sInteg_"),
                           logic = "or",ignore.case = T,switch = F)){
    y.label <- bquote(italic(I)[EPR]~~"("~p.d.u.~")")
  }
  if (sjmisc::str_contains(Intensity,
                           c("double","dinteg","d_integ","double_","doubleinteg","dintegral","di","sec"),
                           logic = "or",ignore.case = T,switch = F)){
    y.label <- bquote(italic(DI)[EPR]~~"("~p.d.u.~")")
  }
  ## Themes for the spectra, whether the ticks are displayed or not:
  theme.ticks <- theme(axis.ticks.length = unit(6,"pt"),
                       axis.text.x = element_text(margin = margin(4,8,6,8,unit = "pt"),size = axis.text.size),
                       axis.text.y = element_text(margin = margin(8,8,8,0,unit = "pt"),size = axis.text.size),
                       axis.title.y = element_text(margin = margin(2,4,2,6,unit = "pt"),size = axis.title.size),
                       axis.title.x = element_text(margin = margin(2,6,2,6,unit = "pt"),size = axis.title.size),
                       panel.border = element_rect(color = "black",fill = NA),
                       plot.background = element_rect(fill = "transparent")
  ) ## theme in order to have ticks outside the graph
  theme.Noticks <- theme(axis.ticks.length = unit(-6,"pt"),
                         axis.text.x = element_text(margin = margin(10,8,6,8,unit = "pt"),size = axis.text.size),
                         axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                         axis.title.y = element_text(margin = margin(2,12,2,6,unit = "pt"),size = axis.title.size),
                         axis.title.x = element_text(margin = margin(2,6,2,6,unit = "pt"),size = axis.title.size),
                         panel.border = element_rect(color = "black",fill = NA),
                         plot.background = element_rect(fill = "transparent")
  ) ## theme in order to have ticks inside the graph
  theme.Nogrid <- theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank()
                        ) ## theme with no grid
  ## x ticks of the upper axis also inside the graph:
  axis_x_duplicate <- scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL))
  ## The lot function (5 G distance from the y-axis borders ('B.start-0.5(or 5)','B.end+0.5 (or 5)')):
  if (B == "B_mT"){
    x.plot.limits <- c(B.start - 0.5,B.end + 0.5)
  }
  if (B == "B_G"){
    x.plot.limits <- c(B.start - 5,B.end + 5)
  }
  ## Basic simple plot:
  simplePlot <- ggplot(spectrum.data) +
    geom_line(aes(x = .data[[B]], y = .data[[Intensity]]),
              size = line.size,color = line.color,show.legend = FALSE) +
    labs(x = x.label,y = y.label) +
    coord_cartesian(xlim = x.plot.limits)
  ## Conditions for plotting
  if (basic.theme == "theme_gray"){
    if (isTRUE(yTicks)){
      if (isTRUE(grid)){
        p <- simplePlot +
          theme_gray() +
          theme.ticks
      } else{
        p <- simplePlot +
          theme_gray() +
          theme.ticks +
          theme.Nogrid
      }
    } else{
      if (isTRUE(grid)){
        p <- simplePlot +
          theme_gray() +
          theme.Noticks +
          axis_x_duplicate
      } else{
        p <- simplePlot +
          theme_gray() +
          theme.Noticks +
          axis_x_duplicate +
          theme.Nogrid
      }
    }
  }
  if (basic.theme == "theme_bw"){
    if (isTRUE(yTicks)){
      if (isTRUE(grid)){
        p <- simplePlot +
          theme_bw() +
          theme.ticks
      } else{
        p <- simplePlot +
          theme_bw() +
          theme.ticks +
          theme.Nogrid
      }
    } else{
      if (isTRUE(grid)){
        p <- simplePlot +
          theme_bw() +
          theme.Noticks +
          axis_x_duplicate
      } else{
        p <- simplePlot +
          theme_bw() +
          theme.Noticks +
          axis_x_duplicate +
          theme.Nogrid
      }
    }
  }
  if (basic.theme == "theme_light"){
    if (isTRUE(yTicks)){
      if (isTRUE(grid)){
        p <- simplePlot +
          theme_light() +
          theme.ticks +
          theme(panel.border = element_blank())
      } else{
        p <- simplePlot +
          theme_light() +
          theme.ticks +
          theme.Nogrid +
          theme(panel.border = element_blank())
      }
    } else{
      if (isTRUE(grid)){
        p <- simplePlot +
          theme_light() +
          theme.Noticks +
          axis_x_duplicate +
          theme(panel.border = element_blank())
      } else{
        p <- simplePlot +
          theme_light() +
          theme.Noticks +
          axis_x_duplicate +
          theme.Nogrid +
          theme(panel.border = element_blank())
      }
    }
  }
  if (basic.theme == "theme_linedraw"){
    if (isTRUE(yTicks)){
      if (isTRUE(grid)){
        p <- simplePlot +
          theme_linedraw() +
          theme.ticks
      } else{
        p <- simplePlot +
          theme_linedraw() +
          theme.ticks +
          theme.Nogrid
      }
    } else{
      if (isTRUE(grid)){
        p <- simplePlot +
          theme_linedraw() +
          theme.Noticks +
          axis_x_duplicate
      } else{
        p <- simplePlot +
          theme_linedraw() +
          theme.Noticks +
          axis_x_duplicate +
          theme.Nogrid
      }
    }
  }
  if (basic.theme == "theme_classic"){
     if (isTRUE(yTicks)){
      if (isTRUE(grid)){
        p <- simplePlot +
          theme_classic() +
          theme.ticks +
          theme(panel.border = element_blank())
      } else{
        p <- simplePlot +
          theme_classic() +
          theme.ticks +
          theme.Nogrid +
          theme(panel.border = element_blank())
      }
    } else{
      if (isTRUE(grid)){
        p <- simplePlot +
          theme_classic() +
          theme.Noticks +
          theme(panel.border = element_blank())
      } else{
        p <- simplePlot +
          theme_classic() +
          theme.Noticks +
          theme.Nogrid +
          theme(panel.border = element_blank())
      }
    }
  }
  return(p)
}
