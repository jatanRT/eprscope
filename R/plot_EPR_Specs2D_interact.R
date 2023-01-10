#
#' Interactive (incl. Zooming, Data Reading...etc) EPR Spectrum Plot
#'
#'
#' @description
#' tbc
#'
#'
#' @param data.spectra Spectrum data frame/table where the magnetic flux density (in \code{mT}) column
#'   must be labeled as \code{B_mT} in mT (or \code{B_G} in gauss) and that of the derivative
#'   intensity as \code{dIepr_over_dB}, \code{index} column can be included as well, integrated/simulated spectra
#'   (incl. other \code{Intensity} and \code{B} columns) can be read as well
#' @param x Character/String pointing to \code{x}-axis/column quantity like magnetic flux density \eqn{B}, \eqn{g}-Value
#'   or \eqn{RF} (radio frequency), \strong{default}: \code{x = "B_mT"}
#' @param Intensity Character/String pointing to \code{intensity column} if other than \code{dIepr_over_dB}
#'   name/label is used (e.g. for integrated or simulated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param time tbc
#' @param line.color Character/String corresponding to \strong{line color} in case \strong{of simple spectrum}
#'   (not for \code{time.series}), therefore \strong{default:} \code{line.color = "darkviolet"}
#' @param bg.color Character/String corresponding to \strong{background color}
#' @param grid.color Character/String corresponding to \strong{grid color}
#' @param line.width Numeric, linewidth of the plot line in \code{pt}, \strong{default}: \code{line.width = 0.75}
#' @param border.line.width tbc
#' @param border.line.color tbc
#' @param legend.title tbc
#' @param time.series Boolean, whether the input ASCII spectrum data comes from the time series experiment
#'   with the additional \code{time} (labeled as \code{time_s}) column (ONLY IN CASE of "Xenon" software)
#' @param axis.title.size Numeric, text size (in \code{pt}) for the axes title,
#'   \strong{default}: \code{axis.title.size = 15}
#' @param axis.text.size Numeric, text size (in \code{pt}) for the axes units/descriptions,
#'   \strong{default}: \code{axis.text.size = 14}
#'
#'
#' @return Interactive EPR spectrum/spectra plot/graph based on \pkg{plotly}
#'
#'
#' @examples
#' \dontrun{
#' TODO
#' TODO
#' }
#'
#' @export
#'
#'
#' @importFrom plotly ggplotly
plot_EPR_Specs2D_interact <- function(data.spectra,
                                  x = "B_mT",
                                  Intensity = "dIepr_over_dB",
                                  time = "time_s",
                                  line.color = "darkviolet",
                                  bg.color = "#e5ecf6",
                                  grid.color = "#ffff",
                                  line.width = 0.75,
                                  border.line.width = 1.2,
                                  border.line.color = "black",
                                  legend.title = NULL,
                                  axis.title.size = 15,
                                  axis.text.size = 14,
                                  time.series = FALSE){
  #
  ## Labels based on `Intensity` and `x` quantity (B, g, RF) conditions:
  if (sjmisc::str_contains(x,c("B","mT"),logic = "and",ignore.case = F)){
    xlabel <- "<i>B</i> (mT)"
  }
  if (sjmisc::str_contains(x,c("B","G"),logic = "and",ignore.case = F)){
    xlabel <- "<i>B</i> (G)"
  }
  if (sjmisc::str_contains(x,c("RF","MHz"),logic = "and",ignore.case = T)){
    xlabel <- "<i>&#957;</i><sub>RF</sub> (MHz)"
  }
  if (sjmisc::str_contains(x,c("g",
                               "g_value",
                               "g_Value",
                               "gval",
                               "gVal",
                               "g_factor",
                               "g_Factor",
                               "gfac",
                               "gFac"),
                           logic = "or",ignore.case = F)){
    xlabel <- "<i>g</i>"
  }
  if (sjmisc::str_contains(Intensity,c("dB",
                                       "_dB",
                                       "intens",
                                       "deriv",
                                       "Intens",
                                       "Deriv",
                                       "dIepr",
                                       "dIepr_over_dB",
                                       "dIepr_dB"),logic = "or",ignore.case = F)){
    ylabel = "d <i>I</i><sub>EPR</sub> / d <i>B</i>  (p.d.u.)"
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
                             "sInteg_",
                             "sInteg",
                             "singleI"),
                           logic = "or",ignore.case = F)){
    ylabel <- "<i>I</i><sub>EPR</sub>  (p.d.u.)"
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
                             "Second",
                             "dInteg",
                             "doubleI"),
                           logic = "or",ignore.case = F,switch = F)){
    ylabel <- "<i>DI</i><sub>EPR</sub>  (p.d.u.)"
  }
  #
  ## plot precursor
  if (isTRUE(time.series)){
    #
    ## basis defined by `ggplot`
    simplePlot <- ggplot(data.spectra,aes(x = .data[[x]],
                                           y = .data[[Intensity]],
                                           color = as.factor(.data[[time]]))) +
      geom_line(linewidth = line.width)
    #
  } else{
    simplePlot <- ggplot(data.spectra,aes(x = .data[[x]], y = .data[[Intensity]])) +
      geom_line(linewidth = line.width,color = line.color)
  }
  ## final plot with layout
  if (isTRUE(time.series)){
    final_plot <- ggplotly(simplePlot) %>%
      plotly::layout(
        plot_bgcolor = bg.color,
        xaxis = list(title = list(text = xlabel,
                                  font = list(size = axis.title.size)),
                     tickfont = list(size = axis.text.size),
                     gridcolor = grid.color,
                     linecolor = plotly::toRGB(border.line.color),
                     linewidth = border.line.width,showline = T,mirror = T),
        yaxis = list(title = list(text = ylabel,
                                  font = list(size = axis.title.size)),
                     tickfont = list(size = axis.text.size),
                     gridcolor = grid.color,
                     linecolor = plotly::toRGB(border.line.color),
                     linewidth = border.line.width,showline = T,mirror = T),
        legend = list(title = list(text = legend.title,
                                   font = list(size = 13)))
      )
  } else{
    final_plot <- ggplotly(simplePlot) %>%
      plotly::layout(
        plot_bgcolor = bg.color,
        xaxis = list(title = list(text = xlabel,
                                  font = list(size = axis.title.size)),
                     tickfont = list(size = axis.text.size),
                     gridcolor = grid.color,
                     linecolor = plotly::toRGB(border.line.color),
                     linewidth = border.line.width,showline = T,mirror = T),
        yaxis = list(title = list(text = ylabel,
                                  font = list(size = axis.title.size)),
                     tickfont = list(size = axis.text.size),
                     gridcolor = grid.color,
                     linecolor = plotly::toRGB(border.line.color),
                     linewidth = border.line.width,showline = T,mirror = T)
      )
  }
  #
  return(final_plot)
  #
}
