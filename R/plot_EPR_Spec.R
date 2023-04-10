#
#' EPR/ENDOR Spectrum Simple Plot
#'
#' @description Graph/Plot of an EPR/ENDOR spectrum based on \pkg{ggplot2}-functionality. Spectral data
#'   are in the form of data frame, which must contain the \code{dIepr_over_dB} (or its corresponding integrated
#'   form,\code{Iepr}) and the following \code{x}-axis quantities like \eqn{B} (in \code{mT} or \code{G})
#'   or \eqn{g}-Value (dimensionless) or \eqn{RF} (radio-frequency in \code{MHz})
#'   Theme of the graphic spectrum representation as well its line color can be varied like
#'   in \pkg{ggplot2} (see below). Within a theme \code{y} ticks can be displayed
#'   or skipped \code{y} (\code{dIepr_over_dB} in 'procedure defined unit',
#'   see \href{http://www.iupac.org/divisions/VII/VII.C.1/C-NPU_Uppsala_081023_25_minutes_confirmed.pdf}{p.d.u.}),
#'   this is common for presenting the EPR spectra. Function can be additionally combined by \code{+} sign
#'   with other functions like in \pkg{ggplot2}, e.g. present or skip \code{grid} within the code.
#'
#'
#' @param data.spectrum SEPR/ENDOR spectrum data frame/table with magnetic flux density \eqn{B} (in \code{mT} or \code{G})
#'   or \eqn{g}-Value or \eqn{RF} (in \code{MHz}) column and that of the derivative \code{dIepr_over_dB}
#'   or integrated \code{Intensity}. \code{Index} column may be included as well.
#' @param x Character/String pointing to \code{x}-axis/column quantity like magnetic flux density \eqn{B}, \eqn{g}-Value
#'   or \eqn{RF} (radio frequency), \strong{default}: \code{x = "B_mT"}
#' @param x.unit Character/String pointing to unit of quantity (coming from original ASCII data, see also
#'   \code{column.names} parameter) which is to be presented on \eqn{x} abscissa of the EPR spectrum,
#'   like \code{"G"} (`Gauss`), \code{"mT"} (`millitesla`), \code{"MHz"} (`megahertz` in case of ENDOR spectra)
#'   or \code{"Unitless"} in case of \eqn{g}-values, \strong{default}: \code{x.unit = "mT"}.
#' @param xlim Numeric vector corresponding to border limits of the selected \eqn{x} region,
#'   e.g. like `xlim = c(3495.4,3595.4)` (\eqn{B} in \code{G}) or `xlim = c(12.5,21.2)` (\eqn{RF} in \code{MHz})
#'   or `xlim = c(2.004,2.001)` (\eqn{g} dimensionless). \strong{Default}: \code{xlim = NULL} (corresponding
#'   to entire `x` range)
#' @param Intensity Character/String pointing to \code{intensity column} if other than \code{dIepr_over_dB}
#'   name/label is used (e.g. for simulated or integrated spectra), \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param line.color String, line color to plot simple EPR spectrum. All \pkg{ggplot2} compatible
#'   colors are allowed, \strong{default}: \code{line.color = "steelblue"}
#' @param line.width Numeric, linewidth of the plot line in \code{pt}, \strong{default}: \code{line.width = 0.75}
#' @param border.line.width tbc
#' @param border.line.color tbc
#' @param theme.basic Character/String, which calls a ggplot theme base. The following ones are defined:
#'   \itemize{
#'     \item \code{"theme_gray"} (\strong{default} one) => the gray background with white grid lines
#'     \item \code{"theme_bw"} => the white background with thin gray grid lines
#'     \item \code{"theme_light"} => similar to \code{theme_bw()} but without the pronounced axis black lines
#'     \item \code{"theme_classic"} => without grid, pronounced axis lines, however no opposite ones
#'     \item \code{"theme_linedraw"} => pronounced axis lines (both for origin and opposite) as well as the grid-lines,
#'     theme is proposed \strong{for publications} (if the \code{grid} is set to \code{FALSE})
#'   }
#' @param axis.text.size Numeric, text size (in \code{pt}) for the axes units/descriptions,
#'   \strong{default}: \code{axis.text.size = 14}
#' @param axis.title.size Numeric, text size (in \code{pt}) for the axes title,
#'   \strong{default}: \code{axis.title.size = 15}
#' @param legend.title tbc
#' @param legend.title.size tbc
#' @param grid Logical, whether to dislay the \code{grid} within the plot/graph, \strong{default}: \code{grid = TRUE}
#' @param yTicks Logical, whether to display the \code{y} (\code{dIepr_over_dB}) ticks and the corresponding text
#'   (not the axis title!), which is usually skipped in the EPR community, \strong{default}: \code{yTicks = TRUE}
#'
#'
#' @return EPR simple spectrum \pkg{ggplot2} graph/plot with key parameter (e.g. line-color and theme,grid...etc.) variation
#'
#'
#' @examples
#' \dontrun{
#' plot_EPR_Spec(data.spectrum)
#' plot_EPR_Spec(data.spectrum,
#'               x = "B_G",
#'               x.unit = "G",
#'               Intensity = "dIepr_over_dB_Sim")
#' plot_EPR_Spec(data.spectrum,
#'               x = "B_mT_Sim",
#'               "single_Integ")
#' plot_EPR_Spec(data.spectrum,
#'               line.color = "blue",
#'               basic.theme = "theme_linedraw",
#'               yTicks = FALSE)
#' plot_EPR_Spec(data.spectrum,
#'               x = "g_Value",
#'               x.unit = "Unitless",
#'               theme.basic = "theme_bw",
#'               grid = TRUE)
#' plot_EPR_Spec(data.spectrum,
#'               x = "RF_MHz",
#'               x.unit = "MHz",
#'               line.color = "darkred",
#'               line.width = 1.2)
#' }
#'
#'
#' @export
#'
#'
#' @importFrom ggplot2 ggplot geom_line theme aes labs coord_cartesian scale_x_continuous scale_y_continuous
#'   scale_color_manual element_blank element_text element_rect dup_axis unit margin theme_bw theme_light theme_gray
#'   theme_minimal theme_classic theme_linedraw
plot_EPR_Spec <- function(data.spectrum,
                          x = "B_mT",
                          x.unit = "mT",
                          xlim = NULL,
                          Intensity = "dIepr_over_dB",
                          line.color = "steelblue",
                          line.width = 0.75,
                          border.line.width = 0.5,
                          border.line.color = "black",
                          theme.basic = "theme_gray",
                          axis.title.size = 15,
                          axis.text.size = 14,
                          legend.title = NULL,
                          legend.title.size = 13,
                          grid = TRUE,
                          yTicks = TRUE) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## Define limits if `xlim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.x.region <- c(min(data.spectrum[[x]]),max(data.spectrum[[x]]))
  xlim <- xlim %>% `if`(is.null(xlim),data.x.region, .)
  #
  ## EPR spectrum borders for the visualization (see 'coord_cartesian')
  x.start <- xlim[1]
  x.end <- xlim[2]
  #
  ## Labels based on `Intensity` and `x` quantity (B, g, RF) conditions:
  ## Select labels by defining the corresponding vectors
  slct.vec.x.g <- c(
    "g_value", "g_Value", "gval", "gVal",
    "g_factor", "g_Factor", "gfac", "gFac"
  )
  #
  slct.vec.deriv.EPR.intens <- c(
    "dB", "_dB", "intens", "deriv", "Intens",
    "Deriv", "dIepr", "dIepr_over_dB", "dIepr_dB",
    "MW_Absorp", "MW_intens", "MW_Intens"
  )
  #
  slct.vec.integ.EPR.intens <- c(
    "single", "Single", "SInteg", "sinteg", "s_integ",
    "single_", "singleinteg", "sintegral", "integral_Single",
    "Integral_single", "sInteg_", "sInteg", "singleI",
    "Sinteg", "Single_", "integral_single", "SingleI",
    "SingleInteg", "Isingle", "iSingle", "singleInteg", "ISingle",
    "IntegralSingl", "intergralSingl", "IntegSingl",
    "integSingl", "IntegSingl", "integSingl"
  )
  #
  slct.vec.Dinteg.EPR.intens <- c(
    "double", "Double", "Dinteg", "DInteg", "dinteg",
    "d_integ", "dInteg", "doubleInteg", "second", "Idouble",
    "D_integ", "D_Integ", "double_", "Double_", "doubleinteg",
    "DoubleInteg", "Dintegral", "DIntegral", "dintegral",
    "di", "DI", "Second", "dInteg", "doubleI", "sigm", "Sigm",
    "Idouble", "iDouble", "IDouble", "iSigm", "Isigm", "ISigm",
    "dIntegral", "integral_doub", "integral_Doub", "integral_Sigm",
    "IntegralDoub", "intergralDoub", "integral_sigm", "IntegSigm",
    "integSigm", "IntegDoub", "integDoub"
  )
  #
  ## label <=> selection
  ## & the plot function (distance from the y-axis borders
  ## e.g. ('B.start-0.5 (or 5 or 3)','B.end+0.5 (or 5 or 3)')):
  if (x.unit == "mT" || x.unit == "G") {
    x.label <- bquote(italic(B) ~ "(" ~ .(x.unit) ~ ")")
    x.plot.limits <- c(x.start - 1, x.end + 1)
  }
  if (x.unit == "MHz") {
    x.label <- bquote(italic(nu)[RF] ~ "(" ~ .(x.unit) ~ ")")
    x.plot.limits <- c(x.start - 3, x.end + 3)
  }
  if (any(grepl(paste(slct.vec.x.g,collapse = "|"), x))) {
    x.label <- bquote(italic(g))
    x.plot.limits <- c(x.start - 0.0002, x.end + 0.0002)
  }
  if (any(grepl(paste(slct.vec.deriv.EPR.intens,collapse = "|"), Intensity))) {
    y.label <- bquote("d" ~ italic(I)[EPR] ~ "/" ~ "d" ~ italic(B) ~ ~"(" ~ p.d.u. ~ ")")
  }
  if (any(grepl(paste(slct.vec.integ.EPR.intens,collapse = "|"), Intensity)) ||
      any(grepl(paste(slct.vec.Dinteg.EPR.intens,collapse = "|"), Intensity))) {
    y.label <- bquote(italic(Intensity) ~ ~"(" ~ p.d.u. ~ ")")
  }
  #
  ## Themes for the spectra, whether the ticks are displayed or not:
  theme.ticks <- theme(
    axis.ticks.length = unit(6, "pt"),
    axis.text.x = element_text(margin = margin(4, 6, 6, 6, unit = "pt"), size = axis.text.size),
    axis.text.y = element_text(margin = margin(6, 6, 6, 0, unit = "pt"), size = axis.text.size),
    axis.title.y = element_text(margin = margin(2, 4, 2, 6, unit = "pt"), size = axis.title.size),
    axis.title.x = element_text(margin = margin(2, 6, 2, 6, unit = "pt"), size = axis.title.size),
    panel.border = element_rect(color = border.line.color, fill = NA,linewidth = border.line.width)
  ) ## theme in order to have ticks outside the graph
  theme.Noticks <- theme(
    axis.ticks.length = unit(-6, "pt"),
    axis.text.x = element_text(margin = margin(6, 6, 6, 6, unit = "pt"), size = axis.text.size),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    axis.title.y = element_text(margin = margin(2, 8, 2, 6, unit = "pt"), size = axis.title.size),
    axis.title.x = element_text(margin = margin(2, 6, 2, 6, unit = "pt"), size = axis.title.size),
    panel.border = element_rect(color = border.line.color, fill = NA,linewidth = border.line.width)
  ) ## theme in order to have ticks inside the graph
  #
  theme.Nogrid <- theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) ## theme with no grid
  ## x ticks of the upper axis also inside the graph:
  axis_x_duplicate <- scale_x_continuous(sec.axis = dup_axis(name = "", labels = NULL))
  #
  ## Basic simple plot:
  if (is.null(legend.title)){
    simplePlot <- ggplot(data.spectrum) +
      geom_line(aes(x = .data[[x]], y = .data[[Intensity]]),
                linewidth = line.width, color = line.color, show.legend = FALSE
      ) +
      labs(x = x.label, y = y.label) +
      coord_cartesian(xlim = x.plot.limits)
  } else{
    #
    ## legend definition
    legend.strings <- stringr::str_split(legend.title,pattern = "[[:space:]]+")
    legend.strings <- unlist(legend.strings)
    if (length(legend.strings) == 1){
      legend.title <- bquote(italic(.(legend.strings[1])))
    }
    if (length(legend.strings) == 2){
      if (any(grepl("\\(",legend.strings))){
        legend.title <- bquote(italic(.(legend.strings[1]))~~.(legend.strings[2]))
      } else{
        legend.title <- bquote(atop(italic(.(legend.strings[1])),
                                    italic(.(legend.strings[2]))))
      }
    }
    if (length(legend.strings) == 3){
      if (any(grepl("\\(",legend.strings))){
        legend.title <- bquote(atop(italic(.(legend.strings[1])),
                                    italic(.(legend.strings[2])),
                                    .(legend.strings[3])))
      } else{
        legend.title <- bquote(atop(italic(.(legend.strings[1])),
                                    italic(.(legend.strings[2])),
                                    italic(.(legend.strings[3]))))
      }
    }
    #
    simplePlot <- ggplot(data.spectrum) +
      geom_line(aes(x = .data[[x]], y = .data[[Intensity]],color = ""),
                linewidth = line.width) +
      coord_cartesian(xlim = x.plot.limits) +
      scale_color_manual(values = line.color) +
      labs(color = legend.title, x = x.label, y = y.label)
  }
  #
  ## Conditions for plotting
  if (theme.basic == "theme_gray") {
    if (isTRUE(yTicks)) {
      if (isTRUE(grid)) {
        p <- simplePlot +
          theme_gray() +
          theme.ticks
      } else {
        p <- simplePlot +
          theme_gray() +
          theme.ticks +
          theme.Nogrid
      }
    } else {
      if (isTRUE(grid)) {
        p <- simplePlot +
          theme_gray() +
          theme.Noticks +
          axis_x_duplicate
      } else {
        p <- simplePlot +
          theme_gray() +
          theme.Noticks +
          axis_x_duplicate +
          theme.Nogrid
      }
    }
  }
  if (theme.basic == "theme_bw") {
    if (isTRUE(yTicks)) {
      if (isTRUE(grid)) {
        p <- simplePlot +
          theme_bw() +
          theme.ticks
      } else {
        p <- simplePlot +
          theme_bw() +
          theme.ticks +
          theme.Nogrid
      }
    } else {
      if (isTRUE(grid)) {
        p <- simplePlot +
          theme_bw() +
          theme.Noticks +
          axis_x_duplicate
      } else {
        p <- simplePlot +
          theme_bw() +
          theme.Noticks +
          axis_x_duplicate +
          theme.Nogrid
      }
    }
  }
  if (theme.basic == "theme_light") {
    if (isTRUE(yTicks)) {
      if (isTRUE(grid)) {
        p <- simplePlot +
          theme_light() +
          theme.ticks +
          theme(panel.border = element_blank())
      } else {
        p <- simplePlot +
          theme_light() +
          theme.ticks +
          theme.Nogrid +
          theme(panel.border = element_blank())
      }
    } else {
      if (isTRUE(grid)) {
        p <- simplePlot +
          theme_light() +
          theme.Noticks +
          axis_x_duplicate +
          theme(panel.border = element_blank())
      } else {
        p <- simplePlot +
          theme_light() +
          theme.Noticks +
          axis_x_duplicate +
          theme.Nogrid +
          theme(panel.border = element_blank())
      }
    }
  }
  if (theme.basic == "theme_linedraw") {
    if (isTRUE(yTicks)) {
      if (isTRUE(grid)) {
        p <- simplePlot +
          theme_linedraw() +
          theme.ticks
      } else {
        p <- simplePlot +
          theme_linedraw() +
          theme.ticks +
          theme.Nogrid
      }
    } else {
      if (isTRUE(grid)) {
        p <- simplePlot +
          theme_linedraw() +
          theme.Noticks +
          axis_x_duplicate
      } else {
        p <- simplePlot +
          theme_linedraw() +
          theme.Noticks +
          axis_x_duplicate +
          theme.Nogrid
      }
    }
  }
  if (theme.basic == "theme_classic") {
    if (isTRUE(yTicks)) {
      if (isTRUE(grid)) {
        p <- simplePlot +
          theme_classic() +
          theme.ticks +
          theme(panel.border = element_blank())
      } else {
        p <- simplePlot +
          theme_classic() +
          theme.ticks +
          theme.Nogrid +
          theme(panel.border = element_blank())
      }
    } else {
      if (isTRUE(grid)) {
        p <- simplePlot +
          theme_classic() +
          theme.Noticks +
          theme(panel.border = element_blank())
      } else {
        p <- simplePlot +
          theme_classic() +
          theme.Noticks +
          theme.Nogrid +
          theme(panel.border = element_blank())
      }
    }
  }
  #
  return(p)
  #
}
