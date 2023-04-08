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
#' @param var2nd String/Character referred to name of the variable/quantity column (e.g. like `time`,`Temperature`,
#'   `Electrochemical Potential`,`Microwave Power`...etc) altered upon individual experiments as a second variable
#'   (\code{var2nd}) and related to spectra/data (see also \code{var2nd.series} parameter). Data must be available
#'   in \strong{long table} (or \strong{tidy}) \strong{format} (see also \code{\link{readEPR_Exp_Specs_multif}}).
#'   \strong{Default}: \code{var2nd = NULL}. If \code{var2nd.series = FALSE}, otherwise \strong{usually}
#'   \code{var2nd = "time_s"} if \code{var2nd.series = TRUE}
#' @param line.color Character/String corresponding to \strong{line color} in case \strong{of simple spectrum}
#'   (not for \code{var2nd.series}), therefore \strong{default:} \code{line.color = "darkviolet"}
#' @param bg.color Character/String corresponding to \strong{background color}
#' @param grid.color Character/String corresponding to \strong{grid color}
#' @param line.width Numeric, linewidth of the plot line in \code{pt}, \strong{default}: \code{line.width = 0.75}
#' @param border.line.width tbc
#' @param border.line.color tbc
#' @param legend.title tbc
#' @param legend.title.size tbc
#' @param var2nd.series Boolean, whether the input ASCII spectrum data comes from the series of experiments
#'   each corresponding to alteration of a second variable (usually like time series, see also parameter \code{var2nd})
#'   where the ASCII data are in the long table/tidy format (e.g. for time series => 3 columns like "B_mT","time_s"
#'   and "Intensity" must be supplied). \strong{Deffault}: \code{var2nd.series = FALSE}.
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
                                      var2nd = NULL,
                                      line.color = "darkviolet",
                                      bg.color = "#e5ecf6",
                                      grid.color = "#ffff",
                                      line.width = 0.75,
                                      border.line.width = 1.2,
                                      border.line.color = "black",
                                      legend.title = NULL,
                                      legend.title.size = 13,
                                      axis.title.size = 15,
                                      axis.text.size = 14,
                                      var2nd.series = FALSE) {
  #
  ## Labels based on `Intensity` and `x` quantity (B, g, RF) conditions:
  ## Select labels by defining the corresponding vectors
  slct.vec.x.g <- c(
    "g", "g_value", "g_Value", "gval", "gVal",
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
    "single_", "singleinteg", "sintegral", "integral",
    "Integral", "sInteg_", "sInteg", "singleI", "integ", "Integ"
  )
  #
  slct.vec.Dinteg.EPR.intens <- c(
    "double", "Double", "Dinteg", "DInteg", "dinteg", "d_integ",
    "D_integ", "D_Integ", "double_", "Double_", "doubleinteg",
    "DoubleInteg", "Dintegral", "DIntegral", "dintegral",
    "di", "DI", "Second", "dInteg", "doubleI", "sigm", "Sigm"
  )
  #
  ## label <=> selection
  ## Labels based on `Intensity` and `x` quantity (B, g, RF) conditions:
  if (any(grepl(x, "B_mT|mT|BField_mT|Field_mT"))) {
    xlabel <- "<i>B</i> (mT)"
  }
  if (any(grepl(x, "B_G|G|BField_G|Field_G"))) {
    xlabel <- "<i>B</i> (G)"
  }
  if (any(grepl(x, "RF|MHz|radio|radio_f|freq",ignore.case = T))) {
    xlabel <- "<i>&#957;</i><sub>RF</sub> (MHz)"
  }
  if (any(grepl(x,paste(slct.vec.x.g,collapse = "|")))) {
    xlabel <- "<i>g</i>"
  }
  if (any(grepl(Intensity,paste(slct.vec.deriv.EPR.intens,collapse = "|")))) {
    ylabel <- "d <i>I</i><sub>EPR</sub> / d <i>B</i>  (p.d.u.)"
  }
  if (any(grepl(Intensity,paste(slct.vec.integ.EPR.intens,collapse = "|")))) {
    ylabel <- "<i>I</i><sub>EPR</sub>  (p.d.u.)"
  }
  if (any(grepl(Intensity,paste(slct.vec.Dinteg.EPR.intens,collapse = "|")))) {
    ylabel <- "<i>DI</i><sub>EPR</sub>  (p.d.u.)"
  }
  #
  ## plot precursor
  if (isTRUE(var2nd.series)) {
    if (is.null(var2nd)) {
      stop(" 'var2nd' string is not specified. Please, define! ")
    } else {
      #
      ## basis defined by `ggplot`
      simplePlot <- ggplot(data.spectra, aes(
        x = .data[[x]],
        y = .data[[Intensity]],
        color = as.factor(.data[[var2nd]])
      )) +
        geom_line(linewidth = line.width)
      #
    }
  } else {
    simplePlot <- ggplot(data.spectra, aes(x = .data[[x]], y = .data[[Intensity]])) +
      geom_line(linewidth = line.width, color = line.color)
  }
  ## final plot with layout
  if (isTRUE(var2nd.series)) {
    final_plot <- ggplotly(simplePlot) %>%
      plotly::layout(
        plot_bgcolor = bg.color,
        xaxis = list(
          title = list(
            text = xlabel,
            font = list(size = axis.title.size)
          ),
          tickfont = list(size = axis.text.size),
          gridcolor = grid.color,
          linecolor = plotly::toRGB(border.line.color),
          linewidth = border.line.width, showline = T, mirror = T
        ),
        yaxis = list(
          title = list(
            text = ylabel,
            font = list(size = axis.title.size)
          ),
          tickfont = list(size = axis.text.size),
          gridcolor = grid.color,
          linecolor = plotly::toRGB(border.line.color),
          linewidth = border.line.width, showline = T, mirror = T
        ),
        legend = list(title = list(
          text = legend.title,
          font = list(size = legend.title.size)
        ))
      )
  } else {
    final_plot <- ggplotly(simplePlot) %>%
      plotly::layout(
        plot_bgcolor = bg.color,
        xaxis = list(
          title = list(
            text = xlabel,
            font = list(size = axis.title.size)
          ),
          tickfont = list(size = axis.text.size),
          gridcolor = grid.color,
          linecolor = plotly::toRGB(border.line.color),
          linewidth = border.line.width, showline = T, mirror = T
        ),
        yaxis = list(
          title = list(
            text = ylabel,
            font = list(size = axis.title.size)
          ),
          tickfont = list(size = axis.text.size),
          gridcolor = grid.color,
          linecolor = plotly::toRGB(border.line.color),
          linewidth = border.line.width, showline = T, mirror = T
        )
      )
  }
  #
  return(final_plot)
  #
}
