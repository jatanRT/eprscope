#
#' Interactive (incl. Zooming, Data Reading...etc) EPR Spectrum Plot
#'
#'
#' @family Visualization and Graphics
#'
#'
#' @description
#' tbc
#'
#'
#' @param data.spectra Spectrum data frame/table containing magnetic flux density, \eqn{g}-value
#'   or radio-frequency columns as \code{x} variable. They can be labeled as \code{Field}, \code{B_mT}
#'   in mT (or \code{B_G} in gauss), see also \code{x} parameter/argument. The \code{y/Intensity} variable
#'   can be labeled as \code{dIepr_over_dB}, in case of derivative intensity, or if
#'   integrated or simulated spectra intensities are present, they can be labeled accordingly.
#'   See also \code{Intensity} parameter/argument. For spectral series the second independent variable
#'   \code{var2nd.series} column (like e.g. \code{var2nd.series = "time_s"}) must be available. In such case
#'   the entire \code{data.spectra} has to be in form of `tidy` table format (see also parameter/argument
#'   \code{var2nd.series}).
#' @param x Character/String pointing to \code{x}-axis/column quantity in original \code{data.spectrum} like
#'   magnetic flux density \eqn{B}, \eqn{Field}, \eqn{g}-Value or \eqn{RF} (radio frequency),
#'   \strong{default}: \code{x = "B_mT"}.
#' @param x.unit Character/String pointing to unit of quantity (coming from original ASCII data, see also
#'   \code{column.names} parameter) which is to be presented on \eqn{x} abscissa of the EPR spectrum,
#'   like \code{"G"} (`Gauss`), \code{"mT"} (`millitesla`), \code{"MHz"} (`megahertz` in case of ENDOR spectra)
#'   or \code{"Unitless"} in case of \eqn{g}-values, \strong{default}: \code{x.unit = "mT"}.
#' @param Intensity Character/String pointing to \code{intensity column} in the original \code{data.spectrum}
#'   if other than \code{dIepr_over_dB} name/label is used (e.g. for integrated or simulated spectra),
#'   \strong{default}: \code{Intesity = "dIepr_over_dB"}
#' @param var2nd.series String/Character referred to name of the second independent variable/quantity
#'   column in the original \code{data.spectra} (e.g. like `time`,`Temperature`, `Electrochemical Potential`,
#'   `Microwave Power`...etc) altered upon individual experiments as a second variable
#'   (\code{var2nd.series}) and related to spectra/data. Data must be available in \strong{long table}
#'   (or \strong{tidy}) \strong{format} (see also \code{\link{readEPR_Exp_Specs_multif}}).
#'   \strong{Default}: \code{var2nd.series = NULL}. Otherwise \strong{usually} \code{var2nd.series = "time_s"}.
#' @param lineSpecs.form Character string describing either \code{"derivative"} (\strong{default})
#'   or \code{"integrated"} (i.e. \code{"absorption"} or sigmoid-integrated which can be used as well)
#'   line form of the analyzed EPR spectrum/data.
#' @param line.colors Character string or its vector. In case of \strong{of SIMPLE SPECTRUM}
#'   (NOT FOR \code{var2nd.series}) ONLY ONE COLOR CHARCTER STRING IS REQUIRED => therefore,
#'   \strong{default:} \code{line.color = "darkviolet"}. For the SERIES OF SPECTRA CHARACTER COLOR VECTOR
#'   WITH LENGTH \eqn{\geq 2} must be DEFINED (e.g. like \code{line.colors = c("darkorange","darkblue")}).
#' @param line.width Numeric, linewidth of the plot line in \code{pt}, \strong{default}: \code{line.width = 0.75}
#' @param bg.color Character string corresponding to \strong{background color}
#' @param grid.color Character string corresponding to \strong{grid color}
#' @param border.line.width tbc
#' @param border.line.color tbc
#' @param legend.title tbc
#' @param legend.title.size tbc
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
                                      x.unit = "mT",
                                      Intensity = "dIepr_over_dB",
                                      var2nd.series = NULL,
                                      lineSpecs.form = "derivative",
                                      line.colors = "darkviolet",
                                      line.width = 0.75,
                                      bg.color = "#e5ecf6",
                                      grid.color = "#ffff",
                                      border.line.width = 1.2,
                                      border.line.color = "black",
                                      legend.title = NULL,
                                      legend.title.size = NULL,
                                      axis.title.size = 15,
                                      axis.text.size = 14) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## Labels based on `Intensity` and `x` quantity (B, g, RF) conditions:
  ## Select labels by defining the corresponding vectors
  slct.vec.x.g <- c(
    "g_value", "g_Value", "gval", "gVal",
    "g_factor", "g_Factor", "gfac", "gFac"
  )
  #
  ## label <=> selection
  ## Labels based on `Intensity` and `x` quantity (B, g, RF) conditions:
  if (x.unit == "G" || x.unit == "mT") {
    xlabel <- paste0("<i>B</i> (",x.unit,")")
  }
  if (x.unit == "MHz") {
    xlabel <- paste0("<i>&#957;</i><sub>RF</sub> (",x.unit,")")
  }
  if (any(grepl(paste(slct.vec.x.g,collapse = "|"), x))) {
    xlabel <- "<i>g</i>"
  }
  if (lineSpecs.form == "derivative") {
    ylabel <- "d <i>I</i><sub>EPR</sub> / d <i>B</i>  (p.d.u.)"
  }
  if (lineSpecs.form == "integrated" || lineSpecs.form == "absorption") {
    ylabel <- "<i>Intensity</i>  (p.d.u.)"
  }
  #
  ## plot precursor
  if (!is.null(var2nd.series)) {
      ## length of `var2nd.series`
      var2nd.series.df <- data.spectra %>%
        dplyr::group_by(.data[[var2nd.series]]) %>%
        dplyr::group_keys()
      var2nd.series.len <- length(var2nd.series.df[[var2nd.series]])
      #
      ## vector colors def.
      plot.vector.colors <-
        grDevices::colorRampPalette(colors = line.colors)(var2nd.series.len)
      #
      ## basis defined by `ggplot`
      simplePlot <- ggplot(data.spectra, aes(
        x = .data[[x]],
        y = .data[[Intensity]],
        color = as.factor(.data[[var2nd.series]])
      )) +
        geom_line(linewidth = line.width) +
        scale_color_manual(values = plot.vector.colors)
      #
  } else {
    simplePlot <- ggplot(data.spectra, aes(x = .data[[x]], y = .data[[Intensity]])) +
      geom_line(linewidth = line.width, color = line.colors)
  }
  ## final plot with layout
  if (!is.null(var2nd.series)) {
    if (is.null(legend.title)){
      stop(" `legend.title` is not specified. Please, define ! ")
    } else{
      ## Legend title
      legend.title.size <- legend.title.size %>% `if`(is.null(legend.title.size),13, .)
      #
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
    }
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
