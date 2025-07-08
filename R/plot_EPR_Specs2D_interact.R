#
#' Interactive Plot (incl. Zooming, Data Reading...etc) of EPR Spectra
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#'   Interactive visualization of EPR spectra or their integrals based on \href{https://plotly.com/r/}{plotly} package.
#'   In the first step function takes the essential plot parts as \href{https://ggplot2.tidyverse.org/}{ggplot2},
#'   which is subsequently transferred by \code{\link[plotly]{ggplotly}} into the final interactive format.
#'   Such plots mostly contain buttons in order to zoom,
#'   move and select (parts of) the EPR spectra/integrals as well as to display the point values directly within
#'   graph/panel. Additionally, plots can be exported into \code{.png} image
#'   or \code{.html} formats (see also \code{\link{plot_EPR_present_interact}}) and can optionally
#'   also \href{https://plotly.com/r/configuration-options/}{contain tools to draw lines, circles or rectangles}
#'   directly into the plot panel for annotations.
#'
#'
#' @param data.spectra Spectrum data frame/table object, containing magnetic flux density, \eqn{g}-value
#'   or radio-frequency columns as \code{x} variable. They can be labeled as \code{Field}, \code{B_mT}
#'   in mT (or \code{B_G} in gauss), see also \code{x} parameter/argument. The \code{y/Intensity} variable
#'   can be labeled as \code{dIepr_over_dB}, in case of derivative intensity, or if
#'   integrated or simulated spectra intensities are present, they can be labeled accordingly.
#'   See also \code{Intensity} parameter/argument. For spectral series the second independent variable
#'   \code{var2nd.series} column (e.g. \code{var2nd.series = "time_s"}) must be available. In such case,
#'   the entire \code{data.spectra} must be present in the form of
#'   \href{https://r4ds.had.co.nz/tidy-data.html}{tidy/long table format}
#'   (see also parameter/argument \code{var2nd.series}).
#' @param x Character string, pointing to \eqn{x}-axis/column quantity in the original \code{data.spectrum} like
#'   magnetic flux density \eqn{B}, \eqn{Field}, \eqn{g}-Value or \eqn{RF} (radio frequency),
#'   \strong{default}: \code{x = "B_mT"}.
#' @param x.unit Character string pointing to \eqn{x}-unit of quantity (coming from the original ASCII data, see also
#'   \code{column.names} parameter) which is to be presented on \eqn{x}-axis of the EPR spectrum,
#'   like \code{"G"} ("Gauss"), \code{"mT"} ("millitesla"), \code{"MHz"} ("megahertz", in the case of ENDOR spectra)
#'   or \code{"Unitless"} in case of \eqn{g}-values, \strong{default}: \code{x.unit = "mT"}.
#' @param Intensity Character string, corresponding to the intensity column header in the original \code{data.spectrum}
#'   if other than \code{dIepr_over_dB} name/label is used (e.g. for integrated or simulated spectra),
#'   \strong{default}: \code{Intesity = "dIepr_over_dB"}.
#' @param var2nd.series Character string, referred to name of the second independent variable/quantity
#'   column in the original \code{data.spectra} (such as time, Temperature, Electrochemical Potential,
#'   Microwave Power...etc) altered upon individual experiments. Data must be available in \strong{long table}
#'   (or \strong{tidy}) \strong{format} (see also \code{data.spectra} argument).
#'   \strong{Default}: \code{var2nd.series = NULL}. Otherwise, usually, \code{var2nd.series = "time_s"}.
#' @param lineSpecs.form Character string, describing either \code{"derivative"} (\strong{default})
#'   or \code{"integrated"} (i.e. \code{"absorption"} which can be used as well)
#'   line form of the analyzed EPR spectrum/integral.
#' @param line.colors Character string (vector). In case of of SIMPLE SPECTRUM
#'   (NOT FOR \code{var2nd.series}) ONLY ONE COLOR CHARACTER STRING IS REQUIRED => therefore,
#'   \strong{default:} \code{line.color = "darkviolet"}. For the SERIES OF SPECTRA CHARACTER COLOR VECTOR
#'   WITH THE LENGTH \eqn{\geq 2} must be DEFINED (e.g. \code{line.colors = c("darkorange","darkblue")}).
#' @param line.width Numeric, linewidth of the plot line in \code{mm}, \strong{default}: \code{line.width = 0.75}.
#' @param line.type Character string or integer, corresponding to width of the (spectral) line(s). Following types
#'   can be specified: \code{0 = "blank"}, \code{1 = "solid"} (\strong{default}), \code{2 = "dashed"}, \code{3 = "dotted"},
#'   \code{4 = "dotdash"}, \code{5 = "longdash"} and \code{6 = "twodash"}.
#' @param bg.color Character string, corresponding to background color of the panel/graph.
#'   Available colors are listed on the \href{https://www.w3.org/TR/css-color-3/}{CSS Color Module Homepage}.
#'   \strong{Default}: \code{bg.color = "#e5ecf6"} (corresponding to light blue-gray).
#' @param grid.color Character string, corresponding to grid lines color of the panel/graph.
#'   Available colors are listed on the \href{https://www.w3.org/TR/css-color-3/}{CSS Color Module Homepage}.
#'   \strong{Default}: \code{grid.color = "white"}.
#' @param border.line.width Numeric, width (in \code{px}) of the graph/plot panel border line, \strong{default}:
#'   \code{border.line.width = 1.2}.
#' @param border.line.color Character string, referring to color of the plot graph/panel border line. Available colors
#'   are listed on \href{https://www.w3.org/TR/css-color-3/}{CSS Color Module Homepage}.
#'   \strong{Default}: \code{border.line.color = "black"}.
#' @param legend.title Character string \eqn{\equiv} title of the legend (if the \code{var2nd.series} in NOT \code{NULL}).
#'   It can be defined either by simple text like \code{legend.title = "Time (s)"}
#'   or if additional formatting is required,
#'   the \href{https://www.w3schools.com/html/html_formatting.asp}{\code{html} markup language},
#'   such as \code{legend.title = "<i>Time</i> (s)"}
#'   or \code{legend.title = "<i>Potential<i> <br> <i>vs</i> Fc/Fc<sup>+</sup> (V)"} is used. If a \eqn{\LaTeX}
#'   typesetting is required for the title, please refer to
#'   e.g. \href{https://plotly.com/r/LaTeX/}{LaTeX Plotly Tepesetting}.
#'   \strong{Default}: \code{legend.title = NULL} (in all cases if \code{var2nd.series = NULL}).
#' @param legend.title.size Numeric, text size (in \code{px}) for the legend title,
#'   \strong{default}: \code{legend.title.size = NULL}, which actually corresponds to \code{13} if not otherwise defined.
#' @param axis.title.size Numeric, text size (in \code{px}) for the axis title,
#'   \strong{default}: \code{axis.title.size = 15}.
#' @param axis.text.size Numeric, text size (in \code{px}) for the axis unit values/descriptions,
#'   \strong{default}: \code{axis.text.size = 14}.
#'
#'
#' @return Interactive plot object of EPR spectrum/spectra based on \pkg{{plotly}} package.
#'
#'
#' @examples
#' ## load built-in EPR spectral data
#' data.file.path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.asc")
#' ## spectrum recorded as accumulation of 20 scans
#' data.epr <-
#'   readEPR_Exp_Specs(path_to_file = data.file.path,
#'                     col.names = c("B_G", "dIepr_over_dB"),
#'                     qValue = 3500,
#'                     norm.vec.add = 20,
#'                     origin = "winepr")
#' ## interactive plot or screenshot
#' plot_EPR_Specs2D_interact(data.spectra = data.epr)
#' #
#' ## loading the built-in CW ENDOR spectrum
#' ## of perinaphthenyl (PNT)
#' pnt.file.path <- load_data_example("PNT_ENDOR_a.txt")
#' ## read the PNT CW ENDOR data without intensity
#' ## normalization
#' pnt.endor.data <-
#'   readEPR_Exp_Specs(pnt.file.path,
#'                     col.names = c("index",
#'                                   "RF_MHz",
#'                                   "dIepr_over_dB"),
#'                     x.unit = "MHz")
#' ## plotting ENDOR spectrum
#' plot_EPR_Specs2D_interact(data.spectra = pnt.endor.data,
#'                           x = "RF_MHz",
#'                           x.unit = "MHz",
#'                           line.colors = "darkgreen",
#'                           bg.color = "cornsilk",
#'                           grid.color = "darkgrey")
#' #
#' \dontrun{
#' ## plot time series EPR spectra = verdazyl radical
#' ## oxidation kinetics (`verdazylRad.kinet.spectr`)
#' plot_EPR_Specs2D_interact(verdazylRad.kinet.spectr,
#'   x = "B_G",
#'   x.unit = "G",
#'   var2nd.series = "time_s",
#'   legend.title = "<i>Time</i> (s)",
#'   line.colors = c("darkorange","darkblue")
#'   )
#' }
#'
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
                                      line.type = 1,
                                      bg.color = "#e5ecf6",
                                      grid.color = "white",
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
    "g_factor", "g_Factor", "gfac", "gFac","g"
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
  ## g-factor condition =>
  g.factor.cond <- ifelse(any(grepl(paste(slct.vec.x.g,collapse = "|"), x)),TRUE,FALSE)
  #
  if (grepl("deriv|Deriv",lineSpecs.form)) {
    ylabel <- "d <i>I</i><sub>EPR</sub> / d <i>B</i>  (p.d.u.)"
  }
  if (grepl("integ|Integ|absorpt|Absorpt",lineSpecs.form)) {
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
      ## vector colors condition & def.
      if (length(line.colors) == 1) {
        line.colors <- c(line.colors,"darkorange","blue")
        message(" Because the `var2nd.series` is not NULL, `line.colors` has been converted\n
                into color vector ! You may set your custom colors accordingly !\n
                Please, consult the `line.colors` argument definition !")
      } else {
        line.colors <- line.colors
      }
      #
      plot.vector.colors <-
        grDevices::colorRampPalette(colors = line.colors)(var2nd.series.len)
      #
      ## `var2nd.series` factorization
      if (!is.factor(var2nd.series)) {
        data.spectra <- data.spectra %>%
          dplyr::mutate(!!rlang::quo_name(var2nd.series) :=
                          as.factor(.data[[var2nd.series]]))
      } else {
        data.spectra <- data.spectra
      }

      #
      ## basis defined by `ggplot`
      simplePlot <- ggplot(data.spectra, aes(
        x = .data[[x]],
        y = .data[[Intensity]],
        color = .data[[var2nd.series]]
      )) +
        geom_line(linewidth = line.width) +
        {if(g.factor.cond)scale_x_reverse()} +
        scale_color_manual(values = plot.vector.colors)
      #
  } else {
    ## g factor condition
    if (isTRUE(g.factor.cond)){
      simplePlot <- ggplot(data.spectra,
                           aes(x = .data[[x]],
                               y = .data[[Intensity]])) +
        geom_line(linewidth = line.width,
                  linetype = line.type,
                  color = line.colors) +
        scale_x_reverse()
    } else {
      simplePlot <- ggplot(data.spectra,
                           aes(x = .data[[x]],
                               y = .data[[Intensity]])) +
        geom_line(linewidth = line.width,
                  linetype = line.type,
                  color = line.colors)
    }
    #
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
            linewidth = border.line.width, showline = TRUE, mirror = TRUE
          ),
          yaxis = list(
            title = list(
              text = ylabel,
              font = list(size = axis.title.size)
            ),
            tickfont = list(size = axis.text.size),
            gridcolor = grid.color,
            linecolor = plotly::toRGB(border.line.color),
            linewidth = border.line.width, showline = TRUE, mirror = TRUE
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
          linewidth = border.line.width, showline = TRUE, mirror = TRUE
        ),
        yaxis = list(
          title = list(
            text = ylabel,
            font = list(size = axis.title.size)
          ),
          tickfont = list(size = axis.text.size),
          gridcolor = grid.color,
          linecolor = plotly::toRGB(border.line.color),
          linewidth = border.line.width, showline = TRUE, mirror = TRUE
        )
      )
  }
  #
  return(final_plot)
  #
}
