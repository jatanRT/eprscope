#
#' Essential Plotting of EPR/ENDOR Spectrum/Spectra
#'
#' @description Graph/Plot of an EPR/ENDOR spectrum/spectra ('overlay' plot) based on \pkg{ggplot2}-functionality.
#'   Spectral data are in the form of data frame (details see below).
#'   Theme of the graphic spectrum representation as well its line colors can be varied like
#'   in \pkg{ggplot2}. Within a theme \code{y} ticks can be displayed
#'   or skipped \code{y} (e.g. \code{dIepr_over_dB} in 'procedure defined unit',
#'   see \href{http://www.iupac.org/divisions/VII/VII.C.1/C-NPU_Uppsala_081023_25_minutes_confirmed.pdf}{p.d.u.}),
#'   this is common for presenting the EPR spectra. Function can be additionally combined by \code{+} sign
#'   with other functions (e.g. with \code{\link{plot_theme_In_ticks}}) like in \pkg{ggplot2},
#'   e.g. present or skip \code{grid} within the code.
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
#' @param x Character/String pointing to \code{x}-axis/column quantity in the original \code{data.spectra}
#'   like magnetic flux density \eqn{B}, \eqn{g}-Value or \eqn{RF} (radio frequency),
#'   \strong{default}: \code{x = "B_mT"}.
#' @param x.unit Character/String pointing to unit of quantity (coming from original ASCII data, see also
#'   \code{column.names} parameter) which is to be presented on \eqn{x} abscissa of the EPR spectrum,
#'   like \code{"G"} (`Gauss`), \code{"mT"} (`millitesla`), \code{"MHz"} (`megahertz` in case of ENDOR spectra)
#'   or \code{"Unitless"} in case of \eqn{g}-values, \strong{default}: \code{x.unit = "mT"}.
#' @param xlim Numeric vector corresponding to border limits of the selected \eqn{x} region,
#'   e.g. like `xlim = c(3495.4,3595.4)` (\eqn{B} in \code{G}) or `xlim = c(12.5,21.2)` (\eqn{RF} in \code{MHz})
#'   or `xlim = c(2.004,2.001)` (\eqn{g} dimensionless). \strong{Default}: \code{xlim = NULL} (corresponding
#'   to entire `x` range)
#' @param var2nd.series String/Character referred to name of the second independent variable/quantity
#'   column in the original \code{data.spectra} (e.g. like `time`,`Temperature`, `Electrochemical Potential`,
#'   `Microwave Power`...etc) altered upon individual experiments as a second variable
#'   (\code{var2nd.series}) and related to spectra/data. Data must be available in \strong{long table}
#'   (or \strong{tidy}) \strong{format} (see also \code{\link{readEPR_Exp_Specs_multif}}).
#'   \strong{Default}: \code{var2nd.series = NULL}. Otherwise \strong{usually} \code{var2nd.series = "time_s"}.
#' @param var2nd.series.slct.by Numeric, number corresponding to each \eqn{n-th} presented spectrum in the plot,
#'   like e.g. display each second (\code{var2nd.series.slct.by = 2}), third (\code{var2nd.series.slct.by = 3}),
#'   fourth (\code{var2nd.series.slct.by = 4})...etc. spectrum. The argument is used in case
#'   \code{var2nd.series} is \strong{NOT NULL} (e.g. \code{var2nd.series = "time_s"}) and one wants to present
#'   separated labels/levels for spectra (not the continuous one). Recommended max. number of spectra/lines
#'   is 12.
#' @param Intensity Character/String pointing to \code{intensity column} in the original \code{data.spectra}
#'   if other than \code{dIepr_over_dB} name/label is used (e.g. for simulated or integrated spectra),
#'   \strong{default}: \code{Intesity = "dIepr_over_dB"}.
#' @param line.colors Character string, line color to plot EPR spectrum/spectra. All \pkg{ggplot2} compatible
#'   colors are allowed for plotting individual spectrum, \strong{default}: \code{line.colors = "steelblue"}.
#'   For series of EPR spectra two colorscales are used
#'   \enumerate{
#'   \item \strong{Continuous.} This is the case when \code{var2nd.series} \strong{IS NOT} \code{NULL}
#'   and \code{var2nd.series.slct.by = NULL}. The \code{line.colors} argument is identical with the continuous
#'   \code{colorscales} one from \code{\link[ggplot2]{scale_colour_gradientn}}. Following color definitions
#'   are allowed =>
#'   \itemize{
#'     \item an arbitrary vector color like \code{c("blue","green","red")} with the length of \eqn{\geq 2}
#'     \item any color definition from \pkg{grDevices} like \code{hcl.colors(n,pallete)}, \code{rainbow(n)},
#'     \code{heat.colors(n)}, \code{terrain.colors(n)}, \code{topo.colors(n)}, \code{cm.colors(n)} where the number
#'     of colors \eqn{n \geq 2} should be specified.
#'     See also \href{https://www.rdocumentation.org/packages/grDevices/versions/3.6.2/topics/Palettes}{grDevices Palettes}
#'     and \href{https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/}{HCL Color
#'     Palettes}
#'   }
#'
#'   \item \strong{Discrete.} This is the case when both \code{var2nd.series}
#'   as well as \code{var2nd.series.slct.by} are \strong{DISTINCT} from \code{NULL}. Following color definitions
#'   are allowed =>
#'   \itemize{
#'   \item an arbitrary vector color like \code{c("blue","green","red")} with the length of \eqn{\geq 2}
#'   \item any color definition from \code{\link[ggplot2]{scale_color_viridis_d}} \code{"option"}.
#'   These involve \code{"magma"} (or \code{"A"}), \code{"inferno"} (or \code{"B"}), \code{"plasma"} (or \code{"C"}),
#'   \code{"viridis"} (or \code{"D"}), \code{"cividis"} (or \code{"E"}), \code{"rocket"} (or \code{"F"}),
#'   \code{"mako"} (or \code{"G"}) and \code{"turbo"} (or \code{"H"})
#'   }
#'   }
#' @param line.width Numeric, linewidth of the plot line in \code{pt}, \strong{default}: \code{line.width = 0.75}
#' @param border.line.width tbc
#' @param border.line.color tbc
#' @param theme.basic Character/String, which calls a ggplot theme base. The following ones are defined:
#'   \describe{
#'     \item{\code{"theme_gray"}}{(\strong{default} one) => the gray background with white grid lines}
#'     \item{\code{"theme_bw"}}{ => the white background with thin gray grid lines}
#'     \item{\code{"theme_light"}}{ => similar to \code{theme_bw()} but without the pronounced axis black lines}
#'     \item{\code{"theme_classic"}}{ => without grid, pronounced axis lines, however no opposite ones}
#'     \item{\code{"theme_linedraw"}}{ => pronounced axis lines (both for origin and opposite)
#'     as well as the grid-lines, theme is proposed \strong{for publications}
#'     (if the \code{grid} is set to \code{FALSE})}
#'   }
#' @param axis.text.size Numeric, text size (in \code{pt}) for the axes units/descriptions,
#'   \strong{default}: \code{axis.text.size = 14}
#' @param axis.title.size Numeric, text size (in \code{pt}) for the axes title,
#'   \strong{default}: \code{axis.title.size = 15}
#' @param legend.title Character string tbc
#' @param legend.title.size tbc
#' @param legend.text.size description
#' @param grid Logical, whether to dislay the \code{grid} within the plot/graph, \strong{default}: \code{grid = TRUE}
#' @param yTicks Logical, whether to display the \code{y} (\code{dIepr_over_dB}) ticks and the corresponding text
#'   (not the axis title!), which is usually skipped in the EPR community, \strong{default}: \code{yTicks = TRUE}
#'
#'
#' @return EPR spectrum/spectra ('overlay' plot) by \pkg{ggplot2} with key parameter
#'   (e.g. line-color and theme,grid...etc.) variation
#'
#'
#' @examples
#' \dontrun{
#' plot_EPR_Specs(data.spectra)
#' plot_EPR_Specs(data.spectra,
#'               x = "B_G",
#'               x.unit = "G",
#'               Intensity = "dIepr_over_dB_Sim")
#' plot_EPR_Specs(data.spectra,
#'               x = "B_mT_Sim",
#'               Intensity = "single_Integ")
#' plot_EPR_Specs(data.spectra,
#'               x = "Field",
#'               x.unit = "G",
#'               var2nd.series = "time_s",
#'               line.colors = grDevices::rainbow(6),
#'               basic.theme = "theme_linedraw",
#'               legend.title = "Time (s)",
#'               legend.title.size = 13,
#'               legend.text.size = 11,
#'               yTicks = FALSE)
#' plot_EPR_Specs(data.spectra,
#'               x = "g_Value",
#'               x.unit = "Unitless",
#'               theme.basic = "theme_bw",
#'               grid = TRUE)
#' plot_EPR_Specs(data.spectra,
#'               x = "RF_MHz",
#'               x.unit = "MHz",
#'               line.colors = "darkred",
#'               line.width = 1.2)
#' }
#'
#'
#' @export
#'
#'
#' @importFrom ggplot2 ggplot geom_line theme aes labs coord_cartesian scale_x_continuous scale_y_continuous
#'   scale_color_manual element_blank element_text element_rect dup_axis unit margin theme_bw theme_light theme_gray
#'   theme_minimal theme_classic theme_linedraw scale_color_gradientn theme scale_color_viridis_d
plot_EPR_Specs <- function(data.spectra,
                           x = "B_mT",
                           x.unit = "mT",
                           xlim = NULL,
                           var2nd.series = NULL,
                           var2nd.series.slct.by = NULL,
                           Intensity = "dIepr_over_dB",
                           line.colors = "steelblue",
                           line.width = 0.75,
                           border.line.width = 0.5,
                           border.line.color = "black",
                           theme.basic = "theme_gray",
                           axis.title.size = 15,
                           axis.text.size = 14,
                           legend.title = NULL,
                           legend.title.size = NULL,
                           legend.text.size = NULL,
                           grid = TRUE,
                           yTicks = TRUE) {
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## Define limits if `xlim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.x.region <- c(min(data.spectra[[x]]),max(data.spectra[[x]]))
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
    "integSigm", "IntegDoub", "integDoub", "area", "Area", "AREA"
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
    simplePlot <- ggplot(data.spectra) +
      geom_line(aes(x = .data[[x]], y = .data[[Intensity]]),
                linewidth = line.width, color = line.colors, show.legend = FALSE
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
    if (is.null(var2nd.series)){
      simplePlot <- ggplot(data.spectra) +
        geom_line(aes(x = .data[[x]], y = .data[[Intensity]],color = ""),
                  linewidth = line.width) +
        coord_cartesian(xlim = x.plot.limits) +
        scale_color_manual(values = line.colors) +
        labs(color = legend.title, x = x.label, y = y.label)
    } else{
      if (is.null(legend.title)){
        stop(" The `legend.title` is not specified. Please, define ! ")
      } else{
        ## Legend title and text definition
        legend.title.size <- legend.title.size %>% `if`(is.null(legend.title.size),13, .)
        legend.text.size <- legend.text.size %>% `if`(is.null(legend.text.size),11, .)
        #
        simplePlot <- ggplot(data.spectra) +
          geom_line(aes(x = .data[[x]],
                        y = .data[[Intensity]],
                        color = .data[[var2nd.series]]),
                    linewidth = line.width) +
          coord_cartesian(xlim = x.plot.limits) +
          scale_color_gradientn(colors = line.colors) +
          labs(color = legend.title, x = x.label, y = y.label) +
          theme(legend.title = element_text(size = legend.title.size),
                legend.text = element_text(size = legend.text.size))
        #
        if (!is.null(var2nd.series.slct.by)){
          ## OVERLAY SELECT PLOT
          ## `var2nd.series` definition
          var2nd.series.df <- data.spectra %>%
            dplyr::group_by(.data[[var2nd.series]]) %>%
            dplyr::group_keys()
          #
          var2nd.series.keys <- var2nd.series.df[[var2nd.series]]
          var2nd.series.len <- length(var2nd.series.keys)
          #
          data.spectra <- data.spectra %>%
            dplyr::filter(.data[[var2nd.series]] %in% var2nd.series.keys[seq(1,var2nd.series.len,
                                                                             by = var2nd.series.slct.by)]) %>%
            dplyr::mutate(!!rlang::quo_name(var2nd.series) := as.factor(.data[[var2nd.series]]))
          #
          ## simple plot without color
          simplePlot.nocolor <- ggplot(data.spectra) +
            geom_line(aes(x = .data[[x]],
                          y = .data[[Intensity]],
                          color = .data[[var2nd.series]]),
                      linewidth = line.width) +
            coord_cartesian(xlim = x.plot.limits)
          #
          ## colors definition for the plot
          if (length(line.colors) > 1){
            plot.vector.colors <-
              grDevices::colorRampPalette(colors = line.colors)(var2nd.series.len/var2nd.series.slct.by)
            #
            simplePlot <- simplePlot.nocolor +
              scale_color_manual(values = plot.vector.colors) +
              labs(color = legend.title, x = x.label, y = y.label) +
              theme(legend.title = element_text(size = legend.title.size),
                    legend.text = element_text(size = legend.text.size))
          }
          if (length(line.colors) == 1){
            plot.vector.colors <- line.colors
            #
            simplePlot <- simplePlot.nocolor +
              scale_color_viridis_d(option = plot.vector.colors) +
              labs(color = legend.title, x = x.label, y = y.label) +
              theme(legend.title = element_text(size = legend.title.size),
                    legend.text = element_text(size = legend.text.size))
          }
         #
        }
        #
      }
      #
    }
    #
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
