#
#' Essential Plotting of EPR/ENDOR Spectrum/Spectra
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description Plot of EPR/ENDOR spectrum/spectra (overlay plot) based
#'   on \href{https://ggplot2.tidyverse.org/}{ggplot2}-functionality. Spectral data input must be available
#'   as a data frame object (see also argument \code{data.spectra}).
#'   Theme of the graphic spectrum representation as well as other plot elements can be varied like
#'   using the \code{{ggplot2}} package. Within a theme, the \code{y} ticks can be either displayed
#'   or skipped and the intensity (e.g. \code{dIepr_over_dB}) is presented in "procedure defined unit"
#'   (see \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6803776/}{p.d.u.}),
#'   which is common for visualization of EPR spectra.
#'
#'
#' @details
#'   Output from the \code{plot_EPR_Specs} can be additionally combined by the \code{+} sign with other
#'   \code{ggplot2} or internal plotting functions
#'   (e.g. with \code{\link{plot_theme_In_ticks}}). For details of \code{ggplot2} theme elements, please
#'   refer to \href{https://ggplot2.tidyverse.org/reference/theme.html}{Modify Components of a Theme}
#'   (see also \code{\link[ggplot2]{theme}}) or to
#'   \href{https://henrywang.nl/ggplot2-theme-elements-demonstration/}{ggplot2 Elements Demonstration by Henry Wang}.
#'   The actual function does not assume a deep knowledge of \href{https://ggplot2.tidyverse.org/}{ggplot2}
#'   package (as well as the grammar of graphics philosophy, see \code{vignette("functionality")}) and literally,
#'   one can easily start to plot from scratch (see also building plot in example of \code{\link{plot_theme_NoY_ticks}}).
#'   However, there is a \href{https://dreamrs.github.io/esquisse/index.html}{great interactive Rstudio addin/package}
#'   called \code{esquisse} which nicely helps to dive into the \href{https://ggplot2.tidyverse.org/}{ggplot2}
#'   even for complete beginners.
#'
#'
#' @param data.spectra Data frame/table object containing magnetic flux density, \eqn{g}-value
#'   or radio-frequency columns as \code{x} variable. They can be labeled as \code{Field}, \code{B_mT},
#'   \code{B_G} or \code{RF_MHz} see also \code{x} argument. The \code{y / Intensity} variable
#'   can be labeled as \code{dIepr_over_dB}, in case of derivative intensity, or if
#'   integrated or simulated spectra intensities are present, they can be labeled accordingly.
#'   See also \code{Intensity} parameter/argument. For spectral series the second independent variable
#'   \code{var2nd.series} column (e.g. \code{var2nd.series = "time_s"} or \code{var2nd.series = "T_K"})
#'   must be available. In such case the entire \code{data.spectra} must inherit the form
#'   of \href{https://r4ds.had.co.nz/tidy-data.html}{tidy/long table format} (see also argument \code{var2nd.series}).
#' @param x Character string pointing to \eqn{x}-axis/column quantity header in the original \code{data.spectra}
#'   like magnetic flux density \eqn{B}, \eqn{g}-Value or \eqn{RF} (radio frequency),
#'   \strong{default}: \code{x = "B_mT"}.
#' @param x.unit Character string pointing to unit of \code{x}-quantity coming from the original \code{data.spectra}.
#'   Units like \code{"G"} (Gauss), \code{"mT"} (millitesla), \code{"MHz"} (megahertz in case of ENDOR spectra)
#'   or \code{"Unitless"} / \code{"unitless"} (in case of \eqn{g}-values) can be used. \strong{Default}: \code{x.unit = "mT"}.
#' @param xlim Numeric vector referring to lower and upper limit of the selected \eqn{x}-region,
#'   e.g. \code{xlim = c(3495.4,3595.4)} (\eqn{B} in \code{G}) or \code{xlim = c(12.5,21.2)} (\eqn{RF} in \code{MHz})
#'   or \code{xlim = c(2.004,2.001)} (dimensionless \eqn{g}). \strong{Default}: \code{xlim = NULL} (actually corresponding
#'   to the entire \eqn{x}-range).
#' @param var2nd.series Character string referred to name of the second independent variable/quantity
#'   column in the original \code{data.spectra} (such as time, Temperature, Electrochemical Potential,
#'   Microwave Power...etc) altered upon individual experiments. Data must be available in \strong{long table} / \strong{tidy}
#'   format (see also \code{\link{readEPR_Exp_Specs_multif}} or \code{\link{plot_EPR_Specs2D_interact}}).
#'   \strong{Default}: \code{var2nd.series = NULL}. Otherwise \strong{usually} \code{var2nd.series = "time_s"}.
#' @param var2nd.series.slct.by Numeric, number corresponding to each \eqn{n-th} presented spectrum in the overlay plot,
#'   e.g. display each second (\code{var2nd.series.slct.by = 2}) or third (\code{var2nd.series.slct.by = 3}),
#'   ...etc. spectrum. The argument is only used if \code{var2nd.series} is \strong{NOT NULL}
#'   (e.g. \code{var2nd.series = "time_s"}) and one wants to present
#'   DISCRETE LABELS / LEVELS for the overlay spectra (see also \code{line.colors} argument).
#'   THE \code{var2nd.series.slct.by = 1} MAY DISPLAY ALL DISCRETE SPECTRA WITHIN THE SERIES.
#'   However, the RECOMENDED MAX. NUMBER of spectra/lines IS \code{12}.
#' @param Intensity Character string pointing to \code{intensity column} name in the original \code{data.spectra}
#'   if other than \code{dIepr_over_dB} name/label is used (e.g. for simulated or integrated spectra),
#'   \strong{default}: \code{Intesity = "dIepr_over_dB"}.
#' @param Ilim Numeric vector corresponding to limits of the selected \code{y} / \code{Intensity}
#'   region, e.g. \code{Ilim = c(-2e-3,2e-3)}. \strong{Default}: \code{Ilim = NULL} (actually corresponding
#'   to the entire Intensity range).
#' @param lineSpecs.form Character string describing either \code{"derivative"} (\strong{default})
#'   or \code{"integrated"} (in such case also \code{"absorption"} can be used)
#'   line form of the analyzed EPR spectrum/data.
#' @param line.colors Character string, line color(s) to plot EPR spectrum/spectra. All \code{{ggplot2}} compatible
#'   colors are allowed to plot the individual spectrum, \strong{default}: \code{line.colors = "steelblue"}.
#'   For the series of EPR spectra two color scales are used:
#'   \enumerate{
#'   \item \strong{Continuous.} This is the case when \code{var2nd.series} \strong{IS NOT} \code{NULL}
#'   and \code{var2nd.series.slct.by = NULL}. The \code{line.colors} argument is identical with the continuous
#'   \code{colorscales}, i.e. with the one from \code{\link[ggplot2]{scale_colour_gradientn}}. Following color definitions
#'   are allowed =>
#'   \itemize{
#'     \item any color vector like \code{c("blue","green","red")} with the length of \eqn{\geq 2}
#'     \item any color definition from \href{https://r-universe.dev/manuals/grDevices.html}{grDevices}
#'     like \code{hcl.colors(n,pallete)}, \code{rainbow(n)},
#'     \code{heat.colors(n)}, \code{terrain.colors(n)}, \code{topo.colors(n)}, \code{cm.colors(n)} where the number
#'     of colors \eqn{n \geq 2} should be specified.
#'     See also \href{https://www.rdocumentation.org/packages/grDevices/versions/3.6.2/topics/Palettes}{grDevices Palettes}
#'     and \href{https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/}{HCL Color
#'     Palettes}
#'   }
#'
#'   \item \strong{Discrete.} This is the case when both \code{var2nd.series}
#'   as well as \code{var2nd.series.slct.by} are \strong{DISTINCT} FROM \code{NULL}. Following color definitions
#'   are allowed =>
#'   \itemize{
#'   \item any color vector like \code{c("blue","green","red")} with the length of \eqn{\geq 2}
#'   \item any color definition from \code{\link[ggplot2]{scale_color_viridis_d}} \code{"option"}.
#'   These involve \code{line.colors = "magma"} (or ...\code{"A"}), \code{line.colors = "inferno"} (or ...\code{"B"}),
#'   \code{line.colors = "plasma"} (or ...\code{"C"}), \code{line.colors = "viridis"} (or ...\code{"D"}),
#'   \code{line.colors = "cividis"} (or ...\code{"E"}), \code{line.colors = "rocket"} (or ...\code{"F"}),
#'   \code{line.colors = "mako"} (or ...\code{"G"}) and \code{line.colors = "turbo"} (or ...\code{"H"})
#'   }
#'   }
#' @param line.width Numeric, linewidth of the plot line in \code{mm}, \strong{default}: \code{line.width = 0.75}.
#' @param line.type Character string or integer corresponding to width of the (spectral) line(s). Following types
#'   can be specified: \code{0 = "blank"}, \code{1 = "solid"} (\strong{default}), \code{2 = "dashed"}, \code{3 = "dotted"},
#'   \code{4 = "dotdash"}, \code{5 = "longdash"} and \code{6 = "twodash"}.
#' @param border.line.width Numeric, width (in \code{mm}) of the plot graph/panel border line, \strong{default}:
#'   \code{border.line.width = 0.5}.
#' @param border.line.color Character string referring to color of the plot graph/panel border line. \strong{Default}:
#'   \code{border.line.color = "black"}.
#' @param border.line.type Character string or integer corresponding to width of the plot graph/panel border line. Following types
#'   can be specified: \code{0 = "blank"}, \code{1 = "solid"} (\strong{default}), \code{2 = "dashed"}, \code{3 = "dotted"},
#'   \code{4 = "dotdash"}, \code{5 = "longdash"} and \code{6 = "twodash"}.
#' @param theme.basic Character string calling a \code{ggplot} theme base. The following ones are defined:
#'   \describe{
#'     \item{\code{"theme_gray"}}{(\strong{default} one) => gray background with the white grid lines}
#'     \item{\code{"theme_bw"}}{ => white background with thin gray grid lines, the theme is suggested \strong{for publications}}
#'     \item{\code{"theme_light"}}{ => similar to \code{theme_bw()} but without the pronounced axis black lines}
#'     \item{\code{"theme_classic"}}{ => without grid, pronounced axis lines, however no opposite ones}
#'     \item{\code{"theme_linedraw"}}{ => pronounced axis lines (both for the origin and the opposite) as well as the grid ones,
#'     theme is suggested \strong{for publications} if the \code{grid} is set to \code{FALSE}}
#'   }
#' @param axis.text.size Numeric, text size in \code{p}t for the axes unit values/descriptions,
#'   \strong{default}: \code{axis.text.size = 14}.
#' @param axis.title.size Numeric, axis text title size in \code{pt}. \strong{Default}: \code{axis.title.size = 15}.
#' @param legend.title Character string identical to legend title, e.g. \code{legend.title = "Time (s)"},
#'   \code{legend.title = "Electrochem. Potential (V)"} or \code{legend.title = "Sample"}. \strong{Default}:
#'   \code{legend.title = NULL} in case of \code{var2nd.series = NULL}.
#' @param legend.title.size Numeric, legend text title size in \code{pt},\strong{default}: \code{legend.title.size = NULL},
#'   actually corresponding to 13/13pt.
#' @param legend.text.size Numeric, legend text size in \code{pt}, \strong{default}: \code{legend.text.size = NULL},
#'   actually corresponding to 11/11pt.
#' @param grid Logical, whether to display the \code{grid} within the panel/graph, \strong{default}: \code{grid = TRUE}.
#' @param yTicks Logical, whether to display the \code{y} (\code{dIepr_over_dB}) ticks and the corresponding text
#'   (not the axis title!), which is usually skipped in the EPR community, \strong{default}: \code{yTicks = TRUE}
#'   (the axis ticks as well as the text are present).
#'
#'
#' @return EPR spectrum/spectra ('overlay') plot object (corresponding to list of plot components) using
#'   the \code{ggplot2} functionality, with the key parameter variations like line color, theme, grid...etc.
#'
#'
#' @examples
#' ## load the following built-in spectral data:
#' aminoxyl.file.path <-
#' load_data_example("Aminoxyl_radical_a.txt")
#' ## read the aminoxyl radical spectrum without intensity
#' ## normalization
#' aminoxyl.data <- readEPR_Exp_Specs(aminoxyl.file.path)
#' #
#' ## simple plot of an EPR spectrum with B in `mT`
#' ## and dIepr_over_dB_Sim in `p.d.u.` (derivative intensity)
#' plot_EPR_Specs(aminoxyl.data)
#' #
#' ## the same spectrum with B in `G` and `theme_bw`
#' ## ggplot2 graph theme
#' plot_EPR_Specs(data.spectra = aminoxyl.data,
#'                x = "B_G",
#'                x.unit = "G",
#'                theme.basic = "theme_bw")
#' #
#' ## single integration (without baseline correction)
#' ## of the previous spectrum by the `eval_integ_EPR_Spec`
#' ## function
#' aminoxyl.data.sinteg <-
#'   eval_integ_EPR_Spec(aminoxyl.data)
#' ## plot the single integrated spectrum
#' plot_EPR_Specs(aminoxyl.data.sinteg,
#'                x = "B_G",
#'                x.unit = "G",
#'                Intensity = "single_Integ",
#'                line.colors = "darkorange",
#'                line.type = "dashed",
#'                lineSpecs.form = "integrated")
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
#' ## plotting the previous ENDOR data spectrum
#' ## in derivative form with `darkred` linecolor
#' ## and 1.2pt linewidth:
#' plot_EPR_Specs(pnt.endor.data,
#'                x = "RF_MHz",
#'                x.unit = "MHz",
#'                line.colors = "darkred",
#'                line.width = 1.2)
#' #
#' ## loading the built-in package example to demonstrate
#' ## visualizatioin of time series EPR spectra:
#' triarylamine.decay.series.dsc.path <-
#' load_data_example(file =
#'         "Triarylamine_radCat_decay_series.DSC")
#' triarylamine.decay.series.asc.path <-
#' load_data_example(file =
#'         "Triarylamine_radCat_decay_series.zip")
#' unzip(triarylamine.decay.series.asc.path,
#'       exdir = tempdir()
#'       )
#' ## loading the kinetics:
#' triarylamine.decay.series.data <-
#'   readEPR_Exp_Specs_kin(name.root =
#'     "Triarylamine_radCat_decay_series",
#'   dir_ASC = tempdir(),
#'   dir_dsc_par =
#'     system.file("extdata",
#'                 package = "eprscope")
#' )
#' #
#' ## plot with the CONTINUOUS COLORSCALE (see argument
#' ## `line.colors` and color definitions `1.`):
#' plot_EPR_Specs(triarylamine.decay.series.data$df,
#'                var2nd.series = "time_s",
#'                line.colors =
#'                grDevices::hcl.colors(8,palette = "Roma"),
#'                legend.title = "Time (s)",
#'                yTicks = FALSE)
#' #
#' ## plot with the DISCRETE COLORSCALE (see argument
#' ## `line.colors` and color definitions `2.`),
#' ## each 10-th spectrum from the original plot is presented:
#' plot_EPR_Specs(triarylamine.decay.series.data$df,
#'                var2nd.series = "time_s",
#'                var2nd.series.slct.by = 10,
#'                line.colors = c("darkblue",
#'                                "darkorange",
#'                                "darkviolet"),
#'                legend.title = "Time (s)")
#' #
#' \dontrun{
#' ## display the variable temperature experiment selecting
#' ## the DISCRETE COLORSCALE where all EPR spectra from
#' ## the series are presented:
#' plot_EPR_Specs(data.spectra,
#    xlim = c(334,345),
#'   Ilim = c(-4e-3,4e-3),
#'   var2nd.series = "T_K", # temperature in Kelvins
#'   var2nd.series.slct.by = 1,
#'   line.colors = c("darkblue",
#'                   "darkorange",
#'                   "magenta",
#'                   "steelblue"),
#'   legend.title = "Temperature (K)")
#' }
#'
#'
#' @export
#'
#'
#' @importFrom ggplot2 ggplot geom_line theme aes labs coord_cartesian scale_x_continuous scale_y_continuous
#'   scale_color_manual scale_x_reverse element_blank element_text element_rect dup_axis unit margin theme_bw
#'   theme_light theme_gray theme_minimal theme_classic theme_linedraw scale_color_gradientn theme scale_color_viridis_d
plot_EPR_Specs <- function(data.spectra,
                           x = "B_mT",
                           x.unit = "mT",
                           xlim = NULL,
                           var2nd.series = NULL,
                           var2nd.series.slct.by = NULL,
                           Intensity = "dIepr_over_dB",
                           Ilim = NULL,
                           lineSpecs.form = "derivative",
                           line.colors = "darkviolet",
                           line.width = 0.75,
                           line.type = 1,
                           border.line.width = 0.5,
                           border.line.color = "black",
                           border.line.type = 1,
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
  ## Define limits for `y` (similarly like in case of `x` above)
  ## 10% of the `max` below the `min` and 10% above the `max`
  data.y.region <- c(min(data.spectra[[Intensity]]),max(data.spectra[[Intensity]]))
  data.y.region.2 <- c(data.y.region[1]  - (data.y.region[2] * 0.1),data.y.region[2] * 1.1)
  Ilim <- Ilim %>% `if`(is.null(Ilim),data.y.region.2, .)
  #
  ## EPR spectrum borders for the visualization (see 'coord_cartesian')
  x.start <- xlim[1]
  x.end <- xlim[2]
  #
  ## Labels based on `Intensity` and `x` quantity (B, g, RF) conditions:
  ## Select labels by defining the corresponding vectors
  slct.vec.x.g <- c(
    "g_value", "g_Value", "gval", "gVal",
    "g_factor", "g_Factor", "gfac", "gFac","g"
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
  ## g-factor condition =>
  g.factor.cond <- ifelse(any(grepl(paste(slct.vec.x.g,collapse = "|"), x)),TRUE,FALSE)
  #
  if (lineSpecs.form == "derivative") {
    y.label <- bquote("d" ~ italic(I)[EPR] ~ "/" ~ "d" ~ italic(B) ~ ~"(" ~ p.d.u. ~ ")")
  }
  if (lineSpecs.form == "integrated" || lineSpecs.form == "absorption") {
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
    panel.border = element_rect(color = border.line.color,
                                fill = NA,
                                linewidth = border.line.width,
                                linetype = border.line.type)
  ) ## theme in order to have ticks outside the graph
  theme.Noticks <- theme(
    axis.ticks.length = unit(-6, "pt"),
    axis.text.x = element_text(margin = margin(6, 6, 6, 6, unit = "pt"), size = axis.text.size),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    axis.title.y = element_text(margin = margin(2, 8, 2, 6, unit = "pt"), size = axis.title.size),
    axis.title.x = element_text(margin = margin(2, 6, 2, 6, unit = "pt"), size = axis.title.size),
    panel.border = element_rect(color = border.line.color,
                                fill = NA,
                                linewidth = border.line.width,
                                linetype = border.line.type)
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
  if (is.null(legend.title) & is.null(var2nd.series.slct.by)){
    if (!is.null(var2nd.series)){
      stop(" Either the `var2nd.series` must be `NULL` \n
           or define the `legend.title` (+ `var2nd.series.slct.by`) ! ")
    } else{
      simplePlot <- ggplot(data.spectra) +
        geom_line(aes(x = .data[[x]], y = .data[[Intensity]]),
                  linewidth = line.width,
                  linetype = line.type,
                  color = line.colors,
                  show.legend = FALSE
        ) +
        coord_cartesian(xlim = x.plot.limits,ylim = Ilim) +
        {if(g.factor.cond)scale_x_reverse()} +
        labs(x = x.label, y = y.label)
    }
  }
  if (!is.null(legend.title)){
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
        legend.title <- bquote(atop(italic(.(legend.strings[1])), # `atop()` is an equivalent of "\n"
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
                  linewidth = line.width,
                  linetype = line.type) +
        coord_cartesian(xlim = x.plot.limits,ylim = Ilim) +
        scale_color_manual(values = line.colors) +
        {if(g.factor.cond)scale_x_reverse()} +
        labs(color = legend.title, x = x.label, y = y.label)
    } else {
      if (is.null(legend.title)){
        stop(" The `legend.title` is not specified. Please, define ! ")
      } else{
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
          ## filtering the `var2nd.series.slct.by` plots
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
                      linewidth = line.width,
                      linetype = line.type) +
            coord_cartesian(xlim = x.plot.limits,ylim = Ilim)
          #
          ## colors definition for the plot
          if (length(line.colors) > 1){
            plot.vector.colors <-
              grDevices::colorRampPalette(colors = line.colors)(var2nd.series.len/var2nd.series.slct.by)
            #
            simplePlot <- simplePlot.nocolor +
              {if(g.factor.cond)scale_x_reverse()} +
              scale_color_manual(values = plot.vector.colors) +
              labs(color = legend.title, x = x.label, y = y.label)
          }
          if (length(line.colors) == 1){
            plot.vector.colors <- line.colors
            #
            simplePlot <- simplePlot.nocolor +
              {if(g.factor.cond)scale_x_reverse()} +
              scale_color_viridis_d(option = plot.vector.colors,
                                    direction = 1) +
              labs(color = legend.title, x = x.label, y = y.label)
          }
         #
        } else {
          simplePlot <- ggplot(data.spectra) +
            geom_line(aes(x = .data[[x]],
                          y = .data[[Intensity]],
                          color = .data[[var2nd.series]]),
                      linewidth = line.width,
                      linetype = line.type) +
            coord_cartesian(xlim = x.plot.limits,ylim = Ilim) +
            {if(g.factor.cond)scale_x_reverse()} +
            scale_color_gradientn(colors = line.colors) +
            labs(color = legend.title, x = x.label, y = y.label)
        }
      }
      #
    }
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
  ## conditions for legend
  ## Legend title and text definition
  legend.title.size.def <- legend.title.size %>% `if`(is.null(legend.title.size),13, .)
  legend.text.size.def <- legend.text.size %>% `if`(is.null(legend.text.size),11, .)
  if (!is.null(legend.title)){
    return(
      p +
        theme(legend.title = element_text(size = legend.title.size.def),
              legend.text = element_text(size = legend.text.size.def)
              )
    )
  } else {
    return(p)
  }
  #
}
