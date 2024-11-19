#'
#' Custom \code{ggplot2} Theme with Axis Ticks Oriented Inside the Panel
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#'   Change the \href{https://ggplot2.tidyverse.org/}{ggplot2}-based theme in order to meet the needs
#'   of graph (such as EPR spectrum, kinetic profiles...etc)
#'   visuals/non-data components of the actual plot. Theme can be mainly applied
#'   for the basic \href{https://ggplot2.tidyverse.org/}{ggplot2} components like
#'   \code{ggplot() + geom_...() + ...} and consists of highlighted panel borders, grid and axis ticks pointing
#'   \strong{inside the graph/plot panel}. For details of \code{ggplot2} theme elements please,
#'   refer to \href{https://ggplot2.tidyverse.org/reference/theme.html}{Modify Components of a Theme}
#'   (see also the \code{\link[ggplot2]{theme}}) or to
#'   \href{https://henrywang.nl/ggplot2-theme-elements-demonstration/}{ggplot2 Elements Demonstration by Henry Wang}.
#'
#'
#' @param axis.text.size Numeric, text size (in \code{pt}) for the axes units/descriptions,
#'   \strong{default}: \code{axis.text.size = 14}.
#' @param axis.title.size Numeric, text size (in \code{pt}) for the axes title,
#'   \strong{default}: \code{axis.title.size = 15}.
#' @param grid Logical, whether to display the \code{grid} within the plot/graph panel, \strong{default}: \code{grid = TRUE}.
#' @param border.line.color Character string, setting up the color of the plot panel border line, \strong{default}:
#'   \code{border.line.color = "black"}.
#' @param border.line.type Character string or integer, corresponding to width of the graph/plot panel border line. Following types
#'   can be specified: \code{0 = "blank"}, \code{1 = "solid"} (\strong{default}), \code{2 = "dashed"}, \code{3 = "dotted"},
#'   \code{4 = "dotdash"}, \code{5 = "longdash"} and \code{6 = "twodash"}..
#' @param border.line.width Numeric, width (in \code{mm}) of the plot panel border line, \strong{default}:
#'   \code{border.line.width = 0.5}.
#' @param bg.transparent Logical, whether the \strong{entire plot background} (excluding the \strong{panel})
#'   should be transparent, \strong{default}: \code{bg.transparent = FALSE}, i.e. no transparent background.
#' @param ... additional arguments specified by the \code{\link[ggplot2]{theme}} (such as \code{panel.backgroud},
#'   \code{axis.line},...etc), which are not specified otherwise.
#'
#'
#' @return Custom \pkg{ggplot2} \code{theme} (list) with \code{x,y-axis} ticks pointing inside the graph/plot panel.
#'
#'
#' @examples
#' ## loading the aminoxyl radical CW EPR spectrum:
#' aminoxyl.data.path <-
#'   load_data_example(file = "Aminoxyl_radical_a.txt")
#' aminoxyl.data <-
#'   readEPR_Exp_Specs(aminoxyl.data.path,
#'                     qValue = 2100)
#' #
#' ## simple `ggplot2` without any theme customization
#' ggplot2::ggplot(data = aminoxyl.data) +
#'   ggplot2::geom_line(
#'   ggplot2::aes(x = B_mT,y = dIepr_over_dB)
#'   )
#' #
#' ## simple `ggplot2` with `in-ticks` theme and tile
#' ggplot2::ggplot(data = aminoxyl.data) +
#'   ggplot2::geom_line(
#'     ggplot2::aes(x = B_mT,y = dIepr_over_dB)
#'     ) +
#'   plot_theme_In_ticks() +
#'   ggplot2::ggtitle(
#'   label = "EPR Spectrum of Aminoxyl Radical"
#'   )
#' #
#' ## basic EPR spectrum plot by the `plot_EPR_Specs()`
#' plot_EPR_Specs(data.spectra = aminoxyl.data)
#' #
#' ## previous spectrum combined with `in-ticks` theme
#' ## without the panel background
#' plot_EPR_Specs(data.spectra = aminoxyl.data) +
#'   plot_theme_In_ticks(
#'     panel.background = ggplot2::element_blank()
#'     )
#'
#'
#' @export
#'
#'
plot_theme_In_ticks <- function(axis.text.size = 14,
                                axis.title.size = 15,
                                grid = TRUE,
                                border.line.color = "black",
                                border.line.type = 1,
                                border.line.width = 0.5,
                                bg.transparent = FALSE,
                                ...) {
  ## theme parts:
  theme_bas <- theme(
    axis.ticks.length = unit(-6, "pt"),
    axis.text.x = element_text(margin = margin(6, 6, 4, 6, unit = "pt"), size = axis.text.size),
    axis.text.y = element_text(margin = margin(4, 6, 6, 0, unit = "pt"), size = axis.text.size),
    axis.title.y = element_text(margin = margin(2, 8, 2, 6, unit = "pt"), size = axis.title.size),
    axis.title.x = element_text(margin = margin(2, 6, 2, 6, unit = "pt"), size = axis.title.size),
    panel.border = element_rect(color = border.line.color,
                                linetype = border.line.type,
                                linewidth = border.line.width,
                                fill = NA),
    plot.title = element_text(margin = margin(b = -4)),
    plot.subtitle = element_text(margin = margin(t = 8,b = -6)),
    ...
  )
  #
  ## no-grid theme part
  theme_Nogrid <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  #
  ## duplicate axis with in-ticks
  axis_x_duplicate <- scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL))
  axis_y_duplicate <- scale_y_continuous(sec.axis = dup_axis(name = "",labels = NULL))
  #
  ## theme:
  if (isTRUE(bg.transparent)) {
    if (isTRUE(grid)) {
      thm <- theme_bas +
        theme(plot.background = element_rect(fill = "transparent"))
    } else {
      thm <- theme_bas +
        theme_Nogrid +
        theme(plot.background = element_rect(fill = "transparent"))
    }
  } else {
    if (isTRUE(grid)) {
      thm <- theme_bas
    } else {
      thm <- theme_bas +
        theme_Nogrid
    }
  }
  #
  ## with the theme + `scaling` function a list must be returned:
  ## (see also https://stackoverflow.com/questions/35559303/r-how-to-add-scale-functio-to-a-theme-object-in-ggplot2)
  return(list(thm,axis_x_duplicate,axis_y_duplicate))
  #
}
#
#
#
#
#' Custom \code{ggplot2} Theme without \code{Y} Axis Ticks
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#'   Change the \href{https://ggplot2.tidyverse.org/}{ggplot2}-based theme in order
#'   to meet the needs of graph (such as EPR spectrum, kinetic profiles...etc)
#'   visuals/non-data components of the actual graph/plot. The theme can be mainly applied for the basic \code{ggplot2} components like
#'   \code{ggplot() + geom_...() + ...} and consists of highlighted panel borders, grid and \strong{x-axis ticks} pointing
#'   \strong{inside the plot panel}. The \strong{y-axis ticks} are \strong{skipped} (see also \code{\link{plot_EPR_Specs}}).
#'   For details of \code{ggplot2} theme elements please,
#'   refer to \href{https://ggplot2.tidyverse.org/reference/theme.html}{Modify Components of a Theme}
#'   (see also \code{\link[ggplot2]{theme}}) or to
#'   \href{https://henrywang.nl/ggplot2-theme-elements-demonstration/}{ggplot2 Elements Demonstration by Henry Wang}.
#'
#'
#' @inheritParams plot_theme_In_ticks
#'
#'
#' @return Custom \pkg{ggplot2} \code{theme} (list) with \code{x-axis} ticks pointing inside the graph/plot panel,
#'   and with \code{y-ticks} being not displayed.
#'
#'
#' @examples
#' #' ## loading the aminoxyl radical CW EPR spectrum:
#' aminoxyl.data.path <-
#'   load_data_example(file = "Aminoxyl_radical_a.txt")
#' aminoxyl.data <-
#'   readEPR_Exp_Specs(aminoxyl.data.path,
#'                     qValue = 2100)
#' #
#' ## simple `ggplot2` without any theme customization
#' ggplot2::ggplot(data = aminoxyl.data) +
#'   ggplot2::geom_line(
#'     ggplot2::aes(x = B_mT,y = dIepr_over_dB)
#'     )
#' #
#' ## simple `ggplot2` with `noY-ticks` theme and tile
#' ## (+subtitle)
#' ggplot2::ggplot(data = aminoxyl.data) +
#'   ggplot2::geom_line(
#'     ggplot2::aes(x = B_mT,y = dIepr_over_dB)
#'     ) +
#'   plot_theme_NoY_ticks() +
#'   ggplot2::ggtitle(label = "Aminoxyl Radical",
#'                    subtitle = "EPR Spectrum")
#' #
#' ## comparison of EPR spectra generated
#' ## by `plot_EPR_Specs()` and by the simple
#' ## `ggplot2` with `noY-ticks` theme
#' plot_EPR_Specs(data.spectra = aminoxyl.data,
#'                yTicks = FALSE) +
#'   ggplot2::ggtitle(label = "Aminoxyl Radical",
#'                    subtitle = "EPR Spectrum")
#' #
#' ## loading example data (incl. `Area` and `time`
#' ## variables) from Xenon: decay of a triarylamine
#' ## radical cation after its generation
#' ## by electrochemical oxidation
#' triaryl_radCat_path <-
#'   load_data_example(file =
#'                      "Triarylamine_radCat_decay_a.txt")
#' ## corresponding data (double integrated EPR
#' ## spectrum = `Area` vs `time`)
#' triaryl_radCat_data <-
#'   readEPR_Exp_Specs(triaryl_radCat_path,
#'                     header = TRUE,
#'                     fill = TRUE,
#'                     select = c(3,7),
#'                     col.names = c("time_s","Area"),
#'                     x.unit = "s",
#'                     x.id = 1,
#'                     Intensity.id = 2,
#'                     qValue = 1700,
#'                     data.structure = "others") %>%
#'   na.omit()
#' #
#' ## simple plot of previous data using
#' ## the `noY-ticks` theme
#' ggplot2::ggplot(data = triaryl_radCat_data) +
#'   ggplot2::geom_point(
#'     ggplot2::aes(x = time_s,y = Area)
#'     ) +
#'   ggplot2::labs(title = "Radical Kinetics",
#'                 x = plot_labels_xyz(Time,s),
#'                 y = plot_labels_xyz(Double~~Integral,p.d.u.)) +
#'   plot_theme_NoY_ticks()
#'
#'
#' @export
#'
#'
plot_theme_NoY_ticks <- function(axis.text.size = 14,
                                 axis.title.size = 15,
                                 grid = TRUE,
                                 border.line.color = "black",
                                 border.line.type = 1,
                                 border.line.width = 0.5,
                                 bg.transparent = FALSE,
                                 ...) {
  ## theme parts:
  theme_bas <- theme(
    axis.ticks.length = unit(-6, "pt"),
    axis.text.x = element_text(margin = margin(6, 6, 4, 6, unit = "pt"), size = axis.text.size),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    axis.title.y = element_text(margin = margin(2, 8, 2, 6, unit = "pt"), size = axis.title.size),
    axis.title.x = element_text(margin = margin(2, 6, 2, 6, unit = "pt"), size = axis.title.size),
    panel.border = element_rect(color = border.line.color,
                                linetype = border.line.type,
                                linewidth = border.line.width,
                                fill = NA),
    plot.title = element_text(margin = margin(b = -4)),
    plot.subtitle = element_text(margin = margin(t = 8,b = -6)),
    ...
  )
  #
  ## no-grid theme part
  theme_Nogrid <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  #
  ## duplicate axis with in-ticks
  axis_x_duplicate <- scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL))
  #
  ## theme:
  if (isTRUE(bg.transparent)) {
    if (isTRUE(grid)) {
      thm <- theme_bas +
        theme(plot.background = element_rect(fill = "transparent"))
    } else {
      thm <- theme_bas +
        theme_Nogrid +
        theme(plot.background = element_rect(fill = "transparent"))
    }
  } else {
    if (isTRUE(grid)) {
      thm <- theme_bas
    } else {
      thm <- theme_bas +
        theme_Nogrid
    }
  }
  #
  ## with the theme + `scaling` function a list must be returned:
  ## (see also https://stackoverflow.com/questions/35559303/r-how-to-add-scale-functio-to-a-theme-object-in-ggplot2)
  return(list(thm,axis_x_duplicate))
  #
}
#
#
#
#
#' Custom \code{ggplot2} Theme with Axis Ticks Oriented Outside the Panel
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#'   Change the \href{https://ggplot2.tidyverse.org/}{ggplot2}-based theme in order to meet the needs
#'   of graph (such as EPR spectrum, kinetic profiles...etc)
#'   visuals/non-data components of the actual graph/plot. The theme can be mainly applied for the basic
#'   \code{ggplot2} components like \code{ggplot() + geom_...() + ...} and consists of highlighted panel borders,
#'   grid and axis ticks pointing \strong{outside of the plot panel}. For details of \code{ggplot2} theme elements please,
#'   refer to \href{https://ggplot2.tidyverse.org/reference/theme.html}{Modify Components of a Theme}
#'   (see also \code{\link[ggplot2]{theme}}) or to
#'   \href{https://henrywang.nl/ggplot2-theme-elements-demonstration/}{ggplot2 Elements Demonstration by Henry Wang}.
#'
#'
#' @inheritParams plot_theme_In_ticks
#'
#'
#' @return Custom \code{ggplot2} \code{theme} (list) with \code{x,y-axis} ticks pointing outside of the graph/plot panel.
#'
#'
#' @examples
#' ## loading TMPD built-in example file:
#' tmpd.data.file.path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.asc")
#' ## reading data:
#' tmpd.data.file <-
#'   readEPR_Exp_Specs(path_to_ASC = tmpd.data.file.path,
#'                     col.names = c("B_G","dIepr_over_dB"),
#'                     qValue = 3500,
#'                     norm.vec.add = 20,
#'                     origin = "winepr")
#' #
#' ggplot2::ggplot(data = tmpd.data.file,
#'        ggplot2::aes(x = B_G,y = dIepr_over_dB)
#'        ) +
#'   ggplot2::geom_line(linewidth = 0.75,color = "darkgreen") +
#'   ggplot2::xlab("B (G)") +
#'   ggplot2::ylab("dIepr / dB  (p.d.u.)") +
#'   plot_theme_Out_ticks()
#'
#'
#' @export
#'
#'
plot_theme_Out_ticks <- function(axis.text.size = 14,
                                 axis.title.size = 15,
                                 grid = TRUE,
                                 border.line.color = "black",
                                 border.line.type = 1,
                                 border.line.width = 0.5,
                                 bg.transparent = FALSE,
                                 ...) {
  ## theme parts:
  theme_bas <- theme(
    axis.ticks.length = unit(6, "pt"),
    axis.text.x = element_text(margin = margin(6, 6, 4, 6, unit = "pt"), size = axis.text.size),
    axis.text.y = element_text(margin = margin(4, 6, 6, 0, unit = "pt"), size = axis.text.size),
    axis.title.y = element_text(margin = margin(2, 8, 2, 6, unit = "pt"), size = axis.title.size),
    axis.title.x = element_text(margin = margin(2, 6, 2, 6, unit = "pt"), size = axis.title.size),
    panel.border = element_rect(color = border.line.color,
                                linetype = border.line.type,
                                linewidth = border.line.width,
                                fill = NA),
    plot.title = element_text(margin = margin(b = -4)),
    plot.subtitle = element_text(margin = margin(t = 8,b = -6)),
    ...
  )
  #
  theme_Nogrid <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  #
  ## theme:
  if (isTRUE(bg.transparent)) {
    if (isTRUE(grid)) {
      thm <- theme_bas +
        theme(plot.background = element_rect(fill = "transparent"))
    } else {
      thm <- theme_bas +
        theme_Nogrid +
        theme(plot.background = element_rect(fill = "transparent"))
    }
  } else {
    if (isTRUE(grid)) {
      thm <- theme_bas
    } else {
      thm <- theme_bas +
        theme_Nogrid
    }
  }
  #
  return(thm)
  #
}
