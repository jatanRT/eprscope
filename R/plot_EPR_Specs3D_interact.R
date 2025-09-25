#
#' Interactive 3D Surface and 2D Contour Plots for the Series of EPR Spectra
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#'   \strong{When viewing the web page, please wait for the images to load.} Interactive plotting of EPR spectra
#'   or their integrals based on \href{https://plotly.com/r/}{plotly} package.
#'   The aim of this function is to nicely visualize the series of EPR spectra (or their corresponding integrals),
#'   while checking out the EPR intensities upon e.g. kinetic (time series), variable temperature or simultaneous
#'   spectroelectrochemical experiments (potential series). For such purpose, the data frame input
#'   (see the \code{data.spectra.series} argument) is transformed into the \code{matrix} (with columns/variables corresponding
#'   to intensities at defined times, potentials, temperatures...etc.). In the next step the \code{\link[plotly]{plot_ly}}
#'   function generates either \strong{3D surface} or \strong{2D contour} plot objects which are finally customized
#'   by the \code{\link[plotly]{layout}} as well as by the \code{\link[plotly]{colorbar}}. Similarly, as for
#'   the \code{\link{plot_EPR_Specs2D_interact}} final plots can be stored or attached to a certain document format,
#'   using the \code{\link{plot_EPR_present_interact}} function (or directly within Rstudio), as \code{.png} or \code{.html}.
#'
#'
#' @inheritParams plot_EPR_Specs2D_interact
#' @param data.spectra.series Spectrum data frame/table, object containing magnetic flux density, \eqn{g}-value
#'   or radio-frequency columns as \code{x} variable. They can be labeled as \code{Field}, \code{B_mT}
#'   in mT (or \code{B_G} in gauss), see also \code{x} parameter/argument. The \code{y/Intensity} variable
#'   can be labeled as \code{dIepr_over_dB}, in case of derivative intensity, or if
#'   integrated or simulated spectral intensities are present, they can be labeled accordingly.
#'   See also \code{Intensity} parameter/argument. A second independent variable
#'   \code{var2nd.series} column (e.g. \code{var2nd.series = "time_s"}) must be available. In such case,
#'   the entire \code{data.spectra} must be present in the form of
#'   \href{https://r4ds.had.co.nz/tidy-data.html}{tidy/long table format}
#'   (see also parameter/argument \code{var2nd.series}).
#' @param var2nd.series Character string, referred to name of the second independent variable/quantity
#'   column in the original \code{data.spectra.series} (such as time, Temperature, Electrochemical Potential,
#'   Microwave Power...etc) altered upon individual experiments. Data must be available in \strong{long table}
#'   (or \strong{tidy}) \strong{format} (see also \code{data.spectra.series} argument).
#'   \strong{Default}: \code{var2nd.series = NULL}. Otherwise, usually, \code{var2nd.series = "time_s"}.
#' @param plot.type Character string, inherited from \code{\link[plotly]{plot_ly}}, specifying the trace. Only two
#'   character strings are available: \code{plot.type = "surface"} (\strong{default}, for 3D surface plots)
#'   or \code{plot.type = "contour"} (for 2D contour plots).
#' @param scheme.color Character string, corresponding to \code{color scale}.
#'   See also \href{https://plot.ly/r/reference/#surface-colorscale}{surface-colorscale}.
#'   Color scale must be an array containing arrays mapping a normalized value to an RGB, RGBa, HEX, HSL, HSV,
#'   or named color string. At minimum, a mapping for the lowest (0) and the highest (1) values are required.
#'   For example, \code{[[0, 'rgb(0,0,255)'], [1, 'rgb(255,0,0)']]} or as a list:
#'   \code{list(c(0, 1), c("tan", "blue"))} or \code{list(c(0, "tan"), c(1, "blue"))}. To control the bounds of the color
#'   scale in the corresponding space, use \code{cmin} and \code{cmax}. Alternatively, \code{color scale}
#'   may be a palette name string of the following list: \code{"Blackbody"},\code{"Bluered"},\code{"Blues"},
#'   \code{"Cividis"},\code{"Earth"},\code{"Electric"},\code{"Greens"},\code{"Greys"},\code{"Hot"},
#'   \code{"Jet"},\code{"Picnic"},\code{"Portland"},\code{"Rainbow"},\code{"RdBu"},\code{"Reds"},
#'   \code{"Viridis"},\code{"YlGnBu"},\code{"YlOrRd"}. \strong{Default}: \code{scheme.color = "Viridis"}.
#' @param contour.labels Logical, whether contours of intensity "hills" and "valleys" are projected
#'   onto the \eqn{x,y}-plane. \strong{Default}: \code{contour.labels = FALSE}.
#' @param xlim Numeric vector, pointing to lower and upper visual limit of the \eqn{x}-axis range.
#'   Assignment of \code{xlim = NULL} (\strong{default}) actually corresponds to the entire original
#'   range (see also the \code{data.spectra.series} argument).
#' @param xlab Character string \eqn{\equiv} title of the \eqn{x}-axis. Either simple, like
#'   \code{xlab = "B (mT)"} can be applied or if additional formatting is required,
#'   the \href{https://www.w3schools.com/html/html_formatting.asp}{\code{html} markup language} is used,
#'   such as \code{xlab = "<i>B</i> (mT)"} (\strong{default}). If a \eqn{\LaTeX} typesetting
#'   is required for the title, please refer to e.g. \href{https://plotly.com/r/LaTeX/}{LaTeX Plotly Tepesetting}.
#' @param ylab Character string \eqn{\equiv} title of the \eqn{y}-axis (see also \code{xlab}), \strong{default}:
#'   \code{ylab = "<i>Time</i> (s)"}.
#' @param zlab Character string \eqn{\equiv} title of the \eqn{z}-axis (see also \code{xlab}), \strong{default}:
#'   \code{zlab = "d <i>I</i><sub>EPR</sub> / d <i>B</i> (p.d.u.)"}.
#' @param bg.x.color Character string, setting the background color of the \eqn{x}-axis wall.
#'   \strong{Default}: \code{bg.x.color = "rgb(220, 220,220)"} (light gray). For additional color
#'   definitions, consult \href{https://www.w3.org/TR/css-color-3/}{CSS Color Module Homepage}.
#' @param grid.x.color Character string, pointing to color of \eqn{x}-axis grid lines,
#'   \strong{default}: \code{grid.x.color = "rgb(255, 255, 255)"} (white). For additional color
#'   definitions, consult \href{https://www.w3.org/TR/css-color-3/}{CSS Color Module Homepage}.
#' @param bg.y.color Character string, setting the background color of the \eqn{y}-axis wall (see also \code{bg.x.color}),
#'   \strong{default}: \code{bg.y.color = "rgb(220, 220,220)"} (light gray).
#' @param grid.y.color Character string, pointing to color of \eqn{y}-axis grid lines,
#'   \strong{default}: \code{grid.y.color = "rgb(255, 255, 255)"} (white).
#' @param bg.z.color Character string, setting the background color of the \eqn{z}-axis wall (see also \code{bg.x.color}),
#'   \strong{default}: \code{bg.z.color = "rgb(220, 220,220)"} (light gray).
#' @param grid.z.color Character string, pointing to color of \eqn{z}-axis grid lines,
#'   \strong{default}: \code{grid.z.color = "rgb(255, 255, 255)"} (white).
#' @param output.matrix.df Logical, if \code{output.matrix.df = TRUE} a wide data frame format,
#'   with all spectral/integral intensities and within the (time, Temperature,...etc.) series,
#'  represented by individual columns/variables, is returned.
#'  \strong{Default}: \code{output.matrix.df = FALSE}.
#'
#'
#' @return Depending on \code{output.matrix.df}, function returns either interactive plot object
#'   (\code{output.matrix.df = FALSE}) or if \code{otput.matrix.df = TRUE}, it results in the list
#'   consisting of
#'   \describe{
#'   \item{plot}{Interactive object plot (see below).}
#'   \item{df}{Associated data fame object in wide table format for subsequent processing by other graphing
#'   software programs. It can be also quite easily transformed into the long/tidy format by
#'   the \code{\link[tidyr]{pivot_longer}}.}
#'   }
#'   In both cases the interactive plot can be visualized either in 3D surface mode (\code{plot.type = "surface"})
#'   or in 2D contour mode with the intensity scale mapped onto the color bar (\code{plot.type = "contour"}).
#'
#'
#' @examples
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
#'                           "Triarylamine_radCat_decay_series",
#'                         dir_ASC = tempdir(),
#'                         dir_dsc_par =
#'                           system.file("extdata",
#'                                       package = "eprscope")
#'                         )
#' #
#' ## plot basics `surface` plot (with default arguments)
#' ## and the `Jet` color scheme including
#' ## Intensity contour labels
#' plot_EPR_Specs3D_interact(triarylamine.decay.series.data$df,
#'   contour.labels = TRUE,
#'   scheme.color = "Jet")
#' #
#' ## plot basic `contour` plot (with default arguments)
#' plot_EPR_Specs3D_interact(triarylamine.decay.series.data$df,
#'   plot.type = "contour")
#' #
#' \dontrun{
#' ## 3D surface plotting EPR spectra series (in the region
#' ## of <334,345> mT) from variable temperature
#' ## (VT) experiments
#' plot_EPR_Specs3D_interact(data.spectra.series,
#'   var2nd.series = "Temperature_K",
#'   xlim = c(334,345),
#'   contour.labels = T,
#'   ylab = "<i>Temperature</i> (K)")
#' }
#'
#'
#' @export
#'
#' @importFrom vctrs vec_as_names
#' @importFrom plotly plot_ly colorbar
plot_EPR_Specs3D_interact <- function(data.spectra.series,
                                      x = "B_mT",
                                      Intensity = "dIepr_over_dB",
                                      var2nd.series = "time_s",
                                      plot.type = "surface",
                                      scheme.color = "Viridis",
                                      contour.labels = FALSE,
                                      xlim = NULL,
                                      xlab = "<i>B</i> (mT)",
                                      ylab = "<i>Time</i> (s)",
                                      zlab = "d <i>I</i><sub>EPR</sub> / d <i>B</i> (p.d.u.)",
                                      axis.title.size = 12,
                                      axis.text.size = 11,
                                      bg.x.color = "rgb(220, 220,220)",
                                      grid.x.color = "rgb(255, 255, 255)",
                                      bg.y.color = "rgb(220, 220,220)",
                                      grid.y.color = "rgb(255, 255, 255)",
                                      bg.z.color = "rgb(220, 220,220)",
                                      grid.z.color = "rgb(255, 255, 255)",
                                      output.matrix.df = FALSE) {
  #
  ## 'Temporary' processing variables
  # index <- NULL
  # #
  # ## ADD `index` if NOT PRESENT
  # if (any(grepl("index", colnames(data.spectra.series)))) {
  #   data.spectra.series <- data.spectra.series
  # } else{
  #   data.spectra.series[["index"]] <- seq(nrow(data.spectra.series))
  #   ## reordering columns
  #   data.spectra.series <- data.spectra.series %>%
  #     dplyr::select(index,dplyr::everything())
  # }
  #
  ## g-factor conditions for x axis =>
  slct.vec.x.g <- c(
    "g_value", "g_Value", "gval", "gVal",
    "g_factor", "g_Factor", "gfac", "gFac","g"
  )
  ## function g-value condition for `x autorange`
  g.factor.cond <-
    function(range,x_axis){
      if (is.null(range)){
        if (any(grepl(paste(slct.vec.x.g,collapse = "|"), x_axis))){
          return("reversed")
        } else {
          return(TRUE)
        }
      } else {
        if (any(grepl(paste(slct.vec.x.g,collapse = "|"), x_axis))){
          return("reversed")
        } else {
          return(FALSE)
        }
      }
    }
  #
  data.spectra.series <- data.spectra.series %>%
    dplyr::select(dplyr::all_of(c(x,var2nd.series,Intensity)))
  ## `var2nd.series` (e.g. time) as factor to properly present the spectral series
  data.spectra.series[[var2nd.series]] <-
    as.factor(data.spectra.series[[var2nd.series]])
  #
  ## Length of the `var2nd.series`
  var2nd_select_df <- data.spectra.series %>%
    dplyr::group_by(.data[[var2nd.series]]) %>%
    dplyr::group_keys()
  #
  var2nd_select_len <- length(var2nd_select_df[[var2nd.series]])
  #
  ## Scaling var2nd.series due to HIGH LOAD of "CONTOUR RENDERING"
  ## Also the "SURFACE RENDERING" will be affected as well
  ##    ||
  ##    \/
  ## var2nd.series length => how many points + conditions + filtering
  if (var2nd_select_len >= 80 & var2nd_select_len < 160) {
    #
    var2nd_select_df <-
      var2nd_select_df[seq(1, var2nd_select_len, by = 2), ]
    #
    message("There are more than 80 EPR spectra in the series.\n
            In order to to speed-up the graph rendering, the number\n
            of spectra was reduced to 1/2 of the original one\n
            to create the plot.")
    #
  } else if (var2nd_select_len >= 160) {
    #
    var2nd_select_df <-
      var2nd_select_df[seq(1, var2nd_select_len, by = 4), ]
    #
    message("There are more than 160 EPR spectra in the series.\n
            In order to to speed-up the graph rendering, the number\n
            of spectra was reduced to 1/4 of the original one\n
            to create the plot.")
  } else {
    var2nd_select_df <- var2nd_select_df
  }
  #
  ## Filtering, accordingly (only those `var2nd.series` values defined
  ## above in `data.spectra.series`)
  data.spectra.series <- data.spectra.series %>%
    dplyr::filter(.data[[var2nd.series]] %in% var2nd_select_df[[var2nd.series]])
  #
  ## select NEW!!!UPDATED!!! `var2nd.series` !!! data frame
  ## values for `yaxis` within 3D plot
  # var2nd_select_df <- data.spectra.series %>%
  #   dplyr::group_by(.data[[var2nd.series]]) %>%
  #   dplyr::group_keys()
  #
  ## convert data from 'long' to 'wide' table format & finally to matrix
  ## it will be converted  step-by-step because `pivot_wider` doesn't work
  ## 1. group by `var2nd.series` into list
  data.spectra.list <-
   lapply(
     var2nd_select_df[[var2nd.series]],
     function(i) {
       subset(
         data.spectra.series,
         data.spectra.series[[var2nd.series]] == i
       )
     }
   )
  ## 2. select only Intensities columns
  intensity.list <- lapply(
  seq(data.spectra.list),
  function(j) {
    data.spectra.list[[j]][[Intensity]]
  }
)
  ## 3. join all columns into matrix
  Intensity_matrix <-
  as.matrix(dplyr::bind_cols(intensity.list,
    .name_repair = ~ vec_as_names(..., repair = "unique", quiet = TRUE)
  ))
## transpose matrix in order to present 3D spectra properly
Intensity_matrix <- t(Intensity_matrix)
  #
  ## `data.spectra.list` & `intensity.list` are not required anymore
  rm(data.spectra.list,intensity.list)
  #
  ## select x data frame column for `xaxis` within 3D plot
  X_select_df <- data.spectra.series %>%
    dplyr::filter(.data[[var2nd.series]] == .data[[var2nd.series]][1])
  #
  ## own 3D plot (different types "surface","contour","")
  if (plot.type == "surface" || plot.type == "Surface") {
    if (isTRUE(contour.labels)) {
      base_plot <- plot_ly(
        x = ~ X_select_df[[x]],
        y = ~ var2nd_select_df[[var2nd.series]],
        z = ~ Intensity_matrix,
        type = plot.type,
        colorscale = scheme.color,
        contours = list(
          z = list(
            show = TRUE,
            usecolormap = TRUE,
            highlightcolor = "#ff0000",
            project = list(z = TRUE)
          )
        )
      )
    } else {
      base_plot <- plot_ly(
        x = ~ X_select_df[[x]],
        y = ~ var2nd_select_df[[var2nd.series]],
        z = ~ Intensity_matrix,
        type = plot.type,
        colorscale = scheme.color
      )
    }
    final_plot <- base_plot %>%
      layout(
        scene = list(
          xaxis = list(
            title = list(
              text = xlab,
              font = list(size = axis.title.size)
            ),
            tickfont = list(size = axis.text.size),
            gridcolor = grid.x.color,
            showbackground = TRUE,
            backgroundcolor = bg.x.color,
            ## automatic set up of `xlim` within the ploty 3D surface
            range = xlim,
            autorange = g.factor.cond(range = xlim,x_axis = x)
          ),
          yaxis = list(
            title = list(
              text = ylab,
              font = list(size = axis.title.size)
            ),
            tickfont = list(size = axis.text.size),
            gridcolor = grid.y.color,
            showbackground = TRUE,
            backgroundcolor = bg.y.color
          ),
          zaxis = list(
            title = list(
              text = zlab,
              font = list(size = axis.title.size)
            ),
            tickfont = list(size = axis.text.size),
            gridcolor = grid.z.color,
            showbackground = TRUE,
            backgroundcolor = bg.z.color,
            tickformat = ".1e"
          )
        )
      ) %>%
      colorbar(
        title = list(
          text = zlab,
          font = list(size = axis.title.size)
        ),
        tickfont = list(size = axis.text.size),
        tickformat = ".1e"
      )
  }
  #
  if (plot.type == "contour" || plot.type == "Contour") {
    base_plot <- plot_ly(
      x = ~ X_select_df[[x]],
      y = ~ var2nd_select_df[[var2nd.series]],
      z = ~ Intensity_matrix,
      type = plot.type,
      colorscale = scheme.color,
      contours = list(
        coloring = "heatmap", # for continuous color presentation
        showlabels = contour.labels
      )
    )
    final_plot <- base_plot %>%
      layout(
        xaxis = list(
          title = list(
            text = xlab,
            font = list(size = axis.title.size)
          ),
          tickfont = list(size = axis.text.size),
          range = xlim,
          autorange = g.factor.cond(range = xlim,x_axis = x)
        ),
        yaxis = list(
          title = list(
            text = ylab,
            font = list(size = axis.title.size)
          ),
          tickfont = list(size = axis.text.size)
        )
      ) %>%
      colorbar(
        title = list(
          text = zlab,
          font = list(size = axis.title.size)
        ),
        tickfont = list(size = axis.text.size),
        tickformat = ".1e"
      )
  }
  #
  ## matrix table output
  if (isTRUE(output.matrix.df)) {
  ## re-transpose
  Intensity_matrix <- t(Intensity_matrix)
  ## matrix -> data frame
  matrix_to_df_table <- as.data.frame(Intensity_matrix)
  ## column names
  colnames(matrix_to_df_table) <- as.character(var2nd_select_df[[var2nd.series]])
  ## return both plot and table in list
  final_plotPlusTable <- list(plot = final_plot, df = matrix_to_df_table)
} else {
  final_plotPlusTable <- final_plot ## no table/df included
}
  #
  return(final_plotPlusTable)
  #
}
