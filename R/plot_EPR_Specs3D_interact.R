#
#' Interactive (incl. Zooming, Data Reading...etc) 3D Plot for the Series of EPR Spectra
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#' tbc
#'
#'
#' @param data.spectra.series tbc
#' @param x tbc
#' @param Intensity tbc
#' @param var2nd.series String/Character referred to name of the second independent variable/quantity
#'   column in the original \code{data.spectra} (e.g. like `time`,`Temperature`, `Electrochemical Potential`,
#'   `Microwave Power`...etc) altered upon individual experiments as a second variable
#'   (\code{var2nd.series}) and related to spectra/data. Data must be available in \strong{long table}
#'   (or \strong{tidy}) \strong{format} (see also \code{\link{readEPR_Exp_Specs_multif}}).
#'   \strong{Default}: \code{var2nd.series = NULL}. Otherwise \strong{usually} \code{var2nd.series = "time_s"}.
#' @param plot.type Character/String, inherited from \code{\link[plotly]{plot_ly}}, specifying the trace. Only two
#'   character/strings are available: \code{plot.type = "surface"} (\strong{default}, for 3D surface plots)
#'   or \code{plot.type = "contour"} (for 2D contour plots).
#' @param scheme.color Character/String corresponding to \code{colorscale}.
#'   See also \href{https://plotly.com/r/reference/surface/#surface}{R>Figure Reference>surface Traces}
#'   or \href{https://plotly.com/r/reference/contour/#contour}{R>Figure Reference>contour Traces} and parameter `colorscales`.
#'   The colorscale must be an array containing arrays mapping a normalized value to an rgb, rgba, hex, hsl, hsv,
#'   or named color string. At minimum, a mapping for the lowest (0) and highest (1) values are required. For example,
#'   `[[0, 'rgb(0,0,255)']`, `[1, 'rgb(255,0,0)']]`. To control the bounds of the colorscale in color space,
#'   use `cmin` and `cmax`. Alternatively, `colorscale` may be a palette name string of the following list:
#'   \code{"Blackbody"},\code{"Bluered"},\code{"Blues"},\code{"Cividis"},\code{"Earth"},\code{"Electric"},\code{"Greens"},
#'   \code{"Greys"},\code{"Hot"},\code{"Jet"},\code{"Picnic"},\code{"Portland"},\code{"Rainbow"},\code{"RdBu"},\code{"Reds"},
#'   \code{"Viridis"},\code{"YlGnBu"},\code{"YlOrRd"}. \strong{Default}: \code{scheme.color = "Viridis"}
#' @param contour.labels tbc
#' @param xlim tbc
#' @param xlab tbc
#' @param ylab tbc
#' @param zlab tbc
#' @param axis.title.size tbc
#' @param axis.text.size tbc
#' @param bg.x.color tbc
#' @param grid.x.color tbc
#' @param bg.y.color tbc
#' @param grid.y.color tbc
#' @param bg.z.color tbc
#' @param grid.z.color tbc
#' @param output.matrix.df tbc
#'
#'
#' @return tbc
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#'
#' @export
#'
#'
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
  ## `var2nd.series` (e.g. time) as factor to properly present
  data.spectra.series[[var2nd.series]] <- as.factor(data.spectra.series[[var2nd.series]])
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
    var2nd_select_df <- var2nd_select_df[seq(1, var2nd_select_len, by = 2), ]
  }
  if (var2nd_select_len >= 160) {
    var2nd_select_df <- var2nd_select_df[seq(1, var2nd_select_len, by = 4), ]
  } else{
    var2nd_select_df <- var2nd_select_df
  }
  #
  ## Filtering, accordingly (only those `var2nd.series` values defined above in `data.spectra.series`)
  data.spectra.series <- data.spectra.series %>%
    dplyr::filter(.data[[var2nd.series]] %in% var2nd_select_df[[var2nd.series]])
  #
  ## select NEW!!!UPDATED!!! `var2nd.series` !!! data frame values for `yaxis` within 3D plot
  # var2nd_select_df <- data.spectra.series %>%
  #   dplyr::group_by(.data[[var2nd.series]]) %>%
  #   dplyr::group_keys()
  #
  ## convert data from 'long' to 'wide' table format & finally to matrix
  Intensity_matrix <- data.spectra.series %>%
    dplyr::select(dplyr::all_of(c(var2nd.series,x,Intensity))) %>%
    tidyr::pivot_wider(names_from = dplyr::all_of(c(var2nd.series)),
                       values_from = dplyr::all_of(c(Intensity))) %>%
    dplyr::select(!dplyr::all_of(c(x))) %>%
    as.matrix()
  ## transpose matrix in order to present 3D spectra properly
  Intensity_matrix <- t(Intensity_matrix)
  #
  ## select x data frame column for `xaxis` within 3D plot
  X_select_df <- data.spectra.series %>%
    dplyr::filter(.data[[var2nd.series]] == .data[[var2nd.series]][1])
  #
  ## own 3D plot (different types "surface","contour","")
  if (plot.type == "surface") {
    if (isTRUE(contour.labels)) {
      base_plot <- plotly::plot_ly(
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
      base_plot <- plotly::plot_ly(
        x = ~ X_select_df[[x]],
        y = ~ var2nd_select_df[[var2nd.series]],
        z = ~ Intensity_matrix,
        type = plot.type,
        colorscale = scheme.color
      )
    }
    if (is.null(xlim)) {
      final_plot <- base_plot %>%
        plotly::layout(
          scene = list(
            xaxis = list(
              title = list(
                text = xlab,
                font = list(size = axis.title.size)
              ),
              tickfont = list(size = axis.text.size),
              gridcolor = grid.x.color,
              showbackground = TRUE,
              backgroundcolor = bg.x.color
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
        plotly::colorbar(
          title = list(
            text = zlab,
            font = list(size = axis.title.size)
          ),
          tickfont = list(size = axis.text.size),
          tickformat = ".1e"
        )
    } else {
      final_plot <- base_plot %>%
        plotly::layout(
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
              range = xlim
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
        plotly::colorbar(
          title = list(
            text = zlab,
            font = list(size = axis.title.size)
          ),
          tickfont = list(size = axis.text.size),
          tickformat = ".1e"
        )
    }
  }
  #
  if (plot.type == "contour") {
    base_plot <- plotly::plot_ly(
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
    if (is.null(xlim)) {
      final_plot <- base_plot %>%
        plotly::layout(
          xaxis = list(
            title = list(
              text = xlab,
              font = list(size = axis.title.size)
            ),
            tickfont = list(size = axis.text.size)
          ),
          yaxis = list(
            title = list(
              text = ylab,
              font = list(size = axis.title.size)
            ),
            tickfont = list(size = axis.text.size)
          )
        ) %>%
        plotly::colorbar(
          title = list(
            text = zlab,
            font = list(size = axis.title.size)
          ),
          tickfont = list(size = axis.text.size),
          tickformat = ".1e"
        )
    } else {
      final_plot <- base_plot %>%
        plotly::layout(
          xaxis = list(
            title = list(
              text = xlab,
              font = list(size = axis.title.size)
            ),
            tickfont = list(size = axis.text.size),
            range = xlim
          ),
          yaxis = list(
            title = list(
              text = ylab,
              font = list(size = axis.title.size)
            ),
            tickfont = list(size = axis.text.size)
          )
        ) %>%
        plotly::colorbar(
          title = list(
            text = zlab,
            font = list(size = axis.title.size)
          ),
          tickfont = list(size = axis.text.size),
          tickformat = ".1e"
        )
    }
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
