#
#' Interactive (incl. Zooming, Data Reading...etc) 3D Plot for Time Series of EPR Spectra
#'
#'
#' @description
#' tbc
#'
#'
#' @param data.time.spectra tbc
#' @param x tbc
#' @param Intensity tbc
#' @param time tbc
#' @param plot.type tbc
#' @param scheme.color tbc
#' @param contour.labels tbc
#' @param xlim tbc
#' @param xlab tbc
#' @param ylab tbc
#' @param zlab tbc
#' @param bg.x.color tbc
#' @param grid.x.color tbc
#' @param bg.y.color tbc
#' @param grid.y.color tbc
#' @param bg.z.color tbc
#' @param grid.z.color tbc
#' @param output.matrix.table tbc
#'
#'
#' @return tbc
#'
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
plot_EPR_Specs3D_interact <- function(data.time.spectra,
                                    x = "B_mT",
                                    Intensity = "dIepr_over_dB",
                                    time = "time_s",
                                    plot.type = "surface",
                                    scheme.color = "Viridis",
                                    contour.labels = FALSE,
                                    xlim = NULL,
                                    xlab = "<i>B</i> (mT)",
                                    ylab = "<i>Time</i> (s)",
                                    zlab = "d <i>I</i><sub>EPR</sub> / d <i>B</i> (p.d.u.)",
                                    bg.x.color = "rgb(220, 220,220)",
                                    grid.x.color = "rgb(255, 255, 255)",
                                    bg.y.color = "rgb(220, 220,220)",
                                    grid.y.color = "rgb(255, 255, 255)",
                                    bg.z.color = "rgb(220, 220,220)",
                                    grid.z.color = "rgb(255, 255, 255)",
                                    output.matrix.table = FALSE){
  #
  ## `time` as factor
  data.time.spectra[[time]] <- as.factor(data.time.spectra[[time]])
  #
  ## convert data from 'long' to 'wide' table format & finally to matrix
  Intensity_matrix <- data.time.spectra %>%
    dplyr::select(.data[[time]],.data[[x]],.data[[Intensity]]) %>%
    tidyr::pivot_wider(names_from = .data[[time]],values_from = .data[[Intensity]]) %>%
    dplyr::select(-.data[[x]]) %>%
    as.matrix()
  ## transpose matrix in order to present 3D spectra properly
  Intensity_matrix <- t(Intensity_matrix)
  #
  ## select time data frame values for `yaxis` within 3D plot
  time_select_df <- data.time.spectra %>%
    dplyr::group_by(.data[[time]]) %>%
    dplyr::group_keys()
  #
  ## select x data frame column for `xaxis` within 3D plot
  X_select_df <- data.time.spectra %>%
    dplyr::filter(.data[[time]] == .data[[time]][1])
  #
  ## own 3D plot (different types "surface","contour","")
  if (plot.type == "surface"){
    base_plot <- plotly::plot_ly(x = ~X_select_df[[x]],
                          y = ~time_select_df[[time]],
                          z = ~Intensity_matrix,
                          type = plot.type,
                          colorscale = scheme.color,
                          contours = list(
                            z = list(
                              show = TRUE,
                              usecolormap = TRUE,
                              highlightcolor = "#ff0000",
                              project = list(z = TRUE))
                                        )
                        )
    if (is.null(xlim)){
      final_plot <- base_plot %>%
        plotly::layout(
          scene = list(
            xaxis = list(title = list(text = xlab),
                         gridcolor = grid.x.color,
                         showbackground = TRUE,
                         backgroundcolor = bg.x.color),
            yaxis = list(title = list(text = ylab),
                         gridcolor = grid.y.color,
                         showbackground = TRUE,
                         backgroundcolor = bg.y.color),
            zaxis = list(title = list(text = zlab),
                         gridcolor = grid.z.color,
                         showbackground = TRUE,
                         backgroundcolor = bg.z.color,
                         tickformat = ".1e")
          )) %>%
        plotly::colorbar(title = zlab,
                         tickformat = ".1e")
    } else{
      final_plot <- base_plot %>%
        plotly::layout(
          scene = list(
            xaxis = list(title = list(text = xlab),
                         gridcolor = grid.x.color,
                         showbackground = TRUE,
                         backgroundcolor = bg.x.color,
                         range = xlim),
            yaxis = list(title = list(text = ylab),
                         gridcolor = grid.y.color,
                         showbackground = TRUE,
                         backgroundcolor = bg.y.color),
            zaxis = list(title = list(text = zlab),
                         gridcolor = grid.z.color,
                         showbackground = TRUE,
                         backgroundcolor = bg.z.color,
                         tickformat = ".1e")
          )) %>%
        plotly::colorbar(title = zlab,
                         tickformat = ".1e")
    }
  }
  #
  if (plot.type == "contour"){
    base_plot <- plotly::plot_ly(x = ~X_select_df[[x]],
                          y = ~time_select_df[[time]],
                          z = ~Intensity_matrix,
                          type = plot.type,
                          colorscale = scheme.color,
                          contours = list(
                            coloring = "heatmap", # for continuous color presentation
                            showlabels = contour.labels
                          ))
    if (is.null(xlim)){
      final_plot <- base_plot %>%
        plotly::layout(
          xaxis = list(title = list(text = xlab)),
          yaxis = list(title = list(text = ylab))
        ) %>%
        plotly::colorbar(title = zlab,
                         tickformat = ".1e")
    } else{
      final_plot <- base_plot %>%
        plotly::layout(
          xaxis = list(title = list(text = xlab),
                       range = xlim),
          yaxis = list(title = list(text = ylab))
        ) %>%
        plotly::colorbar(title = zlab,
                         tickformat = ".1e")
    }
  }
  #
  ## matrix table output
  if (isTRUE(output.matrix.table)){
    ## re-transpose
    Intensity_matrix <- t(Intensity_matrix)
    ## matrix -> data frame
    matrix_to_df_table <- as.data.frame(Intensity_matrix)
    ## column names
    colnames(matrix_to_df_table) <- as.character(time_select_df[[time]])
    ## return both plot and table in list
    final_plotPlusTable <- list(plot = final_plot,table = matrix_to_df_table)
  } else{
    final_plotPlusTable <- final_plot ## no table/df included
  }
  #
  return(final_plotPlusTable)
  #
}
