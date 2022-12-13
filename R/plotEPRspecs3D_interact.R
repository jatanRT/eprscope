#
#' Interactive (incl. Zooming, Data Reading...etc) 3D Plot for Time Series of EPR Spectra
#'
#'
#' @description
#' tbc
#'
#'
#' @param data.time.spectra tbc
#' @param B tbc
#' @param Intensity tbc
#' @param time tbc
#' @param plot.type tbc
#' @param scheme.color tbc
#' @param contour.labels tbc
#' @param Blim tbc
#' @param x.label tbc
#' @param y.label tbc
#' @param z.label tbc
#' @param x.bg.color tbc
#' @param x.grid.color tbc
#' @param y.bg.color tbc
#' @param y.grid.color tbc
#' @param z.bg.color tbc
#' @param z.grid.color tbc
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
plotEPRspecs3D_interact <- function(data.time.spectra,
                                    B = "B_mT",
                                    Intensity = "dIepr_over_dB",
                                    time = "time_s",
                                    plot.type = "surface",
                                    scheme.color = "Viridis",
                                    contour.labels = FALSE,
                                    Blim,
                                    x.label = "<i>B</i> (mT)",
                                    y.label = "<i>Time</i> (s)",
                                    z.label = "d <i>I</i><sub>EPR</sub> / d <i>B</i> (p.d.u.)",
                                    x.bg.color = "rgb(220, 220,220)",
                                    x.grid.color = "rgb(255, 255, 255)",
                                    y.bg.color = "rgb(220, 220,220)",
                                    y.grid.color = "rgb(255, 255, 255)",
                                    z.bg.color = "rgb(220, 220,220)",
                                    z.grid.color = "rgb(255, 255, 255)",
                                    output.matrix.table = FALSE){
  #
  ## `time` as factor
  data.time.spectra[[time]] <- as.factor(data.time.spectra[[time]])
  #
  ## convert data from 'long' to 'wide' table format & finally to matrix
  Intensity_matrix <- data.time.spectra %>%
    dplyr::select(.data[[time]],.data[[B]],.data[[Intensity]]) %>%
    tidyr::pivot_wider(names_from = .data[[time]],values_from = .data[[Intensity]]) %>%
    dplyr::select(-.data[[B]]) %>%
    as.matrix()
  ## transpose matrix in order to present 3D spectra properly
  Intensity_matrix <- t(Intensity_matrix)
  #
  ## select time data frame values for `yaxis` within 3D plot
  time_select_df <- data.time.spectra %>%
    dplyr::group_by(.data[[time]]) %>%
    dplyr::group_keys()
  #
  ## select B data frame column for `xaxis` within 3D plot
  B_select_df <- data.time.spectra %>%
    dplyr::filter(.data[[time]] == .data[[time]][1])
  #
  ## own 3D plot (different types "surface","contour","")
  if (plot.type == "surface"){
    final_plot <- plotly::plot_ly(x = ~B_select_df[[B]],
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
                        ) %>%
                          plotly::layout(
                            scene = list(
                              xaxis = list(title = list(text = x.label),
                                           gridcolor = x.grid.color,
                                           showbackground = TRUE,
                                           backgroundcolor = x.bg.color,
                                           range = Blim),
                              yaxis = list(title = list(text = y.label),
                                           gridcolor = y.grid.color,
                                           showbackground = TRUE,
                                           backgroundcolor = y.bg.color),
                              zaxis = list(title = list(text = z.label),
                                           gridcolor = z.grid.color,
                                           showbackground = TRUE,
                                           backgroundcolor = z.bg.color)
                          )) %>%
                          plotly::colorbar(title = z.label)
  }
  #
  if (plot.type == "contour"){
    final_plot <- plotly::plot_ly(x = ~B_select_df[[B]],
                          y = ~time_select_df[[time]],
                          z = ~Intensity_matrix,
                          type = plot.type,
                          colorscale = scheme.color,
                          contours = list(
                            coloring = "heatmap", # for continuous color presentation
                            showlabels = contour.labels
                          )) %>%
                          plotly::layout(
                            xaxis = list(title = list(text = x.label),
                                         range = Blim),
                            yaxis = list(title = list(text = y.label))
                          ) %>%
                          plotly::colorbar(title = z.label)
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
