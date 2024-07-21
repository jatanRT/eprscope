#'
#' Plotting Integrated Forms of EPR Spectra Acquired by \code{\link{eval_integ_EPR_Spec}}
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#' tbc
#'
#'
#'
#' @param data.spectra.integ tbc
#' @param B tbc
#' @param B.unit tbc
#' @param Blim tbc
#' @param ylim tbc...doesn't apply for separated integrals...it works only for overlay
#' @param slct.integs tbc
#' @param line.width tbc
#' @param axis.title.size tbc
#' @param axis.text.size tbc
#' @param legend.title.size tbc
#' @param legend.text.size tbc
#' @param separate.integs tbc
#' @param separate.integ.scales Character/String corresponding to how the axis scales should or shouldn't be fixed,
#'   unless the \code{separate.integs = FALSE}. Inherited from \code{\link[ggplot2]{facet_wrap}}.
#'   Following expressions are available => \code{"fixed"}, \code{"free"} or in one dimension \code{"free_x"}
#'   or \code{"free_y"}. \strong{Default}: \code{separate.integ.scales = NULL}
#'   in case of \code{separate.integs = FALSE}.
#' @param output.df Logical, description tbc...data frame/table in `tidy` (long) format...for additional
#'   plotting by `plotly` (see \code{\link{plot_EPR_Specs2D_interact}})
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
#' @importFrom ggplot2 facet_wrap
plot_EPR_Specs_integ <- function(data.spectra.integ,
                                 B = "B_G",
                                 B.unit = "G",
                                 Blim = NULL,
                                 ylim = NULL,
                                 slct.integs = c(
                                   "single_Integ",
                                   "baseline_Integ_fit",
                                   "single_Integ_correct"
                                 ),
                                 line.width = 0.75,
                                 axis.title.size = 15,
                                 axis.text.size = 14,
                                 legend.title.size = 13,
                                 legend.text.size = 11,
                                 separate.integs = FALSE,
                                 separate.integ.scales = NULL,
                                 output.df = FALSE) {
  #
## 'Temporary' processing variables
. <- NULL
Integrals <- NULL
#
## B and y range to present integrated spectrum and baseline correction
## 1st entire y region
data.B.region <- data.B.region <- c(min(data.spectra.integ[[B]]), max(data.spectra.integ[[B]]))
#
## condition to present intensity region (which has the max integral ?)
max.integs.vec <- sapply(slct.integs, function(m) max(data.spectra.integ[[m]]))
max.integs.df <- data.frame(
  "slct_Integrals" = slct.integs,
  "max_Integral" = max.integs.vec
)
max.integ <- max.integs.df %>%
  dplyr::filter(.data[["max_Integral"]] == max(.data[["max_Integral"]])) %>%
  dplyr::pull(.data[["slct_Integrals"]])
## therefore
data.y.region <- c(
  min(data.spectra.integ[[max.integ]]) - max(data.spectra.integ[[max.integ]]) / 10,
  max(data.spectra.integ[[max.integ]]) + max(data.spectra.integ[[max.integ]]) / 10
)
## B & y range condition
Blim <- Blim %>% `if`(is.null(Blim), data.B.region, .)
#
ylim <- ylim %>% `if`(is.null(ylim), data.y.region, .)
#
## converting the wide table into longer one
data.spectra.integ.new <- data.spectra.integ %>%
  ## select only `B` and `integrals`
  dplyr::select(c(.data[[B]], dplyr::matches("integ"))) %>%
  ## long table format
  tidyr::pivot_longer(!dplyr::all_of(c(B)),
    names_to = "Integrals",
    values_to = "Intensity"
  ) %>%
  ## variables to factors in order to arrange in the right order
  dplyr::mutate(Integrals = factor(Integrals,
    levels = grep("integ",
      colnames(data.spectra.integ),
      ignore.case = TRUE,
      value = TRUE
    )
  )) %>%
  ## arrange `Integrals` (in order to group it) according to factors
  dplyr::arrange(Integrals)

#
## plot themes
plot_themes <- plot_theme_In_ticks(
  axis.title.size = axis.title.size,
  axis.text.size = axis.text.size
) +
  theme(
    legend.title = element_text(size = legend.title.size),
    legend.text = element_text(size = legend.text.size)
  )
#
## Plot only specific integrals or corrections
if (isFALSE(separate.integs)) {
  plot.integs <- data.spectra.integ.new %>%
    dplyr::filter(Integrals %in% slct.integs) %>%
    ggplot(aes(x = .data[[B]], y = .data$Intensity, color = .data$Integrals)) +
    geom_line(linewidth = line.width) +
    coord_cartesian(xlim = Blim, ylim = ylim) +
    labs(
      x = bquote(italic(B) ~ "(" ~ .(B.unit) ~ ")"),
      y = bquote(italic(Intensity) ~ "(" ~ p.d.u. ~ ")"),
      color = bquote(atop(italic(Integrated), EPR ~ ~ italic(Spectra)))
    ) +
    plot_themes +
    scale_x_continuous(sec.axis = dup_axis(name = "", labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = "", labels = NULL))
} else {
  if (is.null(separate.integ.scales)) {
    stop(" Please specify the `separate.integ.scales` for the facets ! ")
  } else {
    plot.integs <- data.spectra.integ.new %>%
      dplyr::filter(Integrals %in% slct.integs) %>%
      ggplot(aes(x = .data[[B]], y = .data$Intensity, color = .data$Integrals)) +
      geom_line(linewidth = line.width, show.legend = FALSE) +
      coord_cartesian(xlim = Blim) +
      facet_wrap(~ .data$Integrals,
        scales = separate.integ.scales,
        # labeller = label_parsed,
        nrow = 1
      ) +
      labs(
        x = bquote(italic(B) ~ "(" ~ .(B.unit) ~ ")"),
        y = bquote(italic(Intensity) ~ "(" ~ p.d.u. ~ ")")
      ) +
      plot_theme_In_ticks(
        axis.title.size = axis.title.size,
        axis.text.size = axis.text.size
      ) +
      scale_y_continuous(sec.axis = dup_axis(name = "", labels = NULL)) +
      theme(
        strip.background = element_rect(fill = "#363636"),
        strip.text = element_text(size = 13, color = "white", face = "bold"),
        panel.spacing = ggplot2::unit(10, "pt")
      )
  }
}
#
## the old data frame is not required anymore
rm(data.spectra.integ, max.integs.df)
#
## if the entire table/table should be included
if (isFALSE(output.df)) {
  results.integ <- plot.integs
} else {
  results.integ <- list(plot = plot.integs, df = data.spectra.integ.new)
}
#
return(results.integ)
#
}
