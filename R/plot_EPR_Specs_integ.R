#'
#' Plotting Integrated Forms of EPR Spectra Acquired by \code{\link{eval_integ_EPR_Spec}}
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#'   tbc...description
#'
#'
#' @inheritParams plot_EPR_Specs
#' @param data.spectra.integ Data frame object, inherited output from the \code{\link{eval_integ_EPR_Spec}},
#'   if \code{output.vecs = FALSE} (argument from the latter) corresponding to data frame including the original
#'   EPR spectral data and the integral(s).
#' @param B Character string pointing to magnetic flux density \code{column} of the original (\code{data.spectra.integ})
#'   data frame, either in "millitesla" or in "gauss", that is \code{B = "B_G"} (\strong{default})
#'   or \code{B = "B_mT"} or \code{B = "Bsim_G"} to include simulated EPR spectra as well.
#' @param B.unit Character string denoting the magnetic flux density unit e.g. \code{B.unit = "G"}
#'   (gauss, \strong{default}) or \code{B.unit = "mT"} (millitesla).
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to lower and upper limit
#'   of the selected \eqn{B}-region, such as \code{Blim = c(3495.4,3595.4)}. \strong{Default}: \code{Blim = NULL}
#'   (corresponding to the entire \eqn{B}-range of the integrated EPR spectrum).
#' @param ylim Numeric vector corresponding to lower and upper limit of the \eqn{y}-axis scale
#'   (e.g. \code{ylim = c(-1e-4,1e-3)}). This doesn't apply for separated integrals (if \code{separate.integs = TRUE})
#'   and works only in overlay mode (in one graph/panel). \strong{Default}: \code{ylim = NULL} corresponding to the entire
#'   \eqn{y}-range of presented integrals.
#' @param slct.integs Character string vector corresponding to selected integrals/columns/variables (of the original
#'   \code{data.spectra.integ} data frame) to be presented in the actual plot. \strong{Default}:
#'   \code{slct.integs = c("single_Integ","baseline_Integ_fit","single_Integ_correct")}.
#' @param separate.integs Logical, should be the integrals presented in overlay mode (in one graph/panel)
#'   or on separated panels (by \code{\link[ggplot2]{facet_wrap}}, see also the next argument) ?
#'   \strong{Default}: \code{separate.integs = FALSE} (integrals are presented in overlay mode).
#' @param separate.integ.scales Character string related to \eqn{y}-axes scales, unless the \code{separate.integs = FALSE},
#'   inherited from \code{\link[ggplot2]{facet_wrap}}. Following expressions are available => \code{"fixed"}, \code{"free"}
#'   or in one dimension \code{"free_x"} or \code{"free_y"}. \strong{Default}: \code{separate.integ.scales = NULL}
#'   in case of \code{separate.integs = FALSE}.
#' @param output.df Logical, whether a transformed \code{data.spectra.integ} data frame into
#'   \href{https://r4ds.had.co.nz/tidy-data.html}{tidy/long table format} is required for additional processing
#'   or plotting. \strong{Default}: \code{output.df = FALSE}.
#'
#'
#' @return Depending on \code{output.df} argument, function returns plot object including all selected
#'   integrated EPR spectra (\code{output.df = FALSE},\strong{default}) or list (\code{output.df = FALSE})
#'   consisting of
#'   \describe{
#'   \item{df}{Data frame object with intensities of all selected integrals and magnetic flux density \eqn{B}
#'   variables/columns in tidy/long table format.}
#'   \item{plot}{Plot object showing all integrated EPR spectra corresponding to \code{df}.}
#'   }
#'
#'
#' @examples
#' ## loading the package built-in example
#' TMPD.data.path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.asc")
#' TMPD.data <-
#'   readEPR_Exp_Specs(TMPD.data.path,
#'                     col.names = c("B_G","dIepr_over_dB"),
#'                     qValue = 3500,
#'                     norm.vec.add = c(20,0.001),
#'                     origin = "winepr")
#' ## integration of the `TMPD` EPR spectrum
#' TMPD.data.integs <-
#'   eval_integ_EPR_Spec(TMPD.data,sigmoid.integ = TRUE)
#' #
#' ## plotting integrals in overlay mode
#' plot_EPR_Specs_integ(TMPD.data.integs,
#'   slct.integs = c("single_Integ",
#'                   "sigmoid_Integ"),
#'   B = "B_mT",
#'   B.unit = "mT"
#'   )
#' #
#' ## separate integrals within the plot
#' plot_EPR_Specs_integ(TMPD.data.integs,
#'   slct.integs = c("single_Integ",
#'                   "sigmoid_Integ"),
#'   B = "B_mT",
#'   B.unit = "mT",
#'   separate.integs = TRUE,
#'   separate.integ.scales = "free_y"
#'   )
#'
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
                                 line.type = 1,
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
    geom_line(linewidth = line.width,
              linetype = line.type) +
    coord_cartesian(xlim = Blim, ylim = ylim) +
    labs(
      x = bquote(italic(B) ~ "(" ~ .(B.unit) ~ ")"),
      y = bquote(italic(Intensity) ~ "(" ~ p.d.u. ~ ")"),
      color = bquote(atop(italic(Integrated), EPR ~ ~ italic(Spectra)))
    ) +
    plot_themes
   #
} else {
  if (is.null(separate.integ.scales)) {
    stop(" Please specify the `separate.integ.scales` for the facets ! ")
  } else {
    plot.integs <- data.spectra.integ.new %>%
      dplyr::filter(Integrals %in% slct.integs) %>%
      ggplot(aes(x = .data[[B]], y = .data$Intensity, color = .data$Integrals)) +
      geom_line(linewidth = line.width,
                linetype = line.type,
                show.legend = FALSE) +
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
