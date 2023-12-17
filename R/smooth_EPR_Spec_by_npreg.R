#'
#' Smoothing and Fitting of an EPR Spectrum by Splines
#'
#'
#' @family Simulations and Optimization
#'
#'
#' @param data.spectr Data fame object ...TBC...
#' @param B Character string ...TBC...
#' @param B.unit Character string ...TBC...
#' @param lineSpecs.form Character string ...TBC...
#' @param Intensity Character string ...TBC...
#' @param method Character string ...TBC...
#' @param output.vecs Logical, description ...TBC...
#' @param ... additional arguments passed to the function (see also \code{\link[npreg]{ss}}).
#'
#'
#' @return List with the following values: ...TBC...
#'
#'
#' @examples
#' \dontrun{
#' TODO
#' TODO
#' }
#'
#'
#' @export
#'
#'
smooth_EPR_Spec_by_npreg <- function(data.spectr,
                                     B = "B_mT",
                                     B.unit = "mT",
                                     lineSpecs.form = "derivative",
                                     Intensity = "dIepr_over_dB",
                                     method = "BIC",
                                     output.vecs = FALSE,
                                     ...){
  #
  ## 'Temporary' processing variables
  y <- NULL
  smoothed <- NULL
  #
  ## smooth EPR spectrum data
  smooth.epr.spec.list <-
    npreg::ss(x = data.spectr[[B]],
              y = data.spectr[[Intensity]],
              method = method,
              ...)
  #
  ## new column with smoothed Intensity
  data.spectr[["smoothed"]] <- smooth.epr.spec.list$y
  #
  ## derivative/integrated spectrum condition
  deriv.form.cond <- ifelse(lineSpecs.form == "derivative",TRUE,FALSE)
  #
  ## plot both EPR spectra
  plot.expr.smoothed <- ggplot(data.spectr) +
    geom_point(
      aes(
        x = .data[[B]],
        y = .data[[Intensity]],
        color = "Experimental\nData"
      ),
      size = 2.6
    ) +
    geom_line(
      aes(
        x = .data[[B]],
        y = .data$smoothed,
        color = "\nSmoothed"
      ),
      linewidth = 1.1
    ) +
    scale_color_manual(
      values = c("darkcyan", "magenta"),
      breaks = c("Experimental\nData", "\nSmoothed"),
      guide = guide_legend(override.aes = list(
        shape = c(16, NA),
        linetype = c("blank", "solid")
      ))
    ) +
    labs(color = NULL,
         x = bquote(italic(B)~~"("~.(B.unit)~")"),
         y = switch(2-deriv.form.cond,
                    bquote(d*italic(I)[EPR]~~"/"~~d*italic(B)~~~"("~p.d.u.~")"),
                    bquote(italic(Intensity) ~ ~"(" ~ p.d.u. ~ ")"))
         ) +
    plot_theme_In_ticks() +
    scale_x_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    scale_y_continuous(sec.axis = dup_axis(name = "",labels = NULL)) +
    theme(legend.text = element_text(size = 13))
  #
  ## RESULTS
  if (isFALSE(output.vecs)){
    results <- list(
      df = data.spectr,
      plot = plot.expr.smoothed
    )
  } else{
    results <- data.spectr$smoothed
  }
  #
  return(results)
  #
}
