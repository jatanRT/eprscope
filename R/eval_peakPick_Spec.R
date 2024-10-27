#'
#' Title
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   A short description...
#'
#'
#' @inheritParams plot_EPR_Specs
#' @param data.spectr
#' @param x Character string, pointing to \eqn{x}-axis/column quantity header in the original \code{data.spectr}
#'   like magnetic flux density \eqn{B}, \eqn{g}-Value or \eqn{RF} (radio frequency),
#'   \strong{default}: \code{x = "B_mT"}.
#' @param Intensity Character string, pointing to \code{intensity column} name in the original \code{data.spectr}
#'   if other than \code{dIepr_over_dB} name/label is used (e.g. for simulated or integrated spectra),
#'   \strong{default}: \code{Intesity = "dIepr_over_dB"}.
#' @param only.peak.pn Character string, setting up the selection of positive (\code{Intenstity} > 0)
#'   and/or negative (\code{Intensity} < 0) peaks (in the case of \code{lineSpecs.form = "derivative"})
#'   or only positive ones (in the case of \code{lineSpecs.form = "integrated"} or \code{lineSpeccs.form = "absorption"}).
#'   \strong{Default}: \code{only.peak.pn = NULL}, corresponding to automatic selection of positive/negative
#'   peaks depending on \code{lineSpecs.form}. In addition to \code{only.peak.pn = "positive"}
#'   and \code{only.peak.pn = "negative"} strings, the short code like \code{only.peak.pn = "p"} (or \code{"P"})
#'   and \code{only.peak.pn = "n"} (or \code{"N"}) can be applied as well.
#' @param min.peak.height Numeric, setting the \code{Intensity} threshold (its absolute value) in order filter
#'   out only those intensity values, taken to find to maxima and/or minima. \strong{Default}: \eqn{20\,\%}
#'   of the maximum Intensity value.
#' @param min.peak.dist
#' @param min.peak.width
#' @param max.peak.width
#' @param double.sided
#' @param line.color
#' @param peak.color
#' @param peak.text.angle
#' @param peak.text.size
#' @param peak.point.size
#' @param peak.point.shape
#' @param peak.text.overlap
#' @param ...
#'
#'
#' @return
#'
#'
#' @examples
#'
#'
#' @export
#'
#'
eval_peakPick_Spec <- function(data.spectr,
                               x = "B_mT",
                               x.unit = "mT", ## or "G" or "T" or "MHz" or "unitless" (g)
                               xlim = NULL,
                               Intensity = "dIepr_over_dB",
                               Ilim = NULL,
                               lineSpecs.form = "derivative",
                               only.peak.pn = NULL, ## "positive"/"p" or "negative"/"n" as well
                               min.peak.height = NULL, ## 20% of the actual max. Intensity
                               min.peak.dist = NULL, ## only positive integers (for noisy spectra), default, see def. below
                               min.peak.width = 1, ## only positive integers
                               max.peak.width = Inf, ## only positive integers
                               double.sided = TRUE, ## `TRUE` for derivative, `FALSE` for integrated
                               line.color = "darkviolet",
                               peak.color = "steelblue",
                               peak.text.angle = 90,
                               peak.text.size = 3,
                               peak.point.size = 2, ## see https://ggplot2-book.org/scales-other
                               peak.point.shape = 16, ## see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
                               peak.text.overlap = FALSE, ## see https://ggplot2.tidyverse.org/reference/geom_text.html
                               ...){ ## additional arguments specified, see `plot_EPR_Specs()`
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## set up the decimal places/digits option
  options(digits = 8)
  #
  ## Define limits for `y` (similarly like in case of `x` above)
  ## 20% of the `max` below the `min` and 10% above the `max`
  data.y.region <-
    c(min(data.spectr[[Intensity]]),max(data.spectr[[Intensity]]))
  data.y.region.2 <-
    c(data.y.region[1]  - (data.y.region[2] * 0.2),data.y.region[2] * 1.2)
  Ilim <- Ilim %>% `if`(is.null(Ilim),data.y.region.2, .)
  #
  #
  ## ------------------- Calculations: --------------------
  #
  ## definitions for `double.sided` depending on `lineSpecs.form`
  ## expecting mistakes :-)
  if (grepl("deriv|Deriv",lineSpecs.form)){
    if (isFALSE(double.sided)){
      double.sided <- double.sided %>% `if`(isFALSE(double.sided),TRUE,.)
      message(" For the derivative line form both positive and negative values\n
            are taken. Therefore, the `double.sided` argument \n
            was automamtically set to TRUE ")
    } else {
      double.sided <- double.sided
    }
  }
  if (grepl("integ|Integ|absorpt|Absorpt",lineSpecs.form)){
    if (isTRUE(double.sided)){
      stop(" For the integrated/absorption line form only positive values\n
           are taken. To find the peaks => put `double.sided = FALSE` !! ")
    } else {
      double.sided <- double.sided
    }
  }
  #
  ## definition for `min.peak.height`/threshold
  min.peak.height <-
    min.peak.height %>%
    `if`(is.null(min.peak.height),
         0.2 * max(data.spectr[[Intensity]]), .)
  #
  ## definition for `min.peak.dist`
  min.peak.dist <-
    min.peak.dist %>%
    `if` (is.null(min.peak.dist),
          round(0.5 / (data.spectr[[x]][4] - data.spectr[[x]][3])), .)
  #
  peaks.list <-
    gsignal::findpeaks(
      data = data.spectr[[Intensity]],
      MinPeakHeight = min.peak.height,
      MinPeakDistance = min.peak.dist,
      MinPeakWidth = min.peak.width,
      MaxPeakWidth = max.peak.width,
      DoubleSided = double.sided
    )
  #
  ## Intetensity values corresponding to peaks =>
  peaks.values.vec <- peaks.list$pks
  #
  ## x-values/indices corresponding to peaks
  peaks.x.vec <- data.spectr[[x]][peaks.list$loc]
  #
  ## creating a data frame from the previously picked peaks
  ## depending on positive/negative conditions
  peaks.df.base <-
    data.frame(xvar = round(peaks.x.vec,digits = 4)) %>%
    dplyr::mutate(!!rlang::quo_name(Intensity) := peaks.values.vec) %>%
    dplyr::rename(!!rlang::quo_name(x) := xvar)
  #
  if (grepl("deriv|Deriv",lineSpecs.form)){
    if (is.null(only.peak.pn)){
      peaks.df <- peaks.df.base
    } else {
      if (grepl("p|P|positive|Positive",only.peak.pn)){
        peaks.df <- peaks.df.base %>%
          dplyr::filter(.data[[Intensity]] > 0)
      } else if (grepl("n|N|negative|Negative",only.peak.pn)) {
        peaks.df <- peaks.df.base %>%
          dplyr::filter(.data[[Intensity]] < 0)
      }
    }
  }
  if (grepl("integ|Integ|absorpt|Absorpt",lineSpecs.form)) {
    if (is.null(only.peak.pn)) {
      peaks.df <- peaks.df.base
    } else {
      if (grepl("p|P|positive|Positive",only.peak.pn)){
        peaks.df <- peaks.df.base %>%
          dplyr::filter(.data[[Intensity]] > 0)
      } else if (grepl("n|N|negative|Negative",only.peak.pn)) {
        stop(' For the integrated/absorption line form only positive values\n
             are taken. Please, put `only.peak.pn = NULL`\n
             or `only.peak.pn = "p"` !! ')
      }
    }
  }
  #
  ## plotting =>
  plot.spec.peaks <-
    plot_EPR_Specs(
      data.spectra = data.spectr,
      x = x,
      x.unit = x.unit,
      line.colors = line.color,
      Intensity = Intensity,
      lineSpecs.form = lineSpecs.form,
      xlim = xlim,
      Ilim = Ilim,
      ...
    ) +
    geom_point(
      data = peaks.df,
      aes(x = .data[[x]], y = .data[[Intensity]]),
      size = peak.point.size,
      color = peak.color,
      shape = peak.point.shape
    ) +
    geom_text(
      data = peaks.df,
      aes(
        x = .data[[x]],
        y = .data[[Intensity]],
        label = .data[[x]]
      ),
      size = peak.text.size,
      angle = peak.text.angle,
      nudge_y = max(data.spectr[[Intensity]]) * 0.075,
      nudge_x = median(data.spectr[[x]] * 2.5e-4),
      check_overlap = peak.text.overlap,
      color = peak.color
    )
  #
  ## Result, list
  result <- list(
    df = peaks.df,
    plot = plot.spec.peaks
  )
  return(result)
}
