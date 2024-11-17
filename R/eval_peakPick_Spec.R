#'
#' Peak Picking of EPR/ENDOR Spectra
#'
#'
#' @family Evaluations
#'
#'
#' @description
#'   The peak picking expands the functionality of the basic \code{\link{eval_extremeX_Spec}} to quickly
#'   find extremes within several regions of the entire EPR/ENDOR Spectrum (without a need for an
#'   interactive plotting). The function is based on the \code{\link[gsignal]{findpeaks}} to find
#'   the local intensity maxima and/or minima by fitting the experimental EPR spectral parts/points using the shortened
#'   \href{https://www.radfordmathematics.com/functions/quadratic-functions-parabola/vertex-form/vertex-form-finding-equation-parabola.html}{vertex form of a parabola}.
#'   The parabola vertices actually represent the maxima or minima. Visualization of the peak picking is provided
#'   by the \code{\link{plot_EPR_Specs}} together with the \code{geom_text} function (see \code{\link[ggplot2]{geom_label}}).
#'
#' @inheritParams plot_EPR_Specs
#' @param data.spectr Data frame object/table, containing \code{x}-values and the \code{Intensity}
#'   (see the arguments below), from which the peaks are to be found (picked).
#' @param x Character string, pointing to \eqn{x}-axis/column quantity header in the original
#'   \code{data.spectr} like magnetic flux density \eqn{B}, \eqn{g}-Value or \eqn{RF} (radio frequency),
#'   \strong{default}: \code{x = "B_mT"}.
#' @param Intensity Character string, pointing to \code{intensity column} name in the original \code{data.spectr}
#'   if other than \code{dIepr_over_dB} name/label is used (e.g. for simulated or integrated spectra),
#'   \strong{default}: \code{Intesity = "dIepr_over_dB"}.
#' @param only.peak.pn Character string, setting up the selection of positive (\code{Intenstity} > 0)
#'   and/or negative (\code{Intensity} < 0) peaks (in the case of \code{lineSpecs.form = "derivative"})
#'   or only positive ones (in the case of \code{lineSpecs.form = "integrated"}
#'   or \code{lineSpeccs.form = "absorption"}). \strong{Default}: \code{only.peak.pn = NULL},
#'   corresponding to automatic selection of positive/negative peaks depending on \code{lineSpecs.form}.
#'   In addition to \code{only.peak.pn = "positive"} and \code{only.peak.pn = "negative"} strings,
#'   the short code like \code{only.peak.pn = "p"} (or \code{"P"}) and \code{only.peak.pn = "n"} (or \code{"N"})
#'   can be applied as well.
#' @param min.peak.height Numeric, setting the \code{Intensity} threshold (its absolute value)
#'   in order to filter/select only those intensity values, which are taken to find to maxima and/or minima.
#'   \strong{Default}: \code{min.peak.height = NULL}, corresponding to \eqn{20\,\%} of the maximum Intensity value.
#' @param min.peak.dist Numeric (integer > 0), pointing to minimum distance (in points) between the expected peaks,
#'   which are constructed by parabola fits over the points. For such purpose, the shortened (vertex)
#'   parabola (the 2nd polynomial) expression like \eqn{a\,(x - h)^2 + k} is applied,
#'   where \eqn{a} and \eqn{h,k} denote the concavity and the vertex, respectively. These vertices actually
#'   correspond to peak maxima (\eqn{a < 0}) or minima (\eqn{a > 0}). Peaks separated by less than this distance
#'   are considered as a single peak. Please, also refer to documentation of the \code{\link[gsignal]{findpeaks}}
#'   function. The \strong{default} distance (\code{min.peak.dist = NULL}) actually equals
#'   to one-half divided by the distance between the adjacent points, rounded to the integer: e.g.
#'   \deqn{round(0.5\,/\,(x_2 - x_1))}
#'   where such formula corresponds to \code{round(0.5/(data.spectr[[x]][2] - data.spectr[[x]][1]))}.
#'   This is especially useful for rather noisy EPR spectra or spectra with high resolution.
#'   If according to \code{{ggplot2}} graphical representation the peak picking fails, i.e. not all peaks
#'   are properly detected, try lower values than the \strong{default} one (such as 1 or 4 or ...etc.).
#' @param min.peak.width Numeric (integer > 0), setting the minimum peak-width (points) to fit the vertex
#'   parabola expression (see also the \code{min.peak.dist} argument and the \code{\link[gsignal]{findpeaks}}
#'   documentation) to find the peaks. \strong{Default}: \code{min.peak.width = 1}.
#' @param max.peak.width Numeric (integer > 0), pointing to maximum peak-width (points) to find the peaks.
#'   \strong{Default}: \code{max.peak.width = Inf} (infinity).
#' @param double.sided Logical. Should be the peaks found for both intensity sites
#'   (\code{data.spectr[[Intensity]]} > 0 as well as \code{data.spectr[[Intensity]]} < 0)?
#'   If \code{lineSpecs.form = "derivative"} then \code{double.sided = TRUE}, \strong{default}, otherwise,
#'   for the single integrated EPR spectra, it applies \code{double.sided = FALSE}.
#' @param line.color Character string, line color to plot the EPR/ENDOR spectrum.
#'   All \href{https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html}{\code{{ggplot2}} colors}
#'   are available (see also \code{\link[ggplot2]{aes_colour_fill_alpha}}).
#'   \strong{Default}: \code{line.color = "darkviolet"}.
#' @param peak.color Character string, "point" color to visualize/emphasize the peaks.
#'   Similarly, as for the \code{line.color} argument, all \code{{ggplot2}} color definitions are available.
#'   \strong{Default}: \code{peak.color = "steelblue"}.
#' @param peak.text.angle Numeric, setting the angle (in deg) of the peak value (projection onto
#'   the \eqn{x}-axis) annotation text, presented near the local maximum or minimum, and measured relatively
#'   to the \eqn{x}-axis (see also \code{\link[ggplot2]{geom_text}}). \strong{Default}: \code{peak.text.angle = 90}.
#' @param peak.text.size Numeric, pointing to peak annotation text size
#'   (in mm, see the \code{\link[ggplot2]{aes_linetype_size_shape}}). \strong{Default}: \code{peak.text.size = 3}.
#' @param peak.point.size Numeric, size (in mm) of the peak "point" in graphical representation of the peak picking.
#'   Please consult the \code{\link[ggplot2]{aes_linetype_size_shape}}
#'   \href{https://ggplot2-book.org/scales-other}{\code{{ggplot2}} aesthetic arguments}. \strong{Default}:
#'   \code{peak.point.size = 2}.
#' @param peak.point.shape Numeric (integer between 0 and 24), controlling the "point" symbol like square,
#'   triangle, circle, asterisk...etc,
#'   refer to e.g. \href{https://ggplot2.tidyverse.org/articles/ggplot2-specs.html}{\code{{ggplot2}}
#'   aesthetic specifications}. \strong{Default}: \code{peak.point.shape = 16} (filled circle).
#' @param peak.text.overlap Logical, if \code{TRUE}, the overlapping peak text annotation to a previous one,
#'   will be not displayed, for clarity (see also \code{\link[ggplot2]{geom_label}}).
#'   If \code{peak.text.overlap = FALSE} (\strong{default}), all found peaks are presented with their
#'   corresponding values (text annotations).
#' @param ... additional arguments specified, please refer to the \code{\link{plot_EPR_Specs}} function,
#'   in order to customize the graphical output.
#'
#'
#' @return
#'   List, consisting of following components, will be returned:
#'   \describe{
#'   \item{df}{Data frame, corresponding to the original \code{data.spectr}, with all peaks (\code{Intensity}
#'   \emph{vs} \code{x}) found by \code{\link[gsignal]{findpeaks}} algorithm and the arguments defined above.}
#'   \item{plot}{Graphical representation of found peaks in the EPR/ENDOR spectrum.}
#'   }
#'
#'
#' @examples
#' ## loading TMPD built-in example file:
#' tmpd.data.file.path <-
#'   load_data_example(file = "TMPD_specelchem_accu_b.asc")
#' ## reading data:
#' tmpd.data.file <-
#'   readEPR_Exp_Specs(path_to_ASC = tmpd.data.file.path,
#'                     col.names = c("B_G",
#'                                   "dIepr_over_dB"),
#'                     qValue = 3500,
#'                     norm.vec.add = 20,
#'                     origin = "winepr")
#' #
#' ## peak picking of only positive (Intensitity > 0)
#' ## peaks in the derivative EPR spectrum of TMPD:
#' tmpd.peak.pick.01 <-
#'   eval_peakPick_Spec(data.spectr = tmpd.data.file,
#'                      only.peak.pn = "p",
#'                      min.peak.dist = 1)
#' #
#' ## peak picking visualization:
#' tmpd.peak.pick.01$plot
#' #
#' ## found peaks data frame preview
#' tmpd.peak.pick.01$df
#' #
#' ## peak picking in 'G' (not in default 'mT')
#' ## of both positive as well as negative intensities
#' ## with the intensity threshold 15% of the maximum,
#' ## peaks annotation text angle 60 deg:
#' tmpd.peak.pick.02 <-
#'   eval_peakPick_Spec(
#'     data.spectr = tmpd.data.file,
#'     x = "B_G",
#'     x.unit = "G",
#'     min.peak.height =
#'       0.15 * max(tmpd.data.file$dIepr_over_dB),
#'     peak.text.angle = 60
#'   )
#' #
#' ## graph/EPR spectrum peaks preview:
#' tmpd.peak.pick.02$plot
#'
#'
#' @export
#'
#' @importFrom ggplot2 geom_text
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
  xvar <- NULL
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
              was automamtically set to TRUE. ")
    } else {
      double.sided <- double.sided
    }
  }
  if (grepl("integ|Integ|absorpt|Absorpt",lineSpecs.form)){
    if (isTRUE(double.sided)){
      double.sided <- double.sided %>% `if`(isTRUE(double.sided),FALSE,.)
      message(" For the integrated/absorption line form only positive values\n
              are taken. Therefore, the `double.sided` argument \n
              was automamtically set to FALSE. "
      )
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
          round(0.5 / (data.spectr[[x]][2] - data.spectr[[x]][1])), .)
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
  ## Intensity values corresponding to peaks =>
  peaks.values.vec <- peaks.list$pks
  #
  ## x-values/indices corresponding to peaks
  peaks.x.vec <- data.spectr[[x]][peaks.list$loc]
  #
  ## constructing a data frame from the previously picked peaks
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
