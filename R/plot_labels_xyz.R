#
#' Labels for Various Plots (Spectroscopy, EPR, Voltammetry,...etc)
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description
#'   Using physico-chemical notation of quantities or formulas (labels and titles with greek symbols,
#'   subscript and superscript...etc) in static plots. The function is inspired by
#'   \href{https://www.r-bloggers.com/math-notation-for-r-plot-titles-expression-and-bquote/}{R-Bloggers:Math Notation}
#'   and \href{https://www.r-bloggers.com/r-plotmath-functions-combined-with-variable-values/}{R-Bloggers:Plotmath},
#'   in order to simplify the writing. See also \code{\link[grDevices]{plotmath}} documentation or in console
#'   write \code{?plotmath}.
#'
#'
#' @param quantity Variable string \strong{without quotation} (some parts of the expression can be however quoted,
#'   see examples below), corresponding to physical quantity, which should be displayed as the (axis) title
#'   like \eqn{B}, d\eqn{I_{EPR}}/d\eqn{B}, \eqn{time}, \eqn{\Delta B_{pp}}, \emph{Double Integral}...etc.
#' @param unit Variable String \strong{without quotation} (some parts of the expression can be however quoted,
#'   see examples below) referring to displayed physical quantity unit, like \code{mT}, \code{s},
#'   \code{p.d.u.}, ...etc.
#' @param user.defined Logical, in order to bring more flexibility to customize quantities and units
#'   to meet the users needs.
#'
#'
#' @return Axis labels/expressions for different plots.
#'
#'
#' @examples
#' \dontrun{
#' plot_labels_xyz(B,mT)
#' #
#' plot_labels_xyz(d*italic(I)[EPR]~"/"~d*italic(B),
#'                 "("~p.d.u.~")",
#'                 user.defined = TRUE)
#' #
#' plot_labels_xyz(d*italic(I)[EPR]~"/"~d*italic(B),p.d.u.)
#'
#' #
#' plot_labels_xyz(quantity = Delta*B[pp],
#'                 unit = "("~mT~")",
#'                 user.defined = TRUE)
#' #
#' plot_labels_xyz(t,s)
#' #
#' plot_labels_xyz(T,K)
#' #
#' plot_labels_xyz(E,
#'                 "("~V~")"~~~italic(vs)~~~italic(Ref.~Electrode),
#'                 user.defined = TRUE)
#' #
#' plot_labels_xyz(c,mmol~dm^-3)
#' #
#' plot_labels_xyz(Double~~Integral,
#'                 p.d.u.,
#'                 user.defined = FALSE)
#' #
#' plot_labels_xyz(italic(nu)[RF],"("~MHz~")",
#'                 user.defined = TRUE)
#' }
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
#' ## `plot_labels_xyz()`
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
#' @importFrom rlang enquo
plot_labels_xyz <- function(quantity,
                            unit,
                            user.defined = FALSE) {
  #
  ## in 'bquote' bang-bang ('!!') operator does not work
  ## use '.()' instead
  quantity <- enquo(quantity)
  unit <- enquo(unit)
  if (isFALSE(user.defined)) {
    l <- bquote(italic(.(quantity)) ~ ~"(" ~ .(unit) ~ ")")
  } else {
    l <- bquote(.(quantity) ~ ~ .(unit))
  }
  #
  return(l)
  #
}
