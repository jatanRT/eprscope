#
#' Present/Save Interactive Plotly-Plot/-Graph for Publication (HTML,PDF,Word)
#'
#'
#' @family Visualizations and Graphics
#'
#'
#' @description Depending on Output Document Format
#'
#'
#' @param p tbc
#' @param size.width tbc
#' @param size.height tbc
#' @param size.resolv.f tbc
#'
#' @return Plot format depending on the output document one
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
#' @importFrom knitr pandoc_to is_latex_output
plot_EPR_present_interact <- function(p,
                                      size.width = 700,
                                      size.height = 500,
                                      size.resolv.f = 2) {
  #
  ## widget and webshot
  widget <- htmlwidgets::saveWidget(
    widget = p,
    file = paste0(deparse(substitute(p)), ".html"),
    selfcontained = FALSE
  )
  webshot <- webshot2::webshot(
    url = paste0(deparse(substitute(p)), ".html"),
    file = paste0(deparse(substitute(p)), ".png"),
    delay = 2,
    vwidth = size.width,
    vheight = size.height,
    zoom = size.resolv.f
  )
  #
  ## Conditions
  if (knitr::is_latex_output() || knitr::pandoc_to(fmt = "docx")) {
    widget
    webshot
  } else {
    p
  }
}
