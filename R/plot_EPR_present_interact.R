#
#' Presents/Saves Interactive Plotly-Plot/-Graph Depending on Output Document Format
#'
#' @param plot tbc
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
#' @importFrom knitr pandoc_to
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot2 webshot
plot_EPR_present_interact <- function(plot,
                                      size.width = 700,
                                      size.height = 500,
                                      size.resolv.f = 2){
  if(knitr::pandoc_to("pdf","docx")){
    saveWidget(widget = plot,
               file = paste0(deparse(substitute(plot)),".html"),
               selfcontained = F)
    webshot(url = paste0(deparse(substitute(plot)),".html"),
            file = paste0(deparse(substitute(plot)),".png"),
            delay = 2,
            vwidth = 700,
            vheight = 500,
            zoom = 2)
  } else{
    return(plot)
  }
}
