#
#' Draw Molecule by `rcdk` Defined by SMILES
#'
#'
#' @family Visualization and Graphics
#'
#'
#' @description
#' A short description...
#'
#'
#'
#' @param molecules tbc
#' @param name tbc
#' @param sma tbc
#' @param ... tbc
#'
#'
#' @return
#' tbc
#'
#' @examples
#' \dontrun{
#' tbc
#' tbc
#' }
#'
#' @export
#'
#'
#' @importFrom rcdk get.depictor view.image.2d
#' @importFrom graphics rasterImage text par
## function to plot molecule (https://riveradelgado.com/post/2021/04/18/chemistry-in-r/),
## which is not available in `rcdk`
draw_molecules_rcdk <- function(molecules, name = NULL, sma = NULL, ...){
  ## molecule an object as returned by rcdk::load.molecules or rcdk::parse.smiles()
  ## name a character for the name of the molecule,
  ## sma a character witht the smarts string as passed onto get.depictor()
  ## ... other arguments for get.depictor()
  ##
  # Image aesthetics
  dep <- rcdk::get.depictor(
    width = 600, height = 400,
    zoom = 6, sma = sma, ...
  )
  #
  molecules_sdf <- rcdk::view.image.2d(molecules,depictor = dep)
  #
  ## Remove extra margins around the molecule
  graphics::par(mar = c(0,0,0,0))
  plot(NA,
       xlim = c(1, 10),
       ylim = c(1, 10),
       # Remove the black bounding boxes around the molecule
       axes = F)
  #
  ## draw and annotate molecule
  graphics::rasterImage(molecules_sdf, 1,1, 10,10)
  graphics::text(x = 5.5,y = 1.1,deparse(substitute(molecules)))
  #
}
