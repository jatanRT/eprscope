#
#' Draw Molecule by `rcdk` Defined by SMILES
#'
#' @param molecule tbc
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
drawMolecule_rcdk <- function(molecule, name = NULL, sma = NULL, ...){
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
  molecule_sdf <- rcdk::view.image.2d(molecule[[1]],depictor = dep)
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
  graphics::rasterImage(molecule_sdf, 1,1, 10,10)
  graphics::text(x = 5.5,y = 1.1,deparse(substitute(molecule)))
  #
}
