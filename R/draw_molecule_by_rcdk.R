#
#' Draw Molecule by `rcdk` Defined by SMILES
#'
#'
#' @family Visualization and Graphics
#'
#'
#' @description
#'   A short description...
#'
#'
#'
#' @param molecule Character string => either SMILES strings (in case
#'   of \code{type = "smiles"}/\code{type = "SMILES"}) or file path strings
#'   pointing to \code{".sdf"} files.
#' @param type Character string ...tbc... either \code{type = "smiles"}/\code{type = "SMILES"}
#'   or \code{type = "sdf"}/\code{type = "SDF"}. \strong{Default}: \code{type = "smiles"}.
#' @param mol.name Character string pointing to name of molecule/compound, e.g. like
#'   \code{mol.name = c("acetone")} or \code{mol.name = c("PBN")}. If \code{mol.name = NULL}
#'   (\strong{default}) a character string "mol. structure viewer" with gray color
#'   is shown.
#' @param mol.name.color Character string pointing to displayed font color of the chemical structure
#'   name. \strong{Default}: \code{mol.name.color = "black"}.
#' @param mol.name.xy.posit Numeric vector matching the x,y-position of the \code{mol.name}
#'   label within the image having the size of "(1,1,10,10)" =>
#'   see \code{\link[graphics]{rasterImage}}.
#' @param sma Character string allowing to highlight sub-structures using `SMARTS`
#'   (\strong{SM}ILES \strong{A}rbitrary \strong{T}arget \strong{S}pecification) to highlight
#'   common substructures in a set of molecules, e.g. like \code{sma = "C=O"}.
#' @param annotate Character string, whether to display (\code{annotate = "number"}) or not display
#'   (\code{annotate = "off"}) atomic numbers/indexes. \strong{Default}: \code{annotate = "off"}.
#' @param style Character string denoting the plotting style like =>
#'   \tabular{rcl}{
#'   ----------------  \tab ----------------------------- \cr
#'   \strong{Plotting Style} \tab \strong{Style Text String} \cr
#'   ----------------  \tab ----------------------------- \cr
#'   `color on black`  \tab \code{style = "cob"} \cr
#'   `color on white`  \tab \code{style = "cow"} \cr
#'   `black on white`  \tab \code{style = "bow"} \cr
#'   `white on black` \tab \code{style = "wob"} \cr
#'   `neon on black`  \tab \code{style = "nob"} \cr
#'   ---------------- \tab ----------------------------- \cr
#'   }
#'   \strong{Default}: \code{style = "cow"}.
#' @param abbr Character string which controls how the structure is displayed. Following options
#'   can be set => \code{abbr = "off"} (\strong{default}) pointing to present structure as is;
#'   \code{abbr = "groups"} creating an abbreviation for `groups`; \code{abbr = "reagents"} creating
#'   an abbreviation for `reagents` or \code{abbr = "on"} to abbreviate the both latter.
#'   The \code{abbr = "groups"} WORKS ONLY IF \code{annotate = "of"}!
#' @param suppressh Logical, denoting whether to suppress displaying the hydrogen atoms.
#'   The `smiles` or `sdf` STRUCTURE MUST CONTAIN \strong{H} ATOMS!
#'   \strong{Default}: \code{supressh = TRUE}.
#' @param ... Additional options/arguments for \code{\link[rcdk]{get.depictor}}.
#'
#'
#' @return
#' tbc
#'
#'
#' @examples
#' ## draw N,N,N',N'-tetramethyl-p-phenylenediamine based
#' ## on `smiles` code character with highlighting
#' ## the "C(aromatic)--N" bond
#' draw_molecule_by_rcdk("CN(C)C1=C([H])C([H])=C(N(C)C)C([H])=C1[H]",
#'                       type = "smiles",
#'                       sma = "cN")
#' #
#' ## draw N,N,N',N'-tetramethyl-p-phenylenediamine (TMPD) radical
#' ## cation based on `smiles` code character, with hydrogen atoms
#' ## and molecule name label = "TMPD^(+.)"
#' draw_molecule_by_rcdk("CN(C)[C+]1C([H])=C([H])[C.]([N](C)C)C([H])=C1[H]",
#'                       type = "smiles",
#'                       mol.name = expression(TMPD^(+.)),
#'                       mol.name.color = "blue",
#'                       suppressh = FALSE)
#' #
#' ## draw N,N,N',N'-tetramethyl-p-phenylenediamine based
#' ## on `sdf` file path ("TMPD.sdf") with "color on black"
#' ## style + atom numbering
#' draw_molecule_by_rcdk(molecule = load_data_example("TMPD.sdf"),
#'                       type = "sdf",
#'                       annotate = "number",
#'                       style = "cob")
#'
#'
#' @export
#'
#'
#' @importFrom rcdk get.depictor view.image.2d load.molecules  parse.smiles
#' @importFrom graphics rasterImage text par
#' @importFrom stringr str_detect regex
## function to plot molecules inspired by https://riveradelgado.com/post/2021/04/18/chemistry-in-r/,
## which is not available in `rcdk`
draw_molecule_by_rcdk <- function(molecule,
                                  type = "smiles", ## or "sdf"
                                  mol.name = NULL,
                                  mol.name.color = "black",
                                  mol.name.xy.posit = c(8.2,1.2),
                                  sma = NULL,
                                  annotate = "off",
                                  style = "cow",
                                  abbr = "off",
                                  suppressh = TRUE,
                                  ...){
  ## molecules is string vector, however it is converted into an object
  ## as returned by rcdk::load.molecules or rcdk::parse.smiles() => see below;
  ## name a character for the name of the molecule,
  ## sma a character witht the smarts string as passed onto get.depictor()
  ## ... other arguments for get.depictor()
  ##
  # Image aesthetics `depictor`
  dep <- rcdk::get.depictor(
    width = 600,
    height = 400,
    zoom = 6,
    sma = sma,
    annotate = annotate,
    style = style,
    abbr = abbr,
    suppressh = suppressh,
    ...
  )
  #
  ## load molecular types
  if (stringr::str_detect(type,stringr::regex("SMILES",ignore_case = TRUE))){
    #
    molekel <- rcdk::parse.smiles(molecule)
  }
  if (stringr::str_detect(type,stringr::regex("SDF",ignore_case = TRUE))){
    #
    ## create a similar as in the previous case
    molekel <- rcdk::load.molecules(molfiles = molecule)
  }
  #
  molecule_sdf <- rcdk::view.image.2d(molekel[[1]],depictor = dep)
  #
  ## Remove extra margins around the molecule
  graphics::par(mar = c(0,0,0,0))
  plot(NA,
       xlim = c(1, 10),
       ylim = c(1, 10),
       ## Remove the black bounding boxes around the molecule
       axes = FALSE)
  #
  ## draw and annotate molecule
  graphics::rasterImage(molecule_sdf,1,1,10,10)
  #
  ## drawing `mol.name`
  if (is.null(mol.name)){
    graphics::text(x = mol.name.xy.posit[1],
                   y = mol.name.xy.posit[2],
                   labels = c("mol. structure viewer"),
                   col = "gray")
  } else{
    graphics::text(x = mol.name.xy.posit[1],
                   y = mol.name.xy.posit[2],
                   labels = mol.name,
                   col = mol.name.color)
  }
  #
}
