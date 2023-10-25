#
#' Rearrangement of \eqn{A_{iso}}/\eqn{a_{iso}} from Quantum Chemical Computations
#'
#'
#' @family Evaluations and Quantum Chemistry
#'
#'
#' @description
#'   Provides table from Gaussian/ORCA output text file to summarize \eqn{A_{iso}}/\eqn{a_{iso}} values
#'   according to proposed molecular structure/symmetry.
#'
#'
#'
#' @details
#'   The \eqn{A_{iso}}/\eqn{a_{iso}} values are computed for each atom/nucleus
#'   (with its corresponding \code{atomic number within the structure} as well as with the characteristic
#'   \code{isotopic number/value}), such an entire table can be copied from \strong{Gaussian} output
#'   (after \code{'Isotropic Fermi Contact Couplings'} line) or can be constructed from \strong{ORCA} output,
#'   example for such a file structure (from \strong{Gaussian}):
#'   \tabular{rcccc}{
#'   --------- \tab ----------- \tab ----------- \tab ---------- \tab --------- \cr
#'    \strong{No_atom} \tab \strong{Atom_Nucleus} \tab \strong{MegaHertz} \tab \strong{Gauss} \tab \strong{1e-4_cm-1} \cr
#'    --------- \tab ----------- \tab ----------- \tab ---------- \tab --------- \cr
#'    1 \tab N(14) \tab 0.00643 \tab 0.00229 \tab 0.00214 \cr
#'    17 \tab N(14) \tab 13.99707 \tab 4.9945 \tab 4.66892 \cr
#'    28 \tab H(1) \tab 16.34971 \tab 5.83398 \tab 5.45368 \cr
#'    --------- \tab ----------- \tab ----------- \tab ---------- \tab --------- \cr
#'   }
#'
#'
#'
#' @param path_to_ASC Character string pointing to path of ASCII file (\code{txt},\code{csv}...etc,
#' it may be also provided by \code{\link[base]{file.path}}) incl. characteristic
#'   \eqn{A_{iso}} or \eqn{a_{iso}} values.
#' @param col.names Character string vector containing names of all columns from QCH Computational output,
#'   for the names see the example in \code{path_to_ASC}, they must contain atomic/structure number, isotopic value
#'   with element label (nucleus characterization) and \eqn{A} in MHz as well as \eqn{a} in Gauss.
#' @param nuclei.list.slct List of numeric values for the rearrangement of selected atoms/nuclei according to symmetry,
#'   e.g. like: \code{nuclei.list.slct <- list(3,c(21,22),c(20,23),c(24,25),c(27,26))} where the numbers
#'   correspond to indices of atoms/nuclei in the ASCII text file.
#'
#'
#' @return Data frame/Table of \eqn{A_{iso}}/\eqn{a_{iso}} mean values corresponding to nuclei group
#'   structure/symmetry
#'
#'
#' @examples
#' \dontrun{
#' rearrange_aAiso_QCHcomp(
#' "./iso_values_Gaussian.txt",
#' c("No","Nucleus","au","Megahertz","Gauss","10^n4_cm^n1"),
#' list(3,c(21,22),c(20,23),c(24,25),c(27,26))
#' )
#' }
#'
#'
#' @export
#'
#'
#' @importFrom stringr str_subset str_extract regex
rearrange_aAiso_QCHcomp <- function(path_to_ASC,
                                    col.names,
                                    nuclei.list.slct) {
  #
  ## 'Temporary' processing variables
  mT <- NULL
  #
  ## Conditions/Extraction for column names:
  ## use stringr::str_subset(...) or X[grepl(...)] or stringr::str_extract or grep(...,value = T)
  A.str <- stringr::str_subset(col.names,
                               regex("mhz|megahertz",
                                     ignore_case = T))
  a.str <- stringr::str_subset(col.names,
                               regex("gauss|G|Gauss"))
  atomic.num.str <- stringr::str_subset(col.names,
                                        regex("No|Num|num|no|no_|num_|NUM|Num_|NUM_|NO|NO_|No_"))
  nucl.str <- stringr::str_subset(col.names,
                                  regex("nuc|Nuc_atom|nucleus_|NUC|NUC_|ATOM|atom",
                                        ignore_case = T))
  #
  ## Read the data:
  data.Aa.comput <- data.table::fread(path_to_ASC,
    sep = "auto", header = F, skip = 1,
    col.names = col.names
  ) %>%
    dplyr::mutate(
      MegaHertz = abs(round(.data[[A.str]], digits = 3)),
      Gauss = abs(round(.data[[a.str]], digits = 2))
    ) %>%
    dplyr::select(dplyr::all_of(c(atomic.num.str,
                                  nucl.str,
                                  "MegaHertz",
                                  "Gauss")))
  #
  ## Own function to rearrange A/a according to `nuclei.list.slct`
  ## Build up new rearranged data frame for Nuclei A/a:
  data.slct.nucs.group <- data.frame(
    No = integer(), Nucleus = character(),
    MegaHertz = double(), Gauss = double(), NuclearGroup = character()
  )
  #
  ## cycle for each `nuclei.list.slct` component
  for (k in 1:length(nuclei.list.slct)) {
    #
    ## Filter atomic numbers from each part of the list:
    sal <- data.Aa.comput %>%
      dplyr::filter(.data[[atomic.num.str]] %in% nuclei.list.slct[[k]])
    #
    ## How many atoms/nuclei are included in each list part:
    how.many.nucs <- length(nuclei.list.slct[[k]])
    #
    ## Extract atomic/nuclear label:
    mark.nucs <- stringr::str_extract(sal[[nucl.str]][1], "[[:alpha:]]+")
    #
    ## Extract isotope number:
    nucleo.nucs <- stringr::str_extract(sal[[nucl.str]][1], "[[:digit:]]+")
    #
    ## Extract atomic/nuclear numbers and collapse it into one string:
    num.nucs.str <- paste(sal[[atomic.num.str]], collapse = ",")
    #
    ## Now combine all four variables by paste:
    group.nucs <- paste0(how.many.nucs, " x ", nucleo.nucs, mark.nucs, " (", num.nucs.str, ")")
    #
    ## Replicate the previous string variable corresponding to length of a list component/part:
    NuclearGroup <- rep(group.nucs, length(nuclei.list.slct[[k]]))
    #
    ## Build up data frame row by row:
    gal <- data.frame(sal, NuclearGroup) ## at first combine by columns
    data.slct.nucs.group <- rbind(data.slct.nucs.group, gal)
  }
  ## Group by and summarize (find the mean value of each list component) according
  ## to `nuclei.list.slct` (symmetry) and previous `for loop`:
  #
  data.slct.nucs.group <- data.slct.nucs.group %>%
    dplyr::mutate(mT = .data$Gauss / 10) %>%
    dplyr::select(-dplyr::any_of(c("No","Nucleus","Gauss"))) %>%
    dplyr::group_by(.data$NuclearGroup) %>%
    dplyr::summarize(
      Aiso_MHz_QCH = abs(round(mean(.data$MegaHertz), digits = 3)),
      aiso_mT_QCH = abs(round(mean(.data$mT), digits = 2))
    )
  #
  ## Entire output table:
  return(data.slct.nucs.group)
  #
}
