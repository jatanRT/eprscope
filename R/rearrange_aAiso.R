#'
#' Rearrangement of \eqn{A_{iso}}/\eqn{a_{iso}} from Quantum Chemical (QCH) Computations
#'
#'
#' @family Evaluations and Quantum Chemistry
#'
#'
#' @description
#'   Providing table, based on Gaussian/ORCA/...etc. output text files in order to summarize
#'   the mean \eqn{A_{iso}}/\eqn{a_{iso}} values of groups with equivalent nuclei, according to proposed
#'   molecular structure/symmetry.
#'
#'
#'
#' @details
#'   The \eqn{A_{iso}}/\eqn{a_{iso}} values are computed for each atom/nucleus
#'   (with its corresponding \code{atomic number within the structure} as well as with the characteristic
#'   \code{isotopic number/value}), such an entire table can be copied e.g. from \strong{Gaussian} output
#'   (after \code{'Isotropic Fermi Contact Couplings'} line) or can be constructed from \strong{ORCA} (or any other) output,
#'   example for such a file structure (from \strong{Gaussian}):
#'   \tabular{rcccc}{
#'    \strong{No_atom} \tab \strong{Atom_Nucleus} \tab \strong{MegaHertz} \tab \strong{Gauss} \tab \strong{1e-4_cm-1} \cr
#'    1 \tab N(14) \tab 0.00643 \tab 0.00229 \tab 0.00214 \cr
#'    17 \tab N(14) \tab 13.99707 \tab 4.9945 \tab 4.66892 \cr
#'    28 \tab H(1) \tab 16.34971 \tab 5.83398 \tab 5.45368 \cr
#'   }
#'   The input table/data frame, like the previous one, must include following columns: atomic/nucleus number, atom/nucleus notation,
#'   hyperfine coupling constant in \code{MHz} and finally hyperfine splitting constant in \code{G}. These columns/variables
#'   are essential for the evaluation.
#'
#'
#'
#' @param path_to_ASC Character string pointing to path of ASCII file (\code{txt},\code{csv}...etc,
#' it may be also provided by \code{\link[base]{file.path}}). The file must include characteristic
#'   \eqn{A_{iso}} or \eqn{a_{iso}} values.
#' @param col.names Character string vector containing names of all columns from QCH computational output,
#'   for the names see the example in \code{Details}, they must contain atomic/structure number, isotopic value
#'   with element label (nucleus characterization) and \eqn{A} in MHz as well as \eqn{a} in Gauss.
#' @param nuclei.list.slct List of numeric values for the rearrangement of selected atoms/nuclei according to symmetry,
#'   e.g. \code{nuclei.list.slct <- list(3,c(21,22),c(20,23),c(24,25),c(27,26))} where the numbers
#'   correspond to indices of proposed equivalent nuclei in the ASCII text file.
#'
#'
#' @return Data frame/Table of \eqn{A_{iso}}/\eqn{a_{iso}} mean values corresponding to groups
#'   of equivalent nuclei within the structure/symmetry.
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
    dplyr::select(-dplyr::all_of(c("No","Nucleus","Gauss"))) %>%
    dplyr::group_by(.data$NuclearGroup) %>%
    dplyr::summarize(
      Aiso_MHz_QCH = abs(round(mean(.data$MegaHertz), digits = 2)),
      aiso_mT_QCH = abs(round(mean(.data$mT), digits = 2))
    )
  #
  ## Entire output table:
  return(data.slct.nucs.group)
  #
}
#
#
#
#
#' Rearrangement of \eqn{A_{iso}}/\eqn{a_{iso}} from Gaussian & ORCA Computations
#'
#'
#' @family Evaluations and Quantum Chemistry
#'
#'
#' @description
#'   Providing table, specifically from \code{Gaussian} or \code{ORCA} output text files to summarize
#'   the \eqn{A_{iso}}/\eqn{a_{iso}} mean values of groups with equivalent nuclei, according
#'   to proposed molecular structure/symmetry (see also \code{\link{rearrange_aAiso_QCHcomp}}).
#'
#'
#' @inheritParams rearrange_aAiso_QCHcomp
#' @param path_to_QCHoutput Character string corresponding to path of \code{Gaussian} or \code{ORCA} output text files.
#' @param N.nuclei Numeric value that equals to number of atoms/nuclei within the calculated structure.
#' @param origin Character string pointing to origin of DFT EPR calculation parameters <=> which
#'   software package was used. Only two values are available => \code{"gaussian"} (\strong{default})
#'   or \code{"orca"}.
#' @param output.text.origin Logical, whether to write a text file containing the extracted
#'   \eqn{A_{iso}}/\eqn{a_{iso}} values from the the original output file defined by the \code{path_to_QCHoutput}.
#'   \strong{Default}: \code{output.text.origin = FALSE}.
#' @param output.text.path Character string, setting the path to file containing
#'   the extracted \eqn{A_{iso}}/\eqn{a_{iso}} values from the original output file defined
#'   by the \code{path_to_QCHoutput}. See also the previous argument.
#'
#'
#' @return Data frame/Table of \eqn{A_{iso}}/\eqn{a_{iso}} mean values corresponding to groups
#'   of proposed equivalent nuclei within the structure/symmetry constructed directly
#'   from \emph{Gaussian} or \emph{ORCA} output files.
#'
#'
#' @examples
#' ## built-in file and path
#' gauss.file.path <-
#'   load_data_example(file = "TMPDAradCatEPRa.inp.log.zip")
#' gauss.file <- unzip(gauss.file.path)
#' symmetry.As.df <-
#'   rearrange_aAiso_QCHorgau(gauss.file,
#'     N.nuclei = 28,
#'     nuclei.list.slct =
#'     list(c(7, 8), ## 2 x 14N
#'          c(13, 14, 15, 16), ## 4 x 1H (aromatic)
#'          c(17, 18, 19, 20,
#'            21, 22, 23, 24,
#'            25, 26, 27, 28) ## 12 x 1H (methyl groups)
#'          )
#'      )
#' #
#' ## preview
#' symmetry.As.df
#'
#'
#' @export
#'
#'
rearrange_aAiso_QCHorgau <- function(path_to_QCHoutput,
                                     N.nuclei,
                                     nuclei.list.slct,
                                     origin = "gaussian",
                                     output.text.origin = FALSE,
                                     output.text.path = NULL) {
  #
  ## 'Temporary' processing variables
  mT <- NULL
  #
  ## read output files from Gaussian or ORCA + define number (how many) of atoms/nuclei
  ## either of all (the entire molecule) or relevant (in case of ORCA)
  qchfile <- readLines(path_to_QCHoutput)
  No_nuclei_atoms <- N.nuclei
  #
  ## ============================ READING `qchfiles` ==============================
  #
  ## Processing of output file depending on the origin
  if (origin == "gaussian" || origin == "Gaussian" || origin == "GAUSSIAN") {
    #
    ## number of atoms + 1
    ## because it also reads the header therefore, there must be an additional line
    No_nuclei_atoms_mod <- No_nuclei_atoms + 1
    #
    ## indicator (String) to select specific lines (indices) from `qchfile`
    indicator.line <- "Fermi Contact"
    start.reading.line <- grep(indicator.line, qchfile)
    #
    ## selecting all couplings (file lines/row),
    ## however there are two `start.reading.line`
    ## character strings in Gaussian EPR Output
    ## therefore, select only the first one
    qchfile.select.A <- qchfile[start.reading.line[1] + 1:No_nuclei_atoms_mod]
    #
    ## output original gaussian data file in-between:-) =>
    if (isTRUE(output.text.origin)) {
      fileConn <- ifelse(is.null(output.text.path),
                         stop(" Please define `.txt` file path to be saved ! "),
                         file(output.text.path)
      )
      writeLines(qchfile.select.A, fileConn)
      close(fileConn)
    }
    #
    ## each line of `qchfile.select.A` is a long string therefore,
    ## it must be split into individual pieces
    qchfile.select.A <- stringr::str_split(qchfile.select.A, pattern = "\\s+")
    #
    ## create a data frame, the first row line is the header,
    table.select.A <- data.frame("No" = character(), "Nucleus" = character(),
                                 "Megahertz" = character(),"Gauss" = character())
    ## therefore start form [[2]] `second row/line`
    for (i in 2:No_nuclei_atoms_mod) {
      table.select.B <- data.frame(
        "No" = qchfile.select.A[[i]][2],
        "Nucleus" = qchfile.select.A[[i]][3],
        "Megahertz" = qchfile.select.A[[i]][5],
        "Gauss" = qchfile.select.A[[i]][6]
      )
      #
      table.select.A <- rbind(table.select.A,table.select.B)
    }
    #
    ## converting columns into character and numeric (double) format
    table.select.A$No <- as.double(as.character(table.select.A$No))
    table.select.A$Nucleus <- as.character(table.select.A$Nucleus)
    for (j in 3:length(colnames(table.select.A))) {
      table.select.A[, j] <- as.double(as.character(table.select.A[, j]))
    }
    #
  }
  if (origin == "orca" || origin == "Orca" || origin == "ORCA") {
    ## indicator (String) to select specific lines (indices) from `qchfile`
    ## in the case of `ORCA` this indicator tells that this is the last part
    ## of `qchfile` with EPR parameters (`ORCA`) has different output file
    ## structure in comparison to `Gaussian`
    main.indicator.line <- "ELECTRIC AND MAGNETIC HYPERFINE STRUCTURE"
    start.reading.line <- grep(main.indicator.line, qchfile)
    ## Select only part of `qchfile` with EPR params. with all lines down
    qchfile.select <- qchfile[-(1:start.reading.line - 1)]
    #
    ## output original orca data file in-between:-) =>
    if (isTRUE(output.text.origin)) {
      fileConn <- ifelse(is.null(output.text.path),
                         stop(" Please define `.txt` file path to be saved ! "),
                         file(output.text.path)
      )
      writeLines(qchfile.select, fileConn)
      close(fileConn)
    }
    ## Reading the lines with relevant information (Atoms + A HF couplings)
    ## Atoms/Nuclei
    nucleus.indicator <- "Nucleus"
    nuclei.lines <- grep(nucleus.indicator, qchfile.select)
    ## reading only for selected number of atoms
    nuclei.qchfile.select <- lapply(
      seq(No_nuclei_atoms),
      function(x) qchfile.select[nuclei.lines][[x]]
    )
    ## splitting the strings
    nuclei.qchfile.select <- stringr::str_split(nuclei.qchfile.select, pattern = "\\s+")
    ## separate atomic/nucleus number (`n`) and atomic/nucleus label (`L`) + isotope (`I`)
    nuclei.qchfile.select.n <- sapply(
      seq(nuclei.qchfile.select),
      function(n) {
        stringr::str_extract(
          nuclei.qchfile.select[[n]][2],
          "[[:digit:]]+"
        )
      }
    )
    nuclei.qchfile.select.L <- sapply(
      seq(nuclei.qchfile.select),
      function(L) {
        stringr::str_extract(
          nuclei.qchfile.select[[L]][2],
          "[[:alpha:]]+"
        )
      }
    )
    nuclei.qchfile.select.I <- sapply(
      seq(nuclei.qchfile.select),
      function(I) nuclei.qchfile.select[[I]][5]
    )
    ## combine last two vectors into one like `H(1)` in order to be consistent
    ## with the Gaussian output
    nuclei.qchfile.select.LI <- mapply(
      function(L, I) paste(c(L, "(", I, ")"), collapse = ""),
      nuclei.qchfile.select.L,
      nuclei.qchfile.select.I
    )
    #
    ## A values
    A.indicator <- "A\\(iso\\)" ## there are parenthesis,
    ## therefore one should escape the backslashes
    A.lines <- grep(A.indicator, qchfile.select)
    ## reading only for selected number of atoms
    A.qchfile.select <- lapply(seq(No_nuclei_atoms),
                               function(y) qchfile.select[A.lines][[y]])
    ## splitting the strings
    A.qchfile.select <- stringr::str_split(A.qchfile.select, pattern = "\\s+")
    ## select the A iso values by [[i]][6] for i-th list/line element
    A.qchfile.select.iso <- sapply(
      seq(A.qchfile.select),
      function(i) A.qchfile.select[[i]][6]
    )
    #
    ## Generate the data frame,
    ## first of all define numeric columns
    A.qchfile.select.iso.mhz <- as.numeric(as.character(A.qchfile.select.iso))
    A.qchfile.select.iso.gauss <-
      convert_A_MHz_2a(as.numeric(as.character(A.qchfile.select.iso))) * 10
    #
    table.select.A <- data.frame(
      "No" = nuclei.qchfile.select.n,
      "Nucleus" = nuclei.qchfile.select.LI,
      "Megahertz" = A.qchfile.select.iso.mhz,
      "Gauss" = A.qchfile.select.iso.gauss
    )
  }
  #
  ## ## delete original variable which is big and not required anymore
  rm(qchfile) ## instead of `qchfile <- NULL`
  #
  ## ================ PROCESSING (REARRANGEMENT) of `table.select.A` ==================
  #
  ## Own function to rearrange A/a according to `nuclei.list.slct`
  ## Build up new rearranged data frame for Nuclei A/a:
  data.slct.nucs.group <- data.frame(
    "No" = integer(), "Nucleus" = character(),
    "Megahertz" = double(), "Gauss" = double(), "NuclearGroup" = character()
  )
  #
  ## cycle for each `nuclei.list.slct` component
  for (k in 1:length(nuclei.list.slct)) {
    #
    ## Filter atomic numbers from each part of the list:
    sal <- table.select.A %>%
      dplyr::filter(.data$No %in% nuclei.list.slct[[k]])
    #
    ## How many atoms/nuclei are included in each list part:
    how.many.nucs <- length(nuclei.list.slct[[k]])
    #
    ## Extract atomic/nuclear label:
    mark.nucs <- stringr::str_extract(sal$Nucleus[1], "[[:alpha:]]+")
    #
    ## Extract isotope number:
    nucleo.nucs <- stringr::str_extract(sal$Nucleus[1], "[[:digit:]]+")
    #
    ## Extract atomic/nuclear numbers and collapse it into one string:
    num.nucs.str <- paste(sal$No, collapse = ",")
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
    dplyr::select(-dplyr::all_of(c("No","Nucleus","Gauss"))) %>%
    dplyr::group_by(.data$NuclearGroup) %>%
    dplyr::summarize(
      Aiso_MHz_QCH = abs(round(mean(.data$Megahertz), digits = 2)),
      aiso_mT_QCH = abs(round(mean(.data$mT), digits = 2))
    )
  #
  ## Finally rearranged data frame
  return(data.slct.nucs.group)
  #
}
