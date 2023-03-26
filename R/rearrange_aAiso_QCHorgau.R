# Rearrangement for Gaussian & ORCA outputs
rearrange_aAiso_QCHorgau <- function(path_to_QCHoutput,
                                     No_nuclei,
                                     nuclei.list.slct,
                                     origin = "gaussian",
                                     output.text.file = FALSE,
                                     output.text.path = NULL){
  #
  ## 'Temporary' processing variables
  mT <- NULL
  #
  ## read output files from Gaussian or ORCA + define number (how many) of atoms/nuclei
  ## either of all (the entire molecule) or relevant (in case of ORCA)
  qchfile <- readLines(path_to_QCHoutput)
  No_nuclei_atoms <- No_nuclei
  #
  ## ============================ READING `qchfiles` ==============================
  #
  ## Processing of output file depending on the origin
  if (origin == "gaussian"){
    #
    ## number of atoms + 1
    ## because it also reads the header, therefore must be an additional line
    No_nuclei_atoms_mod <- No_nuclei_atoms + 1
    #
    ## indicator (String) to select specific lines (indices) from `qchfile`
    indicator.line <- "Fermi Contact"
    start.reading.line <- grep(indicator.line,qchfile)
    #
    ## selecting all couplings (file lines/row),
    ## however there are two `start.reading.line`
    ## character strings in Gaussian EPR Output
    ## therefore, select only the first one
    qchfile.select.A <- qchfile[start.reading.line[1]+1:No_nuclei_atoms_mod]
    #
    ## output file in-between:-) =>
    if (isTRUE(output.text.file)){
      fileConn <- ifelse(is.null(output.text.path),
                         stop(" Please define `.txt` file path to be saved ! "),
                         file(output.text.path))
      writeLines(qchfile.select.A,fileConn)
      close(fileConn)
    }
    #
    ## each line of `qchfile.select.A` is a long string therefore,
    ## it must be split into individual pieces
    qchfile.select.A <- stringr::str_split(qchfile.select.A,pattern = "\\s+")
    #
    ## starting to create a data frame, the first row line is the header,
    ## therefore start form [[2]]
    table.select.A <- data.frame("No" = qchfile.select.A[[2]][2], ## start from [[2]] = `second row/line`
                                 "Nucleus" = qchfile.select.A[[2]][3],
                                 "Megahertz" = qchfile.select.A[[2]][5],
                                 "Gauss" = qchfile.select.A[[2]][6])
    #
    ## ...keep going the same for the upcoming lines
    for (i in 3:No_Nuclei_Atoms_Mod){
      table.select.B <- data.frame("No" = qchfile.select.A[[i]][2],
                                   "Nucleus" = qchfile.select.A[[i]][3],
                                   "Megahertz" = qchfile.select.A[[i]][5],
                                   "Gauss" = qchfile.select.A[[i]][6])
      #
      table.select.A <- rbind(table.select.A,table.select.B)
    }
    #
    ## converting columns into character and numeric (double) format
    table.select.A$No <- as.double(as.character(table.select.A$No))
    table.select.A$Nucleus <- as.character(table.select.A$Nucleus)
    for (j in 3:length(colnames(table.select.A))){
      table.select.A[,j] <- as.double(as.character(table.select.A[,j]))
    }
    #
  }
  if (origin == "orca"){
    ## indicator (String) to select specific lines (indices) from `qchfile`
    ## in the case of `ORCA` this indicator tells that this is the last part
    ## of `qchfile` with EPR parameters (`ORCA`) has different output file
    ## structure in comparison to `Gaussian`
    main.indicator.line <- "ELECTRIC AND MAGNETIC HYPERFINE STRUCTURE"
    start.reading.line <- grep(main.indicator.line,qchfile)
    ## Select only part of `qchfile` with EPR params. with all lines down
    qchfile.select <- qchfile[-(1:start.reading.line-1)]
    #
    i## output file in-between:-) =>
    if (isTRUE(output.text.file)){
      fileConn <- ifelse(is.null(output.text.path),
                         stop(" Please define `.txt` file path to be saved ! "),
                         file(output.text.path))
      writeLines(qchfile.select,fileConn)
      close(fileConn)
    }
    ## Reading the lines with relevant information (Atoms + A HF couplings)
    ## Atoms/Nuclei
    nucleus.indicator <- "Nucleus"
    nuclei.lines <- grep(nucleus.indicator,qchfile.select)
    ## reading only for selected number of atoms
    nuclei.qchfile.select <- lapply(seq(No_nuclei_atoms),
                                    function(x) qchfile.select[nuclei.lines][[x]])
    ## splitting the strings
    nuclei.qchfile.select <- stringr::str_split(nuclei.qchfile.select,pattern = "\\s+")
    ## separate atomic/nucleus number and atomic/nucleus label + isotope
    nuclei.qchfile.select.n <- sapply(seq(nuclei.qchfile.select),
                                      function(n) stringr::str_extract(nuclei.qchfile.select[[n]][2],
                                                                       "[[:digit:]]+"))
    nuclei.qchfile.select.L <- sapply(seq(nuclei.qchfile.select),
                                      function(L) stringr::str_extract(nuclei.qchfile.select[[L]][2],
                                                                       "[[:alpha:]]+"))
    nuclei.qchfile.select.I <- sapply(seq(nuclei.qchfile.select),
                                      function(I) nuclei.qchfile.select[[I]][5])
    ## combine last two vectors into one like `H(1)` in order to be consistent with the Gaussian output
    nuclei.qchfile.select.LI <- mapply(function(L,I) paste(c(L,"(",I,")"),collapse = ""),
                                       nuclei.qchfile.select.L,
                                       nuclei.qchfile.select.I)
    #
    ## A values
    A.indicator <- "A\\(iso\\)" ## there are parenthesis, therefore one should escape the backslashes
    A.lines <- grep(A.indicator,qchfile.select)
    ## reading only for selected number of atoms
    A.qchfile.select <- lapply(seq(No_nuclei_atoms), function(y) qchfile.select[A.lines][[y]])
    ## splitting the strings
    A.qchfile.select <- stringr::str_split(A.qchfile.select,pattern = "\\s+")
    ## select the A iso values by [[i]][6] for i-th list/line element
    A.qchfile.select.iso <- sapply(seq(A.qchfile.select),
                                   function(i) A.qchfile.select[[i]][6])
    #
    ## Generate data frame,
    ## first of all define numeric columns
    A.qchfile.select.iso.mhz <- as.numeric(as.character(A.qchfile.select.iso))
    A.qchfile.select.iso.gauss <- convert_A_MHz_to_a(as.numeric(as.character(A.qchfile.select.iso)))*10
    #
    table.select.A <- data.frame("No" = nuclei.qchfile.select.n,
                                 "Nucleus" = nuclei.qchfile.select.LI,
                                 "Megahertz" = A.qchfile.select.iso.mhz,
                                 "Gauss" = A.qchfile.select.iso.gauss)
  }
  #
  ## ## delete original variable which is not required anymore
  rm(qchfile) ## instead of `qchfile <- NULL`
  #
  ## ================ PROCESSING (REARRANGEMENT) of `table.select.A` ==================
  #
  ## Own function to rearrange A/a according to `nuclei.list.slct`
  ## Build up new rearranged data frame for Nuclei A/a:
  data.slct.nucs.group <- data.frame(
    No = integer(), Nucleus = character(),
    MegaHertz = double(), Gauss = double(), NuclearGroup = character())
  #
  ## TBC TBC !!!! from `rearrange_aAiso_QCHcomput` function
  #
}
