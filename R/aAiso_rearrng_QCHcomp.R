#
#' @title TODO
#'
#'
#' @description TODO
#'
#'
#' @param path_to_ASC TODO
#' @param data.col.names TODO
#' @param nuclei.list.slct TODO
#'
#'
#' @return TODO
#'
#'
#' @examples
#' TODO
#' TODO
#'
#'
#' @export
#'
#'
#' @importFrom stringr str_subset str_extract
aAiso_rearrng_QCHcomp <- function(path_to_ASC,data.col.names,nuclei.list.slct){
  ## Conditions/Extraction for column names:
  ## use stringr::str_subset(...) or X[grepl(...)] or stringr::str_extract or grep(...,value = T)
  A.str <- str_subset(data.col.names,regex("mhz",ignore_case = T))
  a.str <- str_subset(data.col.names,regex("gauss|G|Gauss"))
  atomic.num.str <- str_subset(data.col.names,regex("No|Num|num|no"))
  nucl.str <- str_subset(data.col.names,regex("nuc",ignore_case = T))
  ## Read the data:
  data.Aa.comput <- data.table::fread(path_to_ASC,sep = "auto",header = F,skip = 1,
                                      col.names = data.col.names) %>%
    mutate(MegaHertz = abs(round(.data[[A.str]],digits = 3)),
           Gauss = abs(round(.data[[a.str]],digits = 2))) %>%
    select(c(.data[[atomic.num.str]],.data[[nucl.str]],.data$MegaHertz,.data$Gauss))
  ## Own function to rearrange A/a according to `nuclei.list.slct`
  ## Build up new rearranged data frame for Nuclei A/a:
  data.slct.nucs.group <- data.frame(No = integer(),Nucleus = character(),
                              MegaHertz = double(),Gauss = double(),NuclearGroup = character())
  ## cycle for each `nuclei.list.slct` component
  for (k in 1:length(nuclei.list.slct)){
    ## Filter atomic numbers from each part of the list:
    sal <- data.Aa.comput %>%
      filter(.data[[atomic.num.str]] %in% nuclei.list.slct[[k]])
    ## How many atoms/nuclei are included in each list part:
    how.many.nucs <- length(nuclei.list.slct[[k]])
    ## Extract atomic/nuclear label:
    mark.nucs <- str_extract(sal[[nucl.str]][1],"[[:alpha:]]+")
    ## Extract isotope number:
    nucleo.nucs <- str_extract(sal[[nucl.str]][1],"[[:digit:]]+")
    ## Extract atomic/nuclear numbers and collapse it into one string:
    num.nucs.str <- paste(sal[[atomic.num.str]],collapse = ",")
    ## Now combine all four variables by paste:
    group.nucs <- paste(how.many.nucs," x ",nucleo.nucs,mark.nucs," (",num.nucs.str,")",sep = "")
    ## Replicate the previous string variable corresponding to length of a list component/part:
    nuclear.group <- rep(group.nucs,length(nuclei.list.slct[[k]]))
    ## Build up data frame row by row:
    gal <- data.frame(sal,nuclear.group) ## at first combine by columns
    data.slct.nucs.group <- rbind(data.slct.nucs.group,gal)
  }
  ## Group by and summarize (find the mean value of each list component) according
  ## to `nuclei.list.slct` (symmetry) and previous `for loop`:
  data.slct.nucs.group <- data.slct.nucs.group %>%
    mutate(mT = .data$Gauss/10) %>%
    select(-c(.data$No,.data$Nucleus,.data$Gauss)) %>%
    dplyr::group_by(.data$NuclearGroup) %>%
    dplyr::summarize(A_MHz_QCH = round(mean(.data$MegaHertz),digits = 3),
                     a_mT_QCH = round(mean(.data$mT),digits = 2))
   ## Entire output table:
  return(data.slct.nucs.group)
}
