#
#' Read all Parameters (incl. the Simulation ones) from \emph{MATLAB} \code{.mat} File
#'
#'
#' @family Data Reading
#'
#'
#' @description
#' Function is based on the \code{\link[R.matlab]{readMat}} and provides the content reading of a `.mat`
#' simulation file from MATLAB incl. structures/variables and fields. It can be also used to read and store
#' simulated EPR spectrum in the form of `data frame`.
#'
#'
#' @param path_to_MAT Character string, path to \code{.mat} MATLAB file with all variables saved in workspace.
#'   The file path can be also defined by \code{\link[base]{file.path}}.
#' @param str.var Character string, \code{structure/variable}, which may contain \code{fields}, e.g. like \code{Sys}
#'   and \code{g} => \strong{Sys}.g, respectively. \strong{Default}: \code{str.var = NULL}.
#' @param field.var Character string, \code{field variable after 'dot'}, which is available only for certain
#'   structures/variables, see e.g. example above (Sys.\strong{g}), therefore
#'   the \strong{default} 'value' is \code{NULL} and the \code{string} \strong{is applied only for structures with fields}.
#'
#'
#' @return Unless the \code{str.var} and/or \code{field.var} are not specified, the output is \code{list} with all original
#'   parameters/structures from MATLAB file. Otherwise, the function returns either numeric or character \code{vector/value},
#'   depending on `class` of the original parameter/field variable.
#'
#'
#' @examples
#' ## loading the package built-in
#' ## `Aminoxyl_radical_a.mat` file as an example
#' aminoxyl.mat.file <- load_data_example(file = "Aminoxyl_radical_a.mat")
#' #
#' ## reading the entire `mat` file as list and assign variable
#' aminoxyl.mat.list <- readMAT_params_file(aminoxyl.mat.file)
#' #
#' ## read the `Sim1` structure/variable content
#' aminoxyl.mat.sim1 <- readMAT_params_file(aminoxyl.mat.file,
#'                                          str.var = "Sim1")
#' ## preview
#' aminoxyl.mat.sim1
#' #
#' ## read the `Sim1` structure/variable and the field `Nucs`
#' ## corresponding the nuclei considered in the EPR simulation
#' aminoxyl.mat.sim1.nucs <-
#'   readMAT_params_file(aminoxyl.mat.file,
#'                       str.var = "Sim1",
#'                       field.var = "Nucs")
#' ## preview
#' aminoxyl.mat.sim1.nucs
#' #
#' ## reading the magnetic flux density `B.G` column/vector
#' ## corresponding to simulated and experimental EPR spectrum
#' aminoxyl.B.G <- readMAT_params_file(aminoxyl.mat.file,
#'                                     str.var = "B")
#' ## preview of the first 6 values
#' aminoxyl.B.G[1:6]
#' #
#' ## reading the intensity related to simulated EPR spectrum
#' aminoxyl.sim.fitSpec <-
#'   readMAT_params_file(aminoxyl.mat.file,
#'                       str.var = "fit1",
#'                       field.var = "fitSpec")
#' ## preview of the first 6 values
#' aminoxyl.sim.fitSpec[1:6]
#' #
#' ## The last two examples can be used to load the simulated
#' ## EPR spectrum by the `EasySpin` from `mat` file =>
#' simulation.aminoxyl.spectr.df <-
#'   data.frame(aminoxyl.B.G,aminoxyl.sim.fitSpec)
#' ## preview
#' head(simulation.aminoxyl.spectr.df)
#'
#'
#' @export
#'
#'
#' @importFrom R.matlab readMat
readMAT_params_file <- function(path_to_MAT,
                                str.var = NULL,
                                field.var = NULL) {
  if (is.null(str.var)){
    ## list
    data.params <- R.matlab::readMat(path_to_MAT)
    return(data.params)
  } else{
    data.params <- R.matlab::readMat(path_to_MAT)
    if (is.null(field.var)) {
      params <- data.params[[str.var]]
      #
      if (inherits(params,"list") || inherits(params,"array") ||
          inherits(params,"matrix")){
        ## this is a list and take it's names
        names(params) <- rownames(params)
        ## and finally the `rownames` are not required anymore
        rownames(params) <- NULL
        ## convert params into list
        params <- list(params)
        ## in only one value is available convert it into vector
        ## otherwise into vector
        if (length(params) == 1){
          params <- params[[1]]
        } else{
          params <- params
        }
      }
      #
    } else {
      params <- data.params[[str.var]][, , 1][[field.var]]
      ## if the field is character converted it into vector
      ## of strings
      params <- as.vector(params)
      if (inherits(params[1],"character")){
        params <- params[1] %>%
          stringr::str_split(pattern = ",|\\s+") %>%
          unlist()
      }
      ## params is a matrix and convert it into vector
      params <- as.vector(params)
    }
    #
    return(params)
    #
  }
  #
}
