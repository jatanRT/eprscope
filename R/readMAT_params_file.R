#
#' Read all Parameters (incl. the Simulation ones) from \emph{MATLAB} \code{.mat} File
#'
#'
#' @description
#' Function is based on the \code{\link[R.matlab]{readMat}} and provides the content reading of a `.mat`
#' simulation file from MATLAB incl. structures/variables and fields. It can be also used to read and store
#' simulated EPR spectrum in the form of `data frame`.
#'
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
#' \dontrun{
#' ## reading the entire `mat` file as a list
#' readMAT_params_file("Sim_file.mat")
#' #
#' ## read the `Sim1` structure/variable content
#' readMAT_params_file("Sim_file.mat",
#'                     str.var = "Sim1")
#' #
#' ## read the `Sim1` structure/variable and the field `Nucs`
#' ## corresponding the nuclei considered in the EPR simulation
#' readMAT_params_file("Sim_file.mat",
#'                     str.var = "Sim1",
#'                     field.var = "Nucs")
#' #
#' ## reading the magnetic flux density `B.G` column/vector
#' ## corresponding to simulated and experimental EPR spectrum
#' B.G <- readMAT_params_file("Sim_file.mat",
#'                     str.var = "B.G")
#' #
#' ## reading the intensity related to simulated EPR spectrum
#' fitSpec <- readMAT_params_file("Sim_file.mat",
#'                     str.var = "fit1",
#'                     field.var = "fitSpec")
#' #
#' ## The last two examples can be used to load the simulated
#' ## EPR spectrum by the `EasySpin` from `mat` file =>
#' simulation.spectrum.df <- data.frame(B.G,fitSpec)
#' #
#' }
#'
#'
#' @export
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
      if (inherits(params,"list") || inherits(params,"array")){
        ## this is a list and take it's names
        names(params) <- rownames(params)
        ## and finally the `rownames` are not required anymore
        rownames(params) <- NULL
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
