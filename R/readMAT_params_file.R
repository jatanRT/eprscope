#
#' Reading EPR Simulation Parameters and Information from the \emph{MATLAB} \code{.mat} File
#'
#'
#' @family Data Reading
#'
#'
#' @description
#'   Function is based on the \code{\link[R.matlab]{readMat}} and provides the reading of a \code{.mat}
#'   simulation file content from \emph{EasySpin} MATLAB, including structures/variables and fields.
#'   It can be also used to read and store simulated EPR spectrum in the form of R data frame (see \code{Examples}).
#'
#'
#' @param path_to_MAT Character string, path to \code{.mat} MATLAB file with all variables saved in MATLAB workspace.
#'   The file path can be also defined by the \code{\link[base]{file.path}}.
#' @param str.var Character string, denoting structure/variable, which may contain \code{fields}, such as \code{Sys}
#'   and \code{g} => \strong{Sys}.g, respectively. \strong{Default}: \code{str.var = NULL}.
#' @param field.var Character string, corresponding to field variable after 'dot', which is available only for certain
#'   structures/variables, see e.g. example above (Sys.\strong{g}). Therefore,
#'   the \strong{default} value is \code{NULL} and the \code{string} \strong{is applied only for structures with fields}.
#'
#'
#' @return Unless the \code{str.var} and/or \code{field.var} are not specified, the output is \code{list} with the all original
#'   parameters/structures from MATLAB file. Otherwise, the function returns either numeric/character \code{vector/value}
#'   or list depending on \code{class} of the original parameter/field variable.
#'
#'
#' @examples
#' ## loading the package built-in
#' ## `Aminoxyl_radical_a.mat` file as an example
#' aminoxyl.mat.file <-
#'   load_data_example(file = "Aminoxyl_radical_a.mat")
#' #
#' ## reading the entire `mat` file as list
#' ## and assign variable
#' aminoxyl.mat.list <-
#'   readMAT_params_file(aminoxyl.mat.file)
#' #
#' ## read the `Sim1` structure/variable content into list
#' aminoxyl.mat.sim1 <-
#'   readMAT_params_file(aminoxyl.mat.file,
#'                       str.var = "Sim1")
#' #
#' ## list preview
#' aminoxyl.mat.sim1
#' #
#' ## compare the previous simulation parameters with
#' ## those obtained by the `eval_sim_EPR_isoFit()`
#' ## function (look at the corresponding examples)
#' #
#' ## alternatively the `Sim1` (its dimension > 2)
#' ## can be also read by the following command
#' ## however, the returned output has a complex
#' ## array-list structure
#' aminoxyl.mat.list$Sim1[, , 1]
#' #
#' ## read the `Sim1` structure/variable
#' ## and the field `Nucs` corresponding to nuclei
#' ## considered in the EPR simulation
#' aminoxyl.mat.sim1.nucs <-
#'   readMAT_params_file(aminoxyl.mat.file,
#'                       str.var = "Sim1",
#'                       field.var = "Nucs")
#' #
#' ## preview
#' aminoxyl.mat.sim1.nucs
#' #
#' ## reading the magnetic flux density
#' ## `B` column/vector corresponding to simulated
#' ## and experimental EPR spectrum
#' aminoxyl.B.G <-
#'   readMAT_params_file(aminoxyl.mat.file,
#'                       str.var = "B")
#' #
#' ## preview of the first 6 values
#' aminoxyl.B.G[1:6]
#' #
#' ## reading the intensity related to simulated
#' ## EPR spectrum
#' aminoxyl.sim.fitSpec <-
#'   readMAT_params_file(aminoxyl.mat.file,
#'                       str.var = "fit1",
#'                       field.var = "fitSpec")
#' #
#' ## for the newer EasySpin version (> 6.0.0),
#' ## the "fitSpec" is replaced by the simple "fit"
#' ## or "fitraw", corresponding to scaled and raw intensity
#' ## of the simulated EPR spectrum, please refer also
#' ## to the EasySpin documentantion:
#' ## https://easyspin.org/easyspin/documentation/esfit.html
#' #
#' ## preview of the first 6 values
#' aminoxyl.sim.fitSpec[1:6]
#' #
#' ## The last two examples can be used
#' ## to load the simulated EPR spectrum
#' ## by the `EasySpin` from `mat` file =>
#' simulation.aminoxyl.spectr.df <-
#'   data.frame(Bsim_G = aminoxyl.B.G,
#'              dIeprSim_over_dB = aminoxyl.sim.fitSpec)
#' #
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
      ## dimension (required fro additional processing) of `params`
      params.dim <- dim(params)
      #
      if (inherits(params,"list") || inherits(params,"array") ||
          inherits(params,"matrix")){
        #
        if (length(params.dim) > 2){
          params <- params[, , 1]
          ## this is a list and convert all its components into vectors
          params <- lapply(params, function(x) c(x))
        }
        if (length(params.dim) <= 2){
          ## convert it into vector
          params <- c(params)
        }
        #
      } else{
        params <- params
      }
      #
    } else {
      params <- data.params[[str.var]][, , 1][[field.var]]
      #
      ## dimension and the condition
      params.dim <- dim(params)
      if (length(params.dim) > 2){
        ## convert it into list
        params <- params[, ,1]
        ## convert all its components into vectors
        params <- lapply(params, function(x) c(x))
      }
      if (length(params.dim) <= 2)
      ## convert it into vector
      params <- c(params)
      #
    }
    #
    return(params)
    #
  }
  #
}
