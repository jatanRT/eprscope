#'
#' Evaluating Full Width at Half-Maximum (FWHM) from Spectra
#'
#'
#' @description
#' A short description...
#'
#'
#'
#' @param data.spec.integ tbc
#' @param x tbc
#' @param Intensity tbc
#' @param xlim tbc
#'
#'
#' @return tbc
#'
#'
#' @examples
#' \dontrun{
#' tbc
#' }
#'
#'
#' @export
#'
#'
eval_FWHMx_Spec <- function(data.spec.integ,
                            x = "B_G",
                            Intensity = "single_integ",
                            xlim = NULL){
  ## 'Temporary' processing variables
  # NO
  #
  ## Define limits
  if (is.null(xlim)){
    ## the entire data region
    xlim <- c(min(data.spec.integ[[x]]),max(data.spec.integ[[x]]))
  } else{
    ## otherwise use predefined vector
    xlim = xlim
  }
  # ======= This is not required, however it's better to select a narrow region ======
  ## see also `which.min` function below
  ## Selecting `x region`
  ## variable set as `xs.init` (data frame) +
  ## Condition to find x values for near(max(`Intensity`)/2)
  xs.init <- data.spec.integ %>%
    dplyr::filter(dplyr::between(.data[[x]],
                                 xlim[1],xlim[2])) %>%
    dplyr::filter(dplyr::near(.data[[Intensity]],
                       max(.data[[Intensity]])/2,
                       tol = max(.data[[Intensity]])/64))
  # ==================================================================================
  #
  ## calculate `x.max` corresponding to max(Intensity)
  x.max <- data.spec.integ  %>%
    dplyr::filter(dplyr::between(.data[[x]],xlim[1],xlim[2]))  %>%
    dplyr::filter(.data[[Intensity]] == max(.data[[Intensity]]))  %>%
    dplyr::pull(.data[[x]])
  #
  ## set the condition for the closest values individually
  ## for x < `x.max` & x > `x.max`
  if (xs.init[[x]] < x.max){
    ## filter x values
    x.init.low <- xs.init %>%
      dplyr::filter(.data[[x]] < x.max)
    ## intensity condition
    Intens.cond <- which.min(abs(x.init.low[[Intensity]]-max(x.init.low[[Intensity]])/2))
    ## finding x
    x.init.low <- x.init.low %>%
      dplyr::filter(.data[[Intensity]] == Intens.cond) %>%
      dplyr::pull(.data[[x]])
  }
  if (xs.init[[x]] > x.max){
    ## filter x values
    x.init.high <- xs.init %>%
      dplyr::filter(.data[[x]] > x.max)
    ## intensity condition
    Intens.cond <- which.min(abs(x.init.high[[Intensity]]-max(x.init.high[[Intensity]])/2))
    ## finding x
    x.init.high <- x.init.high %>%
      dplyr::filter(.data[[Intensity]] == Intens.cond) %>%
      dplyr::pull(.data[[x]])
  }
  #
  FWHM <- round(abs(x.init.low - x.init.high),digits = 4)
  #
  return(FWHM)
  #
}
