#'
#' Evaluating Full Width at Half-Maximum (FWHM) from Spectra
#'
#'
#' @family Evaluations
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
                            Intensity = "single_Integ",
                            xlim = NULL){
  #
  ## 'Temporary' processing variables
  . <- NULL
  #
  ## Define limits if `xlim = NULL` take the entire data region
  ## otherwise use predefined vector
  data.x.region <- c(min(data.spec.integ[[x]]),max(data.spec.integ[[x]]))
  xlim <- xlim %>% `if`(is.null(xlim),data.x.region, .)
  #
  # ===== This is not required, however it's better to select a narrow region at the beginning =====
  #
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
  #
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
  ## however individual values must be compared => create a loop
  ## for all `x` of the `xs.init`
  for (p in seq(xs.init[[x]])){
    if (xs.init[[x]][p] < x.max){
      ## filter x values
      x.init.low <- xs.init %>%
        dplyr::filter(.data[[x]] < x.max)
      ## intensity condition by `which.min` and results in indices (res. one line df)
      ## => it is just like dplyr filtering, therefore
      Intens.cond.left <- which.min(abs(x.init.low[[Intensity]]-max(data.spec.integ[[Intensity]])/2))
      Intens.cond.left <- x.init.low[Intens.cond.left] %>% dplyr::pull(.data[[Intensity]])
      ## finding x
      x.init.low <- x.init.low %>%
        dplyr::filter(.data[[Intensity]] == Intens.cond.left) %>%
        dplyr::pull(.data[[x]])
    }
    if (xs.init[[x]][p] > x.max){
      ## filter x values
      x.init.high <- xs.init %>%
        dplyr::filter(.data[[x]] > x.max)
      ## intensity condition by `which.min` and results in indices (res. one line df)
      ## => it is just like dplyr filtering, therefore
      Intens.cond.right <- which.min(abs(x.init.high[[Intensity]]-max(data.spec.integ[[Intensity]])/2))
      Intens.cond.right <- x.init.high[Intens.cond.right] %>% dplyr::pull(.data[[Intensity]])
      ## finding x
      x.init.high <- x.init.high %>%
        dplyr::filter(.data[[Intensity]] == Intens.cond.right) %>%
        dplyr::pull(.data[[x]])
    }
    #
  }
  #
  FWHM <- round(abs(x.init.low - x.init.high),digits = 3)
  #
  return(FWHM)
  #
}
