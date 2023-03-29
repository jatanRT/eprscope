#
#' Integration of EPR Spectral Data for Quantitative Analysis
#'
#'
#' @description
#'  Evaluates integrated EPR spectrum with option to correct baseline by polynomial of \code{poly.degree}
#'  grade. Single-integrated spectrum (from original derivative form) can be obtained
#'  by \code{\link[pracma:trapz]{pracma::cumtrapz}} function
#'  taken into account \emph{B} and \emph{dIepr_over_dB}. Double Integral Calculation/Presentation
#'  may be also provided by this function, if \code{double.integ = T}
#'
#'
#' @param data.spectrum Spectrum data frame/table with magnetic flux density (in \code{mT} or \code{G}))
#'   and that of the derivative or already single integrated intensity. \code{Index} column may be already presented as well.
#' @param B Character/String pointing to magnetic flux density \code{column} (in the original \code{data.spectrum})
#'   either in \code{millitesla} or in \code{Gauss}, that is \code{B = "B_mT"} or \code{B = "BField"}...etc
#'   or \code{B = "B_G"} (\strong{default}). If
#' @param Intensity Character/String pointing to \code{column} of either derivative
#'   (e.g. \code{Intensity = "dIepr_over_dB"}, \strong{default}) or single integrated EPR
#'   spectrum (e.g. \code{Intensity = "single_Integrated"}) within the actual data frame \code{data.spectrum}.
#' @param B.unit Character/String
#' @param Blim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `Blim = c(3495.4,3595.4)`. \strong{Default}: \code{Blim = NULL}
#'   (corresponding to entire `B` range).
#' @param BpeaKlim Numeric vector, magnetic flux density in \code{mT}/\code{G} corresponding to border limits
#'   of the selected \eqn{B} region, e.g. like `BpeaKlim = c(3535.4,3555.4)`.
#' @param poly.degree Numeric, degree of polynomial function used to fit the baseline under the single integrated
#'   curve of the original EPR spectrum.
#' @param double.integ Logical, whether to present (column in data frame) the double integral of \emph{dIepr_over_dB},
#'   which is required for quantitative analysis, \strong{default}: \code{double.integ = FALSE}.
#'
#'
#' @return Data frame/table including the EPR spectral data (general \emph{Intensity}
#'   (integrated or derivative) \emph{vs} \eqn{B}) as well as its corresponding \code{single}
#'   (column \code{single_Integ}) and/or \code{double} (column \code{double_Integ} required
#'   for quantitative analysis). Single integrals (corresponding to either derivative or already
#'   integrated EPR spectra) can be optionally corrected against the polynomial baseline
#'   fit (column \code{single_Integ_correct}).
#'
#'
#'
#' @examples
#' \dontrun{
#' eval_integ_EPR_Spec(EPR_spectral_data_table,
#'                     B = "B_mT",
#'                     Blim = c(348.2,351.1),
#'                     BpeaKlim = c(349,350),
#'                     poly.degree = 3)
#' }
#'
#'
#' @export
#'
#'
#' @importFrom pracma cumtrapz
eval_integ_EPR_Spec <- function(data.spectrum,
                                B = "B_G",
                                B.unit = "G",
                                Intensity = "dIepr_over_dB",
                                Blim = NULL,
                                correct.integ = FALSE,
                                BpeaKlim = NULL,
                                poly.degree = NULL,
                                double.integ = FALSE) {
  ## 'Temporary' processing variables
  single_Integ <- NULL
  double_Integ <- NULL
  sIntegBaseLinFit <- NULL
  IntensBaseLinFit <- NULL
  single_Integ_correct <- NULL
  #
  ## Define limits if `Blim = NULL` take the entire data region
  ## otherwise use predefined vector
  Blim <- ifelse(is.null(Blim),c(min(data.spectrum[[B]]),max(data.spectrum[[B]])),Blim)
  #
  ## evaluating primary integral based on `Intensity`
  ## and `B` (`B.unit` has to be in "G") parameter
  ## otherwise each integration has to be multiplied by 10,
  ## because 1 mT = 10 G
  ## First of all define vectors with intensity column names =>
  deriv.EPR.intensities <- c("dB","_dB","intens","deriv","Intens",
                             "Deriv","dIepr","dIepr_over_dB","dIepr_dB",
                             "MW_Absorp")
  ## &
  integ.EPR.intensities <- c("single","Single","SInteg","sinteg","s_integ",
                             "single_","singleinteg","sintegral","integral",
                             "Integral","sInteg_","sInteg","singleI","integ","Integ")
  #
  if (sjmisc::str_contains(Intensity,deriv.EPR.intensities,logic = "or",ignore.case = F)){
    #
    ## primary data for integration
    data.spectrum <- data.spectrum %>%
      dplyr::filter(dplyr::between(.data[[B]],Blim[1],Blim[2]))
    #
    ## integration depending on `B` unit
    if (B.unit == "G"){
      data.spectrum <- data.spectrum %>%
        dplyr::mutate(single_Integ = pracma::cumtrapz(.data[[B]],.data[[Intensity]])[,1]) %>%
        { ifelse(isFALSE(double.integ), . ,
                 dplyr::mutate(double_Integ = pracma::cumtrapz(.data[[B]],.data$single_Integ)[,1])) }
    }
    if (B.unit == "mT"){
      data.spectrum <- data.spectrum %>%
        dplyr::mutate(single_Integ = pracma::cumtrapz(.data[[B]],.data[[Intensity]])[,1]*10) %>%
        { ifelse(isFALSE(double.integ), . ,
                 dplyr::mutate(double_Integ = pracma::cumtrapz(.data[[B]],.data$single_Integ)[,1]*10)) }
    }
  }
  if (sjmisc::str_contains(Intensity,integ.EPR.intensities, logic = "or",ignore.case = F)){
    #
    ## primary integrated data
    data.spectrum <- data.spectrum %>%
      dplyr::filter(dplyr::between(.data[[B]],Blim[1],Blim[2]))
    #
    ## integration depending on `B` unit
    if (B.unit == "G"){
      data.spectrum <- data.spectrum %>%
        { ifelse(isFALSE(double.integ), . ,
                 dplyr::mutate(double_Integ = pracma::cumtrapz(.data[[B]],.data[[Intensity]])[,1])) }
    }
    if (B.unit == "mT"){
      data.spectrum <- data.spectrum %>%
        { ifelse(isFALSE(double.integ), . ,
                 dplyr::mutate(double_Integ = pracma::cumtrapz(.data[[B]],.data[[Intensity]])[,1]*10)) }
    }
  }
  #
  ## Integral baseline correction
  if (isTRUE(correct.integ)){
    # select region / range / interval of the peak, which won't be
    ## considered ("!") for the baseline correction / fit
    ## (limits are 'BpeaKlim[1]'<=> 'start','BpeaKlim[2]' <=> 'end'):
    if (is.null(BpeaKlim)){
      stop(" The `B`-range for the peak baseline correction is not defined. Please, specify the range ! ")
    } else{
      data.NoPeak <- data.spectrum %>%
        filter(!between(.data[[B]], BpeaKlim[1], BpeaKlim[2]))
      if (is.null(poly.degree)){
        stop( " The degree of a polynomial to model the baseline is not defined. Please, specify ! " )
      } else{
        ## Polynomial baseline and integrate fit incl. derivative intensities =>
        if (sjmisc::str_contains(Intensity,deriv.EPR.intensities,logic = "or",ignore.case = F)){
          ## Polynomial baseline fit:
          integ.baseline.fit <- stats::lm(data.NoPeak$single_Integ ~ stats::poly(data.NoPeak[[B]],
                                                                                 degree = poly.degree),
                                          data = data.NoPeak)
          #
          ## apply fit to data.spectrum
          data.spectrum <- broom::augment(integ.baseline.fit, newdata = data.spectrum) %>%
            ## remove the .resid colum (which is not required),
            dplyr::select(-.data[[".resid"]]) %>%
            ## rename column with fit
            dplyr::rename(sIntegBaseLinFit = .data[[".fitted"]]) %>%
            ## subtract the baseline
            dplyr::mutate(single_Integ_correct = .data$single_Integ - .data$sIntegBaseLinFit) %>%
            ##  delete `sIntegBaseLinFit` which is not required anymore
            ## & shift the integral baseline up having all the values > 0 (subtract its minimum)
            dplyr::select(-sIntegBaseLinFit) %>%
            dplyr::mutate(single_Integ_correct = single_Integ_correct - min(.data$single_Integ_correct))
          #
          ## integration depending on `B` unit
          if (B.unit == "G"){
            data.spectrum <- data.spectrum %>%
              { ifelse(isFALSE(double.integ), . ,
                       dplyr::mutate(double_Integ = pracma::cumtrapz(.data[[B]],
                                                                     .data$single_Integ_correct)[,1])) }
          }
          if (B.unit == "mT"){
            data.spectrum <- data.spectrum %>%
              dplyr::mutate(single_Integ_correct = single_Integ_correct*10) %>%
              { ifelse(isFALSE(double.integ), . ,
                       dplyr::mutate(double_Integ = pracma::cumtrapz(.data[[B]],
                                                                     .data$single_Integ_correct)[,1]*10)) }
            #
          }
        }
        ## Polynomial baseline fit integrate incl. already single integrated intensities =>
        if (sjmisc::str_contains(Intensity,integ.EPR.intensities, logic = "or",ignore.case = F)){
          ## Polynomial baseline fit:
          integ.baseline.fit <- stats::lm(data.NoPeak[[Intensity]] ~ stats::poly(data.NoPeak[[B]],
                                                                                 degree = poly.degree),
                                          data = data.NoPeak)
          #
          ## apply fit to data.spectrum
          data.spectrum <- broom::augment(integ.baseline.fit, newdata = data.spectrum) %>%
            ## remove the .resid colum (which is not required),
            dplyr::select(-.data[[".resid"]]) %>%
            ## rename column with fit
            dplyr::rename(IntensBaseLinFit = .data[[".fitted"]]) %>%
            ## subtract the baseline
            dplyr::mutate(single_Integ_correct = .data[[Intensity]] - .data$IntensBaseLinFit) %>%
            ##  delete `IntensBaseLinFit` which is not required anymore
            ## & shift the integral baseline up having all the values > 0 (subtract its minimum)
            dplyr::select(-IntensBaseLinFit) %>%
            dplyr::mutate(single_Integ_correct = single_Integ_correct - min(.data$single_Integ_correct))
          #
          ## integration depending on `B` unit
          if (B.unit == "G"){
            data.spectrum <- data.spectrum %>%
              { ifelse(isFALSE(double.integ), . ,
                       dplyr::mutate(double_Integ = pracma::cumtrapz(.data[[B]],
                                                                     .data$single_Integ_correct)[,1])) }
          }
          if (B.unit == "mT"){
            data.spectrum <- data.spectrum %>%
              dplyr::mutate(single_Integ_correct = single_Integ_correct*10) %>%
              { ifelse(isFALSE(double.integ), . ,
                       dplyr::mutate(double_Integ = pracma::cumtrapz(.data[[B]],
                                                                     .data$single_Integ_correct)[,1]*10)) }
          }
        }
      }
    }
  }
  #
  return(data.spectrum)
  #
}
