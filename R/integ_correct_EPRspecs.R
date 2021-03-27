#
integ_correct_EPRspecs <- function(spec.integ.data,
                                   B = "B_G",
                                   B.reg.start,
                                   B.reg.end,
                                   B.peak.start,
                                   B.peak.end,
                                   poly.degree,
                                   double.integ.val = FALSE){
  ## Intensity column from spe.integ.data
  integ.string <- str_subset(colnames(spec.integ.data),
                             regex("single|sinteg|s_integ|single_|singleinteg|sintegral|sInteg_",
                                   ignore_case = T))
  ## select a region / range / interval of a integrated spectrum
  ## in which the second integral will be performed
  ## (limits are 'B.reg.start','B.reg.end'):
  data.slct <- spec.integ.data %>%
    filter(between(.data[[B]],B.reg.start,B.reg.end))
  ## select region / range / interval of the peak, which will be not
  ## considered ("!") for the baseline correction / fit
  ## (limits are 'B.peak.start','B.peak.end'):
  data.NoPeak <- data.slct %>%
    filter(!between(.data[[B]],B.peak.start,B.peak.end))
  ## Polynomial baseline fit:
  integ.baseline.fit <- lm(.data[[integ.string]] ~ stats::poly(.data[[B]],degree = poly.degree),
                           data = data.NoPeak)
  ## apply fit to data.slct, remove the .resid colum (which is not required),
  ## rename column with fit, subtract the baseline,
  ## and finally shift the integral base up having all the values > 0 (subtract its minimum)
  data.slct <- broom::augment(integ.baseline.fit,newdata = data.slct) %>%
    dplyr::select(-.data[[".resid"]]) %>%
    dplyr::rename(sIntegBaseLinFit = .data[[".fitted"]]) %>%
    dplyr::mutate(sIntegCorr = .data[[integ.string]] - .data$sIntegBaseLinFit) %>%
    dplyr::select(-sIntegBaselinFit) %>%
    dplyr::mutate(sIntegCorr - min(.data$sIntegCorr))



}
