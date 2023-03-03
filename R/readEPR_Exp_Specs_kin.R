# reading epr spectra from kinetics
readEPR_Exp_Specs_kin <- function(pattern,
                                  path_to_ASC,
                                  path_to_DSC_or_par,
                                  origin = "xenon",
                                  qValue = NULL){
  #
  ## file name pattern which has to be the same for `ASC`+`DSC`
  ## or `.spc` and `.par`
  file.name.pattern <- pattern
  #
  ## path to `asc` file
  if (origin == "xenon"){
    path.to.asc <- file.path(path_to_ASC,
                             paste0(file.name.pattern,".txt"))
  }
  if (origin == "winepr"){
    path.to.asc <- file.path(path_to_ASC,
                             paste0(file.name.pattern,".asc"))
  }
  #
  ## path to `DSC` or `par`
  if (origin == "xenon"){
    path.to.dsc.par <- file.path(path_to_DSC_or_par,
                                 paste0(file.name.pattern,".DSC"))
  }
  if (origin == "winepr"){
    path.to.dsc.par <- file.path(path_to_DSC_or_par,
                                 paste0(file.name.pattern,".par"))
  }
  #
  ## Qvalue
  if (origin == "xenon"){
    qValue.obtain <- readEPR_param_slct(path_to_DSC_or_par,string = "QValue")
  }
  if (origin == "winepr"){
    ## to obtain `QValue` run the following
    if (is.null(qValue)){
      cat(" 'qValue' is not provided. Please, define! ")
    } else{
      qValue.obtain <- qValue
    }
  }
  #
  ## 'Kinetic' instrum. params
  instrument.params.kinet <- readEPR_params_slct_kin(path_to_DSC_or_par,origin = origin)
  #
  ## Load spectral data
  data.spectra.time <- readEPR_Exp_Specs(path_to_ASC,
                                         qValue = qValue.obtain,
                                         time.series = T,
                                         origin = origin) %>%
    dplyr::filter(dIepr_over_dB != 0) %>% ## only non-zero intensities selected
    dplyr::mutate(time_s = time_correct_EPR_Specs(time.s = time_s,
                                                  Nscans = instrument.params.kinet$Nscans,
                                                  sweep.time.s = instrument.params.kinet$sweepTime))
  #
  ## corrected time
  time.corrected <- data.spectra.time %>%
    dplyr::group_by(time_s) %>%
    dplyr::group_keys()
  #
  data.all.spectra <- list(spectra = data.spectra.time,time = time.corrected)
  #
  return(data.all.spectra)
  #
}
