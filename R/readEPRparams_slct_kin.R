
# read parameters from kinet. experiments
readEPRparams_slct_kin <- function(path_to_DSC_or_par,origin = "xenon"){
  ## Load all required parameters from `.DSC` or `.par`
  if (origin == "xenon"){
    resol <- readEPRparam_slct(path_to_DSC_or_par,string = "A1RS")
    convTime <- readEPRparam_slct()
  }
}
