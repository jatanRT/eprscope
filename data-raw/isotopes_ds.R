## code to prepare `isotopes_ds` dataset goes here
#
## Required libraries
library(dplyr)
library(constants)
library(data.table)
#
##important physical constants
Planck.const <- constants::syms$h
nuclear.mu <- constants::syms$mun ## Nuclear magneton
#
## path to table in `.txt` format
isotopes.ds.raw.path <- system.file("extdata",
                                    "isotopes_DS.txt",
                                    package = "eprscope")
#
## dataset taken from `easyspin` and slightly modified its format
#
## pre-processing database into package
isotopes_ds <- fread(isotopes.ds.raw.path,
                    sep = "auto",
                    skip = 47,
                    col.names = c("No_Proton",
                                  "Isotope",
                                  "Stability",
                                  "Name",
                                  "Spin",
                                  "g_Nuclear",
                                  "Abund_Natur_Percent",
                                  "Q_Barn")) %>%
  ## adding the ENDOR frequencies (in MHz) at 0.35 T
  dplyr::mutate(nu_ENDOR_MHz_035T = - round(((1/Planck.const) *
                  g_Nuclear * nuclear.mu * 0.35 *
                  1e-6),digits = 3))
#
## save resulting data frame as an `.rda` file
usethis::use_data(isotopes_ds, compress = "xz", overwrite = TRUE)
