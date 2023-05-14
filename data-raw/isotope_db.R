## code to prepare `isotope_db` dataset goes here
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
isotope.db.raw.path <- system.file("extdata",
                                   "isotopes_DB.txt",
                                   package = "eprscope")
#
## database taken from `easyspin` and slightly modified its format
#
## pre-processing database into package
isotope_db <- fread(isotope.db.raw.path,
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
  dplyr::mutate(nu_ENDOR_MHz_035T = - (1/Planck.const) *
                  g_Nuclear * nuclear.mu * 0.35 *
                  1e-6)
#
## save resulting data frame as an `.rda` file
usethis::use_data(isotope_db, compress = "xz", overwrite = TRUE)
