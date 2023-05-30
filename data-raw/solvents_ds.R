## code to prepare `solvents_ds` dataset goes here
#
## Required libraries
library(dplyr)
library(rvest)
#
## import the `url` of solvents dataset from American Chemical
## Society/Division of Organic Chemistry (online source)
solvent.link <- "https://organicchemistrydata.org/solvents/"
#
## read the table by `rvest`
solvent.page <- rvest::read_html(solvent.link)
solvents_ds <- solvent.page %>%
  rvest::html_node("table") %>%
  rvest::html_table() %>%
  ## replace "--" and "??" by "NA"
  dplyr::mutate(dplyr::across(c(2,3,7,8,9), na_if, "--"))
solvents_ds[solvents_ds == "??"] <- NA
#
## rename columns
names(solvents_ds) <- c("Solvent",
                       "Formula",
                       "MW",
                       "Boiling_Point_oC",
                       "Melting_Point_oC",
                       "Density_gmL",
                       "Solubility_g100gW",
                       "Dielectric_Const",
                       "Flash_Point_oC")
## convert columns
solvents_ds <- solvents_ds %>%
  dplyr::mutate(MW = as.numeric(MW),
                Boiling_Point_oC = as.numeric(Boiling_Point_oC),
                Melting_Point_oC = as.numeric(Melting_Point_oC),
                Density_gmL = as.numeric(Density_gmL),
                Flash_Point_oC = as.numeric(Flash_Point_oC))
#
## solvent viscosities (in cp) at 20°C from
## https://www.sigmaaldrich.com/deepweb/assets/sigmaaldrich/marketing/global/documents/614/456/labbasics_pg144.pdf
## + PubChem (https://pubchem.ncbi.nlm.nih.gov/) => viscosities at 20°C or 25°C
## + "ACCU DYNE TEST" https://www.accudynetest.com/visc_table.html#014 viscosities at 20°C, 25°C and 30°C
solvents_ds$Viscosity_cp <- c("1.31(25)","0.32","0.37","0.65",
                             "2.95","2.54","0.42(15)","2.25",
                             "0.97","0.80","0.58","0.98",
                             "0.79","0.30","0.24(48)","0.46(25)",
                             "1.1","0.92","2.47","1.44(15)",
                             "1.10(25)","0.46","16.1","934",
                             "0.42",NA,NA,"0.31",
                             "0.55",NA,"0.45(15)","1.65",
                             "0.67","0.24",NA,"2.26",
                             "2.86(15)","0.95","0.55","0.59",
                             "0.347","1.00","1.107","0.81",
                             "0.581","0.65")
#
## save resulting data frame as an `.rda` file
usethis::use_data(solvents_ds,compress = "xz", overwrite = TRUE)
