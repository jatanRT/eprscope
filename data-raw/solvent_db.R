## code to prepare `solvent_db` dataset goes here
#
## Required libraries
library(dplyr)
library(rvest)
#
## import the `url` of solvents database from American Chemical
## Society/Division of Organic Chemistry (online source)
solvent.link <- "https://organicchemistrydata.org/solvents/"
#
## read the table by `rvest`
solvent.page <- rvest::read_html(solvent.link)
solvent_db <- solvent.page %>%
  rvest::html_node("table") %>%
  rvest::html_table() %>%
  ## replace "--" and "??" by "NA"
  dplyr::mutate(dplyr::across(c(2,3,7,8,9), na_if, "--"))
solvent_db[solvent_db == "??"] <- NA
#
## rename columns
names(solvent_db) <- c("Solvent",
                       "Formula",
                       "MW",
                       "Boiling_Point_oC",
                       "Melting_Point_oC",
                       "Density_gmL",
                       "Solubility",
                       "Dielectric_Const",
                       "Flash_Point_oC")
#
## save resulting data frame as an `.rda` file
usethis::use_data(solvent_db,compress = "xz", overwrite = TRUE)
