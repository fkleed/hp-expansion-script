# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")


# Calculate the living space of different building types based on https://www.iwu.de/publikationen/fachinformationen/gebaeudetypologie/ p.18

# One- and Two-family Houses
ls_efh <- round(1463000000 / 9976000)

# Row Houses and semi-detached Houses
ls_rh_or_sh <- round(633000000 / 5030000)

# Apartment Buildings
# Calculate on a flat basis
ls_per_flat_mfh <- 1168000000 / 16495000
ls_per_flat_gmh <- 288000000 / 4674000

# Apartment Buildings (3-6)
ls_ab36 <- round(ls_per_flat_mfh * 4.5)

# Area Apartment Buildings: 7 and More Apartments
share_mfh712 <- 0.19
share_gmh <- 0.087

avg_apartments_mfh712 <- 9.5
avg_apartments_gmh <- 4674000 / 210000


avg_apartments_ab7more <-
  (share_mfh712 / (share_mfh712 + share_gmh)) * avg_apartments_mfh712 + (share_gmh / (share_mfh712 + share_gmh)) * avg_apartments_gmh

ls_ab7more <-
  floor(((share_mfh712 / (share_mfh712 + share_gmh)) * ls_per_flat_mfh + (share_gmh / (share_mfh712 + share_gmh)) * ls_per_flat_gmh
  ) * avg_apartments_ab7more)


# Create the tibble and write to csv
ls_building_types <- tibble(
  BuildingTypeSize = c(
    "One- and Two-family Houses",
    "Row Houses",
    "Semi-detached Houses",
    "Apartment Buildings (3-6)",
    "Apartment Buildings: 7 and More Apartments"
  ),
  LivingSpace = c(
    ls_efh,
    ls_rh_or_sh,
    ls_rh_or_sh,
    ls_ab36,
    ls_ab7more
  )
)

write_csv(
  ls_building_types,
  "data/output/livingspacebuildingtypes/ls_building_types.csv"
)
