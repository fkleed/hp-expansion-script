# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")


# Read living space and load profile data
ls_building_types <-
  read_csv("data/output/livingspacebuildingtypes/ls_building_types.csv") %>% mutate_if(is.character, as.factor)

ls_one_and_two_family_houses <- ls_building_types[1, 2]$LivingSpace
ls_row_and_semi_detached_houses <- ls_building_types[2, 2]$LivingSpace
ls_ab_36 <- ls_building_types[4, 2]$LivingSpace
ls_ab_7more <- ls_building_types[5, 2]$LivingSpace

eh_loadprofile_ashp_reference <-
  read_csv2("data/output/loadprofile/eh_loadprofile_ashp_reference.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_reference",
      "ElectricityDemand_1919_1948_reference",
      "ElectricityDemand_1949_1978_reference",
      "ElectricityDemand_1979_1986_reference",
      "ElectricityDemand_1987_1990_reference",
      "ElectricityDemand_1991_1995_reference",
      "ElectricityDemand_1996_2000_reference",
      "ElectricityDemand_2001_2011_reference",
      "ElectricityDemand_2012_2022_reference",
      "ElectricityDemand_2023_2030_reference"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_reference" = "ElectricityDemand_beginn_1918_reference",
    "EH_ElectricityDemand_1919_1948_reference" = "ElectricityDemand_1919_1948_reference",
    "EH_ElectricityDemand_1949_1978_reference" = "ElectricityDemand_1949_1978_reference",
    "EH_ElectricityDemand_1979_1986_reference" = "ElectricityDemand_1979_1986_reference",
    "EH_ElectricityDemand_1987_1990_reference" = "ElectricityDemand_1987_1990_reference",
    "EH_ElectricityDemand_1991_1995_reference" = "ElectricityDemand_1991_1995_reference",
    "EH_ElectricityDemand_1996_2000_reference" = "ElectricityDemand_1996_2000_reference",
    "EH_ElectricityDemand_2001_2011_reference" = "ElectricityDemand_2001_2011_reference",
    "EH_ElectricityDemand_2012_2022_reference" = "ElectricityDemand_2012_2022_reference",
    "EH_ElectricityDemand_2023_2030_reference" = "ElectricityDemand_2023_2030_reference"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_ashp_reference <-
  read_csv2("data/output/loadprofile/mh_loadprofile_ashp_reference.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_reference",
      "ElectricityDemand_1919_1948_reference",
      "ElectricityDemand_1949_1978_reference",
      "ElectricityDemand_1979_1986_reference",
      "ElectricityDemand_1987_1990_reference",
      "ElectricityDemand_1991_1995_reference",
      "ElectricityDemand_1996_2000_reference",
      "ElectricityDemand_2001_2011_reference",
      "ElectricityDemand_2012_2022_reference",
      "ElectricityDemand_2023_2030_reference"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_reference" = "ElectricityDemand_beginn_1918_reference",
    "MH_ElectricityDemand_1919_1948_reference" = "ElectricityDemand_1919_1948_reference",
    "MH_ElectricityDemand_1949_1978_reference" = "ElectricityDemand_1949_1978_reference",
    "MH_ElectricityDemand_1979_1986_reference" = "ElectricityDemand_1979_1986_reference",
    "MH_ElectricityDemand_1987_1990_reference" = "ElectricityDemand_1987_1990_reference",
    "MH_ElectricityDemand_1991_1995_reference" = "ElectricityDemand_1991_1995_reference",
    "MH_ElectricityDemand_1996_2000_reference" = "ElectricityDemand_1996_2000_reference",
    "MH_ElectricityDemand_2001_2011_reference" = "ElectricityDemand_2001_2011_reference",
    "MH_ElectricityDemand_2012_2022_reference" = "ElectricityDemand_2012_2022_reference",
    "MH_ElectricityDemand_2023_2030_reference" = "ElectricityDemand_2023_2030_reference"
  ) %>% mutate_if(is.character, as.factor)



loadprofile_ashp_reference <- eh_loadprofile_ashp_reference %>%
  inner_join(
    mh_loadprofile_ashp_reference,
    by = c("Time" = "Time", "TemperatureKelvin" = "TemperatureKelvin")
  )

loadprofile_ashp_reference <- loadprofile_ashp_reference %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_ASHP" = EH_ElectricityDemand_beginn_1918_reference * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_ASHP" = EH_ElectricityDemand_1919_1948_reference * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_ASHP" = EH_ElectricityDemand_1949_1978_reference * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_ASHP" = EH_ElectricityDemand_1979_1986_reference * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_ASHP" = EH_ElectricityDemand_1987_1990_reference * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_ASHP" = EH_ElectricityDemand_1991_1995_reference * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_ASHP" = EH_ElectricityDemand_1996_2000_reference * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_ASHP" = EH_ElectricityDemand_2001_2011_reference * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_ASHP" = EH_ElectricityDemand_2012_2022_reference * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_ASHP" = EH_ElectricityDemand_2023_2030_reference * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_ASHP" = EH_ElectricityDemand_beginn_1918_reference * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_ASHP" = EH_ElectricityDemand_1919_1948_reference * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_ASHP" = EH_ElectricityDemand_1949_1978_reference * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_ASHP" = EH_ElectricityDemand_1979_1986_reference * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_ASHP" = EH_ElectricityDemand_1987_1990_reference * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_ASHP" = EH_ElectricityDemand_1991_1995_reference * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_ASHP" = EH_ElectricityDemand_1996_2000_reference * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_ASHP" = EH_ElectricityDemand_2001_2011_reference * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_ASHP" = EH_ElectricityDemand_2012_2022_reference * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_ASHP" = EH_ElectricityDemand_2023_2030_reference * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_ASHP" = MH_ElectricityDemand_beginn_1918_reference * ls_ab_36,
    "ApartmentBuildings36_1919_1948_ASHP" = MH_ElectricityDemand_1919_1948_reference * ls_ab_36,
    "ApartmentBuildings36_1949_1978_ASHP" = MH_ElectricityDemand_1949_1978_reference * ls_ab_36,
    "ApartmentBuildings36_1979_1986_ASHP" = MH_ElectricityDemand_1979_1986_reference * ls_ab_36,
    "ApartmentBuildings36_1987_1990_ASHP" = MH_ElectricityDemand_1987_1990_reference * ls_ab_36,
    "ApartmentBuildings36_1991_1995_ASHP" = MH_ElectricityDemand_1991_1995_reference * ls_ab_36,
    "ApartmentBuildings36_1996_2000_ASHP" = MH_ElectricityDemand_1996_2000_reference * ls_ab_36,
    "ApartmentBuildings36_2001_2011_ASHP" = MH_ElectricityDemand_2001_2011_reference * ls_ab_36,
    "ApartmentBuildings36_2012_2022_ASHP" = MH_ElectricityDemand_2012_2022_reference * ls_ab_36,
    "ApartmentBuildings36_2023_2030_ASHP" = MH_ElectricityDemand_2023_2030_reference * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_ASHP" = MH_ElectricityDemand_beginn_1918_reference * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_ASHP" = MH_ElectricityDemand_1919_1948_reference * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_ASHP" = MH_ElectricityDemand_1949_1978_reference * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_ASHP" = MH_ElectricityDemand_1979_1986_reference * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_ASHP" = MH_ElectricityDemand_1987_1990_reference * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_ASHP" = MH_ElectricityDemand_1991_1995_reference * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_ASHP" = MH_ElectricityDemand_1996_2000_reference * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_ASHP" = MH_ElectricityDemand_2001_2011_reference * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_ASHP" = MH_ElectricityDemand_2012_2022_reference * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_ASHP" = MH_ElectricityDemand_2023_2030_reference * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_reference,
      EH_ElectricityDemand_1919_1948_reference,
      EH_ElectricityDemand_1949_1978_reference,
      EH_ElectricityDemand_1979_1986_reference,
      EH_ElectricityDemand_1987_1990_reference,
      EH_ElectricityDemand_1991_1995_reference,
      EH_ElectricityDemand_1996_2000_reference,
      EH_ElectricityDemand_2001_2011_reference,
      EH_ElectricityDemand_2012_2022_reference,
      EH_ElectricityDemand_2023_2030_reference,
      MH_ElectricityDemand_beginn_1918_reference,
      MH_ElectricityDemand_1919_1948_reference,
      MH_ElectricityDemand_1949_1978_reference,
      MH_ElectricityDemand_1979_1986_reference,
      MH_ElectricityDemand_1987_1990_reference,
      MH_ElectricityDemand_1991_1995_reference,
      MH_ElectricityDemand_1996_2000_reference,
      MH_ElectricityDemand_2001_2011_reference,
      MH_ElectricityDemand_2012_2022_reference,
      MH_ElectricityDemand_2023_2030_reference
    )
  )

summary(loadprofile_ashp_reference)
