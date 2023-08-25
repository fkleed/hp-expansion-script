# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")


# Read living space
ls_building_types <-
  read_csv("data/output/livingspacebuildingtypes/ls_building_types.csv") %>% mutate_if(is.character, as.factor)

ls_one_and_two_family_houses <- ls_building_types[1, 2]$LivingSpace
ls_row_and_semi_detached_houses <-
  ls_building_types[2, 2]$LivingSpace
ls_ab_36 <- ls_building_types[4, 2]$LivingSpace
ls_ab_7more <- ls_building_types[5, 2]$LivingSpace


# Read load profile data and calculate the load profile of the building types

# Space heat and hot water for reference year
eh_loadprofile_ashp_cold <-
  read_csv2("data/output/loadprofile/eh_loadprofile_ashp_cold.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "EH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "EH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "EH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "EH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "EH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "EH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "EH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "EH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "EH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_ashp_cold <-
  read_csv2("data/output/loadprofile/mh_loadprofile_ashp_cold.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "MH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "MH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "MH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "MH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "MH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "MH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "MH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "MH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "MH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_ashp_cold <- eh_loadprofile_ashp_cold %>%
  inner_join(
    mh_loadprofile_ashp_cold,
    by = c("Time" = "Time", "TemperatureKelvin" = "TemperatureKelvin")
  )

loadprofile_ashp_cold <- loadprofile_ashp_cold %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_ASHP" = EH_ElectricityDemand_beginn_1918_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_ASHP" = EH_ElectricityDemand_1919_1948_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_ASHP" = EH_ElectricityDemand_1949_1978_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_ASHP" = EH_ElectricityDemand_1979_1986_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_ASHP" = EH_ElectricityDemand_1987_1990_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_ASHP" = EH_ElectricityDemand_1991_1995_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_ASHP" = EH_ElectricityDemand_1996_2000_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_ASHP" = EH_ElectricityDemand_2001_2011_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_ASHP" = EH_ElectricityDemand_2012_2022_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_ASHP" = EH_ElectricityDemand_2023_2030_cold * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_ASHP" = EH_ElectricityDemand_beginn_1918_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_ASHP" = EH_ElectricityDemand_1919_1948_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_ASHP" = EH_ElectricityDemand_1949_1978_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_ASHP" = EH_ElectricityDemand_1979_1986_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_ASHP" = EH_ElectricityDemand_1987_1990_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_ASHP" = EH_ElectricityDemand_1991_1995_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_ASHP" = EH_ElectricityDemand_1996_2000_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_ASHP" = EH_ElectricityDemand_2001_2011_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_ASHP" = EH_ElectricityDemand_2012_2022_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_ASHP" = EH_ElectricityDemand_2023_2030_cold * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_ASHP" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_36,
    "ApartmentBuildings36_1919_1948_ASHP" = MH_ElectricityDemand_1919_1948_cold * ls_ab_36,
    "ApartmentBuildings36_1949_1978_ASHP" = MH_ElectricityDemand_1949_1978_cold * ls_ab_36,
    "ApartmentBuildings36_1979_1986_ASHP" = MH_ElectricityDemand_1979_1986_cold * ls_ab_36,
    "ApartmentBuildings36_1987_1990_ASHP" = MH_ElectricityDemand_1987_1990_cold * ls_ab_36,
    "ApartmentBuildings36_1991_1995_ASHP" = MH_ElectricityDemand_1991_1995_cold * ls_ab_36,
    "ApartmentBuildings36_1996_2000_ASHP" = MH_ElectricityDemand_1996_2000_cold * ls_ab_36,
    "ApartmentBuildings36_2001_2011_ASHP" = MH_ElectricityDemand_2001_2011_cold * ls_ab_36,
    "ApartmentBuildings36_2012_2022_ASHP" = MH_ElectricityDemand_2012_2022_cold * ls_ab_36,
    "ApartmentBuildings36_2023_2030_ASHP" = MH_ElectricityDemand_2023_2030_cold * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_ASHP" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_ASHP" = MH_ElectricityDemand_1919_1948_cold * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_ASHP" = MH_ElectricityDemand_1949_1978_cold * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_ASHP" = MH_ElectricityDemand_1979_1986_cold * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_ASHP" = MH_ElectricityDemand_1987_1990_cold * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_ASHP" = MH_ElectricityDemand_1991_1995_cold * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_ASHP" = MH_ElectricityDemand_1996_2000_cold * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_ASHP" = MH_ElectricityDemand_2001_2011_cold * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_ASHP" = MH_ElectricityDemand_2012_2022_cold * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_ASHP" = MH_ElectricityDemand_2023_2030_cold * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_cold,
      EH_ElectricityDemand_1919_1948_cold,
      EH_ElectricityDemand_1949_1978_cold,
      EH_ElectricityDemand_1979_1986_cold,
      EH_ElectricityDemand_1987_1990_cold,
      EH_ElectricityDemand_1991_1995_cold,
      EH_ElectricityDemand_1996_2000_cold,
      EH_ElectricityDemand_2001_2011_cold,
      EH_ElectricityDemand_2012_2022_cold,
      EH_ElectricityDemand_2023_2030_cold,
      MH_ElectricityDemand_beginn_1918_cold,
      MH_ElectricityDemand_1919_1948_cold,
      MH_ElectricityDemand_1949_1978_cold,
      MH_ElectricityDemand_1979_1986_cold,
      MH_ElectricityDemand_1987_1990_cold,
      MH_ElectricityDemand_1991_1995_cold,
      MH_ElectricityDemand_1996_2000_cold,
      MH_ElectricityDemand_2001_2011_cold,
      MH_ElectricityDemand_2012_2022_cold,
      MH_ElectricityDemand_2023_2030_cold
    )
  )


eh_loadprofile_gshp_collector_cold <-
  read_csv2("data/output/loadprofile/eh_loadprofile_gshp_collector_cold.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "SoilTemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "EH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "EH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "EH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "EH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "EH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "EH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "EH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "EH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "EH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_gshp_collector_cold <-
  read_csv2("data/output/loadprofile/mh_loadprofile_gshp_collector_cold.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "SoilTemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "MH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "MH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "MH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "MH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "MH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "MH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "MH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "MH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "MH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_gshp_collector_cold <-
  eh_loadprofile_gshp_collector_cold %>%
  inner_join(
    mh_loadprofile_gshp_collector_cold,
    by = c(
      "Time" = "Time",
      "TemperatureKelvin" = "TemperatureKelvin",
      "SoilTemperatureKelvin" = "SoilTemperatureKelvin"
    )
  )

loadprofile_gshp_collector_cold <-
  loadprofile_gshp_collector_cold %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_GSHP_Collector" = EH_ElectricityDemand_beginn_1918_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_GSHP_Collector" = EH_ElectricityDemand_1919_1948_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_GSHP_Collector" = EH_ElectricityDemand_1949_1978_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_GSHP_Collector" = EH_ElectricityDemand_1979_1986_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_GSHP_Collector" = EH_ElectricityDemand_1987_1990_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_GSHP_Collector" = EH_ElectricityDemand_1991_1995_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_GSHP_Collector" = EH_ElectricityDemand_1996_2000_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_GSHP_Collector" = EH_ElectricityDemand_2001_2011_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_GSHP_Collector" = EH_ElectricityDemand_2012_2022_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_GSHP_Collector" = EH_ElectricityDemand_2023_2030_cold * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_GSHP_Collector" = EH_ElectricityDemand_beginn_1918_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_GSHP_Collector" = EH_ElectricityDemand_1919_1948_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_GSHP_Collector" = EH_ElectricityDemand_1949_1978_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_GSHP_Collector" = EH_ElectricityDemand_1979_1986_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_GSHP_Collector" = EH_ElectricityDemand_1987_1990_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_GSHP_Collector" = EH_ElectricityDemand_1991_1995_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_GSHP_Collector" = EH_ElectricityDemand_1996_2000_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_GSHP_Collector" = EH_ElectricityDemand_2001_2011_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_GSHP_Collector" = EH_ElectricityDemand_2012_2022_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_GSHP_Collector" = EH_ElectricityDemand_2023_2030_cold * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_GSHP_Collector" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_36,
    "ApartmentBuildings36_1919_1948_GSHP_Collector" = MH_ElectricityDemand_1919_1948_cold * ls_ab_36,
    "ApartmentBuildings36_1949_1978_GSHP_Collector" = MH_ElectricityDemand_1949_1978_cold * ls_ab_36,
    "ApartmentBuildings36_1979_1986_GSHP_Collector" = MH_ElectricityDemand_1979_1986_cold * ls_ab_36,
    "ApartmentBuildings36_1987_1990_GSHP_Collector" = MH_ElectricityDemand_1987_1990_cold * ls_ab_36,
    "ApartmentBuildings36_1991_1995_GSHP_Collector" = MH_ElectricityDemand_1991_1995_cold * ls_ab_36,
    "ApartmentBuildings36_1996_2000_GSHP_Collector" = MH_ElectricityDemand_1996_2000_cold * ls_ab_36,
    "ApartmentBuildings36_2001_2011_GSHP_Collector" = MH_ElectricityDemand_2001_2011_cold * ls_ab_36,
    "ApartmentBuildings36_2012_2022_GSHP_Collector" = MH_ElectricityDemand_2012_2022_cold * ls_ab_36,
    "ApartmentBuildings36_2023_2030_GSHP_Collector" = MH_ElectricityDemand_2023_2030_cold * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_GSHP_Collector" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_GSHP_Collector" = MH_ElectricityDemand_1919_1948_cold * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_GSHP_Collector" = MH_ElectricityDemand_1949_1978_cold * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_GSHP_Collector" = MH_ElectricityDemand_1979_1986_cold * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_GSHP_Collector" = MH_ElectricityDemand_1987_1990_cold * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_GSHP_Collector" = MH_ElectricityDemand_1991_1995_cold * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_GSHP_Collector" = MH_ElectricityDemand_1996_2000_cold * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_GSHP_Collector" = MH_ElectricityDemand_2001_2011_cold * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_GSHP_Collector" = MH_ElectricityDemand_2012_2022_cold * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_GSHP_Collector" = MH_ElectricityDemand_2023_2030_cold * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_cold,
      EH_ElectricityDemand_1919_1948_cold,
      EH_ElectricityDemand_1949_1978_cold,
      EH_ElectricityDemand_1979_1986_cold,
      EH_ElectricityDemand_1987_1990_cold,
      EH_ElectricityDemand_1991_1995_cold,
      EH_ElectricityDemand_1996_2000_cold,
      EH_ElectricityDemand_2001_2011_cold,
      EH_ElectricityDemand_2012_2022_cold,
      EH_ElectricityDemand_2023_2030_cold,
      MH_ElectricityDemand_beginn_1918_cold,
      MH_ElectricityDemand_1919_1948_cold,
      MH_ElectricityDemand_1949_1978_cold,
      MH_ElectricityDemand_1979_1986_cold,
      MH_ElectricityDemand_1987_1990_cold,
      MH_ElectricityDemand_1991_1995_cold,
      MH_ElectricityDemand_1996_2000_cold,
      MH_ElectricityDemand_2001_2011_cold,
      MH_ElectricityDemand_2012_2022_cold,
      MH_ElectricityDemand_2023_2030_cold
    )
  ) %>% rename("SoilTemperatureKelvinCollector" = "SoilTemperatureKelvin")


eh_loadprofile_gshp_probe_cold <-
  read_csv2("data/output/loadprofile/eh_loadprofile_gshp_probe_cold.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "EH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "EH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "EH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "EH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "EH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "EH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "EH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "EH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "EH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_gshp_probe_cold <-
  read_csv2("data/output/loadprofile/mh_loadprofile_gshp_probe_cold.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "MH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "MH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "MH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "MH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "MH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "MH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "MH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "MH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "MH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_gshp_probe_cold <-
  eh_loadprofile_gshp_probe_cold %>%
  inner_join(
    mh_loadprofile_gshp_probe_cold,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  )


loadprofile_gshp_probe_cold <-
  loadprofile_gshp_probe_cold %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_GSHP_Probe" = EH_ElectricityDemand_beginn_1918_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_GSHP_Probe" = EH_ElectricityDemand_1919_1948_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_GSHP_Probe" = EH_ElectricityDemand_1949_1978_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_GSHP_Probe" = EH_ElectricityDemand_1979_1986_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_GSHP_Probe" = EH_ElectricityDemand_1987_1990_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_GSHP_Probe" = EH_ElectricityDemand_1991_1995_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_GSHP_Probe" = EH_ElectricityDemand_1996_2000_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_GSHP_Probe" = EH_ElectricityDemand_2001_2011_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_GSHP_Probe" = EH_ElectricityDemand_2012_2022_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_GSHP_Probe" = EH_ElectricityDemand_2023_2030_cold * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_GSHP_Probe" = EH_ElectricityDemand_beginn_1918_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_GSHP_Probe" = EH_ElectricityDemand_1919_1948_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_GSHP_Probe" = EH_ElectricityDemand_1949_1978_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_GSHP_Probe" = EH_ElectricityDemand_1979_1986_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_GSHP_Probe" = EH_ElectricityDemand_1987_1990_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_GSHP_Probe" = EH_ElectricityDemand_1991_1995_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_GSHP_Probe" = EH_ElectricityDemand_1996_2000_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_GSHP_Probe" = EH_ElectricityDemand_2001_2011_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_GSHP_Probe" = EH_ElectricityDemand_2012_2022_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_GSHP_Probe" = EH_ElectricityDemand_2023_2030_cold * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_GSHP_Probe" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_36,
    "ApartmentBuildings36_1919_1948_GSHP_Probe" = MH_ElectricityDemand_1919_1948_cold * ls_ab_36,
    "ApartmentBuildings36_1949_1978_GSHP_Probe" = MH_ElectricityDemand_1949_1978_cold * ls_ab_36,
    "ApartmentBuildings36_1979_1986_GSHP_Probe" = MH_ElectricityDemand_1979_1986_cold * ls_ab_36,
    "ApartmentBuildings36_1987_1990_GSHP_Probe" = MH_ElectricityDemand_1987_1990_cold * ls_ab_36,
    "ApartmentBuildings36_1991_1995_GSHP_Probe" = MH_ElectricityDemand_1991_1995_cold * ls_ab_36,
    "ApartmentBuildings36_1996_2000_GSHP_Probe" = MH_ElectricityDemand_1996_2000_cold * ls_ab_36,
    "ApartmentBuildings36_2001_2011_GSHP_Probe" = MH_ElectricityDemand_2001_2011_cold * ls_ab_36,
    "ApartmentBuildings36_2012_2022_GSHP_Probe" = MH_ElectricityDemand_2012_2022_cold * ls_ab_36,
    "ApartmentBuildings36_2023_2030_GSHP_Probe" = MH_ElectricityDemand_2023_2030_cold * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_GSHP_Probe" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_GSHP_Probe" = MH_ElectricityDemand_1919_1948_cold * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_GSHP_Probe" = MH_ElectricityDemand_1949_1978_cold * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_GSHP_Probe" = MH_ElectricityDemand_1979_1986_cold * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_GSHP_Probe" = MH_ElectricityDemand_1987_1990_cold * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_GSHP_Probe" = MH_ElectricityDemand_1991_1995_cold * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_GSHP_Probe" = MH_ElectricityDemand_1996_2000_cold * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_GSHP_Probe" = MH_ElectricityDemand_2001_2011_cold * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_GSHP_Probe" = MH_ElectricityDemand_2012_2022_cold * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_GSHP_Probe" = MH_ElectricityDemand_2023_2030_cold * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_cold,
      EH_ElectricityDemand_1919_1948_cold,
      EH_ElectricityDemand_1949_1978_cold,
      EH_ElectricityDemand_1979_1986_cold,
      EH_ElectricityDemand_1987_1990_cold,
      EH_ElectricityDemand_1991_1995_cold,
      EH_ElectricityDemand_1996_2000_cold,
      EH_ElectricityDemand_2001_2011_cold,
      EH_ElectricityDemand_2012_2022_cold,
      EH_ElectricityDemand_2023_2030_cold,
      MH_ElectricityDemand_beginn_1918_cold,
      MH_ElectricityDemand_1919_1948_cold,
      MH_ElectricityDemand_1949_1978_cold,
      MH_ElectricityDemand_1979_1986_cold,
      MH_ElectricityDemand_1987_1990_cold,
      MH_ElectricityDemand_1991_1995_cold,
      MH_ElectricityDemand_1996_2000_cold,
      MH_ElectricityDemand_2001_2011_cold,
      MH_ElectricityDemand_2012_2022_cold,
      MH_ElectricityDemand_2023_2030_cold
    )
  ) %>% mutate("SoilTemperatureKelvinProbe" = 283.15)


loadprofile_cold <- loadprofile_ashp_cold %>%
  inner_join(
    loadprofile_gshp_collector_cold,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  ) %>% inner_join(
    loadprofile_gshp_probe_cold,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  ) %>% relocate(SoilTemperatureKelvinCollector,
                 .after = TemperatureKelvin) %>% relocate(SoilTemperatureKelvinProbe,
                                                          .after = SoilTemperatureKelvinCollector)


# Only space heat for reference year
eh_loadprofile_ashp_space_heat_only_cold <-
  read_csv2("data/output/loadprofile/eh_loadprofile_ashp_space_heat_only_cold.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "EH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "EH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "EH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "EH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "EH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "EH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "EH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "EH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "EH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_ashp_space_heat_only_cold <-
  read_csv2("data/output/loadprofile/mh_loadprofile_ashp_space_heat_only_cold.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "MH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "MH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "MH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "MH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "MH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "MH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "MH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "MH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "MH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_ashp_space_heat_only_cold <-
  eh_loadprofile_ashp_space_heat_only_cold %>%
  inner_join(
    mh_loadprofile_ashp_space_heat_only_cold,
    by = c("Time" = "Time", "TemperatureKelvin" = "TemperatureKelvin")
  )

loadprofile_ashp_space_heat_only_cold <-
  loadprofile_ashp_space_heat_only_cold %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_ASHP" = EH_ElectricityDemand_beginn_1918_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_ASHP" = EH_ElectricityDemand_1919_1948_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_ASHP" = EH_ElectricityDemand_1949_1978_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_ASHP" = EH_ElectricityDemand_1979_1986_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_ASHP" = EH_ElectricityDemand_1987_1990_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_ASHP" = EH_ElectricityDemand_1991_1995_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_ASHP" = EH_ElectricityDemand_1996_2000_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_ASHP" = EH_ElectricityDemand_2001_2011_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_ASHP" = EH_ElectricityDemand_2012_2022_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_ASHP" = EH_ElectricityDemand_2023_2030_cold * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_ASHP" = EH_ElectricityDemand_beginn_1918_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_ASHP" = EH_ElectricityDemand_1919_1948_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_ASHP" = EH_ElectricityDemand_1949_1978_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_ASHP" = EH_ElectricityDemand_1979_1986_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_ASHP" = EH_ElectricityDemand_1987_1990_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_ASHP" = EH_ElectricityDemand_1991_1995_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_ASHP" = EH_ElectricityDemand_1996_2000_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_ASHP" = EH_ElectricityDemand_2001_2011_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_ASHP" = EH_ElectricityDemand_2012_2022_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_ASHP" = EH_ElectricityDemand_2023_2030_cold * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_ASHP" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_36,
    "ApartmentBuildings36_1919_1948_ASHP" = MH_ElectricityDemand_1919_1948_cold * ls_ab_36,
    "ApartmentBuildings36_1949_1978_ASHP" = MH_ElectricityDemand_1949_1978_cold * ls_ab_36,
    "ApartmentBuildings36_1979_1986_ASHP" = MH_ElectricityDemand_1979_1986_cold * ls_ab_36,
    "ApartmentBuildings36_1987_1990_ASHP" = MH_ElectricityDemand_1987_1990_cold * ls_ab_36,
    "ApartmentBuildings36_1991_1995_ASHP" = MH_ElectricityDemand_1991_1995_cold * ls_ab_36,
    "ApartmentBuildings36_1996_2000_ASHP" = MH_ElectricityDemand_1996_2000_cold * ls_ab_36,
    "ApartmentBuildings36_2001_2011_ASHP" = MH_ElectricityDemand_2001_2011_cold * ls_ab_36,
    "ApartmentBuildings36_2012_2022_ASHP" = MH_ElectricityDemand_2012_2022_cold * ls_ab_36,
    "ApartmentBuildings36_2023_2030_ASHP" = MH_ElectricityDemand_2023_2030_cold * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_ASHP" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_ASHP" = MH_ElectricityDemand_1919_1948_cold * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_ASHP" = MH_ElectricityDemand_1949_1978_cold * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_ASHP" = MH_ElectricityDemand_1979_1986_cold * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_ASHP" = MH_ElectricityDemand_1987_1990_cold * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_ASHP" = MH_ElectricityDemand_1991_1995_cold * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_ASHP" = MH_ElectricityDemand_1996_2000_cold * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_ASHP" = MH_ElectricityDemand_2001_2011_cold * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_ASHP" = MH_ElectricityDemand_2012_2022_cold * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_ASHP" = MH_ElectricityDemand_2023_2030_cold * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_cold,
      EH_ElectricityDemand_1919_1948_cold,
      EH_ElectricityDemand_1949_1978_cold,
      EH_ElectricityDemand_1979_1986_cold,
      EH_ElectricityDemand_1987_1990_cold,
      EH_ElectricityDemand_1991_1995_cold,
      EH_ElectricityDemand_1996_2000_cold,
      EH_ElectricityDemand_2001_2011_cold,
      EH_ElectricityDemand_2012_2022_cold,
      EH_ElectricityDemand_2023_2030_cold,
      MH_ElectricityDemand_beginn_1918_cold,
      MH_ElectricityDemand_1919_1948_cold,
      MH_ElectricityDemand_1949_1978_cold,
      MH_ElectricityDemand_1979_1986_cold,
      MH_ElectricityDemand_1987_1990_cold,
      MH_ElectricityDemand_1991_1995_cold,
      MH_ElectricityDemand_1996_2000_cold,
      MH_ElectricityDemand_2001_2011_cold,
      MH_ElectricityDemand_2012_2022_cold,
      MH_ElectricityDemand_2023_2030_cold
    )
  )


eh_loadprofile_gshp_collector_space_heat_only_cold <-
  read_csv2(
    "data/output/loadprofile/eh_loadprofile_gshp_collector_space_heat_only_cold.csv"
  ) %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "SoilTemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "EH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "EH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "EH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "EH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "EH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "EH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "EH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "EH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "EH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_gshp_collector_space_heat_only_cold <-
  read_csv2(
    "data/output/loadprofile/mh_loadprofile_gshp_collector_space_heat_only_cold.csv"
  ) %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "SoilTemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "MH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "MH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "MH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "MH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "MH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "MH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "MH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "MH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "MH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_gshp_collector_space_heat_only_cold <-
  eh_loadprofile_gshp_collector_space_heat_only_cold %>%
  inner_join(
    mh_loadprofile_gshp_collector_space_heat_only_cold,
    by = c(
      "Time" = "Time",
      "TemperatureKelvin" = "TemperatureKelvin",
      "SoilTemperatureKelvin" = "SoilTemperatureKelvin"
    )
  )

loadprofile_gshp_collector_space_heat_only_cold <-
  loadprofile_gshp_collector_space_heat_only_cold %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_GSHP_Collector" = EH_ElectricityDemand_beginn_1918_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_GSHP_Collector" = EH_ElectricityDemand_1919_1948_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_GSHP_Collector" = EH_ElectricityDemand_1949_1978_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_GSHP_Collector" = EH_ElectricityDemand_1979_1986_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_GSHP_Collector" = EH_ElectricityDemand_1987_1990_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_GSHP_Collector" = EH_ElectricityDemand_1991_1995_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_GSHP_Collector" = EH_ElectricityDemand_1996_2000_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_GSHP_Collector" = EH_ElectricityDemand_2001_2011_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_GSHP_Collector" = EH_ElectricityDemand_2012_2022_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_GSHP_Collector" = EH_ElectricityDemand_2023_2030_cold * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_GSHP_Collector" = EH_ElectricityDemand_beginn_1918_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_GSHP_Collector" = EH_ElectricityDemand_1919_1948_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_GSHP_Collector" = EH_ElectricityDemand_1949_1978_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_GSHP_Collector" = EH_ElectricityDemand_1979_1986_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_GSHP_Collector" = EH_ElectricityDemand_1987_1990_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_GSHP_Collector" = EH_ElectricityDemand_1991_1995_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_GSHP_Collector" = EH_ElectricityDemand_1996_2000_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_GSHP_Collector" = EH_ElectricityDemand_2001_2011_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_GSHP_Collector" = EH_ElectricityDemand_2012_2022_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_GSHP_Collector" = EH_ElectricityDemand_2023_2030_cold * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_GSHP_Collector" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_36,
    "ApartmentBuildings36_1919_1948_GSHP_Collector" = MH_ElectricityDemand_1919_1948_cold * ls_ab_36,
    "ApartmentBuildings36_1949_1978_GSHP_Collector" = MH_ElectricityDemand_1949_1978_cold * ls_ab_36,
    "ApartmentBuildings36_1979_1986_GSHP_Collector" = MH_ElectricityDemand_1979_1986_cold * ls_ab_36,
    "ApartmentBuildings36_1987_1990_GSHP_Collector" = MH_ElectricityDemand_1987_1990_cold * ls_ab_36,
    "ApartmentBuildings36_1991_1995_GSHP_Collector" = MH_ElectricityDemand_1991_1995_cold * ls_ab_36,
    "ApartmentBuildings36_1996_2000_GSHP_Collector" = MH_ElectricityDemand_1996_2000_cold * ls_ab_36,
    "ApartmentBuildings36_2001_2011_GSHP_Collector" = MH_ElectricityDemand_2001_2011_cold * ls_ab_36,
    "ApartmentBuildings36_2012_2022_GSHP_Collector" = MH_ElectricityDemand_2012_2022_cold * ls_ab_36,
    "ApartmentBuildings36_2023_2030_GSHP_Collector" = MH_ElectricityDemand_2023_2030_cold * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_GSHP_Collector" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_GSHP_Collector" = MH_ElectricityDemand_1919_1948_cold * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_GSHP_Collector" = MH_ElectricityDemand_1949_1978_cold * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_GSHP_Collector" = MH_ElectricityDemand_1979_1986_cold * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_GSHP_Collector" = MH_ElectricityDemand_1987_1990_cold * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_GSHP_Collector" = MH_ElectricityDemand_1991_1995_cold * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_GSHP_Collector" = MH_ElectricityDemand_1996_2000_cold * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_GSHP_Collector" = MH_ElectricityDemand_2001_2011_cold * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_GSHP_Collector" = MH_ElectricityDemand_2012_2022_cold * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_GSHP_Collector" = MH_ElectricityDemand_2023_2030_cold * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_cold,
      EH_ElectricityDemand_1919_1948_cold,
      EH_ElectricityDemand_1949_1978_cold,
      EH_ElectricityDemand_1979_1986_cold,
      EH_ElectricityDemand_1987_1990_cold,
      EH_ElectricityDemand_1991_1995_cold,
      EH_ElectricityDemand_1996_2000_cold,
      EH_ElectricityDemand_2001_2011_cold,
      EH_ElectricityDemand_2012_2022_cold,
      EH_ElectricityDemand_2023_2030_cold,
      MH_ElectricityDemand_beginn_1918_cold,
      MH_ElectricityDemand_1919_1948_cold,
      MH_ElectricityDemand_1949_1978_cold,
      MH_ElectricityDemand_1979_1986_cold,
      MH_ElectricityDemand_1987_1990_cold,
      MH_ElectricityDemand_1991_1995_cold,
      MH_ElectricityDemand_1996_2000_cold,
      MH_ElectricityDemand_2001_2011_cold,
      MH_ElectricityDemand_2012_2022_cold,
      MH_ElectricityDemand_2023_2030_cold
    )
  ) %>% rename("SoilTemperatureKelvinCollector" = "SoilTemperatureKelvin")

eh_loadprofile_gshp_probe_space_heat_only_cold <-
  read_csv2(
    "data/output/loadprofile/eh_loadprofile_gshp_probe_space_heat_only_cold.csv"
  ) %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "EH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "EH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "EH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "EH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "EH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "EH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "EH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "EH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "EH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_gshp_probe_space_heat_only_cold <-
  read_csv2(
    "data/output/loadprofile/mh_loadprofile_gshp_probe_space_heat_only_cold.csv"
  ) %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_cold",
      "ElectricityDemand_1919_1948_cold",
      "ElectricityDemand_1949_1978_cold",
      "ElectricityDemand_1979_1986_cold",
      "ElectricityDemand_1987_1990_cold",
      "ElectricityDemand_1991_1995_cold",
      "ElectricityDemand_1996_2000_cold",
      "ElectricityDemand_2001_2011_cold",
      "ElectricityDemand_2012_2022_cold",
      "ElectricityDemand_2023_2030_cold"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_cold" = "ElectricityDemand_beginn_1918_cold",
    "MH_ElectricityDemand_1919_1948_cold" = "ElectricityDemand_1919_1948_cold",
    "MH_ElectricityDemand_1949_1978_cold" = "ElectricityDemand_1949_1978_cold",
    "MH_ElectricityDemand_1979_1986_cold" = "ElectricityDemand_1979_1986_cold",
    "MH_ElectricityDemand_1987_1990_cold" = "ElectricityDemand_1987_1990_cold",
    "MH_ElectricityDemand_1991_1995_cold" = "ElectricityDemand_1991_1995_cold",
    "MH_ElectricityDemand_1996_2000_cold" = "ElectricityDemand_1996_2000_cold",
    "MH_ElectricityDemand_2001_2011_cold" = "ElectricityDemand_2001_2011_cold",
    "MH_ElectricityDemand_2012_2022_cold" = "ElectricityDemand_2012_2022_cold",
    "MH_ElectricityDemand_2023_2030_cold" = "ElectricityDemand_2023_2030_cold"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_gshp_probe_space_heat_only_cold <-
  eh_loadprofile_gshp_probe_space_heat_only_cold %>%
  inner_join(
    mh_loadprofile_gshp_probe_space_heat_only_cold,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  )


loadprofile_gshp_probe_space_heat_only_cold <-
  loadprofile_gshp_probe_space_heat_only_cold %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_GSHP_Probe" = EH_ElectricityDemand_beginn_1918_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_GSHP_Probe" = EH_ElectricityDemand_1919_1948_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_GSHP_Probe" = EH_ElectricityDemand_1949_1978_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_GSHP_Probe" = EH_ElectricityDemand_1979_1986_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_GSHP_Probe" = EH_ElectricityDemand_1987_1990_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_GSHP_Probe" = EH_ElectricityDemand_1991_1995_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_GSHP_Probe" = EH_ElectricityDemand_1996_2000_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_GSHP_Probe" = EH_ElectricityDemand_2001_2011_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_GSHP_Probe" = EH_ElectricityDemand_2012_2022_cold * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_GSHP_Probe" = EH_ElectricityDemand_2023_2030_cold * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_GSHP_Probe" = EH_ElectricityDemand_beginn_1918_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_GSHP_Probe" = EH_ElectricityDemand_1919_1948_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_GSHP_Probe" = EH_ElectricityDemand_1949_1978_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_GSHP_Probe" = EH_ElectricityDemand_1979_1986_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_GSHP_Probe" = EH_ElectricityDemand_1987_1990_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_GSHP_Probe" = EH_ElectricityDemand_1991_1995_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_GSHP_Probe" = EH_ElectricityDemand_1996_2000_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_GSHP_Probe" = EH_ElectricityDemand_2001_2011_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_GSHP_Probe" = EH_ElectricityDemand_2012_2022_cold * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_GSHP_Probe" = EH_ElectricityDemand_2023_2030_cold * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_GSHP_Probe" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_36,
    "ApartmentBuildings36_1919_1948_GSHP_Probe" = MH_ElectricityDemand_1919_1948_cold * ls_ab_36,
    "ApartmentBuildings36_1949_1978_GSHP_Probe" = MH_ElectricityDemand_1949_1978_cold * ls_ab_36,
    "ApartmentBuildings36_1979_1986_GSHP_Probe" = MH_ElectricityDemand_1979_1986_cold * ls_ab_36,
    "ApartmentBuildings36_1987_1990_GSHP_Probe" = MH_ElectricityDemand_1987_1990_cold * ls_ab_36,
    "ApartmentBuildings36_1991_1995_GSHP_Probe" = MH_ElectricityDemand_1991_1995_cold * ls_ab_36,
    "ApartmentBuildings36_1996_2000_GSHP_Probe" = MH_ElectricityDemand_1996_2000_cold * ls_ab_36,
    "ApartmentBuildings36_2001_2011_GSHP_Probe" = MH_ElectricityDemand_2001_2011_cold * ls_ab_36,
    "ApartmentBuildings36_2012_2022_GSHP_Probe" = MH_ElectricityDemand_2012_2022_cold * ls_ab_36,
    "ApartmentBuildings36_2023_2030_GSHP_Probe" = MH_ElectricityDemand_2023_2030_cold * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_GSHP_Probe" = MH_ElectricityDemand_beginn_1918_cold * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_GSHP_Probe" = MH_ElectricityDemand_1919_1948_cold * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_GSHP_Probe" = MH_ElectricityDemand_1949_1978_cold * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_GSHP_Probe" = MH_ElectricityDemand_1979_1986_cold * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_GSHP_Probe" = MH_ElectricityDemand_1987_1990_cold * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_GSHP_Probe" = MH_ElectricityDemand_1991_1995_cold * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_GSHP_Probe" = MH_ElectricityDemand_1996_2000_cold * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_GSHP_Probe" = MH_ElectricityDemand_2001_2011_cold * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_GSHP_Probe" = MH_ElectricityDemand_2012_2022_cold * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_GSHP_Probe" = MH_ElectricityDemand_2023_2030_cold * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_cold,
      EH_ElectricityDemand_1919_1948_cold,
      EH_ElectricityDemand_1949_1978_cold,
      EH_ElectricityDemand_1979_1986_cold,
      EH_ElectricityDemand_1987_1990_cold,
      EH_ElectricityDemand_1991_1995_cold,
      EH_ElectricityDemand_1996_2000_cold,
      EH_ElectricityDemand_2001_2011_cold,
      EH_ElectricityDemand_2012_2022_cold,
      EH_ElectricityDemand_2023_2030_cold,
      MH_ElectricityDemand_beginn_1918_cold,
      MH_ElectricityDemand_1919_1948_cold,
      MH_ElectricityDemand_1949_1978_cold,
      MH_ElectricityDemand_1979_1986_cold,
      MH_ElectricityDemand_1987_1990_cold,
      MH_ElectricityDemand_1991_1995_cold,
      MH_ElectricityDemand_1996_2000_cold,
      MH_ElectricityDemand_2001_2011_cold,
      MH_ElectricityDemand_2012_2022_cold,
      MH_ElectricityDemand_2023_2030_cold
    )
  ) %>% mutate("SoilTemperatureKelvinProbe" = 283.15)

loadprofile_space_heat_only_cold <-
  loadprofile_ashp_space_heat_only_cold %>%
  inner_join(
    loadprofile_gshp_collector_space_heat_only_cold,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  ) %>% inner_join(
    loadprofile_gshp_probe_space_heat_only_cold,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  ) %>% relocate(SoilTemperatureKelvinCollector,
                 .after = TemperatureKelvin) %>% relocate(SoilTemperatureKelvinProbe,
                                                          .after = SoilTemperatureKelvinCollector)


# Write output to csv
write_csv(
  loadprofile_cold,
  "data/output/electricityconsumptionbuildingtypes/loadprofile_cold.csv"
)
write_csv(
  loadprofile_space_heat_only_cold,
  "data/output/electricityconsumptionbuildingtypes/loadprofile_space_heat_only_cold.csv"
)
