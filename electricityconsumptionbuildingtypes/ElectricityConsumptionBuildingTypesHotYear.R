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

# Space heat and hot water for hot year
eh_loadprofile_ashp_hot <-
  read_csv2("data/output/loadprofile/eh_loadprofile_ashp_hot.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "EH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "EH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "EH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "EH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "EH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "EH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "EH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "EH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "EH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_ashp_hot <-
  read_csv2("data/output/loadprofile/mh_loadprofile_ashp_hot.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "MH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "MH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "MH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "MH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "MH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "MH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "MH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "MH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "MH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_ashp_hot <- eh_loadprofile_ashp_hot %>%
  inner_join(
    mh_loadprofile_ashp_hot,
    by = c("Time" = "Time", "TemperatureKelvin" = "TemperatureKelvin")
  )

loadprofile_ashp_hot <- loadprofile_ashp_hot %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_ASHP" = EH_ElectricityDemand_beginn_1918_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_ASHP" = EH_ElectricityDemand_1919_1948_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_ASHP" = EH_ElectricityDemand_1949_1978_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_ASHP" = EH_ElectricityDemand_1979_1986_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_ASHP" = EH_ElectricityDemand_1987_1990_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_ASHP" = EH_ElectricityDemand_1991_1995_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_ASHP" = EH_ElectricityDemand_1996_2000_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_ASHP" = EH_ElectricityDemand_2001_2011_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_ASHP" = EH_ElectricityDemand_2012_2022_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_ASHP" = EH_ElectricityDemand_2023_2030_hot * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_ASHP" = EH_ElectricityDemand_beginn_1918_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_ASHP" = EH_ElectricityDemand_1919_1948_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_ASHP" = EH_ElectricityDemand_1949_1978_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_ASHP" = EH_ElectricityDemand_1979_1986_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_ASHP" = EH_ElectricityDemand_1987_1990_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_ASHP" = EH_ElectricityDemand_1991_1995_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_ASHP" = EH_ElectricityDemand_1996_2000_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_ASHP" = EH_ElectricityDemand_2001_2011_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_ASHP" = EH_ElectricityDemand_2012_2022_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_ASHP" = EH_ElectricityDemand_2023_2030_hot * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_ASHP" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_36,
    "ApartmentBuildings36_1919_1948_ASHP" = MH_ElectricityDemand_1919_1948_hot * ls_ab_36,
    "ApartmentBuildings36_1949_1978_ASHP" = MH_ElectricityDemand_1949_1978_hot * ls_ab_36,
    "ApartmentBuildings36_1979_1986_ASHP" = MH_ElectricityDemand_1979_1986_hot * ls_ab_36,
    "ApartmentBuildings36_1987_1990_ASHP" = MH_ElectricityDemand_1987_1990_hot * ls_ab_36,
    "ApartmentBuildings36_1991_1995_ASHP" = MH_ElectricityDemand_1991_1995_hot * ls_ab_36,
    "ApartmentBuildings36_1996_2000_ASHP" = MH_ElectricityDemand_1996_2000_hot * ls_ab_36,
    "ApartmentBuildings36_2001_2011_ASHP" = MH_ElectricityDemand_2001_2011_hot * ls_ab_36,
    "ApartmentBuildings36_2012_2022_ASHP" = MH_ElectricityDemand_2012_2022_hot * ls_ab_36,
    "ApartmentBuildings36_2023_2030_ASHP" = MH_ElectricityDemand_2023_2030_hot * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_ASHP" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_ASHP" = MH_ElectricityDemand_1919_1948_hot * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_ASHP" = MH_ElectricityDemand_1949_1978_hot * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_ASHP" = MH_ElectricityDemand_1979_1986_hot * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_ASHP" = MH_ElectricityDemand_1987_1990_hot * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_ASHP" = MH_ElectricityDemand_1991_1995_hot * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_ASHP" = MH_ElectricityDemand_1996_2000_hot * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_ASHP" = MH_ElectricityDemand_2001_2011_hot * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_ASHP" = MH_ElectricityDemand_2012_2022_hot * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_ASHP" = MH_ElectricityDemand_2023_2030_hot * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_hot,
      EH_ElectricityDemand_1919_1948_hot,
      EH_ElectricityDemand_1949_1978_hot,
      EH_ElectricityDemand_1979_1986_hot,
      EH_ElectricityDemand_1987_1990_hot,
      EH_ElectricityDemand_1991_1995_hot,
      EH_ElectricityDemand_1996_2000_hot,
      EH_ElectricityDemand_2001_2011_hot,
      EH_ElectricityDemand_2012_2022_hot,
      EH_ElectricityDemand_2023_2030_hot,
      MH_ElectricityDemand_beginn_1918_hot,
      MH_ElectricityDemand_1919_1948_hot,
      MH_ElectricityDemand_1949_1978_hot,
      MH_ElectricityDemand_1979_1986_hot,
      MH_ElectricityDemand_1987_1990_hot,
      MH_ElectricityDemand_1991_1995_hot,
      MH_ElectricityDemand_1996_2000_hot,
      MH_ElectricityDemand_2001_2011_hot,
      MH_ElectricityDemand_2012_2022_hot,
      MH_ElectricityDemand_2023_2030_hot
    )
  )


eh_loadprofile_gshp_collector_hot <-
  read_csv2("data/output/loadprofile/eh_loadprofile_gshp_collector_hot.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "SoilTemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "EH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "EH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "EH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "EH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "EH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "EH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "EH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "EH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "EH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_gshp_collector_hot <-
  read_csv2("data/output/loadprofile/mh_loadprofile_gshp_collector_hot.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "SoilTemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "MH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "MH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "MH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "MH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "MH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "MH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "MH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "MH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "MH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_gshp_collector_hot <-
  eh_loadprofile_gshp_collector_hot %>%
  inner_join(
    mh_loadprofile_gshp_collector_hot,
    by = c(
      "Time" = "Time",
      "TemperatureKelvin" = "TemperatureKelvin",
      "SoilTemperatureKelvin" = "SoilTemperatureKelvin"
    )
  )

loadprofile_gshp_collector_hot <-
  loadprofile_gshp_collector_hot %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_GSHP_Collector" = EH_ElectricityDemand_beginn_1918_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_GSHP_Collector" = EH_ElectricityDemand_1919_1948_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_GSHP_Collector" = EH_ElectricityDemand_1949_1978_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_GSHP_Collector" = EH_ElectricityDemand_1979_1986_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_GSHP_Collector" = EH_ElectricityDemand_1987_1990_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_GSHP_Collector" = EH_ElectricityDemand_1991_1995_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_GSHP_Collector" = EH_ElectricityDemand_1996_2000_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_GSHP_Collector" = EH_ElectricityDemand_2001_2011_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_GSHP_Collector" = EH_ElectricityDemand_2012_2022_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_GSHP_Collector" = EH_ElectricityDemand_2023_2030_hot * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_GSHP_Collector" = EH_ElectricityDemand_beginn_1918_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_GSHP_Collector" = EH_ElectricityDemand_1919_1948_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_GSHP_Collector" = EH_ElectricityDemand_1949_1978_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_GSHP_Collector" = EH_ElectricityDemand_1979_1986_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_GSHP_Collector" = EH_ElectricityDemand_1987_1990_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_GSHP_Collector" = EH_ElectricityDemand_1991_1995_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_GSHP_Collector" = EH_ElectricityDemand_1996_2000_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_GSHP_Collector" = EH_ElectricityDemand_2001_2011_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_GSHP_Collector" = EH_ElectricityDemand_2012_2022_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_GSHP_Collector" = EH_ElectricityDemand_2023_2030_hot * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_GSHP_Collector" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_36,
    "ApartmentBuildings36_1919_1948_GSHP_Collector" = MH_ElectricityDemand_1919_1948_hot * ls_ab_36,
    "ApartmentBuildings36_1949_1978_GSHP_Collector" = MH_ElectricityDemand_1949_1978_hot * ls_ab_36,
    "ApartmentBuildings36_1979_1986_GSHP_Collector" = MH_ElectricityDemand_1979_1986_hot * ls_ab_36,
    "ApartmentBuildings36_1987_1990_GSHP_Collector" = MH_ElectricityDemand_1987_1990_hot * ls_ab_36,
    "ApartmentBuildings36_1991_1995_GSHP_Collector" = MH_ElectricityDemand_1991_1995_hot * ls_ab_36,
    "ApartmentBuildings36_1996_2000_GSHP_Collector" = MH_ElectricityDemand_1996_2000_hot * ls_ab_36,
    "ApartmentBuildings36_2001_2011_GSHP_Collector" = MH_ElectricityDemand_2001_2011_hot * ls_ab_36,
    "ApartmentBuildings36_2012_2022_GSHP_Collector" = MH_ElectricityDemand_2012_2022_hot * ls_ab_36,
    "ApartmentBuildings36_2023_2030_GSHP_Collector" = MH_ElectricityDemand_2023_2030_hot * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_GSHP_Collector" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_GSHP_Collector" = MH_ElectricityDemand_1919_1948_hot * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_GSHP_Collector" = MH_ElectricityDemand_1949_1978_hot * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_GSHP_Collector" = MH_ElectricityDemand_1979_1986_hot * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_GSHP_Collector" = MH_ElectricityDemand_1987_1990_hot * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_GSHP_Collector" = MH_ElectricityDemand_1991_1995_hot * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_GSHP_Collector" = MH_ElectricityDemand_1996_2000_hot * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_GSHP_Collector" = MH_ElectricityDemand_2001_2011_hot * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_GSHP_Collector" = MH_ElectricityDemand_2012_2022_hot * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_GSHP_Collector" = MH_ElectricityDemand_2023_2030_hot * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_hot,
      EH_ElectricityDemand_1919_1948_hot,
      EH_ElectricityDemand_1949_1978_hot,
      EH_ElectricityDemand_1979_1986_hot,
      EH_ElectricityDemand_1987_1990_hot,
      EH_ElectricityDemand_1991_1995_hot,
      EH_ElectricityDemand_1996_2000_hot,
      EH_ElectricityDemand_2001_2011_hot,
      EH_ElectricityDemand_2012_2022_hot,
      EH_ElectricityDemand_2023_2030_hot,
      MH_ElectricityDemand_beginn_1918_hot,
      MH_ElectricityDemand_1919_1948_hot,
      MH_ElectricityDemand_1949_1978_hot,
      MH_ElectricityDemand_1979_1986_hot,
      MH_ElectricityDemand_1987_1990_hot,
      MH_ElectricityDemand_1991_1995_hot,
      MH_ElectricityDemand_1996_2000_hot,
      MH_ElectricityDemand_2001_2011_hot,
      MH_ElectricityDemand_2012_2022_hot,
      MH_ElectricityDemand_2023_2030_hot
    )
  ) %>% rename("SoilTemperatureKelvinCollector" = "SoilTemperatureKelvin")


eh_loadprofile_gshp_probe_hot <-
  read_csv2("data/output/loadprofile/eh_loadprofile_gshp_probe_hot.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "EH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "EH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "EH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "EH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "EH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "EH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "EH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "EH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "EH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_gshp_probe_hot <-
  read_csv2("data/output/loadprofile/mh_loadprofile_gshp_probe_hot.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "MH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "MH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "MH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "MH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "MH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "MH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "MH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "MH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "MH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_gshp_probe_hot <-
  eh_loadprofile_gshp_probe_hot %>%
  inner_join(
    mh_loadprofile_gshp_probe_hot,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  )


loadprofile_gshp_probe_hot <-
  loadprofile_gshp_probe_hot %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_GSHP_Probe" = EH_ElectricityDemand_beginn_1918_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_GSHP_Probe" = EH_ElectricityDemand_1919_1948_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_GSHP_Probe" = EH_ElectricityDemand_1949_1978_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_GSHP_Probe" = EH_ElectricityDemand_1979_1986_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_GSHP_Probe" = EH_ElectricityDemand_1987_1990_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_GSHP_Probe" = EH_ElectricityDemand_1991_1995_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_GSHP_Probe" = EH_ElectricityDemand_1996_2000_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_GSHP_Probe" = EH_ElectricityDemand_2001_2011_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_GSHP_Probe" = EH_ElectricityDemand_2012_2022_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_GSHP_Probe" = EH_ElectricityDemand_2023_2030_hot * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_GSHP_Probe" = EH_ElectricityDemand_beginn_1918_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_GSHP_Probe" = EH_ElectricityDemand_1919_1948_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_GSHP_Probe" = EH_ElectricityDemand_1949_1978_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_GSHP_Probe" = EH_ElectricityDemand_1979_1986_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_GSHP_Probe" = EH_ElectricityDemand_1987_1990_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_GSHP_Probe" = EH_ElectricityDemand_1991_1995_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_GSHP_Probe" = EH_ElectricityDemand_1996_2000_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_GSHP_Probe" = EH_ElectricityDemand_2001_2011_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_GSHP_Probe" = EH_ElectricityDemand_2012_2022_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_GSHP_Probe" = EH_ElectricityDemand_2023_2030_hot * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_GSHP_Probe" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_36,
    "ApartmentBuildings36_1919_1948_GSHP_Probe" = MH_ElectricityDemand_1919_1948_hot * ls_ab_36,
    "ApartmentBuildings36_1949_1978_GSHP_Probe" = MH_ElectricityDemand_1949_1978_hot * ls_ab_36,
    "ApartmentBuildings36_1979_1986_GSHP_Probe" = MH_ElectricityDemand_1979_1986_hot * ls_ab_36,
    "ApartmentBuildings36_1987_1990_GSHP_Probe" = MH_ElectricityDemand_1987_1990_hot * ls_ab_36,
    "ApartmentBuildings36_1991_1995_GSHP_Probe" = MH_ElectricityDemand_1991_1995_hot * ls_ab_36,
    "ApartmentBuildings36_1996_2000_GSHP_Probe" = MH_ElectricityDemand_1996_2000_hot * ls_ab_36,
    "ApartmentBuildings36_2001_2011_GSHP_Probe" = MH_ElectricityDemand_2001_2011_hot * ls_ab_36,
    "ApartmentBuildings36_2012_2022_GSHP_Probe" = MH_ElectricityDemand_2012_2022_hot * ls_ab_36,
    "ApartmentBuildings36_2023_2030_GSHP_Probe" = MH_ElectricityDemand_2023_2030_hot * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_GSHP_Probe" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_GSHP_Probe" = MH_ElectricityDemand_1919_1948_hot * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_GSHP_Probe" = MH_ElectricityDemand_1949_1978_hot * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_GSHP_Probe" = MH_ElectricityDemand_1979_1986_hot * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_GSHP_Probe" = MH_ElectricityDemand_1987_1990_hot * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_GSHP_Probe" = MH_ElectricityDemand_1991_1995_hot * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_GSHP_Probe" = MH_ElectricityDemand_1996_2000_hot * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_GSHP_Probe" = MH_ElectricityDemand_2001_2011_hot * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_GSHP_Probe" = MH_ElectricityDemand_2012_2022_hot * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_GSHP_Probe" = MH_ElectricityDemand_2023_2030_hot * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_hot,
      EH_ElectricityDemand_1919_1948_hot,
      EH_ElectricityDemand_1949_1978_hot,
      EH_ElectricityDemand_1979_1986_hot,
      EH_ElectricityDemand_1987_1990_hot,
      EH_ElectricityDemand_1991_1995_hot,
      EH_ElectricityDemand_1996_2000_hot,
      EH_ElectricityDemand_2001_2011_hot,
      EH_ElectricityDemand_2012_2022_hot,
      EH_ElectricityDemand_2023_2030_hot,
      MH_ElectricityDemand_beginn_1918_hot,
      MH_ElectricityDemand_1919_1948_hot,
      MH_ElectricityDemand_1949_1978_hot,
      MH_ElectricityDemand_1979_1986_hot,
      MH_ElectricityDemand_1987_1990_hot,
      MH_ElectricityDemand_1991_1995_hot,
      MH_ElectricityDemand_1996_2000_hot,
      MH_ElectricityDemand_2001_2011_hot,
      MH_ElectricityDemand_2012_2022_hot,
      MH_ElectricityDemand_2023_2030_hot
    )
  ) %>% mutate("SoilTemperatureKelvinProbe" = 283.15)


loadprofile_hot <- loadprofile_ashp_hot %>%
  inner_join(
    loadprofile_gshp_collector_hot,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  ) %>% inner_join(
    loadprofile_gshp_probe_hot,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  ) %>% relocate(SoilTemperatureKelvinCollector,
                 .after = TemperatureKelvin) %>% relocate(SoilTemperatureKelvinProbe,
                                                          .after = SoilTemperatureKelvinCollector)


# Only space heat for hot year
eh_loadprofile_ashp_space_heat_only_hot <-
  read_csv2("data/output/loadprofile/eh_loadprofile_ashp_space_heat_only_hot.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "EH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "EH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "EH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "EH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "EH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "EH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "EH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "EH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "EH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_ashp_space_heat_only_hot <-
  read_csv2("data/output/loadprofile/mh_loadprofile_ashp_space_heat_only_hot.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "MH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "MH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "MH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "MH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "MH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "MH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "MH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "MH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "MH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_ashp_space_heat_only_hot <-
  eh_loadprofile_ashp_space_heat_only_hot %>%
  inner_join(
    mh_loadprofile_ashp_space_heat_only_hot,
    by = c("Time" = "Time", "TemperatureKelvin" = "TemperatureKelvin")
  )

loadprofile_ashp_space_heat_only_hot <-
  loadprofile_ashp_space_heat_only_hot %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_ASHP" = EH_ElectricityDemand_beginn_1918_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_ASHP" = EH_ElectricityDemand_1919_1948_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_ASHP" = EH_ElectricityDemand_1949_1978_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_ASHP" = EH_ElectricityDemand_1979_1986_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_ASHP" = EH_ElectricityDemand_1987_1990_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_ASHP" = EH_ElectricityDemand_1991_1995_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_ASHP" = EH_ElectricityDemand_1996_2000_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_ASHP" = EH_ElectricityDemand_2001_2011_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_ASHP" = EH_ElectricityDemand_2012_2022_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_ASHP" = EH_ElectricityDemand_2023_2030_hot * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_ASHP" = EH_ElectricityDemand_beginn_1918_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_ASHP" = EH_ElectricityDemand_1919_1948_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_ASHP" = EH_ElectricityDemand_1949_1978_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_ASHP" = EH_ElectricityDemand_1979_1986_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_ASHP" = EH_ElectricityDemand_1987_1990_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_ASHP" = EH_ElectricityDemand_1991_1995_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_ASHP" = EH_ElectricityDemand_1996_2000_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_ASHP" = EH_ElectricityDemand_2001_2011_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_ASHP" = EH_ElectricityDemand_2012_2022_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_ASHP" = EH_ElectricityDemand_2023_2030_hot * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_ASHP" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_36,
    "ApartmentBuildings36_1919_1948_ASHP" = MH_ElectricityDemand_1919_1948_hot * ls_ab_36,
    "ApartmentBuildings36_1949_1978_ASHP" = MH_ElectricityDemand_1949_1978_hot * ls_ab_36,
    "ApartmentBuildings36_1979_1986_ASHP" = MH_ElectricityDemand_1979_1986_hot * ls_ab_36,
    "ApartmentBuildings36_1987_1990_ASHP" = MH_ElectricityDemand_1987_1990_hot * ls_ab_36,
    "ApartmentBuildings36_1991_1995_ASHP" = MH_ElectricityDemand_1991_1995_hot * ls_ab_36,
    "ApartmentBuildings36_1996_2000_ASHP" = MH_ElectricityDemand_1996_2000_hot * ls_ab_36,
    "ApartmentBuildings36_2001_2011_ASHP" = MH_ElectricityDemand_2001_2011_hot * ls_ab_36,
    "ApartmentBuildings36_2012_2022_ASHP" = MH_ElectricityDemand_2012_2022_hot * ls_ab_36,
    "ApartmentBuildings36_2023_2030_ASHP" = MH_ElectricityDemand_2023_2030_hot * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_ASHP" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_ASHP" = MH_ElectricityDemand_1919_1948_hot * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_ASHP" = MH_ElectricityDemand_1949_1978_hot * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_ASHP" = MH_ElectricityDemand_1979_1986_hot * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_ASHP" = MH_ElectricityDemand_1987_1990_hot * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_ASHP" = MH_ElectricityDemand_1991_1995_hot * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_ASHP" = MH_ElectricityDemand_1996_2000_hot * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_ASHP" = MH_ElectricityDemand_2001_2011_hot * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_ASHP" = MH_ElectricityDemand_2012_2022_hot * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_ASHP" = MH_ElectricityDemand_2023_2030_hot * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_hot,
      EH_ElectricityDemand_1919_1948_hot,
      EH_ElectricityDemand_1949_1978_hot,
      EH_ElectricityDemand_1979_1986_hot,
      EH_ElectricityDemand_1987_1990_hot,
      EH_ElectricityDemand_1991_1995_hot,
      EH_ElectricityDemand_1996_2000_hot,
      EH_ElectricityDemand_2001_2011_hot,
      EH_ElectricityDemand_2012_2022_hot,
      EH_ElectricityDemand_2023_2030_hot,
      MH_ElectricityDemand_beginn_1918_hot,
      MH_ElectricityDemand_1919_1948_hot,
      MH_ElectricityDemand_1949_1978_hot,
      MH_ElectricityDemand_1979_1986_hot,
      MH_ElectricityDemand_1987_1990_hot,
      MH_ElectricityDemand_1991_1995_hot,
      MH_ElectricityDemand_1996_2000_hot,
      MH_ElectricityDemand_2001_2011_hot,
      MH_ElectricityDemand_2012_2022_hot,
      MH_ElectricityDemand_2023_2030_hot
    )
  )


eh_loadprofile_gshp_collector_space_heat_only_hot <-
  read_csv2(
    "data/output/loadprofile/eh_loadprofile_gshp_collector_space_heat_only_hot.csv"
  ) %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "SoilTemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "EH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "EH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "EH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "EH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "EH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "EH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "EH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "EH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "EH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_gshp_collector_space_heat_only_hot <-
  read_csv2(
    "data/output/loadprofile/mh_loadprofile_gshp_collector_space_heat_only_hot.csv"
  ) %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "SoilTemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "MH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "MH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "MH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "MH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "MH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "MH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "MH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "MH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "MH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_gshp_collector_space_heat_only_hot <-
  eh_loadprofile_gshp_collector_space_heat_only_hot %>%
  inner_join(
    mh_loadprofile_gshp_collector_space_heat_only_hot,
    by = c(
      "Time" = "Time",
      "TemperatureKelvin" = "TemperatureKelvin",
      "SoilTemperatureKelvin" = "SoilTemperatureKelvin"
    )
  )

loadprofile_gshp_collector_space_heat_only_hot <-
  loadprofile_gshp_collector_space_heat_only_hot %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_GSHP_Collector" = EH_ElectricityDemand_beginn_1918_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_GSHP_Collector" = EH_ElectricityDemand_1919_1948_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_GSHP_Collector" = EH_ElectricityDemand_1949_1978_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_GSHP_Collector" = EH_ElectricityDemand_1979_1986_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_GSHP_Collector" = EH_ElectricityDemand_1987_1990_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_GSHP_Collector" = EH_ElectricityDemand_1991_1995_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_GSHP_Collector" = EH_ElectricityDemand_1996_2000_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_GSHP_Collector" = EH_ElectricityDemand_2001_2011_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_GSHP_Collector" = EH_ElectricityDemand_2012_2022_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_GSHP_Collector" = EH_ElectricityDemand_2023_2030_hot * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_GSHP_Collector" = EH_ElectricityDemand_beginn_1918_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_GSHP_Collector" = EH_ElectricityDemand_1919_1948_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_GSHP_Collector" = EH_ElectricityDemand_1949_1978_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_GSHP_Collector" = EH_ElectricityDemand_1979_1986_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_GSHP_Collector" = EH_ElectricityDemand_1987_1990_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_GSHP_Collector" = EH_ElectricityDemand_1991_1995_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_GSHP_Collector" = EH_ElectricityDemand_1996_2000_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_GSHP_Collector" = EH_ElectricityDemand_2001_2011_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_GSHP_Collector" = EH_ElectricityDemand_2012_2022_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_GSHP_Collector" = EH_ElectricityDemand_2023_2030_hot * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_GSHP_Collector" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_36,
    "ApartmentBuildings36_1919_1948_GSHP_Collector" = MH_ElectricityDemand_1919_1948_hot * ls_ab_36,
    "ApartmentBuildings36_1949_1978_GSHP_Collector" = MH_ElectricityDemand_1949_1978_hot * ls_ab_36,
    "ApartmentBuildings36_1979_1986_GSHP_Collector" = MH_ElectricityDemand_1979_1986_hot * ls_ab_36,
    "ApartmentBuildings36_1987_1990_GSHP_Collector" = MH_ElectricityDemand_1987_1990_hot * ls_ab_36,
    "ApartmentBuildings36_1991_1995_GSHP_Collector" = MH_ElectricityDemand_1991_1995_hot * ls_ab_36,
    "ApartmentBuildings36_1996_2000_GSHP_Collector" = MH_ElectricityDemand_1996_2000_hot * ls_ab_36,
    "ApartmentBuildings36_2001_2011_GSHP_Collector" = MH_ElectricityDemand_2001_2011_hot * ls_ab_36,
    "ApartmentBuildings36_2012_2022_GSHP_Collector" = MH_ElectricityDemand_2012_2022_hot * ls_ab_36,
    "ApartmentBuildings36_2023_2030_GSHP_Collector" = MH_ElectricityDemand_2023_2030_hot * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_GSHP_Collector" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_GSHP_Collector" = MH_ElectricityDemand_1919_1948_hot * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_GSHP_Collector" = MH_ElectricityDemand_1949_1978_hot * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_GSHP_Collector" = MH_ElectricityDemand_1979_1986_hot * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_GSHP_Collector" = MH_ElectricityDemand_1987_1990_hot * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_GSHP_Collector" = MH_ElectricityDemand_1991_1995_hot * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_GSHP_Collector" = MH_ElectricityDemand_1996_2000_hot * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_GSHP_Collector" = MH_ElectricityDemand_2001_2011_hot * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_GSHP_Collector" = MH_ElectricityDemand_2012_2022_hot * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_GSHP_Collector" = MH_ElectricityDemand_2023_2030_hot * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_hot,
      EH_ElectricityDemand_1919_1948_hot,
      EH_ElectricityDemand_1949_1978_hot,
      EH_ElectricityDemand_1979_1986_hot,
      EH_ElectricityDemand_1987_1990_hot,
      EH_ElectricityDemand_1991_1995_hot,
      EH_ElectricityDemand_1996_2000_hot,
      EH_ElectricityDemand_2001_2011_hot,
      EH_ElectricityDemand_2012_2022_hot,
      EH_ElectricityDemand_2023_2030_hot,
      MH_ElectricityDemand_beginn_1918_hot,
      MH_ElectricityDemand_1919_1948_hot,
      MH_ElectricityDemand_1949_1978_hot,
      MH_ElectricityDemand_1979_1986_hot,
      MH_ElectricityDemand_1987_1990_hot,
      MH_ElectricityDemand_1991_1995_hot,
      MH_ElectricityDemand_1996_2000_hot,
      MH_ElectricityDemand_2001_2011_hot,
      MH_ElectricityDemand_2012_2022_hot,
      MH_ElectricityDemand_2023_2030_hot
    )
  ) %>% rename("SoilTemperatureKelvinCollector" = "SoilTemperatureKelvin")

eh_loadprofile_gshp_probe_space_heat_only_hot <-
  read_csv2(
    "data/output/loadprofile/eh_loadprofile_gshp_probe_space_heat_only_hot.csv"
  ) %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "EH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "EH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "EH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "EH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "EH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "EH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "EH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "EH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "EH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "EH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

mh_loadprofile_gshp_probe_space_heat_only_hot <-
  read_csv2(
    "data/output/loadprofile/mh_loadprofile_gshp_probe_space_heat_only_hot.csv"
  ) %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "ElectricityDemand_beginn_1918_hot",
      "ElectricityDemand_1919_1948_hot",
      "ElectricityDemand_1949_1978_hot",
      "ElectricityDemand_1979_1986_hot",
      "ElectricityDemand_1987_1990_hot",
      "ElectricityDemand_1991_1995_hot",
      "ElectricityDemand_1996_2000_hot",
      "ElectricityDemand_2001_2011_hot",
      "ElectricityDemand_2012_2022_hot",
      "ElectricityDemand_2023_2030_hot"
    )
  ) %>%
  rename(
    "MH_ElectricityDemand_beginn_1918_hot" = "ElectricityDemand_beginn_1918_hot",
    "MH_ElectricityDemand_1919_1948_hot" = "ElectricityDemand_1919_1948_hot",
    "MH_ElectricityDemand_1949_1978_hot" = "ElectricityDemand_1949_1978_hot",
    "MH_ElectricityDemand_1979_1986_hot" = "ElectricityDemand_1979_1986_hot",
    "MH_ElectricityDemand_1987_1990_hot" = "ElectricityDemand_1987_1990_hot",
    "MH_ElectricityDemand_1991_1995_hot" = "ElectricityDemand_1991_1995_hot",
    "MH_ElectricityDemand_1996_2000_hot" = "ElectricityDemand_1996_2000_hot",
    "MH_ElectricityDemand_2001_2011_hot" = "ElectricityDemand_2001_2011_hot",
    "MH_ElectricityDemand_2012_2022_hot" = "ElectricityDemand_2012_2022_hot",
    "MH_ElectricityDemand_2023_2030_hot" = "ElectricityDemand_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_gshp_probe_space_heat_only_hot <-
  eh_loadprofile_gshp_probe_space_heat_only_hot %>%
  inner_join(
    mh_loadprofile_gshp_probe_space_heat_only_hot,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  )


loadprofile_gshp_probe_space_heat_only_hot <-
  loadprofile_gshp_probe_space_heat_only_hot %>%
  mutate(
    "OneAndTwoFamilyHouses_beginn_1918_GSHP_Probe" = EH_ElectricityDemand_beginn_1918_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1919_1948_GSHP_Probe" = EH_ElectricityDemand_1919_1948_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1949_1978_GSHP_Probe" = EH_ElectricityDemand_1949_1978_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1979_1986_GSHP_Probe" = EH_ElectricityDemand_1979_1986_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1987_1990_GSHP_Probe" = EH_ElectricityDemand_1987_1990_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1991_1995_GSHP_Probe" = EH_ElectricityDemand_1991_1995_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_1996_2000_GSHP_Probe" = EH_ElectricityDemand_1996_2000_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2001_2011_GSHP_Probe" = EH_ElectricityDemand_2001_2011_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2012_2022_GSHP_Probe" = EH_ElectricityDemand_2012_2022_hot * ls_one_and_two_family_houses,
    "OneAndTwoFamilyHouses_2023_2030_GSHP_Probe" = EH_ElectricityDemand_2023_2030_hot * ls_one_and_two_family_houses,
    "RowAndSemiDetachedHouses_beginn_1918_GSHP_Probe" = EH_ElectricityDemand_beginn_1918_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1919_1948_GSHP_Probe" = EH_ElectricityDemand_1919_1948_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1949_1978_GSHP_Probe" = EH_ElectricityDemand_1949_1978_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1979_1986_GSHP_Probe" = EH_ElectricityDemand_1979_1986_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1987_1990_GSHP_Probe" = EH_ElectricityDemand_1987_1990_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1991_1995_GSHP_Probe" = EH_ElectricityDemand_1991_1995_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_1996_2000_GSHP_Probe" = EH_ElectricityDemand_1996_2000_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2001_2011_GSHP_Probe" = EH_ElectricityDemand_2001_2011_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2012_2022_GSHP_Probe" = EH_ElectricityDemand_2012_2022_hot * ls_row_and_semi_detached_houses,
    "RowAndSemiDetachedHouses_2023_2030_GSHP_Probe" = EH_ElectricityDemand_2023_2030_hot * ls_row_and_semi_detached_houses,
    "ApartmentBuildings36_beginn_1918_GSHP_Probe" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_36,
    "ApartmentBuildings36_1919_1948_GSHP_Probe" = MH_ElectricityDemand_1919_1948_hot * ls_ab_36,
    "ApartmentBuildings36_1949_1978_GSHP_Probe" = MH_ElectricityDemand_1949_1978_hot * ls_ab_36,
    "ApartmentBuildings36_1979_1986_GSHP_Probe" = MH_ElectricityDemand_1979_1986_hot * ls_ab_36,
    "ApartmentBuildings36_1987_1990_GSHP_Probe" = MH_ElectricityDemand_1987_1990_hot * ls_ab_36,
    "ApartmentBuildings36_1991_1995_GSHP_Probe" = MH_ElectricityDemand_1991_1995_hot * ls_ab_36,
    "ApartmentBuildings36_1996_2000_GSHP_Probe" = MH_ElectricityDemand_1996_2000_hot * ls_ab_36,
    "ApartmentBuildings36_2001_2011_GSHP_Probe" = MH_ElectricityDemand_2001_2011_hot * ls_ab_36,
    "ApartmentBuildings36_2012_2022_GSHP_Probe" = MH_ElectricityDemand_2012_2022_hot * ls_ab_36,
    "ApartmentBuildings36_2023_2030_GSHP_Probe" = MH_ElectricityDemand_2023_2030_hot * ls_ab_36,
    "ApartmentBuildings7More_beginn_1918_GSHP_Probe" = MH_ElectricityDemand_beginn_1918_hot * ls_ab_7more,
    "ApartmentBuildings7More_1919_1948_GSHP_Probe" = MH_ElectricityDemand_1919_1948_hot * ls_ab_7more,
    "ApartmentBuildings7More_1949_1978_GSHP_Probe" = MH_ElectricityDemand_1949_1978_hot * ls_ab_7more,
    "ApartmentBuildings7More_1979_1986_GSHP_Probe" = MH_ElectricityDemand_1979_1986_hot * ls_ab_7more,
    "ApartmentBuildings7More_1987_1990_GSHP_Probe" = MH_ElectricityDemand_1987_1990_hot * ls_ab_7more,
    "ApartmentBuildings7More_1991_1995_GSHP_Probe" = MH_ElectricityDemand_1991_1995_hot * ls_ab_7more,
    "ApartmentBuildings7More_1996_2000_GSHP_Probe" = MH_ElectricityDemand_1996_2000_hot * ls_ab_7more,
    "ApartmentBuildings7More_2001_2011_GSHP_Probe" = MH_ElectricityDemand_2001_2011_hot * ls_ab_7more,
    "ApartmentBuildings7More_2012_2022_GSHP_Probe" = MH_ElectricityDemand_2012_2022_hot * ls_ab_7more,
    "ApartmentBuildings7More_2023_2030_GSHP_Probe" = MH_ElectricityDemand_2023_2030_hot * ls_ab_7more
  ) %>% select(
    -c(
      EH_ElectricityDemand_beginn_1918_hot,
      EH_ElectricityDemand_1919_1948_hot,
      EH_ElectricityDemand_1949_1978_hot,
      EH_ElectricityDemand_1979_1986_hot,
      EH_ElectricityDemand_1987_1990_hot,
      EH_ElectricityDemand_1991_1995_hot,
      EH_ElectricityDemand_1996_2000_hot,
      EH_ElectricityDemand_2001_2011_hot,
      EH_ElectricityDemand_2012_2022_hot,
      EH_ElectricityDemand_2023_2030_hot,
      MH_ElectricityDemand_beginn_1918_hot,
      MH_ElectricityDemand_1919_1948_hot,
      MH_ElectricityDemand_1949_1978_hot,
      MH_ElectricityDemand_1979_1986_hot,
      MH_ElectricityDemand_1987_1990_hot,
      MH_ElectricityDemand_1991_1995_hot,
      MH_ElectricityDemand_1996_2000_hot,
      MH_ElectricityDemand_2001_2011_hot,
      MH_ElectricityDemand_2012_2022_hot,
      MH_ElectricityDemand_2023_2030_hot
    )
  ) %>% mutate("SoilTemperatureKelvinProbe" = 283.15)

loadprofile_space_heat_only_hot <-
  loadprofile_ashp_space_heat_only_hot %>%
  inner_join(
    loadprofile_gshp_collector_space_heat_only_hot,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  ) %>% inner_join(
    loadprofile_gshp_probe_space_heat_only_hot,
    by = c("Time" = "Time",
           "TemperatureKelvin" = "TemperatureKelvin")
  ) %>% relocate(SoilTemperatureKelvinCollector,
                 .after = TemperatureKelvin) %>% relocate(SoilTemperatureKelvinProbe,
                                                          .after = SoilTemperatureKelvinCollector)


# Write output to csv
write_csv(
  loadprofile_hot,
  "data/output/electricityconsumptionbuildingtypes/loadprofile_hot.csv"
)
write_csv(
  loadprofile_space_heat_only_hot,
  "data/output/electricityconsumptionbuildingtypes/loadprofile_space_heat_only_hot.csv"
)
