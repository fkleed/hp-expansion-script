# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")


# Read data
eh_loadprofile_ashp_space_heat_only_hot <-
  read_csv2("data/output/loadprofile/eh_loadprofile_gshp_probe_space_heat_only_hot.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "COPHPReal_beginn_1918_hot",
      "COPHPReal_1919_1948_hot",
      "COPHPReal_1949_1978_hot",
      "COPHPReal_1979_1986_hot",
      "COPHPReal_1987_1990_hot",
      "COPHPReal_1991_1995_hot",
      "COPHPReal_1996_2000_hot",
      "COPHPReal_2001_2011_hot",
      "COPHPReal_2012_2022_hot",
      "COPHPReal_2023_2030_hot"
    )
  ) %>%
  rename(
    "EH_COPHPReal_beginn_1918_hot" = "COPHPReal_beginn_1918_hot",
    "EH_COPHPReal_1919_1948_hot" = "COPHPReal_1919_1948_hot",
    "EH_COPHPReal_1949_1978_hot" = "COPHPReal_1949_1978_hot",
    "EH_COPHPReal_1979_1986_hot" = "COPHPReal_1979_1986_hot",
    "EH_COPHPReal_1987_1990_hot" = "COPHPReal_1987_1990_hot",
    "EH_COPHPReal_1991_1995_hot" = "COPHPReal_1991_1995_hot",
    "EH_COPHPReal_1996_2000_hot" = "COPHPReal_1996_2000_hot",
    "EH_COPHPReal_2001_2011_hot" = "COPHPReal_2001_2011_hot",
    "EH_COPHPReal_2012_2022_hot" = "COPHPReal_2012_2022_hot",
    "EH_COPHPReal_2023_2030_hot" = "COPHPReal_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)


mh_loadprofile_ashp_space_heat_only_hot <-
  read_csv2("data/output/loadprofile/mh_loadprofile_gshp_probe_space_heat_only_hot.csv") %>%
  select(
    c(
      "Time",
      "TemperatureKelvin",
      "COPHPReal_beginn_1918_hot",
      "COPHPReal_1919_1948_hot",
      "COPHPReal_1949_1978_hot",
      "COPHPReal_1979_1986_hot",
      "COPHPReal_1987_1990_hot",
      "COPHPReal_1991_1995_hot",
      "COPHPReal_1996_2000_hot",
      "COPHPReal_2001_2011_hot",
      "COPHPReal_2012_2022_hot",
      "COPHPReal_2023_2030_hot"
    )
  ) %>%
  rename(
    "MH_COPHPReal_beginn_1918_hot" = "COPHPReal_beginn_1918_hot",
    "MH_COPHPReal_1919_1948_hot" = "COPHPReal_1919_1948_hot",
    "MH_COPHPReal_1949_1978_hot" = "COPHPReal_1949_1978_hot",
    "MH_COPHPReal_1979_1986_hot" = "COPHPReal_1979_1986_hot",
    "MH_COPHPReal_1987_1990_hot" = "COPHPReal_1987_1990_hot",
    "MH_COPHPReal_1991_1995_hot" = "COPHPReal_1991_1995_hot",
    "MH_COPHPReal_1996_2000_hot" = "COPHPReal_1996_2000_hot",
    "MH_COPHPReal_2001_2011_hot" = "COPHPReal_2001_2011_hot",
    "MH_COPHPReal_2012_2022_hot" = "COPHPReal_2012_2022_hot",
    "MH_COPHPReal_2023_2030_hot" = "COPHPReal_2023_2030_hot"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_ashp_space_heat_only_hot <-
  eh_loadprofile_ashp_space_heat_only_hot %>%
  inner_join(
    mh_loadprofile_ashp_space_heat_only_hot,
    by = c("Time" = "Time", "TemperatureKelvin" = "TemperatureKelvin")
  )

test <- loadprofile_ashp_space_heat_only_hot %>%
  filter(
    EH_COPHPReal_beginn_1918_hot <= 0 |
      EH_COPHPReal_beginn_1918_hot > 8 |
      is.na(EH_COPHPReal_beginn_1918_hot) |
      EH_COPHPReal_1919_1948_hot <= 0 |
      EH_COPHPReal_1919_1948_hot > 8 |
      is.na(EH_COPHPReal_1919_1948_hot) |
      EH_COPHPReal_1949_1978_hot <= 0 |
      EH_COPHPReal_1949_1978_hot > 8 |
      is.na(EH_COPHPReal_1949_1978_hot) |
      EH_COPHPReal_1979_1986_hot <= 0 |
      EH_COPHPReal_1979_1986_hot > 8 |
      is.na(EH_COPHPReal_1979_1986_hot) |
      EH_COPHPReal_1987_1990_hot <= 0 |
      EH_COPHPReal_1987_1990_hot > 8 |
      is.na(EH_COPHPReal_1987_1990_hot) |
      EH_COPHPReal_1991_1995_hot <= 0 |
      EH_COPHPReal_1991_1995_hot > 8 |
      is.na(EH_COPHPReal_1991_1995_hot) |
      EH_COPHPReal_1996_2000_hot <= 0 |
      EH_COPHPReal_1996_2000_hot > 8 |
      is.na(EH_COPHPReal_1996_2000_hot) |
      EH_COPHPReal_2001_2011_hot <= 0 |
      EH_COPHPReal_2001_2011_hot > 8 |
      is.na(EH_COPHPReal_2001_2011_hot) |
      EH_COPHPReal_2012_2022_hot <= 0 |
      EH_COPHPReal_2012_2022_hot > 8 |
      is.na(EH_COPHPReal_2012_2022_hot) |
      EH_COPHPReal_2023_2030_hot <= 0 |
      EH_COPHPReal_2023_2030_hot > 8 |
      is.na(EH_COPHPReal_2023_2030_hot) |
      MH_COPHPReal_beginn_1918_hot <= 0 |
      MH_COPHPReal_beginn_1918_hot > 8 |
      is.na(MH_COPHPReal_beginn_1918_hot) |
      MH_COPHPReal_1919_1948_hot <= 0 |
      MH_COPHPReal_1919_1948_hot > 8 |
      is.na(MH_COPHPReal_1919_1948_hot) |
      MH_COPHPReal_1949_1978_hot <= 0 |
      MH_COPHPReal_1949_1978_hot > 8 |
      is.na(MH_COPHPReal_1949_1978_hot) |
      MH_COPHPReal_1979_1986_hot <= 0 |
      MH_COPHPReal_1979_1986_hot > 8 |
      is.na(MH_COPHPReal_1979_1986_hot) |
      MH_COPHPReal_1987_1990_hot <= 0 |
      MH_COPHPReal_1987_1990_hot > 8 |
      is.na(MH_COPHPReal_1987_1990_hot) |
      MH_COPHPReal_1991_1995_hot <= 0 |
      MH_COPHPReal_1991_1995_hot > 8 |
      is.na(MH_COPHPReal_1991_1995_hot) |
      MH_COPHPReal_1996_2000_hot <= 0 |
      MH_COPHPReal_1996_2000_hot > 8 |
      is.na(MH_COPHPReal_1996_2000_hot) |
      MH_COPHPReal_2001_2011_hot <= 0 |
      MH_COPHPReal_2001_2011_hot > 8 |
      is.na(MH_COPHPReal_2001_2011_hot) |
      MH_COPHPReal_2012_2022_hot <= 0 |
      MH_COPHPReal_2012_2022_hot > 8 |
      is.na(MH_COPHPReal_2012_2022_hot) |
      MH_COPHPReal_2023_2030_hot <= 0 |
      MH_COPHPReal_2023_2030_hot > 8 |
      is.na(MH_COPHPReal_2023_2030_hot)
  )
