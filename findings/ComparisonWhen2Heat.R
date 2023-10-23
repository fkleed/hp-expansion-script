# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)
library(zoo)



# Read data
when2heat <-
  read_csv2("data/when2heat/when2heat.csv") %>%
  select(
    c(
      "utc_timestamp",
      "cet_cest_timestamp",
      "DE_COP_ASHP_floor",
      "DE_COP_ASHP_radiator"
    )
  )

building_stock_2030_with_hp_distribution <-
  read_csv("data/output/heatpumpexpansion/building_stock_2030_with_hp_distribution.csv") %>%
  filter(YearOfConstruction != "2023 - 2030")

weather_data_reference <-
  read_csv2("data/output/weathermodel/year2017.csv") %>%
  mutate(
    TemperatureKelvin = as.numeric(RoundedMeanTemperature) + 273.15,
    date_iso = ISOdate(
      2017,
      ifelse(substr(Date, 1, 2) == "00", 0, sub("^0+", "", substr(Date, 1, 2))),
      ifelse(substr(Date, 3, 4) == "00", 0, sub("^0+", "", substr(Date, 3, 4))),
      ifelse(substr(Date, 5, 6) == "00", 0, sub("^0+", "", substr(Date, 5, 6)))
    )
  ) %>%
  select(-c(Date, MeanTemperature, RoundedMeanTemperature))


# Calculate mixed temperature
weather_data_reference <-
  tibble::rowid_to_column(weather_data_reference, "RowNumber")

weather_data_reference  <-
  weather_data_reference %>% mutate(T72WeightedAverage = rollmeanr(TemperatureKelvin,
                                                                   72,
                                                                   fill = NA))

weather_data_reference_until_71 <-
  weather_data_reference %>% filter(RowNumber < 72) %>%
  mutate(CumSumTemperatureKelvin = cumsum(TemperatureKelvin)) %>%
  mutate(T72WeightedAverage = (CumSumTemperatureKelvin / RowNumber)) %>%
  select(-c(CumSumTemperatureKelvin))

weather_data_reference <- weather_data_reference %>%
  filter(RowNumber >= 72)

weather_data_reference <-
  rbind(weather_data_reference_until_71, weather_data_reference)

rm(weather_data_reference_until_71)

weather_data_reference <- weather_data_reference %>%
  mutate(TMix = ifelse(
    substr(date_iso, 12, 13) %in% c("10", "11", "12", "13", "14", "15", "16"),
    TemperatureKelvin,
    T72WeightedAverage
  )) %>%
  select(-c(RowNumber, T72WeightedAverage))


# Reference year
eh_loadprofile_ashp_space_heat_only_reference <-
  read_csv2("data/output/loadprofile/eh_loadprofile_ashp_space_heat_only_reference.csv") %>%
  select(
    c(
      "Time",
      "COPHPReal_beginn_1918_reference",
      "COPHPReal_1919_1948_reference",
      "COPHPReal_1949_1978_reference",
      "COPHPReal_1979_1986_reference",
      "COPHPReal_1987_1990_reference",
      "COPHPReal_1991_1995_reference",
      "COPHPReal_1996_2000_reference",
      "COPHPReal_2001_2011_reference",
      "COPHPReal_2012_2022_reference",
      "COPHPReal_2023_2030_reference"
    )
  ) %>%
  rename(
    "EH_COPHPReal_beginn_1918_reference" = "COPHPReal_beginn_1918_reference",
    "EH_COPHPReal_1919_1948_reference" = "COPHPReal_1919_1948_reference",
    "EH_COPHPReal_1949_1978_reference" = "COPHPReal_1949_1978_reference",
    "EH_COPHPReal_1979_1986_reference" = "COPHPReal_1979_1986_reference",
    "EH_COPHPReal_1987_1990_reference" = "COPHPReal_1987_1990_reference",
    "EH_COPHPReal_1991_1995_reference" = "COPHPReal_1991_1995_reference",
    "EH_COPHPReal_1996_2000_reference" = "COPHPReal_1996_2000_reference",
    "EH_COPHPReal_2001_2011_reference" = "COPHPReal_2001_2011_reference",
    "EH_COPHPReal_2012_2022_reference" = "COPHPReal_2012_2022_reference",
    "EH_COPHPReal_2023_2030_reference" = "COPHPReal_2023_2030_reference"
  ) %>% mutate_if(is.character, as.factor)


summary(eh_loadprofile_ashp_space_heat_only_reference)


mh_loadprofile_ashp_space_heat_only_reference <-
  read_csv2("data/output/loadprofile/mh_loadprofile_ashp_space_heat_only_reference.csv") %>%
  select(
    c(
      "Time",
      "COPHPReal_beginn_1918_reference",
      "COPHPReal_1919_1948_reference",
      "COPHPReal_1949_1978_reference",
      "COPHPReal_1979_1986_reference",
      "COPHPReal_1987_1990_reference",
      "COPHPReal_1991_1995_reference",
      "COPHPReal_1996_2000_reference",
      "COPHPReal_2001_2011_reference",
      "COPHPReal_2012_2022_reference",
      "COPHPReal_2023_2030_reference"
    )
  ) %>%
  rename(
    "MH_COPHPReal_beginn_1918_reference" = "COPHPReal_beginn_1918_reference",
    "MH_COPHPReal_1919_1948_reference" = "COPHPReal_1919_1948_reference",
    "MH_COPHPReal_1949_1978_reference" = "COPHPReal_1949_1978_reference",
    "MH_COPHPReal_1979_1986_reference" = "COPHPReal_1979_1986_reference",
    "MH_COPHPReal_1987_1990_reference" = "COPHPReal_1987_1990_reference",
    "MH_COPHPReal_1991_1995_reference" = "COPHPReal_1991_1995_reference",
    "MH_COPHPReal_1996_2000_reference" = "COPHPReal_1996_2000_reference",
    "MH_COPHPReal_2001_2011_reference" = "COPHPReal_2001_2011_reference",
    "MH_COPHPReal_2012_2022_reference" = "COPHPReal_2012_2022_reference",
    "MH_COPHPReal_2023_2030_reference" = "COPHPReal_2023_2030_reference"
  ) %>% mutate_if(is.character, as.factor)

loadprofile_ashp_space_heat_only_reference <-
  eh_loadprofile_ashp_space_heat_only_reference %>%
  inner_join(mh_loadprofile_ashp_space_heat_only_reference,
             by = c("Time" = "Time")) %>%
  mutate(date_iso = ISOdate(
    2017,
    ifelse(substr(Time, 4, 5) == "00", 0, sub("^0+", "", substr(Time, 4, 5))),
    ifelse(substr(Time, 1, 2) == "00", 0, sub("^0+", "", substr(Time, 1, 2))),
    ifelse(substr(Time, 7, 8) == "00", 0, sub("^0+", "", substr(Time, 7, 8)))
  )) %>%
  select(-c("Time"))


# Calculate the weighted average of the COP

# Get the share of building category with years of construction range
total_ashp_amount <-
  sum(building_stock_2030_with_hp_distribution$HPAmountAir)

building_stock_2030_aggregated <-
  building_stock_2030_with_hp_distribution %>%
  select(c("BuildingTypeSize",
           "YearOfConstruction",
           "HPAmountAir")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    BuildingTypeSize = fct_recode(
      BuildingTypeSize,
      "ab" = "Apartment Buildings (3-6)",
      "ab" = "Apartment Buildings: 7 and More Apartments",
      "sah" = "One- and Two-family Houses",
      "sah" = "Row Houses",
      "sah" = "Semi-detached Houses"
    )
  ) %>%
  group_by(BuildingTypeSize, YearOfConstruction) %>%
  summarise(HPAmountAir = sum(HPAmountAir), .groups = "drop")

# Get the ASHP shares in the building types with years of construction

# Apartment buildings
share_ashp_ab_Before_1919 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "ab" &
               YearOfConstruction == "Before 1919")
  )$HPAmountAir / total_ashp_amount

share_ashp_ab_1919_1948 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "ab" &
               YearOfConstruction == "1919 - 1948")
  )$HPAmountAir / total_ashp_amount


share_ashp_ab_1949_1978 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "ab" &
               YearOfConstruction == "1949 - 1978")
  )$HPAmountAir / total_ashp_amount

share_ashp_ab_1979_1986 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "ab" &
               YearOfConstruction == "1979 - 1986")
  )$HPAmountAir / total_ashp_amount

share_ashp_ab_1987_1990 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "ab" &
               YearOfConstruction == "1987 - 1990")
  )$HPAmountAir / total_ashp_amount

share_ashp_ab_1991_1995 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "ab" &
               YearOfConstruction == "1991 - 1995")
  )$HPAmountAir / total_ashp_amount

share_ashp_ab_1996_2000 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "ab" &
               YearOfConstruction == "1996 - 2000")
  )$HPAmountAir / total_ashp_amount

share_ashp_ab_2001_2011 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "ab" &
               YearOfConstruction == "2001 - 2011")
  )$HPAmountAir / total_ashp_amount

share_ashp_ab_2012_2022 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "ab" &
               YearOfConstruction == "2012 - 2022")
  )$HPAmountAir / total_ashp_amount

# Standalone houses
share_ashp_sah_Before_1919 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "sah" &
               YearOfConstruction == "Before 1919")
  )$HPAmountAir / total_ashp_amount

share_ashp_sah_1919_1948 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "sah" &
               YearOfConstruction == "1919 - 1948")
  )$HPAmountAir / total_ashp_amount


share_ashp_sah_1949_1978 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "sah" &
               YearOfConstruction == "1949 - 1978")
  )$HPAmountAir / total_ashp_amount

share_ashp_sah_1979_1986 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "sah" &
               YearOfConstruction == "1979 - 1986")
  )$HPAmountAir / total_ashp_amount

share_ashp_sah_1987_1990 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "sah" &
               YearOfConstruction == "1987 - 1990")
  )$HPAmountAir / total_ashp_amount

share_ashp_sah_1991_1995 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "sah" &
               YearOfConstruction == "1991 - 1995")
  )$HPAmountAir / total_ashp_amount

share_ashp_sah_1996_2000 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "sah" &
               YearOfConstruction == "1996 - 2000")
  )$HPAmountAir / total_ashp_amount

share_ashp_sah_2001_2011 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "sah" &
               YearOfConstruction == "2001 - 2011")
  )$HPAmountAir / total_ashp_amount

share_ashp_sah_2012_2022 <-
  (
    building_stock_2030_aggregated %>%
      filter(BuildingTypeSize == "sah" &
               YearOfConstruction == "2012 - 2022")
  )$HPAmountAir / total_ashp_amount


share_ab <-
  share_ashp_ab_1919_1948 + share_ashp_ab_1949_1978 + share_ashp_ab_1979_1986 + share_ashp_ab_1987_1990 + share_ashp_ab_1991_1995 + share_ashp_ab_1996_2000 + share_ashp_ab_2001_2011 + share_ashp_ab_2012_2022 + share_ashp_ab_Before_1919
share_sah <-
  share_ashp_sah_1919_1948 + share_ashp_sah_1949_1978 + share_ashp_sah_1979_1986 + share_ashp_sah_1987_1990 + share_ashp_sah_1991_1995 + share_ashp_sah_1996_2000 + share_ashp_sah_2001_2011 + share_ashp_sah_2012_2022 + share_ashp_sah_Before_1919

share_ab + share_sah

# Calculate the weighted COP
loadprofile_ashp_space_heat_only_reference_weighted_cop <-
  loadprofile_ashp_space_heat_only_reference %>%
  mutate(
    WeightedCOP =
      EH_COPHPReal_beginn_1918_reference * share_ashp_sah_Before_1919 +
      EH_COPHPReal_1919_1948_reference * share_ashp_sah_1919_1948 +
      EH_COPHPReal_1949_1978_reference * share_ashp_sah_1949_1978 +
      EH_COPHPReal_1979_1986_reference * share_ashp_sah_1979_1986 +
      EH_COPHPReal_1987_1990_reference * share_ashp_sah_1987_1990 +
      EH_COPHPReal_1991_1995_reference * share_ashp_sah_1991_1995 +
      EH_COPHPReal_1996_2000_reference * share_ashp_sah_1996_2000 +
      EH_COPHPReal_2001_2011_reference * share_ashp_sah_2001_2011 +
      EH_COPHPReal_2012_2022_reference * share_ashp_sah_2012_2022 +
      MH_COPHPReal_beginn_1918_reference * share_ashp_ab_Before_1919 +
      MH_COPHPReal_1919_1948_reference * share_ashp_ab_1919_1948 +
      MH_COPHPReal_1949_1978_reference * share_ashp_ab_1949_1978 +
      MH_COPHPReal_1979_1986_reference * share_ashp_ab_1979_1986 +
      MH_COPHPReal_1987_1990_reference * share_ashp_ab_1987_1990 +
      MH_COPHPReal_1991_1995_reference * share_ashp_ab_1991_1995 +
      MH_COPHPReal_1996_2000_reference * share_ashp_ab_1996_2000 +
      MH_COPHPReal_2001_2011_reference * share_ashp_ab_2001_2011 +
      MH_COPHPReal_2012_2022_reference * share_ashp_ab_2012_2022
  ) %>%
  select(c("date_iso",
           "WeightedCOP"))

summary(loadprofile_ashp_space_heat_only_reference_weighted_cop)


# Get the average COP from When2Heat for ASHPs
when2heat_reference <-
  when2heat %>%
  mutate(UTCModified = as.character(utc_timestamp)) %>%
  mutate(UTCModified = ifelse(
    nchar(UTCModified) == 10,
    paste(UTCModified, "00:00:00"),
    UTCModified
  )) %>%
  filter(substring(UTCModified, 1, 4) == "2017") %>%
  mutate(date_iso = ISOdate(
    2017,
    ifelse(substring(UTCModified, 6, 7) == "00", 0, sub("^0+", "", substring(UTCModified, 6, 7))),
    ifelse(substring(UTCModified, 9, 10) == "00", 0, sub("^0+", "", substring(UTCModified, 9, 10))),
    ifelse(substring(UTCModified, 12, 13) == "00", 0, sub("^0+", "", substring(UTCModified, 12, 13)))
  )) %>%
  mutate(AVGCOPWhen2Heat = (DE_COP_ASHP_floor + DE_COP_ASHP_radiator) / 2) %>%
  select(c("date_iso",
           "AVGCOPWhen2Heat"))


# Join data and perform regression
cop_comparison_ashp_reference <-
  loadprofile_ashp_space_heat_only_reference_weighted_cop %>%
  inner_join(when2heat_reference, by = c("date_iso")) %>%
  inner_join(weather_data_reference, by = c("date_iso"))

summary(cop_comparison_ashp_reference)


plot(
  cop_comparison_ashp_reference$AVGCOPWhen2Heat,
  cop_comparison_ashp_reference$WeightedCOP
)


# Perform the linear regression
model <-
  lm(AVGCOPWhen2Heat ~ WeightedCOP, data = cop_comparison_ashp_reference)
abline(model)

summary(model)

# Transform time to date_iso

# Save results
write_csv(
  cop_comparison_ashp_reference,
  "data/output/findings/comparison-when2heat/cop_comparison_ashp_reference.csv"
)

