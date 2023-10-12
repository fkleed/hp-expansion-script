# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read the regions electricity demand data
regions_electricity_demand_reference <-
  read_csv("data/regionselectricitydemand/regions_electricity_demand_reference.csv")

regions_electricity_demand_cold <-
  read_csv("data/regionselectricitydemand/regions_electricity_demand_cold.csv")

regions_electricity_demand_hot <-
  read_csv("data/regionselectricitydemand/regions_electricity_demand_hot.csv")


regions_electricity_demand_space_heat_only_reference <-
  read_csv(
    "data/regionselectricitydemand/regions_electricity_demand_space_heat_only_reference.csv"
  )

regions_electricity_demand_space_heat_only_cold <-
  read_csv(
    "data/regionselectricitydemand/regions_electricity_demand_space_heat_only_cold.csv"
  )

regions_electricity_demand_space_heat_only_hot <-
  read_csv(
    "data/regionselectricitydemand/regions_electricity_demand_space_heat_only_hot.csv"
  )


# Transform time in DATEISO
regions_electricity_demand_reference_iso <-
  regions_electricity_demand_reference %>%
  mutate(date_iso = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time")) %>%
  relocate(date_iso, .before = hourly_electricity_demand)

regions_electricity_demand_cold_iso <-
  regions_electricity_demand_cold %>%
  mutate(date_iso = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time")) %>%
  relocate(date_iso, .before = hourly_electricity_demand)

regions_electricity_demand_hot_iso <-
  regions_electricity_demand_hot %>%
  mutate(date_iso = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time")) %>%
  relocate(date_iso, .before = hourly_electricity_demand)


regions_electricity_demand_space_heat_only_reference_iso <-
  regions_electricity_demand_space_heat_only_reference %>%
  mutate(date_iso = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time")) %>%
  relocate(date_iso, .before = hourly_electricity_demand)

regions_electricity_demand_space_heat_only_cold_iso <-
  regions_electricity_demand_space_heat_only_cold %>%
  mutate(date_iso = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time")) %>%
  relocate(date_iso, .before = hourly_electricity_demand)

regions_electricity_demand_space_heat_only_hot_iso <-
  regions_electricity_demand_space_heat_only_hot %>%
  mutate(date_iso = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time")) %>%
  relocate(date_iso, .before = hourly_electricity_demand)


# Write output to csv
write_csv(
  regions_electricity_demand_reference_iso,
  "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_reference_iso.csv"
)

write_csv(
  regions_electricity_demand_cold_iso,
  "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_cold_iso.csv"
)

write_csv(
  regions_electricity_demand_hot_iso,
  "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_hot_iso.csv"
)


write_csv(
  regions_electricity_demand_space_heat_only_reference_iso,
  "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_reference_iso.csv"
)

write_csv(
  regions_electricity_demand_space_heat_only_cold_iso,
  "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_cold_iso.csv"
)


write_csv(
  regions_electricity_demand_space_heat_only_hot_iso,
  "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_hot_iso.csv"
)


