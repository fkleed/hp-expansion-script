# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read the regions electricity demand data
regions_electricity_demand_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_reference_iso.csv"
  )

regions_electricity_demand_cold_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_cold_iso.csv"
  )

regions_electricity_demand_hot_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_hot_iso.csv"
  )


regions_electricity_demand_space_heat_only_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_reference_iso.csv"
  )

regions_electricity_demand_space_heat_only_cold_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_cold_iso.csv"
  )

regions_electricity_demand_space_heat_only_hot_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_hot_iso.csv"
  )

# Aggregate region electricity demand data to level of whole Germany

# Space heat and hot water
electricity_demand_reference_germany_sh_and_hw <-
  regions_electricity_demand_reference_iso %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso) %>%
  summarise("hourly_electricity_demand" = sum(hourly_electricity_demand),
            .groups = "drop")


electricity_demand_cold_germany_sh_and_hw <-
  regions_electricity_demand_cold_iso %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso) %>%
  summarise("hourly_electricity_demand" = sum(hourly_electricity_demand),
            .groups = "drop")


electricity_demand_hot_germany_sh_and_hw <-
  regions_electricity_demand_hot_iso %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso) %>%
  summarise("hourly_electricity_demand" = sum(hourly_electricity_demand),
            .groups = "drop")


# Space heat only
electricity_demand_reference_germany_sh_only <-
  regions_electricity_demand_space_heat_only_reference_iso %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso) %>%
  summarise("hourly_electricity_demand" = sum(hourly_electricity_demand),
            .groups = "drop")


electricity_demand_cold_germany_sh_only <-
  regions_electricity_demand_space_heat_only_cold_iso %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso) %>%
  summarise("hourly_electricity_demand" = sum(hourly_electricity_demand),
            .groups = "drop")


electricity_demand_hot_germany_sh_only <-
  regions_electricity_demand_space_heat_only_hot_iso %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso) %>%
  summarise("hourly_electricity_demand" = sum(hourly_electricity_demand),
            .groups = "drop")


# Total electricity demand over year

# Space heat and hot water
sum(electricity_demand_cold_germany_sh_and_hw$hourly_electricity_demand) / 1000000000
sum(electricity_demand_reference_germany_sh_and_hw$hourly_electricity_demand) / 1000000000
sum(electricity_demand_hot_germany_sh_and_hw$hourly_electricity_demand) / 1000000000

# Space heat only
sum(electricity_demand_cold_germany_sh_only$hourly_electricity_demand) / 1000000000
sum(electricity_demand_reference_germany_sh_only$hourly_electricity_demand) / 1000000000
sum(electricity_demand_hot_germany_sh_only$hourly_electricity_demand) / 1000000000


# Max hourly electricity demand

# Space heat and hot water
max(electricity_demand_cold_germany_sh_and_hw$hourly_electricity_demand) / 1000000
max(electricity_demand_reference_germany_sh_and_hw$hourly_electricity_demand) / 1000000
max(electricity_demand_hot_germany_sh_and_hw$hourly_electricity_demand) / 1000000

# Space heat only
max(electricity_demand_cold_germany_sh_only$hourly_electricity_demand) / 1000000
max(electricity_demand_reference_germany_sh_only$hourly_electricity_demand) / 1000000
max(electricity_demand_hot_germany_sh_only$hourly_electricity_demand) / 1000000


# Mean hourly electricity demand

# Space heat and hot water
mean(electricity_demand_cold_germany_sh_and_hw$hourly_electricity_demand) / 1000000
mean(electricity_demand_reference_germany_sh_and_hw$hourly_electricity_demand) / 1000000
mean(electricity_demand_hot_germany_sh_and_hw$hourly_electricity_demand) / 1000000

# Space heat only
mean(electricity_demand_cold_germany_sh_only$hourly_electricity_demand) / 1000000
mean(electricity_demand_reference_germany_sh_only$hourly_electricity_demand) / 1000000
mean(electricity_demand_hot_germany_sh_only$hourly_electricity_demand) / 1000000


# Write output to csv
write_csv(
  electricity_demand_reference_germany_sh_and_hw,
  "data/output/findings/electricity-demand-germany/electricity_demand_reference_germany_sh_and_hw.csv"
)

write_csv(
  electricity_demand_cold_germany_sh_and_hw,
  "data/output/findings/electricity-demand-germany/electricity_demand_cold_germany_sh_and_hw.csv"
)

write_csv(
  electricity_demand_hot_germany_sh_and_hw,
  "data/output/findings/electricity-demand-germany/electricity_demand_hot_germany_sh_and_hw.csv"
)


write_csv(
  electricity_demand_reference_germany_sh_only,
  "data/output/findings/electricity-demand-germany/electricity_demand_reference_germany_sh_only.csv"
)

write_csv(
  electricity_demand_cold_germany_sh_only,
  "data/output/findings/electricity-demand-germany/electricity_demand_cold_germany_sh_only.csv"
)

write_csv(
  electricity_demand_hot_germany_sh_only,
  "data/output/findings/electricity-demand-germany/electricity_demand_hot_germany_sh_only.csv"
)
