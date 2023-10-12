# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)

# Read the data
nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx") %>%
  rename("nuts1_code" = "NUTS1Code") %>%
  select(c("nuts1_code",
           "NUTS3Code"))

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

# Calculate the electricity demand for the federal states

# Space heating and hot water
electricity_demand_reference_federal_states_sh_and_hw <-
  regions_electricity_demand_reference_iso %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso, nuts1_code) %>%
  summarise(hourly_electricity_demand = sum(hourly_electricity_demand), .groups = "drop") %>%
  arrange(nuts1_code) %>%
  relocate(nuts1_code, .before = date_iso)

electricity_demand_cold_federal_states_sh_and_hw <-
  regions_electricity_demand_cold_iso %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso, nuts1_code) %>%
  summarise(hourly_electricity_demand = sum(hourly_electricity_demand), .groups = "drop") %>%
  arrange(nuts1_code) %>%
  relocate(nuts1_code, .before = date_iso)

electricity_demand_hot_federal_states_sh_and_hw <-
  regions_electricity_demand_hot_iso %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso, nuts1_code) %>%
  summarise(hourly_electricity_demand = sum(hourly_electricity_demand), .groups = "drop") %>%
  arrange(nuts1_code) %>%
  relocate(nuts1_code, .before = date_iso)


# Space heating only
electricity_demand_reference_federal_states_sh_only <-
  regions_electricity_demand_space_heat_only_reference_iso %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso, nuts1_code) %>%
  summarise(hourly_electricity_demand = sum(hourly_electricity_demand), .groups = "drop") %>%
  arrange(nuts1_code) %>%
  relocate(nuts1_code, .before = date_iso)

electricity_demand_cold_federal_states_sh_only <-
  regions_electricity_demand_space_heat_only_cold_iso %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso, nuts1_code) %>%
  summarise(hourly_electricity_demand = sum(hourly_electricity_demand), .groups = "drop") %>%
  arrange(nuts1_code) %>%
  relocate(nuts1_code, .before = date_iso)

electricity_demand_hot_federal_states_sh_only <-
  regions_electricity_demand_space_heat_only_hot_iso %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code")) %>%
  group_by(date_iso, nuts1_code) %>%
  summarise(hourly_electricity_demand = sum(hourly_electricity_demand), .groups = "drop") %>%
  arrange(nuts1_code) %>%
  relocate(nuts1_code, .before = date_iso)


# Write output to csv
write_csv(
  electricity_demand_reference_federal_states_sh_and_hw,
  "data/output/findings/electricity-demand-federal-states/electricity_demand_reference_federal_states_sh_and_hw.csv"
)

write_csv(
  electricity_demand_cold_federal_states_sh_and_hw,
  "data/output/findings/electricity-demand-federal-states/electricity_demand_cold_federal_states_sh_and_hw.csv"
)

write_csv(
  electricity_demand_hot_federal_states_sh_and_hw,
  "data/output/findings/electricity-demand-federal-states/electricity_demand_hot_federal_states_sh_and_hw.csv"
)


write_csv(
  electricity_demand_reference_federal_states_sh_only,
  "data/output/findings/electricity-demand-federal-states/electricity_demand_reference_federal_states_sh_only.csv"
)

write_csv(
  electricity_demand_cold_federal_states_sh_only,
  "data/output/findings/electricity-demand-federal-states/electricity_demand_cold_federal_states_sh_only.csv"
)

write_csv(
  electricity_demand_hot_federal_states_sh_only,
  "data/output/findings/electricity-demand-federal-states/electricity_demand_hot_federal_states_sh_only.csv"
)
