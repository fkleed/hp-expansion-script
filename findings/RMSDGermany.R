# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)

# Read data
regions_electricity_demand_cold_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_cold_iso.csv"
  )

regions_electricity_demand_hot_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_hot_iso.csv"
  )

regions_electricity_demand_space_heat_only_cold_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_cold_iso.csv"
  )

regions_electricity_demand_space_heat_only_hot_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_hot_iso.csv"
  )


# Perform the RMSD
difference_cold_hot_sh_and_hw <-
  sum(regions_electricity_demand_cold_iso$hourly_electricity_demand) - sum(regions_electricity_demand_hot_iso$hourly_electricity_demand)

difference_cold_hot_sh_only <-
  sum(regions_electricity_demand_space_heat_only_cold_iso$hourly_electricity_demand) - sum(regions_electricity_demand_space_heat_only_hot_iso$hourly_electricity_demand)

rmsd <- difference_cold_hot_sh_and_hw - difference_cold_hot_sh_only

rmsd
