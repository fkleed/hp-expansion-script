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

regions_electricity_demand_sh_only_reference <-
  read_csv("data/regionselectricitydemand/regions_electricity_demand_space_heat_only_reference.csv")

regions_electricity_demand_sh_only_cold <-
  read_csv("data/regionselectricitydemand/regions_electricity_demand_space_heat_only_cold.csv")

regions_electricity_demand_sh_only_hot <-
  read_csv("data/regionselectricitydemand/regions_electricity_demand_space_heat_only_hot.csv")


# Calculate total annual electricity demand for Germany
sum(regions_electricity_demand_cold$hourly_electricity_demand)
sum(regions_electricity_demand_reference$hourly_electricity_demand)
sum(regions_electricity_demand_hot$hourly_electricity_demand)

sum(regions_electricity_demand_sh_only_cold$hourly_electricity_demand)
sum(regions_electricity_demand_sh_only_reference$hourly_electricity_demand)
sum(regions_electricity_demand_sh_only_hot$hourly_electricity_demand)


# Check the sum of the heat pump electricity demand for Bamberg Landkreis and Stadt
hp_electricity_demand_bamberg_city_reference <-
  regions_electricity_demand_reference %>% filter(nuts3_code == "DE241")

hp_electricity_demand_bamberg_district_reference <-
  regions_electricity_demand_reference %>% filter(nuts3_code == "DE245")

hp_electricity_demand_bamberg_city_cold <-
  regions_electricity_demand_cold %>% filter(nuts3_code == "DE241")

hp_electricity_demand_bamberg_district_cold <-
  regions_electricity_demand_cold %>% filter(nuts3_code == "DE245")

hp_electricity_demand_bamberg_city_hot <-
  regions_electricity_demand_hot %>% filter(nuts3_code == "DE241")

hp_electricity_demand_bamberg_district_hot <-
  regions_electricity_demand_hot %>% filter(nuts3_code == "DE245")

# Reference year
# Bamberg Stadt
sum(hp_electricity_demand_bamberg_city_reference$hourly_electricity_demand) / 1000
# Bamberg Landkreis
sum(hp_electricity_demand_bamberg_district_reference$hourly_electricity_demand) / 1000

# Cold year
# Bamberg Stadt
sum(hp_electricity_demand_bamberg_city_cold$hourly_electricity_demand) / 1000
# Bamberg Landkreis
sum(hp_electricity_demand_bamberg_district_cold$hourly_electricity_demand) / 1000

# Hot year
# Bamberg Stadt
sum(hp_electricity_demand_bamberg_city_hot$hourly_electricity_demand) / 1000
# Bamberg Landkreis
sum(hp_electricity_demand_bamberg_district_hot$hourly_electricity_demand) / 1000


# Check Nürnberg Stadt
hp_electricity_demand_nuernberg_city_reference <-
  regions_electricity_demand_reference %>% filter(nuts3_code == "DE241")



hp_distribution <-  read_csv("data/output/heatpumpexpansion/building_stock_2030_with_hp_distribution.csv")

summary(hp_distribution)


hp_distribution_bamberg <- hp_distribution %>% filter(NUTS3Code == "DE245")


hp_amount_air <- sum(hp_distribution_bamberg$HPAmountAir) + sum(hp_distribution_bamberg$HPAmountCollector) + sum(hp_distribution_bamberg$HPAmountProbe)
