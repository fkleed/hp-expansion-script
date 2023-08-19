# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")
library("rjson")

real_building_data <-
  as.data.frame(fromJSON(file = "data/realbuildingdata/RealBuildingData.json"))


# Get the space heat demand for 2022 from building 36 38
space_heat_2022_building_36_38 <- real_building_data %>%
  select(
    contains("B36") &
      contains("2022") & contains("0x8") |
      contains("B38") & contains("2022") & contains("0x8")
  )


# Optional remove building B36ME49.2022.0x8 because it seems that nobody was living there in the beginning of 2022
# But then the square meter count has to be adapted.


# Calculate the sum of space heat for the months
space_heat_2022_building_36_38 <- space_heat_2022_building_36_38 %>%
  mutate(TotalSpaceHeatDemandPerMonth = rowSums(space_heat_2022_building_36_38, na.rm = TRUE))


# Estimate the building size over Google Earth: 42 meters length and 15 meters width + 4 floors
# Space of 2520 m2
# Calculate the heat demand per m2 per month

space_heat_2022_building_36_38 <- space_heat_2022_building_36_38 %>%
  mutate(SpaceHeatDemandPerM2PerMonth = TotalSpaceHeatDemandPerMonth / 2200) %>%
  select(c(TotalSpaceHeatDemandPerMonth, SpaceHeatDemandPerM2PerMonth))



# Compare with the model data from 2022
mh_heat_demand_2022 <- read.csv2("data/output/heatdemand/mh_combined_heat_demand_hot.csv") %>%
  select(c(Time, SpaceHeat_2012_2022_hot)) %>%
  mutate(Month = substr(Time, 4, 5)) %>%
  select(-c(Time)) %>%
  group_by(Month) %>%
  summarise(SpaceHeatDemandPerM2PerMonthModel = sum(SpaceHeat_2012_2022_hot),
            .groups = 'drop')
