# Load required packages
library(tidyverse)
library("dplyr")


# Read the building stock data with hp potential
building_stock_2030_with_hp_potential <-
  read_csv2("data/output/heatpumppotential/building_stock_2030_with_hp_potential.csv") %>% mutate_if(is.character, as.factor)


# Remove buildings with heating type district heating
# Assumption: District heating won't be replaced by heat pumps
building_stock_2030_with_hp_potential <-
  building_stock_2030_with_hp_potential %>%
  filter(HeatingType != "District heating")


# Remove Solar Thermal Energy Ice Storage
# Assumption: Solar Thermal Energy Ice Storage won't play a key role
building_stock_2030_with_hp_potential <-
  building_stock_2030_with_hp_potential %>%
  select(-c(HPPotentialSolarThermalEnergyIceStorage))


summary(building_stock_2030_with_hp_potential)

# The calculations will be implemented in Kotlin
write_csv(
  building_stock_2030_with_hp_potential,
  "data/output/heatpumpexpansion/building_stock_2030_with_hp_potential_processed.csv"
)
