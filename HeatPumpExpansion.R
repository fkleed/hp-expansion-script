# Load required packages
library(tidyverse)
library("dplyr")


# Read the building stock data with hp potential
building_stock_2030_with_hp_potential <-
  read_csv("data/output/heatpumppotential/building_stock_2030_with_hp_potential.csv") %>% mutate_if(is.character, as.factor)


# The calculations will be implemented in Kotlin
# Read the Kotlin calculation results
building_stock_2030_with_hp_distribution <- read_csv("data/output/heatpumpexpansion/building_stock_2030_with_hp_distribution.csv")

sum(building_stock_2030_with_hp_distribution$HPAmountAir) + sum(building_stock_2030_with_hp_distribution$HPAmountProbe) + sum(building_stock_2030_with_hp_distribution$HPAmountCollector)

