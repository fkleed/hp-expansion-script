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


# Calculate the amount of buildings with heat pump potential
building_stock_2030_with_hp_potential <-
  building_stock_2030_with_hp_potential %>%
  mutate(BuildingCountHPPotential = BuildingCount * HPPotentialTotal)


# Calculate the maximum of possible potential for Air, Probe, and Collector
building_stock_2030_with_hp_potential <-
  building_stock_2030_with_hp_potential %>%
  mutate(
    MaxBuildingCountHPPotentialAir = BuildingCount * HPPotentialAir,
    MaxBuildingCountHPPotentialProbe = BuildingCount * HPPotentialProbe,
    MaxBuildingCountHPPotentialCollector = BuildingCount * HPPotentialCollector
  )


# Split the building stock to calculate the distribution of heat pumps for 2023 - 2030, 2012 - 2022, and <= 2011
building_stock_2030_with_hp_potential_2023_2030 <-
  building_stock_2030_with_hp_potential %>%
  filter(YearOfConstruction == "2023 - 2030")

building_stock_2030_with_hp_potential_2012_2022 <-
  building_stock_2030_with_hp_potential %>%
  filter(YearOfConstruction == "2012 - 2022")

building_stock_2030_with_hp_potential_beginn_2011 <-
  building_stock_2030_with_hp_potential %>%
  filter(!(YearOfConstruction %in% c("2012 - 2022", "2023 - 2030")))


# Function for calculating the distribution of heat pumps
share_ashp <- 0.8
share_gshp <- (1 - share_ashp)
share_gshp_probe <- share_gshp * 0.75
share_gshp_collector <- share_gshp * 0.25


heat_pump_distribution_function <-
  function(total_hp_amount,
           max_hp_amount_air,
           max_hp_amount_probe,
           max_hp_amount_collector) {
    remaining <- total_hp_amount

    hp_amount_air <- 0
    hp_amount_probe <- 0
    hp_amount_collector <- 0

    if (max_hp_amount_air >= (share_ashp * total_hp_amount)) {
      hp_amount_air <-  share_ashp * total_hp_amount
      remaining <- remaining - (share_ashp * total_hp_amount)
    } else {
      hp_amount_air <- max_hp_amount_air
      remaining <- remaining - max_hp_amount_air
    }

    if (max_hp_amount_probe >= (share_gshp_probe * total_hp_amount)) {
      hp_amount_probe <-  share_gshp_probe * total_hp_amount
      remaining <- remaining - (share_gshp_probe * total_hp_amount)
    } else {
      hp_amount_probe <- max_hp_amount_probe
      remaining <- remaining - max_hp_amount_probe
    }

    if (max_hp_amount_collector >= (share_gshp_collector * total_hp_amount)) {
      hp_amount_collector <-  share_gshp_collector * total_hp_amount
      remaining <-
        remaining - (share_gshp_collector * total_hp_amount)
    } else {
      hp_amount_collector <- max_hp_amount_collector
      remaining <- remaining - max_hp_amount_collector
    }


    if (remaining > 0) {
      remaining_hp_potential_air <-
        ifelse(
          max_hp_amount_air > (share_ashp * total_hp_amount),
          max_hp_amount_air - (share_ashp * total_hp_amount),
          0
        )

      remaining_hp_potential_probe <-
        ifelse(
          max_hp_amount_probe > (share_gshp_probe * total_hp_amount),
          max_hp_amount_probe - (share_gshp_probe * total_hp_amount),
          0
        )

      remaining_hp_potential_collector <-
        ifelse(
          max_hp_amount_collector > (share_gshp_collector * total_hp_amount),
          max_hp_amount_collector - (share_gshp_collector * total_hp_amount),
          0
        )
    }



  }


# Calculate the heat pump distribution for 2023 - 2030



# Calculate the amount of assigned heat pumps
heat_pumps_2030 <- 6000000


building_stock_2030_with_hp_potential <-
  building_stock_2030_with_hp_potential %>%
  mutate(SumBuildingsHPPotential = sum(BuildingCountHPPotential))

building_stock_2030_with_hp_potential <-
  building_stock_2030_with_hp_potential %>%
  mutate(ShareBuildingsHPPotential = BuildingCountHPPotential / SumBuildingsHPPotential)

building_stock_2030_with_hp_potential <-
  building_stock_2030_with_hp_potential %>%
  mutate(HPAmount = ShareBuildingsHPPotential * heat_pumps_2030)
