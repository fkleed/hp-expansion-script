# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read the heat demand data
eh_combined_heat_demand_avg <-
  read_csv2("data/loadprofile/heatdemand/eh_combined_heat_demand_avg.csv")

eh_combined_heat_demand_cold <-
  read_csv2("data/loadprofile/heatdemand/eh_combined_heat_demand_cold.csv")

eh_combined_heat_demand_hot <-
  read_csv2("data/loadprofile/heatdemand/eh_combined_heat_demand_hot.csv")

mh_combined_heat_demand_avg <-
  read_csv2("data/loadprofile/heatdemand/mh_combined_heat_demand_avg.csv")

mh_combined_heat_demand_cold <-
  read_csv2("data/loadprofile/heatdemand/mh_combined_heat_demand_cold.csv")

mh_combined_heat_demand_hot <-
  read_csv2("data/loadprofile/heatdemand/mh_combined_heat_demand_hot.csv")

# Read the weather data
weather_data_avg <-
  read_csv2("data/loadprofile/weatherdata/averageyear.csv") %>%
  mutate(
    RoundedMeanTemperature = as.numeric(RoundedMeanTemperature),
    Time = paste(paste(substr(Date, 3, 4), substr(Date, 1, 2), sep = "-"),
                 paste(substr(Date, 5, 6), "00", sep = ":"))
  ) %>%
  select(-c(Date, MeanTemperature))


weather_data_cold <-
  read_csv2("data/loadprofile/weatherdata/year2010.csv") %>%
  mutate(
    RoundedMeanTemperature = as.numeric(RoundedMeanTemperature),
    Time = paste(paste(substr(Date, 3, 4), substr(Date, 1, 2), sep = "-"),
                 paste(substr(Date, 5, 6), "00", sep = ":"))
  ) %>%
  select(-c(Date, MeanTemperature))

weather_data_hot <-
  read_csv2("data/loadprofile/weatherdata/year2022.csv") %>%
  mutate(
    RoundedMeanTemperature = as.numeric(RoundedMeanTemperature),
    Time = paste(paste(substr(Date, 3, 4), substr(Date, 1, 2), sep = "-"),
                 paste(substr(Date, 5, 6), "00", sep = ":"))
  ) %>%
  select(-c(Date, MeanTemperature))


