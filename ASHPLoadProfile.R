# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")
library(zoo)

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
    TemperatureKelvin = as.numeric(RoundedMeanTemperature) + 273.15,
    Time = paste(paste(substr(Date, 3, 4), substr(Date, 1, 2), sep = "-"),
                 paste(substr(Date, 5, 6), "00", sep = ":"))
  ) %>%
  select(-c(Date, MeanTemperature, RoundedMeanTemperature))


weather_data_cold <-
  read_csv2("data/loadprofile/weatherdata/year2010.csv") %>%
  mutate(
    TemperatureKelvin = as.numeric(RoundedMeanTemperature) + 273.15,
    Time = paste(paste(substr(Date, 3, 4), substr(Date, 1, 2), sep = "-"),
                 paste(substr(Date, 5, 6), "00", sep = ":"))
  ) %>%
  select(-c(Date, MeanTemperature, RoundedMeanTemperature))

weather_data_hot <-
  read_csv2("data/loadprofile/weatherdata/year2022.csv") %>%
  mutate(
    TemperatureKelvin = as.numeric(RoundedMeanTemperature) + 273.15,
    Time = paste(paste(substr(Date, 3, 4), substr(Date, 1, 2), sep = "-"),
                 paste(substr(Date, 5, 6), "00", sep = ":"))
  ) %>%
  select(-c(Date, MeanTemperature, RoundedMeanTemperature))


# Calculate the hourly mixed temperature of the weather data
weather_data_avg <-
  tibble::rowid_to_column(weather_data_avg, "RowNumber")

weather_data_avg  <-
  weather_data_avg %>% mutate(T72WeightedAverage = round(rollmeanr(TemperatureKelvin,
                                                                   72,
                                                                   fill = NA), 2))

weather_data_avg_until_71 <-
  weather_data_avg %>% filter(RowNumber < 72) %>%
  mutate(CumSumTemperatureKelvin = cumsum(TemperatureKelvin)) %>%
  mutate(T72WeightedAverage = round(CumSumTemperatureKelvin / RowNumber, 2)) %>%
  select(-c(CumSumTemperatureKelvin))

weather_data_avg <- weather_data_avg %>%
  filter(RowNumber >= 72)

weather_data_avg <-
  rbind(weather_data_avg_until_71, weather_data_avg)

rm(weather_data_avg_until_71)

weather_data_avg <- weather_data_avg %>%
  mutate(TMix = ifelse(
    substr(Time, 7, 8) %in% c("10", "11", "12", "13", "14", "15", "16"),
    TemperatureKelvin,
    T72WeightedAverage
  )) %>%
  select(-c(RowNumber, T72WeightedAverage))


weather_data_cold <-
  tibble::rowid_to_column(weather_data_cold, "RowNumber")

weather_data_cold  <-
  weather_data_cold %>% mutate(T72WeightedAverage = round(rollmeanr(TemperatureKelvin,
                                                                    72,
                                                                    fill = NA), 2))

weather_data_cold_until_71 <-
  weather_data_cold %>% filter(RowNumber < 72) %>%
  mutate(CumSumTemperatureKelvin = cumsum(TemperatureKelvin)) %>%
  mutate(T72WeightedAverage = round(CumSumTemperatureKelvin / RowNumber, 2)) %>%
  select(-c(CumSumTemperatureKelvin))

weather_data_cold <- weather_data_cold %>%
  filter(RowNumber >= 72)

weather_data_cold <-
  rbind(weather_data_cold_until_71, weather_data_cold)

rm(weather_data_cold_until_71)

weather_data_cold <- weather_data_cold %>%
  mutate(TMix = ifelse(
    substr(Time, 7, 8) %in% c("10", "11", "12", "13", "14", "15", "16"),
    TemperatureKelvin,
    T72WeightedAverage
  )) %>%
  select(-c(RowNumber, T72WeightedAverage))


weather_data_hot <-
  tibble::rowid_to_column(weather_data_hot, "RowNumber")

weather_data_hot  <-
  weather_data_hot %>% mutate(T72WeightedAverage = round(rollmeanr(TemperatureKelvin,
                                                                   72,
                                                                   fill = NA), 2))

weather_data_hot_until_71 <-
  weather_data_hot %>% filter(RowNumber < 72) %>%
  mutate(CumSumTemperatureKelvin = cumsum(TemperatureKelvin)) %>%
  mutate(T72WeightedAverage = round(CumSumTemperatureKelvin / RowNumber, 2)) %>%
  select(-c(CumSumTemperatureKelvin))

weather_data_hot <- weather_data_hot %>%
  filter(RowNumber >= 72)

weather_data_hot <-
  rbind(weather_data_hot_until_71, weather_data_hot)

rm(weather_data_hot_until_71)

weather_data_hot <- weather_data_hot %>%
  mutate(TMix = ifelse(
    substr(Time, 7, 8) %in% c("10", "11", "12", "13", "14", "15", "16"),
    TemperatureKelvin,
    T72WeightedAverage
  )) %>%
  select(-c(RowNumber, T72WeightedAverage))


# Calculate the flow temperature
# Assumption: the slope of the heating curve for determining the flow temperature ranges between 0.3 (minimum) - 1.6 (maximum)
# The degree of the slope per building is determined by the maximum space heating demand per m2
# https://www.viessmann.de/de/wissen/anleitungen-und-tipps/heizkurve-einstellen.html
# The level is zero and the target room temperature 293.15K
# Calculation of flow temperatur based on:
# https://www.viessmann-community.com/t5/Gas/Mathematische-Formel-fuer-Vorlauftemperatur-aus-den-vier/td-p/68843#:~:text=Zu%20dem%20Ansatz%20V%20%3D%20T,den%20Angaben%20der%20Anlage%20%C3%BCbereinstimmen.&text=Gel%C3%B6st!
eh_combined_heat_demand_avg <- eh_combined_heat_demand_avg %>%
  mutate(
    Max_SpaceHeat_beginn_1918_avg  = max(SpaceHeat_beginn_1918_avg),
    Max_SpaceHeat_1919_1948_avg = max(SpaceHeat_1919_1948_avg),
    Max_SpaceHeat_1949_1978_avg = max(SpaceHeat_1949_1978_avg),
    Max_SpaceHeat_1979_1986_avg = max(SpaceHeat_1979_1986_avg),
    Max_SpaceHeat_1987_1990_avg = max(SpaceHeat_1987_1990_avg),
    Max_SpaceHeat_1991_1995_avg = max(SpaceHeat_1991_1995_avg),
    Max_SpaceHeat_1996_2000_avg = max(SpaceHeat_1996_2000_avg),
    Max_SpaceHeat_2001_2011_avg = max(SpaceHeat_2001_2011_avg),
    Max_SpaceHeat_2012_2022_avg = max(SpaceHeat_2012_2022_avg),
    Max_SpaceHeat_2023_2030_avg = max(SpaceHeat_2023_2030_avg)
  )

minSpaceHeatPerM2 <-
  min(
    eh_combined_heat_demand_avg$Max_SpaceHeat_beginn_1918_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1919_1948_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1949_1978_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1979_1986_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1987_1990_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1991_1995_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1996_2000_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_2001_2011_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_2012_2022_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_2023_2030_avg
  )

maxSpaceHeatPerM2 <-
  max(
    eh_combined_heat_demand_avg$Max_SpaceHeat_beginn_1918_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1919_1948_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1949_1978_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1979_1986_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1987_1990_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1991_1995_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_1996_2000_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_2001_2011_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_2012_2022_avg,
    eh_combined_heat_demand_avg$Max_SpaceHeat_2023_2030_avg
  )

minSlope <- 0.3

maxSlope <- 1.6

slope_function <- function(x) {
  return (0.3 + (x - minSpaceHeatPerM2) * ((maxSlope - minSlope) / (maxSpaceHeatPerM2 - minSpaceHeatPerM2)))
}

eh_combined_heat_demand_avg <- eh_combined_heat_demand_avg %>%
  mutate(
    Slope_SpaceHeat_beginn_1918_avg = slope_function(Max_SpaceHeat_beginn_1918_avg),
    Slope_SpaceHeat_1919_1948_avg = slope_function(Max_SpaceHeat_1919_1948_avg),
    Slope_SpaceHeat_1949_1978_avg = slope_function(Max_SpaceHeat_1949_1978_avg),
    Slope_SpaceHeat_1979_1986_avg = slope_function(Max_SpaceHeat_1979_1986_avg),
    Slope_SpaceHeat_1987_1990_avg = slope_function(Max_SpaceHeat_1987_1990_avg),
    Slope_SpaceHeat_1991_1995_avg = slope_function(Max_SpaceHeat_1991_1995_avg),
    Slope_SpaceHeat_1996_2000_avg = slope_function(Max_SpaceHeat_1996_2000_avg),
    Slope_SpaceHeat_2001_2011_avg = slope_function(Max_SpaceHeat_2001_2011_avg),
    Slope_SpaceHeat_2012_2022_avg = slope_function(Max_SpaceHeat_2012_2022_avg),
    Slope_SpaceHeat_2023_2030_avg = slope_function(Max_SpaceHeat_2023_2030_avg)
  )


