# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")
library(zoo)

# Read the heat demand data
eh_combined_heat_demand_reference <-
  read_csv2("data/output/heatdemand/eh_combined_heat_demand_reference.csv")

eh_combined_heat_demand_cold <-
  read_csv2("data/output/heatdemand/eh_combined_heat_demand_cold.csv")

eh_combined_heat_demand_hot <-
  read_csv2("data/output/heatdemand/eh_combined_heat_demand_hot.csv")

mh_combined_heat_demand_reference <-
  read_csv2("data/output/heatdemand/mh_combined_heat_demand_reference.csv")

mh_combined_heat_demand_cold <-
  read_csv2("data/output/heatdemand/mh_combined_heat_demand_cold.csv")

mh_combined_heat_demand_hot <-
  read_csv2("data/output/heatdemand/mh_combined_heat_demand_hot.csv")


# Read the weather data
weather_data_reference <-
  read_csv2("data/output/weathermodel/year2017.csv") %>%
  mutate(
    TemperatureKelvin = as.numeric(RoundedMeanTemperature) + 273.15,
    Time = paste(paste(substr(Date, 3, 4), substr(Date, 1, 2), sep = "-"),
                 paste(substr(Date, 5, 6), "00", sep = ":"))
  ) %>%
  select(-c(Date, MeanTemperature, RoundedMeanTemperature))

weather_data_cold <-
  read_csv2("data/output/weathermodel/year2010.csv") %>%
  mutate(
    TemperatureKelvin = as.numeric(RoundedMeanTemperature) + 273.15,
    Time = paste(paste(substr(Date, 3, 4), substr(Date, 1, 2), sep = "-"),
                 paste(substr(Date, 5, 6), "00", sep = ":"))
  ) %>%
  select(-c(Date, MeanTemperature, RoundedMeanTemperature))

weather_data_hot <-
  read_csv2("data/output/weathermodel/year2022.csv") %>%
  mutate(
    TemperatureKelvin = as.numeric(RoundedMeanTemperature) + 273.15,
    Time = paste(paste(substr(Date, 3, 4), substr(Date, 1, 2), sep = "-"),
                 paste(substr(Date, 5, 6), "00", sep = ":"))
  ) %>%
  select(-c(Date, MeanTemperature, RoundedMeanTemperature))


# Define the soil temperature
soil_temperature <- 283.15


# Calculate the hourly mixed temperature of the weather data
weather_data_reference <-
  tibble::rowid_to_column(weather_data_reference, "RowNumber")

weather_data_reference  <-
  weather_data_reference %>% mutate(T72WeightedAverage = rollmeanr(TemperatureKelvin,
                                                                   72,
                                                                   fill = NA))

weather_data_reference_until_71 <-
  weather_data_reference %>% filter(RowNumber < 72) %>%
  mutate(CumSumTemperatureKelvin = cumsum(TemperatureKelvin)) %>%
  mutate(T72WeightedAverage = (CumSumTemperatureKelvin / RowNumber)) %>%
  select(-c(CumSumTemperatureKelvin))

weather_data_reference <- weather_data_reference %>%
  filter(RowNumber >= 72)

weather_data_reference <-
  rbind(weather_data_reference_until_71, weather_data_reference)

rm(weather_data_reference_until_71)

weather_data_reference <- weather_data_reference %>%
  mutate(TMix = ifelse(
    substr(Time, 7, 8) %in% c("10", "11", "12", "13", "14", "15", "16"),
    TemperatureKelvin,
    T72WeightedAverage
  )) %>%
  select(-c(RowNumber, T72WeightedAverage))


weather_data_cold <-
  tibble::rowid_to_column(weather_data_cold, "RowNumber")

weather_data_cold  <-
  weather_data_cold %>% mutate(T72WeightedAverage = rollmeanr(TemperatureKelvin,
                                                              72,
                                                              fill = NA))

weather_data_cold_until_71 <-
  weather_data_cold %>% filter(RowNumber < 72) %>%
  mutate(CumSumTemperatureKelvin = cumsum(TemperatureKelvin)) %>%
  mutate(T72WeightedAverage = (CumSumTemperatureKelvin / RowNumber)) %>%
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
  weather_data_hot %>% mutate(T72WeightedAverage = rollmeanr(TemperatureKelvin,
                                                             72,
                                                             fill = NA))

weather_data_hot_until_71 <-
  weather_data_hot %>% filter(RowNumber < 72) %>%
  mutate(CumSumTemperatureKelvin = cumsum(TemperatureKelvin)) %>%
  mutate(T72WeightedAverage = (CumSumTemperatureKelvin / RowNumber)) %>%
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
# Calculation of flow temperature based on:
# https://www.viessmann-community.com/t5/Gas/Mathematische-Formel-fuer-Vorlauftemperatur-aus-den-vier/td-p/68843#:~:text=Zu%20dem%20Ansatz%20V%20%3D%20T,den%20Angaben%20der%20Anlage%20%C3%BCbereinstimmen.&text=Gel%C3%B6st!
eh_combined_heat_demand_reference <- eh_combined_heat_demand_reference %>%
  mutate(
    Max_SpaceHeat_beginn_1918_reference  = max(SpaceHeat_beginn_1918_reference),
    Max_SpaceHeat_1919_1948_reference = max(SpaceHeat_1919_1948_reference),
    Max_SpaceHeat_1949_1978_reference = max(SpaceHeat_1949_1978_reference),
    Max_SpaceHeat_1979_1986_reference = max(SpaceHeat_1979_1986_reference),
    Max_SpaceHeat_1987_1990_reference = max(SpaceHeat_1987_1990_reference),
    Max_SpaceHeat_1991_1995_reference = max(SpaceHeat_1991_1995_reference),
    Max_SpaceHeat_1996_2000_reference = max(SpaceHeat_1996_2000_reference),
    Max_SpaceHeat_2001_2011_reference = max(SpaceHeat_2001_2011_reference),
    Max_SpaceHeat_2012_2022_reference = max(SpaceHeat_2012_2022_reference),
    Max_SpaceHeat_2023_2030_reference = max(SpaceHeat_2023_2030_reference)
  )

eh_min_space_heat_per_m2 <-
  min(
    eh_combined_heat_demand_reference$Max_SpaceHeat_beginn_1918_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1919_1948_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1949_1978_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1979_1986_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1987_1990_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1991_1995_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1996_2000_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2001_2011_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2012_2022_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2023_2030_reference
  )

eh_max_space_heat_per_m2 <-
  max(
    eh_combined_heat_demand_reference$Max_SpaceHeat_beginn_1918_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1919_1948_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1949_1978_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1979_1986_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1987_1990_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1991_1995_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1996_2000_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2001_2011_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2012_2022_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2023_2030_reference
  )

# Separate calculation for multi-family houses, since larger losses
mh_combined_heat_demand_reference <- mh_combined_heat_demand_reference %>%
  mutate(
    Max_SpaceHeat_beginn_1918_reference  = max(SpaceHeat_beginn_1918_reference),
    Max_SpaceHeat_1919_1948_reference = max(SpaceHeat_1919_1948_reference),
    Max_SpaceHeat_1949_1978_reference = max(SpaceHeat_1949_1978_reference),
    Max_SpaceHeat_1979_1986_reference = max(SpaceHeat_1979_1986_reference),
    Max_SpaceHeat_1987_1990_reference = max(SpaceHeat_1987_1990_reference),
    Max_SpaceHeat_1991_1995_reference = max(SpaceHeat_1991_1995_reference),
    Max_SpaceHeat_1996_2000_reference = max(SpaceHeat_1996_2000_reference),
    Max_SpaceHeat_2001_2011_reference = max(SpaceHeat_2001_2011_reference),
    Max_SpaceHeat_2012_2022_reference = max(SpaceHeat_2012_2022_reference),
    Max_SpaceHeat_2023_2030_reference = max(SpaceHeat_2023_2030_reference)
  )

mh_min_space_heat_per_m2 <-
  min(
    mh_combined_heat_demand_reference$Max_SpaceHeat_beginn_1918_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1919_1948_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1949_1978_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1979_1986_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1987_1990_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1991_1995_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1996_2000_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2001_2011_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2012_2022_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2023_2030_reference
  )

mh_max_space_heat_per_m2 <-
  max(
    mh_combined_heat_demand_reference$Max_SpaceHeat_beginn_1918_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1919_1948_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1949_1978_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1979_1986_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1987_1990_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1991_1995_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1996_2000_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2001_2011_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2012_2022_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2023_2030_reference
  )

min_slope <- 0.3

max_slope <- 1.6

slope_function <-
  function(x,
           min_space_heat_per_m2,
           max_space_heat_per_m2) {
    return (min_slope + (x - min_space_heat_per_m2) * ((max_slope - min_slope) / (max_space_heat_per_m2 - min_space_heat_per_m2)
    ))
  }

eh_combined_slopes <- eh_combined_heat_demand_reference %>%
  mutate(
    Slope_SpaceHeat_beginn_1918 = slope_function(
      Max_SpaceHeat_beginn_1918_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1919_1948 = slope_function(
      Max_SpaceHeat_1919_1948_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1949_1978 = slope_function(
      Max_SpaceHeat_1949_1978_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1979_1986 = slope_function(
      Max_SpaceHeat_1979_1986_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1987_1990 = slope_function(
      Max_SpaceHeat_1987_1990_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1991_1995 = slope_function(
      Max_SpaceHeat_1991_1995_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1996_2000 = slope_function(
      Max_SpaceHeat_1996_2000_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2001_2011 = slope_function(
      Max_SpaceHeat_2001_2011_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2012_2022 = slope_function(
      Max_SpaceHeat_2012_2022_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2023_2030 = slope_function(
      Max_SpaceHeat_2023_2030_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    )
  ) %>%
  select(
    c(
      Time,
      Slope_SpaceHeat_beginn_1918,
      Slope_SpaceHeat_1919_1948,
      Slope_SpaceHeat_1949_1978,
      Slope_SpaceHeat_1979_1986,
      Slope_SpaceHeat_1987_1990,
      Slope_SpaceHeat_1991_1995,
      Slope_SpaceHeat_1996_2000,
      Slope_SpaceHeat_2001_2011,
      Slope_SpaceHeat_2012_2022,
      Slope_SpaceHeat_2023_2030
    )
  )

eh_combined_heat_demand_reference <- eh_combined_heat_demand_reference %>%
  select(
    -c(
      Max_SpaceHeat_beginn_1918_reference,
      Max_SpaceHeat_1919_1948_reference,
      Max_SpaceHeat_1949_1978_reference,
      Max_SpaceHeat_1979_1986_reference,
      Max_SpaceHeat_1987_1990_reference,
      Max_SpaceHeat_1991_1995_reference,
      Max_SpaceHeat_1996_2000_reference,
      Max_SpaceHeat_2001_2011_reference,
      Max_SpaceHeat_2012_2022_reference,
      Max_SpaceHeat_2023_2030_reference
    )
  )

mh_combined_slopes <- mh_combined_heat_demand_reference %>%
  mutate(
    Slope_SpaceHeat_beginn_1918 = slope_function(
      Max_SpaceHeat_beginn_1918_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1919_1948 = slope_function(
      Max_SpaceHeat_1919_1948_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1949_1978 = slope_function(
      Max_SpaceHeat_1949_1978_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1979_1986 = slope_function(
      Max_SpaceHeat_1979_1986_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1987_1990 = slope_function(
      Max_SpaceHeat_1987_1990_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1991_1995 = slope_function(
      Max_SpaceHeat_1991_1995_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1996_2000 = slope_function(
      Max_SpaceHeat_1996_2000_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2001_2011 = slope_function(
      Max_SpaceHeat_2001_2011_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2012_2022 = slope_function(
      Max_SpaceHeat_2012_2022_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2023_2030 = slope_function(
      Max_SpaceHeat_2023_2030_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    )
  ) %>%
  select(
    c(
      Time,
      Slope_SpaceHeat_beginn_1918,
      Slope_SpaceHeat_1919_1948,
      Slope_SpaceHeat_1949_1978,
      Slope_SpaceHeat_1979_1986,
      Slope_SpaceHeat_1987_1990,
      Slope_SpaceHeat_1991_1995,
      Slope_SpaceHeat_1996_2000,
      Slope_SpaceHeat_2001_2011,
      Slope_SpaceHeat_2012_2022,
      Slope_SpaceHeat_2023_2030
    )
  )

mh_combined_heat_demand_reference <- mh_combined_heat_demand_reference %>%
  select(
    -c(
      Max_SpaceHeat_beginn_1918_reference,
      Max_SpaceHeat_1919_1948_reference,
      Max_SpaceHeat_1949_1978_reference,
      Max_SpaceHeat_1979_1986_reference,
      Max_SpaceHeat_1987_1990_reference,
      Max_SpaceHeat_1991_1995_reference,
      Max_SpaceHeat_1996_2000_reference,
      Max_SpaceHeat_2001_2011_reference,
      Max_SpaceHeat_2012_2022_reference,
      Max_SpaceHeat_2023_2030_reference
    )
  )

room_target_temperature <- 293.15
level <- 0

# dar = Tmix - room_target_temperature
flow_temperature_function <- function(slope, dar) {
  return (room_target_temperature + level - slope * dar * (1.4347 + 0.021 * dar + 247.9 * 10 ^
                                                             (-6) * dar ^ 2))
}

# Remove space heat demand when temperature is higher 293.15K
eh_combined_heat_demand_reference <- eh_combined_heat_demand_reference %>%
  left_join(weather_data_reference, by = "Time")

eh_combined_heat_demand_cold <- eh_combined_heat_demand_cold %>%
  left_join(weather_data_cold, by = "Time")

eh_combined_heat_demand_hot <- eh_combined_heat_demand_hot %>%
  left_join(weather_data_hot, by = "Time")

mh_combined_heat_demand_reference <- mh_combined_heat_demand_reference %>%
  left_join(weather_data_reference, by = "Time")

mh_combined_heat_demand_cold <- mh_combined_heat_demand_cold %>%
  left_join(weather_data_cold, by = "Time")

mh_combined_heat_demand_hot <- mh_combined_heat_demand_hot %>%
  left_join(weather_data_hot, by = "Time")

eh_combined_heat_demand_reference <- eh_combined_heat_demand_reference %>%
  mutate(
    SpaceHeat_beginn_1918_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_beginn_1918_reference),
    SpaceHeat_1919_1948_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1919_1948_reference),
    SpaceHeat_1949_1978_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1949_1978_reference),
    SpaceHeat_1979_1986_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1979_1986_reference),
    SpaceHeat_1987_1990_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1987_1990_reference),
    SpaceHeat_1991_1995_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1991_1995_reference),
    SpaceHeat_1996_2000_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1996_2000_reference),
    SpaceHeat_2001_2011_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2001_2011_reference),
    SpaceHeat_2012_2022_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2012_2022_reference),
    SpaceHeat_2023_2030_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2023_2030_reference)
  )


eh_combined_heat_demand_cold <- eh_combined_heat_demand_cold %>%
  mutate(
    SpaceHeat_beginn_1918_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_beginn_1918_cold),
    SpaceHeat_1919_1948_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1919_1948_cold),
    SpaceHeat_1949_1978_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1949_1978_cold),
    SpaceHeat_1979_1986_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1979_1986_cold),
    SpaceHeat_1987_1990_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1987_1990_cold),
    SpaceHeat_1991_1995_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1991_1995_cold),
    SpaceHeat_1996_2000_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1996_2000_cold),
    SpaceHeat_2001_2011_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2001_2011_cold),
    SpaceHeat_2012_2022_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2012_2022_cold),
    SpaceHeat_2023_2030_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2023_2030_cold)
  )

eh_combined_heat_demand_hot <- eh_combined_heat_demand_hot %>%
  mutate(
    SpaceHeat_beginn_1918_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_beginn_1918_hot),
    SpaceHeat_1919_1948_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1919_1948_hot),
    SpaceHeat_1949_1978_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1949_1978_hot),
    SpaceHeat_1979_1986_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1979_1986_hot),
    SpaceHeat_1987_1990_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1987_1990_hot),
    SpaceHeat_1991_1995_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1991_1995_hot),
    SpaceHeat_1996_2000_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1996_2000_hot),
    SpaceHeat_2001_2011_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2001_2011_hot),
    SpaceHeat_2012_2022_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2012_2022_hot),
    SpaceHeat_2023_2030_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2023_2030_hot)
  )

mh_combined_heat_demand_reference <- mh_combined_heat_demand_reference %>%
  mutate(
    SpaceHeat_beginn_1918_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_beginn_1918_reference),
    SpaceHeat_1919_1948_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1919_1948_reference),
    SpaceHeat_1949_1978_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1949_1978_reference),
    SpaceHeat_1979_1986_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1979_1986_reference),
    SpaceHeat_1987_1990_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1987_1990_reference),
    SpaceHeat_1991_1995_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1991_1995_reference),
    SpaceHeat_1996_2000_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1996_2000_reference),
    SpaceHeat_2001_2011_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2001_2011_reference),
    SpaceHeat_2012_2022_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2012_2022_reference),
    SpaceHeat_2023_2030_reference = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2023_2030_reference)
  )


mh_combined_heat_demand_cold <- mh_combined_heat_demand_cold %>%
  mutate(
    SpaceHeat_beginn_1918_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_beginn_1918_cold),
    SpaceHeat_1919_1948_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1919_1948_cold),
    SpaceHeat_1949_1978_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1949_1978_cold),
    SpaceHeat_1979_1986_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1979_1986_cold),
    SpaceHeat_1987_1990_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1987_1990_cold),
    SpaceHeat_1991_1995_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1991_1995_cold),
    SpaceHeat_1996_2000_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1996_2000_cold),
    SpaceHeat_2001_2011_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2001_2011_cold),
    SpaceHeat_2012_2022_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2012_2022_cold),
    SpaceHeat_2023_2030_cold = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2023_2030_cold)
  )

mh_combined_heat_demand_hot <- mh_combined_heat_demand_hot %>%
  mutate(
    SpaceHeat_beginn_1918_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_beginn_1918_hot),
    SpaceHeat_1919_1948_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1919_1948_hot),
    SpaceHeat_1949_1978_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1949_1978_hot),
    SpaceHeat_1979_1986_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1979_1986_hot),
    SpaceHeat_1987_1990_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1987_1990_hot),
    SpaceHeat_1991_1995_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1991_1995_hot),
    SpaceHeat_1996_2000_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_1996_2000_hot),
    SpaceHeat_2001_2011_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2001_2011_hot),
    SpaceHeat_2012_2022_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2012_2022_hot),
    SpaceHeat_2023_2030_hot = ifelse(TMix >= room_target_temperature, 0,  SpaceHeat_2023_2030_hot)
  )


# Calculate the flow temperature
hot_water_temperature_eh <- 323
hot_water_temperature_mh <- 373

eh_combined_heat_demand_reference <- eh_combined_heat_demand_reference %>%
  mutate(Dar = TMix - room_target_temperature) %>%
  left_join(eh_combined_slopes, by = "Time") %>%
  mutate(
    FlowTemp_SpaceHeat_beginn_1918_reference = flow_temperature_function(Slope_SpaceHeat_beginn_1918, Dar),
    FlowTemp_SpaceHeat_1919_1948_reference = flow_temperature_function(Slope_SpaceHeat_1919_1948, Dar),
    FlowTemp_SpaceHeat_1949_1978_reference = flow_temperature_function(Slope_SpaceHeat_1949_1978, Dar),
    FlowTemp_SpaceHeat_1979_1986_reference = flow_temperature_function(Slope_SpaceHeat_1979_1986, Dar),
    FlowTemp_SpaceHeat_1987_1990_reference = flow_temperature_function(Slope_SpaceHeat_1987_1990, Dar),
    FlowTemp_SpaceHeat_1991_1995_reference = flow_temperature_function(Slope_SpaceHeat_1991_1995, Dar),
    FlowTemp_SpaceHeat_1996_2000_reference = flow_temperature_function(Slope_SpaceHeat_1996_2000, Dar),
    FlowTemp_SpaceHeat_2001_2011_reference = flow_temperature_function(Slope_SpaceHeat_2001_2011, Dar),
    FlowTemp_SpaceHeat_2012_2022_reference = flow_temperature_function(Slope_SpaceHeat_2012_2022, Dar),
    FlowTemp_SpaceHeat_2023_2030_reference = flow_temperature_function(Slope_SpaceHeat_2023_2030, Dar),
    Share_SpaceHeat_beginn_1918_reference = SpaceHeat_beginn_1918_reference / (SpaceHeat_beginn_1918_reference + HotWater_reference),
    Share_SpaceHeat_1919_1948_reference = SpaceHeat_1919_1948_reference / (SpaceHeat_1919_1948_reference + HotWater_reference),
    Share_SpaceHeat_1949_1978_reference = SpaceHeat_1949_1978_reference / (SpaceHeat_1949_1978_reference + HotWater_reference),
    Share_SpaceHeat_1979_1986_reference = SpaceHeat_1979_1986_reference / (SpaceHeat_1979_1986_reference + HotWater_reference),
    Share_SpaceHeat_1987_1990_reference = SpaceHeat_1987_1990_reference / (SpaceHeat_1987_1990_reference + HotWater_reference),
    Share_SpaceHeat_1991_1995_reference = SpaceHeat_1991_1995_reference / (SpaceHeat_1991_1995_reference + HotWater_reference),
    Share_SpaceHeat_1996_2000_reference = SpaceHeat_1996_2000_reference / (SpaceHeat_1996_2000_reference + HotWater_reference),
    Share_SpaceHeat_2001_2011_reference = SpaceHeat_2001_2011_reference / (SpaceHeat_2001_2011_reference + HotWater_reference),
    Share_SpaceHeat_2012_2022_reference = SpaceHeat_2012_2022_reference / (SpaceHeat_2012_2022_reference + HotWater_reference),
    Share_SpaceHeat_2023_2030_reference = SpaceHeat_2023_2030_reference / (SpaceHeat_2023_2030_reference + HotWater_reference),
    FlowTemp_beginn_1918_reference = Share_SpaceHeat_beginn_1918_reference * FlowTemp_SpaceHeat_beginn_1918_reference + (1 - Share_SpaceHeat_beginn_1918_reference) * hot_water_temperature_eh,
    FlowTemp_1919_1948_reference = Share_SpaceHeat_1919_1948_reference * FlowTemp_SpaceHeat_1919_1948_reference + (1 - Share_SpaceHeat_1919_1948_reference) * hot_water_temperature_eh,
    FlowTemp_1949_1978_reference = Share_SpaceHeat_1949_1978_reference * FlowTemp_SpaceHeat_1949_1978_reference + (1 - Share_SpaceHeat_1949_1978_reference) * hot_water_temperature_eh,
    FlowTemp_1979_1986_reference = Share_SpaceHeat_1979_1986_reference * FlowTemp_SpaceHeat_1979_1986_reference + (1 - Share_SpaceHeat_1979_1986_reference) * hot_water_temperature_eh,
    FlowTemp_1987_1990_reference = Share_SpaceHeat_1987_1990_reference * FlowTemp_SpaceHeat_1987_1990_reference + (1 - Share_SpaceHeat_1987_1990_reference) * hot_water_temperature_eh,
    FlowTemp_1991_1995_reference = Share_SpaceHeat_1991_1995_reference * FlowTemp_SpaceHeat_1991_1995_reference + (1 - Share_SpaceHeat_1991_1995_reference) * hot_water_temperature_eh,
    FlowTemp_1996_2000_reference = Share_SpaceHeat_1996_2000_reference * FlowTemp_SpaceHeat_1996_2000_reference + (1 - Share_SpaceHeat_1996_2000_reference) * hot_water_temperature_eh,
    FlowTemp_2001_2011_reference = Share_SpaceHeat_2001_2011_reference * FlowTemp_SpaceHeat_2001_2011_reference + (1 - Share_SpaceHeat_2001_2011_reference) * hot_water_temperature_eh,
    FlowTemp_2012_2022_reference = Share_SpaceHeat_2012_2022_reference * FlowTemp_SpaceHeat_2012_2022_reference + (1 - Share_SpaceHeat_2012_2022_reference) * hot_water_temperature_eh,
    FlowTemp_2023_2030_reference = Share_SpaceHeat_2023_2030_reference * FlowTemp_SpaceHeat_2023_2030_reference + (1 - Share_SpaceHeat_2023_2030_reference) * hot_water_temperature_eh
  ) %>%
  select(
    -c(
      Slope_SpaceHeat_beginn_1918,
      Slope_SpaceHeat_1919_1948,
      Slope_SpaceHeat_1949_1978,
      Slope_SpaceHeat_1979_1986,
      Slope_SpaceHeat_1987_1990,
      Slope_SpaceHeat_1991_1995,
      Slope_SpaceHeat_1996_2000,
      Slope_SpaceHeat_2001_2011,
      Slope_SpaceHeat_2012_2022,
      Slope_SpaceHeat_2023_2030,
      FlowTemp_SpaceHeat_beginn_1918_reference,
      FlowTemp_SpaceHeat_1919_1948_reference,
      FlowTemp_SpaceHeat_1949_1978_reference,
      FlowTemp_SpaceHeat_1979_1986_reference,
      FlowTemp_SpaceHeat_1987_1990_reference,
      FlowTemp_SpaceHeat_1991_1995_reference,
      FlowTemp_SpaceHeat_1996_2000_reference,
      FlowTemp_SpaceHeat_2001_2011_reference,
      FlowTemp_SpaceHeat_2012_2022_reference,
      FlowTemp_SpaceHeat_2023_2030_reference,
      Share_SpaceHeat_beginn_1918_reference,
      Share_SpaceHeat_1919_1948_reference,
      Share_SpaceHeat_1949_1978_reference,
      Share_SpaceHeat_1979_1986_reference,
      Share_SpaceHeat_1987_1990_reference,
      Share_SpaceHeat_1991_1995_reference,
      Share_SpaceHeat_1996_2000_reference,
      Share_SpaceHeat_2001_2011_reference,
      Share_SpaceHeat_2012_2022_reference,
      Share_SpaceHeat_2023_2030_reference,
      Dar
    )
  )

eh_combined_heat_demand_cold <- eh_combined_heat_demand_cold %>%
  mutate(Dar = TMix - room_target_temperature) %>%
  left_join(eh_combined_slopes, by = "Time") %>%
  mutate(
    FlowTemp_SpaceHeat_beginn_1918_cold = flow_temperature_function(Slope_SpaceHeat_beginn_1918, Dar),
    FlowTemp_SpaceHeat_1919_1948_cold = flow_temperature_function(Slope_SpaceHeat_1919_1948, Dar),
    FlowTemp_SpaceHeat_1949_1978_cold = flow_temperature_function(Slope_SpaceHeat_1949_1978, Dar),
    FlowTemp_SpaceHeat_1979_1986_cold = flow_temperature_function(Slope_SpaceHeat_1979_1986, Dar),
    FlowTemp_SpaceHeat_1987_1990_cold = flow_temperature_function(Slope_SpaceHeat_1987_1990, Dar),
    FlowTemp_SpaceHeat_1991_1995_cold = flow_temperature_function(Slope_SpaceHeat_1991_1995, Dar),
    FlowTemp_SpaceHeat_1996_2000_cold = flow_temperature_function(Slope_SpaceHeat_1996_2000, Dar),
    FlowTemp_SpaceHeat_2001_2011_cold = flow_temperature_function(Slope_SpaceHeat_2001_2011, Dar),
    FlowTemp_SpaceHeat_2012_2022_cold = flow_temperature_function(Slope_SpaceHeat_2012_2022, Dar),
    FlowTemp_SpaceHeat_2023_2030_cold = flow_temperature_function(Slope_SpaceHeat_2023_2030, Dar),
    Share_SpaceHeat_beginn_1918_cold = SpaceHeat_beginn_1918_cold / (SpaceHeat_beginn_1918_cold + HotWater_cold),
    Share_SpaceHeat_1919_1948_cold = SpaceHeat_1919_1948_cold / (SpaceHeat_1919_1948_cold + HotWater_cold),
    Share_SpaceHeat_1949_1978_cold = SpaceHeat_1949_1978_cold / (SpaceHeat_1949_1978_cold + HotWater_cold),
    Share_SpaceHeat_1979_1986_cold = SpaceHeat_1979_1986_cold / (SpaceHeat_1979_1986_cold + HotWater_cold),
    Share_SpaceHeat_1987_1990_cold = SpaceHeat_1987_1990_cold / (SpaceHeat_1987_1990_cold + HotWater_cold),
    Share_SpaceHeat_1991_1995_cold = SpaceHeat_1991_1995_cold / (SpaceHeat_1991_1995_cold + HotWater_cold),
    Share_SpaceHeat_1996_2000_cold = SpaceHeat_1996_2000_cold / (SpaceHeat_1996_2000_cold + HotWater_cold),
    Share_SpaceHeat_2001_2011_cold = SpaceHeat_2001_2011_cold / (SpaceHeat_2001_2011_cold + HotWater_cold),
    Share_SpaceHeat_2012_2022_cold = SpaceHeat_2012_2022_cold / (SpaceHeat_2012_2022_cold + HotWater_cold),
    Share_SpaceHeat_2023_2030_cold = SpaceHeat_2023_2030_cold / (SpaceHeat_2023_2030_cold + HotWater_cold),
    FlowTemp_beginn_1918_cold = Share_SpaceHeat_beginn_1918_cold * FlowTemp_SpaceHeat_beginn_1918_cold + (1 - Share_SpaceHeat_beginn_1918_cold) * hot_water_temperature_eh,
    FlowTemp_1919_1948_cold = Share_SpaceHeat_1919_1948_cold * FlowTemp_SpaceHeat_1919_1948_cold + (1 - Share_SpaceHeat_1919_1948_cold) * hot_water_temperature_eh,
    FlowTemp_1949_1978_cold = Share_SpaceHeat_1949_1978_cold * FlowTemp_SpaceHeat_1949_1978_cold + (1 - Share_SpaceHeat_1949_1978_cold) * hot_water_temperature_eh,
    FlowTemp_1979_1986_cold = Share_SpaceHeat_1979_1986_cold * FlowTemp_SpaceHeat_1979_1986_cold + (1 - Share_SpaceHeat_1979_1986_cold) * hot_water_temperature_eh,
    FlowTemp_1987_1990_cold = Share_SpaceHeat_1987_1990_cold * FlowTemp_SpaceHeat_1987_1990_cold + (1 - Share_SpaceHeat_1987_1990_cold) * hot_water_temperature_eh,
    FlowTemp_1991_1995_cold = Share_SpaceHeat_1991_1995_cold * FlowTemp_SpaceHeat_1991_1995_cold + (1 - Share_SpaceHeat_1991_1995_cold) * hot_water_temperature_eh,
    FlowTemp_1996_2000_cold = Share_SpaceHeat_1996_2000_cold * FlowTemp_SpaceHeat_1996_2000_cold + (1 - Share_SpaceHeat_1996_2000_cold) * hot_water_temperature_eh,
    FlowTemp_2001_2011_cold = Share_SpaceHeat_2001_2011_cold * FlowTemp_SpaceHeat_2001_2011_cold + (1 - Share_SpaceHeat_2001_2011_cold) * hot_water_temperature_eh,
    FlowTemp_2012_2022_cold = Share_SpaceHeat_2012_2022_cold * FlowTemp_SpaceHeat_2012_2022_cold + (1 - Share_SpaceHeat_2012_2022_cold) * hot_water_temperature_eh,
    FlowTemp_2023_2030_cold = Share_SpaceHeat_2023_2030_cold * FlowTemp_SpaceHeat_2023_2030_cold + (1 - Share_SpaceHeat_2023_2030_cold) * hot_water_temperature_eh
  ) %>%
  select(
    -c(
      Slope_SpaceHeat_beginn_1918,
      Slope_SpaceHeat_1919_1948,
      Slope_SpaceHeat_1949_1978,
      Slope_SpaceHeat_1979_1986,
      Slope_SpaceHeat_1987_1990,
      Slope_SpaceHeat_1991_1995,
      Slope_SpaceHeat_1996_2000,
      Slope_SpaceHeat_2001_2011,
      Slope_SpaceHeat_2012_2022,
      Slope_SpaceHeat_2023_2030,
      FlowTemp_SpaceHeat_beginn_1918_cold,
      FlowTemp_SpaceHeat_1919_1948_cold,
      FlowTemp_SpaceHeat_1949_1978_cold,
      FlowTemp_SpaceHeat_1979_1986_cold,
      FlowTemp_SpaceHeat_1987_1990_cold,
      FlowTemp_SpaceHeat_1991_1995_cold,
      FlowTemp_SpaceHeat_1996_2000_cold,
      FlowTemp_SpaceHeat_2001_2011_cold,
      FlowTemp_SpaceHeat_2012_2022_cold,
      FlowTemp_SpaceHeat_2023_2030_cold,
      Share_SpaceHeat_beginn_1918_cold,
      Share_SpaceHeat_1919_1948_cold,
      Share_SpaceHeat_1949_1978_cold,
      Share_SpaceHeat_1979_1986_cold,
      Share_SpaceHeat_1987_1990_cold,
      Share_SpaceHeat_1991_1995_cold,
      Share_SpaceHeat_1996_2000_cold,
      Share_SpaceHeat_2001_2011_cold,
      Share_SpaceHeat_2012_2022_cold,
      Share_SpaceHeat_2023_2030_cold,
      Dar
    )
  )

eh_combined_heat_demand_hot <- eh_combined_heat_demand_hot %>%
  mutate(Dar = TMix - room_target_temperature) %>%
  left_join(eh_combined_slopes, by = "Time") %>%
  mutate(
    FlowTemp_SpaceHeat_beginn_1918_hot = flow_temperature_function(Slope_SpaceHeat_beginn_1918, Dar),
    FlowTemp_SpaceHeat_1919_1948_hot = flow_temperature_function(Slope_SpaceHeat_1919_1948, Dar),
    FlowTemp_SpaceHeat_1949_1978_hot = flow_temperature_function(Slope_SpaceHeat_1949_1978, Dar),
    FlowTemp_SpaceHeat_1979_1986_hot = flow_temperature_function(Slope_SpaceHeat_1979_1986, Dar),
    FlowTemp_SpaceHeat_1987_1990_hot = flow_temperature_function(Slope_SpaceHeat_1987_1990, Dar),
    FlowTemp_SpaceHeat_1991_1995_hot = flow_temperature_function(Slope_SpaceHeat_1991_1995, Dar),
    FlowTemp_SpaceHeat_1996_2000_hot = flow_temperature_function(Slope_SpaceHeat_1996_2000, Dar),
    FlowTemp_SpaceHeat_2001_2011_hot = flow_temperature_function(Slope_SpaceHeat_2001_2011, Dar),
    FlowTemp_SpaceHeat_2012_2022_hot = flow_temperature_function(Slope_SpaceHeat_2012_2022, Dar),
    FlowTemp_SpaceHeat_2023_2030_hot = flow_temperature_function(Slope_SpaceHeat_2023_2030, Dar),
    Share_SpaceHeat_beginn_1918_hot = SpaceHeat_beginn_1918_hot / (SpaceHeat_beginn_1918_hot + HotWater_hot),
    Share_SpaceHeat_1919_1948_hot = SpaceHeat_1919_1948_hot / (SpaceHeat_1919_1948_hot + HotWater_hot),
    Share_SpaceHeat_1949_1978_hot = SpaceHeat_1949_1978_hot / (SpaceHeat_1949_1978_hot + HotWater_hot),
    Share_SpaceHeat_1979_1986_hot = SpaceHeat_1979_1986_hot / (SpaceHeat_1979_1986_hot + HotWater_hot),
    Share_SpaceHeat_1987_1990_hot = SpaceHeat_1987_1990_hot / (SpaceHeat_1987_1990_hot + HotWater_hot),
    Share_SpaceHeat_1991_1995_hot = SpaceHeat_1991_1995_hot / (SpaceHeat_1991_1995_hot + HotWater_hot),
    Share_SpaceHeat_1996_2000_hot = SpaceHeat_1996_2000_hot / (SpaceHeat_1996_2000_hot + HotWater_hot),
    Share_SpaceHeat_2001_2011_hot = SpaceHeat_2001_2011_hot / (SpaceHeat_2001_2011_hot + HotWater_hot),
    Share_SpaceHeat_2012_2022_hot = SpaceHeat_2012_2022_hot / (SpaceHeat_2012_2022_hot + HotWater_hot),
    Share_SpaceHeat_2023_2030_hot = SpaceHeat_2023_2030_hot / (SpaceHeat_2023_2030_hot + HotWater_hot),
    FlowTemp_beginn_1918_hot = Share_SpaceHeat_beginn_1918_hot * FlowTemp_SpaceHeat_beginn_1918_hot + (1 - Share_SpaceHeat_beginn_1918_hot) * hot_water_temperature_eh,
    FlowTemp_1919_1948_hot = Share_SpaceHeat_1919_1948_hot * FlowTemp_SpaceHeat_1919_1948_hot + (1 - Share_SpaceHeat_1919_1948_hot) * hot_water_temperature_eh,
    FlowTemp_1949_1978_hot = Share_SpaceHeat_1949_1978_hot * FlowTemp_SpaceHeat_1949_1978_hot + (1 - Share_SpaceHeat_1949_1978_hot) * hot_water_temperature_eh,
    FlowTemp_1979_1986_hot = Share_SpaceHeat_1979_1986_hot * FlowTemp_SpaceHeat_1979_1986_hot + (1 - Share_SpaceHeat_1979_1986_hot) * hot_water_temperature_eh,
    FlowTemp_1987_1990_hot = Share_SpaceHeat_1987_1990_hot * FlowTemp_SpaceHeat_1987_1990_hot + (1 - Share_SpaceHeat_1987_1990_hot) * hot_water_temperature_eh,
    FlowTemp_1991_1995_hot = Share_SpaceHeat_1991_1995_hot * FlowTemp_SpaceHeat_1991_1995_hot + (1 - Share_SpaceHeat_1991_1995_hot) * hot_water_temperature_eh,
    FlowTemp_1996_2000_hot = Share_SpaceHeat_1996_2000_hot * FlowTemp_SpaceHeat_1996_2000_hot + (1 - Share_SpaceHeat_1996_2000_hot) * hot_water_temperature_eh,
    FlowTemp_2001_2011_hot = Share_SpaceHeat_2001_2011_hot * FlowTemp_SpaceHeat_2001_2011_hot + (1 - Share_SpaceHeat_2001_2011_hot) * hot_water_temperature_eh,
    FlowTemp_2012_2022_hot = Share_SpaceHeat_2012_2022_hot * FlowTemp_SpaceHeat_2012_2022_hot + (1 - Share_SpaceHeat_2012_2022_hot) * hot_water_temperature_eh,
    FlowTemp_2023_2030_hot = Share_SpaceHeat_2023_2030_hot * FlowTemp_SpaceHeat_2023_2030_hot + (1 - Share_SpaceHeat_2023_2030_hot) * hot_water_temperature_eh
  ) %>%
  select(
    -c(
      Slope_SpaceHeat_beginn_1918,
      Slope_SpaceHeat_1919_1948,
      Slope_SpaceHeat_1949_1978,
      Slope_SpaceHeat_1979_1986,
      Slope_SpaceHeat_1987_1990,
      Slope_SpaceHeat_1991_1995,
      Slope_SpaceHeat_1996_2000,
      Slope_SpaceHeat_2001_2011,
      Slope_SpaceHeat_2012_2022,
      Slope_SpaceHeat_2023_2030,
      FlowTemp_SpaceHeat_beginn_1918_hot,
      FlowTemp_SpaceHeat_1919_1948_hot,
      FlowTemp_SpaceHeat_1949_1978_hot,
      FlowTemp_SpaceHeat_1979_1986_hot,
      FlowTemp_SpaceHeat_1987_1990_hot,
      FlowTemp_SpaceHeat_1991_1995_hot,
      FlowTemp_SpaceHeat_1996_2000_hot,
      FlowTemp_SpaceHeat_2001_2011_hot,
      FlowTemp_SpaceHeat_2012_2022_hot,
      FlowTemp_SpaceHeat_2023_2030_hot,
      Share_SpaceHeat_beginn_1918_hot,
      Share_SpaceHeat_1919_1948_hot,
      Share_SpaceHeat_1949_1978_hot,
      Share_SpaceHeat_1979_1986_hot,
      Share_SpaceHeat_1987_1990_hot,
      Share_SpaceHeat_1991_1995_hot,
      Share_SpaceHeat_1996_2000_hot,
      Share_SpaceHeat_2001_2011_hot,
      Share_SpaceHeat_2012_2022_hot,
      Share_SpaceHeat_2023_2030_hot,
      Dar
    )
  )

mh_combined_heat_demand_reference <- mh_combined_heat_demand_reference %>%
  mutate(Dar = TMix - room_target_temperature) %>%
  left_join(mh_combined_slopes, by = "Time") %>%
  mutate(
    FlowTemp_SpaceHeat_beginn_1918_reference = flow_temperature_function(Slope_SpaceHeat_beginn_1918, Dar),
    FlowTemp_SpaceHeat_1919_1948_reference = flow_temperature_function(Slope_SpaceHeat_1919_1948, Dar),
    FlowTemp_SpaceHeat_1949_1978_reference = flow_temperature_function(Slope_SpaceHeat_1949_1978, Dar),
    FlowTemp_SpaceHeat_1979_1986_reference = flow_temperature_function(Slope_SpaceHeat_1979_1986, Dar),
    FlowTemp_SpaceHeat_1987_1990_reference = flow_temperature_function(Slope_SpaceHeat_1987_1990, Dar),
    FlowTemp_SpaceHeat_1991_1995_reference = flow_temperature_function(Slope_SpaceHeat_1991_1995, Dar),
    FlowTemp_SpaceHeat_1996_2000_reference = flow_temperature_function(Slope_SpaceHeat_1996_2000, Dar),
    FlowTemp_SpaceHeat_2001_2011_reference = flow_temperature_function(Slope_SpaceHeat_2001_2011, Dar),
    FlowTemp_SpaceHeat_2012_2022_reference = flow_temperature_function(Slope_SpaceHeat_2012_2022, Dar),
    FlowTemp_SpaceHeat_2023_2030_reference = flow_temperature_function(Slope_SpaceHeat_2023_2030, Dar),
    Share_SpaceHeat_beginn_1918_reference = SpaceHeat_beginn_1918_reference / (SpaceHeat_beginn_1918_reference + HotWater_reference),
    Share_SpaceHeat_1919_1948_reference = SpaceHeat_1919_1948_reference / (SpaceHeat_1919_1948_reference + HotWater_reference),
    Share_SpaceHeat_1949_1978_reference = SpaceHeat_1949_1978_reference / (SpaceHeat_1949_1978_reference + HotWater_reference),
    Share_SpaceHeat_1979_1986_reference = SpaceHeat_1979_1986_reference / (SpaceHeat_1979_1986_reference + HotWater_reference),
    Share_SpaceHeat_1987_1990_reference = SpaceHeat_1987_1990_reference / (SpaceHeat_1987_1990_reference + HotWater_reference),
    Share_SpaceHeat_1991_1995_reference = SpaceHeat_1991_1995_reference / (SpaceHeat_1991_1995_reference + HotWater_reference),
    Share_SpaceHeat_1996_2000_reference = SpaceHeat_1996_2000_reference / (SpaceHeat_1996_2000_reference + HotWater_reference),
    Share_SpaceHeat_2001_2011_reference = SpaceHeat_2001_2011_reference / (SpaceHeat_2001_2011_reference + HotWater_reference),
    Share_SpaceHeat_2012_2022_reference = SpaceHeat_2012_2022_reference / (SpaceHeat_2012_2022_reference + HotWater_reference),
    Share_SpaceHeat_2023_2030_reference = SpaceHeat_2023_2030_reference / (SpaceHeat_2023_2030_reference + HotWater_reference),
    FlowTemp_beginn_1918_reference = Share_SpaceHeat_beginn_1918_reference * FlowTemp_SpaceHeat_beginn_1918_reference + (1 - Share_SpaceHeat_beginn_1918_reference) * hot_water_temperature_mh,
    FlowTemp_1919_1948_reference = Share_SpaceHeat_1919_1948_reference * FlowTemp_SpaceHeat_1919_1948_reference + (1 - Share_SpaceHeat_1919_1948_reference) * hot_water_temperature_mh,
    FlowTemp_1949_1978_reference = Share_SpaceHeat_1949_1978_reference * FlowTemp_SpaceHeat_1949_1978_reference + (1 - Share_SpaceHeat_1949_1978_reference) * hot_water_temperature_mh,
    FlowTemp_1979_1986_reference = Share_SpaceHeat_1979_1986_reference * FlowTemp_SpaceHeat_1979_1986_reference + (1 - Share_SpaceHeat_1979_1986_reference) * hot_water_temperature_mh,
    FlowTemp_1987_1990_reference = Share_SpaceHeat_1987_1990_reference * FlowTemp_SpaceHeat_1987_1990_reference + (1 - Share_SpaceHeat_1987_1990_reference) * hot_water_temperature_mh,
    FlowTemp_1991_1995_reference = Share_SpaceHeat_1991_1995_reference * FlowTemp_SpaceHeat_1991_1995_reference + (1 - Share_SpaceHeat_1991_1995_reference) * hot_water_temperature_mh,
    FlowTemp_1996_2000_reference = Share_SpaceHeat_1996_2000_reference * FlowTemp_SpaceHeat_1996_2000_reference + (1 - Share_SpaceHeat_1996_2000_reference) * hot_water_temperature_mh,
    FlowTemp_2001_2011_reference = Share_SpaceHeat_2001_2011_reference * FlowTemp_SpaceHeat_2001_2011_reference + (1 - Share_SpaceHeat_2001_2011_reference) * hot_water_temperature_mh,
    FlowTemp_2012_2022_reference = Share_SpaceHeat_2012_2022_reference * FlowTemp_SpaceHeat_2012_2022_reference + (1 - Share_SpaceHeat_2012_2022_reference) * hot_water_temperature_mh,
    FlowTemp_2023_2030_reference = Share_SpaceHeat_2023_2030_reference * FlowTemp_SpaceHeat_2023_2030_reference + (1 - Share_SpaceHeat_2023_2030_reference) * hot_water_temperature_mh
  ) %>%
  select(
    -c(
      Slope_SpaceHeat_beginn_1918,
      Slope_SpaceHeat_1919_1948,
      Slope_SpaceHeat_1949_1978,
      Slope_SpaceHeat_1979_1986,
      Slope_SpaceHeat_1987_1990,
      Slope_SpaceHeat_1991_1995,
      Slope_SpaceHeat_1996_2000,
      Slope_SpaceHeat_2001_2011,
      Slope_SpaceHeat_2012_2022,
      Slope_SpaceHeat_2023_2030,
      FlowTemp_SpaceHeat_beginn_1918_reference,
      FlowTemp_SpaceHeat_1919_1948_reference,
      FlowTemp_SpaceHeat_1949_1978_reference,
      FlowTemp_SpaceHeat_1979_1986_reference,
      FlowTemp_SpaceHeat_1987_1990_reference,
      FlowTemp_SpaceHeat_1991_1995_reference,
      FlowTemp_SpaceHeat_1996_2000_reference,
      FlowTemp_SpaceHeat_2001_2011_reference,
      FlowTemp_SpaceHeat_2012_2022_reference,
      FlowTemp_SpaceHeat_2023_2030_reference,
      Share_SpaceHeat_beginn_1918_reference,
      Share_SpaceHeat_1919_1948_reference,
      Share_SpaceHeat_1949_1978_reference,
      Share_SpaceHeat_1979_1986_reference,
      Share_SpaceHeat_1987_1990_reference,
      Share_SpaceHeat_1991_1995_reference,
      Share_SpaceHeat_1996_2000_reference,
      Share_SpaceHeat_2001_2011_reference,
      Share_SpaceHeat_2012_2022_reference,
      Share_SpaceHeat_2023_2030_reference,
      Dar
    )
  )

mh_combined_heat_demand_cold <- mh_combined_heat_demand_cold %>%
  mutate(Dar = TMix - room_target_temperature) %>%
  left_join(mh_combined_slopes, by = "Time") %>%
  mutate(
    FlowTemp_SpaceHeat_beginn_1918_cold = flow_temperature_function(Slope_SpaceHeat_beginn_1918, Dar),
    FlowTemp_SpaceHeat_1919_1948_cold = flow_temperature_function(Slope_SpaceHeat_1919_1948, Dar),
    FlowTemp_SpaceHeat_1949_1978_cold = flow_temperature_function(Slope_SpaceHeat_1949_1978, Dar),
    FlowTemp_SpaceHeat_1979_1986_cold = flow_temperature_function(Slope_SpaceHeat_1979_1986, Dar),
    FlowTemp_SpaceHeat_1987_1990_cold = flow_temperature_function(Slope_SpaceHeat_1987_1990, Dar),
    FlowTemp_SpaceHeat_1991_1995_cold = flow_temperature_function(Slope_SpaceHeat_1991_1995, Dar),
    FlowTemp_SpaceHeat_1996_2000_cold = flow_temperature_function(Slope_SpaceHeat_1996_2000, Dar),
    FlowTemp_SpaceHeat_2001_2011_cold = flow_temperature_function(Slope_SpaceHeat_2001_2011, Dar),
    FlowTemp_SpaceHeat_2012_2022_cold = flow_temperature_function(Slope_SpaceHeat_2012_2022, Dar),
    FlowTemp_SpaceHeat_2023_2030_cold = flow_temperature_function(Slope_SpaceHeat_2023_2030, Dar),
    Share_SpaceHeat_beginn_1918_cold = SpaceHeat_beginn_1918_cold / (SpaceHeat_beginn_1918_cold + HotWater_cold),
    Share_SpaceHeat_1919_1948_cold = SpaceHeat_1919_1948_cold / (SpaceHeat_1919_1948_cold + HotWater_cold),
    Share_SpaceHeat_1949_1978_cold = SpaceHeat_1949_1978_cold / (SpaceHeat_1949_1978_cold + HotWater_cold),
    Share_SpaceHeat_1979_1986_cold = SpaceHeat_1979_1986_cold / (SpaceHeat_1979_1986_cold + HotWater_cold),
    Share_SpaceHeat_1987_1990_cold = SpaceHeat_1987_1990_cold / (SpaceHeat_1987_1990_cold + HotWater_cold),
    Share_SpaceHeat_1991_1995_cold = SpaceHeat_1991_1995_cold / (SpaceHeat_1991_1995_cold + HotWater_cold),
    Share_SpaceHeat_1996_2000_cold = SpaceHeat_1996_2000_cold / (SpaceHeat_1996_2000_cold + HotWater_cold),
    Share_SpaceHeat_2001_2011_cold = SpaceHeat_2001_2011_cold / (SpaceHeat_2001_2011_cold + HotWater_cold),
    Share_SpaceHeat_2012_2022_cold = SpaceHeat_2012_2022_cold / (SpaceHeat_2012_2022_cold + HotWater_cold),
    Share_SpaceHeat_2023_2030_cold = SpaceHeat_2023_2030_cold / (SpaceHeat_2023_2030_cold + HotWater_cold),
    FlowTemp_beginn_1918_cold = Share_SpaceHeat_beginn_1918_cold * FlowTemp_SpaceHeat_beginn_1918_cold + (1 - Share_SpaceHeat_beginn_1918_cold) * hot_water_temperature_mh,
    FlowTemp_1919_1948_cold = Share_SpaceHeat_1919_1948_cold * FlowTemp_SpaceHeat_1919_1948_cold + (1 - Share_SpaceHeat_1919_1948_cold) * hot_water_temperature_mh,
    FlowTemp_1949_1978_cold = Share_SpaceHeat_1949_1978_cold * FlowTemp_SpaceHeat_1949_1978_cold + (1 - Share_SpaceHeat_1949_1978_cold) * hot_water_temperature_mh,
    FlowTemp_1979_1986_cold = Share_SpaceHeat_1979_1986_cold * FlowTemp_SpaceHeat_1979_1986_cold + (1 - Share_SpaceHeat_1979_1986_cold) * hot_water_temperature_mh,
    FlowTemp_1987_1990_cold = Share_SpaceHeat_1987_1990_cold * FlowTemp_SpaceHeat_1987_1990_cold + (1 - Share_SpaceHeat_1987_1990_cold) * hot_water_temperature_mh,
    FlowTemp_1991_1995_cold = Share_SpaceHeat_1991_1995_cold * FlowTemp_SpaceHeat_1991_1995_cold + (1 - Share_SpaceHeat_1991_1995_cold) * hot_water_temperature_mh,
    FlowTemp_1996_2000_cold = Share_SpaceHeat_1996_2000_cold * FlowTemp_SpaceHeat_1996_2000_cold + (1 - Share_SpaceHeat_1996_2000_cold) * hot_water_temperature_mh,
    FlowTemp_2001_2011_cold = Share_SpaceHeat_2001_2011_cold * FlowTemp_SpaceHeat_2001_2011_cold + (1 - Share_SpaceHeat_2001_2011_cold) * hot_water_temperature_mh,
    FlowTemp_2012_2022_cold = Share_SpaceHeat_2012_2022_cold * FlowTemp_SpaceHeat_2012_2022_cold + (1 - Share_SpaceHeat_2012_2022_cold) * hot_water_temperature_mh,
    FlowTemp_2023_2030_cold = Share_SpaceHeat_2023_2030_cold * FlowTemp_SpaceHeat_2023_2030_cold + (1 - Share_SpaceHeat_2023_2030_cold) * hot_water_temperature_mh
  ) %>%
  select(
    -c(
      Slope_SpaceHeat_beginn_1918,
      Slope_SpaceHeat_1919_1948,
      Slope_SpaceHeat_1949_1978,
      Slope_SpaceHeat_1979_1986,
      Slope_SpaceHeat_1987_1990,
      Slope_SpaceHeat_1991_1995,
      Slope_SpaceHeat_1996_2000,
      Slope_SpaceHeat_2001_2011,
      Slope_SpaceHeat_2012_2022,
      Slope_SpaceHeat_2023_2030,
      FlowTemp_SpaceHeat_beginn_1918_cold,
      FlowTemp_SpaceHeat_1919_1948_cold,
      FlowTemp_SpaceHeat_1949_1978_cold,
      FlowTemp_SpaceHeat_1979_1986_cold,
      FlowTemp_SpaceHeat_1987_1990_cold,
      FlowTemp_SpaceHeat_1991_1995_cold,
      FlowTemp_SpaceHeat_1996_2000_cold,
      FlowTemp_SpaceHeat_2001_2011_cold,
      FlowTemp_SpaceHeat_2012_2022_cold,
      FlowTemp_SpaceHeat_2023_2030_cold,
      Share_SpaceHeat_beginn_1918_cold,
      Share_SpaceHeat_1919_1948_cold,
      Share_SpaceHeat_1949_1978_cold,
      Share_SpaceHeat_1979_1986_cold,
      Share_SpaceHeat_1987_1990_cold,
      Share_SpaceHeat_1991_1995_cold,
      Share_SpaceHeat_1996_2000_cold,
      Share_SpaceHeat_2001_2011_cold,
      Share_SpaceHeat_2012_2022_cold,
      Share_SpaceHeat_2023_2030_cold,
      Dar
    )
  )

mh_combined_heat_demand_hot <- mh_combined_heat_demand_hot %>%
  mutate(Dar = TMix - room_target_temperature) %>%
  left_join(mh_combined_slopes, by = "Time") %>%
  mutate(
    FlowTemp_SpaceHeat_beginn_1918_hot = flow_temperature_function(Slope_SpaceHeat_beginn_1918, Dar),
    FlowTemp_SpaceHeat_1919_1948_hot = flow_temperature_function(Slope_SpaceHeat_1919_1948, Dar),
    FlowTemp_SpaceHeat_1949_1978_hot = flow_temperature_function(Slope_SpaceHeat_1949_1978, Dar),
    FlowTemp_SpaceHeat_1979_1986_hot = flow_temperature_function(Slope_SpaceHeat_1979_1986, Dar),
    FlowTemp_SpaceHeat_1987_1990_hot = flow_temperature_function(Slope_SpaceHeat_1987_1990, Dar),
    FlowTemp_SpaceHeat_1991_1995_hot = flow_temperature_function(Slope_SpaceHeat_1991_1995, Dar),
    FlowTemp_SpaceHeat_1996_2000_hot = flow_temperature_function(Slope_SpaceHeat_1996_2000, Dar),
    FlowTemp_SpaceHeat_2001_2011_hot = flow_temperature_function(Slope_SpaceHeat_2001_2011, Dar),
    FlowTemp_SpaceHeat_2012_2022_hot = flow_temperature_function(Slope_SpaceHeat_2012_2022, Dar),
    FlowTemp_SpaceHeat_2023_2030_hot = flow_temperature_function(Slope_SpaceHeat_2023_2030, Dar),
    Share_SpaceHeat_beginn_1918_hot = SpaceHeat_beginn_1918_hot / (SpaceHeat_beginn_1918_hot + HotWater_hot),
    Share_SpaceHeat_1919_1948_hot = SpaceHeat_1919_1948_hot / (SpaceHeat_1919_1948_hot + HotWater_hot),
    Share_SpaceHeat_1949_1978_hot = SpaceHeat_1949_1978_hot / (SpaceHeat_1949_1978_hot + HotWater_hot),
    Share_SpaceHeat_1979_1986_hot = SpaceHeat_1979_1986_hot / (SpaceHeat_1979_1986_hot + HotWater_hot),
    Share_SpaceHeat_1987_1990_hot = SpaceHeat_1987_1990_hot / (SpaceHeat_1987_1990_hot + HotWater_hot),
    Share_SpaceHeat_1991_1995_hot = SpaceHeat_1991_1995_hot / (SpaceHeat_1991_1995_hot + HotWater_hot),
    Share_SpaceHeat_1996_2000_hot = SpaceHeat_1996_2000_hot / (SpaceHeat_1996_2000_hot + HotWater_hot),
    Share_SpaceHeat_2001_2011_hot = SpaceHeat_2001_2011_hot / (SpaceHeat_2001_2011_hot + HotWater_hot),
    Share_SpaceHeat_2012_2022_hot = SpaceHeat_2012_2022_hot / (SpaceHeat_2012_2022_hot + HotWater_hot),
    Share_SpaceHeat_2023_2030_hot = SpaceHeat_2023_2030_hot / (SpaceHeat_2023_2030_hot + HotWater_hot),
    FlowTemp_beginn_1918_hot = Share_SpaceHeat_beginn_1918_hot * FlowTemp_SpaceHeat_beginn_1918_hot + (1 - Share_SpaceHeat_beginn_1918_hot) * hot_water_temperature_mh,
    FlowTemp_1919_1948_hot = Share_SpaceHeat_1919_1948_hot * FlowTemp_SpaceHeat_1919_1948_hot + (1 - Share_SpaceHeat_1919_1948_hot) * hot_water_temperature_mh,
    FlowTemp_1949_1978_hot = Share_SpaceHeat_1949_1978_hot * FlowTemp_SpaceHeat_1949_1978_hot + (1 - Share_SpaceHeat_1949_1978_hot) * hot_water_temperature_mh,
    FlowTemp_1979_1986_hot = Share_SpaceHeat_1979_1986_hot * FlowTemp_SpaceHeat_1979_1986_hot + (1 - Share_SpaceHeat_1979_1986_hot) * hot_water_temperature_mh,
    FlowTemp_1987_1990_hot = Share_SpaceHeat_1987_1990_hot * FlowTemp_SpaceHeat_1987_1990_hot + (1 - Share_SpaceHeat_1987_1990_hot) * hot_water_temperature_mh,
    FlowTemp_1991_1995_hot = Share_SpaceHeat_1991_1995_hot * FlowTemp_SpaceHeat_1991_1995_hot + (1 - Share_SpaceHeat_1991_1995_hot) * hot_water_temperature_mh,
    FlowTemp_1996_2000_hot = Share_SpaceHeat_1996_2000_hot * FlowTemp_SpaceHeat_1996_2000_hot + (1 - Share_SpaceHeat_1996_2000_hot) * hot_water_temperature_mh,
    FlowTemp_2001_2011_hot = Share_SpaceHeat_2001_2011_hot * FlowTemp_SpaceHeat_2001_2011_hot + (1 - Share_SpaceHeat_2001_2011_hot) * hot_water_temperature_mh,
    FlowTemp_2012_2022_hot = Share_SpaceHeat_2012_2022_hot * FlowTemp_SpaceHeat_2012_2022_hot + (1 - Share_SpaceHeat_2012_2022_hot) * hot_water_temperature_mh,
    FlowTemp_2023_2030_hot = Share_SpaceHeat_2023_2030_hot * FlowTemp_SpaceHeat_2023_2030_hot + (1 - Share_SpaceHeat_2023_2030_hot) * hot_water_temperature_mh
  ) %>%
  select(
    -c(
      Slope_SpaceHeat_beginn_1918,
      Slope_SpaceHeat_1919_1948,
      Slope_SpaceHeat_1949_1978,
      Slope_SpaceHeat_1979_1986,
      Slope_SpaceHeat_1987_1990,
      Slope_SpaceHeat_1991_1995,
      Slope_SpaceHeat_1996_2000,
      Slope_SpaceHeat_2001_2011,
      Slope_SpaceHeat_2012_2022,
      Slope_SpaceHeat_2023_2030,
      FlowTemp_SpaceHeat_beginn_1918_hot,
      FlowTemp_SpaceHeat_1919_1948_hot,
      FlowTemp_SpaceHeat_1949_1978_hot,
      FlowTemp_SpaceHeat_1979_1986_hot,
      FlowTemp_SpaceHeat_1987_1990_hot,
      FlowTemp_SpaceHeat_1991_1995_hot,
      FlowTemp_SpaceHeat_1996_2000_hot,
      FlowTemp_SpaceHeat_2001_2011_hot,
      FlowTemp_SpaceHeat_2012_2022_hot,
      FlowTemp_SpaceHeat_2023_2030_hot,
      Share_SpaceHeat_beginn_1918_hot,
      Share_SpaceHeat_1919_1948_hot,
      Share_SpaceHeat_1949_1978_hot,
      Share_SpaceHeat_1979_1986_hot,
      Share_SpaceHeat_1987_1990_hot,
      Share_SpaceHeat_1991_1995_hot,
      Share_SpaceHeat_1996_2000_hot,
      Share_SpaceHeat_2001_2011_hot,
      Share_SpaceHeat_2012_2022_hot,
      Share_SpaceHeat_2023_2030_hot,
      Dar
    )
  )


# Calculate the theoretical COP for the monovalent heat pump system
eh_combined_heat_demand_reference <- eh_combined_heat_demand_reference %>%
  mutate(
    COPHPTheo_beginn_1918_reference = FlowTemp_beginn_1918_reference / (FlowTemp_beginn_1918_reference - soil_temperature),
    COPHPTheo_1919_1948_reference = FlowTemp_1919_1948_reference / (FlowTemp_1919_1948_reference - soil_temperature),
    COPHPTheo_1949_1978_reference = FlowTemp_1949_1978_reference / (FlowTemp_1949_1978_reference - soil_temperature),
    COPHPTheo_1979_1986_reference = FlowTemp_1979_1986_reference / (FlowTemp_1979_1986_reference - soil_temperature),
    COPHPTheo_1987_1990_reference = FlowTemp_1987_1990_reference / (FlowTemp_1987_1990_reference - soil_temperature),
    COPHPTheo_1991_1995_reference = FlowTemp_1991_1995_reference / (FlowTemp_1991_1995_reference - soil_temperature),
    COPHPTheo_1996_2000_reference = FlowTemp_1996_2000_reference / (FlowTemp_1996_2000_reference - soil_temperature),
    COPHPTheo_2001_2011_reference = FlowTemp_2001_2011_reference / (FlowTemp_2001_2011_reference - soil_temperature),
    COPHPTheo_2012_2022_reference = FlowTemp_2012_2022_reference / (FlowTemp_2012_2022_reference - soil_temperature),
    COPHPTheo_2023_2030_reference = FlowTemp_2023_2030_reference / (FlowTemp_2023_2030_reference - soil_temperature)
  )

eh_combined_heat_demand_cold <- eh_combined_heat_demand_cold %>%
  mutate(
    COPHPTheo_beginn_1918_cold = FlowTemp_beginn_1918_cold / (FlowTemp_beginn_1918_cold - soil_temperature),
    COPHPTheo_1919_1948_cold = FlowTemp_1919_1948_cold / (FlowTemp_1919_1948_cold - soil_temperature),
    COPHPTheo_1949_1978_cold = FlowTemp_1949_1978_cold / (FlowTemp_1949_1978_cold - soil_temperature),
    COPHPTheo_1979_1986_cold = FlowTemp_1979_1986_cold / (FlowTemp_1979_1986_cold - soil_temperature),
    COPHPTheo_1987_1990_cold = FlowTemp_1987_1990_cold / (FlowTemp_1987_1990_cold - soil_temperature),
    COPHPTheo_1991_1995_cold = FlowTemp_1991_1995_cold / (FlowTemp_1991_1995_cold - soil_temperature),
    COPHPTheo_1996_2000_cold = FlowTemp_1996_2000_cold / (FlowTemp_1996_2000_cold - soil_temperature),
    COPHPTheo_2001_2011_cold = FlowTemp_2001_2011_cold / (FlowTemp_2001_2011_cold - soil_temperature),
    COPHPTheo_2012_2022_cold = FlowTemp_2012_2022_cold / (FlowTemp_2012_2022_cold - soil_temperature),
    COPHPTheo_2023_2030_cold = FlowTemp_2023_2030_cold / (FlowTemp_2023_2030_cold - soil_temperature)
  )

eh_combined_heat_demand_hot <- eh_combined_heat_demand_hot %>%
  mutate(
    COPHPTheo_beginn_1918_hot = FlowTemp_beginn_1918_hot / (FlowTemp_beginn_1918_hot - soil_temperature),
    COPHPTheo_1919_1948_hot = FlowTemp_1919_1948_hot / (FlowTemp_1919_1948_hot - soil_temperature),
    COPHPTheo_1949_1978_hot = FlowTemp_1949_1978_hot / (FlowTemp_1949_1978_hot - soil_temperature),
    COPHPTheo_1979_1986_hot = FlowTemp_1979_1986_hot / (FlowTemp_1979_1986_hot - soil_temperature),
    COPHPTheo_1987_1990_hot = FlowTemp_1987_1990_hot / (FlowTemp_1987_1990_hot - soil_temperature),
    COPHPTheo_1991_1995_hot = FlowTemp_1991_1995_hot / (FlowTemp_1991_1995_hot - soil_temperature),
    COPHPTheo_1996_2000_hot = FlowTemp_1996_2000_hot / (FlowTemp_1996_2000_hot - soil_temperature),
    COPHPTheo_2001_2011_hot = FlowTemp_2001_2011_hot / (FlowTemp_2001_2011_hot - soil_temperature),
    COPHPTheo_2012_2022_hot = FlowTemp_2012_2022_hot / (FlowTemp_2012_2022_hot - soil_temperature),
    COPHPTheo_2023_2030_hot = FlowTemp_2023_2030_hot / (FlowTemp_2023_2030_hot - soil_temperature)
  )


mh_combined_heat_demand_reference <- mh_combined_heat_demand_reference %>%
  mutate(
    COPHPTheo_beginn_1918_reference = FlowTemp_beginn_1918_reference / (FlowTemp_beginn_1918_reference - soil_temperature),
    COPHPTheo_1919_1948_reference = FlowTemp_1919_1948_reference / (FlowTemp_1919_1948_reference - soil_temperature),
    COPHPTheo_1949_1978_reference = FlowTemp_1949_1978_reference / (FlowTemp_1949_1978_reference - soil_temperature),
    COPHPTheo_1979_1986_reference = FlowTemp_1979_1986_reference / (FlowTemp_1979_1986_reference - soil_temperature),
    COPHPTheo_1987_1990_reference = FlowTemp_1987_1990_reference / (FlowTemp_1987_1990_reference - soil_temperature),
    COPHPTheo_1991_1995_reference = FlowTemp_1991_1995_reference / (FlowTemp_1991_1995_reference - soil_temperature),
    COPHPTheo_1996_2000_reference = FlowTemp_1996_2000_reference / (FlowTemp_1996_2000_reference - soil_temperature),
    COPHPTheo_2001_2011_reference = FlowTemp_2001_2011_reference / (FlowTemp_2001_2011_reference - soil_temperature),
    COPHPTheo_2012_2022_reference = FlowTemp_2012_2022_reference / (FlowTemp_2012_2022_reference - soil_temperature),
    COPHPTheo_2023_2030_reference = FlowTemp_2023_2030_reference / (FlowTemp_2023_2030_reference - soil_temperature)
  )

mh_combined_heat_demand_cold <- mh_combined_heat_demand_cold %>%
  mutate(
    COPHPTheo_beginn_1918_cold = FlowTemp_beginn_1918_cold / (FlowTemp_beginn_1918_cold - soil_temperature),
    COPHPTheo_1919_1948_cold = FlowTemp_1919_1948_cold / (FlowTemp_1919_1948_cold - soil_temperature),
    COPHPTheo_1949_1978_cold = FlowTemp_1949_1978_cold / (FlowTemp_1949_1978_cold - soil_temperature),
    COPHPTheo_1979_1986_cold = FlowTemp_1979_1986_cold / (FlowTemp_1979_1986_cold - soil_temperature),
    COPHPTheo_1987_1990_cold = FlowTemp_1987_1990_cold / (FlowTemp_1987_1990_cold - soil_temperature),
    COPHPTheo_1991_1995_cold = FlowTemp_1991_1995_cold / (FlowTemp_1991_1995_cold - soil_temperature),
    COPHPTheo_1996_2000_cold = FlowTemp_1996_2000_cold / (FlowTemp_1996_2000_cold - soil_temperature),
    COPHPTheo_2001_2011_cold = FlowTemp_2001_2011_cold / (FlowTemp_2001_2011_cold - soil_temperature),
    COPHPTheo_2012_2022_cold = FlowTemp_2012_2022_cold / (FlowTemp_2012_2022_cold - soil_temperature),
    COPHPTheo_2023_2030_cold = FlowTemp_2023_2030_cold / (FlowTemp_2023_2030_cold - soil_temperature)
  )

mh_combined_heat_demand_hot <- mh_combined_heat_demand_hot %>%
  mutate(
    COPHPTheo_beginn_1918_hot = FlowTemp_beginn_1918_hot / (FlowTemp_beginn_1918_hot - soil_temperature),
    COPHPTheo_1919_1948_hot = FlowTemp_1919_1948_hot / (FlowTemp_1919_1948_hot - soil_temperature),
    COPHPTheo_1949_1978_hot = FlowTemp_1949_1978_hot / (FlowTemp_1949_1978_hot - soil_temperature),
    COPHPTheo_1979_1986_hot = FlowTemp_1979_1986_hot / (FlowTemp_1979_1986_hot - soil_temperature),
    COPHPTheo_1987_1990_hot = FlowTemp_1987_1990_hot / (FlowTemp_1987_1990_hot - soil_temperature),
    COPHPTheo_1991_1995_hot = FlowTemp_1991_1995_hot / (FlowTemp_1991_1995_hot - soil_temperature),
    COPHPTheo_1996_2000_hot = FlowTemp_1996_2000_hot / (FlowTemp_1996_2000_hot - soil_temperature),
    COPHPTheo_2001_2011_hot = FlowTemp_2001_2011_hot / (FlowTemp_2001_2011_hot - soil_temperature),
    COPHPTheo_2012_2022_hot = FlowTemp_2012_2022_hot / (FlowTemp_2012_2022_hot - soil_temperature),
    COPHPTheo_2023_2030_hot = FlowTemp_2023_2030_hot / (FlowTemp_2023_2030_hot - soil_temperature)
  )

# Inclusion of efficiency losses
# Assume the Efficiency of the ASHP is 0.4 of the theoretical COP
# https://www.renewableinstitute.org/heat-pumps-reducing-losses-increasing-efficiency/
efficiency_heatpump <- 0.4

eh_combined_heat_demand_reference <- eh_combined_heat_demand_reference %>%
  mutate(
    COPHPReal_beginn_1918_reference = ifelse(COPHPTheo_beginn_1918_reference * efficiency_heatpump < 0 | COPHPTheo_beginn_1918_reference * efficiency_heatpump > 8, 8, COPHPTheo_beginn_1918_reference * efficiency_heatpump),
    COPHPReal_1919_1948_reference = ifelse(COPHPTheo_1919_1948_reference * efficiency_heatpump < 0 | COPHPTheo_1919_1948_reference * efficiency_heatpump > 8, 8, COPHPTheo_1919_1948_reference * efficiency_heatpump),
    COPHPReal_1949_1978_reference = ifelse(COPHPTheo_1949_1978_reference * efficiency_heatpump < 0 | COPHPTheo_1949_1978_reference * efficiency_heatpump > 8, 8, COPHPTheo_1949_1978_reference * efficiency_heatpump),
    COPHPReal_1979_1986_reference = ifelse(COPHPTheo_1979_1986_reference * efficiency_heatpump < 0 | COPHPTheo_1979_1986_reference * efficiency_heatpump > 8, 8, COPHPTheo_1979_1986_reference * efficiency_heatpump),
    COPHPReal_1987_1990_reference = ifelse(COPHPTheo_1987_1990_reference * efficiency_heatpump < 0 | COPHPTheo_1987_1990_reference * efficiency_heatpump > 8, 8, COPHPTheo_1987_1990_reference * efficiency_heatpump),
    COPHPReal_1991_1995_reference = ifelse(COPHPTheo_1991_1995_reference * efficiency_heatpump < 0 | COPHPTheo_1991_1995_reference * efficiency_heatpump > 8, 8, COPHPTheo_1991_1995_reference * efficiency_heatpump),
    COPHPReal_1996_2000_reference = ifelse(COPHPTheo_1996_2000_reference * efficiency_heatpump < 0 | COPHPTheo_1996_2000_reference * efficiency_heatpump > 8, 8, COPHPTheo_1996_2000_reference * efficiency_heatpump),
    COPHPReal_2001_2011_reference = ifelse(COPHPTheo_2001_2011_reference * efficiency_heatpump < 0 | COPHPTheo_2001_2011_reference * efficiency_heatpump > 8, 8, COPHPTheo_2001_2011_reference * efficiency_heatpump),
    COPHPReal_2012_2022_reference = ifelse(COPHPTheo_2012_2022_reference * efficiency_heatpump < 0 | COPHPTheo_2012_2022_reference * efficiency_heatpump > 8, 8, COPHPTheo_2012_2022_reference * efficiency_heatpump),
    COPHPReal_2023_2030_reference = ifelse(COPHPTheo_2023_2030_reference * efficiency_heatpump < 0 | COPHPTheo_2023_2030_reference * efficiency_heatpump > 8, 8, COPHPTheo_2023_2030_reference * efficiency_heatpump)
  ) %>%
  select(
    -c(
      COPHPTheo_beginn_1918_reference,
      COPHPTheo_1919_1948_reference,
      COPHPTheo_1949_1978_reference,
      COPHPTheo_1979_1986_reference,
      COPHPTheo_1987_1990_reference,
      COPHPTheo_1991_1995_reference,
      COPHPTheo_1996_2000_reference,
      COPHPTheo_2001_2011_reference,
      COPHPTheo_2012_2022_reference,
      COPHPTheo_2023_2030_reference
    )
  )

eh_combined_heat_demand_cold <- eh_combined_heat_demand_cold %>%
  mutate(
    COPHPReal_beginn_1918_cold = ifelse(COPHPTheo_beginn_1918_cold * efficiency_heatpump < 0 | COPHPTheo_beginn_1918_cold * efficiency_heatpump > 8, 8, COPHPTheo_beginn_1918_cold * efficiency_heatpump),
    COPHPReal_1919_1948_cold = ifelse(COPHPTheo_1919_1948_cold * efficiency_heatpump < 0 | COPHPTheo_1919_1948_cold * efficiency_heatpump > 8, 8, COPHPTheo_1919_1948_cold * efficiency_heatpump),
    COPHPReal_1949_1978_cold = ifelse(COPHPTheo_1949_1978_cold * efficiency_heatpump < 0 | COPHPTheo_1949_1978_cold * efficiency_heatpump > 8, 8, COPHPTheo_1949_1978_cold * efficiency_heatpump),
    COPHPReal_1979_1986_cold = ifelse(COPHPTheo_1979_1986_cold * efficiency_heatpump < 0 | COPHPTheo_1979_1986_cold * efficiency_heatpump > 8, 8, COPHPTheo_1979_1986_cold * efficiency_heatpump),
    COPHPReal_1987_1990_cold = ifelse(COPHPTheo_1987_1990_cold * efficiency_heatpump < 0 | COPHPTheo_1987_1990_cold * efficiency_heatpump > 8, 8, COPHPTheo_1987_1990_cold * efficiency_heatpump),
    COPHPReal_1991_1995_cold = ifelse(COPHPTheo_1991_1995_cold * efficiency_heatpump < 0 | COPHPTheo_1991_1995_cold * efficiency_heatpump > 8, 8, COPHPTheo_1991_1995_cold * efficiency_heatpump),
    COPHPReal_1996_2000_cold = ifelse(COPHPTheo_1996_2000_cold * efficiency_heatpump < 0 | COPHPTheo_1996_2000_cold * efficiency_heatpump > 8, 8, COPHPTheo_1996_2000_cold * efficiency_heatpump),
    COPHPReal_2001_2011_cold = ifelse(COPHPTheo_2001_2011_cold * efficiency_heatpump < 0 | COPHPTheo_2001_2011_cold * efficiency_heatpump > 8, 8, COPHPTheo_2001_2011_cold * efficiency_heatpump),
    COPHPReal_2012_2022_cold = ifelse(COPHPTheo_2012_2022_cold * efficiency_heatpump < 0 | COPHPTheo_2012_2022_cold * efficiency_heatpump > 8, 8, COPHPTheo_2012_2022_cold * efficiency_heatpump),
    COPHPReal_2023_2030_cold = ifelse(COPHPTheo_2023_2030_cold * efficiency_heatpump < 0 | COPHPTheo_2023_2030_cold * efficiency_heatpump > 8, 8, COPHPTheo_2023_2030_cold * efficiency_heatpump)
  ) %>%
  select(
    -c(
      COPHPTheo_beginn_1918_cold,
      COPHPTheo_1919_1948_cold,
      COPHPTheo_1949_1978_cold,
      COPHPTheo_1979_1986_cold,
      COPHPTheo_1987_1990_cold,
      COPHPTheo_1991_1995_cold,
      COPHPTheo_1996_2000_cold,
      COPHPTheo_2001_2011_cold,
      COPHPTheo_2012_2022_cold,
      COPHPTheo_2023_2030_cold
    )
  )

eh_combined_heat_demand_hot <- eh_combined_heat_demand_hot %>%
  mutate(
    COPHPReal_beginn_1918_hot = ifelse(COPHPTheo_beginn_1918_hot * efficiency_heatpump < 0 | COPHPTheo_beginn_1918_hot * efficiency_heatpump > 8, 8, COPHPTheo_beginn_1918_hot * efficiency_heatpump),
    COPHPReal_1919_1948_hot = ifelse(COPHPTheo_1919_1948_hot * efficiency_heatpump < 0 | COPHPTheo_1919_1948_hot * efficiency_heatpump > 8, 8, COPHPTheo_1919_1948_hot * efficiency_heatpump),
    COPHPReal_1949_1978_hot = ifelse(COPHPTheo_1949_1978_hot * efficiency_heatpump < 0 | COPHPTheo_1949_1978_hot * efficiency_heatpump > 8, 8, COPHPTheo_1949_1978_hot * efficiency_heatpump),
    COPHPReal_1979_1986_hot = ifelse(COPHPTheo_1979_1986_hot * efficiency_heatpump < 0 | COPHPTheo_1979_1986_hot * efficiency_heatpump > 8, 8, COPHPTheo_1979_1986_hot * efficiency_heatpump),
    COPHPReal_1987_1990_hot = ifelse(COPHPTheo_1987_1990_hot * efficiency_heatpump < 0 | COPHPTheo_1987_1990_hot * efficiency_heatpump > 8, 8, COPHPTheo_1987_1990_hot * efficiency_heatpump),
    COPHPReal_1991_1995_hot = ifelse(COPHPTheo_1991_1995_hot * efficiency_heatpump < 0 | COPHPTheo_1991_1995_hot * efficiency_heatpump > 8, 8, COPHPTheo_1991_1995_hot * efficiency_heatpump),
    COPHPReal_1996_2000_hot = ifelse(COPHPTheo_1996_2000_hot * efficiency_heatpump < 0 | COPHPTheo_1996_2000_hot * efficiency_heatpump > 8, 8, COPHPTheo_1996_2000_hot * efficiency_heatpump),
    COPHPReal_2001_2011_hot = ifelse(COPHPTheo_2001_2011_hot * efficiency_heatpump < 0 | COPHPTheo_2001_2011_hot * efficiency_heatpump > 8, 8, COPHPTheo_2001_2011_hot * efficiency_heatpump),
    COPHPReal_2012_2022_hot = ifelse(COPHPTheo_2012_2022_hot * efficiency_heatpump < 0 | COPHPTheo_2012_2022_hot * efficiency_heatpump > 8, 8, COPHPTheo_2012_2022_hot * efficiency_heatpump),
    COPHPReal_2023_2030_hot = ifelse(COPHPTheo_2023_2030_hot * efficiency_heatpump < 0 | COPHPTheo_2023_2030_hot * efficiency_heatpump > 8, 8, COPHPTheo_2023_2030_hot * efficiency_heatpump)
  ) %>%
  select(
    -c(
      COPHPTheo_beginn_1918_hot,
      COPHPTheo_1919_1948_hot,
      COPHPTheo_1949_1978_hot,
      COPHPTheo_1979_1986_hot,
      COPHPTheo_1987_1990_hot,
      COPHPTheo_1991_1995_hot,
      COPHPTheo_1996_2000_hot,
      COPHPTheo_2001_2011_hot,
      COPHPTheo_2012_2022_hot,
      COPHPTheo_2023_2030_hot
    )
  )


mh_combined_heat_demand_reference <- mh_combined_heat_demand_reference %>%
  mutate(
    COPHPReal_beginn_1918_reference = ifelse(COPHPTheo_beginn_1918_reference * efficiency_heatpump < 0 | COPHPTheo_beginn_1918_reference * efficiency_heatpump > 8, 8, COPHPTheo_beginn_1918_reference * efficiency_heatpump),
    COPHPReal_1919_1948_reference = ifelse(COPHPTheo_1919_1948_reference * efficiency_heatpump < 0 | COPHPTheo_1919_1948_reference * efficiency_heatpump > 8, 8, COPHPTheo_1919_1948_reference * efficiency_heatpump),
    COPHPReal_1949_1978_reference = ifelse(COPHPTheo_1949_1978_reference * efficiency_heatpump < 0 | COPHPTheo_1949_1978_reference * efficiency_heatpump > 8, 8, COPHPTheo_1949_1978_reference * efficiency_heatpump),
    COPHPReal_1979_1986_reference = ifelse(COPHPTheo_1979_1986_reference * efficiency_heatpump < 0 | COPHPTheo_1979_1986_reference * efficiency_heatpump > 8, 8, COPHPTheo_1979_1986_reference * efficiency_heatpump),
    COPHPReal_1987_1990_reference = ifelse(COPHPTheo_1987_1990_reference * efficiency_heatpump < 0 | COPHPTheo_1987_1990_reference * efficiency_heatpump > 8, 8, COPHPTheo_1987_1990_reference * efficiency_heatpump),
    COPHPReal_1991_1995_reference = ifelse(COPHPTheo_1991_1995_reference * efficiency_heatpump < 0 | COPHPTheo_1991_1995_reference * efficiency_heatpump > 8, 8, COPHPTheo_1991_1995_reference * efficiency_heatpump),
    COPHPReal_1996_2000_reference = ifelse(COPHPTheo_1996_2000_reference * efficiency_heatpump < 0 | COPHPTheo_1996_2000_reference * efficiency_heatpump > 8, 8, COPHPTheo_1996_2000_reference * efficiency_heatpump),
    COPHPReal_2001_2011_reference = ifelse(COPHPTheo_2001_2011_reference * efficiency_heatpump < 0 | COPHPTheo_2001_2011_reference * efficiency_heatpump > 8, 8, COPHPTheo_2001_2011_reference * efficiency_heatpump),
    COPHPReal_2012_2022_reference = ifelse(COPHPTheo_2012_2022_reference * efficiency_heatpump < 0 | COPHPTheo_2012_2022_reference * efficiency_heatpump > 8, 8, COPHPTheo_2012_2022_reference * efficiency_heatpump),
    COPHPReal_2023_2030_reference = ifelse(COPHPTheo_2023_2030_reference * efficiency_heatpump < 0 | COPHPTheo_2023_2030_reference * efficiency_heatpump > 8, 8, COPHPTheo_2023_2030_reference * efficiency_heatpump)
  ) %>%
  select(
    -c(
      COPHPTheo_beginn_1918_reference,
      COPHPTheo_1919_1948_reference,
      COPHPTheo_1949_1978_reference,
      COPHPTheo_1979_1986_reference,
      COPHPTheo_1987_1990_reference,
      COPHPTheo_1991_1995_reference,
      COPHPTheo_1996_2000_reference,
      COPHPTheo_2001_2011_reference,
      COPHPTheo_2012_2022_reference,
      COPHPTheo_2023_2030_reference
    )
  )

mh_combined_heat_demand_cold <- mh_combined_heat_demand_cold %>%
  mutate(
    COPHPReal_beginn_1918_cold = ifelse(COPHPTheo_beginn_1918_cold * efficiency_heatpump < 0 | COPHPTheo_beginn_1918_cold * efficiency_heatpump > 8, 8, COPHPTheo_beginn_1918_cold * efficiency_heatpump),
    COPHPReal_1919_1948_cold = ifelse(COPHPTheo_1919_1948_cold * efficiency_heatpump < 0 | COPHPTheo_1919_1948_cold * efficiency_heatpump > 8, 8, COPHPTheo_1919_1948_cold * efficiency_heatpump),
    COPHPReal_1949_1978_cold = ifelse(COPHPTheo_1949_1978_cold * efficiency_heatpump < 0 | COPHPTheo_1949_1978_cold * efficiency_heatpump > 8, 8, COPHPTheo_1949_1978_cold * efficiency_heatpump),
    COPHPReal_1979_1986_cold = ifelse(COPHPTheo_1979_1986_cold * efficiency_heatpump < 0 | COPHPTheo_1979_1986_cold * efficiency_heatpump > 8, 8, COPHPTheo_1979_1986_cold * efficiency_heatpump),
    COPHPReal_1987_1990_cold = ifelse(COPHPTheo_1987_1990_cold * efficiency_heatpump < 0 | COPHPTheo_1987_1990_cold * efficiency_heatpump > 8, 8, COPHPTheo_1987_1990_cold * efficiency_heatpump),
    COPHPReal_1991_1995_cold = ifelse(COPHPTheo_1991_1995_cold * efficiency_heatpump < 0 | COPHPTheo_1991_1995_cold * efficiency_heatpump > 8, 8, COPHPTheo_1991_1995_cold * efficiency_heatpump),
    COPHPReal_1996_2000_cold = ifelse(COPHPTheo_1996_2000_cold * efficiency_heatpump < 0 | COPHPTheo_1996_2000_cold * efficiency_heatpump > 8, 8, COPHPTheo_1996_2000_cold * efficiency_heatpump),
    COPHPReal_2001_2011_cold = ifelse(COPHPTheo_2001_2011_cold * efficiency_heatpump < 0 | COPHPTheo_2001_2011_cold * efficiency_heatpump > 8, 8, COPHPTheo_2001_2011_cold * efficiency_heatpump),
    COPHPReal_2012_2022_cold = ifelse(COPHPTheo_2012_2022_cold * efficiency_heatpump < 0 | COPHPTheo_2012_2022_cold * efficiency_heatpump > 8, 8, COPHPTheo_2012_2022_cold * efficiency_heatpump),
    COPHPReal_2023_2030_cold = ifelse(COPHPTheo_2023_2030_cold * efficiency_heatpump < 0 | COPHPTheo_2023_2030_cold * efficiency_heatpump > 8, 8, COPHPTheo_2023_2030_cold * efficiency_heatpump)
  ) %>%
  select(
    -c(
      COPHPTheo_beginn_1918_cold,
      COPHPTheo_1919_1948_cold,
      COPHPTheo_1949_1978_cold,
      COPHPTheo_1979_1986_cold,
      COPHPTheo_1987_1990_cold,
      COPHPTheo_1991_1995_cold,
      COPHPTheo_1996_2000_cold,
      COPHPTheo_2001_2011_cold,
      COPHPTheo_2012_2022_cold,
      COPHPTheo_2023_2030_cold
    )
  )

mh_combined_heat_demand_hot <- mh_combined_heat_demand_hot %>%
  mutate(
    COPHPReal_beginn_1918_hot = ifelse(COPHPTheo_beginn_1918_hot * efficiency_heatpump < 0 | COPHPTheo_beginn_1918_hot * efficiency_heatpump > 8, 8, COPHPTheo_beginn_1918_hot * efficiency_heatpump),
    COPHPReal_1919_1948_hot = ifelse(COPHPTheo_1919_1948_hot * efficiency_heatpump < 0 | COPHPTheo_1919_1948_hot * efficiency_heatpump > 8, 8, COPHPTheo_1919_1948_hot * efficiency_heatpump),
    COPHPReal_1949_1978_hot = ifelse(COPHPTheo_1949_1978_hot * efficiency_heatpump < 0 | COPHPTheo_1949_1978_hot * efficiency_heatpump > 8, 8, COPHPTheo_1949_1978_hot * efficiency_heatpump),
    COPHPReal_1979_1986_hot = ifelse(COPHPTheo_1979_1986_hot * efficiency_heatpump < 0 | COPHPTheo_1979_1986_hot * efficiency_heatpump > 8, 8, COPHPTheo_1979_1986_hot * efficiency_heatpump),
    COPHPReal_1987_1990_hot = ifelse(COPHPTheo_1987_1990_hot * efficiency_heatpump < 0 | COPHPTheo_1987_1990_hot * efficiency_heatpump > 8, 8, COPHPTheo_1987_1990_hot * efficiency_heatpump),
    COPHPReal_1991_1995_hot = ifelse(COPHPTheo_1991_1995_hot * efficiency_heatpump < 0 | COPHPTheo_1991_1995_hot * efficiency_heatpump > 8, 8, COPHPTheo_1991_1995_hot * efficiency_heatpump),
    COPHPReal_1996_2000_hot = ifelse(COPHPTheo_1996_2000_hot * efficiency_heatpump < 0 | COPHPTheo_1996_2000_hot * efficiency_heatpump > 8, 8, COPHPTheo_1996_2000_hot * efficiency_heatpump),
    COPHPReal_2001_2011_hot = ifelse(COPHPTheo_2001_2011_hot * efficiency_heatpump < 0 | COPHPTheo_2001_2011_hot * efficiency_heatpump > 8, 8, COPHPTheo_2001_2011_hot * efficiency_heatpump),
    COPHPReal_2012_2022_hot = ifelse(COPHPTheo_2012_2022_hot * efficiency_heatpump < 0 | COPHPTheo_2012_2022_hot * efficiency_heatpump > 8, 8, COPHPTheo_2012_2022_hot * efficiency_heatpump),
    COPHPReal_2023_2030_hot = ifelse(COPHPTheo_2023_2030_hot * efficiency_heatpump < 0 | COPHPTheo_2023_2030_hot * efficiency_heatpump > 8, 8, COPHPTheo_2023_2030_hot * efficiency_heatpump)
  ) %>%
  select(
    -c(
      COPHPTheo_beginn_1918_hot,
      COPHPTheo_1919_1948_hot,
      COPHPTheo_1949_1978_hot,
      COPHPTheo_1979_1986_hot,
      COPHPTheo_1987_1990_hot,
      COPHPTheo_1991_1995_hot,
      COPHPTheo_1996_2000_hot,
      COPHPTheo_2001_2011_hot,
      COPHPTheo_2012_2022_hot,
      COPHPTheo_2023_2030_hot
    )
  )


# Calculate the required electricity
eh_combined_heat_demand_reference <- eh_combined_heat_demand_reference %>%
  mutate(
    ElectricityDemand_beginn_1918_reference = (SpaceHeat_beginn_1918_reference + HotWater_reference) /
      COPHPReal_beginn_1918_reference,
    ElectricityDemand_1919_1948_reference = (SpaceHeat_1919_1948_reference + HotWater_reference) /
      COPHPReal_1919_1948_reference,
    ElectricityDemand_1949_1978_reference = (SpaceHeat_1949_1978_reference + HotWater_reference) /
      COPHPReal_1949_1978_reference,
    ElectricityDemand_1979_1986_reference = (SpaceHeat_1979_1986_reference + HotWater_reference) /
      COPHPReal_1979_1986_reference,
    ElectricityDemand_1987_1990_reference = (SpaceHeat_1987_1990_reference + HotWater_reference) /
      COPHPReal_1987_1990_reference,
    ElectricityDemand_1991_1995_reference = (SpaceHeat_1991_1995_reference + HotWater_reference) /
      COPHPReal_1991_1995_reference,
    ElectricityDemand_1996_2000_reference = (SpaceHeat_1996_2000_reference + HotWater_reference) /
      COPHPReal_1996_2000_reference,
    ElectricityDemand_2001_2011_reference = (SpaceHeat_2001_2011_reference + HotWater_reference) /
      COPHPReal_2001_2011_reference,
    ElectricityDemand_2012_2022_reference = (SpaceHeat_2012_2022_reference + HotWater_reference) /
      COPHPReal_2012_2022_reference,
    ElectricityDemand_2023_2030_reference = (SpaceHeat_2023_2030_reference + HotWater_reference) /
      COPHPReal_2023_2030_reference
  )

eh_combined_heat_demand_cold <- eh_combined_heat_demand_cold %>%
  mutate(
    ElectricityDemand_beginn_1918_cold = (SpaceHeat_beginn_1918_cold + HotWater_cold) /
      COPHPReal_beginn_1918_cold,
    ElectricityDemand_1919_1948_cold = (SpaceHeat_1919_1948_cold + HotWater_cold) /
      COPHPReal_1919_1948_cold,
    ElectricityDemand_1949_1978_cold = (SpaceHeat_1949_1978_cold + HotWater_cold) /
      COPHPReal_1949_1978_cold,
    ElectricityDemand_1979_1986_cold = (SpaceHeat_1979_1986_cold + HotWater_cold) /
      COPHPReal_1979_1986_cold,
    ElectricityDemand_1987_1990_cold = (SpaceHeat_1987_1990_cold + HotWater_cold) /
      COPHPReal_1987_1990_cold,
    ElectricityDemand_1991_1995_cold = (SpaceHeat_1991_1995_cold + HotWater_cold) /
      COPHPReal_1991_1995_cold,
    ElectricityDemand_1996_2000_cold = (SpaceHeat_1996_2000_cold + HotWater_cold) /
      COPHPReal_1996_2000_cold,
    ElectricityDemand_2001_2011_cold = (SpaceHeat_2001_2011_cold + HotWater_cold) /
      COPHPReal_2001_2011_cold,
    ElectricityDemand_2012_2022_cold = (SpaceHeat_2012_2022_cold + HotWater_cold) /
      COPHPReal_2012_2022_cold,
    ElectricityDemand_2023_2030_cold = (SpaceHeat_2023_2030_cold + HotWater_cold) /
      COPHPReal_2023_2030_cold
  )

eh_combined_heat_demand_hot <- eh_combined_heat_demand_hot %>%
  mutate(
    ElectricityDemand_beginn_1918_hot = (SpaceHeat_beginn_1918_hot + HotWater_hot) /
      COPHPReal_beginn_1918_hot,
    ElectricityDemand_1919_1948_hot = (SpaceHeat_1919_1948_hot + HotWater_hot) /
      COPHPReal_1919_1948_hot,
    ElectricityDemand_1949_1978_hot = (SpaceHeat_1949_1978_hot + HotWater_hot) /
      COPHPReal_1949_1978_hot,
    ElectricityDemand_1979_1986_hot = (SpaceHeat_1979_1986_hot + HotWater_hot) /
      COPHPReal_1979_1986_hot,
    ElectricityDemand_1987_1990_hot = (SpaceHeat_1987_1990_hot + HotWater_hot) /
      COPHPReal_1987_1990_hot,
    ElectricityDemand_1991_1995_hot = (SpaceHeat_1991_1995_hot + HotWater_hot) /
      COPHPReal_1991_1995_hot,
    ElectricityDemand_1996_2000_hot = (SpaceHeat_1996_2000_hot + HotWater_hot) /
      COPHPReal_1996_2000_hot,
    ElectricityDemand_2001_2011_hot = (SpaceHeat_2001_2011_hot + HotWater_hot) /
      COPHPReal_2001_2011_hot,
    ElectricityDemand_2012_2022_hot = (SpaceHeat_2012_2022_hot + HotWater_hot) /
      COPHPReal_2012_2022_hot,
    ElectricityDemand_2023_2030_hot = (SpaceHeat_2023_2030_hot + HotWater_hot) /
      COPHPReal_2023_2030_hot
  )


mh_combined_heat_demand_reference <- mh_combined_heat_demand_reference %>%
  mutate(
    ElectricityDemand_beginn_1918_reference = (SpaceHeat_beginn_1918_reference + HotWater_reference) /
      COPHPReal_beginn_1918_reference,
    ElectricityDemand_1919_1948_reference = (SpaceHeat_1919_1948_reference + HotWater_reference) /
      COPHPReal_1919_1948_reference,
    ElectricityDemand_1949_1978_reference = (SpaceHeat_1949_1978_reference + HotWater_reference) /
      COPHPReal_1949_1978_reference,
    ElectricityDemand_1979_1986_reference = (SpaceHeat_1979_1986_reference + HotWater_reference) /
      COPHPReal_1979_1986_reference,
    ElectricityDemand_1987_1990_reference = (SpaceHeat_1987_1990_reference + HotWater_reference) /
      COPHPReal_1987_1990_reference,
    ElectricityDemand_1991_1995_reference = (SpaceHeat_1991_1995_reference + HotWater_reference) /
      COPHPReal_1991_1995_reference,
    ElectricityDemand_1996_2000_reference = (SpaceHeat_1996_2000_reference + HotWater_reference) /
      COPHPReal_1996_2000_reference,
    ElectricityDemand_2001_2011_reference = (SpaceHeat_2001_2011_reference + HotWater_reference) /
      COPHPReal_2001_2011_reference,
    ElectricityDemand_2012_2022_reference = (SpaceHeat_2012_2022_reference + HotWater_reference) /
      COPHPReal_2012_2022_reference,
    ElectricityDemand_2023_2030_reference = (SpaceHeat_2023_2030_reference + HotWater_reference) /
      COPHPReal_2023_2030_reference
  )

mh_combined_heat_demand_cold <- mh_combined_heat_demand_cold %>%
  mutate(
    ElectricityDemand_beginn_1918_cold = (SpaceHeat_beginn_1918_cold + HotWater_cold) /
      COPHPReal_beginn_1918_cold,
    ElectricityDemand_1919_1948_cold = (SpaceHeat_1919_1948_cold + HotWater_cold) /
      COPHPReal_1919_1948_cold,
    ElectricityDemand_1949_1978_cold = (SpaceHeat_1949_1978_cold + HotWater_cold) /
      COPHPReal_1949_1978_cold,
    ElectricityDemand_1979_1986_cold = (SpaceHeat_1979_1986_cold + HotWater_cold) /
      COPHPReal_1979_1986_cold,
    ElectricityDemand_1987_1990_cold = (SpaceHeat_1987_1990_cold + HotWater_cold) /
      COPHPReal_1987_1990_cold,
    ElectricityDemand_1991_1995_cold = (SpaceHeat_1991_1995_cold + HotWater_cold) /
      COPHPReal_1991_1995_cold,
    ElectricityDemand_1996_2000_cold = (SpaceHeat_1996_2000_cold + HotWater_cold) /
      COPHPReal_1996_2000_cold,
    ElectricityDemand_2001_2011_cold = (SpaceHeat_2001_2011_cold + HotWater_cold) /
      COPHPReal_2001_2011_cold,
    ElectricityDemand_2012_2022_cold = (SpaceHeat_2012_2022_cold + HotWater_cold) /
      COPHPReal_2012_2022_cold,
    ElectricityDemand_2023_2030_cold = (SpaceHeat_2023_2030_cold + HotWater_cold) /
      COPHPReal_2023_2030_cold
  )

mh_combined_heat_demand_hot <- mh_combined_heat_demand_hot %>%
  mutate(
    ElectricityDemand_beginn_1918_hot = (SpaceHeat_beginn_1918_hot + HotWater_hot) /
      COPHPReal_beginn_1918_hot,
    ElectricityDemand_1919_1948_hot = (SpaceHeat_1919_1948_hot + HotWater_hot) /
      COPHPReal_1919_1948_hot,
    ElectricityDemand_1949_1978_hot = (SpaceHeat_1949_1978_hot + HotWater_hot) /
      COPHPReal_1949_1978_hot,
    ElectricityDemand_1979_1986_hot = (SpaceHeat_1979_1986_hot + HotWater_hot) /
      COPHPReal_1979_1986_hot,
    ElectricityDemand_1987_1990_hot = (SpaceHeat_1987_1990_hot + HotWater_hot) /
      COPHPReal_1987_1990_hot,
    ElectricityDemand_1991_1995_hot = (SpaceHeat_1991_1995_hot + HotWater_hot) /
      COPHPReal_1991_1995_hot,
    ElectricityDemand_1996_2000_hot = (SpaceHeat_1996_2000_hot + HotWater_hot) /
      COPHPReal_1996_2000_hot,
    ElectricityDemand_2001_2011_hot = (SpaceHeat_2001_2011_hot + HotWater_hot) /
      COPHPReal_2001_2011_hot,
    ElectricityDemand_2012_2022_hot = (SpaceHeat_2012_2022_hot + HotWater_hot) /
      COPHPReal_2012_2022_hot,
    ElectricityDemand_2023_2030_hot = (SpaceHeat_2023_2030_hot + HotWater_hot) /
      COPHPReal_2023_2030_hot
  )


# Reference year eh
(sum(eh_combined_heat_demand_reference$SpaceHeat_beginn_1918_reference) + sum(eh_combined_heat_demand_reference$HotWater_reference)) / sum(eh_combined_heat_demand_reference$ElectricityDemand_beginn_1918_reference)
(sum(eh_combined_heat_demand_reference$SpaceHeat_1996_2000_reference) + sum(eh_combined_heat_demand_reference$HotWater_reference)) / sum(eh_combined_heat_demand_reference$ElectricityDemand_1996_2000_reference)
(sum(eh_combined_heat_demand_reference$SpaceHeat_2023_2030_reference) + sum(eh_combined_heat_demand_reference$HotWater_reference)) / sum(eh_combined_heat_demand_reference$ElectricityDemand_2023_2030_reference)
sum(eh_combined_heat_demand_reference$SpaceHeat_beginn_1918_reference) + sum(eh_combined_heat_demand_reference$HotWater_reference)
sum(eh_combined_heat_demand_reference$SpaceHeat_1996_2000_reference) + sum(eh_combined_heat_demand_reference$HotWater_reference)
sum(eh_combined_heat_demand_reference$SpaceHeat_2023_2030_reference) + sum(eh_combined_heat_demand_reference$HotWater_reference)

# Cold year eh
(sum(eh_combined_heat_demand_cold$SpaceHeat_beginn_1918_cold) + sum(eh_combined_heat_demand_cold$HotWater_cold)) / sum(eh_combined_heat_demand_cold$ElectricityDemand_beginn_1918_cold)
(sum(eh_combined_heat_demand_cold$SpaceHeat_1996_2000_cold) + sum(eh_combined_heat_demand_cold$HotWater_cold)) / sum(eh_combined_heat_demand_cold$ElectricityDemand_1996_2000_cold)
(sum(eh_combined_heat_demand_cold$SpaceHeat_2023_2030_cold) + sum(eh_combined_heat_demand_cold$HotWater_cold)) / sum(eh_combined_heat_demand_cold$ElectricityDemand_2023_2030_cold)
sum(eh_combined_heat_demand_cold$SpaceHeat_beginn_1918_cold) + sum(eh_combined_heat_demand_cold$HotWater_cold)
sum(eh_combined_heat_demand_cold$SpaceHeat_1996_2000_cold) + sum(eh_combined_heat_demand_cold$HotWater_cold)
sum(eh_combined_heat_demand_cold$SpaceHeat_2023_2030_cold) + sum(eh_combined_heat_demand_cold$HotWater_cold)

# Hot year eh
(sum(eh_combined_heat_demand_hot$SpaceHeat_beginn_1918_hot) + sum(eh_combined_heat_demand_hot$HotWater_hot)) / sum(eh_combined_heat_demand_hot$ElectricityDemand_beginn_1918_hot)
(sum(eh_combined_heat_demand_hot$SpaceHeat_1996_2000_hot) + sum(eh_combined_heat_demand_hot$HotWater_hot)) / sum(eh_combined_heat_demand_hot$ElectricityDemand_1996_2000_hot)
(sum(eh_combined_heat_demand_hot$SpaceHeat_2023_2030_hot) + sum(eh_combined_heat_demand_hot$HotWater_hot)) / sum(eh_combined_heat_demand_hot$ElectricityDemand_2023_2030_hot)
sum(eh_combined_heat_demand_hot$SpaceHeat_beginn_1918_hot) + sum(eh_combined_heat_demand_hot$HotWater_hot)
sum(eh_combined_heat_demand_hot$SpaceHeat_1996_2000_hot) + sum(eh_combined_heat_demand_hot$HotWater_hot)
sum(eh_combined_heat_demand_hot$SpaceHeat_2023_2030_hot) + sum(eh_combined_heat_demand_hot$HotWater_hot)


# Reference year mh
(sum(mh_combined_heat_demand_reference$SpaceHeat_beginn_1918_reference) + sum(mh_combined_heat_demand_reference$HotWater_reference)) / sum(mh_combined_heat_demand_reference$ElectricityDemand_beginn_1918_reference)
(sum(mh_combined_heat_demand_reference$SpaceHeat_1996_2000_reference) + sum(mh_combined_heat_demand_reference$HotWater_reference)) / sum(mh_combined_heat_demand_reference$ElectricityDemand_1996_2000_reference)
(sum(mh_combined_heat_demand_reference$SpaceHeat_2023_2030_reference) + sum(mh_combined_heat_demand_reference$HotWater_reference)) / sum(mh_combined_heat_demand_reference$ElectricityDemand_2023_2030_reference)
sum(mh_combined_heat_demand_reference$SpaceHeat_beginn_1918_reference) + sum(mh_combined_heat_demand_reference$HotWater_reference)
sum(mh_combined_heat_demand_reference$SpaceHeat_1996_2000_reference) + sum(mh_combined_heat_demand_reference$HotWater_reference)
sum(mh_combined_heat_demand_reference$SpaceHeat_2023_2030_reference) + sum(mh_combined_heat_demand_reference$HotWater_reference)

# Cold year mh
(sum(mh_combined_heat_demand_cold$SpaceHeat_beginn_1918_cold) + sum(mh_combined_heat_demand_cold$HotWater_cold)) / sum(mh_combined_heat_demand_cold$ElectricityDemand_beginn_1918_cold)
(sum(mh_combined_heat_demand_cold$SpaceHeat_1996_2000_cold) + sum(mh_combined_heat_demand_cold$HotWater_cold)) / sum(mh_combined_heat_demand_cold$ElectricityDemand_1996_2000_cold)
(sum(mh_combined_heat_demand_cold$SpaceHeat_2023_2030_cold) + sum(mh_combined_heat_demand_cold$HotWater_cold)) / sum(mh_combined_heat_demand_cold$ElectricityDemand_2023_2030_cold)
sum(mh_combined_heat_demand_cold$SpaceHeat_beginn_1918_cold) + sum(mh_combined_heat_demand_cold$HotWater_cold)
sum(mh_combined_heat_demand_cold$SpaceHeat_1996_2000_cold) + sum(mh_combined_heat_demand_cold$HotWater_cold)
sum(mh_combined_heat_demand_cold$SpaceHeat_2023_2030_cold) + sum(mh_combined_heat_demand_cold$HotWater_cold)

# Hot year mh
(sum(mh_combined_heat_demand_hot$SpaceHeat_beginn_1918_hot) + sum(mh_combined_heat_demand_hot$HotWater_hot)) / sum(mh_combined_heat_demand_hot$ElectricityDemand_beginn_1918_hot)
(sum(mh_combined_heat_demand_hot$SpaceHeat_1996_2000_hot) + sum(mh_combined_heat_demand_hot$HotWater_hot)) / sum(mh_combined_heat_demand_hot$ElectricityDemand_1996_2000_hot)
(sum(mh_combined_heat_demand_hot$SpaceHeat_2023_2030_hot) + sum(mh_combined_heat_demand_hot$HotWater_hot)) / sum(mh_combined_heat_demand_hot$ElectricityDemand_2023_2030_hot)
sum(mh_combined_heat_demand_hot$SpaceHeat_beginn_1918_hot) + sum(mh_combined_heat_demand_hot$HotWater_hot)
sum(mh_combined_heat_demand_hot$SpaceHeat_1996_2000_hot) + sum(mh_combined_heat_demand_hot$HotWater_hot)
sum(mh_combined_heat_demand_hot$SpaceHeat_2023_2030_hot) + sum(mh_combined_heat_demand_hot$HotWater_hot)


# Write output to csv
write_csv2(eh_combined_heat_demand_reference, "data/output/loadprofile/eh_loadprofile_gshp_probe_reference.csv")
write_csv2(eh_combined_heat_demand_cold, "data/output/loadprofile/eh_loadprofile_gshp_probe_cold.csv")
write_csv2(eh_combined_heat_demand_hot, "data/output/loadprofile/eh_loadprofile_gshp_probe_hot.csv")


write_csv2(mh_combined_heat_demand_reference, "data/output/loadprofile/mh_loadprofile_gshp_probe_reference.csv")
write_csv2(mh_combined_heat_demand_cold, "data/output/loadprofile/mh_loadprofile_gshp_probe_cold.csv")
write_csv2(mh_combined_heat_demand_hot, "data/output/loadprofile/mh_loadprofile_gshp_probe_hot.csv")
