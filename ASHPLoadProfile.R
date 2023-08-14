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

eh_min_space_heat_per_m2 <-
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

eh_max_space_heat_per_m2 <-
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

# Separate calculation for multi-family houses, since larger losses
mh_combined_heat_demand_avg <- mh_combined_heat_demand_avg %>%
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

mh_min_space_heat_per_m2 <-
  min(
    mh_combined_heat_demand_avg$Max_SpaceHeat_beginn_1918_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1919_1948_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1949_1978_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1979_1986_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1987_1990_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1991_1995_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1996_2000_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_2001_2011_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_2012_2022_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_2023_2030_avg
  )

mh_max_space_heat_per_m2 <-
  max(
    mh_combined_heat_demand_avg$Max_SpaceHeat_beginn_1918_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1919_1948_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1949_1978_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1979_1986_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1987_1990_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1991_1995_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_1996_2000_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_2001_2011_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_2012_2022_avg,
    mh_combined_heat_demand_avg$Max_SpaceHeat_2023_2030_avg
  )

min_slope <- 0.3

max_slope <- 1.6

slope_function <-
  function(x,
           min_space_heat_per_m2,
           max_space_heat_per_m2) {
    return (0.3 + (x - min_space_heat_per_m2) * ((max_slope - min_slope) / (max_space_heat_per_m2 - min_space_heat_per_m2)
    ))
  }

eh_combined_slopes <- eh_combined_heat_demand_avg %>%
  mutate(
    Slope_SpaceHeat_beginn_1918 = slope_function(
      Max_SpaceHeat_beginn_1918_avg,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1919_1948 = slope_function(
      Max_SpaceHeat_1919_1948_avg,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1949_1978 = slope_function(
      Max_SpaceHeat_1949_1978_avg,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1979_1986 = slope_function(
      Max_SpaceHeat_1979_1986_avg,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1987_1990 = slope_function(
      Max_SpaceHeat_1987_1990_avg,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1991_1995 = slope_function(
      Max_SpaceHeat_1991_1995_avg,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1996_2000 = slope_function(
      Max_SpaceHeat_1996_2000_avg,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2001_2011 = slope_function(
      Max_SpaceHeat_2001_2011_avg,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2012_2022 = slope_function(
      Max_SpaceHeat_2012_2022_avg,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2023_2030 = slope_function(
      Max_SpaceHeat_2023_2030_avg,
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

eh_combined_heat_demand_avg <- eh_combined_heat_demand_avg %>%
  select(
    -c(
      Max_SpaceHeat_beginn_1918_avg,
      Max_SpaceHeat_1919_1948_avg,
      Max_SpaceHeat_1949_1978_avg,
      Max_SpaceHeat_1979_1986_avg,
      Max_SpaceHeat_1987_1990_avg,
      Max_SpaceHeat_1991_1995_avg,
      Max_SpaceHeat_1996_2000_avg,
      Max_SpaceHeat_2001_2011_avg,
      Max_SpaceHeat_2012_2022_avg,
      Max_SpaceHeat_2023_2030_avg
    )
  )

mh_combined_slopes <- mh_combined_heat_demand_avg %>%
  mutate(
    Slope_SpaceHeat_beginn_1918 = slope_function(
      Max_SpaceHeat_beginn_1918_avg,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1919_1948 = slope_function(
      Max_SpaceHeat_1919_1948_avg,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1949_1978 = slope_function(
      Max_SpaceHeat_1949_1978_avg,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1979_1986 = slope_function(
      Max_SpaceHeat_1979_1986_avg,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1987_1990 = slope_function(
      Max_SpaceHeat_1987_1990_avg,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1991_1995 = slope_function(
      Max_SpaceHeat_1991_1995_avg,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1996_2000 = slope_function(
      Max_SpaceHeat_1996_2000_avg,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2001_2011 = slope_function(
      Max_SpaceHeat_2001_2011_avg,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2012_2022 = slope_function(
      Max_SpaceHeat_2012_2022_avg,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2023_2030 = slope_function(
      Max_SpaceHeat_2023_2030_avg,
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

mh_combined_heat_demand_avg <- mh_combined_heat_demand_avg %>%
  select(
    -c(
      Max_SpaceHeat_beginn_1918_avg,
      Max_SpaceHeat_1919_1948_avg,
      Max_SpaceHeat_1949_1978_avg,
      Max_SpaceHeat_1979_1986_avg,
      Max_SpaceHeat_1987_1990_avg,
      Max_SpaceHeat_1991_1995_avg,
      Max_SpaceHeat_1996_2000_avg,
      Max_SpaceHeat_2001_2011_avg,
      Max_SpaceHeat_2012_2022_avg,
      Max_SpaceHeat_2023_2030_avg
    )
  )

romm_target_temperature <- 293.15
level <- 0

# dar = Tmix - romm_target_temperature
flow_temperature_function <- function(slope, dar) {
  return (romm_target_temperature + level - slope * dar * (1.4347 + 0.021 * dar + 247.9 * 10 ^
                                                             (-6) * dar ^ 2))
}

# Remove space heat demand when temperature is higher 293.15K
eh_combined_heat_demand_avg <- eh_combined_heat_demand_avg %>%
  left_join(weather_data_avg, by = "Time")

eh_combined_heat_demand_cold <- eh_combined_heat_demand_cold %>%
  left_join(weather_data_cold, by = "Time")

eh_combined_heat_demand_hot <- eh_combined_heat_demand_hot %>%
  left_join(weather_data_hot, by = "Time")

mh_combined_heat_demand_avg <- mh_combined_heat_demand_avg %>%
  left_join(weather_data_avg, by = "Time")

mh_combined_heat_demand_cold <- mh_combined_heat_demand_cold %>%
  left_join(weather_data_cold, by = "Time")

mh_combined_heat_demand_hot <- mh_combined_heat_demand_hot %>%
  left_join(weather_data_hot, by = "Time")

eh_combined_heat_demand_avg <- eh_combined_heat_demand_avg %>%
  mutate(
    SpaceHeat_beginn_1918_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_beginn_1918_avg),
    SpaceHeat_1919_1948_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1919_1948_avg),
    SpaceHeat_1949_1978_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1949_1978_avg),
    SpaceHeat_1979_1986_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1979_1986_avg),
    SpaceHeat_1987_1990_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1987_1990_avg),
    SpaceHeat_1991_1995_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1991_1995_avg),
    SpaceHeat_1996_2000_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1996_2000_avg),
    SpaceHeat_2001_2011_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_2001_2011_avg),
    SpaceHeat_2012_2022_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_2012_2022_avg),
    SpaceHeat_2023_2030_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_2023_2030_avg)
  )


eh_combined_heat_demand_cold <- eh_combined_heat_demand_cold %>%
  mutate(
    SpaceHeat_beginn_1918_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_beginn_1918_cold),
    SpaceHeat_1919_1948_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1919_1948_cold),
    SpaceHeat_1949_1978_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1949_1978_cold),
    SpaceHeat_1979_1986_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1979_1986_cold),
    SpaceHeat_1987_1990_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1987_1990_cold),
    SpaceHeat_1991_1995_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1991_1995_cold),
    SpaceHeat_1996_2000_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1996_2000_cold),
    SpaceHeat_2001_2011_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_2001_2011_cold),
    SpaceHeat_2012_2022_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_2012_2022_cold),
    SpaceHeat_2023_2030_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_2023_2030_cold)
  )

eh_combined_heat_demand_hot <- eh_combined_heat_demand_hot %>%
  mutate(
    SpaceHeat_beginn_1918_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_beginn_1918_hot),
    SpaceHeat_1919_1948_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1919_1948_hot),
    SpaceHeat_1949_1978_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1949_1978_hot),
    SpaceHeat_1979_1986_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1979_1986_hot),
    SpaceHeat_1987_1990_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1987_1990_hot),
    SpaceHeat_1991_1995_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1991_1995_hot),
    SpaceHeat_1996_2000_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1996_2000_hot),
    SpaceHeat_2001_2011_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_2001_2011_hot),
    SpaceHeat_2012_2022_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_2012_2022_hot),
    SpaceHeat_2023_2030_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_2023_2030_hot)
  )

mh_combined_heat_demand_avg <- mh_combined_heat_demand_avg %>%
  mutate(
    SpaceHeat_beginn_1918_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_beginn_1918_avg),
    SpaceHeat_1919_1948_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1919_1948_avg),
    SpaceHeat_1949_1978_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1949_1978_avg),
    SpaceHeat_1979_1986_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1979_1986_avg),
    SpaceHeat_1987_1990_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1987_1990_avg),
    SpaceHeat_1991_1995_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1991_1995_avg),
    SpaceHeat_1996_2000_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_1996_2000_avg),
    SpaceHeat_2001_2011_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_2001_2011_avg),
    SpaceHeat_2012_2022_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_2012_2022_avg),
    SpaceHeat_2023_2030_avg = ifelse(TMix >= 293.15, 0,  SpaceHeat_2023_2030_avg)
  )


mh_combined_heat_demand_cold <- mh_combined_heat_demand_cold %>%
  mutate(
    SpaceHeat_beginn_1918_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_beginn_1918_cold),
    SpaceHeat_1919_1948_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1919_1948_cold),
    SpaceHeat_1949_1978_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1949_1978_cold),
    SpaceHeat_1979_1986_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1979_1986_cold),
    SpaceHeat_1987_1990_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1987_1990_cold),
    SpaceHeat_1991_1995_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1991_1995_cold),
    SpaceHeat_1996_2000_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_1996_2000_cold),
    SpaceHeat_2001_2011_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_2001_2011_cold),
    SpaceHeat_2012_2022_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_2012_2022_cold),
    SpaceHeat_2023_2030_cold = ifelse(TMix >= 293.15, 0,  SpaceHeat_2023_2030_cold)
  )

mh_combined_heat_demand_hot <- mh_combined_heat_demand_hot %>%
  mutate(
    SpaceHeat_beginn_1918_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_beginn_1918_hot),
    SpaceHeat_1919_1948_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1919_1948_hot),
    SpaceHeat_1949_1978_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1949_1978_hot),
    SpaceHeat_1979_1986_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1979_1986_hot),
    SpaceHeat_1987_1990_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1987_1990_hot),
    SpaceHeat_1991_1995_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1991_1995_hot),
    SpaceHeat_1996_2000_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_1996_2000_hot),
    SpaceHeat_2001_2011_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_2001_2011_hot),
    SpaceHeat_2012_2022_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_2012_2022_hot),
    SpaceHeat_2023_2030_hot = ifelse(TMix >= 293.15, 0,  SpaceHeat_2023_2030_hot)
  )


# Calculate the flow temperature
eh_combined_heat_demand_avg <- eh_combined_heat_demand_avg %>%
  mutate(Dar = TMix - 293.15) %>%
  left_join(eh_combined_slopes, by = "Time") %>%
  mutate(
    FlowTemp_SpaceHeat_beginn_1918_avg = flow_temperature_function(Slope_SpaceHeat_beginn_1918, Dar),
    FlowTemp_SpaceHeat_1919_1948_avg = flow_temperature_function(Slope_SpaceHeat_1919_1948, Dar),
    FlowTemp_SpaceHeat_1949_1978_avg = flow_temperature_function(Slope_SpaceHeat_1949_1978, Dar),
    FlowTemp_SpaceHeat_1979_1986_avg = flow_temperature_function(Slope_SpaceHeat_1979_1986, Dar),
    FlowTemp_SpaceHeat_1987_1990_avg = flow_temperature_function(Slope_SpaceHeat_1987_1990, Dar),
    FlowTemp_SpaceHeat_1991_1995_avg = flow_temperature_function(Slope_SpaceHeat_1991_1995, Dar),
    FlowTemp_SpaceHeat_1996_2000_avg = flow_temperature_function(Slope_SpaceHeat_1996_2000, Dar),
    FlowTemp_SpaceHeat_2001_2011_avg = flow_temperature_function(Slope_SpaceHeat_2001_2011, Dar),
    FlowTemp_SpaceHeat_2012_2022_avg = flow_temperature_function(Slope_SpaceHeat_2012_2022, Dar),
    FlowTemp_SpaceHeat_2023_2030_avg = flow_temperature_function(Slope_SpaceHeat_2023_2030, Dar),
    Share_SpaceHeat_beginn_1918_avg = SpaceHeat_beginn_1918_avg / (SpaceHeat_beginn_1918_avg + HotWater_avg),
    Share_SpaceHeat_1919_1948_avg = SpaceHeat_1919_1948_avg / (SpaceHeat_1919_1948_avg + HotWater_avg),
    Share_SpaceHeat_1949_1978_avg = SpaceHeat_1949_1978_avg / (SpaceHeat_1949_1978_avg + HotWater_avg),
    Share_SpaceHeat_1979_1986_avg = SpaceHeat_1979_1986_avg / (SpaceHeat_1979_1986_avg + HotWater_avg),
    Share_SpaceHeat_1987_1990_avg = SpaceHeat_1987_1990_avg / (SpaceHeat_1987_1990_avg + HotWater_avg),
    Share_SpaceHeat_1991_1995_avg = SpaceHeat_1991_1995_avg / (SpaceHeat_1991_1995_avg + HotWater_avg),
    Share_SpaceHeat_1996_2000_avg = SpaceHeat_1996_2000_avg / (SpaceHeat_1996_2000_avg + HotWater_avg),
    Share_SpaceHeat_2001_2011_avg = SpaceHeat_2001_2011_avg / (SpaceHeat_2001_2011_avg + HotWater_avg),
    Share_SpaceHeat_2012_2022_avg = SpaceHeat_2012_2022_avg / (SpaceHeat_2012_2022_avg + HotWater_avg),
    Share_SpaceHeat_2023_2030_avg = SpaceHeat_2023_2030_avg / (SpaceHeat_2023_2030_avg + HotWater_avg),
    FlowTemp_beginn_1918_avg = Share_SpaceHeat_beginn_1918_avg * FlowTemp_SpaceHeat_beginn_1918_avg + (1 - Share_SpaceHeat_beginn_1918_avg) * 323,
    FlowTemp_1919_1948_avg = Share_SpaceHeat_1919_1948_avg * FlowTemp_SpaceHeat_1919_1948_avg + (1 - Share_SpaceHeat_1919_1948_avg) * 323,
    FlowTemp_1949_1978_avg = Share_SpaceHeat_1949_1978_avg * FlowTemp_SpaceHeat_1949_1978_avg + (1 - Share_SpaceHeat_1949_1978_avg) * 323,
    FlowTemp_1979_1986_avg = Share_SpaceHeat_1979_1986_avg * FlowTemp_SpaceHeat_1979_1986_avg + (1 - Share_SpaceHeat_1979_1986_avg) * 323,
    FlowTemp_1987_1990_avg = Share_SpaceHeat_1987_1990_avg * FlowTemp_SpaceHeat_1987_1990_avg + (1 - Share_SpaceHeat_1987_1990_avg) * 323,
    FlowTemp_1991_1995_avg = Share_SpaceHeat_1991_1995_avg * FlowTemp_SpaceHeat_1991_1995_avg + (1 - Share_SpaceHeat_1991_1995_avg) * 323,
    FlowTemp_1996_2000_avg = Share_SpaceHeat_1996_2000_avg * FlowTemp_SpaceHeat_1996_2000_avg + (1 - Share_SpaceHeat_1996_2000_avg) * 323,
    FlowTemp_2001_2011_avg = Share_SpaceHeat_2001_2011_avg * FlowTemp_SpaceHeat_2001_2011_avg + (1 - Share_SpaceHeat_2001_2011_avg) * 323,
    FlowTemp_2012_2022_avg = Share_SpaceHeat_2012_2022_avg * FlowTemp_SpaceHeat_2012_2022_avg + (1 - Share_SpaceHeat_2012_2022_avg) * 323,
    FlowTemp_2023_2030_avg = Share_SpaceHeat_2023_2030_avg * FlowTemp_SpaceHeat_2023_2030_avg + (1 - Share_SpaceHeat_2023_2030_avg) * 323
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
      FlowTemp_SpaceHeat_beginn_1918_avg,
      FlowTemp_SpaceHeat_1919_1948_avg,
      FlowTemp_SpaceHeat_1949_1978_avg,
      FlowTemp_SpaceHeat_1979_1986_avg,
      FlowTemp_SpaceHeat_1987_1990_avg,
      FlowTemp_SpaceHeat_1991_1995_avg,
      FlowTemp_SpaceHeat_1996_2000_avg,
      FlowTemp_SpaceHeat_2001_2011_avg,
      FlowTemp_SpaceHeat_2012_2022_avg,
      FlowTemp_SpaceHeat_2023_2030_avg,
      Share_SpaceHeat_beginn_1918_avg,
      Share_SpaceHeat_1919_1948_avg,
      Share_SpaceHeat_1949_1978_avg,
      Share_SpaceHeat_1979_1986_avg,
      Share_SpaceHeat_1987_1990_avg,
      Share_SpaceHeat_1991_1995_avg,
      Share_SpaceHeat_1996_2000_avg,
      Share_SpaceHeat_2001_2011_avg,
      Share_SpaceHeat_2012_2022_avg,
      Share_SpaceHeat_2023_2030_avg,
      Dar
    )
  )

eh_combined_heat_demand_cold <- eh_combined_heat_demand_cold %>%
  mutate(Dar = TMix - 293.15) %>%
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
    FlowTemp_beginn_1918_cold = Share_SpaceHeat_beginn_1918_cold * FlowTemp_SpaceHeat_beginn_1918_cold + (1 - Share_SpaceHeat_beginn_1918_cold) * 323,
    FlowTemp_1919_1948_cold = Share_SpaceHeat_1919_1948_cold * FlowTemp_SpaceHeat_1919_1948_cold + (1 - Share_SpaceHeat_1919_1948_cold) * 323,
    FlowTemp_1949_1978_cold = Share_SpaceHeat_1949_1978_cold * FlowTemp_SpaceHeat_1949_1978_cold + (1 - Share_SpaceHeat_1949_1978_cold) * 323,
    FlowTemp_1979_1986_cold = Share_SpaceHeat_1979_1986_cold * FlowTemp_SpaceHeat_1979_1986_cold + (1 - Share_SpaceHeat_1979_1986_cold) * 323,
    FlowTemp_1987_1990_cold = Share_SpaceHeat_1987_1990_cold * FlowTemp_SpaceHeat_1987_1990_cold + (1 - Share_SpaceHeat_1987_1990_cold) * 323,
    FlowTemp_1991_1995_cold = Share_SpaceHeat_1991_1995_cold * FlowTemp_SpaceHeat_1991_1995_cold + (1 - Share_SpaceHeat_1991_1995_cold) * 323,
    FlowTemp_1996_2000_cold = Share_SpaceHeat_1996_2000_cold * FlowTemp_SpaceHeat_1996_2000_cold + (1 - Share_SpaceHeat_1996_2000_cold) * 323,
    FlowTemp_2001_2011_cold = Share_SpaceHeat_2001_2011_cold * FlowTemp_SpaceHeat_2001_2011_cold + (1 - Share_SpaceHeat_2001_2011_cold) * 323,
    FlowTemp_2012_2022_cold = Share_SpaceHeat_2012_2022_cold * FlowTemp_SpaceHeat_2012_2022_cold + (1 - Share_SpaceHeat_2012_2022_cold) * 323,
    FlowTemp_2023_2030_cold = Share_SpaceHeat_2023_2030_cold * FlowTemp_SpaceHeat_2023_2030_cold + (1 - Share_SpaceHeat_2023_2030_cold) * 323
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
  mutate(Dar = TMix - 293.15) %>%
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
    FlowTemp_beginn_1918_hot = Share_SpaceHeat_beginn_1918_hot * FlowTemp_SpaceHeat_beginn_1918_hot + (1 - Share_SpaceHeat_beginn_1918_hot) * 323,
    FlowTemp_1919_1948_hot = Share_SpaceHeat_1919_1948_hot * FlowTemp_SpaceHeat_1919_1948_hot + (1 - Share_SpaceHeat_1919_1948_hot) * 323,
    FlowTemp_1949_1978_hot = Share_SpaceHeat_1949_1978_hot * FlowTemp_SpaceHeat_1949_1978_hot + (1 - Share_SpaceHeat_1949_1978_hot) * 323,
    FlowTemp_1979_1986_hot = Share_SpaceHeat_1979_1986_hot * FlowTemp_SpaceHeat_1979_1986_hot + (1 - Share_SpaceHeat_1979_1986_hot) * 323,
    FlowTemp_1987_1990_hot = Share_SpaceHeat_1987_1990_hot * FlowTemp_SpaceHeat_1987_1990_hot + (1 - Share_SpaceHeat_1987_1990_hot) * 323,
    FlowTemp_1991_1995_hot = Share_SpaceHeat_1991_1995_hot * FlowTemp_SpaceHeat_1991_1995_hot + (1 - Share_SpaceHeat_1991_1995_hot) * 323,
    FlowTemp_1996_2000_hot = Share_SpaceHeat_1996_2000_hot * FlowTemp_SpaceHeat_1996_2000_hot + (1 - Share_SpaceHeat_1996_2000_hot) * 323,
    FlowTemp_2001_2011_hot = Share_SpaceHeat_2001_2011_hot * FlowTemp_SpaceHeat_2001_2011_hot + (1 - Share_SpaceHeat_2001_2011_hot) * 323,
    FlowTemp_2012_2022_hot = Share_SpaceHeat_2012_2022_hot * FlowTemp_SpaceHeat_2012_2022_hot + (1 - Share_SpaceHeat_2012_2022_hot) * 323,
    FlowTemp_2023_2030_hot = Share_SpaceHeat_2023_2030_hot * FlowTemp_SpaceHeat_2023_2030_hot + (1 - Share_SpaceHeat_2023_2030_hot) * 323
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

mh_combined_heat_demand_avg <- mh_combined_heat_demand_avg %>%
  mutate(Dar = TMix - 293.15) %>%
  left_join(mh_combined_slopes, by = "Time") %>%
  mutate(
    FlowTemp_SpaceHeat_beginn_1918_avg = flow_temperature_function(Slope_SpaceHeat_beginn_1918, Dar),
    FlowTemp_SpaceHeat_1919_1948_avg = flow_temperature_function(Slope_SpaceHeat_1919_1948, Dar),
    FlowTemp_SpaceHeat_1949_1978_avg = flow_temperature_function(Slope_SpaceHeat_1949_1978, Dar),
    FlowTemp_SpaceHeat_1979_1986_avg = flow_temperature_function(Slope_SpaceHeat_1979_1986, Dar),
    FlowTemp_SpaceHeat_1987_1990_avg = flow_temperature_function(Slope_SpaceHeat_1987_1990, Dar),
    FlowTemp_SpaceHeat_1991_1995_avg = flow_temperature_function(Slope_SpaceHeat_1991_1995, Dar),
    FlowTemp_SpaceHeat_1996_2000_avg = flow_temperature_function(Slope_SpaceHeat_1996_2000, Dar),
    FlowTemp_SpaceHeat_2001_2011_avg = flow_temperature_function(Slope_SpaceHeat_2001_2011, Dar),
    FlowTemp_SpaceHeat_2012_2022_avg = flow_temperature_function(Slope_SpaceHeat_2012_2022, Dar),
    FlowTemp_SpaceHeat_2023_2030_avg = flow_temperature_function(Slope_SpaceHeat_2023_2030, Dar),
    Share_SpaceHeat_beginn_1918_avg = SpaceHeat_beginn_1918_avg / (SpaceHeat_beginn_1918_avg + HotWater_avg),
    Share_SpaceHeat_1919_1948_avg = SpaceHeat_1919_1948_avg / (SpaceHeat_1919_1948_avg + HotWater_avg),
    Share_SpaceHeat_1949_1978_avg = SpaceHeat_1949_1978_avg / (SpaceHeat_1949_1978_avg + HotWater_avg),
    Share_SpaceHeat_1979_1986_avg = SpaceHeat_1979_1986_avg / (SpaceHeat_1979_1986_avg + HotWater_avg),
    Share_SpaceHeat_1987_1990_avg = SpaceHeat_1987_1990_avg / (SpaceHeat_1987_1990_avg + HotWater_avg),
    Share_SpaceHeat_1991_1995_avg = SpaceHeat_1991_1995_avg / (SpaceHeat_1991_1995_avg + HotWater_avg),
    Share_SpaceHeat_1996_2000_avg = SpaceHeat_1996_2000_avg / (SpaceHeat_1996_2000_avg + HotWater_avg),
    Share_SpaceHeat_2001_2011_avg = SpaceHeat_2001_2011_avg / (SpaceHeat_2001_2011_avg + HotWater_avg),
    Share_SpaceHeat_2012_2022_avg = SpaceHeat_2012_2022_avg / (SpaceHeat_2012_2022_avg + HotWater_avg),
    Share_SpaceHeat_2023_2030_avg = SpaceHeat_2023_2030_avg / (SpaceHeat_2023_2030_avg + HotWater_avg),
    FlowTemp_beginn_1918_avg = Share_SpaceHeat_beginn_1918_avg * FlowTemp_SpaceHeat_beginn_1918_avg + (1 - Share_SpaceHeat_beginn_1918_avg) * 373,
    FlowTemp_1919_1948_avg = Share_SpaceHeat_1919_1948_avg * FlowTemp_SpaceHeat_1919_1948_avg + (1 - Share_SpaceHeat_1919_1948_avg) * 373,
    FlowTemp_1949_1978_avg = Share_SpaceHeat_1949_1978_avg * FlowTemp_SpaceHeat_1949_1978_avg + (1 - Share_SpaceHeat_1949_1978_avg) * 373,
    FlowTemp_1979_1986_avg = Share_SpaceHeat_1979_1986_avg * FlowTemp_SpaceHeat_1979_1986_avg + (1 - Share_SpaceHeat_1979_1986_avg) * 373,
    FlowTemp_1987_1990_avg = Share_SpaceHeat_1987_1990_avg * FlowTemp_SpaceHeat_1987_1990_avg + (1 - Share_SpaceHeat_1987_1990_avg) * 373,
    FlowTemp_1991_1995_avg = Share_SpaceHeat_1991_1995_avg * FlowTemp_SpaceHeat_1991_1995_avg + (1 - Share_SpaceHeat_1991_1995_avg) * 373,
    FlowTemp_1996_2000_avg = Share_SpaceHeat_1996_2000_avg * FlowTemp_SpaceHeat_1996_2000_avg + (1 - Share_SpaceHeat_1996_2000_avg) * 373,
    FlowTemp_2001_2011_avg = Share_SpaceHeat_2001_2011_avg * FlowTemp_SpaceHeat_2001_2011_avg + (1 - Share_SpaceHeat_2001_2011_avg) * 373,
    FlowTemp_2012_2022_avg = Share_SpaceHeat_2012_2022_avg * FlowTemp_SpaceHeat_2012_2022_avg + (1 - Share_SpaceHeat_2012_2022_avg) * 373,
    FlowTemp_2023_2030_avg = Share_SpaceHeat_2023_2030_avg * FlowTemp_SpaceHeat_2023_2030_avg + (1 - Share_SpaceHeat_2023_2030_avg) * 373
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
      FlowTemp_SpaceHeat_beginn_1918_avg,
      FlowTemp_SpaceHeat_1919_1948_avg,
      FlowTemp_SpaceHeat_1949_1978_avg,
      FlowTemp_SpaceHeat_1979_1986_avg,
      FlowTemp_SpaceHeat_1987_1990_avg,
      FlowTemp_SpaceHeat_1991_1995_avg,
      FlowTemp_SpaceHeat_1996_2000_avg,
      FlowTemp_SpaceHeat_2001_2011_avg,
      FlowTemp_SpaceHeat_2012_2022_avg,
      FlowTemp_SpaceHeat_2023_2030_avg,
      Share_SpaceHeat_beginn_1918_avg,
      Share_SpaceHeat_1919_1948_avg,
      Share_SpaceHeat_1949_1978_avg,
      Share_SpaceHeat_1979_1986_avg,
      Share_SpaceHeat_1987_1990_avg,
      Share_SpaceHeat_1991_1995_avg,
      Share_SpaceHeat_1996_2000_avg,
      Share_SpaceHeat_2001_2011_avg,
      Share_SpaceHeat_2012_2022_avg,
      Share_SpaceHeat_2023_2030_avg,
      Dar
    )
  )

mh_combined_heat_demand_cold <- mh_combined_heat_demand_cold %>%
  mutate(Dar = TMix - 293.15) %>%
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
    FlowTemp_beginn_1918_cold = Share_SpaceHeat_beginn_1918_cold * FlowTemp_SpaceHeat_beginn_1918_cold + (1 - Share_SpaceHeat_beginn_1918_cold) * 373,
    FlowTemp_1919_1948_cold = Share_SpaceHeat_1919_1948_cold * FlowTemp_SpaceHeat_1919_1948_cold + (1 - Share_SpaceHeat_1919_1948_cold) * 373,
    FlowTemp_1949_1978_cold = Share_SpaceHeat_1949_1978_cold * FlowTemp_SpaceHeat_1949_1978_cold + (1 - Share_SpaceHeat_1949_1978_cold) * 373,
    FlowTemp_1979_1986_cold = Share_SpaceHeat_1979_1986_cold * FlowTemp_SpaceHeat_1979_1986_cold + (1 - Share_SpaceHeat_1979_1986_cold) * 373,
    FlowTemp_1987_1990_cold = Share_SpaceHeat_1987_1990_cold * FlowTemp_SpaceHeat_1987_1990_cold + (1 - Share_SpaceHeat_1987_1990_cold) * 373,
    FlowTemp_1991_1995_cold = Share_SpaceHeat_1991_1995_cold * FlowTemp_SpaceHeat_1991_1995_cold + (1 - Share_SpaceHeat_1991_1995_cold) * 373,
    FlowTemp_1996_2000_cold = Share_SpaceHeat_1996_2000_cold * FlowTemp_SpaceHeat_1996_2000_cold + (1 - Share_SpaceHeat_1996_2000_cold) * 373,
    FlowTemp_2001_2011_cold = Share_SpaceHeat_2001_2011_cold * FlowTemp_SpaceHeat_2001_2011_cold + (1 - Share_SpaceHeat_2001_2011_cold) * 373,
    FlowTemp_2012_2022_cold = Share_SpaceHeat_2012_2022_cold * FlowTemp_SpaceHeat_2012_2022_cold + (1 - Share_SpaceHeat_2012_2022_cold) * 373,
    FlowTemp_2023_2030_cold = Share_SpaceHeat_2023_2030_cold * FlowTemp_SpaceHeat_2023_2030_cold + (1 - Share_SpaceHeat_2023_2030_cold) * 373
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
  mutate(Dar = TMix - 293.15) %>%
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
    FlowTemp_beginn_1918_hot = Share_SpaceHeat_beginn_1918_hot * FlowTemp_SpaceHeat_beginn_1918_hot + (1 - Share_SpaceHeat_beginn_1918_hot) * 373,
    FlowTemp_1919_1948_hot = Share_SpaceHeat_1919_1948_hot * FlowTemp_SpaceHeat_1919_1948_hot + (1 - Share_SpaceHeat_1919_1948_hot) * 373,
    FlowTemp_1949_1978_hot = Share_SpaceHeat_1949_1978_hot * FlowTemp_SpaceHeat_1949_1978_hot + (1 - Share_SpaceHeat_1949_1978_hot) * 373,
    FlowTemp_1979_1986_hot = Share_SpaceHeat_1979_1986_hot * FlowTemp_SpaceHeat_1979_1986_hot + (1 - Share_SpaceHeat_1979_1986_hot) * 373,
    FlowTemp_1987_1990_hot = Share_SpaceHeat_1987_1990_hot * FlowTemp_SpaceHeat_1987_1990_hot + (1 - Share_SpaceHeat_1987_1990_hot) * 373,
    FlowTemp_1991_1995_hot = Share_SpaceHeat_1991_1995_hot * FlowTemp_SpaceHeat_1991_1995_hot + (1 - Share_SpaceHeat_1991_1995_hot) * 373,
    FlowTemp_1996_2000_hot = Share_SpaceHeat_1996_2000_hot * FlowTemp_SpaceHeat_1996_2000_hot + (1 - Share_SpaceHeat_1996_2000_hot) * 373,
    FlowTemp_2001_2011_hot = Share_SpaceHeat_2001_2011_hot * FlowTemp_SpaceHeat_2001_2011_hot + (1 - Share_SpaceHeat_2001_2011_hot) * 373,
    FlowTemp_2012_2022_hot = Share_SpaceHeat_2012_2022_hot * FlowTemp_SpaceHeat_2012_2022_hot + (1 - Share_SpaceHeat_2012_2022_hot) * 373,
    FlowTemp_2023_2030_hot = Share_SpaceHeat_2023_2030_hot * FlowTemp_SpaceHeat_2023_2030_hot + (1 - Share_SpaceHeat_2023_2030_hot) * 373
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


# Determine the share of the heat pump and the share of the immersion heater
# Assumption: For every building the heat pump covers 70% and the immersion heater 30% of the maximum thermal capacity
eh_combined_max_capacity <- eh_combined_heat_demand_avg %>%
  mutate(
    MaxThermalCapacity_beginn_1918 = max(SpaceHeat_beginn_1918_avg + HotWater_avg),
    MaxThermalCapacity_1919_1948 = max(SpaceHeat_1919_1948_avg + HotWater_avg),
    MaxThermalCapacity_1949_1978 = max(SpaceHeat_1949_1978_avg + HotWater_avg),
    MaxThermalCapacity_1979_1986 = max(SpaceHeat_1979_1986_avg + HotWater_avg),
    MaxThermalCapacity_1987_1990 = max(SpaceHeat_1987_1990_avg + HotWater_avg),
    MaxThermalCapacity_1991_1995 = max(SpaceHeat_1991_1995_avg + HotWater_avg),
    MaxThermalCapacity_1996_2000 = max(SpaceHeat_1996_2000_avg + HotWater_avg),
    MaxThermalCapacity_2001_2011 = max(SpaceHeat_2001_2011_avg + HotWater_avg),
    MaxThermalCapacity_2012_2022 = max(SpaceHeat_2012_2022_avg + HotWater_avg),
    MaxThermalCapacity_2023_2030 = max(SpaceHeat_2023_2030_avg + HotWater_avg)
  ) %>%
  select(
    Time,
    MaxThermalCapacity_beginn_1918,
    MaxThermalCapacity_1919_1948,
    MaxThermalCapacity_1949_1978,
    MaxThermalCapacity_1979_1986,
    MaxThermalCapacity_1987_1990,
    MaxThermalCapacity_1991_1995,
    MaxThermalCapacity_1996_2000,
    MaxThermalCapacity_2001_2011,
    MaxThermalCapacity_2012_2022,
    MaxThermalCapacity_2023_2030
  )

mh_combined_max_capacity <- mh_combined_heat_demand_avg %>%
  mutate(
    MaxThermalCapacity_beginn_1918 = max(SpaceHeat_beginn_1918_avg + HotWater_avg),
    MaxThermalCapacity_1919_1948 = max(SpaceHeat_1919_1948_avg + HotWater_avg),
    MaxThermalCapacity_1949_1978 = max(SpaceHeat_1949_1978_avg + HotWater_avg),
    MaxThermalCapacity_1979_1986 = max(SpaceHeat_1979_1986_avg + HotWater_avg),
    MaxThermalCapacity_1987_1990 = max(SpaceHeat_1987_1990_avg + HotWater_avg),
    MaxThermalCapacity_1991_1995 = max(SpaceHeat_1991_1995_avg + HotWater_avg),
    MaxThermalCapacity_1996_2000 = max(SpaceHeat_1996_2000_avg + HotWater_avg),
    MaxThermalCapacity_2001_2011 = max(SpaceHeat_2001_2011_avg + HotWater_avg),
    MaxThermalCapacity_2012_2022 = max(SpaceHeat_2012_2022_avg + HotWater_avg),
    MaxThermalCapacity_2023_2030 = max(SpaceHeat_2023_2030_avg + HotWater_avg)
  ) %>%
  select(
    Time,
    MaxThermalCapacity_beginn_1918,
    MaxThermalCapacity_1919_1948,
    MaxThermalCapacity_1949_1978,
    MaxThermalCapacity_1979_1986,
    MaxThermalCapacity_1987_1990,
    MaxThermalCapacity_1991_1995,
    MaxThermalCapacity_1996_2000,
    MaxThermalCapacity_2001_2011,
    MaxThermalCapacity_2012_2022,
    MaxThermalCapacity_2023_2030
  )

share_heatpump_function <-
  function(space_heat,
           hot_water,
           max_thermal_capacity) {
    ifelse((space_heat + hot_water) <= (0.7 * max_thermal_capacity),
           1,
           (0.7 * max_thermal_capacity) / (space_heat + hot_water)
    )
  }



eh_combined_heat_demand_avg <- eh_combined_heat_demand_avg %>%
  left_join(eh_combined_max_capacity, by = "Time") %>%
  mutate(
    ShareHP_beginn_1918_avg = share_heatpump_function(
      SpaceHeat_beginn_1918_avg,
      HotWater_avg,
      MaxThermalCapacity_beginn_1918
    ),
    ShareHP_1919_1948_avg = share_heatpump_function(
      SpaceHeat_1919_1948_avg,
      HotWater_avg,
      MaxThermalCapacity_1919_1948
    ),
    ShareHP_1949_1978_avg = share_heatpump_function(
      SpaceHeat_1949_1978_avg,
      HotWater_avg,
      MaxThermalCapacity_1949_1978
    ),
    ShareHP_1979_1986_avg = share_heatpump_function(
      SpaceHeat_1979_1986_avg,
      HotWater_avg,
      MaxThermalCapacity_1979_1986
    ),
    ShareHP_1987_1990_avg = share_heatpump_function(
      SpaceHeat_1987_1990_avg,
      HotWater_avg,
      MaxThermalCapacity_1987_1990
    ),
    ShareHP_1991_1995_avg = share_heatpump_function(
      SpaceHeat_1991_1995_avg,
      HotWater_avg,
      MaxThermalCapacity_1991_1995
    ),
    ShareHP_1996_2000_avg = share_heatpump_function(
      SpaceHeat_1996_2000_avg,
      HotWater_avg,
      MaxThermalCapacity_1996_2000
    ),
    ShareHP_2001_2011_avg = share_heatpump_function(
      SpaceHeat_2001_2011_avg,
      HotWater_avg,
      MaxThermalCapacity_2001_2011
    ),
    ShareHP_2012_2022_avg = share_heatpump_function(
      SpaceHeat_2012_2022_avg,
      HotWater_avg,
      MaxThermalCapacity_2012_2022
    ),
    ShareHP_2023_2030_avg = share_heatpump_function(
      SpaceHeat_2023_2030_avg,
      HotWater_avg,
      MaxThermalCapacity_2023_2030
    )
  ) %>%
  select(
    -c(
      MaxThermalCapacity_beginn_1918,
      MaxThermalCapacity_1919_1948,
      MaxThermalCapacity_1949_1978,
      MaxThermalCapacity_1979_1986,
      MaxThermalCapacity_1987_1990,
      MaxThermalCapacity_1991_1995,
      MaxThermalCapacity_1996_2000,
      MaxThermalCapacity_2001_2011,
      MaxThermalCapacity_2012_2022,
      MaxThermalCapacity_2023_2030
    )
  )

eh_combined_heat_demand_cold <- eh_combined_heat_demand_cold %>%
  left_join(eh_combined_max_capacity, by = "Time") %>%
  mutate(
    ShareHP_beginn_1918_cold = share_heatpump_function(
      SpaceHeat_beginn_1918_cold,
      HotWater_cold,
      MaxThermalCapacity_beginn_1918
    ),
    ShareHP_1919_1948_cold = share_heatpump_function(
      SpaceHeat_1919_1948_cold,
      HotWater_cold,
      MaxThermalCapacity_1919_1948
    ),
    ShareHP_1949_1978_cold = share_heatpump_function(
      SpaceHeat_1949_1978_cold,
      HotWater_cold,
      MaxThermalCapacity_1949_1978
    ),
    ShareHP_1979_1986_cold = share_heatpump_function(
      SpaceHeat_1979_1986_cold,
      HotWater_cold,
      MaxThermalCapacity_1979_1986
    ),
    ShareHP_1987_1990_cold = share_heatpump_function(
      SpaceHeat_1987_1990_cold,
      HotWater_cold,
      MaxThermalCapacity_1987_1990
    ),
    ShareHP_1991_1995_cold = share_heatpump_function(
      SpaceHeat_1991_1995_cold,
      HotWater_cold,
      MaxThermalCapacity_1991_1995
    ),
    ShareHP_1996_2000_cold = share_heatpump_function(
      SpaceHeat_1996_2000_cold,
      HotWater_cold,
      MaxThermalCapacity_1996_2000
    ),
    ShareHP_2001_2011_cold = share_heatpump_function(
      SpaceHeat_2001_2011_cold,
      HotWater_cold,
      MaxThermalCapacity_2001_2011
    ),
    ShareHP_2012_2022_cold = share_heatpump_function(
      SpaceHeat_2012_2022_cold,
      HotWater_cold,
      MaxThermalCapacity_2012_2022
    ),
    ShareHP_2023_2030_cold = share_heatpump_function(
      SpaceHeat_2023_2030_cold,
      HotWater_cold,
      MaxThermalCapacity_2023_2030
    )
  ) %>%
  select(
    -c(
      MaxThermalCapacity_beginn_1918,
      MaxThermalCapacity_1919_1948,
      MaxThermalCapacity_1949_1978,
      MaxThermalCapacity_1979_1986,
      MaxThermalCapacity_1987_1990,
      MaxThermalCapacity_1991_1995,
      MaxThermalCapacity_1996_2000,
      MaxThermalCapacity_2001_2011,
      MaxThermalCapacity_2012_2022,
      MaxThermalCapacity_2023_2030
    )
  )

eh_combined_heat_demand_hot <- eh_combined_heat_demand_hot %>%
  left_join(eh_combined_max_capacity, by = "Time") %>%
  mutate(
    ShareHP_beginn_1918_hot = share_heatpump_function(
      SpaceHeat_beginn_1918_hot,
      HotWater_hot,
      MaxThermalCapacity_beginn_1918
    ),
    ShareHP_1919_1948_hot = share_heatpump_function(
      SpaceHeat_1919_1948_hot,
      HotWater_hot,
      MaxThermalCapacity_1919_1948
    ),
    ShareHP_1949_1978_hot = share_heatpump_function(
      SpaceHeat_1949_1978_hot,
      HotWater_hot,
      MaxThermalCapacity_1949_1978
    ),
    ShareHP_1979_1986_hot = share_heatpump_function(
      SpaceHeat_1979_1986_hot,
      HotWater_hot,
      MaxThermalCapacity_1979_1986
    ),
    ShareHP_1987_1990_hot = share_heatpump_function(
      SpaceHeat_1987_1990_hot,
      HotWater_hot,
      MaxThermalCapacity_1987_1990
    ),
    ShareHP_1991_1995_hot = share_heatpump_function(
      SpaceHeat_1991_1995_hot,
      HotWater_hot,
      MaxThermalCapacity_1991_1995
    ),
    ShareHP_1996_2000_hot = share_heatpump_function(
      SpaceHeat_1996_2000_hot,
      HotWater_hot,
      MaxThermalCapacity_1996_2000
    ),
    ShareHP_2001_2011_hot = share_heatpump_function(
      SpaceHeat_2001_2011_hot,
      HotWater_hot,
      MaxThermalCapacity_2001_2011
    ),
    ShareHP_2012_2022_hot = share_heatpump_function(
      SpaceHeat_2012_2022_hot,
      HotWater_hot,
      MaxThermalCapacity_2012_2022
    ),
    ShareHP_2023_2030_hot = share_heatpump_function(
      SpaceHeat_2023_2030_hot,
      HotWater_hot,
      MaxThermalCapacity_2023_2030
    )
  ) %>%
  select(
    -c(
      MaxThermalCapacity_beginn_1918,
      MaxThermalCapacity_1919_1948,
      MaxThermalCapacity_1949_1978,
      MaxThermalCapacity_1979_1986,
      MaxThermalCapacity_1987_1990,
      MaxThermalCapacity_1991_1995,
      MaxThermalCapacity_1996_2000,
      MaxThermalCapacity_2001_2011,
      MaxThermalCapacity_2012_2022,
      MaxThermalCapacity_2023_2030
    )
  )


mh_combined_heat_demand_avg <- mh_combined_heat_demand_avg %>%
  left_join(mh_combined_max_capacity, by = "Time") %>%
  mutate(
    ShareHP_beginn_1918_avg = share_heatpump_function(
      SpaceHeat_beginn_1918_avg,
      HotWater_avg,
      MaxThermalCapacity_beginn_1918
    ),
    ShareHP_1919_1948_avg = share_heatpump_function(
      SpaceHeat_1919_1948_avg,
      HotWater_avg,
      MaxThermalCapacity_1919_1948
    ),
    ShareHP_1949_1978_avg = share_heatpump_function(
      SpaceHeat_1949_1978_avg,
      HotWater_avg,
      MaxThermalCapacity_1949_1978
    ),
    ShareHP_1979_1986_avg = share_heatpump_function(
      SpaceHeat_1979_1986_avg,
      HotWater_avg,
      MaxThermalCapacity_1979_1986
    ),
    ShareHP_1987_1990_avg = share_heatpump_function(
      SpaceHeat_1987_1990_avg,
      HotWater_avg,
      MaxThermalCapacity_1987_1990
    ),
    ShareHP_1991_1995_avg = share_heatpump_function(
      SpaceHeat_1991_1995_avg,
      HotWater_avg,
      MaxThermalCapacity_1991_1995
    ),
    ShareHP_1996_2000_avg = share_heatpump_function(
      SpaceHeat_1996_2000_avg,
      HotWater_avg,
      MaxThermalCapacity_1996_2000
    ),
    ShareHP_2001_2011_avg = share_heatpump_function(
      SpaceHeat_2001_2011_avg,
      HotWater_avg,
      MaxThermalCapacity_2001_2011
    ),
    ShareHP_2012_2022_avg = share_heatpump_function(
      SpaceHeat_2012_2022_avg,
      HotWater_avg,
      MaxThermalCapacity_2012_2022
    ),
    ShareHP_2023_2030_avg = share_heatpump_function(
      SpaceHeat_2023_2030_avg,
      HotWater_avg,
      MaxThermalCapacity_2023_2030
    )
  ) %>%
  select(
    -c(
      MaxThermalCapacity_beginn_1918,
      MaxThermalCapacity_1919_1948,
      MaxThermalCapacity_1949_1978,
      MaxThermalCapacity_1979_1986,
      MaxThermalCapacity_1987_1990,
      MaxThermalCapacity_1991_1995,
      MaxThermalCapacity_1996_2000,
      MaxThermalCapacity_2001_2011,
      MaxThermalCapacity_2012_2022,
      MaxThermalCapacity_2023_2030
    )
  )

mh_combined_heat_demand_cold <- mh_combined_heat_demand_cold %>%
  left_join(mh_combined_max_capacity, by = "Time") %>%
  mutate(
    ShareHP_beginn_1918_cold = share_heatpump_function(
      SpaceHeat_beginn_1918_cold,
      HotWater_cold,
      MaxThermalCapacity_beginn_1918
    ),
    ShareHP_1919_1948_cold = share_heatpump_function(
      SpaceHeat_1919_1948_cold,
      HotWater_cold,
      MaxThermalCapacity_1919_1948
    ),
    ShareHP_1949_1978_cold = share_heatpump_function(
      SpaceHeat_1949_1978_cold,
      HotWater_cold,
      MaxThermalCapacity_1949_1978
    ),
    ShareHP_1979_1986_cold = share_heatpump_function(
      SpaceHeat_1979_1986_cold,
      HotWater_cold,
      MaxThermalCapacity_1979_1986
    ),
    ShareHP_1987_1990_cold = share_heatpump_function(
      SpaceHeat_1987_1990_cold,
      HotWater_cold,
      MaxThermalCapacity_1987_1990
    ),
    ShareHP_1991_1995_cold = share_heatpump_function(
      SpaceHeat_1991_1995_cold,
      HotWater_cold,
      MaxThermalCapacity_1991_1995
    ),
    ShareHP_1996_2000_cold = share_heatpump_function(
      SpaceHeat_1996_2000_cold,
      HotWater_cold,
      MaxThermalCapacity_1996_2000
    ),
    ShareHP_2001_2011_cold = share_heatpump_function(
      SpaceHeat_2001_2011_cold,
      HotWater_cold,
      MaxThermalCapacity_2001_2011
    ),
    ShareHP_2012_2022_cold = share_heatpump_function(
      SpaceHeat_2012_2022_cold,
      HotWater_cold,
      MaxThermalCapacity_2012_2022
    ),
    ShareHP_2023_2030_cold = share_heatpump_function(
      SpaceHeat_2023_2030_cold,
      HotWater_cold,
      MaxThermalCapacity_2023_2030
    )
  ) %>%
  select(
    -c(
      MaxThermalCapacity_beginn_1918,
      MaxThermalCapacity_1919_1948,
      MaxThermalCapacity_1949_1978,
      MaxThermalCapacity_1979_1986,
      MaxThermalCapacity_1987_1990,
      MaxThermalCapacity_1991_1995,
      MaxThermalCapacity_1996_2000,
      MaxThermalCapacity_2001_2011,
      MaxThermalCapacity_2012_2022,
      MaxThermalCapacity_2023_2030
    )
  )

mh_combined_heat_demand_hot <- mh_combined_heat_demand_hot %>%
  left_join(mh_combined_max_capacity, by = "Time") %>%
  mutate(
    ShareHP_beginn_1918_hot = share_heatpump_function(
      SpaceHeat_beginn_1918_hot,
      HotWater_hot,
      MaxThermalCapacity_beginn_1918
    ),
    ShareHP_1919_1948_hot = share_heatpump_function(
      SpaceHeat_1919_1948_hot,
      HotWater_hot,
      MaxThermalCapacity_1919_1948
    ),
    ShareHP_1949_1978_hot = share_heatpump_function(
      SpaceHeat_1949_1978_hot,
      HotWater_hot,
      MaxThermalCapacity_1949_1978
    ),
    ShareHP_1979_1986_hot = share_heatpump_function(
      SpaceHeat_1979_1986_hot,
      HotWater_hot,
      MaxThermalCapacity_1979_1986
    ),
    ShareHP_1987_1990_hot = share_heatpump_function(
      SpaceHeat_1987_1990_hot,
      HotWater_hot,
      MaxThermalCapacity_1987_1990
    ),
    ShareHP_1991_1995_hot = share_heatpump_function(
      SpaceHeat_1991_1995_hot,
      HotWater_hot,
      MaxThermalCapacity_1991_1995
    ),
    ShareHP_1996_2000_hot = share_heatpump_function(
      SpaceHeat_1996_2000_hot,
      HotWater_hot,
      MaxThermalCapacity_1996_2000
    ),
    ShareHP_2001_2011_hot = share_heatpump_function(
      SpaceHeat_2001_2011_hot,
      HotWater_hot,
      MaxThermalCapacity_2001_2011
    ),
    ShareHP_2012_2022_hot = share_heatpump_function(
      SpaceHeat_2012_2022_hot,
      HotWater_hot,
      MaxThermalCapacity_2012_2022
    ),
    ShareHP_2023_2030_hot = share_heatpump_function(
      SpaceHeat_2023_2030_hot,
      HotWater_hot,
      MaxThermalCapacity_2023_2030
    )
  ) %>%
  select(
    -c(
      MaxThermalCapacity_beginn_1918,
      MaxThermalCapacity_1919_1948,
      MaxThermalCapacity_1949_1978,
      MaxThermalCapacity_1979_1986,
      MaxThermalCapacity_1987_1990,
      MaxThermalCapacity_1991_1995,
      MaxThermalCapacity_1996_2000,
      MaxThermalCapacity_2001_2011,
      MaxThermalCapacity_2012_2022,
      MaxThermalCapacity_2023_2030
    )
  )



# Calculate the theoretical COP for the mono energetic heat pump system
eh_combined_heat_demand_avg <- eh_combined_heat_demand_avg %>%
  mutate(
    COPHPTheo_beginn_1918_avg = (
      ShareHP_beginn_1918_avg * (FlowTemp_beginn_1918_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_beginn_1918_avg * (FlowTemp_beginn_1918_avg - TemperatureKelvin)
    ),
    COPHPTheo_1919_1948_avg = (
      ShareHP_1919_1948_avg * (FlowTemp_1919_1948_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1919_1948_avg * (FlowTemp_1919_1948_avg - TemperatureKelvin)
    ),
    COPHPTheo_1949_1978_avg = (
      ShareHP_1949_1978_avg * (FlowTemp_1949_1978_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1949_1978_avg * (FlowTemp_1949_1978_avg - TemperatureKelvin)
    ),
    COPHPTheo_1979_1986_avg = (
      ShareHP_1979_1986_avg * (FlowTemp_1979_1986_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1979_1986_avg * (FlowTemp_1979_1986_avg - TemperatureKelvin)
    ),
    COPHPTheo_1987_1990_avg = (
      ShareHP_1987_1990_avg * (FlowTemp_1987_1990_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1987_1990_avg * (FlowTemp_1987_1990_avg - TemperatureKelvin)
    ),
    COPHPTheo_1991_1995_avg = (
      ShareHP_1991_1995_avg * (FlowTemp_1991_1995_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1991_1995_avg * (FlowTemp_1991_1995_avg - TemperatureKelvin)
    ),
    COPHPTheo_1996_2000_avg = (
      ShareHP_1996_2000_avg * (FlowTemp_1996_2000_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1996_2000_avg * (FlowTemp_1996_2000_avg - TemperatureKelvin)
    ),
    COPHPTheo_2001_2011_avg = (
      ShareHP_2001_2011_avg * (FlowTemp_2001_2011_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2001_2011_avg * (FlowTemp_2001_2011_avg - TemperatureKelvin)
    ),
    COPHPTheo_2012_2022_avg = (
      ShareHP_2012_2022_avg * (FlowTemp_2012_2022_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2012_2022_avg * (FlowTemp_2012_2022_avg - TemperatureKelvin)
    ),
    COPHPTheo_2023_2030_avg = (
      ShareHP_2023_2030_avg * (FlowTemp_2023_2030_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2023_2030_avg * (FlowTemp_2023_2030_avg - TemperatureKelvin)
    )
  )

eh_combined_heat_demand_cold <- eh_combined_heat_demand_cold %>%
  mutate(
    COPHPTheo_beginn_1918_cold = (
      ShareHP_beginn_1918_cold * (FlowTemp_beginn_1918_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_beginn_1918_cold * (FlowTemp_beginn_1918_cold - TemperatureKelvin)
    ),
    COPHPTheo_1919_1948_cold = (
      ShareHP_1919_1948_cold * (FlowTemp_1919_1948_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1919_1948_cold * (FlowTemp_1919_1948_cold - TemperatureKelvin)
    ),
    COPHPTheo_1949_1978_cold = (
      ShareHP_1949_1978_cold * (FlowTemp_1949_1978_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1949_1978_cold * (FlowTemp_1949_1978_cold - TemperatureKelvin)
    ),
    COPHPTheo_1979_1986_cold = (
      ShareHP_1979_1986_cold * (FlowTemp_1979_1986_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1979_1986_cold * (FlowTemp_1979_1986_cold - TemperatureKelvin)
    ),
    COPHPTheo_1987_1990_cold = (
      ShareHP_1987_1990_cold * (FlowTemp_1987_1990_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1987_1990_cold * (FlowTemp_1987_1990_cold - TemperatureKelvin)
    ),
    COPHPTheo_1991_1995_cold = (
      ShareHP_1991_1995_cold * (FlowTemp_1991_1995_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1991_1995_cold * (FlowTemp_1991_1995_cold - TemperatureKelvin)
    ),
    COPHPTheo_1996_2000_cold = (
      ShareHP_1996_2000_cold * (FlowTemp_1996_2000_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1996_2000_cold * (FlowTemp_1996_2000_cold - TemperatureKelvin)
    ),
    COPHPTheo_2001_2011_cold = (
      ShareHP_2001_2011_cold * (FlowTemp_2001_2011_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2001_2011_cold * (FlowTemp_2001_2011_cold - TemperatureKelvin)
    ),
    COPHPTheo_2012_2022_cold = (
      ShareHP_2012_2022_cold * (FlowTemp_2012_2022_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2012_2022_cold * (FlowTemp_2012_2022_cold - TemperatureKelvin)
    ),
    COPHPTheo_2023_2030_cold = (
      ShareHP_2023_2030_cold * (FlowTemp_2023_2030_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2023_2030_cold * (FlowTemp_2023_2030_cold - TemperatureKelvin)
    )
  )

eh_combined_heat_demand_hot <- eh_combined_heat_demand_hot %>%
  mutate(
    COPHPTheo_beginn_1918_hot = (
      ShareHP_beginn_1918_hot * (FlowTemp_beginn_1918_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_beginn_1918_hot * (FlowTemp_beginn_1918_hot - TemperatureKelvin)
    ),
    COPHPTheo_1919_1948_hot = (
      ShareHP_1919_1948_hot * (FlowTemp_1919_1948_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1919_1948_hot * (FlowTemp_1919_1948_hot - TemperatureKelvin)
    ),
    COPHPTheo_1949_1978_hot = (
      ShareHP_1949_1978_hot * (FlowTemp_1949_1978_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1949_1978_hot * (FlowTemp_1949_1978_hot - TemperatureKelvin)
    ),
    COPHPTheo_1979_1986_hot = (
      ShareHP_1979_1986_hot * (FlowTemp_1979_1986_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1979_1986_hot * (FlowTemp_1979_1986_hot - TemperatureKelvin)
    ),
    COPHPTheo_1987_1990_hot = (
      ShareHP_1987_1990_hot * (FlowTemp_1987_1990_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1987_1990_hot * (FlowTemp_1987_1990_hot - TemperatureKelvin)
    ),
    COPHPTheo_1991_1995_hot = (
      ShareHP_1991_1995_hot * (FlowTemp_1991_1995_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1991_1995_hot * (FlowTemp_1991_1995_hot - TemperatureKelvin)
    ),
    COPHPTheo_1996_2000_hot = (
      ShareHP_1996_2000_hot * (FlowTemp_1996_2000_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1996_2000_hot * (FlowTemp_1996_2000_hot - TemperatureKelvin)
    ),
    COPHPTheo_2001_2011_hot = (
      ShareHP_2001_2011_hot * (FlowTemp_2001_2011_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2001_2011_hot * (FlowTemp_2001_2011_hot - TemperatureKelvin)
    ),
    COPHPTheo_2012_2022_hot = (
      ShareHP_2012_2022_hot * (FlowTemp_2012_2022_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2012_2022_hot * (FlowTemp_2012_2022_hot - TemperatureKelvin)
    ),
    COPHPTheo_2023_2030_hot = (
      ShareHP_2023_2030_hot * (FlowTemp_2023_2030_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2023_2030_hot * (FlowTemp_2023_2030_hot - TemperatureKelvin)
    )
  )

mh_combined_heat_demand_avg <- mh_combined_heat_demand_avg %>%
  mutate(
    COPHPTheo_beginn_1918_avg = (
      ShareHP_beginn_1918_avg * (FlowTemp_beginn_1918_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_beginn_1918_avg * (FlowTemp_beginn_1918_avg - TemperatureKelvin)
    ),
    COPHPTheo_1919_1948_avg = (
      ShareHP_1919_1948_avg * (FlowTemp_1919_1948_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1919_1948_avg * (FlowTemp_1919_1948_avg - TemperatureKelvin)
    ),
    COPHPTheo_1949_1978_avg = (
      ShareHP_1949_1978_avg * (FlowTemp_1949_1978_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1949_1978_avg * (FlowTemp_1949_1978_avg - TemperatureKelvin)
    ),
    COPHPTheo_1979_1986_avg = (
      ShareHP_1979_1986_avg * (FlowTemp_1979_1986_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1979_1986_avg * (FlowTemp_1979_1986_avg - TemperatureKelvin)
    ),
    COPHPTheo_1987_1990_avg = (
      ShareHP_1987_1990_avg * (FlowTemp_1987_1990_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1987_1990_avg * (FlowTemp_1987_1990_avg - TemperatureKelvin)
    ),
    COPHPTheo_1991_1995_avg = (
      ShareHP_1991_1995_avg * (FlowTemp_1991_1995_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1991_1995_avg * (FlowTemp_1991_1995_avg - TemperatureKelvin)
    ),
    COPHPTheo_1996_2000_avg = (
      ShareHP_1996_2000_avg * (FlowTemp_1996_2000_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1996_2000_avg * (FlowTemp_1996_2000_avg - TemperatureKelvin)
    ),
    COPHPTheo_2001_2011_avg = (
      ShareHP_2001_2011_avg * (FlowTemp_2001_2011_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2001_2011_avg * (FlowTemp_2001_2011_avg - TemperatureKelvin)
    ),
    COPHPTheo_2012_2022_avg = (
      ShareHP_2012_2022_avg * (FlowTemp_2012_2022_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2012_2022_avg * (FlowTemp_2012_2022_avg - TemperatureKelvin)
    ),
    COPHPTheo_2023_2030_avg = (
      ShareHP_2023_2030_avg * (FlowTemp_2023_2030_avg  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2023_2030_avg * (FlowTemp_2023_2030_avg - TemperatureKelvin)
    )
  )

mh_combined_heat_demand_cold <- mh_combined_heat_demand_cold %>%
  mutate(
    COPHPTheo_beginn_1918_cold = (
      ShareHP_beginn_1918_cold * (FlowTemp_beginn_1918_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_beginn_1918_cold * (FlowTemp_beginn_1918_cold - TemperatureKelvin)
    ),
    COPHPTheo_1919_1948_cold = (
      ShareHP_1919_1948_cold * (FlowTemp_1919_1948_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1919_1948_cold * (FlowTemp_1919_1948_cold - TemperatureKelvin)
    ),
    COPHPTheo_1949_1978_cold = (
      ShareHP_1949_1978_cold * (FlowTemp_1949_1978_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1949_1978_cold * (FlowTemp_1949_1978_cold - TemperatureKelvin)
    ),
    COPHPTheo_1979_1986_cold = (
      ShareHP_1979_1986_cold * (FlowTemp_1979_1986_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1979_1986_cold * (FlowTemp_1979_1986_cold - TemperatureKelvin)
    ),
    COPHPTheo_1987_1990_cold = (
      ShareHP_1987_1990_cold * (FlowTemp_1987_1990_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1987_1990_cold * (FlowTemp_1987_1990_cold - TemperatureKelvin)
    ),
    COPHPTheo_1991_1995_cold = (
      ShareHP_1991_1995_cold * (FlowTemp_1991_1995_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1991_1995_cold * (FlowTemp_1991_1995_cold - TemperatureKelvin)
    ),
    COPHPTheo_1996_2000_cold = (
      ShareHP_1996_2000_cold * (FlowTemp_1996_2000_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1996_2000_cold * (FlowTemp_1996_2000_cold - TemperatureKelvin)
    ),
    COPHPTheo_2001_2011_cold = (
      ShareHP_2001_2011_cold * (FlowTemp_2001_2011_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2001_2011_cold * (FlowTemp_2001_2011_cold - TemperatureKelvin)
    ),
    COPHPTheo_2012_2022_cold = (
      ShareHP_2012_2022_cold * (FlowTemp_2012_2022_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2012_2022_cold * (FlowTemp_2012_2022_cold - TemperatureKelvin)
    ),
    COPHPTheo_2023_2030_cold = (
      ShareHP_2023_2030_cold * (FlowTemp_2023_2030_cold  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2023_2030_cold * (FlowTemp_2023_2030_cold - TemperatureKelvin)
    )
  )

mh_combined_heat_demand_hot <- mh_combined_heat_demand_hot %>%
  mutate(
    COPHPTheo_beginn_1918_hot = (
      ShareHP_beginn_1918_hot * (FlowTemp_beginn_1918_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_beginn_1918_hot * (FlowTemp_beginn_1918_hot - TemperatureKelvin)
    ),
    COPHPTheo_1919_1948_hot = (
      ShareHP_1919_1948_hot * (FlowTemp_1919_1948_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1919_1948_hot * (FlowTemp_1919_1948_hot - TemperatureKelvin)
    ),
    COPHPTheo_1949_1978_hot = (
      ShareHP_1949_1978_hot * (FlowTemp_1949_1978_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1949_1978_hot * (FlowTemp_1949_1978_hot - TemperatureKelvin)
    ),
    COPHPTheo_1979_1986_hot = (
      ShareHP_1979_1986_hot * (FlowTemp_1979_1986_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1979_1986_hot * (FlowTemp_1979_1986_hot - TemperatureKelvin)
    ),
    COPHPTheo_1987_1990_hot = (
      ShareHP_1987_1990_hot * (FlowTemp_1987_1990_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1987_1990_hot * (FlowTemp_1987_1990_hot - TemperatureKelvin)
    ),
    COPHPTheo_1991_1995_hot = (
      ShareHP_1991_1995_hot * (FlowTemp_1991_1995_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1991_1995_hot * (FlowTemp_1991_1995_hot - TemperatureKelvin)
    ),
    COPHPTheo_1996_2000_hot = (
      ShareHP_1996_2000_hot * (FlowTemp_1996_2000_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_1996_2000_hot * (FlowTemp_1996_2000_hot - TemperatureKelvin)
    ),
    COPHPTheo_2001_2011_hot = (
      ShareHP_2001_2011_hot * (FlowTemp_2001_2011_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2001_2011_hot * (FlowTemp_2001_2011_hot - TemperatureKelvin)
    ),
    COPHPTheo_2012_2022_hot = (
      ShareHP_2012_2022_hot * (FlowTemp_2012_2022_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2012_2022_hot * (FlowTemp_2012_2022_hot - TemperatureKelvin)
    ),
    COPHPTheo_2023_2030_hot = (
      ShareHP_2023_2030_hot * (FlowTemp_2023_2030_hot  - TemperatureKelvin) + TemperatureKelvin
    ) / (
      ShareHP_2023_2030_hot * (FlowTemp_2023_2030_hot - TemperatureKelvin)
    )
  )
