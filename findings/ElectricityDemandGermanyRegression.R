# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")
library(zoo)

# Read data
electricity_demand_reference_germany_sh_and_hw <-
  read_csv("data/output/findings/electricity-demand-germany/electricity_demand_reference_germany_sh_and_hw.csv")


electricity_demand_reference_germany_sh_only <-
  read_csv("data/output/findings/electricity-demand-germany/electricity_demand_reference_germany_sh_only.csv")


air_temp_2017 <-
  read_csv2("data/output/weathermodel/year2017.csv") %>%
  mutate(
    "TemperatureKelvin" = as.numeric(RoundedMeanTemperature) + 273.15
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(Date, 1, 2) == "00", 0, sub("^0+", "", substr(Date, 1, 2))),
    ifelse(substr(Date, 3, 4) == "00", 0, sub("^0+", "", substr(Date, 3, 4))),
    ifelse(substr(Date, 5, 6) == "00", 0, sub("^0+", "", substr(Date, 5, 6)))
  )) %>%
  select(
    -c("MeanTemperature", "RoundedMeanTemperature", "Date")
  )


# Calculate mix temperature
air_temp_2017 <-
  tibble::rowid_to_column(air_temp_2017, "RowNumber")

air_temp_2017  <-
  air_temp_2017 %>% mutate(T72WeightedAverage = rollmeanr(TemperatureKelvin,
                                                                   72,
                                                                   fill = NA))

air_temp_2017_until_71 <-
  air_temp_2017 %>% filter(RowNumber < 72) %>%
  mutate(CumSumTemperatureKelvin = cumsum(TemperatureKelvin)) %>%
  mutate(T72WeightedAverage = (CumSumTemperatureKelvin / RowNumber)) %>%
  select(-c(CumSumTemperatureKelvin))

air_temp_2017 <- air_temp_2017 %>%
  filter(RowNumber >= 72)

air_temp_2017 <-
  rbind(air_temp_2017_until_71, air_temp_2017)

rm(air_temp_2017_until_71)

air_temp_2017 <- air_temp_2017 %>%
  mutate(TMix = ifelse(
    substr(DATEISO, 12, 13) %in% c("10", "11", "12", "13", "14", "15", "16"),
    TemperatureKelvin,
    T72WeightedAverage
  )) %>%
  select(-c(RowNumber, T72WeightedAverage))


# Linear regression space heat and hot water
electricity_demand_reference_germany_sh_and_hw <-
  electricity_demand_reference_germany_sh_and_hw %>%
  left_join(air_temp_2017, by=c("date_iso"= "DATEISO"))

electricity_demand_reference_germany_sh_and_hw_filtered <-
  electricity_demand_reference_germany_sh_and_hw %>%
  filter(TMix < 293.15)


plot(electricity_demand_reference_germany_sh_and_hw$TemperatureKelvin,electricity_demand_reference_germany_sh_and_hw$hourly_electricity_demand)
plot(electricity_demand_reference_germany_sh_and_hw_filtered$TemperatureKelvin,electricity_demand_reference_germany_sh_and_hw_filtered$hourly_electricity_demand)


model_sh_and_hw <- lm(hourly_electricity_demand~TemperatureKelvin, data = electricity_demand_reference_germany_sh_and_hw)
model_sh_and_hw_filtered <- lm(hourly_electricity_demand~TemperatureKelvin, data = electricity_demand_reference_germany_sh_and_hw_filtered)

summary(model_sh_and_hw)
summary(model_sh_and_hw_filtered)


# Linear regression space heat only
electricity_demand_reference_germany_sh_only <-
  electricity_demand_reference_germany_sh_only %>%
  left_join(air_temp_2017, by=c("date_iso"= "DATEISO"))


electricity_demand_reference_germany_sh_only_filterd <-
  electricity_demand_reference_germany_sh_only %>%
  filter(TMix < 293.15)

plot(electricity_demand_reference_germany_sh_only$TemperatureKelvin,electricity_demand_reference_germany_sh_only$hourly_electricity_demand)
plot(electricity_demand_reference_germany_sh_only_filterd$TemperatureKelvin,electricity_demand_reference_germany_sh_only_filterd$hourly_electricity_demand)


model_sh_only <- lm(hourly_electricity_demand~TemperatureKelvin, data = electricity_demand_reference_germany_sh_only)
model_sh_only_filtered <- lm(hourly_electricity_demand~TemperatureKelvin, data = electricity_demand_reference_germany_sh_only_filterd)

summary(model_sh_only)
summary(model_sh_only_filtered)
