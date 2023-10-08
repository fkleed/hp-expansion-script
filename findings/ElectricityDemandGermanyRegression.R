# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read data
electricity_demand_reference_germany_sh_and_hw <-
  read_csv("data/output/findings/electricity-demand-germany/electricity_demand_reference_germany_sh_and_hw.csv")

electricity_demand_cold_germany_sh_and_hw <-
  read_csv("data/output/findings/electricity-demand-germany/electricity_demand_cold_germany_sh_and_hw.csv")

electricity_demand_hot_germany_sh_and_hw <-
  read_csv("data/output/findings/electricity-demand-germany/electricity_demand_hot_germany_sh_and_hw.csv")


electricity_demand_reference_germany_sh_only <-
  read_csv("data/output/findings/electricity-demand-germany/electricity_demand_reference_germany_sh_only.csv")

electricity_demand_cold_germany_sh_only <-
  read_csv("data/output/findings/electricity-demand-germany/electricity_demand_cold_germany_sh_only.csv")

electricity_demand_hot_germany_sh_only <-
  read_csv("data/output/findings/electricity-demand-germany/electricity_demand_hot_germany_sh_only.csv")


# Read the data
air_temp_2010 <-
  read_csv2("data/output/weathermodel/year2010.csv") %>%
  select(-c("RoundedMeanTemperature")) %>%
  rename("Temperature" = "MeanTemperature") %>%
  mutate_if(is.character, as.factor) %>%
  relocate(Date, .before = Temperature) %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(Date, 1, 2) == "00", 0, sub("^0+", "", substr(Date, 1, 2))),
    ifelse(substr(Date, 3, 4) == "00", 0, sub("^0+", "", substr(Date, 3, 4))),
    ifelse(substr(Date, 5, 6) == "00", 0, sub("^0+", "", substr(Date, 5, 6)))
  )) %>%
  select(
    -c("Date")
  )

air_temp_2017 <-
  read_csv2("data/output/weathermodel/year2017.csv") %>%
  select(-c("RoundedMeanTemperature")) %>%
  rename("Temperature" = "MeanTemperature") %>%
  mutate_if(is.character, as.factor) %>%
  relocate(Date, .before = Temperature) %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(Date, 1, 2) == "00", 0, sub("^0+", "", substr(Date, 1, 2))),
    ifelse(substr(Date, 3, 4) == "00", 0, sub("^0+", "", substr(Date, 3, 4))),
    ifelse(substr(Date, 5, 6) == "00", 0, sub("^0+", "", substr(Date, 5, 6)))
  )) %>%
  select(
    -c("Date")
  )

air_temp_2022 <-
  read_csv2("data/output/weathermodel/year2022.csv") %>%
  select(-c("RoundedMeanTemperature")) %>%
  rename("Temperature" = "MeanTemperature") %>%
  mutate_if(is.character, as.factor) %>%
  relocate(Date, .before = Temperature)%>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(Date, 1, 2) == "00", 0, sub("^0+", "", substr(Date, 1, 2))),
    ifelse(substr(Date, 3, 4) == "00", 0, sub("^0+", "", substr(Date, 3, 4))),
    ifelse(substr(Date, 5, 6) == "00", 0, sub("^0+", "", substr(Date, 5, 6)))
  )) %>%
  select(
    -c("Date")
  )


# Linear regression space heat and hot water
electricity_demand_reference_germany_sh_and_hw <-
  electricity_demand_reference_germany_sh_and_hw %>%
  left_join(air_temp_2017, by=c("DATEISO"))

electricity_demand_cold_germany_sh_and_hw <-
  electricity_demand_cold_germany_sh_and_hw %>%
  left_join(air_temp_2010, by=c("DATEISO"))

electricity_demand_hot_germany_sh_and_hw <-
  electricity_demand_hot_germany_sh_and_hw %>%
  left_join(air_temp_2022, by=c("DATEISO"))


electricity_demand_germany_sh_and_hw <-
  electricity_demand_reference_germany_sh_and_hw %>%
  rbind(electricity_demand_cold_germany_sh_and_hw) %>%
  rbind(electricity_demand_hot_germany_sh_and_hw)

electricity_demand_germany_sh_and_hw <-
  electricity_demand_germany_sh_and_hw %>%
  mutate(
    Temperature = Temperature + 273.15
  )

electricity_demand_germany_sh_and_hw_filterd <-
  electricity_demand_germany_sh_and_hw %>%
  filter(Temperature <= 293.15)

plot(electricity_demand_germany_sh_and_hw$Temperature,electricity_demand_germany_sh_and_hw$`Electricity demand`)
plot(electricity_demand_germany_sh_and_hw_filterd$Temperature,electricity_demand_germany_sh_and_hw_filterd$`Electricity demand`)


model_sh_and_hw <- lm(`Electricity demand`~Temperature, data = electricity_demand_germany_sh_and_hw)
model_sh_and_hw_filtered <- lm(`Electricity demand`~Temperature, data = electricity_demand_germany_sh_and_hw_filterd)

summary(model_sh_and_hw)
summary(model_sh_and_hw_filtered)


# Linear regression space heat only
electricity_demand_reference_germany_sh_only <-
  electricity_demand_reference_germany_sh_only %>%
  left_join(air_temp_2017, by=c("DATEISO"))

electricity_demand_cold_germany_sh_only <-
  electricity_demand_cold_germany_sh_only %>%
  left_join(air_temp_2010, by=c("DATEISO"))

electricity_demand_hot_germany_sh_only <-
  electricity_demand_hot_germany_sh_only %>%
  left_join(air_temp_2022, by=c("DATEISO"))


electricity_demand_germany_sh_only <-
  electricity_demand_reference_germany_sh_only %>%
  rbind(electricity_demand_cold_germany_sh_only) %>%
  rbind(electricity_demand_hot_germany_sh_only)

electricity_demand_germany_sh_only <-
  electricity_demand_germany_sh_only %>%
  mutate(
    Temperature = Temperature + 273.15
  )

electricity_demand_germany_sh_only_filterd <-
  electricity_demand_germany_sh_only %>%
  filter(Temperature <= 293.15)

plot(electricity_demand_germany_sh_only$Temperature,electricity_demand_germany_sh_only$`Electricity demand`)
plot(electricity_demand_germany_sh_only_filterd$Temperature,electricity_demand_germany_sh_only_filterd$`Electricity demand`)


model_sh_only <- lm(`Electricity demand`~Temperature, data = electricity_demand_germany_sh_only)
model_sh_only_filtered <- lm(`Electricity demand`~Temperature, data = electricity_demand_germany_sh_only_filterd)

summary(model_sh_only)
summary(model_sh_only_filtered)


# Calculate min air temperatures
min(air_temp_2017$Temperature)
min(air_temp_2010$Temperature)
min(air_temp_2022$Temperature)

mean(air_temp_2017$Temperature)
mean(air_temp_2010$Temperature)
mean(air_temp_2022$Temperature)
