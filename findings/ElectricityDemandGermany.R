# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read the regions electricity demand data
regions_electricity_demand_reference <-
  read_csv("data/regionselectricitydemand/regions_electricity_demand_reference.csv")

regions_electricity_demand_cold <-
  read_csv("data/regionselectricitydemand/regions_electricity_demand_cold.csv")

regions_electricity_demand_hot <-
  read_csv("data/regionselectricitydemand/regions_electricity_demand_hot.csv")


regions_electricity_demand_space_heat_only_reference <-
  read_csv(
    "data/regionselectricitydemand/regions_electricity_demand_space_heat_only_reference.csv"
  )

regions_electricity_demand_space_heat_only_cold <-
  read_csv(
    "data/regionselectricitydemand/regions_electricity_demand_space_heat_only_cold.csv"
  )

regions_electricity_demand_space_heat_only_hot <-
  read_csv(
    "data/regionselectricitydemand/regions_electricity_demand_space_heat_only_hot.csv"
  )

# Aggregate region electricity demand data to level of whole Germany

# Space heat and hot water
electricity_demand_reference_germany_sh_and_hw <-
  regions_electricity_demand_reference %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_reference_germany_sh_and_hw <-
  electricity_demand_reference_germany_sh_and_hw %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_reference_germany_sh_and_hw <-
  electricity_demand_reference_germany_sh_and_hw %>%
  arrange(DATEISO)


electricity_demand_cold_germany_sh_and_hw <-
  regions_electricity_demand_cold %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_cold_germany_sh_and_hw <-
  electricity_demand_cold_germany_sh_and_hw %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_cold_germany_sh_and_hw <-
  electricity_demand_cold_germany_sh_and_hw %>%
  arrange(DATEISO)


electricity_demand_hot_germany_sh_and_hw <-
  regions_electricity_demand_hot %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_hot_germany_sh_and_hw <-
  electricity_demand_hot_germany_sh_and_hw %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_hot_germany_sh_and_hw <-
  electricity_demand_hot_germany_sh_and_hw %>%
  arrange(DATEISO)


# Space heat only
electricity_demand_reference_germany_sh_only <-
  regions_electricity_demand_space_heat_only_reference %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_reference_germany_sh_only <-
  electricity_demand_reference_germany_sh_only %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_reference_germany_sh_only <-
  electricity_demand_reference_germany_sh_only %>%
  arrange(DATEISO)


electricity_demand_cold_germany_sh_only <-
  regions_electricity_demand_space_heat_only_cold %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_cold_germany_sh_only <-
  electricity_demand_cold_germany_sh_only %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_cold_germany_sh_only <-
  electricity_demand_cold_germany_sh_only %>%
  arrange(DATEISO)


electricity_demand_hot_germany_sh_only <-
  regions_electricity_demand_space_heat_only_hot %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_hot_germany_sh_only <-
  electricity_demand_hot_germany_sh_only %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_hot_germany_sh_only <-
  electricity_demand_hot_germany_sh_only %>%
  arrange(DATEISO)


# Total electricity demand over year

# Space heat and hot water
sum(electricity_demand_cold_germany_sh_and_hw$`Electricity demand`) / 1000000000
sum(electricity_demand_reference_germany_sh_and_hw$`Electricity demand`) / 1000000000
sum(electricity_demand_hot_germany_sh_and_hw$`Electricity demand`) / 1000000000

# Space heat only
sum(electricity_demand_cold_germany_sh_only$`Electricity demand`) / 1000000000
sum(electricity_demand_reference_germany_sh_only$`Electricity demand`) / 1000000000
sum(electricity_demand_hot_germany_sh_only$`Electricity demand`) / 1000000000


# Max hourly electricity demand

# Space heat and hot water
max(electricity_demand_cold_germany_sh_and_hw$`Electricity demand`) / 1000000
max(electricity_demand_reference_germany_sh_and_hw$`Electricity demand`) / 1000000
max(electricity_demand_hot_germany_sh_and_hw$`Electricity demand`) / 1000000

# Space heat only
max(electricity_demand_cold_germany_sh_only$`Electricity demand`) / 1000000
max(electricity_demand_reference_germany_sh_only$`Electricity demand`) / 1000000
max(electricity_demand_hot_germany_sh_only$`Electricity demand`) / 1000000


# Mean hourly electricity demand

# Space heat and hot water
mean(electricity_demand_cold_germany_sh_and_hw$`Electricity demand`) / 1000000
mean(electricity_demand_reference_germany_sh_and_hw$`Electricity demand`) / 1000000
mean(electricity_demand_hot_germany_sh_and_hw$`Electricity demand`) / 1000000

# Space heat only
mean(electricity_demand_cold_germany_sh_only$`Electricity demand`) / 1000000
mean(electricity_demand_reference_germany_sh_only$`Electricity demand`) / 1000000
mean(electricity_demand_hot_germany_sh_only$`Electricity demand`) / 1000000


# Write output to csv
write_csv(
  electricity_demand_reference_germany_sh_and_hw,
  "data/output/findings/electricity-demand-germany/electricity_demand_reference_germany_sh_and_hw.csv"
)

write_csv(
  electricity_demand_cold_germany_sh_and_hw,
  "data/output/findings/electricity-demand-germany/electricity_demand_cold_germany_sh_and_hw.csv"
)

write_csv(
  electricity_demand_hot_germany_sh_and_hw,
  "data/output/findings/electricity-demand-germany/electricity_demand_hot_germany_sh_and_hw.csv"
)


write_csv(
  electricity_demand_reference_germany_sh_only,
  "data/output/findings/electricity-demand-germany/electricity_demand_reference_germany_sh_only.csv"
)

write_csv(
  electricity_demand_cold_germany_sh_only,
  "data/output/findings/electricity-demand-germany/electricity_demand_cold_germany_sh_only.csv"
)

write_csv(
  electricity_demand_hot_germany_sh_only,
  "data/output/findings/electricity-demand-germany/electricity_demand_hot_germany_sh_only.csv"
)
