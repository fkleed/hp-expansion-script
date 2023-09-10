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


# Plot the combined electricity demand fo Germany

# For space heating and hot water
electricity_demand_reference_germany_sh_and_hw <-
  regions_electricity_demand_reference %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_reference_germany_sh_and_hw <-
  electricity_demand_reference_germany_sh_and_hw %>%
  mutate(DATEISO = ISOdate(
    2017,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_reference_germany_sh_and_hw <-
  electricity_demand_reference_germany_sh_and_hw %>%
  arrange(DATEISO)

electricity_demand_germany_reference_sh_and_hw_plot <-
  ggplot(data = electricity_demand_reference_germany_sh_and_hw, aes(DATEISO, `Electricity demand` / 1000000)) +
  geom_line(lwd = 0.5) + ylab("Electricity demand in GWh") + xlab("Date") + coord_cartesian(ylim = c(0, 55))

electricity_demand_germany_reference_sh_and_hw_plot

sum(electricity_demand_reference_germany_sh_and_hw$`Electricity demand`)


electricity_demand_cold_germany_sh_and_hw <-
  regions_electricity_demand_cold %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_cold_germany_sh_and_hw <-
  electricity_demand_cold_germany_sh_and_hw %>%
  mutate(DATEISO = ISOdate(
    2010,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_cold_germany_sh_and_hw <-
  electricity_demand_cold_germany_sh_and_hw %>%
  arrange(DATEISO)

electricity_demand_germany_cold_sh_and_hw_plot <-
  ggplot(data = electricity_demand_cold_germany_sh_and_hw, aes(DATEISO, `Electricity demand` / 1000000)) +
  geom_line(lwd = 0.5) + ylab("Electricity demand in GWh") + xlab("Date") + coord_cartesian(ylim = c(0, 55))

electricity_demand_germany_cold_sh_and_hw_plot

sum(electricity_demand_cold_germany_sh_and_hw$`Electricity demand`)


electricity_demand_hot_germany_sh_and_hw <-
  regions_electricity_demand_hot %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_hot_germany_sh_and_hw <-
  electricity_demand_hot_germany_sh_and_hw %>%
  mutate(DATEISO = ISOdate(
    2022,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_hot_germany_sh_and_hw <-
  electricity_demand_hot_germany_sh_and_hw %>%
  arrange(DATEISO)

electricity_demand_germany_hot_sh_and_hw_plot <-
  ggplot(data = electricity_demand_hot_germany_sh_and_hw, aes(DATEISO, `Electricity demand` / 1000000)) +
  geom_line(lwd = 0.5) + ylab("Electricity demand in GWh") + xlab("Date") + coord_cartesian(ylim = c(0, 55))

electricity_demand_germany_hot_sh_and_hw_plot

sum(electricity_demand_hot_germany_sh_and_hw$`Electricity demand`)


# For space heating only
electricity_demand_reference_germany_sh_only <-
  regions_electricity_demand_space_heat_only_reference %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_reference_germany_sh_only <-
  electricity_demand_reference_germany_sh_only %>%
  mutate(DATEISO = ISOdate(
    2017,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_reference_germany_sh_only <-
  electricity_demand_reference_germany_sh_only %>%
  arrange(DATEISO)

electricity_demand_germany_reference_sh_only_plot <-
  ggplot(data = electricity_demand_reference_germany_sh_only, aes(DATEISO, `Electricity demand` / 1000000)) +
  geom_line(lwd = 0.5) + ylab("Electricity demand in GWh") + xlab("Date") + coord_cartesian(ylim = c(0, 55))

electricity_demand_germany_reference_sh_only_plot

sum(electricity_demand_reference_germany_sh_only$`Electricity demand`)


electricity_demand_cold_germany_sh_only <-
  regions_electricity_demand_space_heat_only_cold %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_cold_germany_sh_only <-
  electricity_demand_cold_germany_sh_only %>%
  mutate(DATEISO = ISOdate(
    2010,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_cold_germany_sh_only <-
  electricity_demand_cold_germany_sh_only %>%
  arrange(DATEISO)

electricity_demand_germany_cold_sh_only_plot <-
  ggplot(data = electricity_demand_cold_germany_sh_only, aes(DATEISO, `Electricity demand` / 1000000)) +
  geom_line(lwd = 0.5) + ylab("Electricity demand in GWh") + xlab("Date") + coord_cartesian(ylim = c(0, 55))

electricity_demand_germany_cold_sh_only_plot

sum(electricity_demand_cold_germany_sh_only$`Electricity demand`)


electricity_demand_hot_germany_sh_only <-
  regions_electricity_demand_space_heat_only_hot %>%
  select(-c("nuts3_code")) %>%
  group_by(time) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

electricity_demand_hot_germany_sh_only <-
  electricity_demand_hot_germany_sh_only %>%
  mutate(DATEISO = ISOdate(
    2022,
    ifelse(substr(time, 4, 5) == "00", 0, sub("^0+", "", substr(time, 4, 5))),
    ifelse(substr(time, 1, 2) == "00", 0, sub("^0+", "", substr(time, 1, 2))),
    ifelse(substr(time, 7, 8) == "00", 0, sub("^0+", "", substr(time, 7, 8)))
  )) %>%
  select(-c("time"))

electricity_demand_hot_germany_sh_only <-
  electricity_demand_hot_germany_sh_only %>%
  arrange(DATEISO)

electricity_demand_germany_hot_sh_only_plot <-
  ggplot(data = electricity_demand_hot_germany_sh_only, aes(DATEISO, `Electricity demand` / 1000000)) +
  geom_line(lwd = 0.5) + ylab("Electricity demand in GWh") + xlab("Date") + coord_cartesian(ylim = c(0, 55))

electricity_demand_germany_hot_sh_only_plot

sum(electricity_demand_hot_germany_sh_only$`Electricity demand`)


# Save the plots
ggsave(
  "plots/output/regionselectricitydemand/electricitydemandgermanyovertime/electricity_demand_germany_reference_sh_and_hw_plot.png",
  electricity_demand_germany_reference_sh_and_hw_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandgermanyovertime/electricity_demand_germany_cold_sh_and_hw_plot.png",
  electricity_demand_germany_cold_sh_and_hw_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandgermanyovertime/electricity_demand_germany_hot_sh_and_hw_plot.png",
  electricity_demand_germany_hot_sh_and_hw_plot,
  width = 25,
  units = "cm"
)


ggsave(
  "plots/output/regionselectricitydemand/electricitydemandgermanyovertime/electricity_demand_germany_reference_sh_only_plot.png",
  electricity_demand_germany_reference_sh_only_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandgermanyovertime/electricity_demand_germany_cold_sh_only_plot.png",
  electricity_demand_germany_cold_sh_only_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandgermanyovertime/electricity_demand_germany_hot_sh_only_plot.png",
  electricity_demand_germany_hot_sh_only_plot,
  width = 25,
  units = "cm"
)
