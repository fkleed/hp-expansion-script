# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)

# Read the data
nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx") %>%
  select(c("NUTS1Name",
           "NUTS3Code"))

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


# Calculate the electricity demand of the federal states for space heating and hot water

# Reference year
federal_states_electricity_demand_sh_and_hw_reference <-
  regions_electricity_demand_reference %>%
  select(-c("time"))

federal_states_electricity_demand_sh_and_hw_reference <-
  federal_states_electricity_demand_sh_and_hw_reference %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

federal_states_electricity_demand_sh_and_hw_reference <-
  federal_states_electricity_demand_sh_and_hw_reference %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code"))

federal_states_electricity_demand_sh_and_hw_reference <-
  federal_states_electricity_demand_sh_and_hw_reference %>%
  group_by(NUTS1Name) %>%
  summarise("Electricity demand" = sum(`Electricity demand`),
            .groups = "drop")

# Cold year
federal_states_electricity_demand_sh_and_hw_cold <-
  regions_electricity_demand_cold %>%
  select(-c("time"))

federal_states_electricity_demand_sh_and_hw_cold <-
  federal_states_electricity_demand_sh_and_hw_cold %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

federal_states_electricity_demand_sh_and_hw_cold <-
  federal_states_electricity_demand_sh_and_hw_cold %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code"))

federal_states_electricity_demand_sh_and_hw_cold <-
  federal_states_electricity_demand_sh_and_hw_cold %>%
  group_by(NUTS1Name) %>%
  summarise("Electricity demand" = sum(`Electricity demand`),
            .groups = "drop")

# Hot year
federal_states_electricity_demand_sh_and_hw_hot <-
  regions_electricity_demand_hot %>%
  select(-c("time"))

federal_states_electricity_demand_sh_and_hw_hot <-
  federal_states_electricity_demand_sh_and_hw_hot %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

federal_states_electricity_demand_sh_and_hw_hot <-
  federal_states_electricity_demand_sh_and_hw_hot %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code"))

federal_states_electricity_demand_sh_and_hw_hot <-
  federal_states_electricity_demand_sh_and_hw_hot %>%
  group_by(NUTS1Name) %>%
  summarise("Electricity demand" = sum(`Electricity demand`),
            .groups = "drop")


# Calculate the electricity demand of the federal states for space heating only

# Reference year
federal_states_electricity_demand_sh_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  select(-c("time"))

federal_states_electricity_demand_sh_only_reference <-
  federal_states_electricity_demand_sh_only_reference %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

federal_states_electricity_demand_sh_only_reference <-
  federal_states_electricity_demand_sh_only_reference %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code"))

federal_states_electricity_demand_sh_only_reference <-
  federal_states_electricity_demand_sh_only_reference %>%
  group_by(NUTS1Name) %>%
  summarise("Electricity demand" = sum(`Electricity demand`),
            .groups = "drop")

# Cold year
federal_states_electricity_demand_sh_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  select(-c("time"))

federal_states_electricity_demand_sh_only_cold <-
  federal_states_electricity_demand_sh_only_cold %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

federal_states_electricity_demand_sh_only_cold <-
  federal_states_electricity_demand_sh_only_cold %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code"))

federal_states_electricity_demand_sh_only_cold <-
  federal_states_electricity_demand_sh_only_cold %>%
  group_by(NUTS1Name) %>%
  summarise("Electricity demand" = sum(`Electricity demand`),
            .groups = "drop")

# Hot year
federal_states_electricity_demand_sh_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  select(-c("time"))

federal_states_electricity_demand_sh_only_hot <-
  federal_states_electricity_demand_sh_only_hot %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

federal_states_electricity_demand_sh_only_hot <-
  federal_states_electricity_demand_sh_only_hot %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code")) %>%
  select(-c("nuts3_code"))

federal_states_electricity_demand_sh_only_hot <-
  federal_states_electricity_demand_sh_only_hot %>%
  group_by(NUTS1Name) %>%
  summarise("Electricity demand" = sum(`Electricity demand`),
            .groups = "drop")


# Check the annual electricity  demand
sum(federal_states_electricity_demand_sh_and_hw_reference$`Electricity demand`)
sum(federal_states_electricity_demand_sh_and_hw_cold$`Electricity demand`)
sum(federal_states_electricity_demand_sh_and_hw_hot$`Electricity demand`)

sum(federal_states_electricity_demand_sh_only_reference$`Electricity demand`)
sum(federal_states_electricity_demand_sh_only_cold$`Electricity demand`)
sum(federal_states_electricity_demand_sh_only_hot$`Electricity demand`)


# Plot the bar charts for the electricity consumption per federal state

# Space heating and hot water
federal_states_electricity_demand_sh_and_hw_reference <-
  federal_states_electricity_demand_sh_and_hw_reference %>%
  mutate("Year" = 2017)

federal_states_electricity_demand_sh_and_hw_cold <-
  federal_states_electricity_demand_sh_and_hw_cold %>%
  mutate("Year" = 2010)

federal_states_electricity_demand_sh_and_hw_hot <-
  federal_states_electricity_demand_sh_and_hw_hot %>%
  mutate("Year" = 2022)

federal_states_electricity_demand_sh_and_hw <-
  federal_states_electricity_demand_sh_and_hw_reference %>%
  rbind(federal_states_electricity_demand_sh_and_hw_cold) %>%
  rbind(federal_states_electricity_demand_sh_and_hw_hot)

federal_states_electricity_demand_sh_and_hw_plot <-
  ggplot(data = federal_states_electricity_demand_sh_and_hw,
         aes(
           x = `Electricity demand` / 1000000000,
           y = reorder(NUTS1Name,-`Electricity demand`)
         )) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ Year, ncol = 1) +
  labs(x = "Electricity demand in TWh",
       y = "Federal state")

federal_states_electricity_demand_sh_and_hw_plot

# Space heating only
federal_states_electricity_demand_sh_only_reference <-
  federal_states_electricity_demand_sh_only_reference %>%
  mutate("Year" = 2017)

federal_states_electricity_demand_sh_only_cold <-
  federal_states_electricity_demand_sh_only_cold %>%
  mutate("Year" = 2010)

federal_states_electricity_demand_sh_only_hot <-
  federal_states_electricity_demand_sh_only_hot %>%
  mutate("Year" = 2022)

federal_states_electricity_demand_sh_only <-
  federal_states_electricity_demand_sh_only_reference %>%
  rbind(federal_states_electricity_demand_sh_only_cold) %>%
  rbind(federal_states_electricity_demand_sh_only_hot)

federal_states_electricity_demand_sh_only_plot <-
  ggplot(data = federal_states_electricity_demand_sh_only,
         aes(
           x = `Electricity demand` / 1000000000,
           y = reorder(NUTS1Name,-`Electricity demand`)
         )) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ Year, ncol = 1) +
  labs(x = "Electricity demand in TWh",
       y = "Federal state")

federal_states_electricity_demand_sh_only_plot


# Save the plots
ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/federal_states_electricity_demand_sh_and_hw_plot.png",
  federal_states_electricity_demand_sh_and_hw_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/federal_states_electricity_demand_sh_only_plot.png",
  federal_states_electricity_demand_sh_only_plot,
  width = 25,
  units = "cm"
)
