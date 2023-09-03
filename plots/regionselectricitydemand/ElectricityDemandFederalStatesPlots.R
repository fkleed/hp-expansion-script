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
bar_chart_federal_states_electricity_demand_sh_and_hw_reference <-
  ggplot(data = federal_states_electricity_demand_sh_and_hw_reference) +
  geom_bar(
    mapping = aes(
      x = reorder(NUTS1Name, -`Electricity demand`),
      y = `Electricity demand` / 1000000000
    ),
    stat = "identity"
  ) +
  labs(x = "Federal state",
       y = "Electricity demand in TWh") +
  ylim(0, 14) +
  coord_flip() +
  theme(legend.position = "bottom")

bar_chart_federal_states_electricity_demand_sh_and_hw_reference


bar_chart_federal_states_electricity_demand_sh_and_hw_cold <-
  ggplot(data = federal_states_electricity_demand_sh_and_hw_cold) +
  geom_bar(
    mapping = aes(
      x = reorder(NUTS1Name, -`Electricity demand`),
      y = `Electricity demand` / 1000000000
    ),
    stat = "identity"
  ) +
  labs(x = "Federal state",
       y = "Electricity demand in TWh") +
  ylim(0, 14) +
  coord_flip() +
  theme(legend.position = "bottom")

bar_chart_federal_states_electricity_demand_sh_and_hw_cold


bar_chart_federal_states_electricity_demand_sh_and_hw_hot <-
  ggplot(data = federal_states_electricity_demand_sh_and_hw_hot) +
  geom_bar(
    mapping = aes(
      x = reorder(NUTS1Name, -`Electricity demand`),
      y = `Electricity demand` / 1000000000
    ),
    stat = "identity"
  ) +
  labs(x = "Federal state",
       y = "Electricity demand in TWh") +
  ylim(0, 14) +
  coord_flip() +
  theme(legend.position = "bottom")

bar_chart_federal_states_electricity_demand_sh_and_hw_hot


# Space heating only
bar_chart_federal_states_electricity_demand_sh_only_reference <-
  ggplot(data = federal_states_electricity_demand_sh_only_reference) +
  geom_bar(
    mapping = aes(
      x = reorder(NUTS1Name, -`Electricity demand`),
      y = `Electricity demand` / 1000000000
    ),
    stat = "identity"
  ) +
  labs(x = "Federal state",
       y = "Electricity demand in TWh") +
  ylim(0, 12) +
  coord_flip() +
  theme(legend.position = "bottom")

bar_chart_federal_states_electricity_demand_sh_only_reference


bar_chart_federal_states_electricity_demand_sh_only_cold <-
  ggplot(data = federal_states_electricity_demand_sh_only_cold) +
  geom_bar(
    mapping = aes(
      x = reorder(NUTS1Name, -`Electricity demand`),
      y = `Electricity demand` / 1000000000
    ),
    stat = "identity"
  ) +
  labs(x = "Federal state",
       y = "Electricity demand in TWh") +
  ylim(0, 12) +
  coord_flip() +
  theme(legend.position = "bottom")

bar_chart_federal_states_electricity_demand_sh_only_cold


bar_chart_federal_states_electricity_demand_sh_only_hot <-
  ggplot(data = federal_states_electricity_demand_sh_only_hot) +
  geom_bar(
    mapping = aes(
      x = reorder(NUTS1Name, -`Electricity demand`),
      y = `Electricity demand` / 1000000000
    ),
    stat = "identity"
  ) +
  labs(x = "Federal state",
       y = "Electricity demand in TWh") +
  ylim(0, 12) +
  coord_flip() +
  theme(legend.position = "bottom")

bar_chart_federal_states_electricity_demand_sh_only_hot


# Save the plots

# Space heating and hot water
ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/bar_chart_federal_states_electricity_demand_sh_and_hw_reference.png",
  bar_chart_federal_states_electricity_demand_sh_and_hw_reference,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/bar_chart_federal_states_electricity_demand_sh_and_hw_cold.png",
  bar_chart_federal_states_electricity_demand_sh_and_hw_cold,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/bar_chart_federal_states_electricity_demand_sh_and_hw_hot.png",
  bar_chart_federal_states_electricity_demand_sh_and_hw_hot,
  width = 25,
  units = "cm"
)


# Space heating only
ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/bar_chart_federal_states_electricity_demand_sh_only_reference.png",
  bar_chart_federal_states_electricity_demand_sh_only_reference,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/bar_chart_federal_states_electricity_demand_sh_only_cold.png",
  bar_chart_federal_states_electricity_demand_sh_only_cold,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/bar_chart_federal_states_electricity_demand_sh_only_hot.png",
  bar_chart_federal_states_electricity_demand_sh_only_hot,
  width = 25,
  units = "cm"
)

