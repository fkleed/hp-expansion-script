# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)

# Read the data
nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx") %>%
  mutate(NUTS3Name = paste(gsub(",.*", "", NUTS3Name), NUTS3Type, sep = ", ")) %>%
  select(c("NUTS3Code", "NUTS3Name"))

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


# Calculate the five NUTS 3 regions with the lowest and highest heat pump electricity demand for space heating and hot water

# Reference year
regions_electricity_demand_reference <-
  regions_electricity_demand_reference %>%
  select(-c("time"))

regions_electricity_demand_reference <-
  regions_electricity_demand_reference %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

lowest_regions_electricity_demand_sh_and_hw_reference <-
  regions_electricity_demand_reference %>%
  slice_min(`Electricity demand`, n = 5)

highest_regions_electricity_demand_sh_and_hw_reference <-
  regions_electricity_demand_reference %>%
  slice_max(`Electricity demand`, n = 5)

lowest_highest_regions_electricity_demand_sh_and_hw_reference <-
  lowest_regions_electricity_demand_sh_and_hw_reference %>%
  rbind(highest_regions_electricity_demand_sh_and_hw_reference)

remove(lowest_regions_electricity_demand_sh_and_hw_reference)
remove(highest_regions_electricity_demand_sh_and_hw_reference)

lowest_highest_regions_electricity_demand_sh_and_hw_reference <-
  lowest_highest_regions_electricity_demand_sh_and_hw_reference %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

# Cold year
regions_electricity_demand_cold <-
  regions_electricity_demand_cold %>%
  select(-c("time"))

regions_electricity_demand_cold <-
  regions_electricity_demand_cold %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

lowest_regions_electricity_demand_sh_and_hw_cold <-
  regions_electricity_demand_cold %>%
  slice_min(`Electricity demand`, n = 5)

highest_regions_electricity_demand_sh_and_hw_cold <-
  regions_electricity_demand_cold %>%
  slice_max(`Electricity demand`, n = 5)

lowest_highest_regions_electricity_demand_sh_and_hw_cold <-
  lowest_regions_electricity_demand_sh_and_hw_cold %>%
  rbind(highest_regions_electricity_demand_sh_and_hw_cold)

remove(lowest_regions_electricity_demand_sh_and_hw_cold)
remove(highest_regions_electricity_demand_sh_and_hw_cold)

lowest_highest_regions_electricity_demand_sh_and_hw_cold <-
  lowest_highest_regions_electricity_demand_sh_and_hw_cold %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

# Hot year
regions_electricity_demand_hot <-
  regions_electricity_demand_hot %>%
  select(-c("time"))

regions_electricity_demand_hot <-
  regions_electricity_demand_hot %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

lowest_regions_electricity_demand_sh_and_hw_hot <-
  regions_electricity_demand_hot %>%
  slice_min(`Electricity demand`, n = 5)

highest_regions_electricity_demand_sh_and_hw_hot <-
  regions_electricity_demand_hot %>%
  slice_max(`Electricity demand`, n = 5)

lowest_highest_regions_electricity_demand_sh_and_hw_hot <-
  lowest_regions_electricity_demand_sh_and_hw_hot %>%
  rbind(highest_regions_electricity_demand_sh_and_hw_hot)

remove(lowest_regions_electricity_demand_sh_and_hw_hot)
remove(highest_regions_electricity_demand_sh_and_hw_hot)

lowest_highest_regions_electricity_demand_sh_and_hw_hot <-
  lowest_highest_regions_electricity_demand_sh_and_hw_hot %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))


# Calculate the five NUTS 3 regions with the lowest and highest heat pump electricity demand for space heating only

# Reference year
regions_electricity_demand_space_heat_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  select(-c("time"))

regions_electricity_demand_space_heat_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

lowest_regions_electricity_demand_sh_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  slice_min(`Electricity demand`, n = 5)

highest_regions_electricity_demand_sh_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  slice_max(`Electricity demand`, n = 5)

lowest_highest_regions_electricity_demand_sh_only_reference <-
  lowest_regions_electricity_demand_sh_only_reference %>%
  rbind(highest_regions_electricity_demand_sh_only_reference)

remove(lowest_regions_electricity_demand_sh_only_reference)
remove(highest_regions_electricity_demand_sh_only_reference)

lowest_highest_regions_electricity_demand_sh_only_reference <-
  lowest_highest_regions_electricity_demand_sh_only_reference %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

# Cold year
regions_electricity_demand_space_heat_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  select(-c("time"))

regions_electricity_demand_space_heat_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

lowest_regions_electricity_demand_sh_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  slice_min(`Electricity demand`, n = 5)

highest_regions_electricity_demand_sh_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  slice_max(`Electricity demand`, n = 5)

lowest_highest_regions_electricity_demand_sh_only_cold <-
  lowest_regions_electricity_demand_sh_only_cold %>%
  rbind(highest_regions_electricity_demand_sh_only_cold)

remove(lowest_regions_electricity_demand_sh_only_cold)
remove(highest_regions_electricity_demand_sh_only_cold)

lowest_highest_regions_electricity_demand_sh_only_cold <-
  lowest_highest_regions_electricity_demand_sh_only_cold %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

# Hot year
regions_electricity_demand_space_heat_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  select(-c("time"))

regions_electricity_demand_space_heat_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop")

lowest_regions_electricity_demand_sh_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  slice_min(`Electricity demand`, n = 5)

highest_regions_electricity_demand_sh_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  slice_max(`Electricity demand`, n = 5)

lowest_highest_regions_electricity_demand_sh_only_hot <-
  lowest_regions_electricity_demand_sh_only_hot %>%
  rbind(highest_regions_electricity_demand_sh_only_hot)

remove(lowest_regions_electricity_demand_sh_only_hot)
remove(highest_regions_electricity_demand_sh_only_hot)

lowest_highest_regions_electricity_demand_sh_only_hot <-
  lowest_highest_regions_electricity_demand_sh_only_hot %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))


# Combine data and plot the graphs

# Space heating and hot water
lowest_highest_regions_electricity_demand_sh_and_hw_reference <-
  lowest_highest_regions_electricity_demand_sh_and_hw_reference %>%
  mutate("Year" = 2017)

lowest_highest_regions_electricity_demand_sh_and_hw_cold <-
  lowest_highest_regions_electricity_demand_sh_and_hw_cold %>%
  mutate("Year" = 2010)

lowest_highest_regions_electricity_demand_sh_and_hw_hot <-
  lowest_highest_regions_electricity_demand_sh_and_hw_hot %>%
  mutate("Year" = 2022)

lowest_highest_regions_electricity_demand_sh_and_hw <-
  lowest_highest_regions_electricity_demand_sh_and_hw_reference %>%
  rbind(lowest_highest_regions_electricity_demand_sh_and_hw_cold) %>%
  rbind(lowest_highest_regions_electricity_demand_sh_and_hw_hot)

lowest_highest_regions_electricity_demand_sh_and_hw_plot <-
  ggplot(
    data = lowest_highest_regions_electricity_demand_sh_and_hw,
    aes(
      x = `Electricity demand` / 1000000,
      y = reorder(NUTS3Name,-`Electricity demand`)
    )
  ) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Electricity demand in GWh",
       y = "NUTS 3 Name")


lowest_highest_regions_electricity_demand_sh_and_hw_plot


# Space heating only
lowest_highest_regions_electricity_demand_sh_only_reference <-
  lowest_highest_regions_electricity_demand_sh_only_reference %>%
  mutate("Year" = 2017)

lowest_highest_regions_electricity_demand_sh_only_cold <-
  lowest_highest_regions_electricity_demand_sh_only_cold %>%
  mutate("Year" = 2010)

lowest_highest_regions_electricity_demand_sh_only_hot <-
  lowest_highest_regions_electricity_demand_sh_only_hot %>%
  mutate("Year" = 2022)

lowest_highest_regions_electricity_demand_sh_only <-
  lowest_highest_regions_electricity_demand_sh_only_reference %>%
  rbind(lowest_highest_regions_electricity_demand_sh_only_cold) %>%
  rbind(lowest_highest_regions_electricity_demand_sh_only_hot)

lowest_highest_regions_electricity_demand_sh_only_plot <-
  ggplot(
    data = lowest_highest_regions_electricity_demand_sh_only,
    aes(
      x = `Electricity demand` / 1000000,
      y = reorder(NUTS3Name,-`Electricity demand`)
    )
  ) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Electricity demand in GWh",
       y = "NUTS 3 Name")


lowest_highest_regions_electricity_demand_sh_only_plot


# Save the plots
ggsave(
  "plots/output/regionselectricitydemand/electricitydemandnuts3lowesthighest/lowest_highest_regions_electricity_demand_sh_and_hw_plot.png",
  lowest_highest_regions_electricity_demand_sh_and_hw_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandnuts3lowesthighest/lowest_highest_regions_electricity_demand_sh_only_plot.png",
  lowest_highest_regions_electricity_demand_sh_only_plot,
  width = 25,
  units = "cm"
)
