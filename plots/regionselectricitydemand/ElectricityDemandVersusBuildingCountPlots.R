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

nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx") %>%
  mutate(NUTS3Name = paste(gsub(",.*", "", NUTS3Name), NUTS3Type, sep = ", ")) %>%
  select(c("NUTS3Code", "NUTS3Name"))

summarized_building_stock_2030 <-
  read_csv2("data/output/buildingstructure/summarized_building_stock_2030.csv") %>% mutate_if(is.character, as.factor)


# Get the amount of buildings per NUTS 3
summarized_building_stock_2030 <-
  summarized_building_stock_2030 %>%
  select(c("NUTS3Code",
           "BuildingCount"))

summarized_building_stock_2030 <-
  summarized_building_stock_2030 %>%
  group_by(NUTS3Code) %>%
  summarise("BuildingCount" = sum(BuildingCount),
            .groups = "drop")


# Calculate the electricity demand per NUTS 3

# Space heat and hot water
regions_electricity_demand_reference <-
  regions_electricity_demand_reference %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")

building_count_versus_hp_electricity_demand_sh_and_hw_reference <-
  regions_electricity_demand_reference %>%
  left_join(summarized_building_stock_2030,
            by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_and_hw_reference <-
  building_count_versus_hp_electricity_demand_sh_and_hw_reference %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_and_hw_reference <-
  building_count_versus_hp_electricity_demand_sh_and_hw_reference %>%
  mutate("Year" = 2017)


regions_electricity_demand_cold <-
  regions_electricity_demand_cold %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")

building_count_versus_hp_electricity_demand_sh_and_hw_cold <-
  regions_electricity_demand_cold %>%
  left_join(summarized_building_stock_2030,
            by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_and_hw_cold <-
  building_count_versus_hp_electricity_demand_sh_and_hw_cold %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_and_hw_cold <-
  building_count_versus_hp_electricity_demand_sh_and_hw_cold %>%
  mutate("Year" = 2010)


regions_electricity_demand_hot <-
  regions_electricity_demand_hot %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")

building_count_versus_hp_electricity_demand_sh_and_hw_hot <-
  regions_electricity_demand_hot %>%
  left_join(summarized_building_stock_2030,
            by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_and_hw_hot <-
  building_count_versus_hp_electricity_demand_sh_and_hw_hot %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_and_hw_hot <-
  building_count_versus_hp_electricity_demand_sh_and_hw_hot %>%
  mutate("Year" = 2022)


building_count_versus_hp_electricity_demand_sh_and_hw <-
  building_count_versus_hp_electricity_demand_sh_and_hw_reference %>%
  rbind(building_count_versus_hp_electricity_demand_sh_and_hw_cold) %>%
  rbind(building_count_versus_hp_electricity_demand_sh_and_hw_hot)


# Space heat only
regions_electricity_demand_space_heat_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")

building_count_versus_hp_electricity_demand_sh_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  left_join(summarized_building_stock_2030,
            by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_only_reference <-
  building_count_versus_hp_electricity_demand_sh_only_reference %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_only_reference <-
  building_count_versus_hp_electricity_demand_sh_only_reference %>%
  mutate("Year" = 2017)


regions_electricity_demand_space_heat_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")

building_count_versus_hp_electricity_demand_sh_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  left_join(summarized_building_stock_2030,
            by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_only_cold <-
  building_count_versus_hp_electricity_demand_sh_only_cold %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_only_cold <-
  building_count_versus_hp_electricity_demand_sh_only_cold %>%
  mutate("Year" = 2010)


regions_electricity_demand_space_heat_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")

building_count_versus_hp_electricity_demand_sh_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  left_join(summarized_building_stock_2030,
            by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_only_hot <-
  building_count_versus_hp_electricity_demand_sh_only_hot %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

building_count_versus_hp_electricity_demand_sh_only_hot <-
  building_count_versus_hp_electricity_demand_sh_only_hot %>%
  mutate("Year" = 2022)


building_count_versus_hp_electricity_demand_sh_only <-
  building_count_versus_hp_electricity_demand_sh_only_reference %>%
  rbind(building_count_versus_hp_electricity_demand_sh_only_cold) %>%
  rbind(building_count_versus_hp_electricity_demand_sh_only_hot)


# Plot the graphs

# Space heat and hot water
building_count_versus_hp_electricity_demand_sh_and_hw_plot <- ggplot(
  building_count_versus_hp_electricity_demand_sh_and_hw,
  aes(x = BuildingCount / 1000, y = ElectricityDemand / 1000000)
) +
  geom_point() +
  geom_smooth(method = lm, level = 0.99) +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Number of buildings in thousands",
       y = "Electricity demand in GWh")

building_count_versus_hp_electricity_demand_sh_and_hw_plot


# Space heat only
building_count_versus_hp_electricity_demand_sh_only_plot <- ggplot(
  building_count_versus_hp_electricity_demand_sh_only,
  aes(x = BuildingCount / 1000, y = ElectricityDemand / 1000000)
) +
  geom_point() +
  geom_smooth(method = lm, level = 0.99) +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Number of buildings in thousands",
       y = "Electricity demand in GWh")

building_count_versus_hp_electricity_demand_sh_only_plot


# Save the plots
ggsave(
  "plots/output/regionselectricitydemand/electricitydemandversusbuildingcount/building_count_versus_hp_electricity_demand_sh_and_hw_plot.png",
  building_count_versus_hp_electricity_demand_sh_and_hw_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandversusbuildingcount/building_count_versus_hp_electricity_demand_sh_only_plot.png",
  building_count_versus_hp_electricity_demand_sh_only_plot,
  width = 25,
  units = "cm"
)
