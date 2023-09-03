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


# Get the maximum hourly electricity demand for the NUTS 3 regions

# Space heating and hot water
max_regions_electricity_demand_sh_and_hw_reference <-
  regions_electricity_demand_reference %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("Max hourly electricity demand" = max(hourly_electricity_demand))

max_regions_electricity_demand_sh_and_hw_reference <-
  max_regions_electricity_demand_sh_and_hw_reference %>%
  slice_max(`Max hourly electricity demand`, n = 10)

max_regions_electricity_demand_sh_and_hw_reference <-
  max_regions_electricity_demand_sh_and_hw_reference %>%
  mutate("Year" = 2017)


max_regions_electricity_demand_sh_and_hw_cold <-
  regions_electricity_demand_cold %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("Max hourly electricity demand" = max(hourly_electricity_demand))

max_regions_electricity_demand_sh_and_hw_cold <-
  max_regions_electricity_demand_sh_and_hw_cold %>%
  slice_max(`Max hourly electricity demand`, n = 10)

max_regions_electricity_demand_sh_and_hw_cold <-
  max_regions_electricity_demand_sh_and_hw_cold %>%
  mutate("Year" = 2010)


max_regions_electricity_demand_sh_and_hw_hot <-
  regions_electricity_demand_hot %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("Max hourly electricity demand" = max(hourly_electricity_demand))

max_regions_electricity_demand_sh_and_hw_hot <-
  max_regions_electricity_demand_sh_and_hw_hot %>%
  slice_max(`Max hourly electricity demand`, n = 10)

max_regions_electricity_demand_sh_and_hw_hot <-
  max_regions_electricity_demand_sh_and_hw_hot %>%
  mutate("Year" = 2022)


max_regions_electricity_demand_sh_and_hw <-
  max_regions_electricity_demand_sh_and_hw_reference %>%
  rbind(max_regions_electricity_demand_sh_and_hw_cold) %>%
  rbind(max_regions_electricity_demand_sh_and_hw_hot) %>%
  left_join(
    nuts3regioninfo,
    by = c("nuts3_code" = "NUTS3Code")
  )


# Space heating only
max_regions_electricity_demand_sh_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("Max hourly electricity demand" = max(hourly_electricity_demand))

max_regions_electricity_demand_sh_only_reference <-
  max_regions_electricity_demand_sh_only_reference %>%
  slice_max(`Max hourly electricity demand`, n = 10)

max_regions_electricity_demand_sh_only_reference <-
  max_regions_electricity_demand_sh_only_reference %>%
  mutate("Year" = 2017)


max_regions_electricity_demand_sh_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("Max hourly electricity demand" = max(hourly_electricity_demand))

max_regions_electricity_demand_sh_only_cold <-
  max_regions_electricity_demand_sh_only_cold %>%
  slice_max(`Max hourly electricity demand`, n = 10)

max_regions_electricity_demand_sh_only_cold <-
  max_regions_electricity_demand_sh_only_cold %>%
  mutate("Year" = 2010)


max_regions_electricity_demand_sh_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("Max hourly electricity demand" = max(hourly_electricity_demand))

max_regions_electricity_demand_sh_only_hot <-
  max_regions_electricity_demand_sh_only_hot %>%
  slice_max(`Max hourly electricity demand`, n = 10)

max_regions_electricity_demand_sh_only_hot <-
  max_regions_electricity_demand_sh_only_hot %>%
  mutate("Year" = 2022)


max_regions_electricity_demand_sh_only <-
  max_regions_electricity_demand_sh_only_reference %>%
  rbind(max_regions_electricity_demand_sh_only_cold) %>%
  rbind(max_regions_electricity_demand_sh_only_hot) %>%
  left_join(
    nuts3regioninfo,
    by = c("nuts3_code" = "NUTS3Code")
  )


# Plot the graphs

# For space heating and hot water
max_regions_electricity_demand_sh_and_hw_plot <-
  ggplot(
    data = max_regions_electricity_demand_sh_and_hw,
    aes(
      x = `Max hourly electricity demand`,
      y = reorder(NUTS3Name,-`Max hourly electricity demand`)
    )
  ) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Electricity demand in kWh",
       y = "NUTS 3 Name")

max_regions_electricity_demand_sh_and_hw_plot


# Space heating only
max_regions_electricity_demand_sh_only_plot <-
  ggplot(
    data = max_regions_electricity_demand_sh_only,
    aes(
      x = `Max hourly electricity demand`,
      y = reorder(NUTS3Name,-`Max hourly electricity demand`)
    )
  ) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Electricity demand in kWh",
       y = "NUTS 3 Name")

max_regions_electricity_demand_sh_only_plot


# Save the plots
ggsave(
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandnuts3/max_regions_electricity_demand_sh_and_hw_plot.png",
  max_regions_electricity_demand_sh_and_hw_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandnuts3/max_regions_electricity_demand_sh_only_plot.png",
  max_regions_electricity_demand_sh_only_plot,
  width = 25,
  units = "cm"
)
