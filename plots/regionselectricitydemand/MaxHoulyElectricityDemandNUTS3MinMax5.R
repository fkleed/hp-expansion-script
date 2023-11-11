# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)

# Read the data
nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx") %>%
  mutate(
    NUTS3Type = fct_recode(
      NUTS3Type,
      "District" = "Kreis",
      "Urban district" = "Kreisfreie Stadt",
      "Urban district" = "Stadtkreis",
      "Rural district" = "Landkreis",
      "Regional association" = "Regionalverband"
    ),
    NUTS3Name = paste(gsub(",.*", "", NUTS3Name), NUTS3Type, sep = ", ")
  ) %>%
  select(c("NUTS3Code", "NUTS3Name"))


regions_electricity_demand_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_reference_iso.csv"
  )

regions_electricity_demand_cold_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_cold_iso.csv"
  )

regions_electricity_demand_hot_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_hot_iso.csv"
  )


regions_electricity_demand_space_heat_only_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_reference_iso.csv"
  )

regions_electricity_demand_space_heat_only_cold_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_cold_iso.csv"
  )

regions_electricity_demand_space_heat_only_hot_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_hot_iso.csv"
  )

# Calculate the 5 NUTS 3 regions with the lowest and the 5 NUTS 3 regions with the highest max hourly heat pump electricity demand for space heating and hot water

# Reference year
regions_electricity_demand_reference <-
  regions_electricity_demand_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop")

highest_regions_electricity_demand_sh_and_hw_reference <-
  regions_electricity_demand_reference %>%
  slice_max(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

lowest_regions_electricity_demand_sh_and_hw_reference <-
  regions_electricity_demand_reference %>%
  slice_min(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

highest_lowest_regions_electricity_demand_sh_and_hw_reference <-
  highest_regions_electricity_demand_sh_and_hw_reference %>%
  rbind(lowest_regions_electricity_demand_sh_and_hw_reference)

# Cold year
regions_electricity_demand_cold <-
  regions_electricity_demand_cold_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop")

highest_regions_electricity_demand_sh_and_hw_cold <-
  regions_electricity_demand_cold %>%
  slice_max(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

lowest_regions_electricity_demand_sh_and_hw_cold <-
  regions_electricity_demand_cold %>%
  slice_min(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

highest_lowest_regions_electricity_demand_sh_and_hw_cold <-
  highest_regions_electricity_demand_sh_and_hw_cold %>%
  rbind(lowest_regions_electricity_demand_sh_and_hw_cold)

# Hot year
regions_electricity_demand_hot <-
  regions_electricity_demand_hot_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop")

highest_regions_electricity_demand_sh_and_hw_hot <-
  regions_electricity_demand_hot %>%
  slice_max(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

lowest_regions_electricity_demand_sh_and_hw_hot <-
  regions_electricity_demand_hot %>%
  slice_min(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

highest_lowest_regions_electricity_demand_sh_and_hw_hot <-
  highest_regions_electricity_demand_sh_and_hw_hot %>%
  rbind(lowest_regions_electricity_demand_sh_and_hw_hot)


# Calculate the 5 NUTS 3 regions with the lowest and the 5 NUTS 3 regions with the highest max hourly heat pump electricity demand for space heating only

# Reference year
regions_electricity_demand_space_heat_only_reference <-
  regions_electricity_demand_space_heat_only_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop")

highest_regions_electricity_demand_sh_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  slice_max(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

lowest_regions_electricity_demand_sh_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  slice_min(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

highest_lowest_regions_electricity_demand_sh_only_reference <-
  highest_regions_electricity_demand_sh_only_reference %>%
  rbind(lowest_regions_electricity_demand_sh_only_reference)

# Cold year
regions_electricity_demand_space_heat_only_cold <-
  regions_electricity_demand_space_heat_only_cold_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop")

highest_regions_electricity_demand_sh_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  slice_max(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

lowest_regions_electricity_demand_sh_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  slice_min(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

highest_lowest_regions_electricity_demand_sh_only_cold <-
  highest_regions_electricity_demand_sh_only_cold %>%
  rbind(lowest_regions_electricity_demand_sh_only_cold)

# Hot year
regions_electricity_demand_space_heat_only_hot <-
  regions_electricity_demand_space_heat_only_hot_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop")

highest_regions_electricity_demand_sh_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  slice_max(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

lowest_regions_electricity_demand_sh_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  slice_min(`Electricity demand`, n = 5) %>%
  left_join(nuts3regioninfo, by = c("nuts3_code" = "NUTS3Code"))

highest_lowest_regions_electricity_demand_sh_only_hot <-
  highest_regions_electricity_demand_sh_only_hot %>%
  rbind(lowest_regions_electricity_demand_sh_only_hot)


# Combine data and plot the graphs

# Space heating and hot water
highest_lowest_regions_electricity_demand_sh_and_hw_reference <-
  highest_lowest_regions_electricity_demand_sh_and_hw_reference %>%
  mutate("Year" = 2017)

highest_lowest_regions_electricity_demand_sh_and_hw_cold <-
  highest_lowest_regions_electricity_demand_sh_and_hw_cold %>%
  mutate("Year" = 2010)

highest_lowest_regions_electricity_demand_sh_and_hw_hot <-
  highest_lowest_regions_electricity_demand_sh_and_hw_hot %>%
  mutate("Year" = 2022)

highest_lowest_regions_electricity_demand_sh_and_hw <-
  highest_lowest_regions_electricity_demand_sh_and_hw_reference %>%
  rbind(highest_lowest_regions_electricity_demand_sh_and_hw_cold) %>%
  rbind(highest_lowest_regions_electricity_demand_sh_and_hw_hot)

highest_lowest_regions_max_hourly_electricity_demand_sh_and_hw_plot <-
  ggplot(
    data = highest_lowest_regions_electricity_demand_sh_and_hw,
    aes(
      x = `Electricity demand` / 1000,
      y = reorder(NUTS3Name,-`Electricity demand`)
    )
  ) +
  geom_bar(stat = "identity") +
  coord_cartesian(xlim = c(0, 1000)) +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Electricity demand in MWh",
       y = "NUTS-3 Name")


highest_lowest_regions_max_hourly_electricity_demand_sh_and_hw_plot


# Space heating only
highest_lowest_regions_electricity_demand_sh_only_reference <-
  highest_lowest_regions_electricity_demand_sh_only_reference %>%
  mutate("Year" = 2017)

highest_lowest_regions_electricity_demand_sh_only_cold <-
  highest_lowest_regions_electricity_demand_sh_only_cold %>%
  mutate("Year" = 2010)

highest_lowest_regions_electricity_demand_sh_only_hot <-
  highest_lowest_regions_electricity_demand_sh_only_hot %>%
  mutate("Year" = 2022)

highest_lowest_regions_electricity_demand_sh_only <-
  highest_lowest_regions_electricity_demand_sh_only_reference %>%
  rbind(highest_lowest_regions_electricity_demand_sh_only_cold) %>%
  rbind(highest_lowest_regions_electricity_demand_sh_only_hot)

highest_lowest_regions_max_hourly_electricity_demand_sh_only_plot <-
  ggplot(
    data = highest_lowest_regions_electricity_demand_sh_only,
    aes(
      x = `Electricity demand` / 1000,
      y = reorder(NUTS3Name,-`Electricity demand`)
    )
  ) +
  geom_bar(stat = "identity") +
  coord_cartesian(xlim = c(0, 1000)) +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Electricity demand in MWh",
       y = "NUTS-3 Name")


highest_lowest_regions_max_hourly_electricity_demand_sh_only_plot


# Save the plots
ggsave(
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandnuts3highestlowest10/highest_lowest_regions_max_hourly_electricity_demand_sh_and_hw_plot.png",
  highest_lowest_regions_max_hourly_electricity_demand_sh_and_hw_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandnuts3highestlowest10/highest_lowest_regions_max_hourly_electricity_demand_sh_only_plot.png",
  highest_lowest_regions_max_hourly_electricity_demand_sh_only_plot,
  width = 30,
  units = "cm"
)
