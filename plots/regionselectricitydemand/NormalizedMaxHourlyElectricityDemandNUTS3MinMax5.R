# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)

# Read the data
building_stock_2030_with_hp_distribution <-
  read_csv("data/output/heatpumpexpansion/building_stock_2030_with_hp_distribution.csv")

summary(building_stock_2030_with_hp_distribution)

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

# Join building stock with nuts3 region info
building_stock_2030_with_hp_distribution_aggregated <- building_stock_2030_with_hp_distribution %>%
  mutate("HPSum" = HPAmountAir + HPAmountProbe + HPAmountCollector) %>%
  select(
    "NUTS3Code",
    "HPSum"
  ) %>%
  group_by(NUTS3Code) %>%
  summarise("HPSum" = sum(HPSum),
            .groups = "drop")

nuts3regioninfo_with_hp_amount <- nuts3regioninfo %>%
  left_join(building_stock_2030_with_hp_distribution_aggregated, by = c("NUTS3Code"))


# Calculate the 5 NUTS 3 regions with the lowest and highest annual heat pump electricity demand for space heating and hot water per heat pump

# Reference year
regions_electricity_demand_reference <-
  regions_electricity_demand_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop") %>%
  left_join(nuts3regioninfo_with_hp_amount, by = c("nuts3_code" = "NUTS3Code")) %>%
  mutate(
    "Electricity demand per HP" = `Electricity demand` / HPSum
  )


highest_regions_electricity_demand_per_hp_sh_and_hw_reference <-
  regions_electricity_demand_reference %>%
  slice_max(`Electricity demand per HP`, n = 5)

lowest_regions_electricity_demand_per_hp_sh_and_hw_reference <-
  regions_electricity_demand_reference %>%
  slice_min(`Electricity demand per HP`, n = 5)


highest_lowest_regions_electricity_demand_per_hp_sh_and_hw_reference <-
  highest_regions_electricity_demand_per_hp_sh_and_hw_reference %>%
  rbind(lowest_regions_electricity_demand_per_hp_sh_and_hw_reference) %>%
  mutate("Year" = 2017)

remove(highest_regions_electricity_demand_per_hp_sh_and_hw_reference)
remove(lowest_regions_electricity_demand_per_hp_sh_and_hw_reference)


# Cold year
regions_electricity_demand_cold <-
  regions_electricity_demand_cold_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop") %>%
  left_join(nuts3regioninfo_with_hp_amount, by = c("nuts3_code" = "NUTS3Code")) %>%
  mutate(
    "Electricity demand per HP" = `Electricity demand` / HPSum
  )


highest_regions_electricity_demand_per_hp_sh_and_hw_cold <-
  regions_electricity_demand_cold %>%
  slice_max(`Electricity demand per HP`, n = 5)

lowest_regions_electricity_demand_per_hp_sh_and_hw_cold <-
  regions_electricity_demand_cold %>%
  slice_min(`Electricity demand per HP`, n = 5)


highest_lowest_regions_electricity_demand_per_hp_sh_and_hw_cold <-
  highest_regions_electricity_demand_per_hp_sh_and_hw_cold %>%
  rbind(lowest_regions_electricity_demand_per_hp_sh_and_hw_cold) %>%
  mutate("Year" = 2010)

remove(highest_regions_electricity_demand_per_hp_sh_and_hw_cold)
remove(lowest_regions_electricity_demand_per_hp_sh_and_hw_cold)


# Hot year
regions_electricity_demand_hot <-
  regions_electricity_demand_hot_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop") %>%
  left_join(nuts3regioninfo_with_hp_amount, by = c("nuts3_code" = "NUTS3Code")) %>%
  mutate(
    "Electricity demand per HP" = `Electricity demand` / HPSum
  )


highest_regions_electricity_demand_per_hp_sh_and_hw_hot <-
  regions_electricity_demand_hot %>%
  slice_max(`Electricity demand per HP`, n = 5)

lowest_regions_electricity_demand_per_hp_sh_and_hw_hot <-
  regions_electricity_demand_hot %>%
  slice_min(`Electricity demand per HP`, n = 5)


highest_lowest_regions_electricity_demand_per_hp_sh_and_hw_hot <-
  highest_regions_electricity_demand_per_hp_sh_and_hw_hot %>%
  rbind(lowest_regions_electricity_demand_per_hp_sh_and_hw_hot) %>%
  mutate("Year" = 2022)

remove(highest_regions_electricity_demand_per_hp_sh_and_hw_hot)
remove(lowest_regions_electricity_demand_per_hp_sh_and_hw_hot)


# Combine data and plot the graphs
highest_lowest_regions_electricity_demand_per_hp_sh_and_hw <-
  highest_lowest_regions_electricity_demand_per_hp_sh_and_hw_reference %>%
  rbind(highest_lowest_regions_electricity_demand_per_hp_sh_and_hw_cold) %>%
  rbind(highest_lowest_regions_electricity_demand_per_hp_sh_and_hw_hot)

highest_lowest_regions_max_hourly_electricity_demand_per_hp_sh_and_hw_plot <-
  ggplot(
    data = highest_lowest_regions_electricity_demand_per_hp_sh_and_hw,
    aes(
      x = `Electricity demand per HP`,
      y = reorder(NUTS3Name,-`Electricity demand per HP`)
    )
  ) +
  geom_bar(stat = "identity") +
  coord_cartesian(xlim = c(0, 20)) +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Electricity demand in kWh",
       y = "NUTS-3 Name")


highest_lowest_regions_max_hourly_electricity_demand_per_hp_sh_and_hw_plot


# Calculate the 5 NUTS 3 regions with the lowest and highest annual heat pump electricity demand for space heating only per heat pump

# Reference year
regions_electricity_demand_space_heat_only_reference <-
  regions_electricity_demand_space_heat_only_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop") %>%
  left_join(nuts3regioninfo_with_hp_amount, by = c("nuts3_code" = "NUTS3Code")) %>%
  mutate(
    "Electricity demand per HP" = `Electricity demand` / HPSum
  )


highest_regions_electricity_demand_per_hp_sh_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  slice_max(`Electricity demand per HP`, n = 5)

lowest_regions_electricity_demand_per_hp_sh_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  slice_min(`Electricity demand per HP`, n = 5)


highest_lowest_regions_electricity_demand_per_hp_sh_only_reference <-
  highest_regions_electricity_demand_per_hp_sh_only_reference %>%
  rbind(lowest_regions_electricity_demand_per_hp_sh_only_reference) %>%
  mutate("Year" = 2017)

remove(highest_regions_electricity_demand_per_hp_sh_only_reference)
remove(lowest_regions_electricity_demand_per_hp_sh_only_reference)


# Cold year
regions_electricity_demand_space_heat_only_cold <-
  regions_electricity_demand_space_heat_only_cold_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop") %>%
  left_join(nuts3regioninfo_with_hp_amount, by = c("nuts3_code" = "NUTS3Code")) %>%
  mutate(
    "Electricity demand per HP" = `Electricity demand` / HPSum
  )


highest_regions_electricity_demand_per_hp_sh_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  slice_max(`Electricity demand per HP`, n = 5)

lowest_regions_electricity_demand_per_hp_sh_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  slice_min(`Electricity demand per HP`, n = 5)


highest_lowest_regions_electricity_demand_per_hp_sh_only_cold <-
  highest_regions_electricity_demand_per_hp_sh_only_cold %>%
  rbind(lowest_regions_electricity_demand_per_hp_sh_only_cold) %>%
  mutate("Year" = 2010)

remove(highest_regions_electricity_demand_per_hp_sh_only_cold)
remove(lowest_regions_electricity_demand_per_hp_sh_only_cold)


# Hot year
regions_electricity_demand_space_heat_only_hot <-
  regions_electricity_demand_space_heat_only_hot_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop") %>%
  left_join(nuts3regioninfo_with_hp_amount, by = c("nuts3_code" = "NUTS3Code")) %>%
  mutate(
    "Electricity demand per HP" = `Electricity demand` / HPSum
  )


highest_regions_electricity_demand_per_hp_sh_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  slice_max(`Electricity demand per HP`, n = 5)

lowest_regions_electricity_demand_per_hp_sh_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  slice_min(`Electricity demand per HP`, n = 5)


highest_lowest_regions_electricity_demand_per_hp_sh_only_hot <-
  highest_regions_electricity_demand_per_hp_sh_only_hot %>%
  rbind(lowest_regions_electricity_demand_per_hp_sh_only_hot) %>%
  mutate("Year" = 2022)

remove(highest_regions_electricity_demand_per_hp_sh_only_hot)
remove(lowest_regions_electricity_demand_per_hp_sh_only_hot)


# Combine data and plot the graphs
highest_lowest_regions_electricity_demand_per_hp_sh_only <-
  highest_lowest_regions_electricity_demand_per_hp_sh_only_reference %>%
  rbind(highest_lowest_regions_electricity_demand_per_hp_sh_only_cold) %>%
  rbind(highest_lowest_regions_electricity_demand_per_hp_sh_only_hot)

highest_lowest_regions_max_hourly_electricity_demand_per_hp_sh_only_plot <-
  ggplot(
    data = highest_lowest_regions_electricity_demand_per_hp_sh_only,
    aes(
      x = `Electricity demand per HP`,
      y = reorder(NUTS3Name,-`Electricity demand per HP`)
    )
  ) +
  geom_bar(stat = "identity") +
  coord_cartesian(xlim = c(0, 20)) +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Electricity demand in kWh",
       y = "NUTS-3 Name")


highest_lowest_regions_max_hourly_electricity_demand_per_hp_sh_only_plot


# Save the plots
ggsave(
  "plots/output/regionselectricitydemand/normalizedmaxhourlyelectricitydemandnuts3minmax5/highest_lowest_regions_max_hourly_electricity_demand_per_hp_sh_and_hw_plot.png",
  highest_lowest_regions_max_hourly_electricity_demand_per_hp_sh_and_hw_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/normalizedmaxhourlyelectricitydemandnuts3minmax5/highest_lowest_regions_max_hourly_electricity_demand_per_hp_sh_only_plot.png",
  highest_lowest_regions_max_hourly_electricity_demand_per_hp_sh_only_plot,
  width = 30,
  units = "cm"
)
