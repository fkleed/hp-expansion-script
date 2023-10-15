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
    NUTS3NameID = gsub(",.*", "", NUTS3Name),
    NUTS3Name = paste(gsub(",.*", "", NUTS3Name), NUTS3Type, sep = ", ")
  ) %>%
  select(c("NUTS3Code", "NUTS3Type", "NUTS3NameID", "NUTS3Name"))


regions_electricity_demand_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_reference_iso.csv"
  )


regions_electricity_demand_space_heat_only_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_reference_iso.csv"
  )


# Join building stock with nuts3 regions that have a urban and rural district
building_stock_2030_with_hp_distribution_aggregated <- building_stock_2030_with_hp_distribution %>%
  mutate("HPSum" = HPAmountAir + HPAmountProbe + HPAmountCollector) %>%
  select(
    "NUTS3Code",
    "BuildingCount",
    "HPSum"
  ) %>%
  group_by(NUTS3Code) %>%
  summarise(
    "BuildingCount" = sum(BuildingCount),
    "HPSum" = sum(HPSum),
    .groups = "drop"
    )

nuts3regioninfo_2_occurrences <-
  nuts3regioninfo %>%
  select("NUTS3NameID") %>%
  group_by(NUTS3NameID) %>%
  add_count(name = "NumberOfOccurrence") %>%
  filter(NumberOfOccurrence > 1) %>%
  select(-c("NumberOfOccurrence")) %>%
  distinct()

nuts3regioninfo_urban_rural_with_hp_amount <- nuts3regioninfo %>%
  inner_join(
    nuts3regioninfo_2_occurrences,
    by = c("NUTS3NameID")
    ) %>%
  left_join(
    building_stock_2030_with_hp_distribution_aggregated,
    by = c("NUTS3Code")
  ) %>%
  mutate_if(is.character, as.factor)


# Calculate the annual electricity demand per NUTS 3

# Space heat and hot water
regions_annual_electricity_demand_per_hp_reference <-
  regions_electricity_demand_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop") %>%
  inner_join(nuts3regioninfo_urban_rural_with_hp_amount, by = c("nuts3_code" = "NUTS3Code")) %>%
  mutate(
    "BuildingCount" = BuildingCount / 1000,
    "Electricity demand per HP" = `Electricity demand` / HPSum
  )

# Space heat only
regions_annual_electricity_demand_per_hp_space_heat_only_reference <-
  regions_electricity_demand_space_heat_only_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = sum(hourly_electricity_demand),
            .groups = "drop") %>%
  inner_join(nuts3regioninfo_urban_rural_with_hp_amount, by = c("nuts3_code" = "NUTS3Code")) %>%
  mutate(
    "BuildingCount" = BuildingCount / 1000,
    "Electricity demand per HP" = `Electricity demand` / HPSum
  )


# Plot the graphs for the annual electricity demand per NUTS 3

# Space heat and hot water
regions_annual_electricity_demand_per_hp_reference_plot <- ggplot(
  regions_annual_electricity_demand_per_hp_reference,
  aes(
    x = BuildingCount,
    y = `Electricity demand per HP`
  )
) +
  geom_point(
    aes(
      shape = NUTS3Type,
      color = NUTS3Type,
      size = NUTS3Type
    )
  ) +
  scale_size_manual(values = c(3, 3)) +
  scale_color_brewer(palette = "Set2") +
  geom_text(
    data = filter(
      regions_annual_electricity_demand_per_hp_reference,
      NUTS3NameID %in% pull(select(filter(regions_annual_electricity_demand_per_hp_reference, `Electricity demand per HP` > 13000 | `Electricity demand per HP` < 7000), c("NUTS3NameID")), NUTS3NameID)
    ),
    aes(BuildingCount, `Electricity demand per HP`, label=NUTS3Name, color=NUTS3Type),
    show.legend = FALSE
  ) +
  labs(x = "Number of buildings in thousands",
       y = "Electricity demand in kWh") +
  theme(legend.title=element_blank())

regions_annual_electricity_demand_per_hp_reference_plot


# Space heat only
regions_annual_electricity_demand_per_hp_space_heat_only_reference_plot <- ggplot(
  regions_annual_electricity_demand_per_hp_space_heat_only_reference,
  aes(
    x = BuildingCount,
    y = `Electricity demand per HP`
  )
) +
  geom_point(
    aes(
      shape = NUTS3Type,
      color = NUTS3Type,
      size = NUTS3Type
    )
  ) +
  scale_size_manual(values = c(3, 3)) +
  scale_color_brewer(palette = "Set2") +
  geom_text(
    data = filter(
      regions_annual_electricity_demand_per_hp_space_heat_only_reference,
      NUTS3NameID %in% pull(select(filter(regions_annual_electricity_demand_per_hp_space_heat_only_reference, `Electricity demand per HP` > 10750 | `Electricity demand per HP` < 6000), c("NUTS3NameID")), NUTS3NameID)
    ),
    aes(BuildingCount, `Electricity demand per HP`, label=NUTS3Name, color=NUTS3Type),
    show.legend = FALSE
  ) +
  labs(x = "Number of buildings in thousands",
       y = "Electricity demand in kWh") +
  theme(legend.title=element_blank())

regions_annual_electricity_demand_per_hp_space_heat_only_reference_plot


# Calculate the maximum hourly electricity demand per NUTS 3

# Space heat and hot water
regions_max_hourly_electricity_demand_per_hp_reference <-
  regions_electricity_demand_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop") %>%
  inner_join(nuts3regioninfo_urban_rural_with_hp_amount, by = c("nuts3_code" = "NUTS3Code")) %>%
  mutate(
    "BuildingCount" = BuildingCount / 1000,
    "Electricity demand per HP" = `Electricity demand` / HPSum
  )

# Space heat only
regions_max_hourly_electricity_demand_per_hp_space_heat_only_reference <-
  regions_electricity_demand_space_heat_only_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("Electricity demand" = max(hourly_electricity_demand),
            .groups = "drop") %>%
  inner_join(nuts3regioninfo_urban_rural_with_hp_amount, by = c("nuts3_code" = "NUTS3Code")) %>%
  mutate(
    "BuildingCount" = BuildingCount / 1000,
    "Electricity demand per HP" = `Electricity demand` / HPSum
  )


# Plot the graphs for the maximum hourly electricity demand per NUTS 3

# Space heat and hot water
regions_max_hourly_electricity_demand_per_hp_reference_plot <- ggplot(
  regions_max_hourly_electricity_demand_per_hp_reference,
  aes(
    x = BuildingCount,
    y = `Electricity demand per HP`
  )
) +
  geom_point(
    aes(
      shape = NUTS3Type,
      color = NUTS3Type,
      size = NUTS3Type
    )
  ) +
  scale_size_manual(values = c(3, 3)) +
  scale_color_brewer(palette = "Set2") +
  geom_text(
    data = filter(
      regions_max_hourly_electricity_demand_per_hp_reference,
      NUTS3NameID %in% pull(select(filter(regions_max_hourly_electricity_demand_per_hp_reference, `Electricity demand per HP` > 11 | `Electricity demand per HP` < 6.3), c("NUTS3NameID")), NUTS3NameID)
    ),
    aes(BuildingCount, `Electricity demand per HP`, label=NUTS3Name, color=NUTS3Type),
    show.legend = FALSE
  ) +
  labs(x = "Number of buildings in thousands",
       y = "Electricity demand in kWh") +
  theme(legend.title=element_blank())

regions_max_hourly_electricity_demand_per_hp_reference_plot


# Space heat only
regions_max_hourly_electricity_demand_per_hp_space_heat_only_reference_plot <- ggplot(
  regions_max_hourly_electricity_demand_per_hp_space_heat_only_reference,
  aes(
    x = BuildingCount,
    y = `Electricity demand per HP`
  )
) +
  geom_point(
    aes(
      shape = NUTS3Type,
      color = NUTS3Type,
      size = NUTS3Type
    )
  ) +
  scale_size_manual(values = c(3, 3)) +
  scale_color_brewer(palette = "Set2") +
  geom_text(
    data = filter(
      regions_max_hourly_electricity_demand_per_hp_space_heat_only_reference,
      NUTS3NameID %in% pull(select(filter(regions_max_hourly_electricity_demand_per_hp_space_heat_only_reference, `Electricity demand per HP` > 10.9 | `Electricity demand per HP` < 6.3), c("NUTS3NameID")), NUTS3NameID)
    ),
    aes(BuildingCount, `Electricity demand per HP`, label=NUTS3Name, color=NUTS3Type),
    show.legend = FALSE
  ) +
  labs(x = "Number of buildings in thousands",
       y = "Electricity demand in kWh") +
  theme(legend.title=element_blank())

regions_max_hourly_electricity_demand_per_hp_space_heat_only_reference_plot


# Save the plots
ggsave(
  "plots/output/regionselectricitydemand/urbanversusruralannualrlectricitydemand/regions_annual_electricity_demand_per_hp_reference_plot.png",
  regions_annual_electricity_demand_per_hp_reference_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/urbanversusruralannualrlectricitydemand/regions_annual_electricity_demand_per_hp_space_heat_only_reference_plot.png",
  regions_annual_electricity_demand_per_hp_space_heat_only_reference_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/urbanversusruralannualrlectricitydemand/regions_max_hourly_electricity_demand_per_hp_reference_plot.png",
  regions_max_hourly_electricity_demand_per_hp_reference_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/urbanversusruralannualrlectricitydemand/regions_max_hourly_electricity_demand_per_hp_space_heat_only_reference_plot.png",
  regions_max_hourly_electricity_demand_per_hp_space_heat_only_reference_plot,
  width = 30,
  units = "cm"
)
