# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")
library(ggrepel)

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
  mutate(NUTS3Name = gsub(",.*", "", NUTS3Name)) %>%
  select(c("NUTS3Code", "NUTS3Name", "NUTS3Type"))

summarized_building_stock_2030 <-
  read_csv2("data/output/buildingstructure/summarized_building_stock_2030.csv") %>% mutate_if(is.character, as.factor)


# Get the number of NUTS3Names that have a urban and rural part.
nuts3regioninfo_2_occurrences <-
  nuts3regioninfo %>%
  select("NUTS3Name") %>%
  group_by(NUTS3Name) %>%
  add_count(name = "NumberOfOccurrence")

nuts3regioninfo_2_occurrences <-
  nuts3regioninfo_2_occurrences %>%
  filter(NumberOfOccurrence > 1) %>%
  select(-c("NumberOfOccurrence"))

nuts3regioninfo_2_occurrences <-
  distinct(nuts3regioninfo_2_occurrences)

 # nuts3regioninfo_2_occurrences <- data.frame(nuts3regioninfo_2_occurrences) %>% slice_sample(n = 8)

nuts3regioninfo_urban_rural <- nuts3regioninfo %>%
  inner_join(nuts3regioninfo_2_occurrences, by = c("NUTS3Name")) %>%
  mutate_if(is.character, as.factor)

nuts3regioninfo_urban_rural <- nuts3regioninfo_urban_rural %>%
  mutate(
    NUTS3Type = fct_recode(
      NUTS3Type,
      "Urban district" = "Kreisfreie Stadt",
      "Urban district" = "Stadtkreis",
      "Rural district" = "Landkreis"
    )
  )


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


# Join NUTS3Names that have a urban and rural part with the building stock
nuts3regioninfo_urban_rural <-
  nuts3regioninfo_urban_rural %>%
  left_join(summarized_building_stock_2030, by = c("NUTS3Code"))


# Calculate the electricity demand per NUTS 3

# Space heat and hot water
regions_electricity_demand_reference <-
  regions_electricity_demand_reference %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")

regions_electricity_demand_cold <-
  regions_electricity_demand_cold %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")

regions_electricity_demand_hot <-
  regions_electricity_demand_hot %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")


# Space heat only
regions_electricity_demand_space_heat_only_reference <-
  regions_electricity_demand_space_heat_only_reference %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")

regions_electricity_demand_space_heat_only_cold <-
  regions_electricity_demand_space_heat_only_cold %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")

regions_electricity_demand_space_heat_only_hot <-
  regions_electricity_demand_space_heat_only_hot %>%
  select(-c("time")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = sum(hourly_electricity_demand),
            .groups = "drop")


# Join urban and rural NUTS 3 data with electricity demand per NUTS 3

# Space heat and hot water
nuts3regioninfo_urban_rural_sh_and_hw_reference <- nuts3regioninfo_urban_rural %>%
  left_join(regions_electricity_demand_reference, by = c("NUTS3Code" = "nuts3_code"))

nuts3regioninfo_urban_rural_sh_and_hw_cold <- nuts3regioninfo_urban_rural %>%
  left_join(regions_electricity_demand_cold, by = c("NUTS3Code" = "nuts3_code"))

nuts3regioninfo_urban_rural_sh_and_hw_hot <- nuts3regioninfo_urban_rural %>%
  left_join(regions_electricity_demand_hot, by = c("NUTS3Code" = "nuts3_code"))

# Space heat only
nuts3regioninfo_urban_rural_sh_only_reference <- nuts3regioninfo_urban_rural %>%
  left_join(regions_electricity_demand_space_heat_only_reference, by = c("NUTS3Code" = "nuts3_code"))

nuts3regioninfo_urban_rural_sh_only_cold <- nuts3regioninfo_urban_rural %>%
  left_join(regions_electricity_demand_space_heat_only_cold, by = c("NUTS3Code" = "nuts3_code"))

nuts3regioninfo_urban_rural_sh_only_hot <- nuts3regioninfo_urban_rural %>%
  left_join(regions_electricity_demand_space_heat_only_hot, by = c("NUTS3Code" = "nuts3_code"))


# Plot the graphs
ggplot(
  nuts3regioninfo_urban_rural_sh_and_hw_reference,
  aes(
    x = BuildingCount,
    y = ElectricityDemand
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
  scale_color_brewer(palette = "Set1") +
  geom_text(
    data = filter(
      nuts3regioninfo_urban_rural_sh_and_hw_reference,
      NUTS3Name %in% pull(select(filter(nuts3regioninfo_urban_rural_sh_and_hw_reference, NUTS3Type == "Urban district" & BuildingCount > 40000), c("NUTS3Name")), NUTS3Name)
    ),
    aes(BuildingCount, ElectricityDemand, label=NUTS3Name,  hjust = 0.75)
  )



