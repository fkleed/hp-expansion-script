# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read data
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
  select(c("NUTS3Code", "NUTS3Name", "NUTS3Type"))

regions_electricity_demand_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_reference_iso.csv"
  )

regions_electricity_demand_space_heat_only_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_reference_iso.csv"
  )


# Check why normalized hp electricity demand differs between urban and rural regions
building_stock_district_types <-
  building_stock_2030_with_hp_distribution %>%
  select(c(
    "NUTS3Code",
    "BuildingTypeSize",
    "YearOfConstruction",
    "BuildingCount"
  )) %>%
  left_join(nuts3regioninfo,
            by = c("NUTS3Code")) %>%
  select(-c("NUTS3Code", "NUTS3Name")) %>%
  relocate(NUTS3Type, .before = BuildingTypeSize)

building_stock_district_types <-
  building_stock_district_types %>%
  group_by(NUTS3Type,
           BuildingTypeSize,
           YearOfConstruction) %>%
  summarise(BuildingCount = sum(BuildingCount),
            .groups = "drop") %>%
  mutate_if(is.character, as.factor)


summary(building_stock_district_types)


# Get the number of buildings per district type
buildings_per_district_type <- building_stock_district_types %>%
  select("NUTS3Type", "BuildingCount") %>%
  group_by(NUTS3Type) %>%
  summarise(BuildingCountDistrictType = sum(BuildingCount),
            .groups = "drop")


# Get the share of standalone houses and apartment buildings in the district types
share_sah_ab_district_types <- building_stock_district_types %>%
  select(-c("YearOfConstruction")) %>%
  mutate(
    BuildingTypeSize = fct_recode(
      BuildingTypeSize,
      "ab" = "Apartment Buildings (3-6)",
      "ab" = "Apartment Buildings: 7 and More Apartments",
      "sah" = "One- and Two-family Houses",
      "sah" = "Row Houses",
      "sah" = "Semi-detached Houses"
    )
  ) %>%
  group_by(NUTS3Type, BuildingTypeSize) %>%
  summarise(BuildingCount = sum(BuildingCount), .groups = "drop") %>%
  left_join(buildings_per_district_type, by = "NUTS3Type") %>%
  mutate(ShareBuildingTypeSize = BuildingCount / BuildingCountDistrictType)


# Get the share of buildings built before 1979 in the district types
share_new_old_buildings_district_types <-
  building_stock_district_types %>%
  select(-c("BuildingTypeSize")) %>%
  mutate(
    YearOfConstruction = fct_recode(
      YearOfConstruction,
      "Before 1979" = "Before 1919",
      "Before 1979" = "1919 - 1948",
      "Before 1979" = "1949 - 1978",
      "After 1979" = "1979 - 1986",
      "After 1979" = "1987 - 1990",
      "After 1979" = "1991 - 1995",
      "After 1979" = "1996 - 2000",
      "After 1979" = "2001 - 2011",
      "After 1979" = "2012 - 2022",
      "After 1979" = "2023 - 2030"
    )
  ) %>%
  group_by(NUTS3Type, YearOfConstruction) %>%
  summarise(BuildingCount = sum(BuildingCount), .groups = "drop") %>%
  left_join(buildings_per_district_type, by = "NUTS3Type") %>%
  mutate(ShareYearOfConstruction = BuildingCount / BuildingCountDistrictType)


# Check the regions that have the most buildings
number_buildings_per_region <- building_stock_2030_with_hp_distribution %>%
  select(
    c("NUTS3Code", "BuildingCount")
  ) %>%
  group_by(NUTS3Code) %>%
  summarise(SumBuildingCountNUTS3 = sum(BuildingCount), .groups = "drop") %>%
  left_join(nuts3regioninfo, by = c("NUTS3Code"))


# Get for the nuts regions combined info of share apartment buildings, buildings with yoc under 1979 and the number of buildings

# Calculate the share of apartment buildings for the nuts3 regions

nuts3_share_ab <-
  building_stock_2030_with_hp_distribution %>%
  select(
    "NUTS3Code",
    "BuildingTypeSize",
    "BuildingCount"
  ) %>%
  mutate(
    BuildingTypeSize = fct_recode(
      BuildingTypeSize,
      "ab" = "Apartment Buildings (3-6)",
      "ab" = "Apartment Buildings: 7 and More Apartments",
      "sah" = "One- and Two-family Houses",
      "sah" = "Row Houses",
      "sah" = "Semi-detached Houses"
    )
  ) %>%
  group_by(NUTS3Code, BuildingTypeSize) %>%
  summarise(BuildingCount = sum(BuildingCount), .groups = "drop") %>%
  left_join(number_buildings_per_region, by = c("NUTS3Code")) %>%
  filter(BuildingTypeSize == "ab") %>%
  mutate(ShareApartmentBuildings = BuildingCount / SumBuildingCountNUTS3) %>%
  select(c("NUTS3Code", "ShareApartmentBuildings"))


# Calculate the share of buildings with yoc before 1979
nuts3_share_old_buildings <-
  building_stock_2030_with_hp_distribution %>%
  select(
    "NUTS3Code",
    "YearOfConstruction",
    "BuildingCount"
  ) %>%
  mutate(
    YearOfConstruction = fct_recode(
      YearOfConstruction,
      "Before 1979" = "Before 1919",
      "Before 1979" = "1919 - 1948",
      "Before 1979" = "1949 - 1978",
      "After 1979" = "1979 - 1986",
      "After 1979" = "1987 - 1990",
      "After 1979" = "1991 - 1995",
      "After 1979" = "1996 - 2000",
      "After 1979" = "2001 - 2011",
      "After 1979" = "2012 - 2022",
      "After 1979" = "2023 - 2030"
    )
  ) %>%
  group_by(NUTS3Code, YearOfConstruction) %>%
  summarise(BuildingCount = sum(BuildingCount), .groups = "drop") %>%
  left_join(number_buildings_per_region, by = c("NUTS3Code")) %>%
  filter(YearOfConstruction == "Before 1979") %>%
  mutate(ShareOldBuildings = BuildingCount / SumBuildingCountNUTS3) %>%
  select(c("NUTS3Code", "ShareOldBuildings"))


# Combine data together
nuts3_regression_data <-
  number_buildings_per_region %>%
  left_join(nuts3_share_ab, by = c("NUTS3Code")) %>%
  left_join(nuts3_share_old_buildings, by = c("NUTS3Code"))


# Save data
write_csv(
  nuts3_regression_data,
  "data/output/findings/regressiondata/nuts3_regression_data.csv"
)

