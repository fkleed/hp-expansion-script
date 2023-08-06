# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")


# Load heating and region info
nuts3regioninfo <- read_excel("data/nuts3regioninfo.xlsx")

heatinginfo_without_dormitories <-
  read_csv("data/heatinginfo_without_dormitories.csv")

heatinginfo_only_dormitories <-
  read_csv("data/heatinginfo_only_dormitories.csv")

nuts3regioninfo <-
  nuts3regioninfo %>% mutate_if(is.character, as.factor)

heatinginfo_without_dormitories <-
  heatinginfo_without_dormitories %>% mutate_if(is.character, as.factor)

heatinginfo_only_dormitories <-
  heatinginfo_only_dormitories %>% mutate_if(is.character, as.factor)


# Reduce categories for the building size
heatinginfo_without_dormitories <-
  heatinginfo_without_dormitories %>%
  mutate(
    BuildingTypeSize = fct_recode(
      BuildingTypeSize,
      "Ein- und Zweifamilienhaus" = "Freistehendes Einfamilienhaus",
      "Ein- und Zweifamilienhaus" = "Freistehendes Zweifamilienhaus",
      "Reihenhaus" = "Einfamilienhaus: Reihenhaus",
      "Reihenhaus" = "Zweifamilienhaus: Reihenhaus",
      "Doppelhaushälfte" = "Einfamilienhaus: Doppelhaushälfte",
      "Doppelhaushälfte" = "Zweifamilienhaus: Doppelhaushälfte",
      "Mehrfamilienhaus: 3 - 6 Wohnungen" = "Mehrfamilienhaus: 3 - 6 Wohnungen",
      "Mehrfamilienhaus: 7 und mehr Wohnungen" = "Mehrfamilienhaus: 7 - 12 Wohnungen",
      "Mehrfamilienhaus: 7 und mehr Wohnungen" = "Mehrfamilienhaus: 13 und mehr Wohnungen",
      "Anderer Gebäudetyp" = "Anderer Gebäudetyp"
    )
  ) %>% group_by(BuildingTypeSize,
                 HeatingType,
                 NUTS3Code,
                 YearOfConstruction) %>% summarise(BuildingCount = sum(BuildingCount),
                                                   .groups = 'drop')

heatinginfo_only_dormitories <-
  heatinginfo_only_dormitories %>%
  mutate(
    BuildingTypeSize = fct_recode(
      BuildingTypeSize,
      "Ein- und Zweifamilienhaus" = "Freistehendes Einfamilienhaus",
      "Ein- und Zweifamilienhaus" = "Freistehendes Zweifamilienhaus",
      "Reihenhaus" = "Einfamilienhaus: Reihenhaus",
      "Reihenhaus" = "Zweifamilienhaus: Reihenhaus",
      "Doppelhaushälfte" = "Einfamilienhaus: Doppelhaushälfte",
      "Doppelhaushälfte" = "Zweifamilienhaus: Doppelhaushälfte",
      "Mehrfamilienhaus: 3 - 6 Wohnungen" = "Mehrfamilienhaus: 3 - 6 Wohnungen",
      "Mehrfamilienhaus: 7 und mehr Wohnungen" = "Mehrfamilienhaus: 7 - 12 Wohnungen",
      "Mehrfamilienhaus: 7 und mehr Wohnungen" =
        "Mehrfamilienhaus: 13 und mehr Wohnungen",
      "Anderer Gebäudetyp" = "Anderer Gebäudetyp"
    )
  ) %>% group_by(BuildingTypeSize,
                 HeatingType,
                 NUTS3Code,
                 YearOfConstruction) %>% summarise(BuildingCount = sum(BuildingCount),
                                                   .groups = 'drop')


# Predict distribution of Building Stock for 2022
# Assumption: Buildings without dormitories are distributed on a federal-state basis like the distribution from 2001 onwards

building_stock_2022 <- read_csv("data/building_stock_2022.csv") %>%
  mutate_if(is.character, as.factor) %>%
  left_join(distinct(select(
    nuts3regioninfo, "NUTS1Name", "NUTS1Code"
  )),
  by = c("FederalState" = "NUTS1Name"))

new_buildings_without_dormitories_by_state <-
  heatinginfo_without_dormitories %>%
  left_join(select(nuts3regioninfo, c("NUTS1Code", "NUTS3Code")), by = "NUTS3Code") %>%
  group_by(NUTS1Code) %>%
  summarise(BuildingCount = sum(BuildingCount),
            .groups = 'drop') %>%
  left_join(select(building_stock_2022, c("BuildingsWithoutDormitoriesCount", "NUTS1Code")),
            by = "NUTS1Code") %>%
  mutate(NewBuildingsWithoutDormitoriesCount = BuildingsWithoutDormitoriesCount - BuildingCount) %>%
  select(c("NUTS1Code", "NewBuildingsWithoutDormitoriesCount"))

new_dormitories_by_state <-
  heatinginfo_only_dormitories %>%
  left_join(select(nuts3regioninfo, c("NUTS1Code", "NUTS3Code")), by = "NUTS3Code") %>%
  group_by(NUTS1Code) %>%
  summarise(BuildingCount = sum(BuildingCount),
            .groups = 'drop') %>%
  left_join(select(building_stock_2022, c("DormitoriesCount", "NUTS1Code")),
            by = "NUTS1Code") %>%
  mutate(NewDormitoriesCount = DormitoriesCount - BuildingCount) %>%
  select(c("NUTS1Code", "NewDormitoriesCount"))



summary(building_stock_2022)




levels(heatinginfo_without_dormitories$YearOfConstruction)

test <- heatinginfo_without_dormitories %>%
  filter(YearOfConstruction %in% c("2001 - 2004", "2005 - 2008", "2009 und später")) %>%
  group_by(BuildingTypeSize, HeatingType) %>%
  summarise(BuildingCount = sum(BuildingCount),
            .groups = 'drop') %>%
  mutate(Percentage = round(BuildingCount / sum(BuildingCount) * 100, 2))


# Write output to csv
write_csv2(heatinginfo, "data/output.csv")


# Optional: check if numbers are consistent with building stock
# For example Bavaria had 2.908.526	buildings end of 2011 and zensus was taken mid 2011
heatinginfo_without_dormitories %>%
  inner_join(nuts3regioninfo, by = "NUTS-3-Code") %>%
  group_by(`NUTS1-Name`) %>%
  summarise(sum = sum(`Sum of BuildingCount`))

heatinginfo_only_dormitories %>%
  inner_join(nuts3regioninfo, by = "NUTS-3-Code") %>%
  group_by(`NUTS1-Name`) %>%
  summarise(sum = sum(`Sum of BuildingCount`))

# Optional: Remove the building types without category
heatinginfo_without_dormitories <-
  subset(heatinginfo_without_dormitories,
         BuildingTypeSize != "Anderer Gebäudetyp")

heatinginfo_only_dormitories <-
  subset(heatinginfo_only_dormitories,
         BuildingTypeSize != "Anderer Gebäudetyp")
