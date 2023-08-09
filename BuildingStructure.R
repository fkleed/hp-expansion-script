# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")


# Load heating and region info
nuts3regioninfo <- read_excel("data/buildingstructure/nuts3regioninfo.xlsx")

heatinginfo_without_dormitories <-
  read_csv("data/buildingstructure/heatinginfo_without_dormitories.csv")

heatinginfo_only_dormitories <-
  read_csv("data/buildingstructure/heatinginfo_only_dormitories.csv")

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

building_stock_2022 <- read_csv("data/buildingstructure/building_stock_2022.csv") %>%
  mutate_if(is.character, as.factor) %>%
  inner_join(distinct(select(
    nuts3regioninfo, "NUTS1Name", "NUTS1Code"
  )),
  by = c("FederalState" = "NUTS1Name"))

# Calculate the difference between zensus 2011 and 2022
new_buildings_without_dormitories_by_state <-
  heatinginfo_without_dormitories %>%
  inner_join(select(nuts3regioninfo, c("NUTS1Code", "NUTS3Code")), by = "NUTS3Code") %>%
  group_by(NUTS1Code) %>%
  summarise(BuildingCount = sum(BuildingCount),
            .groups = 'drop') %>%
  inner_join(select(
    building_stock_2022,
    c("BuildingsWithoutDormitoriesCount", "NUTS1Code")
  ),
  by = "NUTS1Code") %>%
  mutate(NewBuildingsWithoutDormitoriesCount = BuildingsWithoutDormitoriesCount - BuildingCount) %>%
  select(c("NUTS1Code", "NewBuildingsWithoutDormitoriesCount"))

new_dormitories_by_state <-
  heatinginfo_only_dormitories %>%
  inner_join(select(nuts3regioninfo, c("NUTS1Code", "NUTS3Code")), by = "NUTS3Code") %>%
  group_by(NUTS1Code) %>%
  summarise(BuildingCount = sum(BuildingCount),
            .groups = 'drop') %>%
  inner_join(select(building_stock_2022, c("DormitoriesCount", "NUTS1Code")),
             by = "NUTS1Code") %>%
  mutate(NewDormitoriesCount = DormitoriesCount - BuildingCount) %>%
  select(c("NUTS1Code", "NewDormitoriesCount"))

# Calculate the distributions of new buildings based on distribution from 2001 onwards on a federal-state level
distribution_buildings_without_dormitories <-
  heatinginfo_without_dormitories %>%
  filter(YearOfConstruction %in% c("2001 - 2004", "2005 - 2008", "2009 und später")) %>%
  inner_join(select(nuts3regioninfo, c("NUTS1Code", "NUTS3Code")), by = "NUTS3Code") %>%
  group_by(NUTS1Code) %>%
  summarise(BuildingsByStateCount = sum(BuildingCount),
            .groups = 'drop') %>%
  inner_join(mutate(
    filter(
      heatinginfo_without_dormitories,
      YearOfConstruction %in% c("2001 - 2004", "2005 - 2008", "2009 und später")
    ),
    NUTS1Code = substr(NUTS3Code, 1, 3)
  ), by = "NUTS1Code") %>%
  select(-c("YearOfConstruction")) %>%
  group_by(NUTS1Code,
           BuildingsByStateCount,
           BuildingTypeSize,
           HeatingType,
           NUTS3Code) %>% summarise(BuildingCount = sum(BuildingCount),
                                    .groups = 'drop') %>%
  mutate(Share = BuildingCount / BuildingsByStateCount) %>%
  inner_join(new_buildings_without_dormitories_by_state, by = "NUTS1Code") %>%
  mutate(NewBuildingCount = round(Share * NewBuildingsWithoutDormitoriesCount)) %>%
  select(
    -c(
      "NUTS1Code",
      "BuildingsByStateCount",
      "BuildingCount",
      "Share",
      "NewBuildingsWithoutDormitoriesCount"
    )
  ) %>%
  mutate(YearOfConstruction = "2012 - 2022") %>%
  relocate(YearOfConstruction, .before = NewBuildingCount)

distribution_dormitories <-
  heatinginfo_only_dormitories %>%
  filter(YearOfConstruction %in% c("2001 - 2004", "2005 - 2008", "2009 und später")) %>%
  inner_join(select(nuts3regioninfo, c("NUTS1Code", "NUTS3Code")), by = "NUTS3Code") %>%
  group_by(NUTS1Code) %>%
  summarise(BuildingsByStateCount = sum(BuildingCount),
            .groups = 'drop') %>%
  inner_join(mutate(
    filter(
      heatinginfo_only_dormitories,
      YearOfConstruction %in% c("2001 - 2004", "2005 - 2008", "2009 und später")
    ),
    NUTS1Code = substr(NUTS3Code, 1, 3)
  ), by = "NUTS1Code") %>%
  select(-c("YearOfConstruction")) %>%
  group_by(NUTS1Code,
           BuildingsByStateCount,
           BuildingTypeSize,
           HeatingType,
           NUTS3Code) %>% summarise(BuildingCount = sum(BuildingCount),
                                    .groups = 'drop') %>%
  mutate(Share = BuildingCount / BuildingsByStateCount) %>%
  inner_join(new_dormitories_by_state, by = "NUTS1Code") %>%
  mutate(NewBuildingCount = round(Share * NewDormitoriesCount)) %>%
  select(
    -c(
      "NUTS1Code",
      "BuildingsByStateCount",
      "BuildingCount",
      "Share",
      "NewDormitoriesCount"
    )
  ) %>%
  mutate(YearOfConstruction = "2012 - 2022") %>%
  relocate(YearOfConstruction, .before = NewBuildingCount)

# Get new heatinginfos until 2022
heatinginfo_without_dormitories_2022 <-
  heatinginfo_without_dormitories %>%
  mutate(YearOfConstruction = fct_recode(YearOfConstruction,
                                         "2009 - 2011" = "2009 und später")) %>%
  rbind(
    rename(distribution_buildings_without_dormitories,
           BuildingCount = NewBuildingCount)
  )

heatinginfo_only_dormitories_2022 <-
  heatinginfo_only_dormitories %>%
  mutate(YearOfConstruction = fct_recode(YearOfConstruction,
                                         "2009 - 2011" = "2009 und später")) %>%
  rbind(rename(distribution_dormitories,
               BuildingCount = NewBuildingCount))


# Predict distribution of Building Stock for 2030
# Assumption: The increase in building stock from 2023 - 2030 is assumed to be distributed as in 2012-2022
# Assumption: Due to increasing surface sealing and rising construction costs, the overall building increase is only 60% compared to 2012 - 2022

new_buildings_without_dormitories_by_state_2030 <-
  new_buildings_without_dormitories_by_state %>%
  mutate(NewBuildingsWithoutDormitoriesCount = NewBuildingsWithoutDormitoriesCount * 0.6)

new_dormitories_by_state_2030 <- new_dormitories_by_state %>%
  mutate(NewDormitoriesCount = NewDormitoriesCount * 0.6)

distribution_buildings_without_dormitories_2030 <-
  heatinginfo_without_dormitories %>%
  filter(YearOfConstruction %in% c("2001 - 2004", "2005 - 2008", "2009 und später")) %>%
  inner_join(select(nuts3regioninfo, c("NUTS1Code", "NUTS3Code")), by = "NUTS3Code") %>%
  group_by(NUTS1Code) %>%
  summarise(BuildingsByStateCount = sum(BuildingCount),
            .groups = 'drop') %>%
  inner_join(mutate(
    filter(
      heatinginfo_without_dormitories,
      YearOfConstruction %in% c("2001 - 2004", "2005 - 2008", "2009 und später")
    ),
    NUTS1Code = substr(NUTS3Code, 1, 3)
  ), by = "NUTS1Code") %>%
  select(-c("YearOfConstruction")) %>%
  group_by(NUTS1Code,
           BuildingsByStateCount,
           BuildingTypeSize,
           HeatingType,
           NUTS3Code) %>% summarise(BuildingCount = sum(BuildingCount),
                                    .groups = 'drop') %>%
  mutate(Share = BuildingCount / BuildingsByStateCount) %>%
  inner_join(new_buildings_without_dormitories_by_state_2030, by = "NUTS1Code") %>%
  mutate(NewBuildingCount = round(Share * NewBuildingsWithoutDormitoriesCount)) %>%
  select(
    -c(
      "NUTS1Code",
      "BuildingsByStateCount",
      "BuildingCount",
      "Share",
      "NewBuildingsWithoutDormitoriesCount"
    )
  ) %>%
  mutate(YearOfConstruction = "2023 - 2030") %>%
  relocate(YearOfConstruction, .before = NewBuildingCount)

distribution_dormitories_2030 <-
  heatinginfo_only_dormitories %>%
  filter(YearOfConstruction %in% c("2001 - 2004", "2005 - 2008", "2009 und später")) %>%
  inner_join(select(nuts3regioninfo, c("NUTS1Code", "NUTS3Code")), by = "NUTS3Code") %>%
  group_by(NUTS1Code) %>%
  summarise(BuildingsByStateCount = sum(BuildingCount),
            .groups = 'drop') %>%
  inner_join(mutate(
    filter(
      heatinginfo_only_dormitories,
      YearOfConstruction %in% c("2001 - 2004", "2005 - 2008", "2009 und später")
    ),
    NUTS1Code = substr(NUTS3Code, 1, 3)
  ), by = "NUTS1Code") %>%
  select(-c("YearOfConstruction")) %>%
  group_by(NUTS1Code,
           BuildingsByStateCount,
           BuildingTypeSize,
           HeatingType,
           NUTS3Code) %>% summarise(BuildingCount = sum(BuildingCount),
                                    .groups = 'drop') %>%
  mutate(Share = BuildingCount / BuildingsByStateCount) %>%
  inner_join(new_dormitories_by_state_2030, by = "NUTS1Code") %>%
  mutate(NewBuildingCount = round(Share * NewDormitoriesCount)) %>%
  select(
    -c(
      "NUTS1Code",
      "BuildingsByStateCount",
      "BuildingCount",
      "Share",
      "NewDormitoriesCount"
    )
  ) %>%
  mutate(YearOfConstruction = "2023 - 2030") %>%
  relocate(YearOfConstruction, .before = NewBuildingCount)

# Get new heatinginfos until 2030

heatinginfo_without_dormitories_2030 <-
  heatinginfo_without_dormitories_2022 %>%
  rbind(
    rename(
      distribution_buildings_without_dormitories_2030,
      BuildingCount = NewBuildingCount
    )
  )

heatinginfo_only_dormitories_2030 <-
  heatinginfo_only_dormitories_2022 %>%
  rbind(rename(distribution_dormitories_2030,
               BuildingCount = NewBuildingCount))

sum(heatinginfo_without_dormitories$BuildingCount) + sum(heatinginfo_only_dormitories$BuildingCount)
sum(heatinginfo_without_dormitories_2022$BuildingCount) + sum(heatinginfo_only_dormitories_2022$BuildingCount)
sum(heatinginfo_without_dormitories_2030$BuildingCount) + sum(heatinginfo_only_dormitories_2030$BuildingCount)


# Calculate the whole summarized building stock
summarized_building_stock <- heatinginfo_without_dormitories %>%
  rbind(heatinginfo_only_dormitories) %>%
  group_by(BuildingTypeSize,
           HeatingType,
           NUTS3Code,
           YearOfConstruction) %>%
  summarise(BuildingCount = sum(BuildingCount), .groups = 'drop')

summarized_building_stock_2022 <-
  heatinginfo_without_dormitories_2022 %>%
  rbind(heatinginfo_only_dormitories_2022) %>%
  group_by(BuildingTypeSize,
           HeatingType,
           NUTS3Code,
           YearOfConstruction) %>%
  summarise(BuildingCount = sum(BuildingCount), .groups = 'drop')

summarized_building_stock_2030 <-
  heatinginfo_without_dormitories_2030 %>%
  rbind(heatinginfo_only_dormitories_2030) %>%
  group_by(BuildingTypeSize,
           HeatingType,
           NUTS3Code,
           YearOfConstruction) %>%
  summarise(BuildingCount = sum(BuildingCount), .groups = 'drop')

sum(summarized_building_stock$BuildingCount)
sum(summarized_building_stock_2022$BuildingCount)
sum(summarized_building_stock_2030$BuildingCount)


# Write output to csv
write_csv2(summarized_building_stock,
           "data/buildingstructure/output/summarized_building_stock.csv")
write_csv2(
  summarized_building_stock_2022,
  "data/buildingstructure/output/summarized_building_stock_2022.csv"
)
write_csv2(
  summarized_building_stock_2030,
  "data/buildingstructure/output/summarized_building_stock_2030.csv"
)


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
