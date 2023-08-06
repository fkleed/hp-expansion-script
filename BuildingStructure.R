# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")


# Load data
nuts3regioninfo <- read_excel("data/nuts3regioninfo.xlsx")

heatinginfo_without_dormitories <-
  read_csv("data/heatinginfo_without_dormitories.csv")

heatinginfo_only_dormitories <-
  read_csv("data/heatinginfo_only_dormitories.csv")


# Transform data

# Reduce categories for the building size
nuts3regioninfo <-
  nuts3regioninfo %>% mutate_if(is.character, as.factor)

heatinginfo_without_dormitories <-
  heatinginfo_without_dormitories %>% mutate_if(is.character, as.factor)

heatinginfo_only_dormitories <-
  heatinginfo_only_dormitories %>% mutate_if(is.character, as.factor)

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
                 `NUTS-3-Code`,
                 YearOfConstruction) %>% summarise(`Sum of BuildingCount` = sum(`Sum of BuildingCount`),
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
                 `NUTS-3-Code`,
                 YearOfConstruction) %>% summarise(`Sum of BuildingCount` = sum(`Sum of BuildingCount`),
                                                   .groups = 'drop')



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

# Optional: Remove later the building types without category
heatinginfo_without_dormitories <-
  subset(heatinginfo_without_dormitories, BuildingTypeSize != "Anderer Gebäudetyp")

heatinginfo_only_dormitories <-
  subset(heatinginfo_only_dormitories, BuildingTypeSize != "Anderer Gebäudetyp")

# Predict distribution of Building Stock for 2022



# Write output to csv
write_csv2(heatinginfo, "data/output.csv")
