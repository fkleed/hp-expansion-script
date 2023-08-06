# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")


# Load data
nuts3regioninfo <- read_excel("data/nuts3regioninfo.xlsx")
heatinginfo <- read_csv("data/heatinginfo.csv")


# Transform data

# Reduce categories for the building size
heatinginfo <- heatinginfo %>% select(-c(RegionKey, RegionName, RegionType))

heatinginfo$BuildingTypeSize <- factor(heatinginfo$BuildingTypeSize)
heatinginfo$HeatingType <- factor(heatinginfo$HeatingType)
heatinginfo$YearOfConstruction <- factor(heatinginfo$YearOfConstruction)

heatinginfo <- heatinginfo %>%
  mutate(BuildingTypeSize = fct_recode(BuildingTypeSize,
    "Ein- und Zweifamilienhaus" = "Freistehendes Einfamilienhaus",
    "Ein- und Zweifamilienhaus" = "Freistehendes Zweifamilienhaus",
    "Reihenhaus" = "Einfamilienhaus: Reihenhaus",
    "Reihenhaus" = "Zweifamilienhaus: Reihenhaus",
    "Doppelhaushälfte" = "Einfamilienhaus: Doppelhaushälfte",
    "Doppelhaushälfte" = "Zweifamilienhaus: Doppelhaushälfte",
    "Mehrfamilienhaus: 3 - 6 Wohnungen" = "Mehrfamilienhaus: 3 - 6 Wohnungen",
    "Mehrfamilienhaus: 7 und mehr Wohnungen" = "Mehrfamilienhaus: 7 - 12 Wohnungen",
    "Mehrfamilienhaus: 7 und mehr Wohnungen" ="Mehrfamilienhaus: 13 und mehr Wohnungen",
    "Anderer Gebäudetyp" = "Anderer Gebäudetyp"
                                       ))

heatinginfo <- heatinginfo %>% group_by(
    BuildingTypeSize,
    HeatingType,
    `NUTS-3-Code`,
    YearOfConstruction
  ) %>% summarise(`Sum of BuildingCount` = sum(`Sum of BuildingCount`),
                  .groups = 'drop')

# Optional: check if numbers are consistent with building stock
buildings_by_federal_state <- heatinginfo %>%
inner_join(nuts3regioninfo, by = "NUTS-3-Code") %>%
  group_by(`NUTS1-Name`) %>%
  summarise(sum = sum(`Sum of BuildingCount`))

# Optional: Remove later the building types without category
heatinginfo <- subset(heatinginfo, BuildingTypeSize != "Anderer Gebäudetyp")

# Calculate Building Stock for 2022



#Write output to csv
write_csv2(heatinginfo, "data/buildingstock2022.csv")
