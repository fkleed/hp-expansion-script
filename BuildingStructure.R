# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")


# Load data
nuts3regioninfo <- read_excel("data/nuts3regioninfo.xlsx")
heatinginfo <- read_csv("data/heatinginfo.csv")


# Transform data
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
heatinginfo <- subset(heatinginfo, BuildingTypeSize != "Anderer Gebäudetyp")

heatinginfo <- heatinginfo %>% group_by(
    BuildingTypeSize,
    HeatingType,
    `NUTS-3-Code`,
    YearOfConstruction
  ) %>% summarise(`Sum of BuildingCount` = sum(`Sum of BuildingCount`),
                  .groups = 'drop')

nrow(heatinginfo)

# Write output to csv
write_csv2(heatinginfo, "data/output.csv")
