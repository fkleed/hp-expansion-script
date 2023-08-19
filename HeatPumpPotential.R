# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")
library(stringr)

# Read the building stock data
building_stock_2030 <-
  read_csv2("data/output/buildingstructure/summarized_building_stock_2030.csv") %>% mutate_if(is.character, as.factor)


# Read the nuts region info data
nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx") %>% mutate_if(is.character, as.factor)


# Read the heat pump potential data
hp_potential_2022 <-
  read_csv("data/heatpumppotential/heat-pump-traffic-light.csv")


# Join the heat pump potential with the nuts region info data
hp_potential_2022_regions <-
  hp_potential_2022 %>% distinct(region) %>% mutate_if(is.character, as.factor) %>% mutate(RegionKey = region)

hp_potential_2022_regions <- hp_potential_2022_regions %>%
  mutate(
    RegionKey = fct_recode(
      RegionKey,
      "Hamburg, Kreisfreie Stadt" = "Hamburg",
      "Göttingen" = "Osterode am Harz",
      "Oldenburg (Oldenburg), Kreisfreie Stadt" = "Oldenburg (Oldb), Kreisfreie Stadt",
      "Mühldorf a.Inn" = "Mühldorf a. Inn",
      "Pfaffenhofen a.d.Ilm" = "Pfaffenhofen a.d. Ilm",
      "Weiden i.d.OPf., Kreisfreie Stadt" = "Weiden i.d. OPf., Kreisfreie Stadt",
      "Neumarkt i.d.OPf." = "Neumarkt i.d. OPf.",
      "Neustadt a.d.Waldnaab" = "Neustadt a.d. Waldnaab",
      "Wunsiedel i.Fichtelgebirge" = "Wunsiedel i. Fichtelgebirge",
      "Neustadt a.d.Aisch-Bad Windsheim" = "Neustadt a.d. Aisch-Bad Windsheim",
      "Dillingen a.d.Donau" = "Dillingen a.d. Donau",
      "Berlin, Kreisfreie Stadt" = "Berlin",
      "Chemnitz, Kreisfreie Stadt" = "Chemnitz",
      "Dresden, Kreisfreie Stadt" = "Dresden",
      "Wartburgkreis" = "Eisenach, Kreisfreie Stadt",
      "Landkreis Rostock" = "Rostock"
    )
  )

nuts3regioninfo <- nuts3regioninfo %>%
  mutate(
    NUTS3NameModified = ifelse(
      NUTS3Type == "Kreisfreie Stadt" | NUTS3Type == "Stadtkreis",
      paste(gsub(",.*", "", NUTS3Name), "Kreisfreie Stadt", sep = ", "),
      gsub(",.*"
           , "", NUTS3Name)
    )
  )

hp_potential_2022_regions <-
  hp_potential_2022_regions %>% full_join(nuts3regioninfo,
                                          by = c("RegionKey" = "NUTS3NameModified"))

hp_potential_2022_regions <- hp_potential_2022_regions %>%
  select(c("region", "NUTS3Code"))

hp_potential_2022 <- hp_potential_2022 %>%
  left_join(hp_potential_2022_regions, by = "region") %>%
  relocate(NUTS3Code, .before = region) %>%
  select(-c("region")) %>%
  rename(
    "BuildingType" = "internal_id_1",
    "HPHeatSource" = "internal_id_2",
    "Potential" = "Sum of value"
  )

remove(hp_potential_2022_regions)

hp_potential_2022 <- hp_potential_2022 %>%
  group_by(NUTS3Code, BuildingType, HPHeatSource) %>%
  summarise(Potential = mean(Potential), .groups = 'drop') %>%
  mutate(BuildingType = as.factor(BuildingType),
         HPHeatSource = as.factor(HPHeatSource))

hp_potential_2022 <- hp_potential_2022 %>%
  mutate(
    BuildingType = fct_recode(
      BuildingType,
      "One- and Two-family Houses" = "1",
      "Apartment Buildings (3-6)" = "6",
      "Row Houses" = "9",
      "Semi-detached Houses" = "11",
      "Apartment Buildings: 7 and More Apartments" = "38",
      "Total" = "100"
    ),
    HPHeatSource = fct_recode(
      HPHeatSource,
      "Total" = "0",
      "Air" = "1",
      "Ground Probe" = "2",
      "Ground Collector" = "3",
      "Solar-Thermal Energy and Ice Storage" = "4"
    )
  )


# Combine the building stock data with heat pump potentials
hp_potential_2022 <-
  hp_potential_2022 %>%
  filter(BuildingType != "Total") %>%
  spread(HPHeatSource, Potential) %>%
  rename(
    "HPPotentialTotal" = "Total",
    "HPPotentialAir" = "Air",
    "HPPotentialProbe" = "Ground Probe",
    "HPPotentialCollector" = "Ground Collector",
    "HPPotentialSolarThermalEnergyIceStorage" = "Solar-Thermal Energy and Ice Storage"
  )

building_stock_2030_with_hp_potential <- building_stock_2030 %>%
  inner_join(
    hp_potential_2022,
    by = c("BuildingTypeSize" = "BuildingType", "NUTS3Code" = "NUTS3Code")
  ) %>%
  relocate(NUTS3Code, .before = BuildingTypeSize)


# Write output to csv
write_csv2(
  building_stock_2030_with_hp_potential,
  "data/output/heatpumppotential/building_stock_2030_with_hp_potential.csv"
)
