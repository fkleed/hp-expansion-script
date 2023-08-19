# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")
library("rjson")
library(stringr)
library(fuzzyjoin)

# Read the building stock data
building_stock_2030 <-
  read_csv2("data/output/buildingstructure/summarized_building_stock_2030.csv") %>% mutate_if(is.character, as.factor)

# Read the nuts region info data
nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx") %>% mutate_if(is.character, as.factor)

# Read the heat pump potential data
hp_potential_2022 <-
  as.data.frame(fromJSON(
    "data/heatpumppotential/waermepumpenampel-german-districts.json"
  )) %>% select(c("region",
                  "internal_id_1",
                  "internal_id_2",
                  "value"))

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
    )
  )

nuts3regioninfo <- nuts3regioninfo %>%
  mutate(
    NUTS3NameModified = ifelse(
      NUTS3Type == "Kreisfreie Stadt" | NUTS3Type == "Stadtkreis",
      paste(gsub(",.*", "", NUTS3Name), "Kreisfreie Stadt", sep = ", "),
      gsub(",.*", "", NUTS3Name)
    )
  )

test <- hp_potential_2022_regions %>% full_join(nuts3regioninfo,
                                                by = c("RegionKey" = "NUTS3NameModified"))


test2 <- filter(test, is.na(region))


