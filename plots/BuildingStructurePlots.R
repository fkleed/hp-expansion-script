# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)

# Read the data
nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx")

summarized_building_stock_2030 <-
  read_csv2("data/output/buildingstructure/summarized_building_stock_2030.csv") %>% mutate_if(is.character, as.factor)

summarized_building_stock_2030 <- summarized_building_stock_2030 %>%
  mutate(
    BuildingTypeSize = fct_recode(
      BuildingTypeSize,
      "Apartment Buildings: 3 to 6 Apartments" = "Apartment Buildings (3-6)"
    )
  )


# Bar chart for the building types per year
bar_chart_building_types_by_year_of_construction <-
  ggplot(data = summarized_building_stock_2030) +
  geom_bar(mapping = aes(
    x = factor(
      YearOfConstruction,
      level = c(
        "Before 1919",
        "1919 - 1948",
        "1949 - 1978",
        "1979 - 1986",
        "1987 - 1990",
        "1991 - 1995",
        "1996 - 2000",
        "2001 - 2011",
        "2012 - 2022",
        "2023 - 2030"
      )
    ),
    y = (BuildingCount / 1000000),
    fill = factor(
      BuildingTypeSize,
      level = c(
        "Other",
        "Apartment Buildings: 7 and More Apartments",
        "Apartment Buildings: 3 to 6 Apartments",
        "Semi-detached Houses",
        "Row Houses",
        "One- and Two-family Houses"
      )
    )
  ),
  stat = "identity") +
  labs(x = "Years of construction",
       y = "Number of buildings in millions",
       fill = "Building type") +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme(legend.position = "bottom")


bar_chart_building_types_by_year_of_construction

# Bar chart for the building types per federal state
nuts3_federal_states <- nuts3regioninfo %>%
  select(c("NUTS1Name", "NUTS3Code")) %>%
  mutate_if(is.character, as.factor)

nuts3_federal_states <- nuts3_federal_states %>%
  mutate(
    NUTS1Name = fct_recode(
      NUTS1Name,
      "Schleswig Holstein" = "Schleswig-Holstein",
      "Baden-Württemberg" = "Baden-Württemberg",
      "Hamburg" = "Hamburg",
      "Lower Saxony" = "Niedersachsen",
      "Bavaria" = "Bayern",
      "Bremen" = "Bremen",
      "Northrhine-Westphalia" = "Nordrhein-Westfalen",
      "Hesse" = "Hessen",
      "Berlin" = "Berlin",
      "Brandenburg" = "Brandenburg",
      "Rhineland Palatinate" = "Rheinland-Pfalz",
      "Mecklenburg Western Pomerania" = "Mecklenburg-Vorpommern",
      "Saarland" = "Saarland",
      "Saxony" = "Sachsen",
      "Saxony-Anhalt" = "Sachsen-Anhalt",
      "Thuringia" = "Thüringen",
    )
  )

summarized_building_stock_2030_with_federal_states <-
  summarized_building_stock_2030 %>%
  left_join(nuts3_federal_states, by = "NUTS3Code") %>%
  select(-c("HeatingType",
            "NUTS3Code",
            "YearOfConstruction"))

summarized_building_stock_2030_with_federal_states <-
  summarized_building_stock_2030_with_federal_states %>%
  group_by(NUTS1Name, BuildingTypeSize) %>%
  summarise(BuildingCount = sum(BuildingCount), .groups = "drop")


sum_of_buildings_per_state <- summarized_building_stock_2030_with_federal_states %>%
  select(-c("BuildingTypeSize")) %>%
  group_by(NUTS1Name) %>%
  summarise(BuildingsPerStateSum = sum(BuildingCount), .groups = "drop")


summarized_building_stock_2030_with_federal_states <- summarized_building_stock_2030_with_federal_states %>%
  left_join(sum_of_buildings_per_state, by = c("NUTS1Name"))


bar_chart_building_types_by_federal_state <-
  ggplot(data = summarized_building_stock_2030_with_federal_states) +
  geom_bar(mapping = aes(
    x = reorder(NUTS1Name, -BuildingsPerStateSum),
    y = (BuildingCount / 1000000),
    fill = factor(
      BuildingTypeSize,
      level = c(
        "Other",
        "Apartment Buildings: 7 and More Apartments",
        "Apartment Buildings: 3 to 6 Apartments",
        "Semi-detached Houses",
        "Row Houses",
        "One- and Two-family Houses"
      )
    )
  ),
  stat = "identity") +
  labs(x = "Federal state",
       y = "Number of buildings in millions",
       fill = "Building type") +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme(legend.position = "bottom")

bar_chart_building_types_by_federal_state


# Save plots
ggsave(
  "plots/output/buildingstructure/bar_chart_building_types_by_year_of_construction.png",
  bar_chart_building_types_by_year_of_construction,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/buildingstructure/bar_chart_building_types_by_federal_state.png",
  bar_chart_building_types_by_federal_state,
  width = 30,
  units = "cm"
)
