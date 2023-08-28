# Load required packages
library(tidyverse)
library("dplyr")

# Read building stock data 2030
summarized_building_stock_2030 <-
  read_csv2("data/output/buildingstructure/summarized_building_stock_2030.csv") %>% mutate_if(is.character, as.factor)

# Plot a bar chart the building types per year
bar_chart_building_types_per_year <- ggplot(data = summarized_building_stock_2030) +
  geom_bar(
    mapping = aes(
      x = factor(YearOfConstruction, level = c(
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
        )),
      y = (BuildingCount / 1000000),
      fill = factor(BuildingTypeSize, level = c(
        "Other",
        "Apartment Buildings: 7 and More Apartments",
        "Apartment Buildings (3-6)",
        "Semi-detached Houses",
        "Row Houses",
        "One- and Two-family Houses"
      ))),
    stat = "identity"
  ) +
  labs(
    x = "Year of construction",
    y = "Number of buildings in millions",
    fill = "Building type"
  ) +
  scale_fill_brewer(palette = "Set3")

bar_chart_building_types_per_year + coord_flip()


sum(summarized_building_stock_2030$BuildingCount)
