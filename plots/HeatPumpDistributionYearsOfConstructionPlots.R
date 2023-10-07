# Load required packages
library(tidyverse)
library("dplyr")

# Read the data
building_stock_2030_with_hp_distribution <-
  read_csv("data/output/heatpumpexpansion/building_stock_2030_with_hp_distribution.csv") %>% mutate_if(is.character, as.factor)


# Calculate the heat pump distribution for the different years of construction
hp_distribution_years_of_construction <-
  building_stock_2030_with_hp_distribution %>%
  mutate(HPSum = HPAmountAir + HPAmountProbe + HPAmountCollector)

hp_distribution_years_of_construction <-
  hp_distribution_years_of_construction %>%
  select(c("BuildingTypeSize",
           "YearOfConstruction",
           "HPSum"))

hp_distribution_years_of_construction <-
  hp_distribution_years_of_construction %>%
  group_by(BuildingTypeSize, YearOfConstruction) %>%
  summarise("HPSum" = sum(HPSum),
            .groups = "drop")

hp_distribution_years_of_construction <-
  hp_distribution_years_of_construction %>%
  mutate(
    BuildingTypeSize = fct_recode(
      BuildingTypeSize,
      "Apartment Buildings: 3 to 6 Apartments" = "Apartment Buildings (3-6)"
    )
  )


# Plot bar chart for the distribution of heat pumps on the building types with different years of construction
bar_chart_hp_per_year_of_construction <-
  ggplot(data = hp_distribution_years_of_construction) +
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
    y = HPSum,
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
       y = "Number of heat pumps",
       fill = "Building type") +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  guides(fill = guide_legend(nrow =  2)) +
  theme(legend.position="bottom")



bar_chart_hp_per_year_of_construction


# Save the bar chart
ggsave(
  "plots/output/heatpumpdistribution/bar_chart_hp_per_year_of_construction.png",
  bar_chart_hp_per_year_of_construction,
  width = 30,
  units = "cm"
)
