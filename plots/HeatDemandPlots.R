# Load required packages
library(tidyverse)
library("dplyr")


# Read data
eh_combined_heat_demand_reference <-
  read_csv2("data/output/heatdemand/eh_combined_heat_demand_reference.csv") %>% mutate(
    "Beginn - 1918" = SpaceHeat_beginn_1918_reference + HotWater_reference,
    "1919 - 1948" = SpaceHeat_1919_1948_reference + HotWater_reference,
    "1949 - 1978" = SpaceHeat_1949_1978_reference + HotWater_reference,
    "1979 - 1986" = SpaceHeat_1979_1986_reference + HotWater_reference,
    "1987 - 1990" = SpaceHeat_1987_1990_reference + HotWater_reference,
    "1991 - 1995" = SpaceHeat_1991_1995_reference + HotWater_reference,
    "1996 - 2000" = SpaceHeat_1996_2000_reference + HotWater_reference,
    "2001 - 2011" = SpaceHeat_2001_2011_reference + HotWater_reference,
    "2012 - 2022" = SpaceHeat_2012_2022_reference + HotWater_reference,
    "2023 - 2030" = SpaceHeat_2023_2030_reference + HotWater_reference,
  ) %>% select(
    -c(
      SpaceHeat_beginn_1918_reference,
      SpaceHeat_1919_1948_reference,
      SpaceHeat_1949_1978_reference,
      SpaceHeat_1979_1986_reference,
      SpaceHeat_1987_1990_reference,
      SpaceHeat_1991_1995_reference,
      SpaceHeat_1996_2000_reference,
      SpaceHeat_2001_2011_reference,
      SpaceHeat_2012_2022_reference,
      SpaceHeat_2023_2030_reference,
      HotWater_reference
    )
  ) %>% gather("Year of construction",
               "Heat demand in kWh",
               2:11) %>% mutate("Year" = 2017)

eh_combined_heat_demand_cold <-
  read_csv2("data/output/heatdemand/eh_combined_heat_demand_cold.csv") %>% mutate(
    "Beginn - 1918" = SpaceHeat_beginn_1918_cold + HotWater_cold,
    "1919 - 1948" = SpaceHeat_1919_1948_cold + HotWater_cold,
    "1949 - 1978" = SpaceHeat_1949_1978_cold + HotWater_cold,
    "1979 - 1986" = SpaceHeat_1979_1986_cold + HotWater_cold,
    "1987 - 1990" = SpaceHeat_1987_1990_cold + HotWater_cold,
    "1991 - 1995" = SpaceHeat_1991_1995_cold + HotWater_cold,
    "1996 - 2000" = SpaceHeat_1996_2000_cold + HotWater_cold,
    "2001 - 2011" = SpaceHeat_2001_2011_cold + HotWater_cold,
    "2012 - 2022" = SpaceHeat_2012_2022_cold + HotWater_cold,
    "2023 - 2030" = SpaceHeat_2023_2030_cold + HotWater_cold,
  ) %>% select(
    -c(
      SpaceHeat_beginn_1918_cold,
      SpaceHeat_1919_1948_cold,
      SpaceHeat_1949_1978_cold,
      SpaceHeat_1979_1986_cold,
      SpaceHeat_1987_1990_cold,
      SpaceHeat_1991_1995_cold,
      SpaceHeat_1996_2000_cold,
      SpaceHeat_2001_2011_cold,
      SpaceHeat_2012_2022_cold,
      SpaceHeat_2023_2030_cold,
      HotWater_cold
    )
  ) %>% gather("Year of construction",
               "Heat demand in kWh",
               2:11) %>% mutate("Year" = 2010)

eh_combined_heat_demand_hot <-
  read_csv2("data/output/heatdemand/eh_combined_heat_demand_hot.csv") %>% mutate(
    "Beginn - 1918" = SpaceHeat_beginn_1918_hot + HotWater_hot,
    "1919 - 1948" = SpaceHeat_1919_1948_hot + HotWater_hot,
    "1949 - 1978" = SpaceHeat_1949_1978_hot + HotWater_hot,
    "1979 - 1986" = SpaceHeat_1979_1986_hot + HotWater_hot,
    "1987 - 1990" = SpaceHeat_1987_1990_hot + HotWater_hot,
    "1991 - 1995" = SpaceHeat_1991_1995_hot + HotWater_hot,
    "1996 - 2000" = SpaceHeat_1996_2000_hot + HotWater_hot,
    "2001 - 2011" = SpaceHeat_2001_2011_hot + HotWater_hot,
    "2012 - 2022" = SpaceHeat_2012_2022_hot + HotWater_hot,
    "2023 - 2030" = SpaceHeat_2023_2030_hot + HotWater_hot,
  ) %>% select(
    -c(
      SpaceHeat_beginn_1918_hot,
      SpaceHeat_1919_1948_hot,
      SpaceHeat_1949_1978_hot,
      SpaceHeat_1979_1986_hot,
      SpaceHeat_1987_1990_hot,
      SpaceHeat_1991_1995_hot,
      SpaceHeat_1996_2000_hot,
      SpaceHeat_2001_2011_hot,
      SpaceHeat_2012_2022_hot,
      SpaceHeat_2023_2030_hot,
      HotWater_hot
    )
  ) %>% gather("Year of construction",
               "Heat demand in kWh",
               2:11) %>% mutate("Year" = 2022)

mh_combined_heat_demand_reference <-
  read_csv2("data/output/heatdemand/mh_combined_heat_demand_reference.csv") %>% mutate(
    "Beginn - 1918" = SpaceHeat_beginn_1918_reference + HotWater_reference,
    "1919 - 1948" = SpaceHeat_1919_1948_reference + HotWater_reference,
    "1949 - 1978" = SpaceHeat_1949_1978_reference + HotWater_reference,
    "1979 - 1986" = SpaceHeat_1979_1986_reference + HotWater_reference,
    "1987 - 1990" = SpaceHeat_1987_1990_reference + HotWater_reference,
    "1991 - 1995" = SpaceHeat_1991_1995_reference + HotWater_reference,
    "1996 - 2000" = SpaceHeat_1996_2000_reference + HotWater_reference,
    "2001 - 2011" = SpaceHeat_2001_2011_reference + HotWater_reference,
    "2012 - 2022" = SpaceHeat_2012_2022_reference + HotWater_reference,
    "2023 - 2030" = SpaceHeat_2023_2030_reference + HotWater_reference,
  ) %>% select(
    -c(
      SpaceHeat_beginn_1918_reference,
      SpaceHeat_1919_1948_reference,
      SpaceHeat_1949_1978_reference,
      SpaceHeat_1979_1986_reference,
      SpaceHeat_1987_1990_reference,
      SpaceHeat_1991_1995_reference,
      SpaceHeat_1996_2000_reference,
      SpaceHeat_2001_2011_reference,
      SpaceHeat_2012_2022_reference,
      SpaceHeat_2023_2030_reference,
      HotWater_reference
    )
  ) %>% gather("Year of construction",
               "Heat demand in kWh",
               2:11) %>% mutate("Year" = 2017)

mh_combined_heat_demand_cold <-
  read_csv2("data/output/heatdemand/mh_combined_heat_demand_cold.csv") %>% mutate(
    "Beginn - 1918" = SpaceHeat_beginn_1918_cold + HotWater_cold,
    "1919 - 1948" = SpaceHeat_1919_1948_cold + HotWater_cold,
    "1949 - 1978" = SpaceHeat_1949_1978_cold + HotWater_cold,
    "1979 - 1986" = SpaceHeat_1979_1986_cold + HotWater_cold,
    "1987 - 1990" = SpaceHeat_1987_1990_cold + HotWater_cold,
    "1991 - 1995" = SpaceHeat_1991_1995_cold + HotWater_cold,
    "1996 - 2000" = SpaceHeat_1996_2000_cold + HotWater_cold,
    "2001 - 2011" = SpaceHeat_2001_2011_cold + HotWater_cold,
    "2012 - 2022" = SpaceHeat_2012_2022_cold + HotWater_cold,
    "2023 - 2030" = SpaceHeat_2023_2030_cold + HotWater_cold,
  ) %>% select(
    -c(
      SpaceHeat_beginn_1918_cold,
      SpaceHeat_1919_1948_cold,
      SpaceHeat_1949_1978_cold,
      SpaceHeat_1979_1986_cold,
      SpaceHeat_1987_1990_cold,
      SpaceHeat_1991_1995_cold,
      SpaceHeat_1996_2000_cold,
      SpaceHeat_2001_2011_cold,
      SpaceHeat_2012_2022_cold,
      SpaceHeat_2023_2030_cold,
      HotWater_cold
    )
  ) %>% gather("Year of construction",
               "Heat demand in kWh",
               2:11) %>% mutate("Year" = 2010)

mh_combined_heat_demand_hot <-
  read_csv2("data/output/heatdemand/mh_combined_heat_demand_hot.csv") %>% mutate(
    "Beginn - 1918" = SpaceHeat_beginn_1918_hot + HotWater_hot,
    "1919 - 1948" = SpaceHeat_1919_1948_hot + HotWater_hot,
    "1949 - 1978" = SpaceHeat_1949_1978_hot + HotWater_hot,
    "1979 - 1986" = SpaceHeat_1979_1986_hot + HotWater_hot,
    "1987 - 1990" = SpaceHeat_1987_1990_hot + HotWater_hot,
    "1991 - 1995" = SpaceHeat_1991_1995_hot + HotWater_hot,
    "1996 - 2000" = SpaceHeat_1996_2000_hot + HotWater_hot,
    "2001 - 2011" = SpaceHeat_2001_2011_hot + HotWater_hot,
    "2012 - 2022" = SpaceHeat_2012_2022_hot + HotWater_hot,
    "2023 - 2030" = SpaceHeat_2023_2030_hot + HotWater_hot,
  ) %>% select(
    -c(
      SpaceHeat_beginn_1918_hot,
      SpaceHeat_1919_1948_hot,
      SpaceHeat_1949_1978_hot,
      SpaceHeat_1979_1986_hot,
      SpaceHeat_1987_1990_hot,
      SpaceHeat_1991_1995_hot,
      SpaceHeat_1996_2000_hot,
      SpaceHeat_2001_2011_hot,
      SpaceHeat_2012_2022_hot,
      SpaceHeat_2023_2030_hot,
      HotWater_hot
    )
  ) %>% gather("Year of construction",
               "Heat demand in kWh",
               2:11) %>% mutate("Year" = 2022)


# Get transformed data for annual heat demand plot
eh_combined_heat_demand <- eh_combined_heat_demand_reference %>%
  mutate(
    "Building type" = "Standalone house",
    `Year of construction` = fct_recode(`Year of construction`,
                                        "Before 1919" = "Beginn - 1918")
  )

mh_combined_heat_demand <- mh_combined_heat_demand_reference %>%
  mutate(
    "Building type" = "Apartment building",
    `Year of construction` = fct_recode(`Year of construction`,
                                        "Before 1919" = "Beginn - 1918")
  )

combined_heat_demand <- eh_combined_heat_demand %>%
  rbind(mh_combined_heat_demand)


# Get heat demand for standalone houses and apartment buildings houses with year of construction 1991 - 1995
eh_combined_heat_demand_1991_1995 <-
  eh_combined_heat_demand_cold %>%
  rbind(eh_combined_heat_demand_reference) %>%
  rbind(eh_combined_heat_demand_hot) %>%
  filter(`Year of construction` == "1991 - 1995") %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(Time, 4, 5) == "00", 0, sub("^0+", "", substr(Time, 4, 5))),
    ifelse(substr(Time, 1, 2) == "00", 0, sub("^0+", "", substr(Time, 1, 2))),
  ),
  Year = as.factor(Year)) %>%
  select(-c("Time"))

eh_combined_heat_demand_1991_1995 <-
  eh_combined_heat_demand_1991_1995 %>%
  group_by(`Year of construction`,
           Year,
           DATEISO) %>% summarise(`Heat demand in kWh` = sum(`Heat demand in kWh`),
                                  .groups = 'drop')


mh_combined_heat_demand_1991_1995 <-
  mh_combined_heat_demand_cold %>%
  rbind(mh_combined_heat_demand_reference) %>%
  rbind(mh_combined_heat_demand_hot) %>%
  filter(`Year of construction` == "1991 - 1995") %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(Time, 4, 5) == "00", 0, sub("^0+", "", substr(Time, 4, 5))),
    ifelse(substr(Time, 1, 2) == "00", 0, sub("^0+", "", substr(Time, 1, 2))),
  ),
  Year = as.factor(Year)) %>%
  select(-c("Time"))

mh_combined_heat_demand_1991_1995 <-
  mh_combined_heat_demand_1991_1995 %>%
  group_by(`Year of construction`,
           Year,
           DATEISO) %>% summarise(`Heat demand in kWh` = sum(`Heat demand in kWh`),
                                  .groups = 'drop')


# Plot the annual heat demand for standalone houses and apartment buildings
heat_demand_plot <- ggplot(combined_heat_demand,
                           aes(x = `Heat demand in kWh`,
                               y = factor(
                                 `Year of construction`,
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

                               ))) +
  geom_bar(stat = "identity") +
  facet_grid(`Building type` ~ .) +
  labs(x = expression("Annual heat demand in kWh per m" ^
                        2),
       y = "Year of construction")


heat_demand_plot


# Plot heat demand for standalone houses and apartment buildings houses with year of construction 1991 - 1995
heat_demand_eh_1991_1995_plot <-
  ggplot(data = eh_combined_heat_demand_1991_1995, aes(DATEISO, `Heat demand in kWh`, color =
                                                         Year)) +
  geom_line(lwd = 1.0) + ylab(expression("Daily heat demand in kWh per m" ^
                                           2)) + xlab("Date") +
  scale_color_brewer(palette = "Set2") +
  guides(color = guide_legend(title = "Annual temperature series")) +
  coord_cartesian(ylim = c(0, 1.5))

heat_demand_eh_1991_1995_plot


heat_demand_mh_1991_1995_plot <-
  ggplot(data = mh_combined_heat_demand_1991_1995, aes(DATEISO, `Heat demand in kWh`, color =
                                                         Year)) +
  geom_line(lwd = 1.0) + ylab(expression("Daily heat demand in kWh per m" ^
                                           2)) + xlab("Date") +
  scale_color_brewer(palette = "Set2") +
  guides(color = guide_legend(title = "Annual temperature series")) +
  coord_cartesian(ylim = c(0, 1.5))

heat_demand_mh_1991_1995_plot


# Save the plots
ggsave(
  "plots/output/heatdemand/heat_demand_plot.png",
  heat_demand_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/heatdemand/heat_demand_eh_1991_1995_plot.png",
  heat_demand_eh_1991_1995_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/heatdemand/heat_demand_mh_1991_1995_plot.png",
  heat_demand_mh_1991_1995_plot,
  width = 30,
  units = "cm"
)
