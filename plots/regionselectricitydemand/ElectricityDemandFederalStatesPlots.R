# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)

# Read the data
nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx") %>%
  select(c("NUTS1Name",
           "NUTS1Code")) %>%
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
  ) %>%
  distinct()

electricity_demand_reference_federal_states_sh_and_hw <-
  read_csv("data/output/findings/electricity-demand-federal-states/electricity_demand_reference_federal_states_sh_and_hw.csv")

electricity_demand_cold_federal_states_sh_and_hw <-
  read_csv("data/output/findings/electricity-demand-federal-states/electricity_demand_cold_federal_states_sh_and_hw.csv")

electricity_demand_hot_federal_states_sh_and_hw <-
  read_csv("data/output/findings/electricity-demand-federal-states/electricity_demand_hot_federal_states_sh_and_hw.csv")


electricity_demand_reference_federal_states_sh_only <-
  read_csv("data/output/findings/electricity-demand-federal-states/electricity_demand_reference_federal_states_sh_only.csv")

electricity_demand_cold_federal_states_sh_only <-
  read_csv("data/output/findings/electricity-demand-federal-states/electricity_demand_cold_federal_states_sh_only.csv")

electricity_demand_hot_federal_states_sh_only <-
  read_csv("data/output/findings/electricity-demand-federal-states/electricity_demand_hot_federal_states_sh_only.csv")


# Check the annual electricity  demand
sum(electricity_demand_cold_federal_states_sh_and_hw$hourly_electricity_demand)
sum(electricity_demand_reference_federal_states_sh_and_hw$hourly_electricity_demand)
sum(electricity_demand_hot_federal_states_sh_and_hw$hourly_electricity_demand)

sum(electricity_demand_cold_federal_states_sh_only$hourly_electricity_demand)
sum(electricity_demand_reference_federal_states_sh_only$hourly_electricity_demand)
sum(electricity_demand_hot_federal_states_sh_only$hourly_electricity_demand)


# Plot the bar charts for the annual and maximum hourly electricity consumption per federal state

# Space heating and hot water
electricity_demand_cold_federal_states_sh_and_hw <-
  electricity_demand_cold_federal_states_sh_and_hw %>%
  mutate("Year" = 2010)

electricity_demand_reference_federal_states_sh_and_hw <-
  electricity_demand_reference_federal_states_sh_and_hw %>%
  mutate("Year" = 2017)

electricity_demand_hot_federal_states_sh_and_hw <-
  electricity_demand_hot_federal_states_sh_and_hw %>%
  mutate("Year" = 2022)

electricity_demand_federal_states_sh_and_hw <-
  electricity_demand_cold_federal_states_sh_and_hw %>%
  rbind(electricity_demand_reference_federal_states_sh_and_hw) %>%
  rbind(electricity_demand_hot_federal_states_sh_and_hw)

electricity_demand_federal_states_sh_and_hw <-
  electricity_demand_federal_states_sh_and_hw %>%
  left_join(
    nuts3regioninfo,
    by = c("nuts1_code" = "NUTS1Code")
  )


# Annual hp electricity demand
federal_states_annual_electricity_demand_sh_and_hw_plot <-
  ggplot(data = electricity_demand_federal_states_sh_and_hw,
         aes(
           x = hourly_electricity_demand / 1000000000,
           y = reorder(NUTS1Name,-hourly_electricity_demand)
         )) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ Year, ncol = 1) +
  labs(x = "Electricity demand in TWh",
       y = "Federal state") +
  coord_cartesian(xlim = c(0, 16))

federal_states_annual_electricity_demand_sh_and_hw_plot


# Maximum hourly hp electricity demand
maximum_hourly_electricity_demand_federal_states_sh_and_hw <-
  electricity_demand_federal_states_sh_and_hw %>%
  select(-c("date_iso")) %>%
  group_by(nuts1_code, Year, NUTS1Name) %>%
  summarise(hourly_electricity_demand = max(hourly_electricity_demand), .groups = "drop")

maximum_hourly_electricity_demand_federal_states_sh_and_hw_plot <-
  ggplot(data = maximum_hourly_electricity_demand_federal_states_sh_and_hw,
         aes(
           x = hourly_electricity_demand / 1000000,
           y = reorder(NUTS1Name,-hourly_electricity_demand)
         )) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ Year, ncol = 1) +
  labs(x = "Electricity demand in GWh",
       y = "Federal state") +
  coord_cartesian(xlim = c(0, 12))

maximum_hourly_electricity_demand_federal_states_sh_and_hw_plot


# Space heating only
electricity_demand_cold_federal_states_sh_only <-
  electricity_demand_cold_federal_states_sh_only %>%
  mutate("Year" = 2010)

electricity_demand_reference_federal_states_sh_only <-
  electricity_demand_reference_federal_states_sh_only %>%
  mutate("Year" = 2017)

electricity_demand_hot_federal_states_sh_only <-
  electricity_demand_hot_federal_states_sh_only %>%
  mutate("Year" = 2022)

electricity_demand_federal_states_sh_only <-
  electricity_demand_cold_federal_states_sh_only %>%
  rbind(electricity_demand_reference_federal_states_sh_only) %>%
  rbind(electricity_demand_hot_federal_states_sh_only)

electricity_demand_federal_states_sh_only <-
  electricity_demand_federal_states_sh_only %>%
  left_join(
    nuts3regioninfo,
    by = c("nuts1_code" = "NUTS1Code")
  )


# Annual hp electricity demand
federal_states_annual_electricity_demand_sh_only_plot <-
  ggplot(data = electricity_demand_federal_states_sh_only,
         aes(
           x = hourly_electricity_demand / 1000000000,
           y = reorder(NUTS1Name,-hourly_electricity_demand)
         )) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ Year, ncol = 1) +
  labs(x = "Electricity demand in TWh",
       y = "Federal state") +
  coord_cartesian(xlim = c(0, 16))

federal_states_annual_electricity_demand_sh_only_plot


# Maximum hourly hp electricity demand
maximum_hourly_electricity_demand_federal_states_sh_only <-
  electricity_demand_federal_states_sh_only %>%
  select(-c("date_iso")) %>%
  group_by(nuts1_code, Year, NUTS1Name) %>%
  summarise(hourly_electricity_demand = max(hourly_electricity_demand), .groups = "drop")

maximum_hourly_electricity_demand_federal_states_sh_only_plot <-
  ggplot(data = maximum_hourly_electricity_demand_federal_states_sh_only,
         aes(
           x = hourly_electricity_demand / 1000000,
           y = reorder(NUTS1Name,-hourly_electricity_demand)
         )) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ Year, ncol = 1) +
  labs(x = "Electricity demand in GWh",
       y = "Federal state") +
  coord_cartesian(xlim = c(0, 12))

maximum_hourly_electricity_demand_federal_states_sh_only_plot


# Save the plots
ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/federal_states_annual_electricity_demand_sh_and_hw_plot.png",
  federal_states_annual_electricity_demand_sh_and_hw_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/maximum_hourly_electricity_demand_federal_states_sh_and_hw_plot.png",
  maximum_hourly_electricity_demand_federal_states_sh_and_hw_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/federal_states_annual_electricity_demand_sh_only_plot.png",
  federal_states_annual_electricity_demand_sh_only_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandfederalstates/maximum_hourly_electricity_demand_federal_states_sh_only_plot.png",
  maximum_hourly_electricity_demand_federal_states_sh_only_plot,
  width = 30,
  units = "cm"
)


