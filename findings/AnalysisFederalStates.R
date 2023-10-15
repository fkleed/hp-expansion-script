# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)
library(psych)

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
  read_csv(
    "data/output/findings/electricity-demand-federal-states/electricity_demand_reference_federal_states_sh_and_hw.csv"
  )

electricity_demand_cold_federal_states_sh_and_hw <-
  read_csv(
    "data/output/findings/electricity-demand-federal-states/electricity_demand_cold_federal_states_sh_and_hw.csv"
  )

electricity_demand_hot_federal_states_sh_and_hw <-
  read_csv(
    "data/output/findings/electricity-demand-federal-states/electricity_demand_hot_federal_states_sh_and_hw.csv"
  )


electricity_demand_reference_federal_states_sh_only <-
  read_csv(
    "data/output/findings/electricity-demand-federal-states/electricity_demand_reference_federal_states_sh_only.csv"
  )

electricity_demand_cold_federal_states_sh_only <-
  read_csv(
    "data/output/findings/electricity-demand-federal-states/electricity_demand_cold_federal_states_sh_only.csv"
  )

electricity_demand_hot_federal_states_sh_only <-
  read_csv(
    "data/output/findings/electricity-demand-federal-states/electricity_demand_hot_federal_states_sh_only.csv"
  )


# Check the annual electricity  demand
sum(electricity_demand_cold_federal_states_sh_and_hw$hourly_electricity_demand)
sum(
  electricity_demand_reference_federal_states_sh_and_hw$hourly_electricity_demand
)
sum(electricity_demand_hot_federal_states_sh_and_hw$hourly_electricity_demand)

sum(electricity_demand_cold_federal_states_sh_only$hourly_electricity_demand)
sum(electricity_demand_reference_federal_states_sh_only$hourly_electricity_demand)
sum(electricity_demand_hot_federal_states_sh_only$hourly_electricity_demand)


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
  left_join(nuts3regioninfo,
            by = c("nuts1_code" = "NUTS1Code")) %>%
  mutate("Case" = "Space heating and hot water together")

maximum_hourly_electricity_demand_federal_states_sh_and_hw <-
  electricity_demand_federal_states_sh_and_hw %>%
  select(-c("date_iso")) %>%
  group_by(nuts1_code, Year, NUTS1Name, Case) %>%
  summarise(
    hourly_electricity_demand = max(hourly_electricity_demand),
    .groups = "drop"
  )


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
  left_join(nuts3regioninfo,
            by = c("nuts1_code" = "NUTS1Code")) %>%
  mutate("Case" = "Space heating only")

maximum_hourly_electricity_demand_federal_states_sh_only <-
  electricity_demand_federal_states_sh_only %>%
  select(-c("date_iso")) %>%
  group_by(nuts1_code, Year, NUTS1Name, Case) %>%
  summarise(
    hourly_electricity_demand = max(hourly_electricity_demand),
    .groups = "drop"
  )


electricity_demand_federal_states <-
  electricity_demand_federal_states_sh_and_hw %>%
  rbind(electricity_demand_federal_states_sh_only)

maximum_hourly_electricity_demand_federal_states <-
  maximum_hourly_electricity_demand_federal_states_sh_and_hw %>%
  rbind(maximum_hourly_electricity_demand_federal_states_sh_only)


# Performing RMSD analysis for annual electricity demand difference between sh and hw and ah only for the difference between the cold and hot temperature series
annual_electricity_demand_federal_states <-
  electricity_demand_federal_states %>%
  select(-c("date_iso")) %>%
  group_by(nuts1_code, Year, NUTS1Name, Case) %>%
  summarise(
    hourly_electricity_demand  = sum(hourly_electricity_demand),
    .groups = "drop"
  ) %>%
  filter(Year != "2017") %>%
  spread(key = Year, value = hourly_electricity_demand)

annual_electricity_demand_federal_states <-
  annual_electricity_demand_federal_states %>%
  mutate("Difference-2010-2022" = `2010` - `2022`) %>%
  select(-c("2010", "2022"))

annual_electricity_demand_federal_states <-
  annual_electricity_demand_federal_states %>%
  spread(key = Case, value = `Difference-2010-2022`)

annual_electricity_demand_federal_states <-
  annual_electricity_demand_federal_states %>%
  mutate("QuadraticDifference" = (`Space heating and hot water together` - `Space heating only`) ^
           2)


rmsd <-
  sqrt(sum(
    annual_electricity_demand_federal_states$QuadraticDifference
  ) / 16)

rmsd

test <- filter(annual_electricity_demand_federal_states, Year == "2017" & Case == "Space heating and hot water together")
test2 <- filter(maximum_hourly_electricity_demand_federal_states, Year == "2017" & Case == "Space heating and hot water together")

# Performing anova for maximum hourly electricity demand
maximum_hourly_electricity_demand_anova_data <-
  maximum_hourly_electricity_demand_federal_states %>%
  mutate(
    AnovaCase = paste(Year, Case, " ")
  ) %>%
  select(
    -c(
      "nuts1_code",
      "Year",
      "NUTS1Name",
      "Case"
    )
  ) %>%
  mutate_if(is.character, as.factor)

summary(maximum_hourly_electricity_demand_anova_data)

anova_training <- aov(maximum_hourly_electricity_demand_anova_data$hourly_electricity_demand ~ maximum_hourly_electricity_demand_anova_data$AnovaCase)
summary(anova_training)

pairwise.t.test(maximum_hourly_electricity_demand_anova_data$hourly_electricity_demand, maximum_hourly_electricity_demand_anova_data$AnovaCase, p.adjust.method = "bonferroni")

# Normal distribution of residuals
describeBy(maximum_hourly_electricity_demand_anova_data$hourly_electricity_demand, maximum_hourly_electricity_demand_anova_data$AnovaCase)
hist(rstandard((anova_training)))
plot(anova_training, 2)
