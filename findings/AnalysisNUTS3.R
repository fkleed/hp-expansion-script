# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)
library(psych)
library(rstatix)

# Read the data
nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx") %>%
  mutate(
    NUTS3Type = fct_recode(
      NUTS3Type,
      "District" = "Kreis",
      "Urban district" = "Kreisfreie Stadt",
      "Urban district" = "Stadtkreis",
      "Rural district" = "Landkreis",
      "Regional association" = "Regionalverband"
    ),
    NUTS3Name = paste(gsub(",.*", "", NUTS3Name), NUTS3Type, sep = ", ")
  ) %>%
  select(c("NUTS3Code", "NUTS3Name"))


regions_electricity_demand_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_reference_iso.csv"
  )

regions_electricity_demand_cold_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_cold_iso.csv"
  )

regions_electricity_demand_hot_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_hot_iso.csv"
  )


regions_electricity_demand_space_heat_only_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_reference_iso.csv"
  )

regions_electricity_demand_space_heat_only_cold_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_cold_iso.csv"
  )

regions_electricity_demand_space_heat_only_hot_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_hot_iso.csv"
  )


# Transform data

# Space heat and hot water
regions_electricity_demand_cold <-
  regions_electricity_demand_cold_iso %>%
  mutate("Year" = 2010)

regions_electricity_demand_reference <-
  regions_electricity_demand_reference_iso %>%
  mutate("Year" = 2017)

regions_electricity_demand_hot <-
  regions_electricity_demand_hot_iso %>%
  mutate("Year" = 2022)

electricity_demand_nuts3_sh_and_hw <-
  regions_electricity_demand_cold %>%
  rbind(regions_electricity_demand_reference) %>%
  rbind(regions_electricity_demand_hot) %>%
  mutate("Case" = "Space heating and hot water together")

annual_electricity_demand_nuts3_sh_and_hw <-
  electricity_demand_nuts3_sh_and_hw %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code, Year, Case) %>%
  summarise(
    hourly_electricity_demand = sum(hourly_electricity_demand),
    .groups = "drop"
  )

max_hourly_electricity_demand_nuts3_sh_and_hw <-
  electricity_demand_nuts3_sh_and_hw %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code, Year, Case) %>%
  summarise(
    hourly_electricity_demand = max(hourly_electricity_demand),
    .groups = "drop"
  )

# space heat only
regions_electricity_demand_cold_space_heat_only <-
  regions_electricity_demand_space_heat_only_cold_iso %>%
  mutate("Year" = 2010)

regions_electricity_demand_reference_space_heat_only <-
  regions_electricity_demand_space_heat_only_reference_iso %>%
  mutate("Year" = 2017)

regions_electricity_demand_hot_space_heat_only <-
  regions_electricity_demand_space_heat_only_hot_iso %>%
  mutate("Year" = 2022)

electricity_demand_nuts3_sh_only <-
  regions_electricity_demand_cold_space_heat_only %>%
  rbind(regions_electricity_demand_reference_space_heat_only) %>%
  rbind(regions_electricity_demand_hot_space_heat_only) %>%
  mutate("Case" = "Space heating only")

annual_electricity_demand_nuts3_sh_only <-
  electricity_demand_nuts3_sh_only %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code, Year, Case) %>%
  summarise(
    hourly_electricity_demand = sum(hourly_electricity_demand),
    .groups = "drop"
  )

max_hourly_electricity_demand_nuts3_sh_only <-
  electricity_demand_nuts3_sh_only %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code, Year, Case) %>%
  summarise(
    hourly_electricity_demand = max(hourly_electricity_demand),
    .groups = "drop"
  )


# Performing RMSD
annual_electricity_demand_nuts3 <-
  annual_electricity_demand_nuts3_sh_and_hw %>%
  rbind(annual_electricity_demand_nuts3_sh_only) %>%
  filter(Year != "2017") %>%
  spread(key = Year, value = hourly_electricity_demand)

annual_electricity_demand_nuts3 <-
  annual_electricity_demand_nuts3 %>%
  mutate("Difference-2010-2022" = `2010` - `2022`) %>%
  select(-c("2010", "2022"))

annual_electricity_demand_nuts3 <-
  annual_electricity_demand_nuts3 %>%
  spread(key = Case, value = `Difference-2010-2022`)

annual_electricity_demand_nuts3 <-
  annual_electricity_demand_nuts3 %>%
  mutate("QuadraticDifference" = (`Space heating and hot water together` - `Space heating only`) ^
           2)

rmsd <-
  sqrt(sum(annual_electricity_demand_nuts3$QuadraticDifference) / 400)

rmsd


# Performing Anova
max_hourly_electricity_demand_nuts3 <-
  max_hourly_electricity_demand_nuts3_sh_and_hw %>%
  rbind(max_hourly_electricity_demand_nuts3_sh_only)

max_hourly_electricity_demand_nuts3_anova <-
  max_hourly_electricity_demand_nuts3 %>%
  mutate(
    AnovaCase = paste(Year, Case, " ")
  ) %>%
  select(
    -c(
      "nuts3_code",
      "Year",
      "Case"
    )
  ) %>%
  mutate_if(is.character, as.factor)


summary(max_hourly_electricity_demand_nuts3_anova)

anova_training <- aov(max_hourly_electricity_demand_nuts3_anova$hourly_electricity_demand ~ max_hourly_electricity_demand_nuts3_anova$AnovaCase)
summary(anova_training)

# Normal distribution of residuals
describeBy(max_hourly_electricity_demand_nuts3_anova$hourly_electricity_demand, max_hourly_electricity_demand_nuts3_anova$AnovaCase)
hist(rstandard((anova_training)))
plot(anova_training, 2)

# Post-hoc-Tests
pairwise.t.test(max_hourly_electricity_demand_nuts3_anova$hourly_electricity_demand, max_hourly_electricity_demand_nuts3_anova$AnovaCase, p.adjust.method = "bonferroni")

max_hourly_electricity_demand_nuts3_anova %>%
  cohens_d(hourly_electricity_demand ~ AnovaCase) %>%
  as.data.frame()


# Anova only for sh and hot water
max_hourly_electricity_demand_nuts3_anova_sh_and_hw <-
  max_hourly_electricity_demand_nuts3_sh_and_hw %>%
  mutate(
    AnovaCase = paste(Year, Case, " ")
  ) %>%
  select(
    -c(
      "nuts3_code",
      "Year",
      "Case"
    )
  ) %>%
  mutate_if(is.character, as.factor)


summary(max_hourly_electricity_demand_nuts3_anova_sh_and_hw)

anova_training_sh_and_hw <- aov(max_hourly_electricity_demand_nuts3_anova_sh_and_hw$hourly_electricity_demand ~ max_hourly_electricity_demand_nuts3_anova_sh_and_hw$AnovaCase)

summary(anova_training_sh_and_hw)

pairwise.t.test(max_hourly_electricity_demand_nuts3_anova_sh_and_hw$hourly_electricity_demand, max_hourly_electricity_demand_nuts3_anova_sh_and_hw$AnovaCase, p.adjust.method = "bonferroni")


# Anova only for sh only
max_hourly_electricity_demand_nuts3_anova_sh_only <-
  max_hourly_electricity_demand_nuts3_sh_only %>%
  mutate(
    AnovaCase = paste(Year, Case, " ")
  ) %>%
  select(
    -c(
      "nuts3_code",
      "Year",
      "Case"
    )
  ) %>%
  mutate_if(is.character, as.factor)


summary(max_hourly_electricity_demand_nuts3_anova_sh_only)

anova_training_sh_only <- aov(max_hourly_electricity_demand_nuts3_anova_sh_only$hourly_electricity_demand ~ max_hourly_electricity_demand_nuts3_anova_sh_only$AnovaCase)

summary(anova_training_sh_only)

pairwise.t.test(max_hourly_electricity_demand_nuts3_anova_sh_only$hourly_electricity_demand, max_hourly_electricity_demand_nuts3_anova_sh_only$AnovaCase, p.adjust.method = "bonferroni")


