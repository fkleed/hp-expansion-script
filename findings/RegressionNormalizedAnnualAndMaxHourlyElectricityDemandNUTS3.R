# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)
library(zoo)
library(lmtest)
library(car)
library(sandwich)


# Read data
nuts3_regression_data <-
  read_csv("data/output/findings/regressiondata/nuts3_regression_data.csv")

building_stock_2030_with_hp_distribution <-
  read_csv("data/output/heatpumpexpansion/building_stock_2030_with_hp_distribution.csv")

regions_electricity_demand_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_reference_iso.csv"
  )

regions_electricity_demand_space_heat_only_reference_iso <-
  read_csv(
    "data/output/findings/electricity-demand-nuts3/regions_electricity_demand_space_heat_only_reference_iso.csv"
  )


# Calculate the hp sum per NUTS 3 region
building_stock_2030_with_hp_distribution_aggregated <-
  building_stock_2030_with_hp_distribution %>%
  mutate("HPSum" = HPAmountAir + HPAmountProbe + HPAmountCollector) %>%
  select("NUTS3Code",
         "HPSum") %>%
  group_by(NUTS3Code) %>%
  summarise("HPSum" = sum(HPSum),
            .groups = "drop")



# Regression analysis for the annual electricity demand per heat pump

# Calculate the annual electricity demand per hp

# Space heat and hot water
regions_annual_electricity_demand_per_hp_reference_sh_and_hw <-
  regions_electricity_demand_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise(
    annual_electricity_demand = sum(hourly_electricity_demand),
    .groups = "drop"
  ) %>%
  left_join(
    building_stock_2030_with_hp_distribution_aggregated,
    by = c("nuts3_code" = "NUTS3Code")
  ) %>%
  mutate(annual_electricity_demand_per_hp = annual_electricity_demand / HPSum) %>%
  left_join(nuts3_regression_data,
            by = c("nuts3_code" = "NUTS3Code")) %>%
  select(
    c(
      "nuts3_code",
      "annual_electricity_demand_per_hp",
      "SumBuildingCountNUTS3",
      "NUTS3Name",
      "NUTS3Type",
      "ShareApartmentBuildings",
      "ShareOldBuildings"
    )
  )

summary(regions_annual_electricity_demand_per_hp_reference_sh_and_hw)

# Space heat only
regions_annual_electricity_demand_per_hp_reference_sh_only <-
  regions_electricity_demand_space_heat_only_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise(
    annual_electricity_demand = sum(hourly_electricity_demand),
    .groups = "drop"
  ) %>%
  left_join(
    building_stock_2030_with_hp_distribution_aggregated,
    by = c("nuts3_code" = "NUTS3Code")
  ) %>%
  mutate(annual_electricity_demand_per_hp = annual_electricity_demand / HPSum) %>%
  left_join(nuts3_regression_data,
            by = c("nuts3_code" = "NUTS3Code")) %>%
  select(
    c(
      "nuts3_code",
      "annual_electricity_demand_per_hp",
      "SumBuildingCountNUTS3",
      "NUTS3Name",
      "NUTS3Type",
      "ShareApartmentBuildings",
      "ShareOldBuildings"
    )
  )

summary(regions_annual_electricity_demand_per_hp_reference_sh_only)


# Calculate the regression

# Space heat and hot water
model_annual_electricity_demand_per_hp_sh_and_hw <-
  lm(
    annual_electricity_demand_per_hp ~ ShareApartmentBuildings + ShareOldBuildings,
    data = regions_annual_electricity_demand_per_hp_reference_sh_and_hw
  )

summary(model_annual_electricity_demand_per_hp_sh_and_hw)


# Check prerequisites

# 1 Normal distribution residuals
plot(model_annual_electricity_demand_per_hp_sh_and_hw, 2)

# 2 a Homoscedasticity
plot(model_annual_electricity_demand_per_hp_sh_and_hw, 1)

bptest(model_annual_electricity_demand_per_hp_sh_and_hw)

# 2 b Autocorrelation
# Also fixed by Homoscedasticity violation

# 3 No Multicollinearity good when under 10, great when under 2
vif(model_annual_electricity_demand_per_hp_sh_and_hw)

# No influential cases
plot(model_annual_electricity_demand_per_hp_sh_and_hw, 4)


# No Homoscedasticity
# Robust standard errors
coeftest(
  model_annual_electricity_demand_per_hp_sh_and_hw,
  vcov = vcovHC(model_annual_electricity_demand_per_hp_sh_and_hw, type = "HC4")
)

# Standardization
zmodel_annual_electricity_demand_per_hp_sh_and_hw <- lm(
  scale(annual_electricity_demand_per_hp) ~ scale(ShareApartmentBuildings) + scale(ShareOldBuildings),
  data = regions_annual_electricity_demand_per_hp_reference_sh_and_hw

)

summary(zmodel_annual_electricity_demand_per_hp_sh_and_hw)


# Space heat only
model_annual_electricity_demand_per_hp_sh_only <-
  lm(
    annual_electricity_demand_per_hp ~ ShareApartmentBuildings + ShareOldBuildings,
    data = regions_annual_electricity_demand_per_hp_reference_sh_only
  )

summary(model_annual_electricity_demand_per_hp_sh_only)


# Check prerequisites

# 1 Normal distribution residuals
plot(model_annual_electricity_demand_per_hp_sh_only, 2)

# 2 a Homoscedasticity
plot(model_annual_electricity_demand_per_hp_sh_only, 1)

bptest(model_annual_electricity_demand_per_hp_sh_only)

# 2 b Autocorrelation
# Also fixed by Homoscedasticity violation

# 3 No Multicollinearity good when under 10, great when under 2
vif(model_annual_electricity_demand_per_hp_sh_only)

# No influential cases
plot(model_annual_electricity_demand_per_hp_sh_only, 4)


# No Homoscedasticity
# Robust standard errors
coeftest(
  model_annual_electricity_demand_per_hp_sh_only,
  vcov = vcovHC(model_annual_electricity_demand_per_hp_sh_only, type = "HC4")
)

# Standardization
zmodel_annual_electricity_demand_per_hp_sh_only <- lm(
  scale(annual_electricity_demand_per_hp) ~ scale(ShareApartmentBuildings) + scale(ShareOldBuildings),
  data = regions_annual_electricity_demand_per_hp_reference_sh_only

)

summary(zmodel_annual_electricity_demand_per_hp_sh_only)



# Regression analysis for the maximum hourly electricity demand per heat pump

# Calculate the maximum hourly electricity demand per hp

# Space heat and hot water
regions_max_hourly_electricity_demand_per_hp_reference_sh_and_hw <-
  regions_electricity_demand_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise(
    max_hourly_electricity_demand = max(hourly_electricity_demand),
    .groups = "drop"
  ) %>%
  left_join(
    building_stock_2030_with_hp_distribution_aggregated,
    by = c("nuts3_code" = "NUTS3Code")
  ) %>%
  mutate(max_hourly_electricity_demand_per_hp = max_hourly_electricity_demand / HPSum) %>%
  left_join(nuts3_regression_data,
            by = c("nuts3_code" = "NUTS3Code")) %>%
  select(
    c(
      "nuts3_code",
      "max_hourly_electricity_demand_per_hp",
      "SumBuildingCountNUTS3",
      "NUTS3Name",
      "NUTS3Type",
      "ShareApartmentBuildings",
      "ShareOldBuildings"
    )
  )

summary(regions_max_hourly_electricity_demand_per_hp_reference_sh_and_hw)

# Space heat only
regions_max_hourly_electricity_demand_per_hp_reference_sh_only <-
  regions_electricity_demand_space_heat_only_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise(
    max_hourly_electricity_demand = max(hourly_electricity_demand),
    .groups = "drop"
  ) %>%
  left_join(
    building_stock_2030_with_hp_distribution_aggregated,
    by = c("nuts3_code" = "NUTS3Code")
  ) %>%
  mutate(max_hourly_electricity_demand_per_hp = max_hourly_electricity_demand / HPSum) %>%
  left_join(nuts3_regression_data,
            by = c("nuts3_code" = "NUTS3Code")) %>%
  select(
    c(
      "nuts3_code",
      "max_hourly_electricity_demand_per_hp",
      "SumBuildingCountNUTS3",
      "NUTS3Name",
      "NUTS3Type",
      "ShareApartmentBuildings",
      "ShareOldBuildings"
    )
  )

summary(regions_max_hourly_electricity_demand_per_hp_reference_sh_only)


# Calculate the regression

# Space heat and hot water
model_max_hourly_electricity_demand_per_hp_sh_and_hw <-
  lm(
    max_hourly_electricity_demand_per_hp ~ ShareApartmentBuildings + ShareOldBuildings,
    data = regions_max_hourly_electricity_demand_per_hp_reference_sh_and_hw
  )

summary(model_max_hourly_electricity_demand_per_hp_sh_and_hw)


# Check prerequisites

# 1 Normal distribution residuals
plot(model_max_hourly_electricity_demand_per_hp_sh_and_hw, 2)

# 2 a Homoscedasticity
plot(model_max_hourly_electricity_demand_per_hp_sh_and_hw, 1)

bptest(model_max_hourly_electricity_demand_per_hp_sh_and_hw)

# 2 b Autocorrelation
# Also fixed by Homoscedasticity violation

# 3 No Multicollinearity good when under 10, great when under 2
vif(model_max_hourly_electricity_demand_per_hp_sh_and_hw)

# No influential cases
plot(model_max_hourly_electricity_demand_per_hp_sh_and_hw, 4)


# No Homoscedasticity
# Robust standard errors
coeftest(
  model_max_hourly_electricity_demand_per_hp_sh_and_hw,
  vcov = vcovHC(model_max_hourly_electricity_demand_per_hp_sh_and_hw, type = "HC4")
)

# Standardization
zmodel_max_hourly_electricity_demand_per_hp_sh_and_hw <- lm(
  scale(max_hourly_electricity_demand_per_hp) ~ scale(ShareApartmentBuildings) + scale(ShareOldBuildings),
  data = regions_max_hourly_electricity_demand_per_hp_reference_sh_and_hw

)

summary(zmodel_max_hourly_electricity_demand_per_hp_sh_and_hw)


# Space heat only
model_max_hourly_electricity_demand_per_hp_sh_only <-
  lm(
    max_hourly_electricity_demand_per_hp ~ ShareApartmentBuildings + ShareOldBuildings,
    data = regions_max_hourly_electricity_demand_per_hp_reference_sh_only
  )

summary(model_max_hourly_electricity_demand_per_hp_sh_only)


# Check prerequisites

# 1 Normal distribution residuals
plot(model_max_hourly_electricity_demand_per_hp_sh_only, 2)

# 2 a Homoscedasticity
plot(model_max_hourly_electricity_demand_per_hp_sh_only, 1)

bptest(model_max_hourly_electricity_demand_per_hp_sh_only)

# 2 b Autocorrelation
# Also fixed by Homoscedasticity violation

# 3 No Multicollinearity good when under 10, great when under 2
vif(model_max_hourly_electricity_demand_per_hp_sh_only)

# No influential cases
plot(model_max_hourly_electricity_demand_per_hp_sh_only, 4)


# No Homoscedasticity
# Robust standard errors
coeftest(
  model_max_hourly_electricity_demand_per_hp_sh_only,
  vcov = vcovHC(model_max_hourly_electricity_demand_per_hp_sh_only, type = "HC4")
)

# Standardization
zmodel_max_hourly_electricity_demand_per_hp_sh_only <- lm(
  scale(max_hourly_electricity_demand_per_hp) ~ scale(ShareApartmentBuildings) + scale(ShareOldBuildings),
  data = regions_max_hourly_electricity_demand_per_hp_reference_sh_only

)

summary(zmodel_max_hourly_electricity_demand_per_hp_sh_only)
