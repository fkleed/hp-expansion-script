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


air_temp_2017 <-
  read_csv2("data/output/weathermodel/year2017.csv") %>%
  mutate(
    "TemperatureKelvin" = as.numeric(RoundedMeanTemperature) + 273.15
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(Date, 1, 2) == "00", 0, sub("^0+", "", substr(Date, 1, 2))),
    ifelse(substr(Date, 3, 4) == "00", 0, sub("^0+", "", substr(Date, 3, 4))),
    ifelse(substr(Date, 5, 6) == "00", 0, sub("^0+", "", substr(Date, 5, 6)))
  )) %>%
  select(
    -c("MeanTemperature", "RoundedMeanTemperature", "Date")
  )

air_temp_2010 <-
  read_csv2("data/output/weathermodel/year2010.csv") %>%
  mutate(
    "TemperatureKelvin" = as.numeric(RoundedMeanTemperature) + 273.15
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(Date, 1, 2) == "00", 0, sub("^0+", "", substr(Date, 1, 2))),
    ifelse(substr(Date, 3, 4) == "00", 0, sub("^0+", "", substr(Date, 3, 4))),
    ifelse(substr(Date, 5, 6) == "00", 0, sub("^0+", "", substr(Date, 5, 6)))
  )) %>%
  select(
    -c("MeanTemperature", "RoundedMeanTemperature", "Date")
  )

air_temp_2022 <-
  read_csv2("data/output/weathermodel/year2022.csv") %>%
  mutate(
    "TemperatureKelvin" = as.numeric(RoundedMeanTemperature) + 273.15
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(DATEISO = ISOdate(
    2030,
    ifelse(substr(Date, 1, 2) == "00", 0, sub("^0+", "", substr(Date, 1, 2))),
    ifelse(substr(Date, 3, 4) == "00", 0, sub("^0+", "", substr(Date, 3, 4))),
    ifelse(substr(Date, 5, 6) == "00", 0, sub("^0+", "", substr(Date, 5, 6)))
  )) %>%
  select(
    -c("MeanTemperature", "RoundedMeanTemperature", "Date")
  )



################
#Reference year#
################
# Regression analysis for the hourly electricity demand reference year

# Calculate the regression data

# Space heat and hot water
regions_hourly_electricity_demand_reference_sh_and_hw <-
  regions_electricity_demand_reference_iso %>%
  left_join(nuts3_regression_data, by = c("nuts3_code" = "NUTS3Code")) %>%
  left_join(air_temp_2017, by = c("date_iso" = "DATEISO"))


summary(regions_hourly_electricity_demand_reference_sh_and_hw)

# Space heat only
regions_hourly_electricity_demand_reference_sh_only <-
  regions_electricity_demand_space_heat_only_reference_iso %>%
  left_join(nuts3_regression_data, by = c("nuts3_code" = "NUTS3Code")) %>%
  left_join(air_temp_2017, by = c("date_iso" = "DATEISO"))

summary(regions_hourly_electricity_demand_reference_sh_only)


# Calculate the regression

# Space heat and hot water
model_hourly_electricity_demand_reference_sh_and_hw <-
  lm(
    hourly_electricity_demand ~ SumBuildingCountNUTS3 + ShareApartmentBuildings + ShareOldBuildings + TemperatureKelvin,
    data = regions_hourly_electricity_demand_reference_sh_and_hw
  )

summary(model_hourly_electricity_demand_reference_sh_and_hw)


# Check prerequisites

# 1 Normal distribution residuals
plot(model_hourly_electricity_demand_reference_sh_and_hw, 2)

# 2 a Homoscedasticity
plot(model_hourly_electricity_demand_reference_sh_and_hw, 1)

bptest(model_hourly_electricity_demand_reference_sh_and_hw)

# 2 b Autocorrelation
# Also fixed by Homoscedasticity violation

# 3 No Multicollinearity good when under 10, great when under 2
vif(model_hourly_electricity_demand_reference_sh_and_hw)

# 4 No influential cases
plot(model_hourly_electricity_demand_reference_sh_and_hw, 4)


# No Homoscedasticity
# Robust standard errors
coeftest(
  model_hourly_electricity_demand_reference_sh_and_hw,
  vcov = vcovHC(model_hourly_electricity_demand_reference_sh_and_hw, type = "HC4")
)

# Standardization
zmodel_hourly_electricity_demand_reference_sh_and_hw <- lm(
  scale(hourly_electricity_demand) ~ scale(SumBuildingCountNUTS3) + scale(ShareApartmentBuildings) + scale(ShareOldBuildings) + scale(TemperatureKelvin),
  data = regions_hourly_electricity_demand_reference_sh_and_hw

)

summary(zmodel_hourly_electricity_demand_reference_sh_and_hw)


# Space heat only
model_hourly_electricity_demand_reference_sh_only <-
  lm(
    hourly_electricity_demand ~ SumBuildingCountNUTS3 + ShareApartmentBuildings + ShareOldBuildings + TemperatureKelvin,
    data = regions_hourly_electricity_demand_reference_sh_only
  )

summary(model_hourly_electricity_demand_reference_sh_only)


# Check prerequisites

# 1 Normal distribution residuals
plot(model_hourly_electricity_demand_reference_sh_only, 2)

# 2 a Homoscedasticity
plot(model_hourly_electricity_demand_reference_sh_only, 1)

bptest(model_hourly_electricity_demand_reference_sh_only)

# 2 b Autocorrelation
# Also fixed by Homoscedasticity violation

# 3 No Multicollinearity good when under 10, great when under 2
vif(model_hourly_electricity_demand_reference_sh_only)

# 4 No influential cases
plot(model_hourly_electricity_demand_reference_sh_only, 4)


# No Homoscedasticity
# Robust standard errors
coeftest(
  model_hourly_electricity_demand_reference_sh_only,
  vcov = vcovHC(model_hourly_electricity_demand_reference_sh_only, type = "HC4")
)

# Standardization
zmodel_hourly_electricity_demand_reference_sh_only <- lm(
  scale(hourly_electricity_demand) ~ scale(SumBuildingCountNUTS3) + scale(ShareApartmentBuildings) + scale(ShareOldBuildings) + scale(TemperatureKelvin),
  data = regions_hourly_electricity_demand_reference_sh_only

)

summary(zmodel_hourly_electricity_demand_reference_sh_only)



###########
#Cold year#
###########
# Regression analysis for the hourly electricity demand cold year

# Calculate the regression data

# Space heat and hot water
regions_hourly_electricity_demand_cold_sh_and_hw <-
  regions_electricity_demand_cold_iso %>%
  left_join(nuts3_regression_data, by = c("nuts3_code" = "NUTS3Code")) %>%
  left_join(air_temp_2010, by = c("date_iso" = "DATEISO"))


summary(regions_hourly_electricity_demand_cold_sh_and_hw)

# Space heat only
regions_hourly_electricity_demand_cold_sh_only <-
  regions_electricity_demand_space_heat_only_cold_iso %>%
  left_join(nuts3_regression_data, by = c("nuts3_code" = "NUTS3Code")) %>%
  left_join(air_temp_2010, by = c("date_iso" = "DATEISO"))

summary(regions_hourly_electricity_demand_cold_sh_only)


# Calculate the regression

# Space heat and hot water
model_hourly_electricity_demand_cold_sh_and_hw <-
  lm(
    hourly_electricity_demand ~ SumBuildingCountNUTS3 + ShareApartmentBuildings + ShareOldBuildings + TemperatureKelvin,
    data = regions_hourly_electricity_demand_cold_sh_and_hw
  )

summary(model_hourly_electricity_demand_cold_sh_and_hw)


# Check prerequisites

# 1 Normal distribution residuals
plot(model_hourly_electricity_demand_cold_sh_and_hw, 2)

# 2 a Homoscedasticity
plot(model_hourly_electricity_demand_cold_sh_and_hw, 1)

bptest(model_hourly_electricity_demand_cold_sh_and_hw)

# 2 b Autocorrelation
# Also fixed by Homoscedasticity violation

# 3 No Multicollinearity good when under 10, great when under 2
vif(model_hourly_electricity_demand_cold_sh_and_hw)

# 4 No influential cases
plot(model_hourly_electricity_demand_cold_sh_and_hw, 4)


# No Homoscedasticity
# Robust standard errors
coeftest(
  model_hourly_electricity_demand_cold_sh_and_hw,
  vcov = vcovHC(model_hourly_electricity_demand_cold_sh_and_hw, type = "HC4")
)

# Standardization
zmodel_hourly_electricity_demand_cold_sh_and_hw <- lm(
  scale(hourly_electricity_demand) ~ scale(SumBuildingCountNUTS3) + scale(ShareApartmentBuildings) + scale(ShareOldBuildings) + scale(TemperatureKelvin),
  data = regions_hourly_electricity_demand_cold_sh_and_hw

)

summary(zmodel_hourly_electricity_demand_cold_sh_and_hw)


# Space heat only
model_hourly_electricity_demand_cold_sh_only <-
  lm(
    hourly_electricity_demand ~ SumBuildingCountNUTS3 + ShareApartmentBuildings + ShareOldBuildings + TemperatureKelvin,
    data = regions_hourly_electricity_demand_cold_sh_only
  )

summary(model_hourly_electricity_demand_cold_sh_only)


# Check prerequisites

# 1 Normal distribution residuals
plot(model_hourly_electricity_demand_cold_sh_only, 2)

# 2 a Homoscedasticity
plot(model_hourly_electricity_demand_cold_sh_only, 1)

bptest(model_hourly_electricity_demand_cold_sh_only)

# 2 b Autocorrelation
# Also fixed by Homoscedasticity violation

# 3 No Multicollinearity good when under 10, great when under 2
vif(model_hourly_electricity_demand_cold_sh_only)

# 4 No influential cases
plot(model_hourly_electricity_demand_cold_sh_only, 4)


# No Homoscedasticity
# Robust standard errors
coeftest(
  model_hourly_electricity_demand_cold_sh_only,
  vcov = vcovHC(model_hourly_electricity_demand_cold_sh_only, type = "HC4")
)

# Standardization
zmodel_hourly_electricity_demand_cold_sh_only <- lm(
  scale(hourly_electricity_demand) ~ scale(SumBuildingCountNUTS3) + scale(ShareApartmentBuildings) + scale(ShareOldBuildings) + scale(TemperatureKelvin),
  data = regions_hourly_electricity_demand_cold_sh_only

)

summary(zmodel_hourly_electricity_demand_cold_sh_only)



##########
#Hot year#
##########
# Regression analysis for the hourly electricity demand hot year

# Calculate the regression data

# Space heat and hot water
regions_hourly_electricity_demand_hot_sh_and_hw <-
  regions_electricity_demand_hot_iso %>%
  left_join(nuts3_regression_data, by = c("nuts3_code" = "NUTS3Code")) %>%
  left_join(air_temp_2022, by = c("date_iso" = "DATEISO"))


summary(regions_hourly_electricity_demand_hot_sh_and_hw)

# Space heat only
regions_hourly_electricity_demand_hot_sh_only <-
  regions_electricity_demand_space_heat_only_hot_iso %>%
  left_join(nuts3_regression_data, by = c("nuts3_code" = "NUTS3Code")) %>%
  left_join(air_temp_2022, by = c("date_iso" = "DATEISO"))

summary(regions_hourly_electricity_demand_hot_sh_only)


# Calculate the regression

# Space heat and hot water
model_hourly_electricity_demand_hot_sh_and_hw <-
  lm(
    hourly_electricity_demand ~ SumBuildingCountNUTS3 + ShareApartmentBuildings + ShareOldBuildings + TemperatureKelvin,
    data = regions_hourly_electricity_demand_hot_sh_and_hw
  )

summary(model_hourly_electricity_demand_hot_sh_and_hw)


# Check prerequisites

# 1 Normal distribution residuals
plot(model_hourly_electricity_demand_hot_sh_and_hw, 2)

# 2 a Homoscedasticity
plot(model_hourly_electricity_demand_hot_sh_and_hw, 1)

bptest(model_hourly_electricity_demand_hot_sh_and_hw)

# 2 b Autocorrelation
# Also fixed by Homoscedasticity violation

# 3 No Multicollinearity good when under 10, great when under 2
vif(model_hourly_electricity_demand_hot_sh_and_hw)

# 4 No influential cases
plot(model_hourly_electricity_demand_hot_sh_and_hw, 4)


# No Homoscedasticity
# Robust standard errors
coeftest(
  model_hourly_electricity_demand_hot_sh_and_hw,
  vcov = vcovHC(model_hourly_electricity_demand_hot_sh_and_hw, type = "HC4")
)

# Standardization
zmodel_hourly_electricity_demand_hot_sh_and_hw <- lm(
  scale(hourly_electricity_demand) ~ scale(SumBuildingCountNUTS3) + scale(ShareApartmentBuildings) + scale(ShareOldBuildings) + scale(TemperatureKelvin),
  data = regions_hourly_electricity_demand_hot_sh_and_hw

)

summary(zmodel_hourly_electricity_demand_hot_sh_and_hw)


# Space heat only
model_hourly_electricity_demand_hot_sh_only <-
  lm(
    hourly_electricity_demand ~ SumBuildingCountNUTS3 + ShareApartmentBuildings + ShareOldBuildings + TemperatureKelvin,
    data = regions_hourly_electricity_demand_hot_sh_only
  )

summary(model_hourly_electricity_demand_hot_sh_only)


# Check prerequisites

# 1 Normal distribution residuals
plot(model_hourly_electricity_demand_hot_sh_only, 2)

# 2 a Homoscedasticity
plot(model_hourly_electricity_demand_hot_sh_only, 1)

bptest(model_hourly_electricity_demand_hot_sh_only)

# 2 b Autocorrelation
# Also fixed by Homoscedasticity violation

# 3 No Multicollinearity good when under 10, great when under 2
vif(model_hourly_electricity_demand_hot_sh_only)

# 4 No influential cases
plot(model_hourly_electricity_demand_hot_sh_only, 4)


# No Homoscedasticity
# Robust standard errors
coeftest(
  model_hourly_electricity_demand_hot_sh_only,
  vcov = vcovHC(model_hourly_electricity_demand_hot_sh_only, type = "HC4")
)

# Standardization
zmodel_hourly_electricity_demand_hot_sh_only <- lm(
  scale(hourly_electricity_demand) ~ scale(SumBuildingCountNUTS3) + scale(ShareApartmentBuildings) + scale(ShareOldBuildings) + scale(TemperatureKelvin),
  data = regions_hourly_electricity_demand_hot_sh_only

)

summary(zmodel_hourly_electricity_demand_hot_sh_only)
