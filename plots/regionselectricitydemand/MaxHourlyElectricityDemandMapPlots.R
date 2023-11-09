# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)
library(sf)
library(stringi)
library(tmap)


# Read the data
nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx") %>% mutate_if(is.character, as.factor)

districts_germany_geo_info <-
  st_read(
    "plots/heatpumpdistributionmap/K-2022-AI008-1-5--AI0801--2023-09-01/K-2022-AI008-1-5--AI0801--2023-09-01.shp",
    quiet = TRUE
  )

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

nuts3regioninfo <- nuts3regioninfo %>%
  mutate(
    NUTS3Type = fct_recode(
      NUTS3Type,
      "District" = "Kreis",
      "Urban district" = "Kreisfreie Stadt",
      "Urban district" = "Stadtkreis",
      "Rural district" = "Landkreis",
      "Regional association" = "Regionalverband"
    )
  )

nuts3regioninfo <- nuts3regioninfo %>%
  mutate(
    NUTS3Name = paste(gsub(",.*", "", NUTS3Name), NUTS3Type, sep = ", ")
  )

# Join the districts geo info with the NUTS 3 region info
districts_germany_geo_info <- districts_germany_geo_info %>%
  mutate(schluessel_modified = as.character(stri_pad_right(schluessel, 5, 0)))


districts_germany_geo_info <- districts_germany_geo_info %>%
  left_join(nuts3regioninfo,
            by = c("schluessel_modified" = "NUTS3RegionKey"))


districts_germany_geo_info <- districts_germany_geo_info %>%
  select(
    c(
      "id",
      "schluessel",
      "gen",
      "jahr",
      "ai0801",
      "Shape_Leng",
      "Shape_Area",
      "NUTS3Code",
      "geometry"
    )
  )


# Calculate the electricity demand per NUTS 3

# Space heat and hot water
regions_electricity_demand_reference <-
  regions_electricity_demand_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = max(hourly_electricity_demand),
            .groups = "drop")

regions_electricity_demand_cold <-
  regions_electricity_demand_cold_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = max(hourly_electricity_demand),
            .groups = "drop")

regions_electricity_demand_hot <-
  regions_electricity_demand_hot_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = max(hourly_electricity_demand),
            .groups = "drop")


# Space heat only
regions_electricity_demand_space_heat_only_reference <-
  regions_electricity_demand_space_heat_only_reference_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = max(hourly_electricity_demand),
            .groups = "drop")

regions_electricity_demand_space_heat_only_cold <-
  regions_electricity_demand_space_heat_only_cold_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = max(hourly_electricity_demand),
            .groups = "drop")

regions_electricity_demand_space_heat_only_hot <-
  regions_electricity_demand_space_heat_only_hot_iso %>%
  select(-c("date_iso")) %>%
  group_by(nuts3_code) %>%
  summarise("ElectricityDemand" = max(hourly_electricity_demand),
            .groups = "drop")


# Join the districts geo info with the heat pump electricity demand per NUTS 3
districts_germany_geo_info_with_electricity_demand_sh_and_hw_reference <-
  districts_germany_geo_info %>%
  left_join(regions_electricity_demand_reference,
            by = c("NUTS3Code" = "nuts3_code")) %>%
  mutate("Electricity demand in MWh" = ElectricityDemand / 1000)

districts_germany_geo_info_with_electricity_demand_sh_and_hw_cold <-
  districts_germany_geo_info %>%
  left_join(regions_electricity_demand_cold,
            by = c("NUTS3Code" = "nuts3_code")) %>%
  mutate("Electricity demand in MWh" = ElectricityDemand / 1000)

districts_germany_geo_info_with_electricity_demand_sh_and_hw_hot <-
  districts_germany_geo_info %>%
  left_join(regions_electricity_demand_hot,
            by = c("NUTS3Code" = "nuts3_code")) %>%
  mutate("Electricity demand in MWh" = ElectricityDemand / 1000)


districts_germany_geo_info_with_electricity_demand_sh_only_reference <-
  districts_germany_geo_info %>%
  left_join(
    regions_electricity_demand_space_heat_only_reference,
    by = c("NUTS3Code" = "nuts3_code")
  ) %>%
  mutate("Electricity demand in MWh" = ElectricityDemand / 1000)

districts_germany_geo_info_with_electricity_demand_sh_only_cold <-
  districts_germany_geo_info %>%
  left_join(regions_electricity_demand_space_heat_only_cold,
            by = c("NUTS3Code" = "nuts3_code")) %>%
  mutate("Electricity demand in MWh" = ElectricityDemand / 1000)

districts_germany_geo_info_with_electricity_demand_sh_only_hot <-
  districts_germany_geo_info %>%
  left_join(regions_electricity_demand_space_heat_only_hot,
            by = c("NUTS3Code" = "nuts3_code")) %>%
  mutate("Electricity demand in MWh" = ElectricityDemand / 1000)


# Plot the maps

# Space heat and hot water
tmap_mode("plot")
hp_electricity_demand_sh_and_hw_reference_plot <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_and_hw_reference) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "NUTS3Code",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    border.col = "white",
    lwd = .1
  ) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_style("gray") +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_and_hw_reference_plot


tmap_mode("plot")
hp_electricity_demand_sh_and_hw_cold_plot <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_and_hw_cold) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "NUTS3Code",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    border.col = "white",
    lwd = .1
  ) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_style("gray") +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_and_hw_cold_plot


tmap_mode("plot")
hp_electricity_demand_sh_and_hw_hot_plot <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_and_hw_hot) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "NUTS3Code",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    border.col = "white",
    lwd = .1
  ) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_style("gray") +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_and_hw_hot_plot


# Space heat only
tmap_mode("plot")
hp_electricity_demand_sh_only_reference_plot <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_only_reference) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "NUTS3Code",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    border.col = "white",
    lwd = .1
  ) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_style("gray") +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_only_reference_plot


tmap_mode("plot")
hp_electricity_demand_sh_only_cold_plot <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_only_cold) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "NUTS3Code",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    border.col = "white",
    lwd = .1
  ) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_style("gray") +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_only_cold_plot


tmap_mode("plot")
hp_electricity_demand_sh_only_hot_plot <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_only_hot) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "NUTS3Code",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    border.col = "white",
    lwd = .1
  ) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_style("gray") +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_only_hot_plot


# Save the plots

# Space heat and hot water
tmap_save(
  hp_electricity_demand_sh_and_hw_reference_plot,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/hp_max_hourly_electricity_demand_sh_and_hw_reference_plot.png",
  width = 30,
  units = "cm"
)

tmap_save(
  hp_electricity_demand_sh_and_hw_cold_plot,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/hp_max_hourly_electricity_demand_sh_and_hw_cold_plot.png",
  width = 30,
  units = "cm"
)

tmap_save(
  hp_electricity_demand_sh_and_hw_hot_plot,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/hp_max_hourly_electricity_demand_sh_and_hw_hot_plot.png",
  width = 30,
  units = "cm"
)

# Space heat only
tmap_save(
  hp_electricity_demand_sh_only_reference_plot,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/hp_max_hourly_electricity_demand_sh_only_reference_plot.png",
  width = 30,
  units = "cm"
)

tmap_save(
  hp_electricity_demand_sh_only_cold_plot,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/hp_max_hourly_electricity_demand_sh_only_cold_plot.png",
  width = 30,
  units = "cm"
)

tmap_save(
  hp_electricity_demand_sh_only_hot_plot,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/hp_max_hourly_electricity_demand_sh_only_hot_plot.png",
  width = 30,
  units = "cm"
)


# Create an interactive map

# Space heat and hot water
tmap_mode("view")
hp_electricity_demand_sh_and_hw_reference_interactive <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_and_hw_reference) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "gen",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    alpha = .5
  ) +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_and_hw_reference_interactive


tmap_mode("view")
hp_electricity_demand_sh_and_hw_cold_interactive <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_and_hw_cold) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "gen",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    alpha = .5
  ) +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_and_hw_cold_interactive


tmap_mode("view")
hp_electricity_demand_sh_and_hw_hot_interactive <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_and_hw_hot) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "gen",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    alpha = .5
  ) +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_and_hw_hot_interactive


# Space heat only
tmap_mode("view")
hp_electricity_demand_sh_only_reference_interactive <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_only_reference) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "gen",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    alpha = .5
  ) +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_only_reference_interactive


tmap_mode("view")
hp_electricity_demand_sh_only_cold_interactive <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_only_cold) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "gen",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    alpha = .5
  ) +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_only_cold_interactive


tmap_mode("view")
hp_electricity_demand_sh_only_hot_interactive <-
  tm_shape(districts_germany_geo_info_with_electricity_demand_sh_only_hot) +
  tm_polygons(
    "Electricity demand in MWh",
    id = "gen",
    palette = "-viridis",
    style = "fisher",
    title = "Electricity demand in MWh",
    alpha = .5
  ) +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_electricity_demand_sh_only_hot_interactive


# Save the interactive maps

# Space heat and hot water
tmap_save(
  hp_electricity_demand_sh_and_hw_reference_interactive,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/interactive/hp_max_hourly_electricity_demand_sh_and_hw_reference_interactive.html",
)

tmap_save(
  hp_electricity_demand_sh_and_hw_cold_interactive,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/interactive/hp_max_hourly_electricity_demand_sh_and_hw_cold_interactive.html",
)

tmap_save(
  hp_electricity_demand_sh_and_hw_hot_interactive,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/interactive/hp_max_hourly_electricity_demand_sh_and_hw_hot_interactive.html",
)


# Space heat only
tmap_save(
  hp_electricity_demand_sh_only_reference_interactive,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/interactive/hp_max_hourly_electricity_demand_sh_only_reference_interactive.html",
)

tmap_save(
  hp_electricity_demand_sh_only_cold_interactive,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/interactive/hp_max_hourly_electricity_demand_sh_only_cold_interactive.html",
)

tmap_save(
  hp_electricity_demand_sh_only_hot_interactive,
  "plots/output/regionselectricitydemand/maxhourlyelectricitydemandmaps/interactive/hp_max_hourly_electricity_demand_sh_only_hot_interactive.html",
)


