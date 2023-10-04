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

building_stock_2030_with_hp_distribution <-
  read_csv("data/output/heatpumpexpansion/building_stock_2030_with_hp_distribution.csv") %>% mutate_if(is.character, as.factor)

districts_germany_geo_info <-
  st_read(
    "plots/heatpumpdistributionmap/K-2022-AI008-1-5--AI0801--2023-09-01/K-2022-AI008-1-5--AI0801--2023-09-01.shp",
    quiet = TRUE
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


# Get the amount of heat pumps per NUTS 3
nuts3_heat_pump_distribution <-
  building_stock_2030_with_hp_distribution %>%
  mutate(HPSum = HPAmountAir + HPAmountProbe + HPAmountCollector) %>%
  select(c("NUTS3Code",
           "HPSum"))

nuts3_heat_pump_distribution <- nuts3_heat_pump_distribution %>%
  group_by(NUTS3Code) %>%
  summarise(`Total heat pump sum` = sum(HPSum))


# Join the districts geo info with the heat pump amount per NUTS 3
districts_germany_geo_info_with_hp_sum <-
  districts_germany_geo_info %>%
  left_join(nuts3_heat_pump_distribution, by = c("NUTS3Code"))


# Plot the map
tmap_mode("plot")
hp_distribution_germany_plot <- tm_shape(districts_germany_geo_info_with_hp_sum) +
  tm_polygons(
    "Total heat pump sum",
    id = "NUTS3Code",
    palette = "-viridis",
    style = "fisher",
    title = "Heat pump amount",
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

hp_distribution_germany_plot


# Save the plot
tmap_save(
  hp_distribution_germany_plot,
  "plots/output/heatpumpdistribution/hp_distribution_germany_plot.png",
  width = 30,
  units = "cm"
)


# Create an interactive map
tmap_mode("view")
hp_distribution_germany_interactive <- tm_shape(districts_germany_geo_info_with_hp_sum) +
  tm_polygons(
    "Total heat pump sum",
    id = "gen",
    palette = "-viridis",
    style = "fisher",
    title = "Heat pump amount",
    alpha = .5
  ) +
  tm_layout(legend.format = list(
    fun = function(x)
      round(x, digits = 0)
  ))

hp_distribution_germany_interactive


# Save the interactive map
tmap_save(
  hp_distribution_germany_interactive,
  "plots/output/heatpumpdistribution/hp_distribution_germany_interactive.html",
)

