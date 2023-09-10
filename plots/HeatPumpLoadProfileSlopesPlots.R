# Load required packages
library(tidyverse)
library("dplyr")

# Read the heat demand data
eh_combined_heat_demand_reference <-
  read_csv2("data/output/heatdemand/eh_combined_heat_demand_reference.csv")

mh_combined_heat_demand_reference <-
  read_csv2("data/output/heatdemand/mh_combined_heat_demand_reference.csv")


# Calculate the flow temperature
eh_combined_heat_demand_reference <-
  eh_combined_heat_demand_reference %>%
  mutate(
    Max_SpaceHeat_beginn_1918_reference  = max(SpaceHeat_beginn_1918_reference),
    Max_SpaceHeat_1919_1948_reference = max(SpaceHeat_1919_1948_reference),
    Max_SpaceHeat_1949_1978_reference = max(SpaceHeat_1949_1978_reference),
    Max_SpaceHeat_1979_1986_reference = max(SpaceHeat_1979_1986_reference),
    Max_SpaceHeat_1987_1990_reference = max(SpaceHeat_1987_1990_reference),
    Max_SpaceHeat_1991_1995_reference = max(SpaceHeat_1991_1995_reference),
    Max_SpaceHeat_1996_2000_reference = max(SpaceHeat_1996_2000_reference),
    Max_SpaceHeat_2001_2011_reference = max(SpaceHeat_2001_2011_reference),
    Max_SpaceHeat_2012_2022_reference = max(SpaceHeat_2012_2022_reference),
    Max_SpaceHeat_2023_2030_reference = max(SpaceHeat_2023_2030_reference)
  )

eh_min_space_heat_per_m2 <-
  min(
    eh_combined_heat_demand_reference$Max_SpaceHeat_beginn_1918_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1919_1948_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1949_1978_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1979_1986_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1987_1990_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1991_1995_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1996_2000_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2001_2011_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2012_2022_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2023_2030_reference
  )

eh_max_space_heat_per_m2 <-
  max(
    eh_combined_heat_demand_reference$Max_SpaceHeat_beginn_1918_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1919_1948_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1949_1978_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1979_1986_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1987_1990_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1991_1995_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_1996_2000_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2001_2011_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2012_2022_reference,
    eh_combined_heat_demand_reference$Max_SpaceHeat_2023_2030_reference
  )

# Separate calculation for multi-family houses, since larger losses
mh_combined_heat_demand_reference <-
  mh_combined_heat_demand_reference %>%
  mutate(
    Max_SpaceHeat_beginn_1918_reference  = max(SpaceHeat_beginn_1918_reference),
    Max_SpaceHeat_1919_1948_reference = max(SpaceHeat_1919_1948_reference),
    Max_SpaceHeat_1949_1978_reference = max(SpaceHeat_1949_1978_reference),
    Max_SpaceHeat_1979_1986_reference = max(SpaceHeat_1979_1986_reference),
    Max_SpaceHeat_1987_1990_reference = max(SpaceHeat_1987_1990_reference),
    Max_SpaceHeat_1991_1995_reference = max(SpaceHeat_1991_1995_reference),
    Max_SpaceHeat_1996_2000_reference = max(SpaceHeat_1996_2000_reference),
    Max_SpaceHeat_2001_2011_reference = max(SpaceHeat_2001_2011_reference),
    Max_SpaceHeat_2012_2022_reference = max(SpaceHeat_2012_2022_reference),
    Max_SpaceHeat_2023_2030_reference = max(SpaceHeat_2023_2030_reference)
  )

mh_min_space_heat_per_m2 <-
  min(
    mh_combined_heat_demand_reference$Max_SpaceHeat_beginn_1918_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1919_1948_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1949_1978_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1979_1986_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1987_1990_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1991_1995_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1996_2000_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2001_2011_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2012_2022_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2023_2030_reference
  )

mh_max_space_heat_per_m2 <-
  max(
    mh_combined_heat_demand_reference$Max_SpaceHeat_beginn_1918_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1919_1948_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1949_1978_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1979_1986_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1987_1990_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1991_1995_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_1996_2000_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2001_2011_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2012_2022_reference,
    mh_combined_heat_demand_reference$Max_SpaceHeat_2023_2030_reference
  )

min_slope <- 0.3

max_slope <- 1.6

slope_function <-
  function(x,
           min_space_heat_per_m2,
           max_space_heat_per_m2) {
    return (min_slope + (x - min_space_heat_per_m2) * ((max_slope - min_slope) / (max_space_heat_per_m2 - min_space_heat_per_m2)
    ))
  }

eh_combined_slopes <- eh_combined_heat_demand_reference %>%
  mutate(
    Slope_SpaceHeat_beginn_1918 = slope_function(
      Max_SpaceHeat_beginn_1918_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1919_1948 = slope_function(
      Max_SpaceHeat_1919_1948_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1949_1978 = slope_function(
      Max_SpaceHeat_1949_1978_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1979_1986 = slope_function(
      Max_SpaceHeat_1979_1986_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1987_1990 = slope_function(
      Max_SpaceHeat_1987_1990_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1991_1995 = slope_function(
      Max_SpaceHeat_1991_1995_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1996_2000 = slope_function(
      Max_SpaceHeat_1996_2000_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2001_2011 = slope_function(
      Max_SpaceHeat_2001_2011_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2012_2022 = slope_function(
      Max_SpaceHeat_2012_2022_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2023_2030 = slope_function(
      Max_SpaceHeat_2023_2030_reference,
      eh_min_space_heat_per_m2,
      eh_max_space_heat_per_m2
    )
  ) %>%
  select(
    c(
      Time,
      Slope_SpaceHeat_beginn_1918,
      Slope_SpaceHeat_1919_1948,
      Slope_SpaceHeat_1949_1978,
      Slope_SpaceHeat_1979_1986,
      Slope_SpaceHeat_1987_1990,
      Slope_SpaceHeat_1991_1995,
      Slope_SpaceHeat_1996_2000,
      Slope_SpaceHeat_2001_2011,
      Slope_SpaceHeat_2012_2022,
      Slope_SpaceHeat_2023_2030
    )
  )

eh_combined_heat_demand_reference <-
  eh_combined_heat_demand_reference %>%
  select(
    -c(
      Max_SpaceHeat_beginn_1918_reference,
      Max_SpaceHeat_1919_1948_reference,
      Max_SpaceHeat_1949_1978_reference,
      Max_SpaceHeat_1979_1986_reference,
      Max_SpaceHeat_1987_1990_reference,
      Max_SpaceHeat_1991_1995_reference,
      Max_SpaceHeat_1996_2000_reference,
      Max_SpaceHeat_2001_2011_reference,
      Max_SpaceHeat_2012_2022_reference,
      Max_SpaceHeat_2023_2030_reference
    )
  )

mh_combined_slopes <- mh_combined_heat_demand_reference %>%
  mutate(
    Slope_SpaceHeat_beginn_1918 = slope_function(
      Max_SpaceHeat_beginn_1918_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1919_1948 = slope_function(
      Max_SpaceHeat_1919_1948_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1949_1978 = slope_function(
      Max_SpaceHeat_1949_1978_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1979_1986 = slope_function(
      Max_SpaceHeat_1979_1986_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1987_1990 = slope_function(
      Max_SpaceHeat_1987_1990_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1991_1995 = slope_function(
      Max_SpaceHeat_1991_1995_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_1996_2000 = slope_function(
      Max_SpaceHeat_1996_2000_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2001_2011 = slope_function(
      Max_SpaceHeat_2001_2011_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2012_2022 = slope_function(
      Max_SpaceHeat_2012_2022_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    ),
    Slope_SpaceHeat_2023_2030 = slope_function(
      Max_SpaceHeat_2023_2030_reference,
      mh_min_space_heat_per_m2,
      mh_max_space_heat_per_m2
    )
  ) %>%
  select(
    c(
      Time,
      Slope_SpaceHeat_beginn_1918,
      Slope_SpaceHeat_1919_1948,
      Slope_SpaceHeat_1949_1978,
      Slope_SpaceHeat_1979_1986,
      Slope_SpaceHeat_1987_1990,
      Slope_SpaceHeat_1991_1995,
      Slope_SpaceHeat_1996_2000,
      Slope_SpaceHeat_2001_2011,
      Slope_SpaceHeat_2012_2022,
      Slope_SpaceHeat_2023_2030
    )
  )

mh_combined_heat_demand_reference <-
  mh_combined_heat_demand_reference %>%
  select(
    -c(
      Max_SpaceHeat_beginn_1918_reference,
      Max_SpaceHeat_1919_1948_reference,
      Max_SpaceHeat_1949_1978_reference,
      Max_SpaceHeat_1979_1986_reference,
      Max_SpaceHeat_1987_1990_reference,
      Max_SpaceHeat_1991_1995_reference,
      Max_SpaceHeat_1996_2000_reference,
      Max_SpaceHeat_2001_2011_reference,
      Max_SpaceHeat_2012_2022_reference,
      Max_SpaceHeat_2023_2030_reference
    )
  )

room_target_temperature <- 293.15
level <- 0

# dar = Tmix - room_target_temperature
flow_temperature_function_c <- function(slope, temp_c) {
  dar <- temp_c - 20
  return (
    room_target_temperature + level - slope * dar * (1.4347 + 0.021 * dar + 247.9 * 10 ^
                                                       (-6) * dar ^ 2) - 273.15
  )
}


# Get the slopes for single-family houses
EH_Slope_SpaceHeat_beginn_1918 <-
  eh_combined_slopes[1,]$Slope_SpaceHeat_beginn_1918
EH_Slope_SpaceHeat_1919_1948 <-
  eh_combined_slopes[1,]$Slope_SpaceHeat_1919_1948
EH_Slope_SpaceHeat_1949_1978 <-
  eh_combined_slopes[1,]$Slope_SpaceHeat_1949_1978
EH_Slope_SpaceHeat_1979_1986 <-
  eh_combined_slopes[1,]$Slope_SpaceHeat_1979_1986
EH_Slope_SpaceHeat_1987_1990 <-
  eh_combined_slopes[1,]$Slope_SpaceHeat_1987_1990
EH_Slope_SpaceHeat_1991_1995 <-
  eh_combined_slopes[1,]$Slope_SpaceHeat_1991_1995
EH_Slope_SpaceHeat_1996_2000 <-
  eh_combined_slopes[1,]$Slope_SpaceHeat_1996_2000
EH_Slope_SpaceHeat_2001_2011 <-
  eh_combined_slopes[1,]$Slope_SpaceHeat_2001_2011
EH_Slope_SpaceHeat_2012_2022 <-
  eh_combined_slopes[1,]$Slope_SpaceHeat_2012_2022
EH_Slope_SpaceHeat_2023_2030 <-
  eh_combined_slopes[1,]$Slope_SpaceHeat_2023_2030


df_heating_curve_eh_before_1919 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(EH_Slope_SpaceHeat_beginn_1918,-20:20),
  year_of_construction = "Before 1919"
)

df_heating_curve_eh_1919_1948 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(EH_Slope_SpaceHeat_1919_1948,-20:20),
  year_of_construction = "1919 - 1948"
)

df_heating_curve_eh_1949_1978 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(EH_Slope_SpaceHeat_1949_1978,-20:20),
  year_of_construction = "1949 - 1978"
)

df_heating_curve_eh_1979_1986 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(EH_Slope_SpaceHeat_1979_1986,-20:20),
  year_of_construction = "1979 - 1986"
)

df_heating_curve_eh_1987_1990 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(EH_Slope_SpaceHeat_1987_1990,-20:20),
  year_of_construction = "1987 - 1990"
)

df_heating_curve_eh_1991_1995 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(EH_Slope_SpaceHeat_1991_1995,-20:20),
  year_of_construction = "1991 - 1995"
)

df_heating_curve_eh_1996_2000 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(EH_Slope_SpaceHeat_1996_2000,-20:20),
  year_of_construction = "1996 - 2000"
)

df_heating_curve_eh_2001_2011 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(EH_Slope_SpaceHeat_2001_2011,-20:20),
  year_of_construction = "2001 - 2011"
)

df_heating_curve_eh_2012_2022 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(EH_Slope_SpaceHeat_2012_2022,-20:20),
  year_of_construction = "2012 - 2022"
)

df_heating_curve_eh_2023_2030 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(EH_Slope_SpaceHeat_2023_2030,-20:20),
  year_of_construction = "2023 - 2030"
)

df_heating_curve_eh <- df_heating_curve_eh_before_1919 %>%
  rbind(df_heating_curve_eh_1919_1948) %>%
  rbind(df_heating_curve_eh_1949_1978) %>%
  rbind(df_heating_curve_eh_1979_1986) %>%
  rbind(df_heating_curve_eh_1987_1990) %>%
  rbind(df_heating_curve_eh_1991_1995) %>%
  rbind(df_heating_curve_eh_1996_2000) %>%
  rbind(df_heating_curve_eh_2001_2011) %>%
  rbind(df_heating_curve_eh_2012_2022) %>%
  rbind(df_heating_curve_eh_2023_2030)

heating_curve_plot_eh <- ggplot(df_heating_curve_eh,
       aes(temp_c,
           values,
           color = factor(
             year_of_construction,
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
  geom_line(linewidth = 1) +
  xlab("Mix temperature (°C)") +
  ylab("Flow temperature space heating (°C)") +
  scale_x_reverse() +
  scale_color_brewer(palette = "Set3") +
  guides(color = guide_legend(title = "Year of construction"))

heating_curve_plot_eh


# Get the slopes for multi-family houses
MH_Slope_SpaceHeat_beginn_1918 <-
  mh_combined_slopes[1,]$Slope_SpaceHeat_beginn_1918
MH_Slope_SpaceHeat_1919_1948 <-
  mh_combined_slopes[1,]$Slope_SpaceHeat_1919_1948
MH_Slope_SpaceHeat_1949_1978 <-
  mh_combined_slopes[1,]$Slope_SpaceHeat_1949_1978
MH_Slope_SpaceHeat_1979_1986 <-
  mh_combined_slopes[1,]$Slope_SpaceHeat_1979_1986
MH_Slope_SpaceHeat_1987_1990 <-
  mh_combined_slopes[1,]$Slope_SpaceHeat_1987_1990
MH_Slope_SpaceHeat_1991_1995 <-
  mh_combined_slopes[1,]$Slope_SpaceHeat_1991_1995
MH_Slope_SpaceHeat_1996_2000 <-
  mh_combined_slopes[1,]$Slope_SpaceHeat_1996_2000
MH_Slope_SpaceHeat_2001_2011 <-
  mh_combined_slopes[1,]$Slope_SpaceHeat_2001_2011
MH_Slope_SpaceHeat_2012_2022 <-
  mh_combined_slopes[1,]$Slope_SpaceHeat_2012_2022
MH_Slope_SpaceHeat_2023_2030 <-
  mh_combined_slopes[1,]$Slope_SpaceHeat_2023_2030

df_heating_curve_mh_before_1919 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(MH_Slope_SpaceHeat_beginn_1918,-20:20),
  year_of_construction = "Before 1919"
)

df_heating_curve_mh_1919_1948 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(MH_Slope_SpaceHeat_1919_1948,-20:20),
  year_of_construction = "1919 - 1948"
)

df_heating_curve_mh_1949_1978 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(MH_Slope_SpaceHeat_1949_1978,-20:20),
  year_of_construction = "1949 - 1978"
)

df_heating_curve_mh_1979_1986 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(MH_Slope_SpaceHeat_1979_1986,-20:20),
  year_of_construction = "1979 - 1986"
)

df_heating_curve_mh_1987_1990 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(MH_Slope_SpaceHeat_1987_1990,-20:20),
  year_of_construction = "1987 - 1990"
)

df_heating_curve_mh_1991_1995 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(MH_Slope_SpaceHeat_1991_1995,-20:20),
  year_of_construction = "1991 - 1995"
)

df_heating_curve_mh_1996_2000 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(MH_Slope_SpaceHeat_1996_2000,-20:20),
  year_of_construction = "1996 - 2000"
)

df_heating_curve_mh_2001_2011 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(MH_Slope_SpaceHeat_2001_2011,-20:20),
  year_of_construction = "2001 - 2011"
)

df_heating_curve_mh_2012_2022 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(MH_Slope_SpaceHeat_2012_2022,-20:20),
  year_of_construction = "2012 - 2022"
)

df_heating_curve_mh_2023_2030 = data.frame(
  temp_c = -20:20,
  values = flow_temperature_function_c(MH_Slope_SpaceHeat_2023_2030,-20:20),
  year_of_construction = "2023 - 2030"
)

df_heating_curve_mh <- df_heating_curve_mh_before_1919 %>%
  rbind(df_heating_curve_mh_1919_1948) %>%
  rbind(df_heating_curve_mh_1949_1978) %>%
  rbind(df_heating_curve_mh_1979_1986) %>%
  rbind(df_heating_curve_mh_1987_1990) %>%
  rbind(df_heating_curve_mh_1991_1995) %>%
  rbind(df_heating_curve_mh_1996_2000) %>%
  rbind(df_heating_curve_mh_2001_2011) %>%
  rbind(df_heating_curve_mh_2012_2022) %>%
  rbind(df_heating_curve_mh_2023_2030)

heating_curve_plot_mh <- ggplot(df_heating_curve_mh,
                                aes(temp_c,
                                    values,
                                    color = factor(
                                      year_of_construction,
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
  geom_line(linewidth = 1) +
  xlab("Mix temperature (°C)") +
  ylab("Flow temperature space heating (°C)") +
  scale_x_reverse() +
  scale_color_brewer(palette = "Set3") +
  guides(color = guide_legend(title = "Year of construction"))

heating_curve_plot_mh


# Save the plots
ggsave(
  "plots/output/loadprofile/heating_curve_plot_eh.png",
  heating_curve_plot_eh,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/loadprofile/heating_curve_plot_mh.png",
  heating_curve_plot_mh,
  width = 25,
  units = "cm"
)
