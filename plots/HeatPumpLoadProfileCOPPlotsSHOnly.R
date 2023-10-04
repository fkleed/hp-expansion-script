# Load required packages
library(tidyverse)
library("dplyr")

# Read data
eh_loadprofile_ashp_reference <-
  read_csv2("data/output/loadprofile/eh_loadprofile_ashp_space_heat_only_reference.csv")
eh_loadprofile_ashp_cold <-
  read_csv2("data/output/loadprofile/eh_loadprofile_ashp_space_heat_only_cold.csv")
eh_loadprofile_ashp_hot <-
  read_csv2("data/output/loadprofile/eh_loadprofile_ashp_space_heat_only_hot.csv")

eh_loadprofile_gshp_collector_reference <-
  read_csv2(
    "data/output/loadprofile/eh_loadprofile_gshp_collector_space_heat_only_reference.csv"
  )
eh_loadprofile_gshp_collector_cold <-
  read_csv2(
    "data/output/loadprofile/eh_loadprofile_gshp_collector_space_heat_only_cold.csv"
  )
eh_loadprofile_gshp_collector_hot <-
  read_csv2(
    "data/output/loadprofile/eh_loadprofile_gshp_collector_space_heat_only_hot.csv"
  )

eh_loadprofile_gshp_probe_reference <-
  read_csv2(
    "data/output/loadprofile/eh_loadprofile_gshp_probe_space_heat_only_reference.csv"
  )
eh_loadprofile_gshp_probe_cold <-
  read_csv2("data/output/loadprofile/eh_loadprofile_gshp_probe_space_heat_only_cold.csv")
eh_loadprofile_gshp_probe_hot <-
  read_csv2("data/output/loadprofile/eh_loadprofile_gshp_probe_space_heat_only_hot.csv")


mh_loadprofile_ashp_reference <-
  read_csv2("data/output/loadprofile/mh_loadprofile_ashp_space_heat_only_reference.csv")
mh_loadprofile_ashp_cold <-
  read_csv2("data/output/loadprofile/mh_loadprofile_ashp_space_heat_only_cold.csv")
mh_loadprofile_ashp_hot <-
  read_csv2("data/output/loadprofile/mh_loadprofile_ashp_space_heat_only_hot.csv")

mh_loadprofile_gshp_collector_reference <-
  read_csv2(
    "data/output/loadprofile/mh_loadprofile_gshp_collector_space_heat_only_reference.csv"
  )
mh_loadprofile_gshp_collector_cold <-
  read_csv2(
    "data/output/loadprofile/mh_loadprofile_gshp_collector_space_heat_only_cold.csv"
  )
mh_loadprofile_gshp_collector_hot <-
  read_csv2(
    "data/output/loadprofile/mh_loadprofile_gshp_collector_space_heat_only_hot.csv"
  )

mh_loadprofile_gshp_probe_reference <-
  read_csv2(
    "data/output/loadprofile/mh_loadprofile_gshp_probe_space_heat_only_reference.csv"
  )
mh_loadprofile_gshp_probe_cold <-
  read_csv2("data/output/loadprofile/mh_loadprofile_gshp_probe_space_heat_only_cold.csv")
mh_loadprofile_gshp_probe_hot <-
  read_csv2("data/output/loadprofile/mh_loadprofile_gshp_probe_space_heat_only_hot.csv")


# COP of load profiles single-family houses
eh_loadprofile_ashp_reference <- eh_loadprofile_ashp_reference %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_reference) / sum(ElectricityDemand_beginn_1918_reference),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_reference) / sum(ElectricityDemand_1919_1948_reference),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_reference) / sum(ElectricityDemand_1949_1978_reference),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_reference) / sum(ElectricityDemand_1979_1986_reference),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_reference) / sum(ElectricityDemand_1987_1990_reference),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_reference) / sum(ElectricityDemand_1991_1995_reference),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_reference) / sum(ElectricityDemand_1996_2000_reference),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_reference) / sum(ElectricityDemand_2001_2011_reference),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_reference) / sum(ElectricityDemand_2012_2022_reference),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_reference) / sum(ElectricityDemand_2023_2030_reference)
  ) %>%
  select(
    c(
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
  )

eh_loadprofile_ashp_reference <-
  head(eh_loadprofile_ashp_reference, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2017",
         Type = "ASHP")


eh_loadprofile_ashp_cold <- eh_loadprofile_ashp_cold %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_cold) / sum(ElectricityDemand_beginn_1918_cold),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_cold) / sum(ElectricityDemand_1919_1948_cold),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_cold) / sum(ElectricityDemand_1949_1978_cold),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_cold) / sum(ElectricityDemand_1979_1986_cold),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_cold) / sum(ElectricityDemand_1987_1990_cold),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_cold) / sum(ElectricityDemand_1991_1995_cold),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_cold) / sum(ElectricityDemand_1996_2000_cold),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_cold) / sum(ElectricityDemand_2001_2011_cold),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_cold) / sum(ElectricityDemand_2012_2022_cold),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_cold) / sum(ElectricityDemand_2023_2030_cold)
  ) %>%
  select(
    c(
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
  )

eh_loadprofile_ashp_cold <-
  head(eh_loadprofile_ashp_cold, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2010",
         Type = "ASHP")


eh_loadprofile_ashp_hot <- eh_loadprofile_ashp_hot %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_hot) / sum(ElectricityDemand_beginn_1918_hot),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_hot) / sum(ElectricityDemand_1919_1948_hot),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_hot) / sum(ElectricityDemand_1949_1978_hot),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_hot) / sum(ElectricityDemand_1979_1986_hot),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_hot) / sum(ElectricityDemand_1987_1990_hot),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_hot) / sum(ElectricityDemand_1991_1995_hot),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_hot) / sum(ElectricityDemand_1996_2000_hot),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_hot) / sum(ElectricityDemand_2001_2011_hot),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_hot) / sum(ElectricityDemand_2012_2022_hot),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_hot) / sum(ElectricityDemand_2023_2030_hot)
  ) %>%
  select(
    c(
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
  )

eh_loadprofile_ashp_hot <-
  head(eh_loadprofile_ashp_hot, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2022",
         Type = "ASHP")


eh_loadprofile_gshp_collector_reference <-
  eh_loadprofile_gshp_collector_reference %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_reference) / sum(ElectricityDemand_beginn_1918_reference),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_reference) / sum(ElectricityDemand_1919_1948_reference),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_reference) / sum(ElectricityDemand_1949_1978_reference),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_reference) / sum(ElectricityDemand_1979_1986_reference),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_reference) / sum(ElectricityDemand_1987_1990_reference),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_reference) / sum(ElectricityDemand_1991_1995_reference),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_reference) / sum(ElectricityDemand_1996_2000_reference),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_reference) / sum(ElectricityDemand_2001_2011_reference),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_reference) / sum(ElectricityDemand_2012_2022_reference),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_reference) / sum(ElectricityDemand_2023_2030_reference)
  ) %>%
  select(
    c(
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
  )

eh_loadprofile_gshp_collector_reference <-
  head(eh_loadprofile_gshp_collector_reference, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2017",
         Type = "GSHP Collector")


eh_loadprofile_gshp_collector_cold <-
  eh_loadprofile_gshp_collector_cold %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_cold) / sum(ElectricityDemand_beginn_1918_cold),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_cold) / sum(ElectricityDemand_1919_1948_cold),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_cold) / sum(ElectricityDemand_1949_1978_cold),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_cold) / sum(ElectricityDemand_1979_1986_cold),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_cold) / sum(ElectricityDemand_1987_1990_cold),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_cold) / sum(ElectricityDemand_1991_1995_cold),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_cold) / sum(ElectricityDemand_1996_2000_cold),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_cold) / sum(ElectricityDemand_2001_2011_cold),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_cold) / sum(ElectricityDemand_2012_2022_cold),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_cold) / sum(ElectricityDemand_2023_2030_cold)
  ) %>%
  select(
    c(
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
  )

eh_loadprofile_gshp_collector_cold <-
  head(eh_loadprofile_gshp_collector_cold, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2010",
         Type = "GSHP Collector")


eh_loadprofile_gshp_collector_hot <-
  eh_loadprofile_gshp_collector_hot %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_hot) / sum(ElectricityDemand_beginn_1918_hot),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_hot) / sum(ElectricityDemand_1919_1948_hot),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_hot) / sum(ElectricityDemand_1949_1978_hot),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_hot) / sum(ElectricityDemand_1979_1986_hot),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_hot) / sum(ElectricityDemand_1987_1990_hot),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_hot) / sum(ElectricityDemand_1991_1995_hot),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_hot) / sum(ElectricityDemand_1996_2000_hot),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_hot) / sum(ElectricityDemand_2001_2011_hot),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_hot) / sum(ElectricityDemand_2012_2022_hot),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_hot) / sum(ElectricityDemand_2023_2030_hot)
  ) %>%
  select(
    c(
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
  )

eh_loadprofile_gshp_collector_hot <-
  head(eh_loadprofile_gshp_collector_hot, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2022",
         Type = "GSHP Collector")


eh_loadprofile_gshp_probe_reference <-
  eh_loadprofile_gshp_probe_reference %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_reference) / sum(ElectricityDemand_beginn_1918_reference),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_reference) / sum(ElectricityDemand_1919_1948_reference),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_reference) / sum(ElectricityDemand_1949_1978_reference),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_reference) / sum(ElectricityDemand_1979_1986_reference),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_reference) / sum(ElectricityDemand_1987_1990_reference),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_reference) / sum(ElectricityDemand_1991_1995_reference),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_reference) / sum(ElectricityDemand_1996_2000_reference),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_reference) / sum(ElectricityDemand_2001_2011_reference),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_reference) / sum(ElectricityDemand_2012_2022_reference),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_reference) / sum(ElectricityDemand_2023_2030_reference)
  ) %>%
  select(
    c(
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
  )

eh_loadprofile_gshp_probe_reference <-
  head(eh_loadprofile_gshp_probe_reference, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2017",
         Type = "GSHP Probe")


eh_loadprofile_gshp_probe_cold <- eh_loadprofile_gshp_probe_cold %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_cold) / sum(ElectricityDemand_beginn_1918_cold),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_cold) / sum(ElectricityDemand_1919_1948_cold),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_cold) / sum(ElectricityDemand_1949_1978_cold),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_cold) / sum(ElectricityDemand_1979_1986_cold),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_cold) / sum(ElectricityDemand_1987_1990_cold),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_cold) / sum(ElectricityDemand_1991_1995_cold),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_cold) / sum(ElectricityDemand_1996_2000_cold),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_cold) / sum(ElectricityDemand_2001_2011_cold),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_cold) / sum(ElectricityDemand_2012_2022_cold),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_cold) / sum(ElectricityDemand_2023_2030_cold)
  ) %>%
  select(
    c(
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
  )

eh_loadprofile_gshp_probe_cold <-
  head(eh_loadprofile_gshp_probe_cold, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2010",
         Type = "GSHP Probe")


eh_loadprofile_gshp_probe_hot <- eh_loadprofile_gshp_probe_hot %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_hot) / sum(ElectricityDemand_beginn_1918_hot),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_hot) / sum(ElectricityDemand_1919_1948_hot),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_hot) / sum(ElectricityDemand_1949_1978_hot),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_hot) / sum(ElectricityDemand_1979_1986_hot),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_hot) / sum(ElectricityDemand_1987_1990_hot),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_hot) / sum(ElectricityDemand_1991_1995_hot),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_hot) / sum(ElectricityDemand_1996_2000_hot),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_hot) / sum(ElectricityDemand_2001_2011_hot),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_hot) / sum(ElectricityDemand_2012_2022_hot),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_hot) / sum(ElectricityDemand_2023_2030_hot)
  ) %>%
  select(
    c(
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
  )

eh_loadprofile_gshp_probe_hot <-
  head(eh_loadprofile_gshp_probe_hot, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2022",
         Type = "GSHP Probe")

eh_loadprofile <- eh_loadprofile_ashp_reference %>%
  rbind(eh_loadprofile_ashp_cold) %>%
  rbind(eh_loadprofile_ashp_hot) %>%
  rbind(eh_loadprofile_gshp_collector_reference) %>%
  rbind(eh_loadprofile_gshp_collector_cold) %>%
  rbind(eh_loadprofile_gshp_collector_hot) %>%
  rbind(eh_loadprofile_gshp_probe_reference) %>%
  rbind(eh_loadprofile_gshp_probe_cold) %>%
  rbind(eh_loadprofile_gshp_probe_hot) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Year = as.factor(Year))


# Plot the COP for single-family houses
eh_loadprofile_space_heat_only_cop_plot <- ggplot(eh_loadprofile,
                                                  aes(x = COP,
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
  facet_grid(Year ~ Type) +
  labs(x = "Coefficient of Performance",
       y = "Years of construction")

eh_loadprofile_space_heat_only_cop_plot


# COP of load profiles multi-family houses
mh_loadprofile_ashp_reference <- mh_loadprofile_ashp_reference %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_reference) / sum(ElectricityDemand_beginn_1918_reference),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_reference) / sum(ElectricityDemand_1919_1948_reference),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_reference) / sum(ElectricityDemand_1949_1978_reference),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_reference) / sum(ElectricityDemand_1979_1986_reference),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_reference) / sum(ElectricityDemand_1987_1990_reference),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_reference) / sum(ElectricityDemand_1991_1995_reference),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_reference) / sum(ElectricityDemand_1996_2000_reference),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_reference) / sum(ElectricityDemand_2001_2011_reference),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_reference) / sum(ElectricityDemand_2012_2022_reference),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_reference) / sum(ElectricityDemand_2023_2030_reference)
  ) %>%
  select(
    c(
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
  )

mh_loadprofile_ashp_reference <-
  head(mh_loadprofile_ashp_reference, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2017",
         Type = "ASHP")


mh_loadprofile_ashp_cold <- mh_loadprofile_ashp_cold %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_cold) / sum(ElectricityDemand_beginn_1918_cold),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_cold) / sum(ElectricityDemand_1919_1948_cold),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_cold) / sum(ElectricityDemand_1949_1978_cold),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_cold) / sum(ElectricityDemand_1979_1986_cold),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_cold) / sum(ElectricityDemand_1987_1990_cold),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_cold) / sum(ElectricityDemand_1991_1995_cold),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_cold) / sum(ElectricityDemand_1996_2000_cold),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_cold) / sum(ElectricityDemand_2001_2011_cold),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_cold) / sum(ElectricityDemand_2012_2022_cold),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_cold) / sum(ElectricityDemand_2023_2030_cold)
  ) %>%
  select(
    c(
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
  )

mh_loadprofile_ashp_cold <-
  head(mh_loadprofile_ashp_cold, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2010",
         Type = "ASHP")


mh_loadprofile_ashp_hot <- mh_loadprofile_ashp_hot %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_hot) / sum(ElectricityDemand_beginn_1918_hot),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_hot) / sum(ElectricityDemand_1919_1948_hot),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_hot) / sum(ElectricityDemand_1949_1978_hot),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_hot) / sum(ElectricityDemand_1979_1986_hot),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_hot) / sum(ElectricityDemand_1987_1990_hot),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_hot) / sum(ElectricityDemand_1991_1995_hot),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_hot) / sum(ElectricityDemand_1996_2000_hot),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_hot) / sum(ElectricityDemand_2001_2011_hot),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_hot) / sum(ElectricityDemand_2012_2022_hot),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_hot) / sum(ElectricityDemand_2023_2030_hot)
  ) %>%
  select(
    c(
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
  )

mh_loadprofile_ashp_hot <-
  head(mh_loadprofile_ashp_hot, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2022",
         Type = "ASHP")


mh_loadprofile_gshp_collector_reference <-
  mh_loadprofile_gshp_collector_reference %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_reference) / sum(ElectricityDemand_beginn_1918_reference),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_reference) / sum(ElectricityDemand_1919_1948_reference),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_reference) / sum(ElectricityDemand_1949_1978_reference),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_reference) / sum(ElectricityDemand_1979_1986_reference),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_reference) / sum(ElectricityDemand_1987_1990_reference),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_reference) / sum(ElectricityDemand_1991_1995_reference),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_reference) / sum(ElectricityDemand_1996_2000_reference),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_reference) / sum(ElectricityDemand_2001_2011_reference),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_reference) / sum(ElectricityDemand_2012_2022_reference),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_reference) / sum(ElectricityDemand_2023_2030_reference)
  ) %>%
  select(
    c(
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
  )

mh_loadprofile_gshp_collector_reference <-
  head(mh_loadprofile_gshp_collector_reference, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2017",
         Type = "GSHP Collector")


mh_loadprofile_gshp_collector_cold <-
  mh_loadprofile_gshp_collector_cold %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_cold) / sum(ElectricityDemand_beginn_1918_cold),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_cold) / sum(ElectricityDemand_1919_1948_cold),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_cold) / sum(ElectricityDemand_1949_1978_cold),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_cold) / sum(ElectricityDemand_1979_1986_cold),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_cold) / sum(ElectricityDemand_1987_1990_cold),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_cold) / sum(ElectricityDemand_1991_1995_cold),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_cold) / sum(ElectricityDemand_1996_2000_cold),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_cold) / sum(ElectricityDemand_2001_2011_cold),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_cold) / sum(ElectricityDemand_2012_2022_cold),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_cold) / sum(ElectricityDemand_2023_2030_cold)
  ) %>%
  select(
    c(
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
  )

mh_loadprofile_gshp_collector_cold <-
  head(mh_loadprofile_gshp_collector_cold, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2010",
         Type = "GSHP Collector")


mh_loadprofile_gshp_collector_hot <-
  mh_loadprofile_gshp_collector_hot %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_hot) / sum(ElectricityDemand_beginn_1918_hot),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_hot) / sum(ElectricityDemand_1919_1948_hot),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_hot) / sum(ElectricityDemand_1949_1978_hot),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_hot) / sum(ElectricityDemand_1979_1986_hot),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_hot) / sum(ElectricityDemand_1987_1990_hot),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_hot) / sum(ElectricityDemand_1991_1995_hot),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_hot) / sum(ElectricityDemand_1996_2000_hot),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_hot) / sum(ElectricityDemand_2001_2011_hot),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_hot) / sum(ElectricityDemand_2012_2022_hot),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_hot) / sum(ElectricityDemand_2023_2030_hot)
  ) %>%
  select(
    c(
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
  )

mh_loadprofile_gshp_collector_hot <-
  head(mh_loadprofile_gshp_collector_hot, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2022",
         Type = "GSHP Collector")


mh_loadprofile_gshp_probe_reference <-
  mh_loadprofile_gshp_probe_reference %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_reference) / sum(ElectricityDemand_beginn_1918_reference),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_reference) / sum(ElectricityDemand_1919_1948_reference),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_reference) / sum(ElectricityDemand_1949_1978_reference),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_reference) / sum(ElectricityDemand_1979_1986_reference),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_reference) / sum(ElectricityDemand_1987_1990_reference),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_reference) / sum(ElectricityDemand_1991_1995_reference),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_reference) / sum(ElectricityDemand_1996_2000_reference),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_reference) / sum(ElectricityDemand_2001_2011_reference),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_reference) / sum(ElectricityDemand_2012_2022_reference),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_reference) / sum(ElectricityDemand_2023_2030_reference)
  ) %>%
  select(
    c(
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
  )

mh_loadprofile_gshp_probe_reference <-
  head(mh_loadprofile_gshp_probe_reference, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2017",
         Type = "GSHP Probe")


mh_loadprofile_gshp_probe_cold <- mh_loadprofile_gshp_probe_cold %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_cold) / sum(ElectricityDemand_beginn_1918_cold),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_cold) / sum(ElectricityDemand_1919_1948_cold),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_cold) / sum(ElectricityDemand_1949_1978_cold),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_cold) / sum(ElectricityDemand_1979_1986_cold),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_cold) / sum(ElectricityDemand_1987_1990_cold),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_cold) / sum(ElectricityDemand_1991_1995_cold),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_cold) / sum(ElectricityDemand_1996_2000_cold),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_cold) / sum(ElectricityDemand_2001_2011_cold),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_cold) / sum(ElectricityDemand_2012_2022_cold),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_cold) / sum(ElectricityDemand_2023_2030_cold)
  ) %>%
  select(
    c(
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
  )

mh_loadprofile_gshp_probe_cold <-
  head(mh_loadprofile_gshp_probe_cold, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2010",
         Type = "GSHP Probe")


mh_loadprofile_gshp_probe_hot <- mh_loadprofile_gshp_probe_hot %>%
  mutate(
    "Before 1919" = sum(SpaceHeat_beginn_1918_hot) / sum(ElectricityDemand_beginn_1918_hot),
    "1919 - 1948" = sum(SpaceHeat_1919_1948_hot) / sum(ElectricityDemand_1919_1948_hot),
    "1949 - 1978" = sum(SpaceHeat_1949_1978_hot) / sum(ElectricityDemand_1949_1978_hot),
    "1979 - 1986" = sum(SpaceHeat_1979_1986_hot) / sum(ElectricityDemand_1979_1986_hot),
    "1987 - 1990" = sum(SpaceHeat_1987_1990_hot) / sum(ElectricityDemand_1987_1990_hot),
    "1991 - 1995" = sum(SpaceHeat_1991_1995_hot) / sum(ElectricityDemand_1991_1995_hot),
    "1996 - 2000" = sum(SpaceHeat_1996_2000_hot) / sum(ElectricityDemand_1996_2000_hot),
    "2001 - 2011" = sum(SpaceHeat_2001_2011_hot) / sum(ElectricityDemand_2001_2011_hot),
    "2012 - 2022" = sum(SpaceHeat_2012_2022_hot) / sum(ElectricityDemand_2012_2022_hot),
    "2023 - 2030" = sum(SpaceHeat_2023_2030_hot) / sum(ElectricityDemand_2023_2030_hot)
  ) %>%
  select(
    c(
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
  )

mh_loadprofile_gshp_probe_hot <-
  head(mh_loadprofile_gshp_probe_hot, 1) %>%
  gather("Year of construction",
         "COP",
         1:10) %>%
  mutate(Year = "2022",
         Type = "GSHP Probe")

mh_loadprofile <- mh_loadprofile_ashp_reference %>%
  rbind(mh_loadprofile_ashp_cold) %>%
  rbind(mh_loadprofile_ashp_hot) %>%
  rbind(mh_loadprofile_gshp_collector_reference) %>%
  rbind(mh_loadprofile_gshp_collector_cold) %>%
  rbind(mh_loadprofile_gshp_collector_hot) %>%
  rbind(mh_loadprofile_gshp_probe_reference) %>%
  rbind(mh_loadprofile_gshp_probe_cold) %>%
  rbind(mh_loadprofile_gshp_probe_hot) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Year = as.factor(Year))


# Plot the COP for multi-family houses
mh_loadprofile_space_heat_only_cop_plot <- ggplot(mh_loadprofile,
                                                  aes(x = COP,
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
  facet_grid(Year ~ Type) +
  labs(x = "Coefficient of Performance",
       y = "Years of construction")

mh_loadprofile_space_heat_only_cop_plot


# Save the plots
ggsave(
  "plots/output/loadprofile/eh_loadprofile_sh_only_cop_plot.png",
  eh_loadprofile_space_heat_only_cop_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/loadprofile/mh_loadprofile_sh_only_cop_plot.png",
  mh_loadprofile_space_heat_only_cop_plot,
  width = 30,
  units = "cm"
)
