# Load required packages
library(tidyverse)
library("dplyr")

# Read the data
loadprofile_reference <-
  read_csv("data/output/electricityconsumptionbuildingtypes/loadprofile_reference.csv")

loadprofile_cold <-
  read_csv("data/output/electricityconsumptionbuildingtypes/loadprofile_cold.csv")

loadprofile_hot <-
  read_csv("data/output/electricityconsumptionbuildingtypes/loadprofile_hot.csv")


# Get the data for one and two family houses reference year
loadprofile_reference_otfh_ashp <- loadprofile_reference %>% select(
  c(
    "Time",
    "OneAndTwoFamilyHouses_beginn_1918_ASHP",
    "OneAndTwoFamilyHouses_1919_1948_ASHP",
    "OneAndTwoFamilyHouses_1949_1978_ASHP",
    "OneAndTwoFamilyHouses_1979_1986_ASHP",
    "OneAndTwoFamilyHouses_1987_1990_ASHP",
    "OneAndTwoFamilyHouses_1991_1995_ASHP",
    "OneAndTwoFamilyHouses_1996_2000_ASHP",
    "OneAndTwoFamilyHouses_2001_2011_ASHP",
    "OneAndTwoFamilyHouses_2012_2022_ASHP",
    "OneAndTwoFamilyHouses_2023_2030_ASHP"
  )
) %>% rename(
  "Before 1919" = "OneAndTwoFamilyHouses_beginn_1918_ASHP",
  "1919 - 1948" = "OneAndTwoFamilyHouses_1919_1948_ASHP",
  "1949 - 1978" = "OneAndTwoFamilyHouses_1949_1978_ASHP",
  "1979 - 1986" = "OneAndTwoFamilyHouses_1979_1986_ASHP",
  "1987 - 1990" = "OneAndTwoFamilyHouses_1987_1990_ASHP",
  "1991 - 1995" = "OneAndTwoFamilyHouses_1991_1995_ASHP",
  "1996 - 2000" = "OneAndTwoFamilyHouses_1996_2000_ASHP",
  "2001 - 2011" = "OneAndTwoFamilyHouses_2001_2011_ASHP",
  "2012 - 2022" = "OneAndTwoFamilyHouses_2012_2022_ASHP",
  "2023 - 2030" = "OneAndTwoFamilyHouses_2023_2030_ASHP"
) %>% mutate(Type = "ASHP")

loadprofile_reference_otfh_gshp_collector <-
  loadprofile_reference %>% select(
    c(
      "Time",
      "OneAndTwoFamilyHouses_beginn_1918_GSHP_Collector",
      "OneAndTwoFamilyHouses_1919_1948_GSHP_Collector",
      "OneAndTwoFamilyHouses_1949_1978_GSHP_Collector",
      "OneAndTwoFamilyHouses_1979_1986_GSHP_Collector",
      "OneAndTwoFamilyHouses_1987_1990_GSHP_Collector",
      "OneAndTwoFamilyHouses_1991_1995_GSHP_Collector",
      "OneAndTwoFamilyHouses_1996_2000_GSHP_Collector",
      "OneAndTwoFamilyHouses_2001_2011_GSHP_Collector",
      "OneAndTwoFamilyHouses_2012_2022_GSHP_Collector",
      "OneAndTwoFamilyHouses_2023_2030_GSHP_Collector"
    )
  ) %>% rename(
    "Before 1919" = "OneAndTwoFamilyHouses_beginn_1918_GSHP_Collector",
    "1919 - 1948" = "OneAndTwoFamilyHouses_1919_1948_GSHP_Collector",
    "1949 - 1978" = "OneAndTwoFamilyHouses_1949_1978_GSHP_Collector",
    "1979 - 1986" = "OneAndTwoFamilyHouses_1979_1986_GSHP_Collector",
    "1987 - 1990" = "OneAndTwoFamilyHouses_1987_1990_GSHP_Collector",
    "1991 - 1995" = "OneAndTwoFamilyHouses_1991_1995_GSHP_Collector",
    "1996 - 2000" = "OneAndTwoFamilyHouses_1996_2000_GSHP_Collector",
    "2001 - 2011" = "OneAndTwoFamilyHouses_2001_2011_GSHP_Collector",
    "2012 - 2022" = "OneAndTwoFamilyHouses_2012_2022_GSHP_Collector",
    "2023 - 2030" = "OneAndTwoFamilyHouses_2023_2030_GSHP_Collector"
  ) %>% mutate(Type = "GSHP Collector")

loadprofile_reference_otfh_gshp_probe <-
  loadprofile_reference %>% select(
    c(
      "Time",
      "OneAndTwoFamilyHouses_beginn_1918_GSHP_Probe",
      "OneAndTwoFamilyHouses_1919_1948_GSHP_Probe",
      "OneAndTwoFamilyHouses_1949_1978_GSHP_Probe",
      "OneAndTwoFamilyHouses_1979_1986_GSHP_Probe",
      "OneAndTwoFamilyHouses_1987_1990_GSHP_Probe",
      "OneAndTwoFamilyHouses_1991_1995_GSHP_Probe",
      "OneAndTwoFamilyHouses_1996_2000_GSHP_Probe",
      "OneAndTwoFamilyHouses_2001_2011_GSHP_Probe",
      "OneAndTwoFamilyHouses_2012_2022_GSHP_Probe",
      "OneAndTwoFamilyHouses_2023_2030_GSHP_Probe"
    )
  ) %>% rename(
    "Before 1919" = "OneAndTwoFamilyHouses_beginn_1918_GSHP_Probe",
    "1919 - 1948" = "OneAndTwoFamilyHouses_1919_1948_GSHP_Probe",
    "1949 - 1978" = "OneAndTwoFamilyHouses_1949_1978_GSHP_Probe",
    "1979 - 1986" = "OneAndTwoFamilyHouses_1979_1986_GSHP_Probe",
    "1987 - 1990" = "OneAndTwoFamilyHouses_1987_1990_GSHP_Probe",
    "1991 - 1995" = "OneAndTwoFamilyHouses_1991_1995_GSHP_Probe",
    "1996 - 2000" = "OneAndTwoFamilyHouses_1996_2000_GSHP_Probe",
    "2001 - 2011" = "OneAndTwoFamilyHouses_2001_2011_GSHP_Probe",
    "2012 - 2022" = "OneAndTwoFamilyHouses_2012_2022_GSHP_Probe",
    "2023 - 2030" = "OneAndTwoFamilyHouses_2023_2030_GSHP_Probe"
  ) %>% mutate(Type = "GSHP Probe")

loadprofile_reference_otfh <- loadprofile_reference_otfh_ashp %>%
  rbind(loadprofile_reference_otfh_gshp_collector) %>%
  rbind(loadprofile_reference_otfh_gshp_probe)

loadprofile_reference_otfh <- loadprofile_reference_otfh %>%
  mutate(DATEISO = ISOdate(
    2017,
    ifelse(substr(Time, 4, 5) == "00", 0, sub("^0+", "", substr(Time, 4, 5))),
    ifelse(substr(Time, 1, 2) == "00", 0, sub("^0+", "", substr(Time, 1, 2))),
    ifelse(substr(Time, 7, 8) == "00", 0, sub("^0+", "", substr(Time, 7, 8)))
  ))

loadprofile_reference_otfh_selected <-
  loadprofile_reference_otfh %>% select(c("DATEISO",
                                          "Type",
                                          "1979 - 1986",
                                          "2001 - 2011")) %>% gather("Year of construction",
                                                                     "Heat pump electricity demand",
                                                                     3:4) %>% mutate_if(is.character, as.factor)



hp_loadprofile_reference_otfh_selected_plot <-
  ggplot(data = loadprofile_reference_otfh_selected,
         aes(DATEISO,
             `Heat pump electricity demand`,)) +
  geom_line(lwd = 0.5) +
  facet_grid(Type ~ `Year of construction`) +
  coord_cartesian(ylim = c(0, 8)) +
  labs(x = "Date",
       y = "Electricity demand in kWh")


hp_loadprofile_reference_otfh_selected_plot


# Get the data for apartment buildings 3-6 reference year
loadprofile_reference_ab36_ashp <- loadprofile_reference %>% select(
  c(
    "Time",
    "ApartmentBuildings36_beginn_1918_ASHP",
    "ApartmentBuildings36_1919_1948_ASHP",
    "ApartmentBuildings36_1949_1978_ASHP",
    "ApartmentBuildings36_1979_1986_ASHP",
    "ApartmentBuildings36_1987_1990_ASHP",
    "ApartmentBuildings36_1991_1995_ASHP",
    "ApartmentBuildings36_1996_2000_ASHP",
    "ApartmentBuildings36_2001_2011_ASHP",
    "ApartmentBuildings36_2012_2022_ASHP",
    "ApartmentBuildings36_2023_2030_ASHP"
  )
) %>% rename(
  "Before 1919" = "ApartmentBuildings36_beginn_1918_ASHP",
  "1919 - 1948" = "ApartmentBuildings36_1919_1948_ASHP",
  "1949 - 1978" = "ApartmentBuildings36_1949_1978_ASHP",
  "1979 - 1986" = "ApartmentBuildings36_1979_1986_ASHP",
  "1987 - 1990" = "ApartmentBuildings36_1987_1990_ASHP",
  "1991 - 1995" = "ApartmentBuildings36_1991_1995_ASHP",
  "1996 - 2000" = "ApartmentBuildings36_1996_2000_ASHP",
  "2001 - 2011" = "ApartmentBuildings36_2001_2011_ASHP",
  "2012 - 2022" = "ApartmentBuildings36_2012_2022_ASHP",
  "2023 - 2030" = "ApartmentBuildings36_2023_2030_ASHP"
) %>% mutate(Type = "ASHP")

loadprofile_reference_ab36_gshp_collector <-
  loadprofile_reference %>% select(
    c(
      "Time",
      "ApartmentBuildings36_beginn_1918_GSHP_Collector",
      "ApartmentBuildings36_1919_1948_GSHP_Collector",
      "ApartmentBuildings36_1949_1978_GSHP_Collector",
      "ApartmentBuildings36_1979_1986_GSHP_Collector",
      "ApartmentBuildings36_1987_1990_GSHP_Collector",
      "ApartmentBuildings36_1991_1995_GSHP_Collector",
      "ApartmentBuildings36_1996_2000_GSHP_Collector",
      "ApartmentBuildings36_2001_2011_GSHP_Collector",
      "ApartmentBuildings36_2012_2022_GSHP_Collector",
      "ApartmentBuildings36_2023_2030_GSHP_Collector"
    )
  ) %>% rename(
    "Before 1919" = "ApartmentBuildings36_beginn_1918_GSHP_Collector",
    "1919 - 1948" = "ApartmentBuildings36_1919_1948_GSHP_Collector",
    "1949 - 1978" = "ApartmentBuildings36_1949_1978_GSHP_Collector",
    "1979 - 1986" = "ApartmentBuildings36_1979_1986_GSHP_Collector",
    "1987 - 1990" = "ApartmentBuildings36_1987_1990_GSHP_Collector",
    "1991 - 1995" = "ApartmentBuildings36_1991_1995_GSHP_Collector",
    "1996 - 2000" = "ApartmentBuildings36_1996_2000_GSHP_Collector",
    "2001 - 2011" = "ApartmentBuildings36_2001_2011_GSHP_Collector",
    "2012 - 2022" = "ApartmentBuildings36_2012_2022_GSHP_Collector",
    "2023 - 2030" = "ApartmentBuildings36_2023_2030_GSHP_Collector"
  ) %>% mutate(Type = "GSHP Collector")

loadprofile_reference_ab36_gshp_probe <-
  loadprofile_reference %>% select(
    c(
      "Time",
      "ApartmentBuildings36_beginn_1918_GSHP_Probe",
      "ApartmentBuildings36_1919_1948_GSHP_Probe",
      "ApartmentBuildings36_1949_1978_GSHP_Probe",
      "ApartmentBuildings36_1979_1986_GSHP_Probe",
      "ApartmentBuildings36_1987_1990_GSHP_Probe",
      "ApartmentBuildings36_1991_1995_GSHP_Probe",
      "ApartmentBuildings36_1996_2000_GSHP_Probe",
      "ApartmentBuildings36_2001_2011_GSHP_Probe",
      "ApartmentBuildings36_2012_2022_GSHP_Probe",
      "ApartmentBuildings36_2023_2030_GSHP_Probe"
    )
  ) %>% rename(
    "Before 1919" = "ApartmentBuildings36_beginn_1918_GSHP_Probe",
    "1919 - 1948" = "ApartmentBuildings36_1919_1948_GSHP_Probe",
    "1949 - 1978" = "ApartmentBuildings36_1949_1978_GSHP_Probe",
    "1979 - 1986" = "ApartmentBuildings36_1979_1986_GSHP_Probe",
    "1987 - 1990" = "ApartmentBuildings36_1987_1990_GSHP_Probe",
    "1991 - 1995" = "ApartmentBuildings36_1991_1995_GSHP_Probe",
    "1996 - 2000" = "ApartmentBuildings36_1996_2000_GSHP_Probe",
    "2001 - 2011" = "ApartmentBuildings36_2001_2011_GSHP_Probe",
    "2012 - 2022" = "ApartmentBuildings36_2012_2022_GSHP_Probe",
    "2023 - 2030" = "ApartmentBuildings36_2023_2030_GSHP_Probe"
  ) %>% mutate(Type = "GSHP Probe")

loadprofile_reference_ab36 <- loadprofile_reference_ab36_ashp %>%
  rbind(loadprofile_reference_ab36_gshp_collector) %>%
  rbind(loadprofile_reference_ab36_gshp_probe)

loadprofile_reference_ab36 <- loadprofile_reference_ab36 %>%
  mutate(DATEISO = ISOdate(
    2017,
    ifelse(substr(Time, 4, 5) == "00", 0, sub("^0+", "", substr(Time, 4, 5))),
    ifelse(substr(Time, 1, 2) == "00", 0, sub("^0+", "", substr(Time, 1, 2))),
    ifelse(substr(Time, 7, 8) == "00", 0, sub("^0+", "", substr(Time, 7, 8)))
  ))

loadprofile_reference_ab36_selected <-
  loadprofile_reference_ab36 %>% select(c("DATEISO",
                                          "Type",
                                          "1979 - 1986",
                                          "2001 - 2011")) %>% gather("Year of construction",
                                                                     "Heat pump electricity demand",
                                                                     3:4) %>% mutate_if(is.character, as.factor)



hp_loadprofile_reference_ab36_selected_plot <-
  ggplot(data = loadprofile_reference_ab36_selected,
         aes(DATEISO,
             `Heat pump electricity demand`,)) +
  geom_line(lwd = 0.5) +
  facet_grid(Type ~ `Year of construction`) +
  coord_cartesian(ylim = c(0, 16)) +
  labs(x = "Date",
       y = "Electricity demand in kWh")


hp_loadprofile_reference_ab36_selected_plot


# Get the data for one and two family houses cold year
loadprofile_cold_otfh_ashp <- loadprofile_cold %>% select(
  c(
    "Time",
    "OneAndTwoFamilyHouses_beginn_1918_ASHP",
    "OneAndTwoFamilyHouses_1919_1948_ASHP",
    "OneAndTwoFamilyHouses_1949_1978_ASHP",
    "OneAndTwoFamilyHouses_1979_1986_ASHP",
    "OneAndTwoFamilyHouses_1987_1990_ASHP",
    "OneAndTwoFamilyHouses_1991_1995_ASHP",
    "OneAndTwoFamilyHouses_1996_2000_ASHP",
    "OneAndTwoFamilyHouses_2001_2011_ASHP",
    "OneAndTwoFamilyHouses_2012_2022_ASHP",
    "OneAndTwoFamilyHouses_2023_2030_ASHP"
  )
) %>% rename(
  "Before 1919" = "OneAndTwoFamilyHouses_beginn_1918_ASHP",
  "1919 - 1948" = "OneAndTwoFamilyHouses_1919_1948_ASHP",
  "1949 - 1978" = "OneAndTwoFamilyHouses_1949_1978_ASHP",
  "1979 - 1986" = "OneAndTwoFamilyHouses_1979_1986_ASHP",
  "1987 - 1990" = "OneAndTwoFamilyHouses_1987_1990_ASHP",
  "1991 - 1995" = "OneAndTwoFamilyHouses_1991_1995_ASHP",
  "1996 - 2000" = "OneAndTwoFamilyHouses_1996_2000_ASHP",
  "2001 - 2011" = "OneAndTwoFamilyHouses_2001_2011_ASHP",
  "2012 - 2022" = "OneAndTwoFamilyHouses_2012_2022_ASHP",
  "2023 - 2030" = "OneAndTwoFamilyHouses_2023_2030_ASHP"
) %>% mutate(Type = "ASHP")

loadprofile_cold_otfh_gshp_collector <-
  loadprofile_cold %>% select(
    c(
      "Time",
      "OneAndTwoFamilyHouses_beginn_1918_GSHP_Collector",
      "OneAndTwoFamilyHouses_1919_1948_GSHP_Collector",
      "OneAndTwoFamilyHouses_1949_1978_GSHP_Collector",
      "OneAndTwoFamilyHouses_1979_1986_GSHP_Collector",
      "OneAndTwoFamilyHouses_1987_1990_GSHP_Collector",
      "OneAndTwoFamilyHouses_1991_1995_GSHP_Collector",
      "OneAndTwoFamilyHouses_1996_2000_GSHP_Collector",
      "OneAndTwoFamilyHouses_2001_2011_GSHP_Collector",
      "OneAndTwoFamilyHouses_2012_2022_GSHP_Collector",
      "OneAndTwoFamilyHouses_2023_2030_GSHP_Collector"
    )
  ) %>% rename(
    "Before 1919" = "OneAndTwoFamilyHouses_beginn_1918_GSHP_Collector",
    "1919 - 1948" = "OneAndTwoFamilyHouses_1919_1948_GSHP_Collector",
    "1949 - 1978" = "OneAndTwoFamilyHouses_1949_1978_GSHP_Collector",
    "1979 - 1986" = "OneAndTwoFamilyHouses_1979_1986_GSHP_Collector",
    "1987 - 1990" = "OneAndTwoFamilyHouses_1987_1990_GSHP_Collector",
    "1991 - 1995" = "OneAndTwoFamilyHouses_1991_1995_GSHP_Collector",
    "1996 - 2000" = "OneAndTwoFamilyHouses_1996_2000_GSHP_Collector",
    "2001 - 2011" = "OneAndTwoFamilyHouses_2001_2011_GSHP_Collector",
    "2012 - 2022" = "OneAndTwoFamilyHouses_2012_2022_GSHP_Collector",
    "2023 - 2030" = "OneAndTwoFamilyHouses_2023_2030_GSHP_Collector"
  ) %>% mutate(Type = "GSHP Collector")

loadprofile_cold_otfh_gshp_probe <-
  loadprofile_cold %>% select(
    c(
      "Time",
      "OneAndTwoFamilyHouses_beginn_1918_GSHP_Probe",
      "OneAndTwoFamilyHouses_1919_1948_GSHP_Probe",
      "OneAndTwoFamilyHouses_1949_1978_GSHP_Probe",
      "OneAndTwoFamilyHouses_1979_1986_GSHP_Probe",
      "OneAndTwoFamilyHouses_1987_1990_GSHP_Probe",
      "OneAndTwoFamilyHouses_1991_1995_GSHP_Probe",
      "OneAndTwoFamilyHouses_1996_2000_GSHP_Probe",
      "OneAndTwoFamilyHouses_2001_2011_GSHP_Probe",
      "OneAndTwoFamilyHouses_2012_2022_GSHP_Probe",
      "OneAndTwoFamilyHouses_2023_2030_GSHP_Probe"
    )
  ) %>% rename(
    "Before 1919" = "OneAndTwoFamilyHouses_beginn_1918_GSHP_Probe",
    "1919 - 1948" = "OneAndTwoFamilyHouses_1919_1948_GSHP_Probe",
    "1949 - 1978" = "OneAndTwoFamilyHouses_1949_1978_GSHP_Probe",
    "1979 - 1986" = "OneAndTwoFamilyHouses_1979_1986_GSHP_Probe",
    "1987 - 1990" = "OneAndTwoFamilyHouses_1987_1990_GSHP_Probe",
    "1991 - 1995" = "OneAndTwoFamilyHouses_1991_1995_GSHP_Probe",
    "1996 - 2000" = "OneAndTwoFamilyHouses_1996_2000_GSHP_Probe",
    "2001 - 2011" = "OneAndTwoFamilyHouses_2001_2011_GSHP_Probe",
    "2012 - 2022" = "OneAndTwoFamilyHouses_2012_2022_GSHP_Probe",
    "2023 - 2030" = "OneAndTwoFamilyHouses_2023_2030_GSHP_Probe"
  ) %>% mutate(Type = "GSHP Probe")

loadprofile_cold_otfh <- loadprofile_cold_otfh_ashp %>%
  rbind(loadprofile_cold_otfh_gshp_collector) %>%
  rbind(loadprofile_cold_otfh_gshp_probe)

loadprofile_cold_otfh <- loadprofile_cold_otfh %>%
  mutate(DATEISO = ISOdate(
    2010,
    ifelse(substr(Time, 4, 5) == "00", 0, sub("^0+", "", substr(Time, 4, 5))),
    ifelse(substr(Time, 1, 2) == "00", 0, sub("^0+", "", substr(Time, 1, 2))),
    ifelse(substr(Time, 7, 8) == "00", 0, sub("^0+", "", substr(Time, 7, 8)))
  ))

loadprofile_cold_otfh_selected <-
  loadprofile_cold_otfh %>% select(c("DATEISO",
                                     "Type",
                                     "1979 - 1986",
                                     "2001 - 2011")) %>% gather("Year of construction",
                                                                "Heat pump electricity demand",
                                                                3:4) %>% mutate_if(is.character, as.factor)



hp_loadprofile_cold_otfh_selected_plot <-
  ggplot(data = loadprofile_cold_otfh_selected,
         aes(DATEISO,
             `Heat pump electricity demand`,)) +
  geom_line(lwd = 0.5) +
  facet_grid(Type ~ `Year of construction`) +
  coord_cartesian(ylim = c(0, 8)) +
  labs(x = "Date",
       y = "Electricity demand in kWh")


hp_loadprofile_cold_otfh_selected_plot


# Get the data for apartment buildings 3-6 cold year
loadprofile_cold_ab36_ashp <- loadprofile_cold %>% select(
  c(
    "Time",
    "ApartmentBuildings36_beginn_1918_ASHP",
    "ApartmentBuildings36_1919_1948_ASHP",
    "ApartmentBuildings36_1949_1978_ASHP",
    "ApartmentBuildings36_1979_1986_ASHP",
    "ApartmentBuildings36_1987_1990_ASHP",
    "ApartmentBuildings36_1991_1995_ASHP",
    "ApartmentBuildings36_1996_2000_ASHP",
    "ApartmentBuildings36_2001_2011_ASHP",
    "ApartmentBuildings36_2012_2022_ASHP",
    "ApartmentBuildings36_2023_2030_ASHP"
  )
) %>% rename(
  "Before 1919" = "ApartmentBuildings36_beginn_1918_ASHP",
  "1919 - 1948" = "ApartmentBuildings36_1919_1948_ASHP",
  "1949 - 1978" = "ApartmentBuildings36_1949_1978_ASHP",
  "1979 - 1986" = "ApartmentBuildings36_1979_1986_ASHP",
  "1987 - 1990" = "ApartmentBuildings36_1987_1990_ASHP",
  "1991 - 1995" = "ApartmentBuildings36_1991_1995_ASHP",
  "1996 - 2000" = "ApartmentBuildings36_1996_2000_ASHP",
  "2001 - 2011" = "ApartmentBuildings36_2001_2011_ASHP",
  "2012 - 2022" = "ApartmentBuildings36_2012_2022_ASHP",
  "2023 - 2030" = "ApartmentBuildings36_2023_2030_ASHP"
) %>% mutate(Type = "ASHP")

loadprofile_cold_ab36_gshp_collector <-
  loadprofile_cold %>% select(
    c(
      "Time",
      "ApartmentBuildings36_beginn_1918_GSHP_Collector",
      "ApartmentBuildings36_1919_1948_GSHP_Collector",
      "ApartmentBuildings36_1949_1978_GSHP_Collector",
      "ApartmentBuildings36_1979_1986_GSHP_Collector",
      "ApartmentBuildings36_1987_1990_GSHP_Collector",
      "ApartmentBuildings36_1991_1995_GSHP_Collector",
      "ApartmentBuildings36_1996_2000_GSHP_Collector",
      "ApartmentBuildings36_2001_2011_GSHP_Collector",
      "ApartmentBuildings36_2012_2022_GSHP_Collector",
      "ApartmentBuildings36_2023_2030_GSHP_Collector"
    )
  ) %>% rename(
    "Before 1919" = "ApartmentBuildings36_beginn_1918_GSHP_Collector",
    "1919 - 1948" = "ApartmentBuildings36_1919_1948_GSHP_Collector",
    "1949 - 1978" = "ApartmentBuildings36_1949_1978_GSHP_Collector",
    "1979 - 1986" = "ApartmentBuildings36_1979_1986_GSHP_Collector",
    "1987 - 1990" = "ApartmentBuildings36_1987_1990_GSHP_Collector",
    "1991 - 1995" = "ApartmentBuildings36_1991_1995_GSHP_Collector",
    "1996 - 2000" = "ApartmentBuildings36_1996_2000_GSHP_Collector",
    "2001 - 2011" = "ApartmentBuildings36_2001_2011_GSHP_Collector",
    "2012 - 2022" = "ApartmentBuildings36_2012_2022_GSHP_Collector",
    "2023 - 2030" = "ApartmentBuildings36_2023_2030_GSHP_Collector"
  ) %>% mutate(Type = "GSHP Collector")

loadprofile_cold_ab36_gshp_probe <-
  loadprofile_cold %>% select(
    c(
      "Time",
      "ApartmentBuildings36_beginn_1918_GSHP_Probe",
      "ApartmentBuildings36_1919_1948_GSHP_Probe",
      "ApartmentBuildings36_1949_1978_GSHP_Probe",
      "ApartmentBuildings36_1979_1986_GSHP_Probe",
      "ApartmentBuildings36_1987_1990_GSHP_Probe",
      "ApartmentBuildings36_1991_1995_GSHP_Probe",
      "ApartmentBuildings36_1996_2000_GSHP_Probe",
      "ApartmentBuildings36_2001_2011_GSHP_Probe",
      "ApartmentBuildings36_2012_2022_GSHP_Probe",
      "ApartmentBuildings36_2023_2030_GSHP_Probe"
    )
  ) %>% rename(
    "Before 1919" = "ApartmentBuildings36_beginn_1918_GSHP_Probe",
    "1919 - 1948" = "ApartmentBuildings36_1919_1948_GSHP_Probe",
    "1949 - 1978" = "ApartmentBuildings36_1949_1978_GSHP_Probe",
    "1979 - 1986" = "ApartmentBuildings36_1979_1986_GSHP_Probe",
    "1987 - 1990" = "ApartmentBuildings36_1987_1990_GSHP_Probe",
    "1991 - 1995" = "ApartmentBuildings36_1991_1995_GSHP_Probe",
    "1996 - 2000" = "ApartmentBuildings36_1996_2000_GSHP_Probe",
    "2001 - 2011" = "ApartmentBuildings36_2001_2011_GSHP_Probe",
    "2012 - 2022" = "ApartmentBuildings36_2012_2022_GSHP_Probe",
    "2023 - 2030" = "ApartmentBuildings36_2023_2030_GSHP_Probe"
  ) %>% mutate(Type = "GSHP Probe")

loadprofile_cold_ab36 <- loadprofile_cold_ab36_ashp %>%
  rbind(loadprofile_cold_ab36_gshp_collector) %>%
  rbind(loadprofile_cold_ab36_gshp_probe)

loadprofile_cold_ab36 <- loadprofile_cold_ab36 %>%
  mutate(DATEISO = ISOdate(
    2010,
    ifelse(substr(Time, 4, 5) == "00", 0, sub("^0+", "", substr(Time, 4, 5))),
    ifelse(substr(Time, 1, 2) == "00", 0, sub("^0+", "", substr(Time, 1, 2))),
    ifelse(substr(Time, 7, 8) == "00", 0, sub("^0+", "", substr(Time, 7, 8)))
  ))

loadprofile_cold_ab36_selected <-
  loadprofile_cold_ab36 %>% select(c("DATEISO",
                                     "Type",
                                     "1979 - 1986",
                                     "2001 - 2011")) %>% gather("Year of construction",
                                                                "Heat pump electricity demand",
                                                                3:4) %>% mutate_if(is.character, as.factor)



hp_loadprofile_cold_ab36_selected_plot <-
  ggplot(data = loadprofile_cold_ab36_selected,
         aes(DATEISO,
             `Heat pump electricity demand`,)) +
  geom_line(lwd = 0.5) +
  facet_grid(Type ~ `Year of construction`) +
  coord_cartesian(ylim = c(0, 16)) +
  labs(x = "Date",
       y = "Electricity demand in kWh")


hp_loadprofile_cold_ab36_selected_plot


# Get the data for one and two family houses hot year
loadprofile_hot_otfh_ashp <- loadprofile_hot %>% select(
  c(
    "Time",
    "OneAndTwoFamilyHouses_beginn_1918_ASHP",
    "OneAndTwoFamilyHouses_1919_1948_ASHP",
    "OneAndTwoFamilyHouses_1949_1978_ASHP",
    "OneAndTwoFamilyHouses_1979_1986_ASHP",
    "OneAndTwoFamilyHouses_1987_1990_ASHP",
    "OneAndTwoFamilyHouses_1991_1995_ASHP",
    "OneAndTwoFamilyHouses_1996_2000_ASHP",
    "OneAndTwoFamilyHouses_2001_2011_ASHP",
    "OneAndTwoFamilyHouses_2012_2022_ASHP",
    "OneAndTwoFamilyHouses_2023_2030_ASHP"
  )
) %>% rename(
  "Before 1919" = "OneAndTwoFamilyHouses_beginn_1918_ASHP",
  "1919 - 1948" = "OneAndTwoFamilyHouses_1919_1948_ASHP",
  "1949 - 1978" = "OneAndTwoFamilyHouses_1949_1978_ASHP",
  "1979 - 1986" = "OneAndTwoFamilyHouses_1979_1986_ASHP",
  "1987 - 1990" = "OneAndTwoFamilyHouses_1987_1990_ASHP",
  "1991 - 1995" = "OneAndTwoFamilyHouses_1991_1995_ASHP",
  "1996 - 2000" = "OneAndTwoFamilyHouses_1996_2000_ASHP",
  "2001 - 2011" = "OneAndTwoFamilyHouses_2001_2011_ASHP",
  "2012 - 2022" = "OneAndTwoFamilyHouses_2012_2022_ASHP",
  "2023 - 2030" = "OneAndTwoFamilyHouses_2023_2030_ASHP"
) %>% mutate(Type = "ASHP")

loadprofile_hot_otfh_gshp_collector <-
  loadprofile_hot %>% select(
    c(
      "Time",
      "OneAndTwoFamilyHouses_beginn_1918_GSHP_Collector",
      "OneAndTwoFamilyHouses_1919_1948_GSHP_Collector",
      "OneAndTwoFamilyHouses_1949_1978_GSHP_Collector",
      "OneAndTwoFamilyHouses_1979_1986_GSHP_Collector",
      "OneAndTwoFamilyHouses_1987_1990_GSHP_Collector",
      "OneAndTwoFamilyHouses_1991_1995_GSHP_Collector",
      "OneAndTwoFamilyHouses_1996_2000_GSHP_Collector",
      "OneAndTwoFamilyHouses_2001_2011_GSHP_Collector",
      "OneAndTwoFamilyHouses_2012_2022_GSHP_Collector",
      "OneAndTwoFamilyHouses_2023_2030_GSHP_Collector"
    )
  ) %>% rename(
    "Before 1919" = "OneAndTwoFamilyHouses_beginn_1918_GSHP_Collector",
    "1919 - 1948" = "OneAndTwoFamilyHouses_1919_1948_GSHP_Collector",
    "1949 - 1978" = "OneAndTwoFamilyHouses_1949_1978_GSHP_Collector",
    "1979 - 1986" = "OneAndTwoFamilyHouses_1979_1986_GSHP_Collector",
    "1987 - 1990" = "OneAndTwoFamilyHouses_1987_1990_GSHP_Collector",
    "1991 - 1995" = "OneAndTwoFamilyHouses_1991_1995_GSHP_Collector",
    "1996 - 2000" = "OneAndTwoFamilyHouses_1996_2000_GSHP_Collector",
    "2001 - 2011" = "OneAndTwoFamilyHouses_2001_2011_GSHP_Collector",
    "2012 - 2022" = "OneAndTwoFamilyHouses_2012_2022_GSHP_Collector",
    "2023 - 2030" = "OneAndTwoFamilyHouses_2023_2030_GSHP_Collector"
  ) %>% mutate(Type = "GSHP Collector")

loadprofile_hot_otfh_gshp_probe <-
  loadprofile_hot %>% select(
    c(
      "Time",
      "OneAndTwoFamilyHouses_beginn_1918_GSHP_Probe",
      "OneAndTwoFamilyHouses_1919_1948_GSHP_Probe",
      "OneAndTwoFamilyHouses_1949_1978_GSHP_Probe",
      "OneAndTwoFamilyHouses_1979_1986_GSHP_Probe",
      "OneAndTwoFamilyHouses_1987_1990_GSHP_Probe",
      "OneAndTwoFamilyHouses_1991_1995_GSHP_Probe",
      "OneAndTwoFamilyHouses_1996_2000_GSHP_Probe",
      "OneAndTwoFamilyHouses_2001_2011_GSHP_Probe",
      "OneAndTwoFamilyHouses_2012_2022_GSHP_Probe",
      "OneAndTwoFamilyHouses_2023_2030_GSHP_Probe"
    )
  ) %>% rename(
    "Before 1919" = "OneAndTwoFamilyHouses_beginn_1918_GSHP_Probe",
    "1919 - 1948" = "OneAndTwoFamilyHouses_1919_1948_GSHP_Probe",
    "1949 - 1978" = "OneAndTwoFamilyHouses_1949_1978_GSHP_Probe",
    "1979 - 1986" = "OneAndTwoFamilyHouses_1979_1986_GSHP_Probe",
    "1987 - 1990" = "OneAndTwoFamilyHouses_1987_1990_GSHP_Probe",
    "1991 - 1995" = "OneAndTwoFamilyHouses_1991_1995_GSHP_Probe",
    "1996 - 2000" = "OneAndTwoFamilyHouses_1996_2000_GSHP_Probe",
    "2001 - 2011" = "OneAndTwoFamilyHouses_2001_2011_GSHP_Probe",
    "2012 - 2022" = "OneAndTwoFamilyHouses_2012_2022_GSHP_Probe",
    "2023 - 2030" = "OneAndTwoFamilyHouses_2023_2030_GSHP_Probe"
  ) %>% mutate(Type = "GSHP Probe")

loadprofile_hot_otfh <- loadprofile_hot_otfh_ashp %>%
  rbind(loadprofile_hot_otfh_gshp_collector) %>%
  rbind(loadprofile_hot_otfh_gshp_probe)

loadprofile_hot_otfh <- loadprofile_hot_otfh %>%
  mutate(DATEISO = ISOdate(
    2022,
    ifelse(substr(Time, 4, 5) == "00", 0, sub("^0+", "", substr(Time, 4, 5))),
    ifelse(substr(Time, 1, 2) == "00", 0, sub("^0+", "", substr(Time, 1, 2))),
    ifelse(substr(Time, 7, 8) == "00", 0, sub("^0+", "", substr(Time, 7, 8)))
  ))

loadprofile_hot_otfh_selected <-
  loadprofile_hot_otfh %>% select(c("DATEISO",
                                    "Type",
                                    "1979 - 1986",
                                    "2001 - 2011")) %>% gather("Year of construction",
                                                               "Heat pump electricity demand",
                                                               3:4) %>% mutate_if(is.character, as.factor)



hp_loadprofile_hot_otfh_selected_plot <-
  ggplot(data = loadprofile_hot_otfh_selected,
         aes(DATEISO,
             `Heat pump electricity demand`,)) +
  geom_line(lwd = 0.5) +
  facet_grid(Type ~ `Year of construction`) +
  coord_cartesian(ylim = c(0, 8)) +
  labs(x = "Date",
       y = "Electricity demand in kWh")


hp_loadprofile_hot_otfh_selected_plot


# Get the data for apartment buildings 3-6 hot year
loadprofile_hot_ab36_ashp <- loadprofile_hot %>% select(
  c(
    "Time",
    "ApartmentBuildings36_beginn_1918_ASHP",
    "ApartmentBuildings36_1919_1948_ASHP",
    "ApartmentBuildings36_1949_1978_ASHP",
    "ApartmentBuildings36_1979_1986_ASHP",
    "ApartmentBuildings36_1987_1990_ASHP",
    "ApartmentBuildings36_1991_1995_ASHP",
    "ApartmentBuildings36_1996_2000_ASHP",
    "ApartmentBuildings36_2001_2011_ASHP",
    "ApartmentBuildings36_2012_2022_ASHP",
    "ApartmentBuildings36_2023_2030_ASHP"
  )
) %>% rename(
  "Before 1919" = "ApartmentBuildings36_beginn_1918_ASHP",
  "1919 - 1948" = "ApartmentBuildings36_1919_1948_ASHP",
  "1949 - 1978" = "ApartmentBuildings36_1949_1978_ASHP",
  "1979 - 1986" = "ApartmentBuildings36_1979_1986_ASHP",
  "1987 - 1990" = "ApartmentBuildings36_1987_1990_ASHP",
  "1991 - 1995" = "ApartmentBuildings36_1991_1995_ASHP",
  "1996 - 2000" = "ApartmentBuildings36_1996_2000_ASHP",
  "2001 - 2011" = "ApartmentBuildings36_2001_2011_ASHP",
  "2012 - 2022" = "ApartmentBuildings36_2012_2022_ASHP",
  "2023 - 2030" = "ApartmentBuildings36_2023_2030_ASHP"
) %>% mutate(Type = "ASHP")

loadprofile_hot_ab36_gshp_collector <-
  loadprofile_hot %>% select(
    c(
      "Time",
      "ApartmentBuildings36_beginn_1918_GSHP_Collector",
      "ApartmentBuildings36_1919_1948_GSHP_Collector",
      "ApartmentBuildings36_1949_1978_GSHP_Collector",
      "ApartmentBuildings36_1979_1986_GSHP_Collector",
      "ApartmentBuildings36_1987_1990_GSHP_Collector",
      "ApartmentBuildings36_1991_1995_GSHP_Collector",
      "ApartmentBuildings36_1996_2000_GSHP_Collector",
      "ApartmentBuildings36_2001_2011_GSHP_Collector",
      "ApartmentBuildings36_2012_2022_GSHP_Collector",
      "ApartmentBuildings36_2023_2030_GSHP_Collector"
    )
  ) %>% rename(
    "Before 1919" = "ApartmentBuildings36_beginn_1918_GSHP_Collector",
    "1919 - 1948" = "ApartmentBuildings36_1919_1948_GSHP_Collector",
    "1949 - 1978" = "ApartmentBuildings36_1949_1978_GSHP_Collector",
    "1979 - 1986" = "ApartmentBuildings36_1979_1986_GSHP_Collector",
    "1987 - 1990" = "ApartmentBuildings36_1987_1990_GSHP_Collector",
    "1991 - 1995" = "ApartmentBuildings36_1991_1995_GSHP_Collector",
    "1996 - 2000" = "ApartmentBuildings36_1996_2000_GSHP_Collector",
    "2001 - 2011" = "ApartmentBuildings36_2001_2011_GSHP_Collector",
    "2012 - 2022" = "ApartmentBuildings36_2012_2022_GSHP_Collector",
    "2023 - 2030" = "ApartmentBuildings36_2023_2030_GSHP_Collector"
  ) %>% mutate(Type = "GSHP Collector")

loadprofile_hot_ab36_gshp_probe <-
  loadprofile_hot %>% select(
    c(
      "Time",
      "ApartmentBuildings36_beginn_1918_GSHP_Probe",
      "ApartmentBuildings36_1919_1948_GSHP_Probe",
      "ApartmentBuildings36_1949_1978_GSHP_Probe",
      "ApartmentBuildings36_1979_1986_GSHP_Probe",
      "ApartmentBuildings36_1987_1990_GSHP_Probe",
      "ApartmentBuildings36_1991_1995_GSHP_Probe",
      "ApartmentBuildings36_1996_2000_GSHP_Probe",
      "ApartmentBuildings36_2001_2011_GSHP_Probe",
      "ApartmentBuildings36_2012_2022_GSHP_Probe",
      "ApartmentBuildings36_2023_2030_GSHP_Probe"
    )
  ) %>% rename(
    "Before 1919" = "ApartmentBuildings36_beginn_1918_GSHP_Probe",
    "1919 - 1948" = "ApartmentBuildings36_1919_1948_GSHP_Probe",
    "1949 - 1978" = "ApartmentBuildings36_1949_1978_GSHP_Probe",
    "1979 - 1986" = "ApartmentBuildings36_1979_1986_GSHP_Probe",
    "1987 - 1990" = "ApartmentBuildings36_1987_1990_GSHP_Probe",
    "1991 - 1995" = "ApartmentBuildings36_1991_1995_GSHP_Probe",
    "1996 - 2000" = "ApartmentBuildings36_1996_2000_GSHP_Probe",
    "2001 - 2011" = "ApartmentBuildings36_2001_2011_GSHP_Probe",
    "2012 - 2022" = "ApartmentBuildings36_2012_2022_GSHP_Probe",
    "2023 - 2030" = "ApartmentBuildings36_2023_2030_GSHP_Probe"
  ) %>% mutate(Type = "GSHP Probe")

loadprofile_hot_ab36 <- loadprofile_hot_ab36_ashp %>%
  rbind(loadprofile_hot_ab36_gshp_collector) %>%
  rbind(loadprofile_hot_ab36_gshp_probe)

loadprofile_hot_ab36 <- loadprofile_hot_ab36 %>%
  mutate(DATEISO = ISOdate(
    2022,
    ifelse(substr(Time, 4, 5) == "00", 0, sub("^0+", "", substr(Time, 4, 5))),
    ifelse(substr(Time, 1, 2) == "00", 0, sub("^0+", "", substr(Time, 1, 2))),
    ifelse(substr(Time, 7, 8) == "00", 0, sub("^0+", "", substr(Time, 7, 8)))
  ))

loadprofile_hot_ab36_selected <-
  loadprofile_hot_ab36 %>% select(c("DATEISO",
                                    "Type",
                                    "1979 - 1986",
                                    "2001 - 2011")) %>% gather("Year of construction",
                                                               "Heat pump electricity demand",
                                                               3:4) %>% mutate_if(is.character, as.factor)



hp_loadprofile_hot_ab36_selected_plot <-
  ggplot(data = loadprofile_hot_ab36_selected,
         aes(DATEISO,
             `Heat pump electricity demand`,)) +
  geom_line(lwd = 0.5) +
  facet_grid(Type ~ `Year of construction`) +
  coord_cartesian(ylim = c(0, 16)) +
  labs(x = "Date",
       y = "Electricity demand in kWh")


hp_loadprofile_hot_ab36_selected_plot


# Get the combined load profile data for one and two family houses
loadprofile_reference_otfh_sums <-
  loadprofile_reference_otfh %>% select(-c(Time,
                                           DATEISO)) %>% group_by(Type) %>% summarise(
                                             "Before 1919" = sum(`Before 1919`),
                                             "1919 - 1948" = sum(`1919 - 1948`),
                                             "1949 - 1978" = sum(`1949 - 1978`),
                                             "1979 - 1986" = sum(`1979 - 1986`),
                                             "1987 - 1990" = sum(`1987 - 1990`),
                                             "1991 - 1995" = sum(`1991 - 1995`),
                                             "1996 - 2000" = sum(`1996 - 2000`),
                                             "2001 - 2011" = sum(`2001 - 2011`),
                                             "2012 - 2022" = sum(`2012 - 2022`),
                                             "2023 - 2030" = sum(`2023 - 2030`)
                                           ) %>% gather("Year of construction",
                                                        "Electricity demand",
                                                        2:11) %>% mutate(Year = "2017")

loadprofile_cold_otfh_sums <- loadprofile_cold_otfh %>% select(-c(Time,
                                                                  DATEISO)) %>% group_by(Type) %>% summarise(
                                                                    "Before 1919" = sum(`Before 1919`),
                                                                    "1919 - 1948" = sum(`1919 - 1948`),
                                                                    "1949 - 1978" = sum(`1949 - 1978`),
                                                                    "1979 - 1986" = sum(`1979 - 1986`),
                                                                    "1987 - 1990" = sum(`1987 - 1990`),
                                                                    "1991 - 1995" = sum(`1991 - 1995`),
                                                                    "1996 - 2000" = sum(`1996 - 2000`),
                                                                    "2001 - 2011" = sum(`2001 - 2011`),
                                                                    "2012 - 2022" = sum(`2012 - 2022`),
                                                                    "2023 - 2030" = sum(`2023 - 2030`)
                                                                  ) %>% gather("Year of construction",
                                                                               "Electricity demand",
                                                                               2:11) %>% mutate(Year = "2010")

loadprofile_hot_otfh_sums <- loadprofile_hot_otfh %>% select(-c(Time,
                                                                DATEISO)) %>% group_by(Type) %>% summarise(
                                                                  "Before 1919" = sum(`Before 1919`),
                                                                  "1919 - 1948" = sum(`1919 - 1948`),
                                                                  "1949 - 1978" = sum(`1949 - 1978`),
                                                                  "1979 - 1986" = sum(`1979 - 1986`),
                                                                  "1987 - 1990" = sum(`1987 - 1990`),
                                                                  "1991 - 1995" = sum(`1991 - 1995`),
                                                                  "1996 - 2000" = sum(`1996 - 2000`),
                                                                  "2001 - 2011" = sum(`2001 - 2011`),
                                                                  "2012 - 2022" = sum(`2012 - 2022`),
                                                                  "2023 - 2030" = sum(`2023 - 2030`)
                                                                ) %>% gather("Year of construction",
                                                                             "Electricity demand",
                                                                             2:11) %>% mutate(Year = "2022")

loadprofile_otfh_sums <- loadprofile_reference_otfh_sums %>%
  rbind(loadprofile_cold_otfh_sums) %>%
  rbind(loadprofile_hot_otfh_sums)


loadprofile_otfh_sums_plot <- ggplot(loadprofile_otfh_sums,
                                     aes(x = `Electricity demand`,
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
  labs(x = "Electricity demand in kWh",
       y = "Year of construction")

loadprofile_otfh_sums_plot


# Get the combined load profile data for apartment buildings 3-6
loadprofile_reference_ab36_sums <-
  loadprofile_reference_ab36 %>% select(-c(Time,
                                           DATEISO)) %>% group_by(Type) %>% summarise(
                                             "Before 1919" = sum(`Before 1919`),
                                             "1919 - 1948" = sum(`1919 - 1948`),
                                             "1949 - 1978" = sum(`1949 - 1978`),
                                             "1979 - 1986" = sum(`1979 - 1986`),
                                             "1987 - 1990" = sum(`1987 - 1990`),
                                             "1991 - 1995" = sum(`1991 - 1995`),
                                             "1996 - 2000" = sum(`1996 - 2000`),
                                             "2001 - 2011" = sum(`2001 - 2011`),
                                             "2012 - 2022" = sum(`2012 - 2022`),
                                             "2023 - 2030" = sum(`2023 - 2030`)
                                           ) %>% gather("Year of construction",
                                                        "Electricity demand",
                                                        2:11) %>% mutate(Year = "2017")

loadprofile_cold_ab36_sums <- loadprofile_cold_ab36 %>% select(-c(Time,
                                                                  DATEISO)) %>% group_by(Type) %>% summarise(
                                                                    "Before 1919" = sum(`Before 1919`),
                                                                    "1919 - 1948" = sum(`1919 - 1948`),
                                                                    "1949 - 1978" = sum(`1949 - 1978`),
                                                                    "1979 - 1986" = sum(`1979 - 1986`),
                                                                    "1987 - 1990" = sum(`1987 - 1990`),
                                                                    "1991 - 1995" = sum(`1991 - 1995`),
                                                                    "1996 - 2000" = sum(`1996 - 2000`),
                                                                    "2001 - 2011" = sum(`2001 - 2011`),
                                                                    "2012 - 2022" = sum(`2012 - 2022`),
                                                                    "2023 - 2030" = sum(`2023 - 2030`)
                                                                  ) %>% gather("Year of construction",
                                                                               "Electricity demand",
                                                                               2:11) %>% mutate(Year = "2010")

loadprofile_hot_ab36_sums <- loadprofile_hot_ab36 %>% select(-c(Time,
                                                                DATEISO)) %>% group_by(Type) %>% summarise(
                                                                  "Before 1919" = sum(`Before 1919`),
                                                                  "1919 - 1948" = sum(`1919 - 1948`),
                                                                  "1949 - 1978" = sum(`1949 - 1978`),
                                                                  "1979 - 1986" = sum(`1979 - 1986`),
                                                                  "1987 - 1990" = sum(`1987 - 1990`),
                                                                  "1991 - 1995" = sum(`1991 - 1995`),
                                                                  "1996 - 2000" = sum(`1996 - 2000`),
                                                                  "2001 - 2011" = sum(`2001 - 2011`),
                                                                  "2012 - 2022" = sum(`2012 - 2022`),
                                                                  "2023 - 2030" = sum(`2023 - 2030`)
                                                                ) %>% gather("Year of construction",
                                                                             "Electricity demand",
                                                                             2:11) %>% mutate(Year = "2022")

loadprofile_ab36_sums <- loadprofile_reference_ab36_sums %>%
  rbind(loadprofile_cold_ab36_sums) %>%
  rbind(loadprofile_hot_ab36_sums)


loadprofile_ab36_sums_plot <- ggplot(loadprofile_ab36_sums,
                                     aes(x = `Electricity demand`,
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
  labs(x = "Electricity demand in kWh",
       y = "Year of construction")

loadprofile_ab36_sums_plot


# Save plots
ggsave(
  "plots/output/electricityconsumptionbuildingtypes/shandhw/hp_loadprofile_reference_otfh_selected_plot.png",
  hp_loadprofile_reference_otfh_selected_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/electricityconsumptionbuildingtypes/shandhw/hp_loadprofile_reference_ab36_selected_plot.png",
  hp_loadprofile_reference_ab36_selected_plot,
  width = 25,
  units = "cm"
)


ggsave(
  "plots/output/electricityconsumptionbuildingtypes/shandhw/hp_loadprofile_cold_otfh_selected_plot.png",
  hp_loadprofile_cold_otfh_selected_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/electricityconsumptionbuildingtypes/shandhw/hp_loadprofile_cold_ab36_selected_plot.png",
  hp_loadprofile_cold_ab36_selected_plot,
  width = 25,
  units = "cm"
)


ggsave(
  "plots/output/electricityconsumptionbuildingtypes/shandhw/hp_loadprofile_hot_otfh_selected_plot.png",
  hp_loadprofile_hot_otfh_selected_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/electricityconsumptionbuildingtypes/shandhw/hp_loadprofile_hot_ab36_selected_plot.png",
  hp_loadprofile_hot_ab36_selected_plot,
  width = 25,
  units = "cm"
)


ggsave(
  "plots/output/electricityconsumptionbuildingtypes/shandhw/loadprofile_otfh_sums_plot.png",
  loadprofile_otfh_sums_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/electricityconsumptionbuildingtypes/shandhw/loadprofile_ab36_sums_plot.png",
  loadprofile_ab36_sums_plot,
  width = 25,
  units = "cm"
)
