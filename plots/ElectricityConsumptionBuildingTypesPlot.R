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


# Get the data for one and two family houses
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
  labs(x = "Date",
       y = "Electricity demand in kWh")


hp_loadprofile_reference_otfh_selected_plot


summary(loadprofile_reference_otfh_selected)
