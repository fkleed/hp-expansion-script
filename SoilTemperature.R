# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read data for soil temperature
bremen <-
  read_delim("data/weatherstations/soiltemperature/bremen.csv", delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_691" = "V_TE100")

bremen$MESS_DATUM <- as.factor(bremen$MESS_DATUM)
bremen$V_TE100_691 <- as.numeric(bremen$V_TE100_691)
bremen$V_TE100_691 <- replace(bremen$V_TE100_691,
                              bremen$V_TE100_691 < -15 |
                                bremen$V_TE100_691 > 30,
                              NA)


dresden_klotzsche <-
  read_delim("data/weatherstations/soiltemperature/dresden-klotzsche.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_1048" = "V_TE100")

dresden_klotzsche$MESS_DATUM <-
  as.factor(dresden_klotzsche$MESS_DATUM)
dresden_klotzsche$V_TE100_1048 <-
  as.numeric(dresden_klotzsche$V_TE100_1048)
dresden_klotzsche$V_TE100_1048 <-
  replace(
    dresden_klotzsche$V_TE100_1048,
    dresden_klotzsche$V_TE100_1048 < -15 |
      dresden_klotzsche$V_TE100_1048 > 30,
    NA
  )


erfurt_weimar <-
  read_delim("data/weatherstations/soiltemperature/erfurt-weimar.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_1270" = "V_TE100")

erfurt_weimar$MESS_DATUM <- as.factor(erfurt_weimar$MESS_DATUM)
erfurt_weimar$V_TE100_1270 <- as.numeric(erfurt_weimar$V_TE100_1270)
erfurt_weimar$V_TE100_1270 <- replace(
  erfurt_weimar$V_TE100_1270,
  erfurt_weimar$V_TE100_1270 < -15 |
    erfurt_weimar$V_TE100_1270 > 30,
  NA
)


frankfurt_main <-
  read_delim("data/weatherstations/soiltemperature/frankfurt-main.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_1420" = "V_TE100")

frankfurt_main$MESS_DATUM <- as.factor(frankfurt_main$MESS_DATUM)
frankfurt_main$V_TE100_1420 <-
  as.numeric(frankfurt_main$V_TE100_1420)
frankfurt_main$V_TE100_1420 <- replace(
  frankfurt_main$V_TE100_1420,
  frankfurt_main$V_TE100_1420 < -15 |
    frankfurt_main$V_TE100_1420 > 30,
  NA
)


hamburg_fuhlsbuettel <-
  read_delim("data/weatherstations/soiltemperature/hamburg-fuhlsbuettel.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_1975" = "V_TE100")

hamburg_fuhlsbuettel$MESS_DATUM <-
  as.factor(hamburg_fuhlsbuettel$MESS_DATUM)
hamburg_fuhlsbuettel$V_TE100_1975 <-
  as.numeric(hamburg_fuhlsbuettel$V_TE100_1975)
hamburg_fuhlsbuettel$V_TE100_1975 <- replace(
  hamburg_fuhlsbuettel$V_TE100_1975,
  hamburg_fuhlsbuettel$V_TE100_1975 < -15 |
    hamburg_fuhlsbuettel$V_TE100_1975 > 30,
  NA
)


hannover <-
  read_delim("data/weatherstations/soiltemperature/hannover.csv", delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_2014" = "V_TE100")

hannover$MESS_DATUM <- as.factor(hannover$MESS_DATUM)
hannover$V_TE100_2014 <- as.numeric(hannover$V_TE100_2014)
hannover$V_TE100_2014 <- replace(hannover$V_TE100_2014,
                                 hannover$V_TE100_2014 < -15 |
                                   hannover$V_TE100_2014 > 30,
                                 NA)


koeln_bonn <-
  read_delim("data/weatherstations/soiltemperature/koeln-bonn.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_2667" = "V_TE100")

koeln_bonn$MESS_DATUM <- as.factor(koeln_bonn$MESS_DATUM)
koeln_bonn$V_TE100_2667 <- as.numeric(koeln_bonn$V_TE100_2667)
koeln_bonn$V_TE100_2667 <- replace(
  koeln_bonn$V_TE100_2667,
  koeln_bonn$V_TE100_2667 < -15 |
    koeln_bonn$V_TE100_2667 > 30,
  NA
)


muenchen_flughafen <-
  read_delim("data/weatherstations/soiltemperature/muenchen-flughafen.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_1262" = "V_TE100")

muenchen_flughafen$MESS_DATUM <-
  as.factor(muenchen_flughafen$MESS_DATUM)
muenchen_flughafen$V_TE100_1262 <-
  as.numeric(muenchen_flughafen$V_TE100_1262)
muenchen_flughafen$V_TE100_1262 <- replace(
  muenchen_flughafen$V_TE100_1262,
  muenchen_flughafen$V_TE100_1262 < -15 |
    muenchen_flughafen$V_TE100_1262 > 30,
  NA
)


potsdam <-
  read_delim("data/weatherstations/soiltemperature/potsdam.csv", delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_3987" = "V_TE100")

potsdam$MESS_DATUM <- as.factor(potsdam$MESS_DATUM)
potsdam$V_TE100_3987 <- as.numeric(potsdam$V_TE100_3987)
potsdam$V_TE100_3987 <- replace(potsdam$V_TE100_3987,
                                potsdam$V_TE100_3987 < -15 |
                                  potsdam$V_TE100_3987 > 30,
                                NA)


stuttgart_echterdingen <-
  read_delim("data/weatherstations/soiltemperature/stuttgart-echterdingen.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_4931" = "V_TE100")

stuttgart_echterdingen$MESS_DATUM <-
  as.factor(stuttgart_echterdingen$MESS_DATUM)
stuttgart_echterdingen$V_TE100_4931 <-
  as.numeric(stuttgart_echterdingen$V_TE100_4931)
stuttgart_echterdingen$V_TE100_4931 <- replace(
  stuttgart_echterdingen$V_TE100_4931,
  stuttgart_echterdingen$V_TE100_4931 < -15 |
    stuttgart_echterdingen$V_TE100_4931 > 30,
  NA
)


saarbruecken_ensheim <-
  read_delim("data/weatherstations/soiltemperature/saarbruecken-ensheim.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_4336" = "V_TE100")

saarbruecken_ensheim$MESS_DATUM <-
  as.factor(saarbruecken_ensheim$MESS_DATUM)
saarbruecken_ensheim$V_TE100_4336 <-
  as.numeric(saarbruecken_ensheim$V_TE100_4336)
saarbruecken_ensheim$V_TE100_4336 <- replace(
  saarbruecken_ensheim$V_TE100_4336,
  saarbruecken_ensheim$V_TE100_4336 < -15 |
    saarbruecken_ensheim$V_TE100_4336 > 30,
  NA
)


berlin_dahlem <-
  read_delim("data/weatherstations/soiltemperature/berlin-dahlem.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_403" = "V_TE100")

berlin_dahlem$MESS_DATUM <- as.factor(berlin_dahlem$MESS_DATUM)
berlin_dahlem$V_TE100_403 <- as.numeric(berlin_dahlem$V_TE100_403)
berlin_dahlem$V_TE100_403 <- replace(
  berlin_dahlem$V_TE100_403,
  berlin_dahlem$V_TE100_403 < -15 |
    berlin_dahlem$V_TE100_403 > 30,
  NA
)


fehmarn <-
  read_delim("data/weatherstations/soiltemperature/fehmarn.csv", delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_5516" = "V_TE100")

fehmarn$MESS_DATUM <- as.factor(fehmarn$MESS_DATUM)
fehmarn$V_TE100_5516 <- as.numeric(fehmarn$V_TE100_5516)
fehmarn$V_TE100_5516 <- replace(fehmarn$V_TE100_5516,
                                fehmarn$V_TE100_5516 < -15 |
                                  fehmarn$V_TE100_5516 > 30,
                                NA)


rostock_warnemuende <-
  read_delim("data/weatherstations/soiltemperature/rostock-warnemuende.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_4271" = "V_TE100")

rostock_warnemuende$MESS_DATUM <-
  as.factor(rostock_warnemuende$MESS_DATUM)
rostock_warnemuende$V_TE100_4271 <-
  as.numeric(rostock_warnemuende$V_TE100_4271)
rostock_warnemuende$V_TE100_4271 <- replace(
  rostock_warnemuende$V_TE100_4271,
  rostock_warnemuende$V_TE100_4271 < -15 |
    rostock_warnemuende$V_TE100_4271 > 30,
  NA
)


magdeburg <-
  read_delim("data/weatherstations/soiltemperature/magdeburg.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_3126" = "V_TE100")

magdeburg$MESS_DATUM <- as.factor(magdeburg$MESS_DATUM)
magdeburg$V_TE100_3126 <- as.numeric(magdeburg$V_TE100_3126)
magdeburg$V_TE100_3126 <- replace(magdeburg$V_TE100_3126,
                                  magdeburg$V_TE100_3126 < -15 |
                                    magdeburg$V_TE100_3126 > 30,
                                  NA)


trier_petrisberg <-
  read_delim("data/weatherstations/soiltemperature/trier-petrisberg.csv",
             delim = ";") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE050,
    eor
  )) %>%
  rename("V_TE100_5100" = "V_TE100")

trier_petrisberg$MESS_DATUM <-
  as.factor(trier_petrisberg$MESS_DATUM)
trier_petrisberg$V_TE100_5100 <-
  as.numeric(trier_petrisberg$V_TE100_5100)
trier_petrisberg$V_TE100_5100 <- replace(
  trier_petrisberg$V_TE100_5100 ,
  trier_petrisberg$V_TE100_5100  < -15 |
    trier_petrisberg$V_TE100_5100  > 30,
  NA
)


# Join the soil temperature data from weather stations
list_soil_temperature <- list(
  bremen,
  dresden_klotzsche,
  erfurt_weimar,
  frankfurt_main,
  hamburg_fuhlsbuettel,
  hannover,
  koeln_bonn,
  muenchen_flughafen,
  potsdam,
  stuttgart_echterdingen,
  saarbruecken_ensheim,
  berlin_dahlem,
  fehmarn,
  rostock_warnemuende,
  magdeburg,
  trier_petrisberg
)

soil_temperature_combined <-
  list_soil_temperature %>% reduce(left_join, by = "MESS_DATUM")


# Calculate the mean soil temperature over all stations
soil_temperature_combined <- soil_temperature_combined %>%
  mutate(MeanSoilTemperature = rowMeans(soil_temperature_combined[, 2:17], na.rm = TRUE))


# Calculate soil temperature values for an average year over the years 2010 - 2022 and filter leap year
soil_temperature_average_yer <- soil_temperature_combined %>%
  select(c("MESS_DATUM", "MeanSoilTemperature")) %>%
  mutate(Date = substr(as.character(MESS_DATUM), 5, 10)) %>%
  select(-c(MESS_DATUM)) %>%
  mutate_if(is.character, as.factor) %>%
  group_by(Date) %>%
  summarise(MeanSoilTemperature = mean(MeanSoilTemperature),
            .groups = 'drop') %>%
  filter(substr(as.character(Date), 1, 4) != "0229")


# Calculate soil temperature values for the cold year 2010
soil_temperature_2010 <- soil_temperature_combined %>%
  select(c("MESS_DATUM", "MeanSoilTemperature")) %>%
  filter(substr(as.character(soil_temperature_combined$MESS_DATUM), 1, 4) == 2010) %>%
  mutate(Date = substr(as.character(MESS_DATUM), 5, 10)) %>%
  select(-c(MESS_DATUM)) %>%
  mutate_if(is.character, as.factor) %>%
  relocate(Date, .before = MeanSoilTemperature)


# Calculate soil temperature values for the hot year 2022
soil_temperature_2022 <- soil_temperature_combined %>%
  select(c("MESS_DATUM", "MeanSoilTemperature")) %>%
  filter(substr(as.character(soil_temperature_combined$MESS_DATUM), 1, 4) == 2022) %>%
  mutate(Date = substr(as.character(MESS_DATUM), 5, 10)) %>%
  select(-c(MESS_DATUM)) %>%
  mutate_if(is.character, as.factor) %>%
  relocate(Date, .before = MeanSoilTemperature)

# Calculate soil temperature values for the hot year 2016
soil_temperature_2016 <- soil_temperature_combined %>%
  select(c("MESS_DATUM", "MeanSoilTemperature")) %>%
  filter(substr(as.character(soil_temperature_combined$MESS_DATUM), 1, 4) == 2016) %>%
  mutate(Date = substr(as.character(MESS_DATUM), 5, 10)) %>%
  select(-c(MESS_DATUM)) %>%
  mutate_if(is.character, as.factor) %>%
  relocate(Date, .before = MeanSoilTemperature)


# Write soil temperature values of average year to csv file
write_csv2(
  soil_temperature_average_yer,
  "data/output/weathermodel/soiltempaverageyear.csv"
)

write_csv2(soil_temperature_2010,
           "data/output/weathermodel/soiltempyear2010.csv")

write_csv2(soil_temperature_2022,
           "data/output/weathermodel/soiltempyear2022.csv")

write_csv2(soil_temperature_2016,
           "data/output/weathermodel/soiltempyear2016.csv")
