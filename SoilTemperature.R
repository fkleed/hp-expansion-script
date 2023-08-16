# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read data for soil temperature
bremen <-
  read_csv2("data/weatherstations/soiltemperature/bremen.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_691" = "V_TE050")

bremen$MESS_DATUM <- as.factor(bremen$MESS_DATUM)
bremen$V_TE050_691 <- as.numeric(bremen$V_TE050_691)
bremen$V_TE050_691 <- replace(bremen$V_TE050_691,
                              bremen$V_TE050_691 < -15 |
                                bremen$V_TE050_691 > 30,
                              NA)


dresden_klotzsche <-
  read_csv2("data/weatherstations/soiltemperature/dresden-klotzsche.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_1048" = "V_TE050")

dresden_klotzsche$MESS_DATUM <-
  as.factor(dresden_klotzsche$MESS_DATUM)
dresden_klotzsche$V_TE050_1048 <-
  as.numeric(dresden_klotzsche$V_TE050_1048)
dresden_klotzsche$V_TE050_1048 <-
  replace(
    dresden_klotzsche$V_TE050_1048,
    dresden_klotzsche$V_TE050_1048 < -15 |
      dresden_klotzsche$V_TE050_1048 > 30,
    NA
  )


erfurt_weimar <-
  read_csv2("data/weatherstations/soiltemperature/erfurt-weimar.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_1270" = "V_TE050")

erfurt_weimar$MESS_DATUM <- as.factor(erfurt_weimar$MESS_DATUM)
erfurt_weimar$V_TE050_1270 <- as.numeric(erfurt_weimar$V_TE050_1270)
erfurt_weimar$V_TE050_1270 <- replace(
  erfurt_weimar$V_TE050_1270,
  erfurt_weimar$V_TE050_1270 < -15 |
    erfurt_weimar$V_TE050_1270 > 30,
  NA
)


frankfurt_main <-
  read_csv2("data/weatherstations/soiltemperature/frankfurt-main.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_1420" = "V_TE050")

frankfurt_main$MESS_DATUM <- as.factor(frankfurt_main$MESS_DATUM)
frankfurt_main$V_TE050_1420 <-
  as.numeric(frankfurt_main$V_TE050_1420)
frankfurt_main$V_TE050_1420 <- replace(
  frankfurt_main$V_TE050_1420,
  frankfurt_main$V_TE050_1420 < -15 |
    frankfurt_main$V_TE050_1420 > 30,
  NA
)


hamburg_fuhlsbuettel <-
  read_csv2("data/weatherstations/soiltemperature/hamburg-fuhlsbuettel.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_1975" = "V_TE050")

hamburg_fuhlsbuettel$MESS_DATUM <-
  as.factor(hamburg_fuhlsbuettel$MESS_DATUM)
hamburg_fuhlsbuettel$V_TE050_1975 <-
  as.numeric(hamburg_fuhlsbuettel$V_TE050_1975)
hamburg_fuhlsbuettel$V_TE050_1975 <- replace(
  hamburg_fuhlsbuettel$V_TE050_1975,
  hamburg_fuhlsbuettel$V_TE050_1975 < -15 |
    hamburg_fuhlsbuettel$V_TE050_1975 > 30,
  NA
)


hannover <-
  read_csv2("data/weatherstations/soiltemperature/hannover.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_2014" = "V_TE050")

hannover$MESS_DATUM <- as.factor(hannover$MESS_DATUM)
hannover$V_TE050_2014 <- as.numeric(hannover$V_TE050_2014)
hannover$V_TE050_2014 <- replace(hannover$V_TE050_2014,
                                 hannover$V_TE050_2014 < -15 |
                                   hannover$V_TE050_2014 > 30,
                                 NA)


koeln_bonn <-
  read_csv2("data/weatherstations/soiltemperature/koeln-bonn.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_2667" = "V_TE050")

koeln_bonn$MESS_DATUM <- as.factor(koeln_bonn$MESS_DATUM)
koeln_bonn$V_TE050_2667 <- as.numeric(koeln_bonn$V_TE050_2667)
koeln_bonn$V_TE050_2667 <- replace(
  koeln_bonn$V_TE050_2667,
  koeln_bonn$V_TE050_2667 < -15 |
    koeln_bonn$V_TE050_2667 > 30,
  NA
)


muenchen_flughafen <-
  read_csv2("data/weatherstations/soiltemperature/muenchen-flughafen.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_1262" = "V_TE050")

muenchen_flughafen$MESS_DATUM <- as.factor(muenchen_flughafen$MESS_DATUM)
muenchen_flughafen$V_TE050_1262 <- as.numeric(muenchen_flughafen$V_TE050_1262)
muenchen_flughafen$V_TE050_1262 <- replace(
  muenchen_flughafen$V_TE050_1262,
  muenchen_flughafen$V_TE050_1262 < -15 |
    muenchen_flughafen$V_TE050_1262 > 30,
  NA
)


potsdam <-
  read_csv2("data/weatherstations/soiltemperature/potsdam.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_3987" = "V_TE050")

potsdam$MESS_DATUM <- as.factor(potsdam$MESS_DATUM)
potsdam$V_TE050_3987 <- as.numeric(potsdam$V_TE050_3987)
potsdam$V_TE050_3987 <- replace(
  potsdam$V_TE050_3987,
  potsdam$V_TE050_3987 < -15 |
    potsdam$V_TE050_3987 > 30,
  NA
)


stuttgart_echterdingen <-
  read_csv2("data/weatherstations/soiltemperature/stuttgart-echterdingen.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_4931" = "V_TE050")

stuttgart_echterdingen$MESS_DATUM <- as.factor(stuttgart_echterdingen$MESS_DATUM)
stuttgart_echterdingen$V_TE050_4931 <- as.numeric(stuttgart_echterdingen$V_TE050_4931)
stuttgart_echterdingen$V_TE050_4931 <- replace(
  stuttgart_echterdingen$V_TE050_4931,
  stuttgart_echterdingen$V_TE050_4931 < -15 |
    stuttgart_echterdingen$V_TE050_4931 > 30,
  NA
)


saarbruecken_ensheim <-
  read_csv2("data/weatherstations/soiltemperature/saarbruecken-ensheim.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_4336" = "V_TE050")

saarbruecken_ensheim$MESS_DATUM <- as.factor(saarbruecken_ensheim$MESS_DATUM)
saarbruecken_ensheim$V_TE050_4336 <- as.numeric(saarbruecken_ensheim$V_TE050_4336)
saarbruecken_ensheim$V_TE050_4336 <- replace(
  saarbruecken_ensheim$V_TE050_4336,
  saarbruecken_ensheim$V_TE050_4336 < -15 |
    saarbruecken_ensheim$V_TE050_4336 > 30,
  NA
)


berlin_dahlem <-
  read_csv2("data/weatherstations/soiltemperature/berlin-dahlem.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_403" = "V_TE050")

berlin_dahlem$MESS_DATUM <- as.factor(berlin_dahlem$MESS_DATUM)
berlin_dahlem$V_TE050_403 <- as.numeric(berlin_dahlem$V_TE050_403)
berlin_dahlem$V_TE050_403 <- replace(
  berlin_dahlem$V_TE050_403,
  berlin_dahlem$V_TE050_403 < -15 |
    berlin_dahlem$V_TE050_403 > 30,
  NA
)


fehmarn <-
  read_csv2("data/weatherstations/soiltemperature/fehmarn.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_5516" = "V_TE050")

fehmarn$MESS_DATUM <- as.factor(fehmarn$MESS_DATUM)
fehmarn$V_TE050_5516 <- as.numeric(fehmarn$V_TE050_5516)
fehmarn$V_TE050_5516 <- replace(
  fehmarn$V_TE050_5516,
  fehmarn$V_TE050_5516 < -15 |
    fehmarn$V_TE050_5516 > 30,
  NA
)


rostock_warnemuende <-
  read_csv2("data/weatherstations/soiltemperature/rostock-warnemuende.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_4271" = "V_TE050")

rostock_warnemuende$MESS_DATUM <- as.factor(rostock_warnemuende$MESS_DATUM)
rostock_warnemuende$V_TE050_4271 <- as.numeric(rostock_warnemuende$V_TE050_4271)
rostock_warnemuende$V_TE050_4271 <- replace(
  rostock_warnemuende$V_TE050_4271,
  rostock_warnemuende$V_TE050_4271 < -15 |
    rostock_warnemuende$V_TE050_4271 > 30,
  NA
)


magdeburg <-
  read_csv2("data/weatherstations/soiltemperature/magdeburg.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_3126" = "V_TE050")

magdeburg$MESS_DATUM <- as.factor(magdeburg$MESS_DATUM)
magdeburg$V_TE050_3126 <- as.numeric(magdeburg$V_TE050_3126)
magdeburg$V_TE050_3126 <- replace(
  magdeburg$V_TE050_3126,
  magdeburg$V_TE050_3126 < -15 |
    magdeburg$V_TE050_3126 > 30,
  NA
)


trier_petrisberg <-
  read_csv2("data/weatherstations/soiltemperature/trier-petrisberg.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(
    STATIONS_ID,
    QN_2,
    V_TE002,
    V_TE005,
    V_TE010,
    V_TE020,
    V_TE100,
    eor
  )) %>%
  rename("V_TE050_5100" = "V_TE050")

trier_petrisberg$MESS_DATUM <- as.factor(trier_petrisberg$MESS_DATUM)
trier_petrisberg$V_TE050_5100 <- as.numeric(trier_petrisberg$V_TE050_5100)
trier_petrisberg$V_TE050_5100 <- replace(
  trier_petrisberg$V_TE050_5100 ,
  trier_petrisberg$V_TE050_5100  < -15 |
    trier_petrisberg$V_TE050_5100  > 30,
  NA
)
