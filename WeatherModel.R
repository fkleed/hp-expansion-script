# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read data from weather stations
bremen <- read_csv2("data/weatherstations/bremen.csv") %>%
  filter(MESS_DATUM >= 2010010100)

dresden_klotzsche <-
  read_csv2("data/weatherstations/dresden-klotzsche.csv") %>%
  filter(MESS_DATUM >= 2010010100)

erfurt_weimar <-
  read_csv2("data/weatherstations/erfurt-weimar.csv") %>%
  filter(MESS_DATUM >= 2010010100)

frankfurt_main <-
  read_csv2("data/weatherstations/frankfurt-main.csv") %>%
  filter(MESS_DATUM >= 2010010100)

hamburg_fuhlsbuettel <-
  read_csv2("data/weatherstations/hamburg-fuhlsbuettel.csv") %>%
  filter(MESS_DATUM >= 2010010100)

hannover <- read_csv2("data/weatherstations/hannover.csv") %>%
  filter(MESS_DATUM >= 2010010100)

koeln_bonn <- read_csv2("data/weatherstations/koeln-bonn.csv") %>%
  filter(MESS_DATUM >= 2010010100)

muenchen_flughafen <-
  read_csv2("data/weatherstations/muenchen-flughafen.csv") %>%
  filter(MESS_DATUM >= 2010010100)

potsdam <- read_csv2("data/weatherstations/potsdam.csv") %>%
  filter(MESS_DATUM >= 2010010100)

stuttgart_echterdingen <-
  read_csv2("data/weatherstations/stuttgart-echterdingen.csv") %>%
  filter(MESS_DATUM >= 2010010100)

saarbruecken_ensheim <-
  read_csv2("data/weatherstations/saarbruecken-ensheim.csv") %>%
  filter(MESS_DATUM >= 2010010100)

nrow(bremen)
nrow(dresden_klotzsche)
nrow(erfurt_weimar)
nrow(frankfurt_main)
nrow(hamburg_fuhlsbuettel)
nrow(hannover)
nrow(koeln_bonn)
nrow(muenchen_flughafen)
nrow(potsdam)
nrow(stuttgart_echterdingen)
nrow(saarbruecken_ensheim)


# Missing values
berlin_dahlem <-
  read_csv2("data/weatherstations/berlin-dahlem.csv") %>%
  filter(MESS_DATUM >= 2010010100)

fehmarn <- read_csv2("data/weatherstations/fehmarn.csv") %>%
  filter(MESS_DATUM >= 2010010100)

rostock_warnemuende <-
  read_csv2("data/weatherstations/rostock-warnemuende.csv") %>%
  filter(MESS_DATUM >= 2010010100)

magdeburg <- read_csv2("data/weatherstations/magdeburg.csv") %>%
  filter(MESS_DATUM >= 2010010100)

trier_petrisberg <-
  read_csv2("data/weatherstations/trier-petrisberg.csv") %>%
  filter(MESS_DATUM >= 2010010100)

nrow(berlin_dahlem)
nrow(fehmarn)
nrow(rostock_warnemuende)
nrow(magdeburg)
nrow(trier_petrisberg)
