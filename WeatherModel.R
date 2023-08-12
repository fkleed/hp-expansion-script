# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read data from weather stations
bremen <- read_csv2("data/weatherstations/bremen.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_691" = "TT_TU")

bremen$MESS_DATUM <- as.factor(bremen$MESS_DATUM)
bremen$TT_TU_691 <- as.numeric(bremen$TT_TU_691)
bremen$TT_TU_691 <- replace(bremen$TT_TU_691,
                            bremen$TT_TU_691 < -40 |
                              bremen$TT_TU_691 > 45,
                            NA)


dresden_klotzsche <-
  read_csv2("data/weatherstations/dresden-klotzsche.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_1048" = "TT_TU")

dresden_klotzsche$MESS_DATUM <-
  as.factor(dresden_klotzsche$MESS_DATUM)
dresden_klotzsche$TT_TU_1048 <-
  as.numeric(dresden_klotzsche$TT_TU_1048)
dresden_klotzsche$TT_TU_1048 <-
  replace(
    dresden_klotzsche$TT_TU_1048,
    dresden_klotzsche$TT_TU_1048 < -40 |
      dresden_klotzsche$TT_TU_1048 > 45,
    NA
  )


erfurt_weimar <-
  read_csv2("data/weatherstations/erfurt-weimar.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_1270" = "TT_TU")

erfurt_weimar$MESS_DATUM <- as.factor(erfurt_weimar$MESS_DATUM)
erfurt_weimar$TT_TU_1270 <- as.numeric(erfurt_weimar$TT_TU_1270)
erfurt_weimar$TT_TU_1270 <- replace(
  erfurt_weimar$TT_TU_1270,
  erfurt_weimar$TT_TU_1270 < -40 |
    erfurt_weimar$TT_TU_1270 > 45,
  NA
)


frankfurt_main <-
  read_csv2("data/weatherstations/frankfurt-main.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_1420" = "TT_TU")

frankfurt_main$MESS_DATUM <- as.factor(frankfurt_main$MESS_DATUM)
frankfurt_main$TT_TU_1420 <- as.numeric(frankfurt_main$TT_TU_1420)
frankfurt_main$TT_TU_1420 <- replace(
  frankfurt_main$TT_TU_1420,
  frankfurt_main$TT_TU_1420 < -40 |
    frankfurt_main$TT_TU_1420 > 45,
  NA
)


hamburg_fuhlsbuettel <-
  read_csv2("data/weatherstations/hamburg-fuhlsbuettel.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_1975" = "TT_TU")

hamburg_fuhlsbuettel$MESS_DATUM <-
  as.factor(hamburg_fuhlsbuettel$MESS_DATUM)
hamburg_fuhlsbuettel$TT_TU_1975 <-
  as.numeric(hamburg_fuhlsbuettel$TT_TU_1975)
hamburg_fuhlsbuettel$TT_TU_1975 <-
  replace(
    hamburg_fuhlsbuettel$TT_TU_1975,
    hamburg_fuhlsbuettel$TT_TU_1975 < -40 |
      hamburg_fuhlsbuettel$TT_TU_1975 > 45,
    NA
  )


hannover <- read_csv2("data/weatherstations/hannover.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_2014" = "TT_TU")

hannover$MESS_DATUM <- as.factor(hannover$MESS_DATUM)
hannover$TT_TU_2014 <- as.numeric(hannover$TT_TU_2014)
hannover$TT_TU_2014  <- replace(hannover$TT_TU_2014,
                                hannover$TT_TU_2014 < -40 |
                                  hannover$TT_TU_2014  > 45,
                                NA)


koeln_bonn <- read_csv2("data/weatherstations/koeln-bonn.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_2667" = "TT_TU")

koeln_bonn$MESS_DATUM <- as.factor(koeln_bonn$MESS_DATUM)
koeln_bonn$TT_TU_2667 <- as.numeric(koeln_bonn$TT_TU_2667)
koeln_bonn$TT_TU_2667  <- replace(koeln_bonn$TT_TU_2667,
                                  koeln_bonn$TT_TU_2667 < -40 |
                                    koeln_bonn$TT_TU_2667  > 45,
                                  NA)


muenchen_flughafen <-
  read_csv2("data/weatherstations/muenchen-flughafen.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_1262" = "TT_TU")

muenchen_flughafen$MESS_DATUM <-
  as.factor(muenchen_flughafen$MESS_DATUM)
muenchen_flughafen$TT_TU_1262 <-
  as.numeric(muenchen_flughafen$TT_TU_1262)
muenchen_flughafen$TT_TU_1262  <-
  replace(
    muenchen_flughafen$TT_TU_1262,
    muenchen_flughafen$TT_TU_1262 < -40 |
      muenchen_flughafen$TT_TU_1262  > 45,
    NA
  )


potsdam <- read_csv2("data/weatherstations/potsdam.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_3987" = "TT_TU")

potsdam$MESS_DATUM <-
  as.factor(potsdam$MESS_DATUM)
potsdam$TT_TU_3987 <-
  as.numeric(potsdam$TT_TU_3987)
potsdam$TT_TU_3987  <-
  replace(potsdam$TT_TU_3987,
          potsdam$TT_TU_3987 < -40 |
            potsdam$TT_TU_3987  > 45,
          NA)


stuttgart_echterdingen <-
  read_csv2("data/weatherstations/stuttgart-echterdingen.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_4931" = "TT_TU")

stuttgart_echterdingen$MESS_DATUM <-
  as.factor(stuttgart_echterdingen$MESS_DATUM)
stuttgart_echterdingen$TT_TU_4931 <-
  as.numeric(stuttgart_echterdingen$TT_TU_4931)
stuttgart_echterdingen$TT_TU_4931  <-
  replace(
    stuttgart_echterdingen$TT_TU_4931,
    stuttgart_echterdingen$TT_TU_4931 < -40 |
      stuttgart_echterdingen$TT_TU_4931  > 45,
    NA
  )



saarbruecken_ensheim <-
  read_csv2("data/weatherstations/saarbruecken-ensheim.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_4336" = "TT_TU")

saarbruecken_ensheim$MESS_DATUM <-
  as.factor(saarbruecken_ensheim$MESS_DATUM)
saarbruecken_ensheim$TT_TU_4336 <-
  as.numeric(saarbruecken_ensheim$TT_TU_4336)
saarbruecken_ensheim$TT_TU_4336  <-
  replace(
    saarbruecken_ensheim$TT_TU_4336,
    saarbruecken_ensheim$TT_TU_4336 < -40 |
      saarbruecken_ensheim$TT_TU_4336  > 45,
    NA
  )


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
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_403" = "TT_TU")

berlin_dahlem$MESS_DATUM <-
  as.factor(berlin_dahlem$MESS_DATUM)
berlin_dahlem$TT_TU_403 <-
  as.numeric(berlin_dahlem$TT_TU_403)
berlin_dahlem$TT_TU_403  <-
  replace(
    berlin_dahlem$TT_TU_403,
    berlin_dahlem$TT_TU_403 < -40 |
      berlin_dahlem$TT_TU_403  > 45,
    NA
  )


fehmarn <- read_csv2("data/weatherstations/fehmarn.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_5516" = "TT_TU")

fehmarn$MESS_DATUM <-
  as.factor(fehmarn$MESS_DATUM)
fehmarn$TT_TU_5516 <-
  as.numeric(fehmarn$TT_TU_5516)
fehmarn$TT_TU_5516  <-
  replace(fehmarn$TT_TU_5516,
          fehmarn$TT_TU_5516 < -40 |
            fehmarn$TT_TU_5516  > 45,
          NA)


rostock_warnemuende <-
  read_csv2("data/weatherstations/rostock-warnemuende.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_4271" = "TT_TU")

rostock_warnemuende$MESS_DATUM <-
  as.factor(rostock_warnemuende$MESS_DATUM)
rostock_warnemuende$TT_TU_4271 <-
  as.numeric(rostock_warnemuende$TT_TU_4271)
rostock_warnemuende$TT_TU_4271  <-
  replace(
    rostock_warnemuende$TT_TU_4271,
    rostock_warnemuende$TT_TU_4271 < -40 |
      rostock_warnemuende$TT_TU_4271  > 45,
    NA
  )


magdeburg <- read_csv2("data/weatherstations/magdeburg.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_3126" = "TT_TU")

magdeburg$MESS_DATUM <-
  as.factor(magdeburg$MESS_DATUM)
magdeburg$TT_TU_3126 <-
  as.numeric(magdeburg$TT_TU_3126)
magdeburg$TT_TU_3126  <-
  replace(magdeburg$TT_TU_3126,
          magdeburg$TT_TU_3126 < -40 |
            magdeburg$TT_TU_3126  > 45,
          NA)

trier_petrisberg <-
  read_csv2("data/weatherstations/trier-petrisberg.csv") %>%
  filter(MESS_DATUM >= 2010010100) %>%
  select(-c(STATIONS_ID, QN_9, RF_TU, eor)) %>%
  rename("TT_TU_5100" = "TT_TU")

trier_petrisberg$MESS_DATUM <-
  as.factor(trier_petrisberg$MESS_DATUM)
trier_petrisberg$TT_TU_5100 <-
  as.numeric(trier_petrisberg$TT_TU_5100)
trier_petrisberg$TT_TU_5100  <-
  replace(
    trier_petrisberg$TT_TU_5100,
    trier_petrisberg$TT_TU_5100 < -40 |
      trier_petrisberg$TT_TU_5100  > 45,
    NA
  )


summary(berlin_dahlem)
summary(fehmarn)
summary(rostock_warnemuende)
summary(magdeburg)
summary(trier_petrisberg)

# Join the weather stations without missing data
list_weather_data <- list(bremen,
                          dresden_klotzsche,
                          erfurt_weimar,
                          frankfurt_main,
                          hamburg_fuhlsbuettel,
                          hannover,
                          koeln_bonn,
                          muenchen_flughafen,
                          potsdam,
                          saarbruecken_ensheim,
                          stuttgart_echterdingen
                          )


weather_data_combined <- list_weather_data %>% reduce(inner_join, by = "MESS_DATUM")


# Join the weather stations with missing data
weather_data_combined <- weather_data_combined %>%
  left_join(berlin_dahlem, by = "MESS_DATUM") %>%
  left_join(fehmarn, by = "MESS_DATUM") %>%
  left_join(rostock_warnemuende, by = "MESS_DATUM") %>%
  left_join(magdeburg, by = "MESS_DATUM") %>%
  left_join(trier_petrisberg, by = "MESS_DATUM")

# Get the amount of columns that are not NA for each row
weather_data_combined <- weather_data_combined %>%
  mutate(ValidValuesAmount = rowSums(!is.na(weather_data_combined[,2:17])))

# Calculate the sum of all columns that are not NA for each row
weather_data_combined <- weather_data_combined %>%
  mutate(RowSumValidValues = rowSums(weather_data_combined[,2:17], na.rm = TRUE))

# Calculate the mean temperature over all stations
weather_data_combined <- weather_data_combined %>%
  mutate(MeanTemperature = rowMeans(weather_data_combined[,2:17], na.rm = TRUE))


# Calculate temperature values for an average year over the years 2010 - 2022
weather_data_average_yer <- weather_data_combined %>%
  select(c("MESS_DATUM", "MeanTemperature")) %>%
  mutate(Date = substr(as.character(MESS_DATUM), 5, 10)) %>%
  select(-c(MESS_DATUM)) %>%
  mutate_if(is.character, as.factor) %>%
  group_by(Date) %>%
  summarise(MeanTemperature = mean(MeanTemperature), .groups = 'drop')

# Filter leap year and round mean temperature
weather_data_average_yer <- weather_data_average_yer %>%
  filter(substr(as.character(weather_data_average_yer$Date), 1, 4) != "0229") %>%
  mutate(RoundedMeanTemperature = as.character(round(MeanTemperature, 1)))

# Write temperature values of average year to txt file
writeLines(weather_data_average_yer$RoundedMeanTemperature,
           "data/weatherstations/output/averageyear.txt",
           sep = ",")

# Write temperature values of average year to csv file
write_csv2(weather_data_average_yer,
           "data/weatherstations/output/averageyear.csv")


# Calculate temperature values for the cold year 2010
weather_data_2010 <- weather_data_combined %>%
  filter(substr(as.character(weather_data_combined$MESS_DATUM), 1, 4) == 2010) %>%
  select(c("MESS_DATUM", "MeanTemperature")) %>%
  mutate(Date = substr(as.character(MESS_DATUM), 5, 10)) %>%
  select(-c(MESS_DATUM)) %>%
  mutate(RoundedMeanTemperature = as.character(round(MeanTemperature, 1)))

# Write temperature values of year 2010 to txt file
writeLines(weather_data_2010$RoundedMeanTemperature,
           "data/weatherstations/output/year2010.txt",
           sep = ",")

# Write temperature values of year 2010 to csv file
write_csv2(weather_data_2010,
           "data/weatherstations/output/year2010.csv")


# Calculate temperature values for the hot year 2022
weather_data_2022 <- weather_data_combined %>%
  filter(substr(as.character(weather_data_combined$MESS_DATUM), 1, 4) == 2022) %>%
  select(c("MESS_DATUM", "MeanTemperature")) %>%
  mutate(Date = substr(as.character(MESS_DATUM), 5, 10)) %>%
  select(-c(MESS_DATUM)) %>%
  mutate(RoundedMeanTemperature = as.character(round(MeanTemperature, 1)))

# Write temperature values of year 2022 to txt file
writeLines(weather_data_2022$RoundedMeanTemperature,
           "data/weatherstations/output/year2022.txt",
           sep = ",")

# Write temperature values of year 2022 to csv file
write_csv2(weather_data_2022,
           "data/weatherstations/output/year2022.csv")
