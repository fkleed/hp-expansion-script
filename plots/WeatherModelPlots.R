# Load required packages
library(tidyverse)
library("dplyr")

# Read the data
air_temp_2010 <-
  read_csv2("data/output/weathermodel/year2010.csv") %>%
  select(-c("RoundedMeanTemperature")) %>%
  rename("Temperature" = "MeanTemperature") %>%
  mutate(Kind = "Air temperature") %>%
  mutate_if(is.character, as.factor) %>%
  relocate(Date, .before = Temperature)

air_temp_2017 <-
  read_csv2("data/output/weathermodel/year2017.csv") %>%
  select(-c("RoundedMeanTemperature")) %>%
  rename("Temperature" = "MeanTemperature") %>%
  mutate(Kind = "Air temperature") %>%
  mutate_if(is.character, as.factor) %>%
  relocate(Date, .before = Temperature)

air_temp_2022 <-
  read_csv2("data/output/weathermodel/year2022.csv") %>%
  select(-c("RoundedMeanTemperature")) %>%
  rename("Temperature" = "MeanTemperature") %>%
  mutate(Kind = "Air temperature") %>%
  mutate_if(is.character, as.factor) %>%
  relocate(Date, .before = Temperature)


soil_temp_collector_2010 <-
  read_csv2("data/output/weathermodel/soiltempyear2010.csv") %>%
  rename("Temperature" = "MeanSoilTemperature") %>%
  mutate(Kind = "Soil temperature collector") %>%
  mutate_if(is.character, as.factor)

soil_temp_collector_2017 <-
  read_csv2("data/output/weathermodel/soiltempyear2017.csv") %>%
  rename("Temperature" = "MeanSoilTemperature") %>%
  mutate(Kind = "Soil temperature collector") %>%
  mutate_if(is.character, as.factor)

soil_temp_collector_2022 <-
  read_csv2("data/output/weathermodel/soiltempyear2022.csv") %>%
  rename("Temperature" = "MeanSoilTemperature") %>%
  mutate(Kind = "Soil temperature collector") %>%
  mutate_if(is.character, as.factor)


soil_temp_probe <- air_temp_2010 %>%
  select(c("Date")) %>%
  mutate(Temperature = 10,
         Kind = "Soil temperature probe")


temp_2010 <- air_temp_2010 %>%
  rbind(soil_temp_collector_2010) %>%
  rbind(soil_temp_probe)

temp_2017 <- air_temp_2017 %>%
  rbind(soil_temp_collector_2017) %>%
  rbind(soil_temp_probe)

temp_2022 <- air_temp_2022 %>%
  rbind(soil_temp_collector_2022) %>%
  rbind(soil_temp_probe)

# Add the ISOdate
temp_2010 <- temp_2010 %>%
  mutate(DATEISO = ISOdate(
    2010,
    ifelse(substr(Date, 1, 2) == "00", 0, sub("^0+", "", substr(Date, 1, 2))),
    ifelse(substr(Date, 3, 4) == "00", 0, sub("^0+", "", substr(Date, 3, 4))),
    ifelse(substr(Date, 5, 6) == "00", 0, sub("^0+", "", substr(Date, 5, 6)))
  ))

temp_2017 <- temp_2017 %>%
  mutate(DATEISO = ISOdate(
    2017,
    ifelse(substr(Date, 1, 2) == "00", 0, sub("^0+", "", substr(Date, 1, 2))),
    ifelse(substr(Date, 3, 4) == "00", 0, sub("^0+", "", substr(Date, 3, 4))),
    ifelse(substr(Date, 5, 6) == "00", 0, sub("^0+", "", substr(Date, 5, 6)))
  ))

temp_2022 <- temp_2022 %>%
  mutate(DATEISO = ISOdate(
    2022,
    ifelse(substr(Date, 1, 2) == "00", 0, sub("^0+", "", substr(Date, 1, 2))),
    ifelse(substr(Date, 3, 4) == "00", 0, sub("^0+", "", substr(Date, 3, 4))),
    ifelse(substr(Date, 5, 6) == "00", 0, sub("^0+", "", substr(Date, 5, 6)))
  ))


# Plot the temperature data for the different years
temp_2010_plot <-
  ggplot(data=temp_2010, aes(DATEISO, Temperature, color=Kind)) +
  geom_line(lwd=1.3) + ylab("Temperature (°C)") + xlab("Date") +
  scale_color_brewer(palette = "Set2") +
  guides(color = guide_legend(title = "Kind of temperature"))

temp_2010_plot

temp_2017_plot <-
  ggplot(data=temp_2017, aes(DATEISO, Temperature, color=Kind)) +
  geom_line(lwd=1.3) + ylab("Temperature (°C)") + xlab("Date") +
  scale_color_brewer(palette = "Set2") +
  guides(color = guide_legend(title = "Kind of temperature"))

temp_2017_plot

temp_2022_plot <-
  ggplot(data=temp_2022, aes(DATEISO, Temperature, color=Kind)) +
  geom_line(lwd=1.3) + ylab("Temperature (°C)") + xlab("Date") +
  scale_color_brewer(palette = "Set2") +
  guides(color = guide_legend(title = "Kind of temperature"))

temp_2022_plot


# Save plots
ggsave(
  "plots/output/weathermodel/temp_2010_plot.png",
  temp_2010_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/weathermodel/temp_2017_plot.png",
  temp_2017_plot,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/weathermodel/temp_2022_plot.png",
  temp_2022_plot,
  width = 25,
  units = "cm"
)
