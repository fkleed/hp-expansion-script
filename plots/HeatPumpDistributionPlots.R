# Load required packages
library(tidyverse)
library("dplyr")
library(readxl)


# Read the data
nuts3regioninfo <-
  read_excel("data/buildingstructure/nuts3regioninfo.xlsx") %>% mutate_if(is.character, as.factor)

building_stock_2030_with_hp_distribution <-
  read_csv("data/output/heatpumpexpansion/building_stock_2030_with_hp_distribution.csv") %>% mutate_if(is.character, as.factor)


# Calculate the regions with the max and min heat pump amount
nuts3_heat_pump_distribution <-
  building_stock_2030_with_hp_distribution %>%
  select(c(
    "NUTS3Code",
    "HPAmountAir",
    "HPAmountProbe",
    "HPAmountCollector"
  ))

nuts3_heat_pump_distribution <-
  nuts3_heat_pump_distribution %>%
  group_by(NUTS3Code) %>%
  summarise(
    "ASHP" = sum(HPAmountAir),
    "GSHP Probe" = sum(HPAmountProbe),
    "GSHP Collector" = sum(HPAmountCollector)
  ) %>%
  mutate(HPSum = ASHP + `GSHP Probe` + `GSHP Collector` )

nuts3_heat_pump_distribution_min_five <-
  nuts3_heat_pump_distribution %>%
  slice_min(HPSum, n = 5)

nuts3_heat_pump_distribution_max_five <-
  nuts3_heat_pump_distribution %>%
  slice_max(HPSum, n = 5)

nuts3_heat_pump_distribution_min_max_five <-
  nuts3_heat_pump_distribution_min_five %>%
  rbind(nuts3_heat_pump_distribution_max_five) %>%
  gather("Type", "Amount", 2:4) %>%
  left_join(nuts3regioninfo, by = c("NUTS3Code"))

nuts3_heat_pump_distribution_min_max_five <-
  nuts3_heat_pump_distribution_min_max_five %>%
  select(c(
    "NUTS3Code",
    "HPSum",
    "Type",
    "Amount",
    "NUTS3Name",
    "NUTS3Type"
  )) %>%
  mutate(NUTS3Name = paste(gsub(",.*", "", NUTS3Name), NUTS3Type, sep = ", ")) %>%
  select(-c("NUTS3Type"))


# Plot bar chart for the amount of heat pumps
bar_chart_hp_amount_min_max_five <-
  ggplot(data = nuts3_heat_pump_distribution_min_max_five) +
  geom_bar(mapping = aes(
    x = reorder(NUTS3Name,-Amount),
    y = Amount,
    fill = factor(Type,
                  level = c("GSHP Collector",
                            "GSHP Probe",
                            "ASHP"))
  ),
  stat = "identity") +
  labs(x = "NUTS 3 Name",
       y = "Number of heat pumps",
       fill = "Heat pump type") +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme(legend.position = "bottom")

bar_chart_hp_amount_min_max_five


# Calculate the heat pump amount on a federal state basis
heat_pump_distribution_federal_states <- nuts3_heat_pump_distribution %>%
  left_join(nuts3regioninfo, by = c("NUTS3Code")) %>%
  select(
    c(
      "ASHP",
      "GSHP Probe",
      "GSHP Collector",
      "HPSum",
      "NUTS1Name"
    )
  ) %>%
  group_by(NUTS1Name) %>%
  summarise(
    "ASHP" = sum(ASHP),
    "GSHP Probe" = sum(`GSHP Probe`),
    "GSHP Collector" = sum(`GSHP Collector`),
    "HPSum" = sum(HPSum)
  )

heat_pump_distribution_federal_states <- heat_pump_distribution_federal_states %>%
  gather("Type", "Amount", 2:4)



# Plot the amount of heat pumps per federal state
bar_chart_hp_per_federal_state <-
  ggplot(data = heat_pump_distribution_federal_states) +
  geom_bar(mapping = aes(
    x = reorder(NUTS1Name,-Amount),
    y = Amount,
    fill = factor(Type,
                  level = c("GSHP Collector",
                            "GSHP Probe",
                            "ASHP"))
  ),
  stat = "identity") +
  labs(x = "Federal state",
       y = "Number of heat pumps",
       fill = "Heat pump type") +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme(legend.position = "bottom")

bar_chart_hp_per_federal_state


# Save the plots
ggsave(
  "plots/output/heatpumpdistribution/bar_chart_hp_amount_min_max_five.png",
  bar_chart_hp_amount_min_max_five,
  width = 25,
  units = "cm"
)

ggsave(
  "plots/output/heatpumpdistribution/bar_chart_hp_per_federal_state.png",
  bar_chart_hp_per_federal_state,
  width = 25,
  units = "cm"
)
