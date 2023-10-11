# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read the regions electricity demand data
electricity_demand_reference_germany_sh_and_hw <-
  read_csv(
    "data/output/findings/electricity-demand-germany/electricity_demand_reference_germany_sh_and_hw.csv"
  )

electricity_demand_cold_germany_sh_and_hw <-
  read_csv(
    "data/output/findings/electricity-demand-germany/electricity_demand_cold_germany_sh_and_hw.csv"
  )

electricity_demand_hot_germany_sh_and_hw <-
  read_csv(
    "data/output/findings/electricity-demand-germany/electricity_demand_hot_germany_sh_and_hw.csv"
  )


electricity_demand_reference_germany_sh_only <-
  read_csv(
    "data/output/findings/electricity-demand-germany/electricity_demand_reference_germany_sh_only.csv"
  )

electricity_demand_cold_germany_sh_only <-
  read_csv(
    "data/output/findings/electricity-demand-germany/electricity_demand_cold_germany_sh_only.csv"
  )

electricity_demand_hot_germany_sh_only <-
  read_csv(
    "data/output/findings/electricity-demand-germany/electricity_demand_hot_germany_sh_only.csv"
  )


# Plot the combined electricity demand fo Germany

# For space heating and hot water

electricity_demand_reference_germany_sh_and_hw <-
  electricity_demand_reference_germany_sh_and_hw %>%
  mutate("Temperature series" = 2017)

electricity_demand_cold_germany_sh_and_hw <-
  electricity_demand_cold_germany_sh_and_hw %>%
  mutate("Temperature series" = 2010)

electricity_demand_hot_germany_sh_and_hw <-
  electricity_demand_hot_germany_sh_and_hw %>%
  mutate("Temperature series" = 2022)


electricity_demand_germany_sh_and_hw <-
  electricity_demand_reference_germany_sh_and_hw %>%
  rbind(electricity_demand_cold_germany_sh_and_hw) %>%
  rbind(electricity_demand_hot_germany_sh_and_hw)

electricity_demand_germany_sh_and_hw_plot <-   ggplot(data = electricity_demand_germany_sh_and_hw,
                                                      aes(DATEISO,
                                                          `Electricity demand` / 1000000)) +
  geom_line(lwd = 0.5) +
  facet_wrap(~ `Temperature series`, ncol = 1) +
  coord_cartesian(ylim = c(0, 55)) +
  labs(x = "Date",
       y = "Electricity demand in GWh")


electricity_demand_germany_sh_and_hw_plot


# For space heating only
electricity_demand_reference_germany_sh_only <-
  electricity_demand_reference_germany_sh_only %>%
  mutate("Temperature series" = 2017)

electricity_demand_cold_germany_sh_only <-
  electricity_demand_cold_germany_sh_only %>%
  mutate("Temperature series" = 2010)

electricity_demand_hot_germany_sh_only <-
  electricity_demand_hot_germany_sh_only %>%
  mutate("Temperature series" = 2022)


electricity_demand_germany_sh_only <-
  electricity_demand_reference_germany_sh_only %>%
  rbind(electricity_demand_cold_germany_sh_only) %>%
  rbind(electricity_demand_hot_germany_sh_only)


electricity_demand_germany_sh_only_plot <-   ggplot(data = electricity_demand_germany_sh_only,
                                                      aes(DATEISO,
                                                          `Electricity demand` / 1000000)) +
  geom_line(lwd = 0.5) +
  facet_wrap(~ `Temperature series`, ncol = 1) +
  coord_cartesian(ylim = c(0, 55)) +
  labs(x = "Date",
       y = "Electricity demand in GWh")


electricity_demand_germany_sh_only_plot


# Save the plots
ggsave(
  "plots/output/regionselectricitydemand/electricitydemandgermanyovertime/electricity_demand_germany_sh_and_hw_plot.png",
  electricity_demand_germany_sh_and_hw_plot,
  width = 30,
  units = "cm"
)

ggsave(
  "plots/output/regionselectricitydemand/electricitydemandgermanyovertime/electricity_demand_germany_sh_only_plot.png",
  electricity_demand_germany_sh_only_plot,
  width = 30,
  units = "cm"
)
