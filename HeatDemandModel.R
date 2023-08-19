# Load required packages
library(tidyverse)
library(readxl)
library("dplyr")

# Read the heat demand of the different building categories with different temperature profile
# Read nPro data from the average year
eh_1919_1948_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-1919-1948.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1919_1948_avg" = "Raumwärme (kW)",
    "HotWater_1919_1948_avg" = "Trinkwarmwasser (kW)"
  )

eh_1949_1957_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-1949-1957.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1949_1957_avg" = "Raumwärme (kW)",
    "HotWater_1949_1957_avg" = "Trinkwarmwasser (kW)"
  )

eh_1958_1968_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-1958-1968.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1958_1968_avg" = "Raumwärme (kW)",
    "HotWater_1958_1968_avg" = "Trinkwarmwasser (kW)"
  )

eh_1969_1978_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-1969-1978.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1969_1978_avg" = "Raumwärme (kW)",
    "HotWater_1969_1978_avg" = "Trinkwarmwasser (kW)"
  )

eh_1979_1983_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-1979-1983.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1979_1983_avg" = "Raumwärme (kW)",
    "HotWater_1979_1983_avg" = "Trinkwarmwasser (kW)"
  )

eh_1984_1994_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-1984-1994.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1984_1994_avg" = "Raumwärme (kW)",
    "HotWater_1984_1994_avg" = "Trinkwarmwasser (kW)"
  )

eh_1995_2001_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-1995-2001.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1995_2001_avg" = "Raumwärme (kW)",
    "HotWater_1995_2001_avg" = "Trinkwarmwasser (kW)"
  )

eh_2002_ende_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-2002-ende.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_2002_ende_avg" = "Raumwärme (kW)",
    "HotWater_2002_ende_avg" = "Trinkwarmwasser (kW)"
  )

eh_beginn_1918_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-beginn-1918.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_beginn_1918_avg" = "Raumwärme (kW)",
    "HotWater_beginn_1918_avg" = "Trinkwarmwasser (kW)"
  )

eh_kfw_40_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-KfW-40.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_40_avg" = "Raumwärme (kW)",
    "HotWater_kfw_40_avg" = "Trinkwarmwasser (kW)"
  )

eh_kfw_55_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-KfW-55.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_55_avg" = "Raumwärme (kW)",
    "HotWater_kfw_55_avg" = "Trinkwarmwasser (kW)"
  )

eh_kfw_70_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-KfW-70.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_70_avg" = "Raumwärme (kW)",
    "HotWater_kfw_70_avg" = "Trinkwarmwasser (kW)"
  )

eh_kfw_85_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-KfW-85.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_85_avg" = "Raumwärme (kW)",
    "HotWater_kfw_85_avg" = "Trinkwarmwasser (kW)"
  )

eh_passivhaus_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/EH-Passivhaus.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_passivhaus_avg" = "Raumwärme (kW)",
    "HotWater_passivhaus_avg" = "Trinkwarmwasser (kW)"
  )

mh_1919_1948_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-1919-1948.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1919_1948_avg" = "Raumwärme (kW)",
    "HotWater_1919_1948_avg" = "Trinkwarmwasser (kW)"
  )

mh_1949_1957_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-1949-1957.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1949_1957_avg" = "Raumwärme (kW)",
    "HotWater_1949_1957_avg" = "Trinkwarmwasser (kW)"
  )

mh_1958_1968_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-1958-1968.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1958_1968_avg" = "Raumwärme (kW)",
    "HotWater_1958_1968_avg" = "Trinkwarmwasser (kW)"
  )

mh_1969_1978_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-1969-1978.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1969_1978_avg" = "Raumwärme (kW)",
    "HotWater_1969_1978_avg" = "Trinkwarmwasser (kW)"
  )

mh_1979_1983_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-1979-1983.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1979_1983_avg" = "Raumwärme (kW)",
    "HotWater_1979_1983_avg" = "Trinkwarmwasser (kW)"
  )

mh_1984_1994_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-1984-1994.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1984_1994_avg" = "Raumwärme (kW)",
    "HotWater_1984_1994_avg" = "Trinkwarmwasser (kW)"
  )

mh_1995_2001_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-1995-2001.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1995_2001_avg" = "Raumwärme (kW)",
    "HotWater_1995_2001_avg" = "Trinkwarmwasser (kW)"
  )

mh_2002_ende_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-2002-ende.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_2002_ende_avg" = "Raumwärme (kW)",
    "HotWater_2002_ende_avg" = "Trinkwarmwasser (kW)"
  )

mh_beginn_1918_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-beginn-1918.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_beginn_1918_avg" = "Raumwärme (kW)",
    "HotWater_beginn_1918_avg" = "Trinkwarmwasser (kW)"
  )

mh_kfw_40_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-KfW-40.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_40_avg" = "Raumwärme (kW)",
    "HotWater_kfw_40_avg" = "Trinkwarmwasser (kW)"
  )

mh_kfw_55_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-KfW-55.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_55_avg" = "Raumwärme (kW)",
    "HotWater_kfw_55_avg" = "Trinkwarmwasser (kW)"
  )

mh_kfw_70_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-KfW-70.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_70_avg" = "Raumwärme (kW)",
    "HotWater_kfw_70_avg" = "Trinkwarmwasser (kW)"
  )

mh_kfw_85_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-KfW-85.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_85_avg" = "Raumwärme (kW)",
    "HotWater_kfw_85_avg" = "Trinkwarmwasser (kW)"
  )

mh_passivhaus_avg <-
  read_csv("data/heatdemand/2023-08-09-Quartieraverageyear/MH-Passivhaus.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_passivhaus_avg" = "Raumwärme (kW)",
    "HotWater_passivhaus_avg" = "Trinkwarmwasser (kW)"
  )


# Read nPro data from the cold year
# Read nPro data from the average year
eh_1919_1948_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-1919-1948.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1919_1948_cold" = "Raumwärme (kW)",
    "HotWater_1919_1948_cold" = "Trinkwarmwasser (kW)"
  )

eh_1949_1957_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-1949-1957.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1949_1957_cold" = "Raumwärme (kW)",
    "HotWater_1949_1957_cold" = "Trinkwarmwasser (kW)"
  )

eh_1958_1968_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-1958-1968.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1958_1968_cold" = "Raumwärme (kW)",
    "HotWater_1958_1968_cold" = "Trinkwarmwasser (kW)"
  )

eh_1969_1978_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-1969-1978.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1969_1978_cold" = "Raumwärme (kW)",
    "HotWater_1969_1978_cold" = "Trinkwarmwasser (kW)"
  )

eh_1979_1983_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-1979-1983.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1979_1983_cold" = "Raumwärme (kW)",
    "HotWater_1979_1983_cold" = "Trinkwarmwasser (kW)"
  )

eh_1984_1994_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-1984-1994.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1984_1994_cold" = "Raumwärme (kW)",
    "HotWater_1984_1994_cold" = "Trinkwarmwasser (kW)"
  )

eh_1995_2001_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-1995-2001.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1995_2001_cold" = "Raumwärme (kW)",
    "HotWater_1995_2001_cold" = "Trinkwarmwasser (kW)"
  )

eh_2002_ende_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-2002-ende.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_2002_ende_cold" = "Raumwärme (kW)",
    "HotWater_2002_ende_cold" = "Trinkwarmwasser (kW)"
  )

eh_beginn_1918_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-beginn-1918.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_beginn_1918_cold" = "Raumwärme (kW)",
    "HotWater_beginn_1918_cold" = "Trinkwarmwasser (kW)"
  )

eh_kfw_40_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-KfW-40.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_40_cold" = "Raumwärme (kW)",
    "HotWater_kfw_40_cold" = "Trinkwarmwasser (kW)"
  )

eh_kfw_55_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-KfW-55.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_55_cold" = "Raumwärme (kW)",
    "HotWater_kfw_55_cold" = "Trinkwarmwasser (kW)"
  )

eh_kfw_70_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-KfW-70.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_70_cold" = "Raumwärme (kW)",
    "HotWater_kfw_70_cold" = "Trinkwarmwasser (kW)"
  )

eh_kfw_85_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-KfW-85.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_85_cold" = "Raumwärme (kW)",
    "HotWater_kfw_85_cold" = "Trinkwarmwasser (kW)"
  )

eh_passivhaus_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/EH-Passivhaus.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_passivhaus_cold" = "Raumwärme (kW)",
    "HotWater_passivhaus_cold" = "Trinkwarmwasser (kW)"
  )

mh_1919_1948_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-1919-1948.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1919_1948_cold" = "Raumwärme (kW)",
    "HotWater_1919_1948_cold" = "Trinkwarmwasser (kW)"
  )

mh_1949_1957_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-1949-1957.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1949_1957_cold" = "Raumwärme (kW)",
    "HotWater_1949_1957_cold" = "Trinkwarmwasser (kW)"
  )

mh_1958_1968_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-1958-1968.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1958_1968_cold" = "Raumwärme (kW)",
    "HotWater_1958_1968_cold" = "Trinkwarmwasser (kW)"
  )

mh_1969_1978_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-1969-1978.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1969_1978_cold" = "Raumwärme (kW)",
    "HotWater_1969_1978_cold" = "Trinkwarmwasser (kW)"
  )

mh_1979_1983_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-1979-1983.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1979_1983_cold" = "Raumwärme (kW)",
    "HotWater_1979_1983_cold" = "Trinkwarmwasser (kW)"
  )

mh_1984_1994_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-1984-1994.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1984_1994_cold" = "Raumwärme (kW)",
    "HotWater_1984_1994_cold" = "Trinkwarmwasser (kW)"
  )

mh_1995_2001_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-1995-2001.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1995_2001_cold" = "Raumwärme (kW)",
    "HotWater_1995_2001_cold" = "Trinkwarmwasser (kW)"
  )

mh_2002_ende_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-2002-ende.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_2002_ende_cold" = "Raumwärme (kW)",
    "HotWater_2002_ende_cold" = "Trinkwarmwasser (kW)"
  )

mh_beginn_1918_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-beginn-1918.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_beginn_1918_cold" = "Raumwärme (kW)",
    "HotWater_beginn_1918_cold" = "Trinkwarmwasser (kW)"
  )

mh_kfw_40_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-KfW-40.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_40_cold" = "Raumwärme (kW)",
    "HotWater_kfw_40_cold" = "Trinkwarmwasser (kW)"
  )

mh_kfw_55_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-KfW-55.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_55_cold" = "Raumwärme (kW)",
    "HotWater_kfw_55_cold" = "Trinkwarmwasser (kW)"
  )

mh_kfw_70_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-KfW-70.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_70_cold" = "Raumwärme (kW)",
    "HotWater_kfw_70_cold" = "Trinkwarmwasser (kW)"
  )

mh_kfw_85_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-KfW-85.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_85_cold" = "Raumwärme (kW)",
    "HotWater_kfw_85_cold" = "Trinkwarmwasser (kW)"
  )

mh_passivhaus_cold <-
  read_csv("data/heatdemand/2023-08-09-Quartiercoldyear(2010)/MH-Passivhaus.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_passivhaus_cold" = "Raumwärme (kW)",
    "HotWater_passivhaus_cold" = "Trinkwarmwasser (kW)"
  )

# Read nPro data from the hot year
# Read nPro data from the average year
eh_1919_1948_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-1919-1948.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1919_1948_hot" = "Raumwärme (kW)",
    "HotWater_1919_1948_hot" = "Trinkwarmwasser (kW)"
  )

eh_1949_1957_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-1949-1957.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1949_1957_hot" = "Raumwärme (kW)",
    "HotWater_1949_1957_hot" = "Trinkwarmwasser (kW)"
  )

eh_1958_1968_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-1958-1968.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1958_1968_hot" = "Raumwärme (kW)",
    "HotWater_1958_1968_hot" = "Trinkwarmwasser (kW)"
  )

eh_1969_1978_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-1969-1978.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1969_1978_hot" = "Raumwärme (kW)",
    "HotWater_1969_1978_hot" = "Trinkwarmwasser (kW)"
  )

eh_1979_1983_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-1979-1983.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1979_1983_hot" = "Raumwärme (kW)",
    "HotWater_1979_1983_hot" = "Trinkwarmwasser (kW)"
  )

eh_1984_1994_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-1984-1994.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1984_1994_hot" = "Raumwärme (kW)",
    "HotWater_1984_1994_hot" = "Trinkwarmwasser (kW)"
  )

eh_1995_2001_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-1995-2001.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1995_2001_hot" = "Raumwärme (kW)",
    "HotWater_1995_2001_hot" = "Trinkwarmwasser (kW)"
  )

eh_2002_ende_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-2002-ende.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_2002_ende_hot" = "Raumwärme (kW)",
    "HotWater_2002_ende_hot" = "Trinkwarmwasser (kW)"
  )

eh_beginn_1918_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-beginn-1918.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_beginn_1918_hot" = "Raumwärme (kW)",
    "HotWater_beginn_1918_hot" = "Trinkwarmwasser (kW)"
  )

eh_kfw_40_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-KfW-40.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_40_hot" = "Raumwärme (kW)",
    "HotWater_kfw_40_hot" = "Trinkwarmwasser (kW)"
  )

eh_kfw_55_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-KfW-55.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_55_hot" = "Raumwärme (kW)",
    "HotWater_kfw_55_hot" = "Trinkwarmwasser (kW)"
  )

eh_kfw_70_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-KfW-70.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_70_hot" = "Raumwärme (kW)",
    "HotWater_kfw_70_hot" = "Trinkwarmwasser (kW)"
  )

eh_kfw_85_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-KfW-85.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_85_hot" = "Raumwärme (kW)",
    "HotWater_kfw_85_hot" = "Trinkwarmwasser (kW)"
  )

eh_passivhaus_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/EH-Passivhaus.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_passivhaus_hot" = "Raumwärme (kW)",
    "HotWater_passivhaus_hot" = "Trinkwarmwasser (kW)"
  )

mh_1919_1948_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-1919-1948.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1919_1948_hot" = "Raumwärme (kW)",
    "HotWater_1919_1948_hot" = "Trinkwarmwasser (kW)"
  )

mh_1949_1957_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-1949-1957.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1949_1957_hot" = "Raumwärme (kW)",
    "HotWater_1949_1957_hot" = "Trinkwarmwasser (kW)"
  )

mh_1958_1968_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-1958-1968.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1958_1968_hot" = "Raumwärme (kW)",
    "HotWater_1958_1968_hot" = "Trinkwarmwasser (kW)"
  )

mh_1969_1978_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-1969-1978.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1969_1978_hot" = "Raumwärme (kW)",
    "HotWater_1969_1978_hot" = "Trinkwarmwasser (kW)"
  )

mh_1979_1983_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-1979-1983.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1979_1983_hot" = "Raumwärme (kW)",
    "HotWater_1979_1983_hot" = "Trinkwarmwasser (kW)"
  )

mh_1984_1994_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-1984-1994.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1984_1994_hot" = "Raumwärme (kW)",
    "HotWater_1984_1994_hot" = "Trinkwarmwasser (kW)"
  )

mh_1995_2001_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-1995-2001.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_1995_2001_hot" = "Raumwärme (kW)",
    "HotWater_1995_2001_hot" = "Trinkwarmwasser (kW)"
  )

mh_2002_ende_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-2002-ende.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_2002_ende_hot" = "Raumwärme (kW)",
    "HotWater_2002_ende_hot" = "Trinkwarmwasser (kW)"
  )

mh_beginn_1918_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-beginn-1918.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_beginn_1918_hot" = "Raumwärme (kW)",
    "HotWater_beginn_1918_hot" = "Trinkwarmwasser (kW)"
  )

mh_kfw_40_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-KfW-40.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_40_hot" = "Raumwärme (kW)",
    "HotWater_kfw_40_hot" = "Trinkwarmwasser (kW)"
  )

mh_kfw_55_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-KfW-55.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_55_hot" = "Raumwärme (kW)",
    "HotWater_kfw_55_hot" = "Trinkwarmwasser (kW)"
  )

mh_kfw_70_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-KfW-70.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_70_hot" = "Raumwärme (kW)",
    "HotWater_kfw_70_hot" = "Trinkwarmwasser (kW)"
  )

mh_kfw_85_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-KfW-85.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_kfw_85_hot" = "Raumwärme (kW)",
    "HotWater_kfw_85_hot" = "Trinkwarmwasser (kW)"
  )

mh_passivhaus_hot <-
  read_csv("data/heatdemand/2023-08-09-Quartierhotyear(2022)/MH-Passivhaus.csv") %>%
  select(
    -c(
      "Klimatisierung (kW)",
      "Prozesskälte (kW)",
      "Nutzerstrom (kW)",
      "Elektromobilität (kW)"
    )
  ) %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    "Time" = "Zeit (TT-MM hh:mm)",
    "SpaceHeat_passivhaus_hot" = "Raumwärme (kW)",
    "HotWater_passivhaus_hot" = "Trinkwarmwasser (kW)"
  )


# Derive data single family homes
# Derive data for 1949 - 1978
eh_1949_1978_avg <-
  list(eh_1949_1957_avg, eh_1958_1968_avg, eh_1969_1978_avg) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1949_1978_avg = (
      SpaceHeat_1949_1957_avg + SpaceHeat_1958_1968_avg + SpaceHeat_1969_1978_avg
    ) / 3,
    HotWater_1949_1978_avg = (
      HotWater_1949_1957_avg + HotWater_1958_1968_avg + HotWater_1969_1978_avg
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_1949_1957_avg",
      "HotWater_1949_1957_avg",
      "SpaceHeat_1958_1968_avg",
      "HotWater_1958_1968_avg",
      "SpaceHeat_1969_1978_avg",
      "HotWater_1969_1978_avg"
    )
  )

eh_1949_1978_cold <-
  list(eh_1949_1957_cold, eh_1958_1968_cold, eh_1969_1978_cold) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1949_1978_cold = (
      SpaceHeat_1949_1957_cold + SpaceHeat_1958_1968_cold + SpaceHeat_1969_1978_cold
    ) / 3,
    HotWater_1949_1978_cold = (
      HotWater_1949_1957_cold + HotWater_1958_1968_cold + HotWater_1969_1978_cold
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_1949_1957_cold",
      "HotWater_1949_1957_cold",
      "SpaceHeat_1958_1968_cold",
      "HotWater_1958_1968_cold",
      "SpaceHeat_1969_1978_cold",
      "HotWater_1969_1978_cold"
    )
  )

eh_1949_1978_hot <-
  list(eh_1949_1957_hot, eh_1958_1968_hot, eh_1969_1978_hot) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1949_1978_hot = (
      SpaceHeat_1949_1957_hot + SpaceHeat_1958_1968_hot + SpaceHeat_1969_1978_hot
    ) / 3,
    HotWater_1949_1978_hot = (
      HotWater_1949_1957_hot + HotWater_1958_1968_hot + HotWater_1969_1978_hot
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_1949_1957_hot",
      "HotWater_1949_1957_hot",
      "SpaceHeat_1958_1968_hot",
      "HotWater_1958_1968_hot",
      "SpaceHeat_1969_1978_hot",
      "HotWater_1969_1978_hot"
    )
  )


# Derive data for 1979 - 1986
eh_1979_1986_avg <-
  list(eh_1979_1983_avg, eh_1984_1994_avg) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1979_1986_avg = (SpaceHeat_1979_1983_avg * 5 / 8 + SpaceHeat_1984_1994_avg * 3 /
                                 8),
    HotWater_1979_1986_avg = (HotWater_1979_1983_avg * 5 / 8 + HotWater_1984_1994_avg * 3 /
                                8)
  ) %>%
  select(
    -c(
      "SpaceHeat_1979_1983_avg",
      "HotWater_1979_1983_avg",
      "SpaceHeat_1984_1994_avg",
      "HotWater_1984_1994_avg"
    )
  )

eh_1979_1986_cold <-
  list(eh_1979_1983_cold, eh_1984_1994_cold) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1979_1986_cold = (SpaceHeat_1979_1983_cold * 5 / 8 + SpaceHeat_1984_1994_cold * 3 /
                                  8),
    HotWater_1979_1986_cold = (HotWater_1979_1983_cold * 5 / 8 + HotWater_1984_1994_cold * 3 /
                                 8)
  ) %>%
  select(
    -c(
      "SpaceHeat_1979_1983_cold",
      "HotWater_1979_1983_cold",
      "SpaceHeat_1984_1994_cold",
      "HotWater_1984_1994_cold"
    )
  )

eh_1979_1986_hot <-
  list(eh_1979_1983_hot, eh_1984_1994_hot) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1979_1986_hot = (SpaceHeat_1979_1983_hot * 5 / 8 + SpaceHeat_1984_1994_hot * 3 /
                                 8),
    HotWater_1979_1986_hot = (HotWater_1979_1983_hot * 5 / 8 + HotWater_1984_1994_hot * 3 /
                                8)
  ) %>%
  select(
    -c(
      "SpaceHeat_1979_1983_hot",
      "HotWater_1979_1983_hot",
      "SpaceHeat_1984_1994_hot",
      "HotWater_1984_1994_hot"
    )
  )


# Derive data for 1991 - 1995
eh_1991_1995_avg <-
  list(eh_1984_1994_avg, eh_1995_2001_avg) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1991_1995_avg = (SpaceHeat_1984_1994_avg * 4 / 5 + SpaceHeat_1995_2001_avg * 1 /
                                 5),
    HotWater_1991_1995_avg = (HotWater_1984_1994_avg * 4 / 5 + HotWater_1995_2001_avg * 1 /
                                5)
  ) %>%
  select(
    -c(
      "SpaceHeat_1984_1994_avg",
      "HotWater_1984_1994_avg",
      "SpaceHeat_1995_2001_avg",
      "HotWater_1995_2001_avg"
    )
  )

eh_1991_1995_cold <-
  list(eh_1984_1994_cold, eh_1995_2001_cold) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1991_1995_cold = (SpaceHeat_1984_1994_cold * 4 / 5 + SpaceHeat_1995_2001_cold * 1 /
                                  5),
    HotWater_1991_1995_cold = (HotWater_1984_1994_cold * 4 / 5 + HotWater_1995_2001_cold * 1 /
                                 5)
  ) %>%
  select(
    -c(
      "SpaceHeat_1984_1994_cold",
      "HotWater_1984_1994_cold",
      "SpaceHeat_1995_2001_cold",
      "HotWater_1995_2001_cold"
    )
  )

eh_1991_1995_hot <-
  list(eh_1984_1994_hot, eh_1995_2001_hot) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1991_1995_hot = (SpaceHeat_1984_1994_hot * 4 / 5 + SpaceHeat_1995_2001_hot * 1 /
                                 5),
    HotWater_1991_1995_hot = (HotWater_1984_1994_hot * 4 / 5 + HotWater_1995_2001_hot * 1 /
                                5)
  ) %>%
  select(
    -c(
      "SpaceHeat_1984_1994_hot",
      "HotWater_1984_1994_hot",
      "SpaceHeat_1995_2001_hot",
      "HotWater_1995_2001_hot"
    )
  )


# Derive data for 2012 - 2022
eh_2012_2022_avg <-
  list(eh_kfw_85_avg, eh_kfw_70_avg, eh_kfw_55_avg, eh_kfw_40_avg) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2012_2022_avg = (
      SpaceHeat_kfw_85_avg + SpaceHeat_kfw_70_avg + SpaceHeat_kfw_55_avg + SpaceHeat_kfw_40_avg
    ) / 4,
    HotWater_2012_2022_avg = (
      HotWater_kfw_85_avg + HotWater_kfw_70_avg + HotWater_kfw_55_avg + HotWater_kfw_40_avg
    ) / 4
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_85_avg",
      "SpaceHeat_kfw_70_avg",
      "SpaceHeat_kfw_55_avg",
      "SpaceHeat_kfw_40_avg",
      "HotWater_kfw_85_avg",
      "HotWater_kfw_70_avg",
      "HotWater_kfw_55_avg",
      "HotWater_kfw_40_avg"
    )
  )


eh_2012_2022_cold <-
  list(eh_kfw_85_cold,
       eh_kfw_70_cold,
       eh_kfw_55_cold,
       eh_kfw_40_cold) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2012_2022_cold = (
      SpaceHeat_kfw_85_cold + SpaceHeat_kfw_70_cold + SpaceHeat_kfw_55_cold + SpaceHeat_kfw_40_cold
    ) / 4,
    HotWater_2012_2022_cold = (
      HotWater_kfw_85_cold + HotWater_kfw_70_cold + HotWater_kfw_55_cold + HotWater_kfw_40_cold
    ) / 4
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_85_cold",
      "SpaceHeat_kfw_70_cold",
      "SpaceHeat_kfw_55_cold",
      "SpaceHeat_kfw_40_cold",
      "HotWater_kfw_85_cold",
      "HotWater_kfw_70_cold",
      "HotWater_kfw_55_cold",
      "HotWater_kfw_40_cold"
    )
  )

eh_2012_2022_hot <-
  list(eh_kfw_85_hot, eh_kfw_70_hot, eh_kfw_55_hot, eh_kfw_40_hot) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2012_2022_hot = (
      SpaceHeat_kfw_85_hot + SpaceHeat_kfw_70_hot + SpaceHeat_kfw_55_hot + SpaceHeat_kfw_40_hot
    ) / 4,
    HotWater_2012_2022_hot = (
      HotWater_kfw_85_hot + HotWater_kfw_70_hot + HotWater_kfw_55_hot + HotWater_kfw_40_hot
    ) / 4
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_85_hot",
      "SpaceHeat_kfw_70_hot",
      "SpaceHeat_kfw_55_hot",
      "SpaceHeat_kfw_40_hot",
      "HotWater_kfw_85_hot",
      "HotWater_kfw_70_hot",
      "HotWater_kfw_55_hot",
      "HotWater_kfw_40_hot"
    )
  )

# Derive data for 2023 - 2030
eh_2023_2030_avg <-
  list(eh_kfw_55_avg, eh_kfw_40_avg, eh_passivhaus_avg) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2023_2030_avg = (
      SpaceHeat_kfw_55_avg + SpaceHeat_kfw_40_avg + SpaceHeat_passivhaus_avg
    ) / 3,
    HotWater_2023_2030_avg = (
      HotWater_kfw_55_avg + HotWater_kfw_40_avg + HotWater_passivhaus_avg
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_55_avg",
      "SpaceHeat_kfw_40_avg",
      "SpaceHeat_passivhaus_avg",
      "HotWater_kfw_55_avg",
      "HotWater_kfw_40_avg",
      "HotWater_passivhaus_avg"
    )
  )

eh_2023_2030_cold <-
  list(eh_kfw_55_cold, eh_kfw_40_cold, eh_passivhaus_cold) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2023_2030_cold = (
      SpaceHeat_kfw_55_cold + SpaceHeat_kfw_40_cold + SpaceHeat_passivhaus_cold
    ) / 3,
    HotWater_2023_2030_cold = (
      HotWater_kfw_55_cold + HotWater_kfw_40_cold + HotWater_passivhaus_cold
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_55_cold",
      "SpaceHeat_kfw_40_cold",
      "SpaceHeat_passivhaus_cold",
      "HotWater_kfw_55_cold",
      "HotWater_kfw_40_cold",
      "HotWater_passivhaus_cold"
    )
  )

eh_2023_2030_hot <-
  list(eh_kfw_55_hot, eh_kfw_40_hot, eh_passivhaus_hot) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2023_2030_hot = (
      SpaceHeat_kfw_55_hot + SpaceHeat_kfw_40_hot + SpaceHeat_passivhaus_hot
    ) / 3,
    HotWater_2023_2030_hot = (
      HotWater_kfw_55_hot + HotWater_kfw_40_hot + HotWater_passivhaus_hot
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_55_hot",
      "SpaceHeat_kfw_40_hot",
      "SpaceHeat_passivhaus_hot",
      "HotWater_kfw_55_hot",
      "HotWater_kfw_40_hot",
      "HotWater_passivhaus_hot"
    )
  )


# Derive data multi family homes
# Derive data for 1949 - 1978
mh_1949_1978_avg <-
  list(mh_1949_1957_avg, mh_1958_1968_avg, mh_1969_1978_avg) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1949_1978_avg = (
      SpaceHeat_1949_1957_avg + SpaceHeat_1958_1968_avg + SpaceHeat_1969_1978_avg
    ) / 3,
    HotWater_1949_1978_avg = (
      HotWater_1949_1957_avg + HotWater_1958_1968_avg + HotWater_1969_1978_avg
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_1949_1957_avg",
      "HotWater_1949_1957_avg",
      "SpaceHeat_1958_1968_avg",
      "HotWater_1958_1968_avg",
      "SpaceHeat_1969_1978_avg",
      "HotWater_1969_1978_avg"
    )
  )

mh_1949_1978_cold <-
  list(mh_1949_1957_cold, mh_1958_1968_cold, mh_1969_1978_cold) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1949_1978_cold = (
      SpaceHeat_1949_1957_cold + SpaceHeat_1958_1968_cold + SpaceHeat_1969_1978_cold
    ) / 3,
    HotWater_1949_1978_cold = (
      HotWater_1949_1957_cold + HotWater_1958_1968_cold + HotWater_1969_1978_cold
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_1949_1957_cold",
      "HotWater_1949_1957_cold",
      "SpaceHeat_1958_1968_cold",
      "HotWater_1958_1968_cold",
      "SpaceHeat_1969_1978_cold",
      "HotWater_1969_1978_cold"
    )
  )

mh_1949_1978_hot <-
  list(mh_1949_1957_hot, mh_1958_1968_hot, mh_1969_1978_hot) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1949_1978_hot = (
      SpaceHeat_1949_1957_hot + SpaceHeat_1958_1968_hot + SpaceHeat_1969_1978_hot
    ) / 3,
    HotWater_1949_1978_hot = (
      HotWater_1949_1957_hot + HotWater_1958_1968_hot + HotWater_1969_1978_hot
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_1949_1957_hot",
      "HotWater_1949_1957_hot",
      "SpaceHeat_1958_1968_hot",
      "HotWater_1958_1968_hot",
      "SpaceHeat_1969_1978_hot",
      "HotWater_1969_1978_hot"
    )
  )


# Derive data for 1979 - 1986
mh_1979_1986_avg <-
  list(mh_1979_1983_avg, mh_1984_1994_avg) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1979_1986_avg = (SpaceHeat_1979_1983_avg * 5 / 8 + SpaceHeat_1984_1994_avg * 3 /
                                 8),
    HotWater_1979_1986_avg = (HotWater_1979_1983_avg * 5 / 8 + HotWater_1984_1994_avg * 3 /
                                8)
  ) %>%
  select(
    -c(
      "SpaceHeat_1979_1983_avg",
      "HotWater_1979_1983_avg",
      "SpaceHeat_1984_1994_avg",
      "HotWater_1984_1994_avg"
    )
  )

mh_1979_1986_cold <-
  list(mh_1979_1983_cold, mh_1984_1994_cold) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1979_1986_cold = (SpaceHeat_1979_1983_cold * 5 / 8 + SpaceHeat_1984_1994_cold * 3 /
                                  8),
    HotWater_1979_1986_cold = (HotWater_1979_1983_cold * 5 / 8 + HotWater_1984_1994_cold * 3 /
                                 8)
  ) %>%
  select(
    -c(
      "SpaceHeat_1979_1983_cold",
      "HotWater_1979_1983_cold",
      "SpaceHeat_1984_1994_cold",
      "HotWater_1984_1994_cold"
    )
  )

mh_1979_1986_hot <-
  list(mh_1979_1983_hot, mh_1984_1994_hot) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1979_1986_hot = (SpaceHeat_1979_1983_hot * 5 / 8 + SpaceHeat_1984_1994_hot * 3 /
                                 8),
    HotWater_1979_1986_hot = (HotWater_1979_1983_hot * 5 / 8 + HotWater_1984_1994_hot * 3 /
                                8)
  ) %>%
  select(
    -c(
      "SpaceHeat_1979_1983_hot",
      "HotWater_1979_1983_hot",
      "SpaceHeat_1984_1994_hot",
      "HotWater_1984_1994_hot"
    )
  )


# Derive data for 1991 - 1995
mh_1991_1995_avg <-
  list(mh_1984_1994_avg, mh_1995_2001_avg) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1991_1995_avg = (SpaceHeat_1984_1994_avg * 4 / 5 + SpaceHeat_1995_2001_avg * 1 /
                                 5),
    HotWater_1991_1995_avg = (HotWater_1984_1994_avg * 4 / 5 + HotWater_1995_2001_avg * 1 /
                                5)
  ) %>%
  select(
    -c(
      "SpaceHeat_1984_1994_avg",
      "HotWater_1984_1994_avg",
      "SpaceHeat_1995_2001_avg",
      "HotWater_1995_2001_avg"
    )
  )

mh_1991_1995_cold <-
  list(mh_1984_1994_cold, mh_1995_2001_cold) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1991_1995_cold = (SpaceHeat_1984_1994_cold * 4 / 5 + SpaceHeat_1995_2001_cold * 1 /
                                  5),
    HotWater_1991_1995_cold = (HotWater_1984_1994_cold * 4 / 5 + HotWater_1995_2001_cold * 1 /
                                 5)
  ) %>%
  select(
    -c(
      "SpaceHeat_1984_1994_cold",
      "HotWater_1984_1994_cold",
      "SpaceHeat_1995_2001_cold",
      "HotWater_1995_2001_cold"
    )
  )

mh_1991_1995_hot <-
  list(mh_1984_1994_hot, mh_1995_2001_hot) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_1991_1995_hot = (SpaceHeat_1984_1994_hot * 4 / 5 + SpaceHeat_1995_2001_hot * 1 /
                                 5),
    HotWater_1991_1995_hot = (HotWater_1984_1994_hot * 4 / 5 + HotWater_1995_2001_hot * 1 /
                                5)
  ) %>%
  select(
    -c(
      "SpaceHeat_1984_1994_hot",
      "HotWater_1984_1994_hot",
      "SpaceHeat_1995_2001_hot",
      "HotWater_1995_2001_hot"
    )
  )


# Derive data for 2012 - 2022
mh_2012_2022_avg <-
  list(mh_kfw_85_avg, mh_kfw_70_avg, mh_kfw_55_avg, mh_kfw_40_avg) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2012_2022_avg = (
      SpaceHeat_kfw_85_avg + SpaceHeat_kfw_70_avg + SpaceHeat_kfw_55_avg + SpaceHeat_kfw_40_avg
    ) / 4,
    HotWater_2012_2022_avg = (
      HotWater_kfw_85_avg + HotWater_kfw_70_avg + HotWater_kfw_55_avg + HotWater_kfw_40_avg
    ) / 4
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_85_avg",
      "SpaceHeat_kfw_70_avg",
      "SpaceHeat_kfw_55_avg",
      "SpaceHeat_kfw_40_avg",
      "HotWater_kfw_85_avg",
      "HotWater_kfw_70_avg",
      "HotWater_kfw_55_avg",
      "HotWater_kfw_40_avg"
    )
  )


mh_2012_2022_cold <-
  list(mh_kfw_85_cold,
       mh_kfw_70_cold,
       mh_kfw_55_cold,
       mh_kfw_40_cold) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2012_2022_cold = (
      SpaceHeat_kfw_85_cold + SpaceHeat_kfw_70_cold + SpaceHeat_kfw_55_cold + SpaceHeat_kfw_40_cold
    ) / 4,
    HotWater_2012_2022_cold = (
      HotWater_kfw_85_cold + HotWater_kfw_70_cold + HotWater_kfw_55_cold + HotWater_kfw_40_cold
    ) / 4
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_85_cold",
      "SpaceHeat_kfw_70_cold",
      "SpaceHeat_kfw_55_cold",
      "SpaceHeat_kfw_40_cold",
      "HotWater_kfw_85_cold",
      "HotWater_kfw_70_cold",
      "HotWater_kfw_55_cold",
      "HotWater_kfw_40_cold"
    )
  )

mh_2012_2022_hot <-
  list(mh_kfw_85_hot, mh_kfw_70_hot, mh_kfw_55_hot, mh_kfw_40_hot) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2012_2022_hot = (
      SpaceHeat_kfw_85_hot + SpaceHeat_kfw_70_hot + SpaceHeat_kfw_55_hot + SpaceHeat_kfw_40_hot
    ) / 4,
    HotWater_2012_2022_hot = (
      HotWater_kfw_85_hot + HotWater_kfw_70_hot + HotWater_kfw_55_hot + HotWater_kfw_40_hot
    ) / 4
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_85_hot",
      "SpaceHeat_kfw_70_hot",
      "SpaceHeat_kfw_55_hot",
      "SpaceHeat_kfw_40_hot",
      "HotWater_kfw_85_hot",
      "HotWater_kfw_70_hot",
      "HotWater_kfw_55_hot",
      "HotWater_kfw_40_hot"
    )
  )

# Derive data for 2023 - 2030
mh_2023_2030_avg <-
  list(mh_kfw_55_avg, mh_kfw_40_avg, mh_passivhaus_avg) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2023_2030_avg = (
      SpaceHeat_kfw_55_avg + SpaceHeat_kfw_40_avg + SpaceHeat_passivhaus_avg
    ) / 3,
    HotWater_2023_2030_avg = (
      HotWater_kfw_55_avg + HotWater_kfw_40_avg + HotWater_passivhaus_avg
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_55_avg",
      "SpaceHeat_kfw_40_avg",
      "SpaceHeat_passivhaus_avg",
      "HotWater_kfw_55_avg",
      "HotWater_kfw_40_avg",
      "HotWater_passivhaus_avg"
    )
  )

mh_2023_2030_cold <-
  list(mh_kfw_55_cold, mh_kfw_40_cold, mh_passivhaus_cold) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2023_2030_cold = (
      SpaceHeat_kfw_55_cold + SpaceHeat_kfw_40_cold + SpaceHeat_passivhaus_cold
    ) / 3,
    HotWater_2023_2030_cold = (
      HotWater_kfw_55_cold + HotWater_kfw_40_cold + HotWater_passivhaus_cold
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_55_cold",
      "SpaceHeat_kfw_40_cold",
      "SpaceHeat_passivhaus_cold",
      "HotWater_kfw_55_cold",
      "HotWater_kfw_40_cold",
      "HotWater_passivhaus_cold"
    )
  )

mh_2023_2030_hot <-
  list(mh_kfw_55_hot, mh_kfw_40_hot, mh_passivhaus_hot) %>%
  reduce(inner_join, by = "Time") %>%
  mutate(
    SpaceHeat_2023_2030_hot = (
      SpaceHeat_kfw_55_hot + SpaceHeat_kfw_40_hot + SpaceHeat_passivhaus_hot
    ) / 3,
    HotWater_2023_2030_hot = (
      HotWater_kfw_55_hot + HotWater_kfw_40_hot + HotWater_passivhaus_hot
    ) / 3
  ) %>%
  select(
    -c(
      "SpaceHeat_kfw_55_hot",
      "SpaceHeat_kfw_40_hot",
      "SpaceHeat_passivhaus_hot",
      "HotWater_kfw_55_hot",
      "HotWater_kfw_40_hot",
      "HotWater_passivhaus_hot"
    )
  )


# Combine data together
eh_combined_heat_demand_avg <-
  list(
    eh_beginn_1918_avg,
    eh_1919_1948_avg,
    eh_1949_1978_avg,
    eh_1979_1986_avg,
    eh_1984_1994_avg,
    eh_1991_1995_avg,
    eh_1995_2001_avg,
    eh_2002_ende_avg,
    eh_2012_2022_avg,
    eh_2023_2030_avg
  ) %>%
  reduce(inner_join, by = "Time") %>%
  rename(
    "SpaceHeat_1987_1990_avg" = "SpaceHeat_1984_1994_avg",
    "SpaceHeat_1996_2000_avg" = "SpaceHeat_1995_2001_avg",
    "SpaceHeat_2001_2011_avg" = "SpaceHeat_2002_ende_avg",
    "HotWater_avg" = "HotWater_2023_2030_avg",
  ) %>%
  select(
    -c(
      "HotWater_beginn_1918_avg",
      "HotWater_1919_1948_avg",
      "HotWater_1949_1978_avg",
      "HotWater_1979_1986_avg",
      "HotWater_1984_1994_avg",
      "HotWater_1991_1995_avg",
      "HotWater_1995_2001_avg",
      "HotWater_2002_ende_avg",
      "HotWater_2012_2022_avg"
    )
  ) %>%
  mutate(
    SpaceHeat_beginn_1918_avg = SpaceHeat_beginn_1918_avg / 10000,
    SpaceHeat_1919_1948_avg = SpaceHeat_1919_1948_avg / 10000,
    SpaceHeat_1949_1978_avg = SpaceHeat_1949_1978_avg / 10000,
    SpaceHeat_1979_1986_avg = SpaceHeat_1979_1986_avg / 10000,
    SpaceHeat_1987_1990_avg = SpaceHeat_1987_1990_avg / 10000,
    SpaceHeat_1991_1995_avg = SpaceHeat_1991_1995_avg / 10000,
    SpaceHeat_1996_2000_avg = SpaceHeat_1996_2000_avg / 10000,
    SpaceHeat_2001_2011_avg = SpaceHeat_2001_2011_avg / 10000,
    SpaceHeat_2012_2022_avg = SpaceHeat_2012_2022_avg / 10000,
    SpaceHeat_2023_2030_avg = SpaceHeat_2023_2030_avg / 10000,
    HotWater_avg = HotWater_avg / 10000
  )

eh_combined_heat_demand_cold <-
  list(
    eh_beginn_1918_cold,
    eh_1919_1948_cold,
    eh_1949_1978_cold,
    eh_1979_1986_cold,
    eh_1984_1994_cold,
    eh_1991_1995_cold,
    eh_1995_2001_cold,
    eh_2002_ende_cold,
    eh_2012_2022_cold,
    eh_2023_2030_cold
  ) %>%
  reduce(inner_join, by = "Time") %>%
  rename(
    "SpaceHeat_1987_1990_cold" = "SpaceHeat_1984_1994_cold",
    "SpaceHeat_1996_2000_cold" = "SpaceHeat_1995_2001_cold",
    "SpaceHeat_2001_2011_cold" = "SpaceHeat_2002_ende_cold",
    "HotWater_cold" = "HotWater_2023_2030_cold",
  ) %>%
  select(
    -c(
      "HotWater_beginn_1918_cold",
      "HotWater_1919_1948_cold",
      "HotWater_1949_1978_cold",
      "HotWater_1979_1986_cold",
      "HotWater_1984_1994_cold",
      "HotWater_1991_1995_cold",
      "HotWater_1995_2001_cold",
      "HotWater_2002_ende_cold",
      "HotWater_2012_2022_cold"
    )
  ) %>%
  mutate(
    SpaceHeat_beginn_1918_cold = SpaceHeat_beginn_1918_cold / 10000,
    SpaceHeat_1919_1948_cold = SpaceHeat_1919_1948_cold / 10000,
    SpaceHeat_1949_1978_cold = SpaceHeat_1949_1978_cold / 10000,
    SpaceHeat_1979_1986_cold = SpaceHeat_1979_1986_cold / 10000,
    SpaceHeat_1987_1990_cold = SpaceHeat_1987_1990_cold / 10000,
    SpaceHeat_1991_1995_cold = SpaceHeat_1991_1995_cold / 10000,
    SpaceHeat_1996_2000_cold = SpaceHeat_1996_2000_cold / 10000,
    SpaceHeat_2001_2011_cold = SpaceHeat_2001_2011_cold / 10000,
    SpaceHeat_2012_2022_cold = SpaceHeat_2012_2022_cold / 10000,
    SpaceHeat_2023_2030_cold = SpaceHeat_2023_2030_cold / 10000,
    HotWater_cold = HotWater_cold / 10000
  )

eh_combined_heat_demand_hot <-
  list(
    eh_beginn_1918_hot,
    eh_1919_1948_hot,
    eh_1949_1978_hot,
    eh_1979_1986_hot,
    eh_1984_1994_hot,
    eh_1991_1995_hot,
    eh_1995_2001_hot,
    eh_2002_ende_hot,
    eh_2012_2022_hot,
    eh_2023_2030_hot
  ) %>%
  reduce(inner_join, by = "Time") %>%
  rename(
    "SpaceHeat_1987_1990_hot" = "SpaceHeat_1984_1994_hot",
    "SpaceHeat_1996_2000_hot" = "SpaceHeat_1995_2001_hot",
    "SpaceHeat_2001_2011_hot" = "SpaceHeat_2002_ende_hot",
    "HotWater_hot" = "HotWater_2023_2030_hot",
  ) %>%
  select(
    -c(
      "HotWater_beginn_1918_hot",
      "HotWater_1919_1948_hot",
      "HotWater_1949_1978_hot",
      "HotWater_1979_1986_hot",
      "HotWater_1984_1994_hot",
      "HotWater_1991_1995_hot",
      "HotWater_1995_2001_hot",
      "HotWater_2002_ende_hot",
      "HotWater_2012_2022_hot"
    )
  ) %>%
  mutate(
    SpaceHeat_beginn_1918_hot = SpaceHeat_beginn_1918_hot / 10000,
    SpaceHeat_1919_1948_hot = SpaceHeat_1919_1948_hot / 10000,
    SpaceHeat_1949_1978_hot = SpaceHeat_1949_1978_hot / 10000,
    SpaceHeat_1979_1986_hot = SpaceHeat_1979_1986_hot / 10000,
    SpaceHeat_1987_1990_hot = SpaceHeat_1987_1990_hot / 10000,
    SpaceHeat_1991_1995_hot = SpaceHeat_1991_1995_hot / 10000,
    SpaceHeat_1996_2000_hot = SpaceHeat_1996_2000_hot / 10000,
    SpaceHeat_2001_2011_hot = SpaceHeat_2001_2011_hot / 10000,
    SpaceHeat_2012_2022_hot = SpaceHeat_2012_2022_hot / 10000,
    SpaceHeat_2023_2030_hot = SpaceHeat_2023_2030_hot / 10000,
    HotWater_hot = HotWater_hot / 10000
  )


mh_combined_heat_demand_avg <-
  list(
    mh_beginn_1918_avg,
    mh_1919_1948_avg,
    mh_1949_1978_avg,
    mh_1979_1986_avg,
    mh_1984_1994_avg,
    mh_1991_1995_avg,
    mh_1995_2001_avg,
    mh_2002_ende_avg,
    mh_2012_2022_avg,
    mh_2023_2030_avg
  ) %>%
  reduce(inner_join, by = "Time") %>%
  rename(
    "SpaceHeat_1987_1990_avg" = "SpaceHeat_1984_1994_avg",
    "SpaceHeat_1996_2000_avg" = "SpaceHeat_1995_2001_avg",
    "SpaceHeat_2001_2011_avg" = "SpaceHeat_2002_ende_avg",
    "HotWater_avg" = "HotWater_2023_2030_avg",
  ) %>%
  select(
    -c(
      "HotWater_beginn_1918_avg",
      "HotWater_1919_1948_avg",
      "HotWater_1949_1978_avg",
      "HotWater_1979_1986_avg",
      "HotWater_1984_1994_avg",
      "HotWater_1991_1995_avg",
      "HotWater_1995_2001_avg",
      "HotWater_2002_ende_avg",
      "HotWater_2012_2022_avg"
    )
  ) %>%
  mutate(
    SpaceHeat_beginn_1918_avg = SpaceHeat_beginn_1918_avg / 10000,
    SpaceHeat_1919_1948_avg = SpaceHeat_1919_1948_avg / 10000,
    SpaceHeat_1949_1978_avg = SpaceHeat_1949_1978_avg / 10000,
    SpaceHeat_1979_1986_avg = SpaceHeat_1979_1986_avg / 10000,
    SpaceHeat_1987_1990_avg = SpaceHeat_1987_1990_avg / 10000,
    SpaceHeat_1991_1995_avg = SpaceHeat_1991_1995_avg / 10000,
    SpaceHeat_1996_2000_avg = SpaceHeat_1996_2000_avg / 10000,
    SpaceHeat_2001_2011_avg = SpaceHeat_2001_2011_avg / 10000,
    SpaceHeat_2012_2022_avg = SpaceHeat_2012_2022_avg / 10000,
    SpaceHeat_2023_2030_avg = SpaceHeat_2023_2030_avg / 10000,
    HotWater_avg = HotWater_avg / 10000
  )

mh_combined_heat_demand_cold <-
  list(
    mh_beginn_1918_cold,
    mh_1919_1948_cold,
    mh_1949_1978_cold,
    mh_1979_1986_cold,
    mh_1984_1994_cold,
    mh_1991_1995_cold,
    mh_1995_2001_cold,
    mh_2002_ende_cold,
    mh_2012_2022_cold,
    mh_2023_2030_cold
  ) %>%
  reduce(inner_join, by = "Time") %>%
  rename(
    "SpaceHeat_1987_1990_cold" = "SpaceHeat_1984_1994_cold",
    "SpaceHeat_1996_2000_cold" = "SpaceHeat_1995_2001_cold",
    "SpaceHeat_2001_2011_cold" = "SpaceHeat_2002_ende_cold",
    "HotWater_cold" = "HotWater_2023_2030_cold",
  ) %>%
  select(
    -c(
      "HotWater_beginn_1918_cold",
      "HotWater_1919_1948_cold",
      "HotWater_1949_1978_cold",
      "HotWater_1979_1986_cold",
      "HotWater_1984_1994_cold",
      "HotWater_1991_1995_cold",
      "HotWater_1995_2001_cold",
      "HotWater_2002_ende_cold",
      "HotWater_2012_2022_cold"
    )
  ) %>%
  mutate(
    SpaceHeat_beginn_1918_cold = SpaceHeat_beginn_1918_cold / 10000,
    SpaceHeat_1919_1948_cold = SpaceHeat_1919_1948_cold / 10000,
    SpaceHeat_1949_1978_cold = SpaceHeat_1949_1978_cold / 10000,
    SpaceHeat_1979_1986_cold = SpaceHeat_1979_1986_cold / 10000,
    SpaceHeat_1987_1990_cold = SpaceHeat_1987_1990_cold / 10000,
    SpaceHeat_1991_1995_cold = SpaceHeat_1991_1995_cold / 10000,
    SpaceHeat_1996_2000_cold = SpaceHeat_1996_2000_cold / 10000,
    SpaceHeat_2001_2011_cold = SpaceHeat_2001_2011_cold / 10000,
    SpaceHeat_2012_2022_cold = SpaceHeat_2012_2022_cold / 10000,
    SpaceHeat_2023_2030_cold = SpaceHeat_2023_2030_cold / 10000,
    HotWater_cold = HotWater_cold / 10000
  )

mh_combined_heat_demand_hot <-
  list(
    mh_beginn_1918_hot,
    mh_1919_1948_hot,
    mh_1949_1978_hot,
    mh_1979_1986_hot,
    mh_1984_1994_hot,
    mh_1991_1995_hot,
    mh_1995_2001_hot,
    mh_2002_ende_hot,
    mh_2012_2022_hot,
    mh_2023_2030_hot
  ) %>%
  reduce(inner_join, by = "Time") %>%
  rename(
    "SpaceHeat_1987_1990_hot" = "SpaceHeat_1984_1994_hot",
    "SpaceHeat_1996_2000_hot" = "SpaceHeat_1995_2001_hot",
    "SpaceHeat_2001_2011_hot" = "SpaceHeat_2002_ende_hot",
    "HotWater_hot" = "HotWater_2023_2030_hot",
  ) %>%
  select(
    -c(
      "HotWater_beginn_1918_hot",
      "HotWater_1919_1948_hot",
      "HotWater_1949_1978_hot",
      "HotWater_1979_1986_hot",
      "HotWater_1984_1994_hot",
      "HotWater_1991_1995_hot",
      "HotWater_1995_2001_hot",
      "HotWater_2002_ende_hot",
      "HotWater_2012_2022_hot"
    )
  ) %>%
  mutate(
    SpaceHeat_beginn_1918_hot = SpaceHeat_beginn_1918_hot / 10000,
    SpaceHeat_1919_1948_hot = SpaceHeat_1919_1948_hot / 10000,
    SpaceHeat_1949_1978_hot = SpaceHeat_1949_1978_hot / 10000,
    SpaceHeat_1979_1986_hot = SpaceHeat_1979_1986_hot / 10000,
    SpaceHeat_1987_1990_hot = SpaceHeat_1987_1990_hot / 10000,
    SpaceHeat_1991_1995_hot = SpaceHeat_1991_1995_hot / 10000,
    SpaceHeat_1996_2000_hot = SpaceHeat_1996_2000_hot / 10000,
    SpaceHeat_2001_2011_hot = SpaceHeat_2001_2011_hot / 10000,
    SpaceHeat_2012_2022_hot = SpaceHeat_2012_2022_hot / 10000,
    SpaceHeat_2023_2030_hot = SpaceHeat_2023_2030_hot / 10000,
    HotWater_hot = HotWater_hot / 10000
  )


# Write output to csv
write_csv2(
  eh_combined_heat_demand_avg,
  "data/output/heatdemand/eh_combined_heat_demand_avg.csv"
)

write_csv2(
  eh_combined_heat_demand_cold,
  "data/output/heatdemand/eh_combined_heat_demand_cold.csv"
)

write_csv2(
  eh_combined_heat_demand_hot,
  "data/output/heatdemand/eh_combined_heat_demand_hot.csv"
)


write_csv2(
  mh_combined_heat_demand_avg,
  "data/output/heatdemand/mh_combined_heat_demand_avg.csv"
)

write_csv2(
  mh_combined_heat_demand_cold,
  "data/output/heatdemand/mh_combined_heat_demand_cold.csv"
)

write_csv2(
  mh_combined_heat_demand_hot,
  "data/output/heatdemand/mh_combined_heat_demand_hot.csv"
)
