# Load required packages
library(tidyverse)
library("dplyr")

# Read the data
cop_comparison_ashp_reference <- read_csv(
  "data/output/findings/comparison-when2heat/cop_comparison_ashp_reference.csv"
)


# Transform data
cop_comparison_ashp_reference_transformed <- cop_comparison_ashp_reference %>%
  rename(
    "COP calculated" = "WeightedCOP",
    "COP When2Heat" = "AVGCOPWhen2Heat"
  ) %>%
  select(
    c(
      "date_iso",
      "COP calculated",
      "COP When2Heat"
    )
  ) %>%
  gather(
    "Type",
    "COP",
    2:3
  )  %>%
  mutate_if(is.character, as.factor)


summary(cop_comparison_ashp_reference_transformed)


# Plot the comparison graph
cop_comparison_plot <-
  ggplot(data=cop_comparison_ashp_reference_transformed, aes(date_iso, COP, color=Type)) +
  geom_line(lwd=1.0) + ylab("COP") + xlab("Date") + coord_cartesian(ylim = c(-5, 20)) +
  scale_color_brewer(palette = "Set2") +
  guides(color = guide_legend(title = "Type of COP"))

cop_comparison_plot


# Save the plot
ggsave(
  "plots/output/comparison-when2heat/cop_comparison_plot.png",
  cop_comparison_plot,
  width = 30,
  units = "cm"
)
