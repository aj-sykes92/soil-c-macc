library(tidyverse)

# read in model summary
summary <- read_rds("model-runs/model-scenario-summary.rds")

# trajectory plot
names <- tibble(scenario = c("bl", "m01", "m02", "m03", "m04", "m05", "m06", "m07", "m08", "m09", "m10"),
       measure_name = c("Baseline",
                        "Reduced tillage",
                        "Zero tillage",
                        "Intermittent reduced tillage",
                        "Intermittent zero tillage",
                        "Residue retention",
                        "Cover cropping, legumes, 2 spp.",
                        "Cover cropping, legumes, 4 spp.",
                        "Cover cropping, no legumes, 2 spp.",
                        "Cover cropping, no legumes, 4 spp.",
                        "Companion cropping with clover")
)

summary %>%
  bind_rows(.id = "scenario") %>%
  left_join(names, by = "scenario") %>%
  ggplot(aes(x = year, y = total_y, group = measure_name, colour = measure_name)) +
  geom_line() +
  geom_vline(xintercept = 2015) +
  scale_colour_brewer(palette = "Set3") +
  labs(x = "Year",
       y = expression("C stocks, tonnes ha"^{-1}),
       colour = "") +
  theme(legend.position = "bottom") +
  theme_classic()
ggsave(project_data(path = "project-data/output-plots/trajectory-plot.png"), height = 7, width = 10)
