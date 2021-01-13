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

# sequestration rate function
co2_seq <- function(summary, start_year, dur) {
  co2_seq <- summary %>%
    map(
      ~.x %>%
        filter(start_year >= 2015,
               start_year < 2015 + dur)
    )
  
  c_stocks_start <- co2_seq[[1]]$total_y[1]
  
  newcol1 <- paste0("c_stocks_", dur, "year")
  newcol2 <- paste0("annual_co2_seq_tha_", dur, "year")
  
  co2_seq <- co2_seq[2:11] %>%
    map(
      ~lm(total_y ~ year, data = .x)
    ) %>%
    map_dfc(~predict(.x, newdata = tibble(year = start_year + dur))) %>%
    gather(key = "scenario", value = {{ newcol1 }}) %>%
    mutate({{ newcol2 }} := (.data[[newcol1]] - c_stocks_start) / dur) %>%
    select(-{{ newcol1 }})
  
  return(co2_seq)
}

# output csv
output <- names %>%
  filter(scenario != "bl") %>%
  left_join(co2_seq(summary, start_year = 2015, dur = 20), by = "scenario") %>%
  left_join(co2_seq(summary, start_year = 2015, dur = 50), by = "scenario")

write_csv(output, "model-runs/co2-sequestration-summary.csv")
