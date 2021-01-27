library(tidyverse)

# read in model summaries
summary1 <- read_rds("model-runs/model-single-scenario-summary.rds")
summary2 <- read_rds("model-runs/model-interaction-scenario-summary.rds")

# single measure trajectory plot
names <- tibble(scenario = c("bl00", "mm01", "mm02", "mm05", "mm06", "mm07", "mm08"),
       measure_name = c("Baseline",
                        "Reduced tillage",
                        "Zero tillage",
                        "Residue retention, 50%",
                        "Cover cropping, no legumes, 2 spp.",
                        "Cover cropping, legumes, 2 spp.",
                        "Companion cropping with clover")
)

summary1 %>%
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
  
  co2_seq <- co2_seq[2:length(summary)] %>%
    map(
      ~lm(total_y ~ year, data = .x)
    ) %>%
    map_dfc(~predict(.x, newdata = tibble(year = start_year + dur))) %>%
    gather(key = "scenario", value = {{ newcol1 }}) %>%
    mutate({{ newcol2 }} := (.data[[newcol1]] - c_stocks_start) / dur) %>%
    select(-{{ newcol1 }})
  
  return(co2_seq)
}

# output csv for single measures
output1 <- names %>%
  filter(scenario != "bl00") %>%
  left_join(co2_seq(summary1, start_year = 2015, dur = 20), by = "scenario") %>%
  left_join(co2_seq(summary1, start_year = 2015, dur = 35), by = "scenario") %>%
  left_join(co2_seq(summary1, start_year = 2015, dur = 82), by = "scenario")

write_csv(output1, "model-runs/co2-sequestration-summary-single.csv")

# output csv for single measures and interactions
output2 <- co2_seq(append(summary1, summary2), start_year = 2015, dur = 20) %>%
  left_join(co2_seq(append(summary1, summary2), start_year = 2015, dur = 35), by = "scenario") %>%
  left_join(co2_seq(append(summary1, summary2), start_year = 2015, dur = 82), by = "scenario")

write_csv(output2, "model-runs/co2-sequestration-summary-single-plus-interactions.csv")

# calculate IFs
interactions <-
  co2_seq(append(summary1[1], summary2), start_year = 2015, dur = 20) %>%
  rename(co2_seq_combd = annual_co2_seq_tha_20year) %>%
  mutate(x = str_sub(scenario, 1, 4),
         y = str_sub(scenario, -4, -1)) %>%
  left_join(co2_seq(summary1, 2015, 20) %>% rename(x = scenario, co2_seq_x = annual_co2_seq_tha_20year), by = "x") %>%
  left_join(co2_seq(summary1, 2015, 20) %>% rename(y = scenario, co2_seq_y = annual_co2_seq_tha_20year), by = "y") %>%
  mutate(co2_seq_sum = co2_seq_x + co2_seq_y,
         int_fac = co2_seq_combd / co2_seq_sum) %>%
  select(-x, -y)

write_csv(interactions, "model-runs/interaction-factor-summary.csv")
