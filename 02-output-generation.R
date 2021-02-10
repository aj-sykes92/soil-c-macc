library(tidyverse)

# read in model summaries
summary <- read_rds("model-runs/model-scenario-summary.rds")
scenarios <- readxl::read_xlsx(
  "combined-measure-args.xlsx",
  sheet = "measure-args-combd",
  na = c("NA", ""),
  col_types = c("numeric", "text", "text", "text", "text", "text",
                "text", "numeric", "text", "numeric")
  ) %>%
  mutate(scenario = paste(measure_a, measure_b, measure_c, measure_d, sep = "_"))

summary %>%
  bind_rows(.id = "scenario") %>%
  left_join(select(scenarios, scenario, description, id), by = "scenario") %>%
  mutate(description = str_replace_all(description, "/.+$", "")) %>%
  filter(id <= 8) %>% # single measures only
  ggplot(aes(x = year, y = total_y, group = description, colour = description)) +
  geom_line() +
  geom_vline(xintercept = 2015) +
  geom_vline(xintercept = 2035, lty = 2, alpha = 0.5) +
  geom_vline(xintercept = 2050, lty = 2, alpha = 0.5) +
  geom_vline(xintercept = 2100, lty = 2, alpha = 0.5) +
  scale_colour_brewer(palette = "Set3") +
  labs(x = "Year",
       y = expression("C stocks, tonnes ha"^{-1}),
       colour = "") +
  theme_classic() +
  theme(legend.position = "bottom")
ggsave(project_data(path = "project-data/output-plots/trajectory-plot.png"), height = 7, width = 10)

summary %>% 
  bind_rows(.id = "scenario") %>%
  left_join(select(scenarios, scenario, description, id), by = "scenario") %>%
  ggplot(aes(x = year, y = total_y, group = id)) +
  geom_line(colour = "darkred", alpha = 0.3) +
  labs(x = "Year",
       y = expression("C stocks, tonnes ha"^{-1}),
       colour = "") +
  theme(legend.position = "bottom") +
  theme_classic()

# sequestration rate function
# summary = list of model runs
# seq_dur = duration over which to calculate sequestration, years
# start_year = year measure implemented
# lm_dur = duration over which to fit linear model, leave at 10 unless good reason to change
# bl_index = index of baseline run in summary
c_seq <- function(summary, seq_dur, start_year = 2015, lm_dur = 10, bl_index = 1) {
  c_seq <- summary %>%
    map(
      ~.x %>%
        filter(year >= (start_year + seq_dur) - lm_dur,
               year < start_year + seq_dur)
    )
  
  newcol1 <- paste0("c_stocks_", seq_dur, "year")
  newcol2 <- paste0("annual_c_seq_tha_", seq_dur, "year")
  
  c_seq <- c_seq %>%
    map(
      ~lm(total_y ~ year, data = .x)
    ) %>%
    map_dfc(~predict(.x, newdata = tibble(year = start_year + seq_dur))) %>%
    gather(key = "scenario", value = {{ newcol1 }}) %>%
    mutate({{ newcol2 }} := (.data[[newcol1]] - .data[[newcol1]][bl_index]) / seq_dur)
  
  return(c_seq)
}

# output csv for single measures
output1 <- scenarios %>%
  select(id:description, scenario) %>%
  left_join(c_seq(summary, seq_dur = 20), by = "scenario") %>%
  left_join(c_seq(summary, seq_dur = 35), by = "scenario") %>%
  left_join(c_seq(summary, seq_dur = 82), by = "scenario") %>%
  select(-scenario)

write_csv(output1, "model-runs/c-sequestration-summary.csv")
write_csv(output1, project_data(path = "project-data/model-scenarios/sequestration-summary.csv"))

# calculate IFs
indi_seq <- output1 %>%
  slice(2:9) %>%
  pull(annual_c_seq_tha_20year) %>%
  set_names(output1$measure_a[2:9])

interactions <-
  c_seq(summary, seq_dur = 20) %>%
  rename(c_seq_combd = annual_c_seq_tha_20year) %>%
  slice(10:n()) %>% # remove baseline and individual measures
  mutate(scenario_list =
           scenario %>%
           str_split("_"),
         scenario_list = 
           scenario_list %>%
           map(~discard(.x, . == "NA")),
         c_seq_indi = map(scenario_list, ~indi_seq[.x]),
         c_seq_cum =  map_dbl(c_seq_indi, ~reduce(.x, `+`)),
         int_fac = c_seq_combd / c_seq_cum
  )

write_rds(interactions, "model-runs/interaction-factor-summary.rds")
write_csv(select(interactions, -scenario_list, -c_seq_indi), "model-runs/interaction-factor-summary.csv")
write_rds(interactions, project_data(path = "project-data/model-scenarios/interactions-summary.rds"))
write_csv(select(interactions, -scenario_list, -c_seq_indi), project_data(path = "project-data/model-scenarios/interactions-summary.csv"))
