library(tidyverse)
library(soilc.ipcc)

# read in data
Dat_nest <- read_rds(project_data(path = "project-data/model-data-input-small-sample-wheat-manure-tillage-cc-data.rds"))

# source scenario functions
source("model-scenario-functions.R")

# sample down for ease of use
set.seed(2605)
Dat_nest <- Dat_nest %>% sample_n(100) 

# write out path
out <- project_data(path = "project-data/model-scenarios/")

# build and run model baseline
Dat_bl00 <- build_scenario(Dat_nest)
write_rds(Dat_bl00, paste0(out, "bl00.rds"))

# run reduced tillage scenario
Dat_mm01 <- build_scenario(Dat_nest, till_rotation = "r", change_year = 2015)
write_rds(Dat_mm01, paste0(out, "mm01.rds"))

# run zero tillage scenario
Dat_mm02 <- build_scenario(Dat_nest, till_rotation = "z", change_year = 2015)
write_rds(Dat_mm02, paste0(out, "mm02.rds"))

# build residue retention scenario
Dat_mm05 <- build_scenario(Dat_nest, frac_remove_new = 0.5, change_year = 2015)
write_rds(Dat_mm05, paste0(out, "mm05.rds"))

# run increased cover crop uptake scenarios
Dat_mm06 <- build_scenario(Dat_nest, cc_type = "nleg2", change_year = 2015)
write_rds(Dat_mm06, paste0(out, "mm06.rds"))

Dat_mm07 <- build_scenario(Dat_nest, cc_type = "leg2", change_year = 2015)
write_rds(Dat_mm07, paste0(out, "mm07.rds"))

# run clover companion cropping scenario
Dat_mm08 <- build_scenario(Dat_nest, comp_yield_tha = 1 / 0.9, change_year = 2015)
write_rds(Dat_mm08, paste0(out, "mm08.rds"))

# summarise scenarios into list
summary <- list(
  bl00 = Dat_bl00,
  mm01 = Dat_mm01,
  mm02 = Dat_mm02,
  mm05 = Dat_mm05,
  mm06 = Dat_mm06,
  mm07 = Dat_mm07,
  mm08 = Dat_mm08
) %>% 
  map(
    ~.x %>%
      unnest(model_output) %>%
      group_by(year) %>%
      summarise(om_input = mean(om_input),
                c_input = mean(c_input),
                n_input = mean(n_input),
                lignin_input = mean(lignin_input),
                active_y = mean(active_y),
                slow_y = mean(slow_y),
                passive_y = mean(passive_y),
                total_y = mean(total_y),
                .groups = "drop")
    )

# write out summary
write_rds(summary, paste0(out, "model-single-scenario-summary.rds"))
write_rds(summary, "model-runs/model-single-scenario-summary.rds")
