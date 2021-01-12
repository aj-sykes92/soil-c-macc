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
Dat_baseline <- build_baseline_scenario(Dat_nest)
write_rds(Dat_baseline, paste0(out, "baseline-scenario.rds"))

# run reduced tillage scenario
Dat_m01 <- build_tillage_scenario(Dat_nest, till_rotation = "r", change_year = 2015)
write_rds(Dat_m01, paste0(out, "m01-consistent-reduced-tillage-scenario.rds"))

# run zero tillage scenario
Dat_m02 <- build_tillage_scenario(Dat_nest, till_rotation = "z", change_year = 2015)
write_rds(Dat_m02, paste0(out, "m02-consistent-zero-tillage-scenario.rds"))

# run mixed-reduced tillage scenario
Dat_m03 <- build_tillage_scenario(Dat_nest, till_rotation = "rrrf", change_year = 2015)
write_rds(Dat_m03, paste0(out, "m03-mixed-reduced-tillage-scenario.rds"))

# run mixed-zero tillage scenario
Dat_m04 <- build_tillage_scenario(Dat_nest, till_rotation = "zzf", change_year = 2015)
write_rds(Dat_m04, paste0(out, "m04-mixed-zero-tillage-scenario.rds"))

# build residue retention scenario
Dat_m05 <- build_residue_scenario(Dat_nest, frac_remove_new = 0, change_year = 2015)
write_rds(Dat_m05, paste0(out, "m05-residue-retention-scenario.rds"))

# run increased cover crop uptake scenarios
Dat_m06 <- build_cc_scenario(Dat_nest, cc_type = "leg2", change_year = 2015)
write_rds(Dat_m06, paste0(out, "m06-cc-leg2-scenario.rds"))

Dat_m07 <- build_cc_scenario(Dat_nest, cc_type = "leg4", change_year = 2015)
write_rds(Dat_m07, paste0(out, "m07-cc-leg4-scenario.rds"))

Dat_m08 <- build_cc_scenario(Dat_nest, cc_type = "nleg2", change_year = 2015)
write_rds(Dat_m08, paste0(out, "m08-cc-nleg2-scenario.rds"))

Dat_m09 <- build_cc_scenario(Dat_nest, cc_type = "nleg4", change_year = 2015)
write_rds(Dat_m09, paste0(out, "m09-cc-nleg4-scenario.rds"))

# run clover companion cropping scenario
Dat_m10 <- build_clover_scenario(Dat_nest, yield_tha = 1 / 0.9, change_year = 2015)
write_rds(Dat_m10, paste0(out, "m10-clover-companion-scenario.rds"))

# summarise scenarios into list
summary <- list(
  bl = Dat_baseline,
  m01 = Dat_m01,
  m02 = Dat_m02,
  m03 = Dat_m03,
  m04 = Dat_m04,
  m05 = Dat_m05,
  m06 = Dat_m06,
  m07 = Dat_m07,
  m08 = Dat_m08,
  m09 = Dat_m09,
  m10 = Dat_m10
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
write_rds(summary, paste0(out, "model-scenario-summary.rds"))
write_rds(summary, "model-runs/model-scenario-summary.rds")
