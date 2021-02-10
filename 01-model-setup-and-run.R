library(tidyverse)
library(soilc.ipcc)

# read in data
Dat_nest <- read_rds(project_data(path = "project-data/model-data-input-small-sample-wheat-manure-tillage-cc-data.rds"))

# sample down for ease of use
set.seed(2605)
Dat_nest <- Dat_nest %>% sample_n(100) 

# read in scenario setup csv
scenarios <- readxl::read_xlsx(
  "combined-measure-args.xlsx",
  sheet = "measure-args-combd",
  na = c("NA", ""),
  col_types = c("numeric", "text", "text", "text", "text", "text",
                "text", "numeric", "text", "numeric")
) # loadsa warnings but all good

# convert to arg list
scenarios <- scenarios %>%
  mutate(scenario_ref = paste(measure_a, measure_b, measure_c, measure_d, sep = "_"),
         args = pmap(list(till_rotation, frac_remove_new, cc_type, comp_yield_tha), list),
         args = map(args, ~set_names(.x, c("till_rotation", "frac_remove_new", "cc_type", "comp_yield_tha"))),
         args = map(args, ~.x %>% discard(is.na)),
         args = map(args, ~append(list(Dat_nest = Dat_nest), .x)),
         args = set_names(args, scenario_ref))
# do not View(scenarios) -- Dat_nest is replicated in args list

# source scenario functions
source("model-scenario-functions.R")

# write out path
out <- project_data(path = "project-data/model-scenarios/")

# run scenarios
scenario_list <- map(scenarios$args, ~do.call(build_scenario, .x))
write_rds(scenario_list, paste0(out, "combined-measure-scenarios.rds"))

# build summary and write out
summary <- scenario_list %>% 
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

write_rds(summary, paste0(out, "model-scenario-summary.rds"))
write_rds(summary, "model-runs/model-scenario-summary.rds")
