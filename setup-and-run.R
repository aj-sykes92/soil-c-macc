
library(tidyverse)
library(dplyr)

# read in data
Dat_nest <- read_rds(project_data(path = "project-data/model-data-input-small-sample-wheat-manure-data.rds"))

# source functions!
source("ipcc-c-model-function.R")

# sample down for ease of use
set.seed(2605)
Dat_nest <- Dat_nest %>% sample_n(50)

# add in dummy assumptions
Dat_nest <- Dat_nest %>%
  mutate(data =  map(data, ~.x %>% mutate(frac_renew = 1,
                                          frac_remove = 0.7,
                                          till_type = "full")))

# build and run model
Dat_nest <- build_model(Dat_nest)

# plot
ts_plot(Dat_nest)
