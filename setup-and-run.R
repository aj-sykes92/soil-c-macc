
library(tidyverse)
# library(dplyr) dplyr is automatically loaded w/ tidyverse!

# read in data
Dat_nest <- read_rds(project_data(path = "project-data/model-data-input-small-sample-wheat-manure-tillage-data.rds"))

# source functions!
source("ipcc-c-model-functions.R")

# sample down for ease of use
set.seed(2605)
Dat_nest <- Dat_nest %>% sample_n(50)

# build and run model
Dat_nest <- build_model(Dat_nest)

# plot
ts_plot(Dat_nest)
