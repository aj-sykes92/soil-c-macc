library(tidyverse)

# read in data
Dat_nest <- read_rds(project_data(path = "project-data/model-data-input-small-sample-wheat-manure-tillage-data.rds"))

# source functions!
source("ipcc-c-model-functions.R")
source("model-scenario-functions.R")
# source scenario function script

# sample down for ease of use
set.seed(2605)
Dat_nest <- Dat_nest %>% sample_n(50) 

# build and run model baseline
Dat_baseline <- build_baseline_scenario(Dat_nest)

# run reduced tillage scenario
Dat_m1 <- build_tillage_scenario(Dat_nest, type = "reduced", uptake = 1, change_year = 2015)

# run biennial tillage scenario
Dat_m2 <- build_tillage_scenario(Dat_nest, type = "biennial", uptake = 1, change_year = 2015)

# run biennial tillage scenario
Dat_m3 <- build_tillage_scenario(Dat_nest, type = "zero", uptake = 1, change_year = 2015)

# run increased cover crop uptake scenario
Dat_m4 <- build_cc_scenario(Dat_nest,
                            cc_probs_pre = "cover-crop-tillage-proportion",
                            cc_probs_post = "cover-crop-tillage-proportion-full-uptake",
                            legume = NULL,
                            combi_min = 2,
                            combi_max = 4,
                            change_year = 2015)

Dat_m5 <- build_cc_scenario(Dat_nest,
                            cc_probs_pre = "cover-crop-tillage-proportion",
                            cc_probs_post = "cover-crop-tillage-proportion-full-uptake",
                            legume = TRUE,
                            combi_min = 2,
                            combi_max = 2,
                            change_year = 2015)

Dat_m6 <- build_cc_scenario(Dat_nest,
                            cc_probs_pre = "cover-crop-tillage-proportion",
                            cc_probs_post = "cover-crop-tillage-proportion-full-uptake",
                            legume = FALSE,
                            combi_min = 2,
                            combi_max = 2,
                            change_year = 2015)

Dat_m7 <- build_cc_scenario(Dat_nest,
                            cc_probs_pre = "cover-crop-tillage-proportion",
                            cc_probs_post = "cover-crop-tillage-proportion-full-uptake",
                            legume = FALSE,
                            combi_min = 2,
                            combi_max = 4,
                            change_year = 2015)
