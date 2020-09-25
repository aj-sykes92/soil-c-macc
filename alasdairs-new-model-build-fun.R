
library(tidyverse)

Dat_nest <- read_rds(project_data(path = "project-data/model-data-input-small-sample-wheat-manure-data.rds"))


##########################
# build and run model for bush estate farm
build_model <- function(Dat_nest) {

  #####################################################
  # calculate crop-specific variables in the crop dataset
  #####################################################
  
  # C inputs from crop residues and manure
  
  # REFACTOR -- WE NEED TO USE THESE FUNCTIONS DIRECTLY ON THE DATA IN Dat_nest
  
  # Dat_crop & Dat_manure were both used in app version -- here everything is tied up in Dat_nest already.
  
  Dat_crop <- Dat_crop %>% # issue here -- Dat_crop not used
    mutate(C_res = pmap_dbl(list(yield_tha, crop_type, frac_renew, frac_remove), # can we find these vars in Dat_nest?
                            C_in_residues)) %>% # CHECK HOW THIS WORKS IN ipcc-model-functions.R
    left_join(Dat_manure, by = "year") %>% # issue here -- Dat_manure not used
    mutate(C_man = pmap_dbl(list(man_nrate, man_type), # can we find these vars in Dat_nest?
                            C_in_manure), # CHECK HOW THIS WORKS IN ipcc-model-functions.R
           N_frac = pmap_dbl(list(crop_type, man_type, C_res, C_man), # can we find these vars in Dat_nest?
                             N_frac), # CHECK HOW THIS WORKS IN ipcc-model-functions.R
           lignin_frac = pmap_dbl(list(crop_type, man_type, C_res, C_man), # can we find these vars in Dat_nest?
                                  lignin_frac), # CHECK HOW THIS WORKS IN ipcc-model-functions.R
           C_tot = C_res + C_man)
  
  #####################################################
  # run in model for 20 year period and add tillage type
  # can't run in with tillage already added, since it can't be averaged
  #####################################################
  runin_years <- 20
  
  Dat_nest <- Dat_nest %>%
    mutate(data_runin = data_full %>%
             map2(runin_years, run_in) %>% # CHECK HOW THIS WORKS IN ipcc-model-functions.R
             map(function(df){
               df %>%
                 left_join(Dat_crop %>% select(year, till_type), by = "year") %>%
                 mutate(till_type = ifelse(is.na(till_type), "full", till_type)) # CHECK THIS OUT -- ADD EARLIER IF POSSIBLE
             }))
  
  #####################################################
  # run model
  #####################################################
  Dat_nest <- Dat_nest %>%
    mutate(scenario_baseline = data_runin %>%
             map(run_model))
  
  return(Dat_nest)
}