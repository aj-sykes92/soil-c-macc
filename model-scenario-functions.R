library(tidyverse)
library(soilc.ipcc)

# model build functions
source("model-build-functions.R")

################################
# build model baseline scenario
################################
build_baseline_scenario <- function(Dat_nest) {
  Dat_nest %>%
    build_inputs() %>%
    combine_inputs() %>%
    build_model()
}

################################
# build tillage scenarios
################################

# modification function
tillage_mod_fun <- function(data, till_rotation, change_year) {
  
  # length of modified rotation
  new_vec_length <- data %>% filter(year >= change_year) %>% nrow()
  
  data %>% filter(year >= change_year) %>% pull(year) %>% min()
  
  # parse till rotation into usable vector
  till_rotation <- as.vector(str_split_fixed(till_rotation, "", str_length(till_rotation)))
  till_rotation <- case_when(
    till_rotation == "f" ~ "full",
    till_rotation == "r" ~ "reduced",
    till_rotation == "z" ~ "zero",
    TRUE ~ till_rotation
  )
  
  rep_no <- ceiling(new_vec_length / length(till_rotation))
  till_rotation <- rep(till_rotation, rep_no)[1:new_vec_length]
  data$till_type[(1 + nrow(data) - new_vec_length):nrow(data)] <- till_rotation
    
  return(data)
}

# main tillage scenario function
build_tillage_scenario <- function(Dat_nest, till_rotation, change_year = 2015) {

  Dat_nest %>%
    mutate(data = map(
      data,
      ~tillage_mod_fun(.x, till_rotation = till_rotation, change_year = change_year))
      ) %>%
    build_inputs() %>%
    combine_inputs() %>%
    build_model()
}

################################
# build residue retention scenarios
################################
build_residue_scenario <- function(Dat_nest, frac_remove_new, change_year = 2015) {

  Dat_nest %>%
    mutate(data = map(
      data,
      # modification of frac_remove
      ~mutate(.x, frac_remove = ifelse(year >= change_year, frac_remove_new, frac_remove)))
    ) %>%
    build_inputs() %>%
    combine_inputs() %>%
    build_model()
}

################################
# build cover crop scenarios
################################
# cc_type = c("leg2, leg4, nleg2, nleg4")
build_cc_scenario <- function(Dat_nest, cc_type, change_year = 2015) {
  
  # cc_data
  cc_data <- read_rds("parameter-data/cc-list.rds")[[cc_type]]
  
  # change year
  n_change <- which(Dat_nest$data[[1]]$year == change_year)
  
  Dat_nest <- Dat_nest %>%
    mutate(cc_input = map(cc_input, function(x) {
      x$om_input[n_change:length(x$om_input)] <- cc_data$om_input
      x$c_input[n_change:length(x$c_input)] <- cc_data$c_input
      x$n_input[n_change:length(x$n_input)] <- cc_data$n_input
      x$lignin_input[n_change:length(x$lignin_input)] <- cc_data$lignin_input
      return(x)
    })) %>%
    build_inputs() %>%
    combine_inputs() %>%
    build_model()
  
  return(Dat_nest)
}

################################
# build clover companion cropping scenario
################################
build_clover_scenario <- function(Dat_nest, yield_tha, change_year = 2015) {

  # change year
  n_change <- which(Dat_nest$data[[1]]$year == change_year)
  pre <- length(1:(n_change - 1))
  post <- length(n_change:nrow(Dat_nest$data[[1]]))
  
  # adding clover as n_fixing_forage
  Dat_nest <- Dat_nest %>%
    build_inputs() %>%
    
    # add in clover input
    mutate(
      clover_input = map(1:nrow(Dat_nest),
                         ~add_crop("n_fixing_forage",
                                   c(rep(0, pre), rep(yield_tha, post)),
                                   0,
                                   1)
      )
    ) %>%
    
    # custom combine inputs code
    mutate(
      soil_input = pmap(
        list(crop_input, man_input, cc_input, clover_input),
        function(x, y, z, clover) {
          if (y$livestock_type == "none") {
            build_soil_input(x, z, clover)
          } else {
            build_soil_input(x, y, z, clover)
          }
        })
    ) %>%
    # custom code ends
    
    build_model()
  
  return(Dat_nest)
}

################################
# extract outputs
################################

