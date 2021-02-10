library(tidyverse)
library(soilc.ipcc)

# model build functions
source("model-build-functions.R")

################################
# modify tillage practices
################################

# modification function
tillage_mod_fun <- function(data, till_rotation, change_year) {
  
  # length of modified rotation
  new_vec_length <- data %>% filter(year >= change_year) %>% nrow()
  
  # parse till rotation into usable vector
  till_rotation <- as.vector(str_split_fixed(till_rotation, "", str_length(till_rotation)))
  till_rotation <- case_when(
    till_rotation == "f" ~ "full",
    till_rotation == "r" ~ "reduced",
    till_rotation == "z" ~ "zero",
    TRUE ~ till_rotation
  )
  till_rotation <- rep(till_rotation, length.out = new_vec_length)
  
  # work out yield effects
  yield_effect <- case_when(
    till_rotation == "full" ~ 1,
    till_rotation == "reduced" ~ 1.07,
    till_rotation == "zero" ~ 1.06,
    TRUE ~ 1
  )
  
  # add to data
  data$till_type[(1 + nrow(data) - new_vec_length):nrow(data)] <- till_rotation
  data$yield_tha[(1 + nrow(data) - new_vec_length):nrow(data)] <- data$yield_tha[(1 + nrow(data) - new_vec_length):nrow(data)] * yield_effect
    
  return(data)
}

# main tillage scenario function
modify_tillage <- function(Dat_nest, till_rotation, change_year = 2015) {

  Dat_nest %>%
    mutate(data = map(
      data,
      ~tillage_mod_fun(.x, till_rotation = till_rotation, change_year = change_year))
      )
  
}

################################
# modify residue management
################################
modify_residues <- function(Dat_nest, frac_remove_new, change_year = 2015) {

  Dat_nest %>%
    mutate(data = map(
      data,
      # modification of frac_remove
      ~mutate(.x, frac_remove = ifelse(year >= change_year, frac_remove_new, frac_remove)))
    )
  
}

################################
# modify cover crop practices
################################
# cc_type = c("leg2, leg4, nleg2, nleg4")
modify_cc <- function(Dat_nest, cc_type, change_year = 2015) {
  
  # cc_data
  cc_data <- read_rds("parameter-data/cc-list.rds")[[cc_type]]
  
  # change year
  n_change <- which(Dat_nest$data[[1]]$year == change_year)
  
  # yield effects
  yield_effects <- case_when(
    cc_type == "leg2" ~ 1.12,
    cc_type == "leg4" ~ 1.12,
    cc_type == "nleg2" ~ 1.04,
    cc_type == "nleg4" ~ 1.04,
    TRUE ~ 1
  )
  yield_effects <- c(rep(1, n_change - 1), rep(yield_effects, nrow(Dat_nest$data[[1]]) - (n_change - 1)))
    
  Dat_nest <- Dat_nest %>%
    mutate(
      cc_input = map(cc_input, function(x) {
        x$om_input[n_change:length(x$om_input)] <- cc_data$om_input
        x$c_input[n_change:length(x$c_input)] <- cc_data$c_input
        x$n_input[n_change:length(x$n_input)] <- cc_data$n_input
        x$lignin_input[n_change:length(x$lignin_input)] <- cc_data$lignin_input
        return(x)
      }),
      data = map(data, ~mutate(.x, yield_tha = yield_tha * yield_effects))
    )
  
  return(Dat_nest)
}

################################
# modify companion cropping practice (clover)
################################
modify_comp <- function(Dat_nest, comp_yield_tha, change_year = 2015) {

  # change year
  n_change <- which(Dat_nest$data[[1]]$year == change_year)
  pre <- length(1:(n_change - 1))
  post <- length(n_change:nrow(Dat_nest$data[[1]]))
  
  # adding clover as n_fixing_forage
  Dat_nest <- Dat_nest %>%
    mutate(
      clover_input = map(1:nrow(Dat_nest),
                         ~add_crop("n_fixing_forage",
                                   c(rep(0, pre), rep(comp_yield_tha, post)),
                                   0,
                                   1)
      )
    )
  
  return(Dat_nest)
}

################################
# combined scenario function
################################
# example args
# till_rotation = c("zzf", "rrrf", "r", "f", "z") # etc
# frac_remove_new = 0.3
# cc_type = c("leg2, leg4, nleg2, nleg4")
# comp_yield_tha = 1.1

build_scenario <- function(Dat_nest,
                           till_rotation = NULL,
                           frac_remove_new = NULL,
                           cc_type = NULL,
                           comp_yield_tha = NULL,
                           change_year = 2015) {
  
  # tillage code
  if (!is.null(till_rotation)) {
    Dat_nest <- modify_tillage(Dat_nest, till_rotation, change_year)
  }
  
  # residue code
  if (!is.null(frac_remove_new)) {
    Dat_nest <- modify_residues(Dat_nest, frac_remove_new, change_year)
  }
  
  # cc code
  if (!is.null(cc_type)) {
    Dat_nest <- modify_cc(Dat_nest, cc_type, change_year)
  }
  
  # stage 1 model build
  Dat_nest <- build_inputs(Dat_nest)
  
  # companion code
  if (!is.null(comp_yield_tha)) {
    Dat_nest <- modify_comp(Dat_nest, comp_yield_tha, change_year)
  }
  
  # stage 2 model build
  if (is.null(comp_yield_tha)) {
    # normal combine inputs
    Dat_nest <- combine_inputs(Dat_nest)
  } else {
    # custom combine inputs if companion present
    Dat_nest <- Dat_nest %>%
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
      )
  }
  
  # stage 3
  Dat_nest <- build_model(Dat_nest)

  return(Dat_nest)
}
