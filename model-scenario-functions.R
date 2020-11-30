
################################
# build model baseline scenario
################################
build_baseline_scenario <- function(Dat_nest) {
  
  # crop residue method lookups
  crop_bgr_coeffs <- read_csv("parameter-data/below-ground-residue-coefficients.csv", na = c("", "NA"), col_types = "cnnn")
  crop_agr_coeffs <- read_csv("parameter-data/above-ground-residue-coefficients.csv", na = c("", "NA"), col_types = "cnnnnn")
  
  # crop and manure fractional content parameters
  man_params <- read_csv("parameter-data/manure-coefficients.csv", na = c("", "NA"), col_type = "cnnn")
  crop_params <- read_csv("parameter-data/crop-N-and-lignin-fractions.csv", na = c("", "NA"), col_type = "cnn")
  
  # cover crop parameters
  cc_probs <- read_csv("parameter-data/cover-crop-tillage-proportion.csv", col_types = "cnn")
  cc_params <- read_csv("parameter-data/cover-crop-parameters.csv", col_types = "ccinnnnnnnncc")
  
  # build model data
  Dat_nest <- Dat_nest %>%
    build_residue_C(crop_agr_coeffs, crop_bgr_coeffs) %>%
    build_cover_crops(cc_probs, cc_params) %>%
    build_manure_C(man_params) %>%
    build_fractions(crop_params, man_params)
  
  # add a 20 year run in period to the model data
  Dat_nest <- Dat_nest %>%
    mutate(data_runin = map(data, ~run_in(.x, years = 20)))
  
  # run model
  Dat_nest <- Dat_nest %>%
    mutate(scenario_baseline = map(data_runin, run_model))
  
  return(Dat_nest)
}

################################
# build tillage scenarios
################################

# modification function
tillage_mod_fun <- function(data, type, uptake, change_year) {
  is_conv <- data$till_type[data$year == change_year] == "full"
  sample <- runif(1)
  
  length <- data %>% filter(year >= change_year) %>% nrow()
  if (type == "reduced") measure <- rep("reduced", length)
  if (type == "biennial") measure <- rep(c("full", "zero"), length)[1:length]
  
  if (is_conv & sample <= uptake) {
    data <- data %>%
      mutate(till_type =
               ifelse(year >= change_year,
                      measure,
                      till_type)
      )
  }
  return(data)
}

# main tillage scenario function
build_tillage_scenario <- function(Dat_nest, type, uptake, change_year = 2015) {
  
  # crop residue method lookups
  crop_bgr_coeffs <- read_csv("parameter-data/below-ground-residue-coefficients.csv", na = c("", "NA"), col_types = "cnnn")
  crop_agr_coeffs <- read_csv("parameter-data/above-ground-residue-coefficients.csv", na = c("", "NA"), col_types = "cnnnnn")
  
  # crop and manure fractional content parameters
  man_params <- read_csv("parameter-data/manure-coefficients.csv", na = c("", "NA"), col_type = "cnnn")
  crop_params <- read_csv("parameter-data/crop-N-and-lignin-fractions.csv", na = c("", "NA"), col_type = "cnn")
  
  # cover crop parameters
  cc_probs <- read_csv("parameter-data/cover-crop-tillage-proportion.csv", col_types = "cnn")
  cc_params <- read_csv("parameter-data/cover-crop-parameters.csv", col_types = "ccinnnnnnnncc")
  
  # modify to measure
  Dat_nest <- Dat_nest %>%
    mutate(data = map(data, ~tillage_mod_fun(.x, type = type, uptake = uptake, change_year = change_year)))
  
  # build model data
  Dat_nest <- Dat_nest %>%
    build_residue_C(crop_agr_coeffs, crop_bgr_coeffs) %>%
    build_cover_crops(cc_probs, cc_params) %>%
    build_manure_C(man_params) %>%
    build_fractions(crop_params, man_params)
  
  # add a 20 year run in period to the model data
  Dat_nest <- Dat_nest %>%
    mutate(data_runin = map(data, ~run_in(.x, years = 20)))
  
  # run model
  Dat_nest <- Dat_nest %>%
    mutate(scenario_baseline = map(data_runin, run_model))
  
  return(Dat_nest)
}
