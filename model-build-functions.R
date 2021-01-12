# functions to build and run soil carbon model

build_inputs <- function(Dat_nest) {
  Dat_nest %>%
    mutate(crop_input = map(data, ~add_crop("wheat", .x$yield_tha, .x$frac_remove, .x$frac_renew)),
           man_input = map2(data, man_type, ~add_manure(.y, .x$man_nrate)),
           cc_input = map(cc_input, ~add_custom(.x$om_input, .x$c_input, .x$n_input, .x$lignin_input))
    )
}

# combine soil inputs into full input dataset
combine_inputs <- function(Dat_nest) {
  Dat_nest %>%
    mutate(
      soil_input = pmap(
        list(crop_input, man_input, cc_input),
        function(x, y, z) {
          if (y$livestock_type == "none") {
            build_soil_input(x, z)
          } else {
            build_soil_input(x, y, z)
          }
        })
    )
}

# build model input + run model
build_model <- function(Dat_nest) {
  Dat_nest %>%
    mutate(
      model_input = map2(data, soil_input,
                         ~bind_cols(select(.x, year, wfac, tfac, sand_frac, till_type), .y)),
      model_output = map(model_input, ~run_model(.x, runin_dur = 50, calculate_climfacs = FALSE))
    )
}
