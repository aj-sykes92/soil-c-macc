library(tidyverse)

# read in data
Dat_nest <- read_rds(project_data(path = "project-data/model-data-input-small-sample-wheat-manure-tillage-data.rds"))

# read cover crop data
cc_probs <- read_csv("parameter-data/cover-crop-tillage-proportion.csv", col_types = "cnn")
cc_params <- read_csv("parameter-data/cover-crop-parameters.csv", col_types = "ccinnnnnnnncc")

# initial wrangle
cc_params <- cc_params %>%
  rename(type = Type_cc) %>%
  group_by(type) %>%
  summarise(yield_tha = mean(Mean_yield_t_ha),
            dry = mean(DRY),
            rs = mean(RS),
            n_frac = mean(N_frac),
            lignin_frac = mean(Lignin_frac),
            cn_ratio = mean(CN_ratio),
            .groups = "drop") %>%
  mutate(agr = yield_tha * dry,
         bgr = agr * rs,
         om_input = agr + bgr,
         n_input = om_input * n_frac,
         lignin_input = om_input * lignin_frac,
         c_input = n_input * cn_ratio) %>%
  select(type, om_input, c_input, n_input, lignin_input)

# split into assumptions based on yields from CM
cc_leg_2spp <- cc_params %>%
  mutate(rel_yield = case_when(
    type == "cereal" ~ 0.5,
    type == "legume" ~ 0.5,
    TRUE ~ 0)
  ) %>%
  summarise(om_input = sum(om_input * rel_yield),
            c_input = sum(c_input * rel_yield),
            n_input = sum(n_input * rel_yield),
            lignin_input = sum(lignin_input * rel_yield)
  ) %>%
  as.list()

cc_leg_4spp <- cc_params %>%
  mutate(rel_yield = case_when(
    type == "cereal" ~ 0.4,
    type == "legume" ~ 0.2,
    type == "brassica" ~ 0.2,
    type == "broadleaf" ~ 0.2,
    TRUE ~ 0)
  ) %>%
  summarise(om_input = sum(om_input * rel_yield),
            c_input = sum(c_input * rel_yield),
            n_input = sum(n_input * rel_yield),
            lignin_input = sum(lignin_input * rel_yield)
  ) %>%
  as.list()

cc_nleg_2spp <- cc_params %>%
  mutate(rel_yield = case_when(
    type == "cereal" ~ 0.5,
    type == "brassica" ~ 0.25,
    type == "broadleaf" ~ 0.25,
    TRUE ~ 0)
  ) %>%
  summarise(om_input = sum(om_input * rel_yield),
            c_input = sum(c_input * rel_yield),
            n_input = sum(n_input * rel_yield),
            lignin_input = sum(lignin_input * rel_yield)
  ) %>%
  as.list()

cc_nleg_4spp <- cc_params %>%
  mutate(rel_yield = case_when(
    type == "cereal" ~ 0.4,
    type == "brassica" ~ 0.2,
    type == "broadleaf" ~ 0.4,
    TRUE ~ 0)
  ) %>%
  summarise(om_input = sum(om_input * rel_yield),
            c_input = sum(c_input * rel_yield),
            n_input = sum(n_input * rel_yield),
            lignin_input = sum(lignin_input * rel_yield)
  ) %>%
  as.list()

# condense to list of lists
cc_list <- list(leg4 = cc_leg_4spp,
                leg2 = cc_leg_2spp,
                nleg4 = cc_nleg_4spp,
                nleg2 = cc_nleg_2spp)
rm(cc_leg_4spp, cc_leg_2spp, cc_nleg_4spp, cc_nleg_2spp)

# add to Dat_nest

# has cover crop in cell/year?
has_cc <- function(data, cc_probs) {
  data %>%
    left_join(cc_probs, by = "till_type") %>%
    mutate(cc_threshold = runif(n = nrow(data), min = frac_cc_min, max = frac_cc_max),
           has_cc = runif(n = nrow(data)) <= cc_threshold) %>%
    pull(has_cc)
}

# stochastic function, lots of sampling
Dat_nest <- Dat_nest %>%
  mutate(has_cc = map(data, ~has_cc(.x, cc_probs)))

expand_list <- function(cc_list, has_cc) {
  map(cc_list, ~.x * has_cc)
}

# add cc if has_cc 
Dat_nest <- Dat_nest %>%
  mutate(cc_input = map(has_cc, ~cc_list[[sample(1:4, size = 1)]] %>% expand_list(.x))) %>%
  select(-has_cc)

# late-stage troubleshoot --- bound manure stochastic samples
Dat_nest <- Dat_nest %>%
  mutate(man_type = man_type %>% str_replace_all("-", "_"),
         data = map(data, ~.x %>%
                      mutate(
                        man_nrate = ifelse(man_nrate < 0, 0, man_nrate),
                        man_nrate = ifelse(man_nrate >= 500, 500, man_nrate)
                      )
         )
  )

# write out
write_rds(Dat_nest, project_data(path = "project-data/model-data-input-small-sample-wheat-manure-tillage-cc-data.rds"))
write_rds(cc_list, "parameter-data/cc-list.rds")
          