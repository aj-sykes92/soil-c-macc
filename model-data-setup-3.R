library(tidyverse)

## gisdata_repo <- "GIS data repository"
## projdata_repo <- "Soils-R-GGREAT/UK Soil C MACC/project-data"

# main dataset from model-data-setup-2.R
Dat_nest <- read_rds(project_data(path = "project-data/model-data-input-small-sample.rds"))

# manure background data
Dat_manrate <- read_rds(project_data(path = "project-data/manure-data/manure-app-rates.rds"))
Dat_manab <- read_rds(project_data(path = "project-data/manure-data/manure-rel-abundance-ts.rds"))

# function to build basis for random selector (no sense repeating > once)
build_man_selector <- function(crop){
  df <- Dat_manrate %>%
    ungroup() %>%
    filter(croptype == crop) %>%
    mutate(treatfrac = treatfrac_mean * 10^-2,
           treatfrac_cum = cumsum(treatfrac)) %>%
    mutate(upper = treatfrac_cum,
           lower = lag(treatfrac_cum, default = 0)) %>%
    select(trans, lower, upper)
  
  df <- df %>%
    bind_rows(tibble(trans = "none",
                     lower = df$upper[nrow(df)],
                     upper = 1))
  return(df)
}

selector_winter <- build_man_selector("winter")
selector_spring <- build_man_selector("spring")

# function to run random selector
run_man_selector <- function(n, season, seed = NULL){
  if(!is.null(seed)) set.seed(seed)
  
  selector <- paste0("selector_", season) %>% get()
  
  rand <- runif(n)
  match <- map_chr(rand, function(rand){
    logical <- rand > selector$lower & rand <= selector$upper
    trans <- selector$trans[logical]
    return(trans)
  })
  return(match)
}

run_season_selector <- function(n, seed){
  if(!is.null(seed)) set.seed(seed)
  
  rand <- runif(n)
  season <- ifelse(rand <= 0.05, "spring", "winter") # https://copeseeds.co.uk/products/conventional-seed/#group-1
  return(season)
}

# run random selection over Dat_nest
# assumes that manure dressing % stays constant and rates vary
# assumes also that manure type/etc. is consistent over time period

Dat_nest <- Dat_nest %>%
  mutate(growing_season = run_season_selector(n = nrow(Dat_nest), seed = 2605),
         man_type = run_man_selector(n = nrow(Dat_nest), season = growing_season, seed = 0212))

# construct manure future relative abundance dataset
# based on CCC (2020) prediction of 8% less cattle/sheep

n <- length(2018:2097)
types <- Dat_manab %>% pull(trans) %>% unique()
const <- rep(1, times = n)
decl <- seq(1, 0.92, length.out = n)

Dat_manab_future <- tibble(trans = rep(types, each = n),
                           year = rep(2018:2097, times = length(types)),
                           rel = c(decl, decl, const, const, decl, const))
  
  
# join to historical manure abundance data
Dat_manab <- Dat_manab %>%
  bind_rows(Dat_manab_future) %>%
  arrange(trans, year)

# add in "none" conversion factor
Dat_manab <- Dat_manab %>%
  bind_rows(tibble(trans = "none",
                   year = 1961:2097,
                   rel = 0))

# add to Dat_nest
Dat_nest <- Dat_nest %>%
  mutate(data = map2(data, man_type, function(df, man_type){
    df %>%
      mutate(trans = man_type) %>%
      left_join(Dat_manab, by = c("trans", "year")) %>%
      select(-trans) %>%
      rename(manure_rel_rate = rel)
  }))
  
# stochastic manure rate estimate
Dat_manrate <- Dat_manrate %>%
  bind_rows(tibble(trans = c("none", "none"),
                   croptype = c("winter", "spring"),
                   nrate_mean = c(0, 0),
                   nrate_sd = c(0, 0)))

Dat_nest <- Dat_nest %>%
  left_join(Dat_manrate %>% select(man_type = trans, growing_season = croptype, nrate_mean, nrate_sd),
            by = c("growing_season", "man_type"))

set.seed(2605)

Dat_nest <- Dat_nest %>%
  mutate(data = pmap(list(data, nrate_mean, nrate_sd), function(df, mean, sd) {
    df %>%
      mutate(man_nrate = rnorm(n = nrow(df), mean, sd) * manure_rel_rate) %>%
      select(-manure_rel_rate)
  })) %>%
  select(-nrate_mean, -nrate_sd)

# write out model data

##write_rds(Dat_nest, find_onedrive(dir = projdata_repo, path = "model-data-input-small-sample-wheat-manure-data.rds"))

write_rds(Dat_nest, project_data(path = "project-data/model-data-input-small-sample-wheat-manure-data.rds"))