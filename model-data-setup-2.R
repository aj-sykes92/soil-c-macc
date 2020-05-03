
library(tidyverse)
library(raster)

gisdata_repo <- "GIS data repository"
projdata_repo <- "Soils-R-GGREAT/UK Soil C MACC/project-data"

# climate data nested df
# small sample version for now so as not to hammer local CPU
Dat_clim <- read_rds(find_onedrive(dir = projdata_repo, path = "climate-data-processed-small-sample.rds"))

# uk shapefile w/ DAs
Shp_UK <- shapefile(find_onedrive(dir = gisdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp"))

# sand % raster
Ras_sand <- raster(find_onedrive(dir = gisdata_repo, path = "SoilGrids 5km/Sand content/Fixed/SNDPPT_M_sl4_5km_ll.tif")) %>%
  crop(Shp_UK) %>%
  mask(Shp_UK)

# wheat area and yield bricks
Brk_wheatarea <- read_rds(find_onedrive(dir = projdata_repo, path = "uk-wheat-area-spatial-ts.rds"))
Brk_wheatyield <- read_rds(find_onedrive(dir = projdata_repo, path = "uk-wheat-yield-spatial-ts.rds"))

# convert to df and join
# bind_rows much less intensive than joining by xy - checked for matching rows
Dat_wheat <- as.data.frame(Brk_wheatarea, xy = T) %>%
  bind_cols(as.data.frame(Brk_wheatyield)) %>%
  as_tibble()

# gather and spread
Dat_wheat <- Dat_wheat %>%
  gather(-x, -y, key = key, value = value) %>%
  mutate(year = str_extract(key, "\\d{4}$") %>% as.numeric(),
         metric = str_extract(key, "^[:lower:]+(?=_)"),
         crop = str_extract(key, "(?<=_)[:lower:]+(?=_)")) %>%
  select(-key) %>%
  spread(key = metric, value = value)

# calculate fractional residual sd for stochastic sim
Dat_wheat_ts <- read_csv(find_onedrive(dir = projdata_repo, path = "faostat-uk-wheat-prod-1961-2018.csv")) %>%
  select(key = Element, year = Year, value = Value) %>%
  spread(key = key, value = value) %>%
  rename(area_kha = `Area harvested`, yield_tha = Yield) %>%
  mutate(yield_tha = yield_tha * 10^-4) %>%
  select(-area_kha)

smooth <- loess(yield_tha ~ year, data = Dat_wheat_ts)

Dat_wheat_ts <- Dat_wheat_ts %>%
  mutate(yield_smooth = predict(smooth, newdata = 1961:2018),
         resid = yield_tha - yield_smooth,
         resid_frac = resid / yield_smooth)

res_sd <- Dat_wheat_ts %>% pull(resid_frac) %>% sd()
yield_smooth_2018 <- Dat_wheat_ts$yield_smooth[Dat_wheat_ts$year == 2018]
rm(Dat_wheat_ts, smooth)
  
# nest by location and crop
Dat_wheat <- Dat_wheat %>%
  drop_na() %>%
  nest(crop_data = c(year, area, yield))

# add in wheat projections (stochastic, by sample)

annual_yield_inc <- 0.008 # estimated UK yield increase for wheat from Ray et al. (2013)

# some notion of altering linear increase assumption here
# not much to work with though!
plateau <- 1 + annual_yield_inc * (2050 - 2020) # Ray et al.'s original predictions were to 2050
yield_gap <- 1 - (linear / plateau)

# apply stochastic projections
set.seed(2605)
Dat_wheat <- Dat_wheat %>%
  mutate(crop_data = crop_data %>%
           map(function(df){
             
             fiveyearmean <- df %>%
               tail(5) %>%
               pull(yield) %>%
               mean()
             
             finalyeararea <- df %>%
               tail(1) %>%
               pull(area)
             
             sim_df <- tibble(year = 2019:2097,
                              area = finalyeararea,
                              yield_smooth = fiveyearmean,
                              frac_increase = seq(from = 1, by = frac_yield_inc, length.out = length(2019:2097))) %>%
               mutate(yield_smooth = yield_smooth * frac_increase,
                      yield = yield_smooth + yield_smooth * rnorm(n = length(2019:2097), mean = 0, sd = res_sd)) %>%
               select(year, area, yield)
             
             bind_rows(list(historic = df,
                            simulated = sim_df),
                       .id = "origin") %>%
               return()
           }))

# join up climate and crop data
# this works but for some reason it loses ~ 300 cells
# why???
temp <- Dat_clim %>%
  ungroup() %>%
  mutate(x = x %>% round(6),
         y = y %>% round(6)) %>%
  left_join(Dat_wheat %>%
              mutate(x = x %>% round(6),
                     y = y %>% round(6)),
            by = c("x", "y")) %>%
  drop_na(crop)



temp <- Dat_wheat %>%
  sample_n(100) %>%
  pull(x)

round(temp, 10) %in% round(Dat_clim$x, 10)

