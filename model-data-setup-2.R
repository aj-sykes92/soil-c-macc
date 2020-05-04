library(raster)
library(tidyverse)

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
plateau <- 1 + (0.008 * (2050-2020)) # Ray et al. only make predictions to 2050

yield_curve <- tibble(year = 2019:2097,
                      ry = plateau + ((1.008-plateau) / (1 + 0.08 * (year - (2019-1))))) # empirically calibrated d value here (0.08) - balance early increases similar to reported (0.008) with reasonable curvature given timespan

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
                              frac_increase = yield_curve$ry) %>%
               mutate(yield_smooth = yield_smooth * frac_increase,
                      yield = yield_smooth + yield_smooth * rnorm(n = length(2019:2097), mean = 0, sd = res_sd)) %>%
               select(year, area, yield)
             
             bind_rows(list(historic = df,
                            simulated = sim_df),
                       .id = "origin") %>%
               return()
           }))

rm(res_sd, yield_smooth_2018, annual_yield_inc, plateau)
Dat_wheat$crop_data[[4]] %>%
  ggplot(aes(x = year, y = yield, colour = origin)) +
  geom_line()

# join up climate and crop data
Dat_main <- Dat_clim %>%
  ungroup() %>%
  mutate(x = x %>% round(6),
         y = y %>% round(6)) %>%
  left_join(Dat_wheat %>%
              mutate(x = x %>% round(6),
                     y = y %>% round(6)),
            by = c("x", "y")) %>%
  drop_na(crop)

# below are checks to ensure join worked correctly
# lay joined data over wheat data
ggplot() +
  geom_raster(data = Dat_wheat,
              aes(x = x, y = y), fill = "blue") +
  geom_raster(data = Dat_main,
              aes(x = x, y = y), fill = "red") +
  coord_quickmap()
# non-joined data = ROI, mainland Europe + 3-4 errant pixels
# former is as it should be and can live with the latter

# lay joined data over climate data
ggplot() +
  geom_raster(data = Dat_clim,
              aes(x = x, y = y), fill = "blue") +
  geom_raster(data = Dat_main,
              aes(x = x, y = y), fill = "red") +
  coord_quickmap()
# no wheat in missing (non-joined) areas - as it should be

# join in sand data too
Dat_main <- Dat_main %>%
  left_join(
    Ras_sand %>%
      as.data.frame(xy = T) %>%
      as_tibble() %>%
      rename(sand_frac = SNDPPT_M_sl4_5km_ll) %>%
      mutate(x = x %>% round(6),
             y = y %>% round(6),
             sand_frac = sand_frac * 10^-2),
    by = c("x", "y")
  )

# renest
Dat_main <- Dat_main %>%
  mutate(data = pmap(list(crop_data,
                          clim_annual,
                          crop,
                          sand_frac),
                     function(df_crop, df_clim, croptype, sandfrac){
                       df_crop %>%
                         left_join(df_clim, by = "year") %>%
                         mutate(crop_type = croptype,
                                sand_frac = sandfrac) %>%
                         rename(yield_tha = yield, area_ha = area)
                     })) %>%
  select(-crop_data, -clim_annual, -crop, -sand_frac)

# write out
write_rds(Dat_main, find_onedrive(dir = projdata_repo, path = "model-data-input-small-sample.rds"))
