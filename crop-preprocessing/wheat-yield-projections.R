
Dat_wheat_ts <- read_csv(find_onedrive(dir = projdata_repo, path = "faostat-uk-wheat-prod-1961-2018.csv")) %>%
  select(key = Element, year = Year, value = Value) %>%
  spread(key = key, value = value) %>%
  rename(area_kha = `Area harvested`, yield_tha = Yield) %>%
  mutate(area_kha = area_kha * 10^-3,
         yield_tha = yield_tha * 10^-4)


Dat_wheat_ts %>%
  ggplot(aes(x = year, y = area_kha)) +
  geom_line() +
  geom_smooth()

Dat_wheat_ts %>%
  ggplot(aes(x = year, y = yield_tha)) +
  geom_line() +
  geom_smooth()

smooth <- loess(yield_tha ~ year, data = Dat_wheat_ts)

Dat_wheat_ts <- Dat_wheat_ts %>%
  mutate(yield_smooth = predict(smooth, newdata = 1961:2018),
         resid = yield_tha - yield_smooth,
         resid_frac = resid / yield_smooth)

Dat_wheat_ts %>%
  ggplot(aes(x = resid)) +
  geom_histogram()

Dat_wheat_ts %>%
  pull(resid) %>%
  summary()

Dat_wheat_ts %>%
  summarise(mean_resid = mean(resid),
            sd_resid = sd(resid))

Dat_wheat_ts %>%
  ggplot(aes(x = year, y = resid_frac)) +
  geom_col()

Dat_wheat_ts %>%
  ggplot(aes(x = year, y = abs(resid_frac))) +
  geom_col() +
  geom_smooth()

set.seed(2605)
Dat_wheat_sim <- Dat_wheat_ts %>%
  select(year, yield_tha, yield_smooth) %>%
  mutate(yield_sim1 = yield_smooth + rnorm(n = nrow(Dat_wheat_ts), mean = 0, sd = sd(Dat_wheat_ts$resid)),
         yield_sim2 = yield_smooth + yield_smooth * rnorm(n = nrow(Dat_wheat_ts), mean = 0, sd = sd(Dat_wheat_ts$resid_frac)))

Dat_wheat_sim %>%
  ggplot() +
  geom_line(aes(x = year, y = yield_tha), colour = "blue") +
  geom_smooth(aes(x = year, y = yield_tha), se = F) +
  geom_line(aes(x = year, y = yield_sim2), colour = "darkred")

# full integrated simulation using method for yield_sim2 above
set.seed(2605)
Dat_wheat_full <- bind_rows(list(historic = Dat_wheat_ts %>% select(year, yield_tha, yield_smooth),
                                 simulated = tibble(year = 2019:2097,
                                                    yield_smooth = Dat_wheat_sim$yield_smooth[nrow(Dat_wheat_sim)],
                                                    frac_increase = seq(from = 1, by = 0.008, length.out = length(2019:2097))) %>%
                                   mutate(yield_smooth = yield_smooth * frac_increase) %>%
                                   select(-frac_increase)),
                            .id = "origin") %>%
  mutate(yield_sim = yield_smooth + yield_smooth * rnorm(n = length(1961:2097), mean = 0, sd = sd(Dat_wheat_ts$resid_frac)),
         yield_sim = ifelse(origin == "historic", yield_tha, yield_sim))

Dat_wheat_full %>%
  ggplot(aes(x = year, y = yield_sim, colour = origin)) +
  geom_line()

# raster to check out current wheat yield spread
# basically sense check linear increase as modelled above
Ras_wheatyield <- raster(find_onedrive(dir = gisdata_repo, path = "MapSpam data/Yield/yield_wheat.tif"))

# masking shapefile
Shp_UK <- find_onedrive(dir = gisdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()

Ras_wheatyield <- Ras_wheatyield %>% crop(Shp_UK) %>% mask(Shp_UK)

hist(Ras_wheatyield)

Ras_wheatyield %>%
  quantile(seq(0, 1, 0.05))

# ok for now — we're not out of CIs with linear prediction

  
