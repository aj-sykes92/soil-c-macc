
library(ncdf4)
library(raster)
library(rgdal)
library(tidyverse)
library(lubridate)

gisdata_repo <- "GIS data repository"
projdata_repo <- "Soils-R-GGREAT/UK Soil C MACC/project-data"

# read in precip and temp anomalies saved in useful format
Dat_tasAnom <- read_rds(find_onedrive(dir = gisdata_repo, path = "UKCP/tasAnom-b8100-1960-2099-regional-3000sample-reshaped.rds"))
Dat_prAnom <- read_rds(find_onedrive(dir = gisdata_repo, path = "UKCP/prAnom-b8100-1960-2099-regional-3000sample-reshaped.rds"))

# select random set of 100 samples to give us something manageable to work with
# random samples from 1:3000, no replacement
set.seed(2605)
samples <- tibble(x = 1:3000) %>%
  sample_n(100, replace = F) %>%
  arrange(x) %>%
  pull(x)

# filter for samples in random selection
# keeps samples consistent between dates
Dat_tasAnom <- Dat_tasAnom %>%
  filter(sample %in% samples) %>%
  arrange(sample) %>%
  arrange(date)

Dat_prAnom <- Dat_prAnom %>%
  filter(sample %in% samples) %>%
  arrange(sample) %>%
  arrange(date)

# check out work so far
glimpse(Dat_tasAnom)
glimpse(Dat_prAnom)

#####################################################
# process environmental spatial data
#####################################################

# uk shapefile w/ DAs
Shp_UK <- shapefile(find_onedrive(dir = gisdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp"))

# sand % raster
Ras_sand <- raster(find_onedrive(dir = gisdata_repo, path = "SoilGrids 5km/Sand content/Fixed/SNDPPT_M_sl4_5km_ll.tif")) %>%
  crop(Shp_UK) %>%
  mask(Shp_UK)

# wheat area and yield bricks
Brk_wheatarea <- read_rds(find_onedrive(dir = projdata_repo, path = "uk-wheat-area-spatial-ts.rds"))
Brk_wheatyield <- read_rds(find_onedrive(dir = projdata_repo, path = "uk-wheat-yield-spatial-ts.rds"))

# create raster so we can join data at DA level
Ras_DA <- Ras_sand # sand % makes a good template
Ras_DA[!is.na(Ras_DA %>% mask(subset(Shp_UK, Shp_UK@data[["NAME_1"]]=="England")))] <- 1
Ras_DA[!is.na(Ras_DA %>% mask(subset(Shp_UK, Shp_UK@data[["NAME_1"]]=="Northern Ireland")))] <- 2
Ras_DA[!is.na(Ras_DA %>% mask(subset(Shp_UK, Shp_UK@data[["NAME_1"]]=="Scotland")))] <- 3
Ras_DA[!is.na(Ras_DA %>% mask(subset(Shp_UK, Shp_UK@data[["NAME_1"]]=="Wales")))] <- 4
names(Ras_DA) <- "DA_num"

#####################################################
# process historical spatial data for monthly climate variables
# opening full date range (1901-2018)
#####################################################

# precipitation, mm per month
Brk_precip <- brick(find_onedrive(dir = gisdata_repo, path = "CRU TS v4-03/pre/cru_ts4.03.1901.2018.pre.dat.nc"), var = "pre")

# monthly average temperature, degrees Celsius
Brk_temp <- brick(find_onedrive(dir = gisdata_repo, path = "CRU TS v4-03/tmp/cru_ts4.03.1901.2018.tmp.dat.nc"), var = "tmp")

# crop to extent of DA raster
Brk_precip <- Brk_precip %>% crop(Ras_DA)
Brk_temp <- Brk_temp %>% crop(Ras_DA)

# resample to DA resolution
Brk_precip <- Brk_precip %>% resample(Ras_DA)
Brk_temp <- Brk_temp %>% resample(Ras_DA)

# mask to DA coverage
Brk_precip <- Brk_precip %>% mask(Ras_DA)
Brk_temp <- Brk_temp %>% mask(Ras_DA)

# remove layers pre-1961 (earliest yield data)
years <- names(Brk_precip) %>%
  str_extract("(?<=^X)\\d{4}") %>%
  as.numeric()
keep <- which(years >= 1961)
Brk_precip <- Brk_precip[[keep]]
Brk_temp <- Brk_temp[[keep]]

# add in DA raster to precipitation brick
Brk_precip <- Brk_precip %>% addLayer(Ras_DA)

# convert to data frame
Dat_precip <- Brk_precip %>% as.data.frame(xy = T) %>% as_tibble()
Dat_temp <- Brk_temp %>% as.data.frame(xy = T) %>% as_tibble()

# check we can bind cols (less intensive than join by c(x, y))
sum(Dat_precip$x != Dat_temp$x)
sum(Dat_precip$y != Dat_temp$y)

colnames(Dat_precip) <- colnames(Dat_precip) %>% str_replace("^X", "precip")
colnames(Dat_temp) <- colnames(Dat_temp) %>% str_replace("^X", "temp")

# bind cols
Dat_clim <- Dat_precip %>%
  bind_cols(Dat_temp %>% select(-x, -y))

# remove precursors
rm(Dat_temp, Dat_precip, Brk_precip, Brk_temp)

# gather
Dat_clim <- Dat_clim %>%
  gather(-x, -y, -DA_num, key = key, value = value) %>%
  mutate(metric = key %>%
           str_extract("^[:lower:]+(?=\\d)"),
         date = key %>%
           str_replace_all("[:lower:]", "") %>%
           ymd()) %>%
  select(-key) %>%
  spread(key = metric, value = value) %>%
  rename(precip_mm = precip, temp_centigrade = temp) %>%
  drop_na()

# calculate average climate for anom processing
clim_av <- Dat_clim %>%
  filter(date >= "1981-01-01" %>% ymd(),
         date <= "2000-12-31" %>% ymd()) %>%
  mutate(month = month(date)) %>%
  group_by(x, y, DA_num, month) %>%
  summarise(precip_mm = mean(precip_mm),
            temp_centigrade = mean(temp_centigrade)) %>%
  ungroup() %>%
  left_join(tibble(DA_num = c(1, 2, 3, 4),
                   DA = c("England", "NorthernIreland", "Scotland", "Wales")),
            by = "DA_num") %>%
  select(x, y, DA, month, precip_mm, temp_centigrade)

# join and prep anomaly data
# not interested in anomalies before 2019 — need some small overlap though
Dat_anom <- full_join(Dat_tasAnom, Dat_prAnom, by = c("sample", "date", "region_chr")) %>%
  rename(precip_mm = prAnom, temp_centigrade = tasAnom, DA = region_chr) %>%
  filter(date >= ymd("2019-01-01"),
         date < ymd("2098-01-01"))

# get rid of large anomaly datasets
rm(Dat_tasAnom, Dat_prAnom)

# join climate average by x, y, and month and adjust with anomalies
Dat_anom <- Dat_anom %>%
  mutate(month = month(date),
         year = year(date)) %>%
  filter(DA %in% c("England", "NorthernIreland", "Wales", "Scotland"))

# nesting data by sample at this point
Dat_main <- Dat_anom %>%
  mutate(date = as_date(date)) %>% # should have done this earlier
  group_by(sample, DA) %>%
  nest()

# compress and interpolate where necessary
# this UKCP data is bloody awkward to get in monthly format
temp <- tibble(date = seq(from = ymd("2019-01-16"), to = ymd("2097-12-17"), by = as.difftime(months(1))) %>% ymd(),
               month = month(date),
               year = year(date)) %>%
  select(-date)

Dat_main <- Dat_main %>%
  mutate(data_reg = data %>%
           map(function(df){
             df %>%
               group_by(month, year) %>%
               summarise(temp_centigrade = mean(temp_centigrade),
                         precip_mm = mean(precip_mm))
           }),
         data_reg = data_reg %>%
           map(function(df){
             df %>%
               right_join(temp, by = c("month", "year"))
           }),
         data_reg = data_reg %>%
           map(function(df){
             df %>%
               mutate(temp_centigrade = temp_centigrade %>% forecast::na.interp() %>% as.numeric(),
                      precip_mm = precip_mm %>% forecast::na.interp() %>% as.numeric())
           })
  )

# join clim_av to individual months/samples and adjust
Dat_main <- Dat_main %>%
  mutate(data_full = map2(DA, data_reg, function(da, df){
    df %>%
      mutate(DA = da) %>%
      full_join(clim_av %>% filter(DA == da), by = c("DA", "month")) %>%
      mutate(temp_centigrade = temp_centigrade.x + temp_centigrade.y,
             precip_mm = (1 + precip_mm.x / 100) * precip_mm.y) %>%
      select(x, y, month, year, precip_mm, temp_centigrade) %>%
      arrange(x, y, year, month)
  }))


# lose transitional data and re-nest with lat/lon outside
Dat_main <- Dat_main %>%
  select(sample, DA, data_full) %>%
  unnest(cols = c(data_full)) %>%
  group_by(x, y, sample, DA) %>%
  nest()

# bind rows to historical climate data
Dat_clim <- Dat_clim %>%
  mutate(month = month(date),
         year = year(date)) %>%
  select(x, y, month, year, precip_mm, temp_centigrade) %>%
  nest(historic = c(month, year, precip_mm, temp_centigrade))

Dat_main <- Dat_main %>%
  rename(simulated = data) %>%
  left_join(Dat_clim, by = c("x", "y"))

# join
Dat_main <- Dat_main %>%
  mutate(clim_joined = map2(historic, simulated, function(df1, df2){
             bind_rows("historical" = df1,
                       "simulated" = df2,
                       .id = "origin")
           }))

# select only joined data
Dat_main <- Dat_main %>% select(-historic, -simulated)

# this is a key stage in data wrangling -- write out .rds here for potential future uses
# write_rds(Dat_main, "D:/Alasdair/GIS data repository/uk-full-climvars-1961-2098-100s.rds")
# Dat_main <- read_rds("D:/Alasdair/GIS data repository/uk-full-climvars-1961-2098-100s.rds"))

# calculate pet using thornthwaite method
# https://upcommons.upc.edu/bitstream/handle/2117/89152/Appendix_10.pdf?sequence=3&isAllowed=y

# daylength calculations using insol
library(insol)

lats <- Dat_main %>%
  pull(y) %>%
  unique()


jdays <- tibble(date = seq(from = ymd("2019-01-01"), to = ymd("2019-12-31"), by = as.difftime(days(1)))) %>%
  mutate(month = month(date),
         jday = yday(date),
         mday = days_in_month(date)) %>%
  group_by(month) %>%
  summarise(mday = mean(mday),
            jday = mean(jday))
  

daylength <- tibble(y = rep(lats, 12),
                    month = rep(1:12, each = length(lats))) %>%
  left_join(jdays, by = "month") %>%
  mutate(lon = 0,
         time_zone = 0,
         daylength = pmap_dbl(list(y, lon, jday, time_zone), function(a, b, c, d){
           return(daylength(a, b, c, d)[3])
         }))

# function required to combat annoying R 'feature' (returns NaN for negative numbers raised to non-integer powers...)
rtp <- function(x, power){
  sign(x) * abs(x) ^ (power)
}

Dat_main <- Dat_main %>%
  mutate(clim_joined = map2(y, clim_joined, function(lat, df){
    df %>%
      mutate(y = lat) %>%
      group_by(year) %>%
      mutate(I = sum(rtp(temp_centigrade / 5, 1.514)),
            alpha = 675*10^-9 * rtp(I, 3) - 771*10^-7 * rtp(I, 2) + 1792*10^-5 * I + 0.49239,
            pet_mm = rtp(16 * ((10 * temp_centigrade) / I), alpha)) %>%
      ungroup() %>%
      left_join(daylength %>% select(y, month, daylength, mday), by = c("y", "month")) %>%
      mutate(pet_mm = pet_mm * daylength / 12 * mday / 30,
             pet_mm = ifelse(pet_mm < 1, 1, pet_mm)) %>% # prevents errors with negative PET/div by zero
      select(-y, -I, -alpha, -mday, -daylength) %>%
      mutate(precip_mm = ifelse(precip_mm < 0, 0, precip_mm)) # # another quick and dirty fix for potential errors - occasional negative precipitation values
    }))

# write out main data as raw file
write_rds(Dat_main, "D:/Alasdair/GIS data repository/uk-full-climvars-1961-2098-100s.rds")

# condense to annual tfac/wfac to minimise data read in/out when running model
source("ipcc-c-model-functions.R")

Dat_main <- Dat_main %>%
  mutate(clim_annual = clim_joined %>%
           map(function(df){
             df %>%
               group_by(year) %>%
               summarise(wfac = wfac(precip = precip_mm, PET = pet_mm),
                         tfac = tfac(temp = temp_centigrade)) %>%
               ungroup()
           }))

write_rds(Dat_main %>% select(-clim_joined), "D:/Alasdair/GIS data repository/uk-annual-climvars-1961-2098-100s.rds")

# small sample (~17MB) to cloud for local analysis/script dev
write_rds(Dat_main %>% select(-clim_joined) %>% filter(sample == 26), find_onedrive(dir = projdata_repo, path = "climate-data-processed-small-sample.rds"))