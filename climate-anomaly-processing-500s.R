
library(ncdf4)
library(raster)
library(rgdal)
library(tidyverse)
library(lubridate)

data_repo <- "GIS data repository"

# read in precip and temp anomalies saved in useful format
Dat_tasAnom <- read_rds(find_onedrive(dir = data_repo, path = "UKCP/tasAnom-b8100-1960-2099-regional-3000sample-reshaped.rds"))
Dat_prAnom <- read_rds(find_onedrive(dir = data_repo, path = "UKCP/prAnom-b8100-1960-2099-regional-3000sample-reshaped.rds"))

# select random set of 500 samples to give us something manageable to work with
# random samples from 1:3000, no replacement
set.seed(2605)
samples <- tibble(x = 1:3000) %>%
  sample_n(500, replace = F) %>%
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
Shp_UK <- shapefile(find_onedrive(dir = data_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp"))

# sand % raster
Ras_sand <- raster(find_onedrive(dir = data_repo, path = "SoilGrids 5km/Sand content/Fixed/SNDPPT_M_sl4_5km_ll.tif")) %>%
  crop(Shp_UK) %>%
  mask(Shp_UK)

# wheat area raster
# Ras_wheat <- raster(find_onedrive(dir = data_repo, path = "MapSPAM data/Physical area/phys_area_wheat.tif"))
# replace this with read-in of .rds-saved bricks in projdata repo (created with separate script)
# may want to rename this script

# create raster so we can join data at DA level
Ras_DA <- Ras_sand # sand % makes a good template
Ras_DA[!is.na(Ras_DA %>% mask(subset(Shp_UK, Shp_UK@data[["NAME_1"]]=="England")))] <- 1
Ras_DA[!is.na(Ras_DA %>% mask(subset(Shp_UK, Shp_UK@data[["NAME_1"]]=="Northern Ireland")))] <- 2
Ras_DA[!is.na(Ras_DA %>% mask(subset(Shp_UK, Shp_UK@data[["NAME_1"]]=="Scotland")))] <- 3
Ras_DA[!is.na(Ras_DA %>% mask(subset(Shp_UK, Shp_UK@data[["NAME_1"]]=="Wales")))] <- 4

#####################################################
# process historical spatial data for monthly climate variables
# opening full date range (1901-2018)
#####################################################

# precipitation, mm per month
Brk_precip <- brick(find_onedrive(dir = data_repo, path = "CRU TS v4-03/pre/cru_ts4.03.1901.2018.pre.dat.nc"), var = "pre")

# monthly average temperature, degrees Celsius
Brk_temp <- brick(find_onedrive(dir = data_repo, path = "CRU TS v4-03/tmp/cru_ts4.03.1901.2018.tmp.dat.nc"), var = "tmp")

# create tibble with climate data extracted from raster bricks
Dat_clim <- tibble(date = Brk_precip %>% names(),
                   precip_mm = raster::extract(Brk_precip, bush_estate) %>% as.numeric(),
                   temp_centigrade = raster::extract(Brk_temp, bush_estate) %>% as.numeric()) %>%
  mutate(date = date %>% str_replace("X", "") %>% ymd())

# remove bricks
rm(Brk_precip, Brk_temp)

clim_av <- Dat_clim %>%
  filter(date >= "1981-01-01" %>% ymd(),
         date <= "2000-12-31" %>% ymd()) %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise(precip_mm = mean(precip_mm),
            temp_centigrade = mean(temp_centigrade))

# join and prep anomaly data
# not interested in anomalies before 2019 — need some small overlap though
Dat_anom <- full_join(Dat_tasAnom, Dat_prAnom, by = c("sample", "date", "region_chr")) %>%
  select(-region_chr) %>%
  rename(precip_mm = prAnom, temp_centigrade = tasAnom) %>%
  filter(date >= ymd("2019-01-01"),
         date < ymd("2098-01-01"))

# get rid of large anomaly datasets
rm(Dat_tasAnom, Dat_prAnom)

# join climate average by month and adjust with anomalies
Dat_anom <- Dat_anom %>%
  mutate(month = month(date),
         year = year(date)) %>%
  left_join(clim_av, by = "month") %>%
  mutate(temp_centigrade = temp_centigrade.x + temp_centigrade.y,
         precip_mm = precip_mm.x + precip_mm.y) %>%
  select(sample, date, month, year, temp_centigrade, precip_mm)

rm(clim_av)

# nesting data by sample at this point
Dat_main <- Dat_anom %>%
  mutate(date = as_date(date)) %>% # should have done this earlier
  group_by(sample) %>%
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

# bind rows to historical climate data
Dat_main <- Dat_main %>%
  mutate(data_full = data_reg %>%
           map(function(df){
             bind_rows("historical" = Dat_clim %>%
                         mutate(month = month(date),
                                year = year(date)) %>%
                         select(month, year, temp_centigrade, precip_mm),
                       "simulated" = df,
                       .id = "origin")
           }))

# calculate pet using thornthwaite method
# https://upcommons.upc.edu/bitstream/handle/2117/89152/Appendix_10.pdf?sequence=3&isAllowed=y

# theoretical daylight hours for edinburgh https://www.timeanddate.com/sun/uk/edinburgh?month=2&year=2020
daylength <- tibble(month = 1:12,
                    days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
                    dayhours = c(7, 9, 11, 14, 16, 17, 16, 14, 12, 10, 8, 6),
                    daymins = c(36, 36, 55, 18, 24, 34, 56, 59, 35, 22, 11, 59),
                    daylight_hours = dayhours + daymins / 60) %>%
  select(month, days, daylight_hours)

# function required to combat annoying R 'feature' (returns NaN for negative numbers raised to non-integer powers...)
rtp <- function(x, power){
  sign(x) * abs(x) ^ (power)
}

Dat_main <- Dat_main %>%
  mutate(data_full = data_full %>%
           map(function(df){
             df %>%
               group_by(year) %>%
               mutate(I = sum(rtp(temp_centigrade / 5, 1.514)),
                      alpha = 675*10^-9 * rtp(I, 3) - 771*10^-7 * rtp(I, 2) + 1792*10^-5 * I + 0.49239,
                      pet_mm = rtp(16 * ((10 * temp_centigrade) / I), alpha)) %>%
               ungroup() %>%
               left_join(daylength, by = "month") %>%
               mutate(pet_mm = pet_mm * daylight_hours / 12 * days / 30,
                      pet_mm = ifelse(pet_mm < 1, 1, pet_mm)) %>% # prevents errors with negative PET/div by zero
               select(-I, -alpha, -days, -daylight_hours)
           }))

# another quick and dirty fix for potential errors - occasional negative precipitation values
Dat_main <- Dat_main %>%
  mutate(data_full = data_full %>%
           map(function(df){
             df %>%
               mutate(precip_mm = ifelse(precip_mm < 0, 0, precip_mm))
           }))

# write out main data
write_rds(Dat_main, "non-model-data/not-for-github/bush-estate-1901-2097-climvars-500-samples.rds")

# trim to 1990-2070 on for student tutorial
Dat_main %>%
  select(sample, data_full) %>%
  mutate(data_full = data_full %>%
           map(function(df){
             df %>%
               filter(year >= 1980,
                      year <= 2070)
           })) %>%
  write_rds("non-model-data/not-for-github/bush-estate-1980-2070-climvars-500-samples.rds")

# don't run unless needed, takes a minute
#Dat_main %>%
#  unnest(cols = data_full) %>%
#  filter(year >= 1980,
#         year <= 2070) %>%
#  mutate(yearmonth = year + (month - 0.5) / 12) %>%
#  ggplot(aes(x = yearmonth, y = precip_mm, group = sample)) +
#  geom_line(alpha = 0.01)


