
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)


## data_repo <- "GIS data repository" access to the GIS data created by the script's author --Alasdair Sykes--

# read netCDF and check out variables
Ncdf_tasAnom <- nc_open(project_data(path = "GIS-data/UKCP/tasAnom_rcp45_land-prob_uk_country_sample_b8100_1y_mon_19601201-20991130.nc"))
Ncdf_tasAnom

Ncdf_prAnom <- nc_open(project_data(path = "GIS-data/UKCP/prAnom_rcp45_land-prob_uk_country_sample_b8100_1y_mon_19601201-20991130.nc"))
Ncdf_prAnom

# extract names / numbers of regions for later use
regions <- tibble(region = ncvar_get(Ncdf_tasAnom, "region"),
                  region_chr = ncvar_get(Ncdf_tasAnom, "geo_region") %>% str_replace_all("\\s+", ""))

# read in netCDF as brick, convert to dataframe, join regions by name and reshape
# this is time-consuming!
Dat_tasAnom <- brick(project_data(path = "GIS-data/UKCP/tasAnom_rcp45_land-prob_uk_country_sample_b8100_1y_mon_19601201-20991130.nc")) %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  rename(sample = x, region = y) %>%
  gather(-sample, -region, key = "date", value = "tasAnom") %>%
  mutate(date = date %>%
           str_replace("X", "") %>%
           ymd_hms()) %>%
  left_join(regions, by = "region") %>%
  select(-region)

Dat_prAnom <- brick(project_data(path = "GIS-data/UKCP/prAnom_rcp45_land-prob_uk_country_sample_b8100_1y_mon_19601201-20991130.nc")) %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  rename(sample = x, region = y) %>%
  gather(-sample, -region, key = "date", value = "prAnom") %>%
  mutate(date = date %>%
           str_replace("X", "") %>%
           ymd_hms()) %>%
  left_join(regions, by = "region") %>%
  select(-region)

# write out full data in this format
write_rds(Dat_tasAnom, project_data(path = "GIS-data/UKCP/tasAnom-b8100-1960-2099-regional-3000sample-reshaped.rds"))
write_rds(Dat_prAnom, project_data(path = "GIS-data/UKCP/prAnom-b8100-1960-2099-regional-3000sample-reshaped.rds"))
