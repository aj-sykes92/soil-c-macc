
# script to implement IPCC steady-state C model for UK wheat production
# aim is to assess applicable soil carbon sequestration measures

#####################################################
# preparation
#####################################################
# load packages
library(raster)
library(tidyverse)
library(lubridate)

# data repositories in Alasdair (the script's author), using a function(dir, path)#
#gisdata_repo <- "GIS data repository"
#projdata_repo <- "Soils-R-GGREAT/UK Soil C MACC/project-data"


# masking shapefile
Shp_UK <- project_data(path = "GIS-data/DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()

#####################################################
# process crop production area and yield data
#####################################################
Ras_wheatarea <- raster(project_data(path = "GIS-data/MapSpam data/Physical area/phys_area_wheat.tif"))
Ras_wheatyield <- raster(project_data(path = "GIS-data/MapSpam data/Yield/yield_wheat.tif"))

# crop to UK extent (rough until , for processing effiency)
Ras_wheatarea <- Ras_wheatarea %>% crop(Shp_UK)
Ras_wheatyield <- Ras_wheatyield %>% crop(Shp_UK)

# time series of UK wheat area/yields from 
Dat_wheat_ts <- read_csv(project_data(path = "project-data/faostat-uk-wheat-prod-1961-2018.csv")) %>%
  select(key = Element, year = Year, value = Value) %>%
  spread(key = key, value = value) %>%
  rename(area_kha = `Area harvested`, yield_tha = Yield) %>%
  mutate(area_kha = area_kha * 10^-3,
         yield_tha = yield_tha * 10^-4)

# normalise to 2010 (mapspam data year) to adjust raster data
Dat_wheat_ts <- Dat_wheat_ts %>%
  mutate(area_kha = area_kha / area_kha[year == 2010],
         yield_tha = yield_tha / yield_tha[year == 2010])

# loops to adjust mapspam data and create brick, by year
Brk_wheatyield <- brick()
for(i in 1:nrow(Dat_wheat_ts)){
  x <- Ras_wheatyield * Dat_wheat_ts$yield_tha[i]
  x@data@names <- paste0("yield_wheat_", Dat_wheat_ts$year[i])
  Brk_wheatyield <- Brk_wheatyield %>% addLayer(x)
  rm(x)
}

Brk_wheatarea <- brick()
for(i in 1:nrow(Dat_wheat_ts)){
  x <- Ras_wheatarea * Dat_wheat_ts$area_kha[i]
  x@data@names <- paste0("area_wheat_", Dat_wheat_ts$year[i])
  Brk_wheatarea <- Brk_wheatarea %>% addLayer(x)
  rm(x)
}

# write out raster bricks as .rds files
write_rds(Brk_wheatyield, project_data(path = "project-data/uk-wheat-yield-spatial-ts.rds"))
write_rds(Brk_wheatarea, project_data(path = "project-data/uk-wheat-area-spatial-ts.rds"))

