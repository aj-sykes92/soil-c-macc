
library(tidyverse)
library(raster)

## gisdata_repo <- "GIS data repository"
## projdata_repo <- "Soils-R-GGREAT/UK Soil C MACC/project-data"

# calculate fraction of wheat area suitable for cover cropping

Ras_clay <- project_data(path = "GIS-data/CLYPPT_M_sl4_250m_ll.tif") %>% raster()# 250m soil grids
Ras_wheatarea <- project_data(path = "GIS-data/MapSPAM data/Physical area/phys_area_wheat.tif") %>% raster()
Shp_UK <- shapefile(project_data(path = "GIS-data/DA shapefile/GBR_adm_shp/GBR_adm1.shp"))

# raster for clay % @250m, uk mask
#Ras_clay <- Ras_clay %>% crop(Shp_UK) %>% mask(Shp_UK)
# write_rds(Ras_clay, find_onedrive(dir = projdata_repo, path = "clay-sl4-sg250-uk-mask.rds"))
Ras_clay <- read_rds(project_data(path = "project-data/clay-sl4-sg250-uk-mask.rds"))

# wheat area, UK mask
Ras_wheatarea <- Ras_wheatarea %>% crop(Shp_UK) %>% mask(Shp_UK)

# data on cover crop uptake from Storr et al. (2019)
Dat_cc <- tibble(years = 1:10,
                 uptake = c(15, 16, 13, 7, 8, 1, 3, 2, 2, 11)) %>%
  mutate(date = 2017 - (years - 1)) %>%
  arrange(date) %>%
  mutate(uptake_cum = cumsum(uptake),
         uptake_frac = uptake_cum / max(uptake_cum) * 0.66 * 0.21) # 66% reported as using, covering 21% of land where used

ggplot(Dat_cc, aes(x = date, y = uptake_frac)) +
  geom_line() +
  geom_smooth()


Dat_prob <- tibble(date = 1961:2097,
                   quant = (date - 1961) / (2097 - 1961)) %>%
  mutate(uptake_frac = pnorm(q = quant, mean = 0.5, sd = 1))
     


Dat_prob <- tibble(date = 1961:2097,
                   uptake_frac = dnorm(x = date, mean = 2023, sd = 6)) %>%
  mutate(uptake_frac = cumsum(uptake_frac),
         uptake_frac = uptake_frac * 0.7 + 0.015)


ggplot() +
  geom_smooth(data = Dat_cc, aes(x = date, y = uptake_frac)) +
  geom_line(data = Dat_prob, aes(x = date, y = uptake_frac))

# Cover crop function

Cover_crop <- function(cc_species, cc_type){
  
  
}
  
  