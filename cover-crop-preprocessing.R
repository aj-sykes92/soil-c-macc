
library(tidyverse)
library(raster)

gisdata_repo <- "GIS data repository"
projdata_repo <- "Soils-R-GGREAT/UK Soil C MACC/project-data"

# calculate fraction of wheat area suitable for cover cropping
Ras_clay <- find_onedrive(dir = gisdata_repo, path = "SoilGrids250/CLYPPT_M_sl4_250m_ll.tif") %>% raster()
Ras_wheatarea <- find_onedrive(dir = gisdata_repo, path = "MapSPAM data/Physical area/phys_area_wheat.tif") %>% raster()
Shp_UK <- shapefile(find_onedrive(dir = gisdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp"))

Ras_clay <- Ras_clay %>% crop(Shp_UK) %>% mask(Shp_UK)
Ras_wheatarea <- Ras_wheatarea %>% crop(Shp_UK) %>% mask(Shp_UK)

plot(Ras_clay)

hist(Ras_clay)

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
                   quqnt = (date - 1961) / (2097 - 1961))
                   uptake_frac = pnorm(q = quant, mean = 0.5, sd = 1))
     


Dat_prob <- tibble(date = 1961:2097,
                   uptake_frac = dnorm(x = date, mean = 2023, sd = 6)) %>%
  mutate(uptake_frac = cumsum(uptake_frac),
         uptake_frac = uptake_frac * 0.7 + 0.015)


ggplot() +
  geom_smooth(data = Dat_cc, aes(x = date, y = uptake_frac)) +
  geom_line(data = Dat_prob, aes(x = date, y = uptake_frac))

library(raster)

Ras_clay <- find_onedrive(dir = projdata_repo, path = )



