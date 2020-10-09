
library(raster)
library(tidyverse)

# NUTS region shapefile
Shp_nuts <- shapefile(project_data("GIS-data/EGM_2019_SHP_20190312/DATA/Countries/GB/NUTS_3.shp"))

# model data
Dat_nest <- read_rds(project_data("project-data/model-data-input-small-sample-wheat-manure-data.rds"))

# tillage data
till_conv <- readxl::read_xlsx(project_data("SCS-measures/Tillage_practice_statistics_2016_eurostat.xlsx"),
                               sheet = "Map 1",
                               range = "B243:F282",
                               col_names = F) %>%
  select(nuts_short = `...1`, region = `...2`, frac_conv = `...5`)

till_cons <- readxl::read_xlsx(project_data("SCS-measures/Tillage_practice_statistics_2016_eurostat.xlsx"),
                               sheet = "Map 2",
                               range = "B243:F282",
                               col_names = F) %>%
  select(nuts_short = `...1`, region = `...2`, frac_cons = `...5`)

till_zero <- readxl::read_xlsx(project_data("SCS-measures/Tillage_practice_statistics_2016_eurostat.xlsx"),
                               sheet = "Map 3",
                               range = "B243:F282",
                               col_names = F) %>%
  select(nuts_short = `...1`, region = `...2`, frac_zero = `...5`)

# combine tillage data
Dat_till <- till_conv %>%
  left_join(till_cons, by = c("nuts_short", "region")) %>%
  left_join(till_zero, by = c("nuts_short", "region"))

# reformat
Dat_till <- Dat_till %>%
  mutate_at(vars(frac_conv:frac_zero), ~as.numeric(.) / 100)

# fix NAs and assume conventional tillage where unknown (mostly cities -- but we don't want NAs)
Dat_till <- Dat_till %>%
  mutate_at(vars(frac_conv:frac_zero), ~replace_na(., 0)) %>%
  mutate(frac_conv = 1 - frac_cons - frac_zero)

rm(till_conv, till_cons, till_zero)

# add abbreviated short NUTS code to match tillage data
# checked manually that this matches correctly!
Shp_nuts$nuts_short <- Shp_nuts$NUTS_CODE %>% str_extract("[:upper:]{3}[:digit:]{1}")

# replace the NUTS codes that some fucker has changed...
Shp_nuts$nuts_short <- ifelse(Shp_nuts$nuts_short == "UKM7", "UKM2", Shp_nuts$nuts_short)
Shp_nuts$nuts_short <- ifelse(Shp_nuts$nuts_short == "UKM8", "UKM3", Shp_nuts$nuts_short)
Shp_nuts$nuts_short <- ifelse(Shp_nuts$nuts_short == "UKM9", "UKM3", Shp_nuts$nuts_short)

# join
Shp_nuts$frac_conv <- Dat_till$frac_conv[match(Shp_nuts$nuts_short, Dat_till$nuts_short)]
Shp_nuts$frac_cons <- Dat_till$frac_cons[match(Shp_nuts$nuts_short, Dat_till$nuts_short)]
Shp_nuts$frac_zero <- Dat_till$frac_zero[match(Shp_nuts$nuts_short, Dat_till$nuts_short)]

# mismatches check
tibble(Shp_nuts$NUTS_CODE, code = Shp_nuts$nuts_short, Shp_nuts$NUTS_LABEL, frac_conv = Shp_nuts$frac_conv) %>%
  filter(is.na(frac_conv)) %>% 
  arrange(code)
# none

# reproject shapefile to WGS84
Shp_nuts <- spTransform(Shp_nuts, CRS("+proj=longlat +datum=WGS84"))
 
# conversion of Dat_nest coords to spatial points
coords <- Dat_nest %>%
  select(x, y, DA)
coordinates(coords) <- ~x + y
proj4string(coords) <- CRS(proj4string(Shp_nuts))

# spatial extraction
till_extract <- over(coords, Shp_nuts) %>%
  as_tibble()

# join to Dat_nest
# verified order is correct
Dat_nest <- Dat_nest %>%
  bind_cols(till_extract %>%
              select(starts_with("frac_"))
            )

# stochastic tillage type selection
set.seed(2605)
Dat_nest <- Dat_nest %>%
  mutate(selector = runif(nrow(Dat_nest)),
         till_type = case_when(
           selector <= frac_conv ~ "full",
           selector > frac_conv & selector <= frac_conv + frac_cons ~ "reduced",
           selector > frac_conv + frac_cons & selector <= frac_conv + frac_cons + frac_zero ~ "zero"
         )) %>%
  select(-(frac_conv:selector)) %>%
  mutate(till_type = till_type %>% replace_na("full")) # a few with missing values (not interected by polygons)

# annual change in tillage practices
frac_change <- readxl::read_xlsx(project_data("SCS-measures/Tillage_practice_statistics_2016_eurostat.xlsx"),
                                 sheet = "Figure 5",
                                 range = "C38:E38",
                                 col_names = F) %>%
  rename(conv = `...1`, cons = `...2`, zero = `...3`) %>%
  mutate_all(~. / (6 * 100)) %>% # 6-year reporting period, fraction from percentage
  as.list()

# convert to time series in nested data

# funs for cells depending on original tillage type
zero_till <- function(data) {
  data %>%
    mutate(till_type = "zero")
}

reduced_till <- function(data) {
  past <- nrow(data %>% filter(origin == "historic"))
  future <- nrow(data %>% filter(origin == "simulated"))
  
  selector <- runif(future)
  changeyear <- which(selector <= frac_change$zero) %>% min()
  if (is.infinite(changeyear)) changeyear <- future
  
  till_ts <- c(rep("reduced", past),
                 rep("reduced", changeyear),
                 rep("zero", future - changeyear))
  
  data <- data %>%
    mutate(till_type = till_ts)
  
  return(data)
}

full_till <- function(data) {
  past <- nrow(data %>% filter(origin == "historic"))
  future <- nrow(data %>% filter(origin == "simulated"))
  
  selector <- runif(future)
  changeyear <- which(selector <= frac_change$cons) %>% min()
  if (is.infinite(changeyear)) changeyear <- future
  
  till_ts <- c(rep("full", past),
               rep("full", changeyear),
               rep("reduced", future - changeyear))
  
  data <- data %>%
    mutate(till_type = till_ts)
  
  return(data)
}

# construct stochastic time series in Dat_nest
Dat_nest <- Dat_nest %>%
  mutate(data = map2(data, till_type,
                     function(data, till_type) {
                       if (till_type == "full") data <- full_till(data)
                       if (till_type == "reduced") data <- reduced_till(data)
                       if (till_type == "zero") data <- zero_till(data)
                       return(data)
                     }))

# can be used to check performance of stochastic ts functions
#Dat_nest %>%
#  filter(till_type == "reduced") %>%
#  mutate(test = map_dbl(data, function(x){
#    x %>%
#      filter(till_type == "zero") %>%
#      pull(till_type) %>%
#      length()
#  })) %>%
#  filter(test > 0) %>%
#  View()

# remove till_type from outer df
Dat_nest <- Dat_nest %>%
  select(-till_type)

# residues
# based on information from Carmen M -- residue removal between half and two-thirds
Dat_nest <- Dat_nest %>%
  mutate(data = data %>%
           map(function(data) {
             data %>%
               mutate(frac_remove = runif(nrow(data), min = 0.5, max = 0.666))
           }))

# save new df
write_rds(Dat_nest, project_data("project-data/model-data-input-small-sample-wheat-manure-tillage-data.rds"))
  