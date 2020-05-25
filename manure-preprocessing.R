
library(tidyverse)
library(readxl)

# part 1
# sheets to read from BSFP manure dataset
sheets <- excel_sheets("Manure-application-rates.xlsx")
sheets <- sheets[str_detect(sheets, "^\\d{4}$")]

sheets = sheets[sheets != "2007"] # data incomplete

# empty df for BSFP data
Dat_man <- tibble(type = c("cattle_fym",
                           "cattle_slurry",
                           "pig_fym",
                           "pig_slurry",
                           "layer",
                           "broiler",
                           "other_fym",
                           "other_farm",
                           "biosolids",
                           "other_nonfarm"))

# very bush league but whatever
for(i in 1:length(sheets)){
  
  # winter
  data <- read_xlsx("Manure-application-rates.xlsx",
            sheet = sheets[i],
            range = "C26:L26",
            col_names = F) %>%
    gather(key = "key", value = "value") %>%
    dplyr::select(value)
  colnames(data) <- paste0(sheets[i], "_nrate_winter")
  
  Dat_man <- bind_cols(Dat_man, data)
  
  # spring
  data <- read_xlsx("Manure-application-rates.xlsx",
                    sheet = sheets[i],
                    range = "C31:L31",
                    col_names = F) %>%
    gather(key = "key", value = "value") %>%
    dplyr::select(value)
  colnames(data) <- paste0(sheets[i], "_nrate_spring")
  
  # grass
  Dat_man <- bind_cols(Dat_man, data)
  
  data <- read_xlsx("Manure-application-rates.xlsx",
                    sheet = sheets[i],
                    range = "C36:L36",
                    col_names = F) %>%
    gather(key = "key", value = "value") %>%
    dplyr::select(value)
  colnames(data) <- paste0(sheets[i], "_nrate_grass")
  
  Dat_man <- bind_cols(Dat_man, data)
  
  # treated area winter
  data <- read_xlsx("Manure-application-rates.xlsx",
                    sheet = sheets[i],
                    range = "C6:L6",
                    col_names = F) %>%
    gather(key = "key", value = "value") %>%
    dplyr::select(value)
  colnames(data) <- paste0(sheets[i], "_treatfrac_winter")
  
  Dat_man <- bind_cols(Dat_man, data)
  
  # treated area spring
  data <- read_xlsx("Manure-application-rates.xlsx",
                    sheet = sheets[i],
                    range = "C12:L12",
                    col_names = F) %>%
    gather(key = "key", value = "value") %>%
    dplyr::select(value)
  colnames(data) <- paste0(sheets[i], "_treatfrac_spring")
  
  Dat_man <- bind_cols(Dat_man, data)
  
  # treated area grass
  data <- read_xlsx("Manure-application-rates.xlsx",
                    sheet = sheets[i],
                    range = "C18:L18",
                    col_names = F) %>%
    gather(key = "key", value = "value") %>%
    dplyr::select(value)
  colnames(data) <- paste0(sheets[i], "_treatfrac_grass")
  
  Dat_man <- bind_cols(Dat_man, data)
  
}

# gather and clean
Dat_man <- Dat_man %>%
  gather(-type, key = "key", value = "value") %>%
  mutate(value = ifelse(is.na(value), 0, value),
         year = key %>% str_extract("^\\d{4}") %>% as.numeric(),
         metric = key %>% str_extract("(?<=_)\\w+(?=_)"),
         croptype = key %>% str_extract("(?<=_)[:alpha:]+$"))

# spread
Dat_man <- Dat_man %>%
  dplyr::select(-key) %>%
  spread(key = metric, value = value)

# part 2
# read in faostat long manure ts
Dat_fs <- read_csv("faostat-manure-ts.csv")

# quick squint
Dat_fs %>%
  filter(Year >= 2000) %>%
  group_by(Item, Element) %>%
  summarise(mean = mean(Value, na.rm = T)) %>%
  ggplot(aes(x = Item, y = mean, fill = Element)) +
  geom_col() +
  coord_flip()
    
# translation
manure_types <- read_csv("parameter-data/manure-coefficients.csv")

trans1 <- Dat_fs %>% distinct(Item)

trans1 <- trans1 %>%
  mutate(trans = c("dairy-cattle",
                  "beef-cattle",
                  "poultry",
                  "poultry",
                  "poultry",
                  "sheep",
                  "horses",
                  "sheep",
                  "swine",
                  "swine",
                  "poultry"))

trans2 <- Dat_man %>% distinct(type)

trans2 <- trans2 %>%
  mutate(trans = c("poultry",
                   "poultry",
                   "beef-cattle",
                   "dairy-cattle",
                   "poultry",
                   "dairy-cattle",
                   "sheep",
                   "horses",
                   "swine",
                   "swine"))

# translate and compress
Dat_fs <- Dat_fs %>%
  left_join(trans1, by = "Item") %>%
  filter(Element == "Manure applied to soils (N content)") %>%
  dplyr::select(trans, year = Year, kg = Value) %>%
  group_by(trans, year) %>%
  summarise(kg = sum(kg, na.rm = T))

Dat_man <- Dat_man %>%
  left_join(trans2, by = "type") %>%
  group_by(trans, year, croptype) %>%
  summarise(nrate = sum(nrate),
            treatfrac = sum(treatfrac))



# view
ggplot(Dat_fs, aes(x = year, y = kg, colour = trans, group = trans)) +
  geom_line()

ggplot(Dat_man %>% mutate(kg = nrate * treatfrac / 100), aes(x = year, y = kg, colour = trans, group = trans)) +
  geom_line() +
  facet_wrap(~ croptype, nrow = 3)
