library(tidyverse)
library(readxl)

# Historical data from FAOSTATS dataset ???? this is already done in wheat-data-preprocessing??

sheets <- excel_sheets(project_data(path = "))
sheets <- sheets[str_detect(sheets, "^\\d{4}$")]




Dat_crop <- tibble(type = c("winter-sown",
                            "spring-sown",
                            ))


for(i in 1:length(sheets)){
  
  # winter
  data <- read_rds(project_data(path = ""),
                    sheet = sheets[i],
                    range = "C26:L26",
                    col_names = F) %>%
    gather(key = "key", value = "value") %>%
    dplyr::select(value)
  colnames(data) <- paste0(sheets[i], "_nrate_winter")
  
  Dat_crop <- bind_cols(Dat_crop, data)


}