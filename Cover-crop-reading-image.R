library(digitize)
library(tidyverse)


cal <- ReadAndCal(project_data(path = "SCS-measures/cc_yield_image.png"))

data <-DigitData(col = 'red')

data_est <- Calibrate(data, cal, "Control", "Mix 3", 0.00, 3.00)

  

