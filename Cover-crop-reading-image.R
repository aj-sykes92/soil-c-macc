library(digitize)
library(bmp)



cc_yield_image <- readbitmap::read.bitmap(project_data(path = "SCS-measures/cover-crop-yield.bmp"))

str(cc_yield_image)

digitize(project_data(path = "SCS-measures/cover-crop-yield.bmp"))

