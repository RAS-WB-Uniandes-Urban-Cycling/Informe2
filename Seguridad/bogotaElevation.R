library(sf)
library(raster)

setwd("/Users/germancarvajal/Downloads/")

data <- raster("dtm2014/DTM_Catastro_EPSG4686.tif")

plot(data)

minValue(data)

maxValue(data)
