file_list <- list.files("C:/Users/pc/Dropbox/PC/Documents/Doctorado/NTL/PCA/EV_andes/OPM", pattern = "*shp", full.names = TRUE)
library(sf)
library(raster)
ff <- list.files("C:/Users/pc/Dropbox/PC/Documents/Doctorado/NTL/PCA/EV_andes/OPM", pattern="\\.shp$", full.names=TRUE)
shapefile_list <- lapply(ff, shapefile)
x = rbind(shapefile_list)

library(rgdal)
writeOGR(shapefile_list, dsn = "C:/Users/pc/Dropbox/PC/Documents/Doctorado/NTL/PCA/EV_andes/OPM" , layer = "oi", driver = "ESRI Shapefile")
 