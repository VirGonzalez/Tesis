require(data.table)
LS2013<-fread("C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/especies_GBIF/0313451-210914110416597.csv",header = T)
LSsf = st_as_sf(cbind.data.frame(LS2013,geojsonsf::geojson_sf(LS2013$.geo)))
plot(LSsf)
library(sqldf)
iris2 <- read.csv.sql("C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/especies_GBIF/0313451-210914110416597.csv")  

library(tidyverse)
gbif_download_key = "0313451-210914110416597"
library(rgbif)
GBIF = occ_download_get(key=gbif_download_key, overwrite = TRUE) %>% occ_download_import
library(rgeos)
library(maptools)
library(proj4)
library(data.table)
library(rgdal)
library(dplyr)
library(raster)
shpfile <- "C:/Users/pc/Dropbox/PC/Documents/Doctorado/gbif.shp"
#'Filter georeferenced records by shapefile
#'
#' @param shapefile A shapefile object (.shp)
#' @param occurrence_df A data frame of georeferenced records. For large csv use fread()
#' @param lat The column name for latitude in the data frame
#' @param lon The column name for longitude in the data frame
#' @param gbifid A unique record key
#' @param map_crs The datum assigned to the occurrence data frame
#' @param mkplot Whether to draw the map plots. Can be very expensive
occurrence.from.shapefile <- function(shapefile, occurrence_df, lat, lon, gbifid, map_crs = "+proj=longlat +datum=WGS84", mkplot = FALSE){
  
  shape <- readOGR(shapefile)
  print(proj4string(shape))
  
  #subset the GBIF data into a data frame
  occ.map <- data.frame(occurrence_df[[lon]], occurrence_df[[lat]], occurrence_df[[gbifid]])
  print(str(occ.map, 1))
  #simplify column names
  names(occ.map)[1:3] <- c('long', 'lat', 'gbifid')
  print(head(occ.map, 10))
  #turning the data frame into a "spatial points data frame"
  coordinates(occ.map) <- c("long", "lat")
  #defining the datum 
  proj4string(occ.map) <- CRS(map_crs)
  #reprojecting the 'gbif' data frame to the same as in the 'shape' object 
  occ.map <- spTransform(occ.map, proj4string(shape))
  
  #Identifying records from gbif that fall within the shape polygons
  inside <- occ.map[apply(gIntersects(occ.map, shape, byid = TRUE), 2, any),]
  if(mkplot){
    raster::plot(shape)
    points(inside, col = "olivedrab3")
  }  
  
  #Prepare data frame for joining with the occcurrence df so only records 
  #that fall inside the polygons get selected 
  res.gbif <- data.frame(inside@data)
  final.gbif <- gbif %>% semi_join(res.gbif, by = c(gbifid = "gbifid"))
  
  return(final.gbif)
}  

shpfile <- readOGR( "gbif.shp", "gbif")
utm18nCRS <- crs(shpfile)
gbif <- SpatialPointsDataFrame(GBIF[,22:23],GBIF,    #the R object to convert
                              proj4string = utm18nCRS)   # assign a CRS 
plot(gbif)
library(raster)
shapefile(gbif, filename='C:/Users/pc/Dropbox/PC/Documents/Doctorado/gbif.shp')

kml_layer.SpatialPoints(gbif)

foo <- writeOGR(gbif, 'C:/Users/pc/Dropbox/PC/Documents/Doctorado/gbif.shp' , "gbif", driver="ESRI Shapefile")
#hago remuestreo eligiendo 100 puntos que me calcule por poligono proporcion de exoticas 
p = shapefile('C:/Users/pc/Dropbox/PC/Documents/Doctorado/NTL/gbif_sample.shp')

grid = shapefile('C:/Users/pc/Dropbox/PC/Documents/Doctorado/NTL/cuadricula.shp')
library("raster")
library("sp")
library(sf)
library(dplyr)
library(GISTools)
plot(grid)
plot(p, add = TRUE)
# Count points within each polygon


df = data.frame(counts)
setwd("C:/temp")
write.table(df, file = "dataframe.csv", sep = ",", col.names = NA) 
counts = poly.counts(p,grid)
# For fun: Compute densities and map them in a choropleth map
choropleth(grid,counts/poly.areas(grid))

ex <- p[p$establishm == "Native"| is.na(p$establishm) ,]
plot(ex, pch=20)
counts = poly.counts(p,grid)
counts2 = poly.counts(ex,grid)
prop_ex = counts2/counts
prop_ex[is.nan(prop_ex)]<-NA
df = data.frame(prop_ex, counts)
setwd("C:/temp")
write.table(df, file = "dataframe1.csv", sep = ",", col.names = NA) 
choropleth(grid, counts/poly.areas(grid))


