library(raster)               # spatial raster data reading and handling
library(sf)                   # spatial vector data reading and handling
library(dplyr)                # data manipulation
library(tidyr)                # data manipulation
library(tmap)                 # spatial viz
library(geofacet)             # geofacet
library(ggplot2)    

if(!require(rgdal))            suppressMessages( suppressWarnings( install.packages("rgdal") ))            # para manipulación y visualización de datos geoespaciales"

if(!require(sp))               suppressMessages( suppressWarnings( install.packages("sp") ))               # para trabajar con datos vectoriales, debiera cargar junto al paquete raster
if(!require(landscapemetrics)) suppressMessages( suppressWarnings( install.packages("landscapemetrics") )) # Para cálculo de métricas espaciales
if(!require(landscapetools))   suppressMessages( suppressWarnings( install.packages("landscapetools") ))   # Herramientas para manipulación y visualización de rasters (esto es opcional)
if(!require(rlang))            suppressMessages( suppressWarnings( install.packages("rlang") ))            # Paquetes complementarios para compatibilidad entre códigos y datos en R
if(!require(glue))             suppressMessages( suppressWarnings( install.packages("glue") ))             # Paquetes complementarios para compatibilidad entre códigos y datos en R

###ABRO RASTER
ciud <- raster("Tesis/PCA/mancha_urb.tif")
grid <- st_read("Tesis/PCA/EV_andes/prit.shp")


check_landscape(ciud) #Para evaluar si el raster cumple con los criterios para poder ser analizado. OJO: el CRS del raster debe estar en metros!
str(ciud)

Paisaje_reproject <- projectRaster(ciud, crs = "+init=epsg:22183", method="ngb") #Reproyectamos con la opción "nearestneighbor"
check_landscape(Paisaje_reproject) #Con este cambio ahora el resultados debiese ser OK="v"
class(Paisaje_reproject) #Con este cambio ahora el resultados debiese ser OK="v"
library(sf)
grid <- st_read("Tesis/PCA/ai.shp")

Paisaje_1 <- crop(Paisaje_reproject, grid[1,]) #Ahora cortamos nuestro raster al tamaño del primero polígono
plot(Paisaje_1, main = "Paisaje 1") #Con este comando vemos el resultado de nuestro nuevo "pedazo" de paisaje

#The calculation of landscape metrics for each cell
my_metric = sample_lsm(Paisaje_reproject , grid,
                       level = "landscape", metric = "ent")
my_metric

grid$plot_id = seq_len(nrow(grid))

###Mido un indice de agregacion

my_metric1 = sample_lsm(Paisaje_reproject , grid,
                        level = "landscape", metric = "ai")
my_metric1


my_grid1 = left_join(grid, my_metric1, by = "plot_id")

plot(my_grid1["value"])

st_write(my_grid1 , dsn = "C:/Users/musimundo/Documents/Tesis/PCA/ai2.shp",layer="ai2.shp", driver="ESRI Shapefile")


grd = grid_auto(grid, names = "plot_id")

rm(list=ls())

##Calculo total core area

my_metric1 = sample_lsm(Paisaje_reproject , grid,
                        level = "landscape", metric = "tca")
my_metric1

st_write(my_grid1 , dsn = "C:/Users/musimundo/Documents/Tesis/PCA/ai2.shp",layer="ai2.shp", driver="ESRI Shapefile")
