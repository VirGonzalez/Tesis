#reclasificar raster
library(raster)
r1 = raster("ucra_sa.tif")

rc <- function(x1) {
  rc <- function(x1) { 
    ifelse(x1 %in% c('1','8', '15', '22') , 5 ,
           ifelse(x1 %in% c( '2', '9', '16', '23') , 4 , 
                  ifelse(x1 %in% c('3','4','10', '11', '17', '18','24', '25')  ,3 ,
                         ifelse( x1 %in% c('5','6','12','13','19','20','26','27'), 2,
                                 ifelse(x1 %in% c('7','14','21','28') , 1, NA)))))
  }
}

r.class <- overlay(r1, fun=rc)
plot(r.class)
#GUARDO RASTER
getwd()

outfile <- writeRaster(r.class , filename='D:/Proy.gbif/Raster/CiudadesClass.tif', format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
r1 = raster("C:/Users/pc/Documents/Doctorado/CiudadesClass.tif")
library(sf)
v = st_read("D:/Proy.gbif/Raster/CiudadesClass.tif/predict.shp")
library(terra)
library(exactextractr)
library(dplyr)
#extract the area of each cell that is contained within each polygon
x <- exact_extract(r1, v, coverage_area = TRUE)
#add polygon names that the results will be grouped by
names(x) <- v$NAME_2
#bind the list output into a df and calculate the proportion cover for each category
test <- bind_rows(x, .id = "ID_1") %>%
  group_by(ID_1, value) %>%
  summarize(total_area = sum(coverage_area)) %>%
  group_by(ID_1) %>%
  mutate(proportion = total_area/sum(total_area))
test$ID_1 = as.numeric(test$ID_1)
library(tidyverse)
joined_data <- left_join(v, test , by = "ID_1")
plot(joined_data)
st_write(joined_data , dsn = "C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/Aves/plantas1.shp",layer="plantas1", driver="ESRI Shapefile" )


#clasifico UN VECTOR de acuerdo de cada clase
library(raster)
x1 = shapefile("D:/Paper Biodiversidad Urbana/predict.shp")
x1$total <- x1$hNODATA + x1$h1 + x1$h2 + x1$h3 + x1$h4 + x1$h5
x1$clase1 <- x1$h1/x1$total*100
x1$clase2 <- x1$h2/x1$total*100
x1$clase3 <- x1$h3/x1$total*100
x1$clase4 <- x1$h4/x1$total*100
x1$clase5 <- x1$h5/x1$total*100
x1$clase <- x1$h1/x1$total*100

x1$tipo <- ifelse(x1$clase1 >= 1 , 'Metropoli' ,
                  ifelse(x1$clase2 >= 1 , 'Grandes Ciudades' ,  
                         ifelse(x1$clase3 >= 1 , 'Ciudad intermedia' ,
                                ifelse( x1$clase4 >= 1 , 'Ciudad Pequeña' ,
                                        ifelse(x1$clase5 >= 2.5 , 'Towns' , NA)))))

data$tipo2 <- ifelse(data$d1 == 'Metropoli' ,1,
                  ifelse(data$d1 == 'Grandes Ciudades' , 2, 
                         ifelse(data$d1 == 'Ciudad intermedia' ,3,
                                ifelse( data$d1 == 'Ciudad Pequeña' ,4, 
                                        ifelse(data$d1 == 'Towns' , 5, NA )))))

data$tipo2[is.na(data$tipo)]="6"
plot(data$tipo)

library(rgdal)
writeOGR(x1 , 'D:/Paper Biodiversidad Urbana', driver = 'ESRI Shapefile', layer = 'pred3', overwrite_layer = T)
writeOGR(x1 , 'D:/Paper Biodiversidad Urbana', driver = 'ESRI Shapefile', layer = 'pred3', overwrite_layer = T)