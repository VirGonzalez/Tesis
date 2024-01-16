getwd()
setwd("C:/Users/pc/Dropbox/PC/Documents/IsocronasEV")
library(raster)
cor <- shapefile("")
proj4string(cor) <- CRS("+proj=tmerc +lat_0=-90 +lon_0=-66 +k=1 +x_0=3500000 +y_0=0 +ellps=WGS84
+towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")

library(rgdal)
library(sf)
writeOGR(cor, dsn = "C:/Users/pc/Dropbox/PC/Documents/IsocronasEV" , layer = "nameofmynewmap", driver = "ESRI Shapefile")

file_list <- list.files("C:/Users/pc/Dropbox/PC/Documents/IsocronasEV", pattern = "*shp", full.names = TRUE)
file_list<- ogrListLayers("C:/Users/pc/Dropbox/PC/Documents/IsocronasEV")
files<-sub(".shp","",file_list)
require(rgeos)
require(plyr)
ff <- list.files("C:/Users/pc/Dropbox/PC/Documents/IsocronasEV/isocronas", pattern="\\.shp$", full.names=TRUE)
eshapefile_list <- lapply(ff ,st_read)
combinedShp <- do.call(what = sf:::rbind.sf, args=eshapefile_list)

writeOGR(x, dsn = "C:/Users/pc/Dropbox/PC/Documents/IsocronasEV" , layer = "parque", driver = "ESRI Shapefile")

st_write(combinedShp, dsn = "C:/Users/pc/Dropbox/PC/Documents/IsocronasEV/isocronas1.shp",layer="isocronas1", driver="ESRI Shapefile")
#UNIR ARCHIVOS DE ACUERDO A UN PATRON 
masterlist =list.files("//directory/",pattern="^name1")
listfiles <- paste("//directory/",masterlist, sep="")

#concatenate files into one file
con_files = c(listfiles)

lasread <- readLAS(con_files)
lasmerge <- rbind(eshapefile_list)
writeLAS(lasmerge, "//write/directory/name1.las")


path <- "C:/Users/pc/Dropbox/PC/Documents/IsocronasEV"
write_path <- "C:/Users/pc/Dropbox/PC/Documents/IsocronasEV"
patterns <- list.files(path, pattern = ".*\\.las")
patterns <- strsplit(patterns, "_")
patterns <- unique(sapply(patterns, function(x) x[1]))

for(p in patterns) {
    masterlist <- list.files(path, pattern = paste0("^", p))
    listfiles <- paste(path, masterlist, sep = "")
    
    #concatenate files into one file
    con_files = c(listfiles)
    
    lasread <- readLAS(con_files)
    lasmerge <- rbind(lasread)
    writeLAS(lasmerge, file.path(write_path, paste0(p, ".las")))
}

#estandarizar indice eze
library(sf)
base = st_read("C:/Users/pc/Dropbox/PC/Documents/accesibilidad.shp")
base$indice_est = (base$indice - min(base$indice))/(max(base$indice, na.rm = TRUE) - min(base$indice, na.rm = TRUE))
st_write(base, dsn = "C:/Users/pc/Dropbox/PC/Documents/accesibilidad2.shp",layer="accesibilidad2", driver="ESRI Shapefile")
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = 5, name = "RdYlGn")
ggplot(data = base )  +geom_sf(aes(fill= indice_est ), color = NA) +xlab("Long") + ylab("Lat")+ scale_fill_gradientn(name= "Predicts", 
                                                                                                               colours= my.palette, na.value="grey" )+
  annotation_scale()  +
  annotation_north_arrow(location='tr')+theme_classic()+
  theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.))) 
library(sf)
base = st_read("C:/Users/pc/Dropbox/PC/Documents/IsocronasEV/parque1.shp")
library(dplyr)
result <- base %>% 
  group_by(group = paste(st_intersects(base, base, sparse = T))) %>% 
  summarise(minutos = min(minutos))
st_write(result, dsn = "C:/Users/pc/Dropbox/PC/Documents/IsocronasEV/parque2.shp",layer="parque2", driver="ESRI Shapefile")
plot(result)
