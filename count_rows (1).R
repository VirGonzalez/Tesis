library(sf)
shape <- st_read("C:/Users/musimundo/Documents/Tesis/paper_migraciones/centroides.shp")
library(dplyr)
str(shape)
as.factor(shape$JOIN_ID)
shape$MIGIJ_Fest<- as.numeric(shape$MIGIJ_Fest)
shape$MIGIJ_Mest<- as.numeric(shape$MIGIJ_Mest)

list <- unique(shape$id_i)



dat <- shape %>% 
  group_by(id_j) %>%
  summarise(mig_f = sum(MIGIJ_Fest))  

plot(dat)


st_write(dat , dsn = "C:/Users/musimundo/Documents/Tesis/paper_migraciones/INMIG_FEM.shp",layer="inmigraciones", driver="ESRI Shapefile")

length(which(duplicated(dat$geometry)))

############ flujo de personas

shape <- st_read("C:/Users/musimundo/Documents/Tesis/paper_migraciones/centroides.shp")

table <- read.csv("C:/Users/musimundo/Documents/Tesis/paper_migraciones/MigrEst_internal_v4.csv" , header = TRUE, sep = ",")

