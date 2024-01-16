library(sf)
bra <- st_read("C:/Users/musimundo/Documents/Proy.gbif/gbif/base_limpia/bra1.shp")
bo<- st_read("C:/Users/musimundo/Documents/Proy.gbif/gbif/base_limpia/bo.shp")
shp <- st_read("C:/Users/musimundo/Documents/Proy.gbif/gbif/base_limpia/sha2.shp")

df1<- read.csv("D:/Proy.gbif/gbif/raw_data12/GBIF.csv" , head(T))
df2<-read.csv("D:/Proy.gbif/gbif/raw_data13/GBIF.csv" , head(T))

shp <- rbind(df1,df2)

shp <- rbind(shp,arg)
st_write(shp , dsn = "C:/Users/musimundo/Documents/Proy.gbif/gbif/base_limpia/sha2.shp",layer="sha2", driver="ESRI Shapefile")

spp <- st_read("C:/Users/musimundo/Documents/Proy.gbif/gbif/base_limpia/sha1.shp")


#######UNIR COLUMNAS
#######

data <- read.csv("D:/Paper Biodiversidad Urbana/pred.csv", header = T, sep = ";")
library(data.table)
library(tidyverse)
library(sf)
mydf <- setDT(data)
res <- mydf[, div := diversity(mydf[, 1:31], 'shannon')]

base <- st_read("D:/Proy.gbif/Analisis/predict.shp")

base$id <-1:nrow(base)
res$id <-1:nrow(res)

Zones = left_join(base, res , by = "id",copy = TRUE)
st_write(Zones  , dsn = "D:/Paper Biodiversidad Urbana/pred.shp",layer="pred", driver="ESRI Shapefile")



######## unir filas de todos los csv de carpeta
######## 
library("dplyr")                                    # Load dplyr package
library("plyr")                                     # Load plyr package
library("readr")                                    # Load readr package

myMergedData <- 
  do.call(rbind,
          lapply(list.files(path = "D:/Tesis/paper_migraciones/occurrences"), read.csv))

data_all <- list.files(path = "D:/Tesis/paper_migraciones/occurrences",  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data_all                                            # Print data to RStudio console

data_all = unique(data_all)
as.data.frame(data_all)                            # Convert tibble to data.frame
