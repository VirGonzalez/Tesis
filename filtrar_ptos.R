library(sf)
exoticas = st_read("cattle/lim_prov.shp")
nativo = st_read("D:/Tesis/paper_migraciones/occurrences/tree_nativeSA.shp")
nat = read.csv("cattle/pcia_andes.csv" , header = T, sep = ",")
library("dplyr")                                    # Load dplyr package
library("plyr")                                     # Load plyr package
library("readr")                                    # Load readr package
ex = list.files(path = "D:/Tesis/paper_migraciones/occurrences",  # Identify all CSV files
                pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set                                           # Print data to RStudio console

data_all = unique(ex)
data_all
data = data_all$taxon
spp = nat$TaxonName

spp = setdiff(spp, data)
n_2 = setdiff(data , unique(nativa_1$searchName))

data = nat$Provincia.equivalente
data = as.vector(data)

nativa_1 <- exoticas %>% filter(NAME_1 %in% data)

st_write(nativa_1 , dsn = "cattle/lim3.shp",layer="lim3", driver="ESRI Shapefile")

