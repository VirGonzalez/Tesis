library(sf)
exoticas = st_read("D:/Tesis/paper_migraciones/occurrences/tree_exotic.shp")
nativo = st_read("D:/Tesis/paper_migraciones/occurrences/tree_nativeSA.shp")
nat = read.csv("D:/Tesis/paper_migraciones/global_tree_search_trees_1_6.csv" , header = T, sep = ",")
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



nativa_1 <- nativo %>% filter(nativo$searchName %in% data )

st_write(nativa_1 , dsn = "D:/Paper Biodiversidad Urbana/arb_nativaGTS.shp",layer="predict", driver="ESRI Shapefile")

