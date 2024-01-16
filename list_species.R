remotes::install_github("ropensci/rgbif@download-format")

library(rnaturalearth)
library(rgbif)
library(dplyr)
# world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")
# country subset
CRpoly <- worldMap %>% filter(subregion == 'South America')

user<- "virgonzalez"
pwd<- "Mvg155074037" #my password
email<- "virginiagonzalez782@gmail.com" #my email

library(sf)
geometry = st_read("C:/Users/Usuario/Documents/Paper Biodiversidad Urbana/geometry.shp")

querie <- occ_download(paste0("geometry within ", geometry), type = "within", format = "SPECIES_LIST",user=user,pwd=pwd,email=email)

res <- occ_download(pred("country", c("PE")), format = "SIMPLE_CSV", user=user,pwd=pwd,email=email)


occ_download(
  pred("country", "EC"),
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN"))),
  format = "SIMPLE_CSV",user=user,pwd=pwd,email=email
)

res 
#> <<gbif download>>
#>   Username: sckott
#>   E-mail: myrmecocystus@gmail.com
#>   Format: SPECIES_LIST
#>   Download key: 0000182-190415153152247

occ_download_meta(res) # see when ready
occ_download_wait('0271549-220831081235567')


occ_download_get('0261807-220831081235567') # download
#> Download file size: 4.73 MB
#> On disk at ./0000182-190415153152247.zip

occ_download_import(x) # import into R

spp <- read.csv("C:/Users/musimundo/Documents/Paper Biodiversidad Urbana/" , header = T)

spp <- subset(spp , kingdom == "Plantae" )
spp <- subset(spp , phylum == "Tracheophyta" )
spp <- subset(list , taxonRank == "SPECIES" )
install.packages("openxlsx")
library(openxlsx)
# for writing a data.frame or list of data.frames to an xlsx file
write.xlsx(spp, 'plant_listAR.xlsx')
library(sf)
spp <- st_read("C:/Users/musimundo/Documents/Paper Biodiversidad Urbana/exotica_conosurptos.shp")
getwd()
setwd("C:/Users/musimundo/Documents/Proy.gbif/bases")
spp <- spp$species
spp <- unique(spp)
library(sf)
spp1 <- st_read("C:/Users/Usuario/Documents/Tesis/paper_migraciones/occurrences/tree_gbif.shp")
spp2 <- st_read("C:/Users/Usuario/Documents/Tesis/paper_migraciones/occurrences/tree_exotic.shp")
spp1<- spp1$species
spp1 = unique(spp1)
spp2<- spp2$accepted_1
spp2 <- unique(spp2)

spp <- setdiff(spp2, spp1)

list = unique(spp1[spp1 %in% spp2])

write.xlsx(spp, 'plantlist.xlsx')



#########Cruzar el listado de exoticas con ocurrencias para saber las no exoticas
#########
library(sf)
list =  read.csv("D:/Proy.gbif/gbif/nativos_v2/03_space_database_n-001.csv", header = TRUE)
list1 <- subset(list, country_suggested == "")
spp1 = list$Output.Taxon2
spp1 = unique(spp1)

spp2 <- st_read("C:/Users/Usuario/Documents/Paper Biodiversidad Urbana/total_oc.shp")

base = spp2[spp2 %in% spp1 == FALSE]
