library(rgbif)
require(scrubr)
library(maps)
# fill in your gbif.org credentials 
user <- "virgonzalez" # your gbif.org username 
pwd <- "Mvg155074037" # your gbif.org password
email <- "virginiagonzalez782@gmail.com" # your email 
library(dplyr)
library(readr)  
library(rgbif) # for occ_download
# The 60,000 tree names file I downloaded from BGCI
file_url <- "https://data-blog.gbif.org/post/2019-07-11-downloading-long-species-lists-on-gbif_files/global_tree_search_trees_1_3.csv"
# match the names 
spp


gbif_taxon_keys <- 
  spp %>%
  #head(1000) %>% # use only first 1000 names for testing
  #pull("taxon") %>% # use fewer names if you want to just test 
  name_backbone_checklist()  %>% # match to backbone
  filter(!matchType == "NONE") %>% # get matched names
  filter(kingdom == "Plantae") %>%
  #filter(country == c("AR","PY", "BR")) %>%
  pull(usageKey) # get the gbif taxonkeys


# gbif_taxon_keys should be a long vector like this c(2977832,2977901,2977966,2977835,2977863)
# !!very important here to use pred_in!!
as.data.frame(gbif_taxon_keys)
write_csv(as.data.frame(gbif_taxon_keys), file.path(dirs, "taxon_keys_tf.csv"))

res <- occ_download(pred("country", "AR"), type = "within", format = "SPECIES_LIST",type = "within",user=user,pwd=pwd,email=email)

gbif_taxon_keys <- read.csv("taxon_keys_tf.csv", header= T , sep = ";")


spp <- unique(gbif_taxon_keys)
spp <- spp$gbif_taxon_keys

occ_download(pred_in("country", c("AR","BR","PY")),
  pred_in("taxonKey", spp),
  pred("hasCoordinate", TRUE),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)

occ_download_wait('0256406-220831081235567')

d <- occ_download_get('0256406-220831081235567') %>%
  occ_download_import()

######LISTADO ESPECIES

occ_download(pred_in("country", c("AR", "BO", "CO", "VE","EC", "PE", "BR","PY")),
             format = "SPECIES_LIST",
             user=user,pwd=pwd,email=email
)