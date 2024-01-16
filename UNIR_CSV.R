library("dplyr")                                    # Load dplyr package
library("plyr")                                     # Load plyr package
library("readr") 

dirs <- file.path(getwd(), "D:/TF/raw_data2")
dirs # This is the path were the codes will be stored
dir.create(dirs) # Function for creating the directory where each dataset will be saved
data_all <- list.files(path = "D:/TF/raw_data2",  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows()



###########

library(data.table)

## PathProject
Pathproject = "D:/TF/raw_data2"

## files
Namefiles = list.files(Pathproject, full.names = TRUE)

## unión de archivos descargados
tmp <- list() 
for (i in 1:length(Namefiles)) tmp[[i]] <- fread(Namefiles[i])
datos <-  rbind.fill(tmp)

data_all 
# Print data to RStudio console
library(data.table)
write_xlsx(data_all , "SPECIESLINK.csv")

fwrite(datos, "BIEN.csv")


readr::write_csv(data_all , file.path(dirs, "BIEN.csv"))



sl1 = read.csv("D:/Proy.gbif/gbif/Arboles_exoticos/00_merged_database.csv")
sl2 = read.csv("D:/Proy.gbif/gbif/Output/Intermediate/00_merged_database.csv")
base = rbind.fill(sl1 , sl2)
