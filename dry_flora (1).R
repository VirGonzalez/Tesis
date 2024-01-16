spp <- read.csv("C:/Users/musimundo/Documents/Proy.gbif/bases/records_py_dryflora.csv" , header = TRUE, sep = ";")
spp <- read.csv("C:/Users/musimundo/Documents/Proy.gbif/bases/GUT.csv" , header = TRUE, sep = ";")
sp2 <- read.csv("C:/Users/musimundo/Documents/Proy.gbif/lista_arbolexoticoArg.csv", header = T , sep = ";")
sp2 <- sp2$Nombre.científico

length(which(duplicated(spp$species)))

library(dplyr)
spp <- spp$species %>%
  unique() %>%
  na.omit() %>%
  c()

sp2 <- setdiff(sp2, spp)
list(spp, sp2)
