library(sf)
hex <- st_read("D:/Tesina/TESINA/saltats1.shp")
hex2 <- st_read("cattle/rio_neuq.shp")
ex <- read.csv("D:/Tesina/TESINA/tablasalta.csv" , header = TRUE, sep = ";")

ex$prop_sum <- ex$sumNTL/ex$totalcount
ex2$prop_count <- ex2$countNTL/ex2$totalcount

ex <- ex[ , c("COUNTRY", "NAME_1", "NAME_2", "variable", "value")]
hex2$tiempo <- hex2$variable

M1 <- merge(hex, ex , by = c("COD_201", "COD_200"))

ex<- ex[!duplicated(ex), ]

library(data.table)
fwrite(M1, "andes_sur.csv")


library(raster)
m1 <- bind(hex, hex2)
M1 <- M1[ , c("COUNTRY", "NAME_1","NAME_2", "variable", "value")]

columnas <- c("COUNTRY", "NAME_1", "NAME_2")


#
library(dplyr)

sf_objeto_unido <- st_join(hex , hex2, join = st_intersects, )

# Filtrar el resultado basado en las columnas en común
sf_objeto_unido_filtrado <- sf_objeto_unido[apply(sf_objeto_unido[, columnas], 1, function(x) all(!is.na(x))), ]

st_write(M1 , dsn = "cattle_delta.shp", append=FALSE)



