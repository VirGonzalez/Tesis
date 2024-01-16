library(reshape2)
library(sf)
ex<- st_read("D:/paper_sri/tablas_ntl/sumntl_120.shp")
ex<- read.csv("D:/paper_sri/tablas_ntl/sumntl_120.csv", header= T , sep = ",")
names(ex)
serie2 <- melt(ex, id.vars=c( "system.index", "bottom", "id", "left", "right" ,"top", ".geo" ))

write.csv(serie2, file = "D:/paper_sri/tablas_ntl/sum_120_m.csv")
st_write(serie2 , dsn = "D:/Ale/sup_melt.shp",layer="sup_melt", driver="ESRI Shapefile" )
