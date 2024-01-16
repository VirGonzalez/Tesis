
library(sf)

setwd("C:/Users/Usuario/Documents/python_scripts/bases")

poligonos<-st_read("paises3.shp")

plot(poligonos)
colnames(poligonos)
poligonos$area<-as.numeric(st_area(poligonos))/1000000

poligonos$dens_pob<-as.numeric(poligonos$TP)/poligonos$area

poligonos$dens_exoticas<-poligonos$exoticas/poligonos$area
attach(poligonos)
plot(ntl21mean,dens_exoticas)



detach(poligonos)
summary(lm(proporcion~as.numeric(TP),data=poligonos))
summary(lm(proporcion~as.numeric(inm_p),data=poligonos))
summary(lm(proporcion~as.numeric(ntl_hab),data=poligonos))
summary(lm(proporcion~as.numeric(precipmean),data=poligonos))
