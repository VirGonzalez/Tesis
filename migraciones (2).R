library(sf)
base = st_read("D:/Tesis/paper_migraciones/mig (2)/mig/paises.shp")
puntos = st_read("D:/Proy.gbif/gbif/arb_nativos/data_n.shp")
names(base)

length(unique(puntos$scientific))
p1 = subset(puntos, country_su == "Colombia")

library(ggplot2)


base$inming = base$inmig_m1 + base$inming_f1

base$emig = base$emig_f1 + base$emig_m1 

base$net = base$inming - base$emig

base$migxpob = base$net/base$tp

library(dplyr)
data = base %>%
  mutate_if(is.numeric, scale)

st_write(base , dsn = "C:/Users/Usuario/Documents/Tesis/paper_migraciones/mig (2)/mig/migraciones.shp",layer="migraciones", driver="ESRI Shapefile" )
library(dplyr)
base = base %>%
  mutate_at(vars(one_of( "inmig", "net")), scale)

ggplot(base ,aes(net , proporcion )) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess",se=TRUE, fullrange=TRUE, size = 1, aes(color = "#E78AC3")) +
  #scale_y_continuous(limits = c(0,1))+
  xlab("Net migrations")+ ylab("Exotic plants proportion")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
  theme_bw()+theme(legend.position = "none")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 

ggplot(data ,aes(net , radsum )) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess",se=TRUE, fullrange=TRUE, size = 1, aes(color = "#E78AC3")) +
  #scale_y_continuous(limits = c(0,3))+
  xlab("Net migrations")+ ylab("NTL sum")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
  theme_bw()+theme(legend.position = "none")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 


#####BOXPLOT########

ggplot(base ,aes(NAME_0 , proporcion , color = NAME_0 )) + 
  geom_boxplot()+
 #scale_y_continuous(limits = c(0,200000))+
  xlab("Country")+ ylab("Exotic plants' proportion")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
  theme_bw()+theme(legend.position = "none")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1)))

library(MASS)

#Fit a linear model by robust regression using an M estimator.
#
#
m1 = lm(proporcion ~ inming, data)  #####MEJOR MODELO SOLO CON MIGRACION
m4 = lm(proporcion ~ net, data)
summary(m1)

m2 = lm(proporcion ~ migxpob , data )
summary(m2)
m3 = lm(proporcion ~ inming + radsum , data)
summary(m1)
m4 = lm(proporcion ~ net + rad.km2 , data)
summary(m4)

step(m4)


######### Hago un raster que sea un mapa de calor#########
######### 


library(MASS)

library(raster)
base = st_read("D:/Tesis/paper_migraciones/mig (2)/mig/puntos.shp")

base_f <- st_transform(base, crs = 22183)
library(spatstat)
W <- as.owin(base_f$geometry[1]) # First county of North Carolina data set in spatstat format
X <- runifpoint(100, win = W)
plot(X, "Random points")

library(ggplot2)

ggplot(data = base) +
  geom_sf() +
  theme_bw() +
  stat_density_2d(mapping = ggplot2::aes(x = purrr::map_dbl(geometry, ~.[1]),
                                         y = purrr::map_dbl(geometry, ~.[2]),
                                         fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 0.5)

> sessionInfo()