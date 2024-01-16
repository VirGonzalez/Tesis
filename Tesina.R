getwd()
setwd("C:/Users/pc/Dropbox/PC/Documents/Paper/Graficas")
cor <- read.csv("graf.csv", header = TRUE, sep = ";",
         dec = ",")
str(cor)

cor$dnsdd10 = cor$dnsdd10
cor$densidad = (cor$dnsdd10 - log(cor$dsd01))/ log(cor$dsd01)
cor$CALIDAD <- (cor$CALIDAD- min(cor$CALIDAD, na.rm = T))/(max(cor$CALIDAD, na.rm = T )- min(cor$CALIDAD, na.rm = T))
#unir shape con csv
cor <- read.csv("tablasalta.csv", header = TRUE, sep = ";",
                dec = ",")
library(raster)
library(rgdal)
library(sf)
library(gam)
na.gam.replace(cor)
cor <- st_read("C:/Users/pc/Dropbox/PC/Documents/Paper/BA7.shp")
names(p)
str(p)

cor$dsd01 = cor$TP_T/cor$area01
cor$densidad = (cor$dnsdd10 - log(cor$dens01))/log(cor$dens01)
cor$CALIDAD <- (cor$cal.materi- min(cor$cal.materi, na.rm = T))/(max(cor$cal.materi, na.rm = T )- min(cor$cal.materi, na.rm = T))
cor$educacn <- (cor$Max.niv.in - min(cor$mx_nv_d, na.rm = T))/(max(cor$mx_nv_d, na.rm = T )- min(cor$mx_nv_d, na.rm = T))
cor$NBI <- (cor$NBI- min(cor$NBI, na.rm = T))/(max(cor$NBI, na.rm = T )- min(cor$NBI, na.rm = T))
cor$densidad <- (cor$densidad- min(cor$densidad, na.rm = T))/(max(cor$densidad, na.rm = T )- min(cor$densidad, na.rm = T))
cor$dnsd01 = log(cor$dnsd01)
cor$dnsd01 <- (cor$dens01- min(cor$dens01, na.rm = T))/(max(cor$dens01 , na.rm = T )- min(cor$dens01, na.rm = T))
st_write(cor , "C:/Users/pc/Dropbox/PC/Documents/Paper/BA7.shp")


sd(cor$tend)
#hago el gam
library(mgcv)
library(dplyr)
#pruebo si es estadisticamente distinto de cero las tendencias
cor <- read.csv("graf.csv", header = TRUE, sep = ";",
                dec = ",")
cor = cor %>% filter( Ciudad == "Great Cordoba")
tendencia = cor$tend
t.test(tendencia)
#probe varios modelos, siempre introduciendo solo la lat, log como temrino suavizado, "te" es un tipo de suavizado, habia visto que este era mejor para lat long
names(p)
plot(p)
deduped.data <- unique( cor[ , 1:25 ] )
m4<- gam(tend ~ te(x , y)+densidad + CALIDAD + EDUC + NBI + den01 ,
                   family = gaussian(), data = cor )
summary(m4)

summary(cor)


#plotear GAM
library(jtools)
plot_summs(model_salta)

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(model_salta, vline.color = "red")

# set up
library(lme4)
library(ggstatsplot)
ggcoefstats(model_salta) +
  ggplot2::labs(
    x = parse(text = "'standardized regression coefficient' ~italic(beta)"),
    y = "fixed effects"
  )

#saco el raster
rastersalta <- raster("C:/Users/pc/Dropbox/PC/Documents/Paper/corTS22.tif")



hist(rastersalta)
plot(rastersalta)
#extraigo mean values por poligono
#llamas tu archivo raster primero y despues el shape. En tu caso los radios y los LST
library(rgdal)
r.vals <- extract(rastersalta, cor ) #colocas primero el nombre del raster y luego del shp
r.mean<- unlist(lapply(r.vals, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
#ahora estima esto el promedio de los valores de tu raster para cada poligono de tu shape
#creo un dataframe 
shp@data <- data.frame(shp@data, Pendiente=r.mean) #aca creas la columna que tendra esos valores
#en mi caso era la pendiente de cambio de productividad
writeOGR(shp, ".", "ros2", driver="ESRI Shapefile", check_exists=TRUE, 
         overwrite_layer=TRUE) #aqui lo volves a escribir a tu shape ya modificado con esa nueva columna
#CODIGO PARA UNIR BASES DE DATOS (CSV, SHAPE)
library(dplyr)
cor$COD_200= as.character(cor$COD_201)
str(cor)
merged <- merge(cor, shp[ , c("Pendiente") ] , by = "COD_201", all.x=TRUE, no.dups = TRUE )
merge<- merge(cor , cor1 , by = 'COD_2010' )

write.csv2(cor, file = "C:/Users/pc/Dropbox/PC/Documents/Paper/capas_yoha/baires3.csv", row.names= TRUE, )

library(dplyr)
cor <- merged %>% 
  distinct(.keep_all = TRUE)

merged <- merge(cor, shp[ , c("Pendiente") ],  by = "COD_201")

 duplicated(merged)

library(tidyverse)
df1 <- cor %>%
  group_by(COD_201) %>%
  mutate(rowid = row_number())

df2 <- shp %>%
  mutate(rowid = row_number())

merged = left_join(shp , cor, by = 'COD_2010')

#HACER BOXPLOT
getwd()
setwd("C:/Users/pc/Dropbox/PC/Documents/Paper/Graficas")
cor <- read.csv("graf.csv", header = TRUE, sep = ";",
                dec = ",")
library(ggplot2)
library(tidyverse)
library(viridis)
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
library(jcolors)
display_all_jcolors()
palette = brewer.pal(n = 8, name = "Set2")
#cor = cor %>% filter(Ciudad == "Great Buenos Aires")

#names(cor)
cor <- read.csv("graf.csv", header = TRUE, sep = ";",
                dec = ",")
cor = cor %>% filter( Ciudad == "Great Buenos Aires")
x = cor$CALIDAD
x1 = mean(cor$densidad, na.rm = T)
x2 = mean(cor$EDUC , na.rm = T)
x3 = mean(cor$NBI, na.rm = T)
x4 = mean(cor$den01, na.rm = T)
equation1=function(x){coef(m1)[3]*x+coef(m1)[6]*x4+coef(m1)[5]*x3+coef(m1)[4]*x2+coef(m1)[2]*x1+coef(m1)[1]}
equation2=function(x){coef(m2)[3]*x+coef(m2)[6]*x4+coef(m2)[5]*x3+coef(m2)[4]*x2+coef(m2)[2]*x1+coef(m2)[1]}
equation3=function(x){coef(m3)[3]*x+coef(m3)[6]*x4+coef(m3)[5]*x3+coef(m3)[4]*x2+coef(m3)[2]*x1+coef(m3)[1]}
equation4=function(x){coef(m4)[3]*x+ coef(m4)[6]*x4+coef(m4)[5]*x3+coef(m4)[4]*x2+coef(m4)[2]*x1+coef(m4)[1]}
equation5=function(x){coef(m5)[3]*x+coef(m5)[6]*x4+coef(m5)[5]*x3+coef(m5)[4]*x2+coef(m5)[2]*x1+coef(m5)[1]}
library(scales)
ggplot(aes(y = tend , x = CALIDAD, color = Ciudad), data = cor) +
  geom_point(alpha = 0.09, size = 0.89)+
 stat_function(fun=equation1,geom="line",size =1.5,aes(color = "Great Salta"))+
  stat_function(fun=equation2,geom="line",size =1.5 ,aes(color = "Great San Miguel de Tucuman"))+
  stat_function(fun=equation3,geom="line",size =1.5, aes(color = "Great Rosario"))+
  stat_function(fun=equation4,geom="line",size =1.5,aes(color = "Great Cordoba"))+
  stat_function(fun=equation5,geom="line",size =1.5,aes(color = "Great Buenos Aires"))+
  scale_colour_manual("Cities", values = c("Great Salta" = "#66C2A5","Great San Miguel de Tucuman" = "#FFD92F", "Great Rosario" = "#8DA0CB" , "Great Cordoba" = "#E78AC3","Great Buenos Aires" = "#A6D854"))+
  scale_y_continuous(limits = c(-0.01, 0.01))+
  scale_x_continuous(limits = c(0, 1))+
  xlab("Household quality change")+ ylab("Annual productivity trends")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
 theme_minimal() + theme(legend.position="right")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 

predict(m4 ,data.frame(CALIDAD = 0))

ggsave("calidad.jpg")

#BOXPLOT

p = ggplot(aes(y = tend , x = Ciudad, color = Ciudad), data = cor) + 
  geom_boxplot()+
  scale_colour_jcolors(palette = "default", name = "Cities" )+ 
  scale_y_continuous(limits = c(-0.01, 0.01))+
  xlab("Cities")+ ylab("Annual productivity trends")+
  theme_minimal() + theme(legend.position="bottom")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 
p

#MAPEO DE VARIOS MAPAS CON IGUAL LEYENDA
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = 5, name = "RdYlGn")
library(tmap)
str(shp$Pendientem)
str(p)
p$densidad <- as.numeric(paste( " ", p$densidad , " "))

shp$Pendiente <- as.numeric(shp$Pendiente)
names(p)
library(sf)
p <- st_read("C:/Users/pc/Dropbox/PC/Documents/Paper/saltats1.shp")
names(p)
ggplot(data = p )  +geom_sf(aes(fill= TendenciaN ), color = NA) +xlab("Long") + ylab("Lat")+ scale_fill_gradientn(name= "Annual productivity trend", 
                       colours= my.palette, na.value="transparent" )+
  annotation_scale()  +
  annotation_north_arrow(location='tr')+theme_classic()+
  theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.))) 

p <- st_read("C:/Users/pc/Dropbox/PC/Documents/Paper/capas_yoha/tucuman1.shp")
names(p)
ggplot(data = p )  +geom_sf(aes(fill= NBI), color = NA) +xlab("Long") + ylab("Lat")+ scale_fill_gradientn(name= "UBN change ", colours= my.palette, na.value="transparent")+
  annotation_scale()  +annotation_north_arrow(location='tr')+theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.))) 

p <- st_read("C:/Users/pc/Dropbox/PC/Documents/Paper/capas_yoha/saltats1.shp")
names(p)
ggplot(data = p )  +geom_sf(aes(fill= densidad ), color = NA) +xlab("Long") + ylab("Lat") + scale_fill_gradientn(name= "Intercensal population density change ", 
                                                                            colours= my.palette, na.value="transparent")+
  annotation_scale() +annotation_north_arrow(location='tr')+theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.))) 

p <- st_read("C:/Users/pc/Dropbox/PC/Documents/Paper/capas_yoha/BA5.shp")

duplicated(p)
ggplot(data = p )  +geom_sf(aes(fill= EDUC ), color = NA) +xlab("Long") + ylab("Lat") + scale_fill_gradientn(name= "Mean Educational level Change", 
                                                             colours= my.palette , na.value="transparent")+
  annotation_scale()  +theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.)))

p <- st_read("C:/Users/pc/Dropbox/PC/Documents/Paper/CAPAS_DEFINITIVAS/capas_yoha/BA7.shp")
ggplot(data = p )  +geom_sf(aes(fill= TendenciaN), color = NA) +xlab("Long") + ylab("Lat") + scale_fill_gradientn(name= "Annual productivity trend ", 
                                                             colours= my.palette, limit=c(-0.01, 0.01), na.value="transparent" )+
  annotation_scale()  +theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.)))

install.packages("patchwork")
library("patchwork")


map_sal_cal + map_sal_edu + map_sal_nbi + map_sal_densidad + map_sal_01+  plot_layout( guides = "collect") &theme(legend.position='bottom',legend.key.width = unit(1, 'cm'))
map_sal_cal + map_sal_edu + map_sal_nbi + map_sal_densidad + map_sal_01+ plot_layout( guides = "auto")
library(ggpubr)

ggarrange(map_sal_cal, map_sal_edu, map_sal_nbi, map_sal_densidad, map_sal_01,  col=2, nrow=3)

library()
install.packages("gridExtra")
library("gridExtra")
library("grid")
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(2, "npc") - lheight, lheight))
}

grid_arrange_shared_legend(map_sal, map_tuc, map_cor, map_ros,map_ba, 
                           ncol = 2, nrow = 3)




library(tmap)
tm_shape(shp) +
  tm_polygons("Pendiente", 
              style="quantile", 
              title="Gran Salta")

p3=grid.arrange(
  p1 + theme(legend.position="none"), p2+ theme(legend.position="none"), nrow=1, widths = unit(c(10.,10), "cm"), heights = unit(rep(8, 1), "cm")))

plot(map_sal_01)


###saco el rango
calidad = cor$CALIDAD

