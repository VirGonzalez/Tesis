setwd("C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/Analisis")
#reclasificar raster
library(raster)
r1 = raster("C:/Users/pc/Documents/Doctorado/Paper Biodiversidad Urbana/UCRA_conosur.tif")
rc <- function(x1) {
  ir1 = raster("C:/Users/pc/Documents/Doctorado/Paper Biodiversidad Urbana/UCRA_conosur.tif")
  rc <- function(x1) { 
    ifelse(x1 %in% c('1','8', '15', '22') , 1 ,
           ifelse(x1 %in% c( '2', '9', '16', '23') , 2 , 
                  ifelse(x1 %in% c('3','4','10', '11', '17', '18','24', '25')  ,3 ,
                         ifelse( x1 %in% c('5','6','12','13','19','20','26','27'), 4,
                                 ifelse(x1 %in% c('7','14','21','28') , 5, NA)))))
  }
}

r.class <- overlay(r1, fun=rc)
plot(r.class)
#clasifico UN VECTOR de acuerdo de cada clase
library(raster)
x1 = shapefile("C:/Users/pc/Documents/Doctorado/Paper Biodiversidad Urbana/Analisis/plantas1.shp")
x1$tipo <- ifelse(x1$clase1 >= 10 , 'Metropoli' ,
                  ifelse(x1$clase2 >= 10 , 'Grandes Ciudades' ,  
                         ifelse(x1$clase3 >= 10 , 'Ciudad intermedia' ,
                                ifelse( x1$clase4 >= 10 , 'Ciudad Pequeña' ,
                                        ifelse(x1$clase5 >= 10 , 'Towns' , NA)))))
library(sf)
data <- st_read("C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/Analisis/GBIF2.shp")
#escalar
library(dplyr)
data1 = data1 %>%
  mutate_if(is.numeric, scale)
#no escala proporcion
data = data %>%
  mutate_at(vars(one_of( "suma_pob","cultivo_","dens_ganad", "precipitac", "div", "access","temp", "gravedadlo", "road_dens", "fuego", "fundacionm","rich_anim", "N")), scale)

##transformo tipos de ciudades en numeros
data$tipo <- ifelse(data$d1 == 'Metropoli' ,1,
                    ifelse(data$d1 == 'Grandes Ciudades' , 2, 
                           ifelse(data$d1 == 'Ciudad intermedia' ,3,
                                  ifelse( data$d1 == 'Ciudad Pequeña' ,4, 
                                          ifelse(data$d1 == 'Towns' , 5,NA )))))

data$tipo[is.na(data$tipo)]="6"

#limpio la base de datos
length(which(is.na(data$tipo)))
data=data[!is.na(data$intervalom),]

data$intervalom = as.factor(data$intervalom)
data$tipo = as.factor(data$tipo)
length(which(is.na(data1$tipo)))
data =data[!is.na(data$N),]

###REMOVER DUPLICADOS
library(tidyverse)
length(which(duplicated(data$id)))
library(dplyr) 
# Remove duplicate rows of the dataframe using NAME variable
data = distinct(data ,ID_1 , .keep_all= TRUE)

###AQUI LE SUMO EL MINIMO A CADA VARIABLE
data$suma_pob = data$suma_pob + min(data$suma_pob, na.rm = TRUE )
data$N = data$N+ min(data$N, na.rm = TRUE )
data$cultivo_ = data$cultivo_ + min(data$cultivo_ , na.rm = TRUE )
data$fundacionm = data$fundacionm + min(data$fundacionm, na.rm = TRUE )
data$temp = data$temp  + min(data$temp , na.rm = TRUE )
data$precipitac = data$precipitac  + min(data$precipitac , na.rm = TRUE )
data$div = data$div  + min(data$div , na.rm = TRUE )
data$dens_ganad = data$dens_ganad  + min(data$dens_ganad , na.rm = TRUE )
data$fuego = data$fuego  + min(data$fuego , na.rm = TRUE )
###HAGO EL MODELO
#hago modelo mixto// este hicimos al ultimo
library(lme4)

modelo1 <-lme(proporcion ~ suma_pob +  N + cultivo_ + fundacionm +
                + temp + precipitac + div + dens_ganad + fuego ,  data = data , random = ~ 1|d1 , 
              na.action=na.omit, method = "REML", control=(msMaxIter=10))

plot(modelo1)
length(which(is.na(data$N)))

library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(modelo4, show.intercept = TRUE,
          show.est = TRUE, digits = 5, p.style = "scientific_stars")

plot(data$proporcion , data$div)


#modelos no lineales mixtos
library(nlme)

##comparo AIC de las distintas distribuciones
require(fitdistrplus)
data$proporcion = asin(sqrt(data$proporcion))
ctrl <- lmeControl(opt='optim')
modelo4<-lme(proporcion ~ suma_pob +  N + cultivo_ + fundacionm  
             +I(dens_ganad^2) + 
               +precipitac+ I(precipitac^2)+ temp + I(temp^2)  ,
             random = ~ intervalom| tipo  ,data = data,na.action=na.omit, method = "ML",
             control= ctrl)
summary(modelo4)
library(lme4)
r4 <- glmer(proporcion ~ suma_pob +  N + cultivo_ + 
              dens_ganad +I(dens_ganad^2) + div + I(fuego^2)
            +precipitac+ I(precipitac^2)+ temp + I(temp^2) + (1|intervalom/tipo),
            data = data , family= binomial(link = "logit"), weights= ocurrencia)

summary(r4)

library(ggplot2)
anova(r2, r3, r4)
st_write(data , "C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/Analisis/plantas4.shp")


library(ggplot2)
ggplot(data , aes(proporcion ,dens_ganad)) + geom_point() + geom_smooth()
###hago los predict
fit = fitted(modelo1 , newdata =data , na.action = na.pass)
data$pred <- predict(r4 , newdata =data, na.action = na.pass, type="response")

library(ggplot2)
resid = r2$residuals
##residuos para glmer 
library(broom)
resid <- tidy((residuals(r2,"pearson", scaled = TRUE)))


ggplot(data ,aes(div, pred ,col= tipo )) + 
  facet_grid(~factor(tipo ))+
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess" , size=0.8, colour = "black") +
  #scale_x_continuous(limits = c(0,3))+
  xlab("Div coberturas")+ ylab("Predicts")+
  theme_bw()+theme(legend.position = "none")

library(ggplot2)
ggplot(data ,aes(intervalom , pred, col= intervalom)) + geom_boxplot()+
  xlab("Tipo de urbanizacion")+ ylab("Predicts")+
  theme_bw()+theme(legend.position = "none")

ggplot(data ,aes(fuego , pred)) + 
  #facet_wrap(~factor(d1, levels=c('Metropoli','Grandes Ciudades','Ciudad intermedia'
  #                                ,'Ciudad Pequeña', 'Towns')))+ ###el que salia alargado era facet_grid
  geom_point(alpha = 0.3) +
  stat_smooth(method = "loess")+
  geom_smooth(method = "lm" , size=0.8, colour = "red") +
  #scale_x_continuous(limits = c(0,3))+
  xlab("Frec.fuegos")+ ylab("Predict modelo mixto")+
  theme_bw()+theme(legend.position = "none")

#y los boxplot
ggplot(data, aes(x= d1 , y= data$rich_animm , color= as.factor(d1))) + geom_boxplot()+
  labs(fill = "Tipo Ciudad" )+ylab('Riqueza Animales')+
  #scale_y_continuous(limits = c(0, 1))+
  xlab('Tipo de ciudad')+theme_classic()+theme(legend.position="bottom")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
tab_model(m1, show.intercept = TRUE,
          show.est = TRUE, digits = 5, p.style = "scientific_stars", 
          title = "Alien species Proportion",  pred.labels = "Gravity")

sjPlot::plot_model(r2, show.values=TRUE, value.offset = .3, show.p=TRUE,)
sjPlot::plot_model(r2,type = "est", 
                   axis.labels=c("Temp","Temp","Precip","Precip","Fuego", "Div","Densidad ganado","Densidad ganado","Cultivo","Plantas nativas", "Poblacion"
                   ),vline.color = "grey",
                   show.values=TRUE, value.offset = .5, show.p=TRUE)

plot(r2  , d1 ~ predict(.), abline = 0 ) 
sjPlot::plot_model(r4 , type = "re")
plot_model(r4 , type = "pred", se = TRUE ,
           pred.type = "re",colors = "v", show.data = TRUE, 
           dot.size = 0.5, line.size = 1 , show.loess = TRUE)+ 
  theme_bw(base_size=12) +
  theme(panel.grid.major.y=element_blank())



library(ggplot2)
ggplot(data1 , aes(x = access , y = proporcion)) +
  facet_grid(~factor(d1, levels=c('Metropoli','Grandes Ciudades','Ciudad intermedia'
                                  ,'Ciudad Pequeña', 'Towns')))+
  geom_smooth(method = "loess", se = FALSE) +
  #geom_line(aes(y = fit), color = "red") + 
  geom_point(alpha = 0.1, size = 3) +
  theme_bw()


library(ggeffects)
plot_model(r4, terms = c("temp [all]"), type = "pred", pred.type = "re",
           show.data = TRUE, 
           dot.size = 0.5, line.size = 1 , show.loess = TRUE, colors = "v")
ggpredict(modelo1_, terms = c("access", "tipo"), type = "re") %>% plot(add.data = TRUE, facet=TRUE)+labs(title = "Predicted values of exotic plant prop", 
                                                                                                         x = "Proportion of exotic plants", 
                                                                                                         y = "Population")+ theme_classic()
##mapping predicts
###MAPEO PREDICTS
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = 5, name = "RdYlGn")
data$pred <- predict(modelo4 , newdata =data, na.action = na.pass)
ggplot(data = data )  +geom_sf(aes(fill= pred ), color = NA) +xlab("Long") + ylab("Lat")+ scale_fill_gradientn(name= "Predicts", 
                                                                                                               colours= my.palette, na.value="grey" )+
  annotation_scale()  +
  annotation_north_arrow(location='tr')+theme_classic()+
  theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.))) 


writeOGR(data, ".", "plantasGBIF2", driver="ESRI Shapefile", check_exists=TRUE, 
         overwrite_layer=TRUE)

st_write(data , dsn = "C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/Analisis/GBIF2.shp",layer="GBIF2", driver="ESRI Shapefile")
