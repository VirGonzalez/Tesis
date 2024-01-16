setwd("C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/Analisis")
library(sf)
data <- st_read("C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/Analisis/aves1.shp")
data <- st_read("C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/Analisis/plantas1.shp")
names(data)
####SC
###normalizo
names(data1)
str(data1)
library(dplyr)
data1[,which(!names(data1) %in% c("proporcion", "suma_pob", 
                                  "cultivo_", 
                                  "dens_ganad", "precipitac", "div", "access",
                                  "temp", "gravedadlo", "road_dens", "fuego", "fundacionm",
                                  "rich_anim", "N"))] <- scale(data1[,which(!names(data1) %in% c("proporcion", "suma_pob", 
                                                                                                 "cultivo_", 
                                                                                                 "dens_ganad", "precipitac", "div", "access",
                                                                                                 "temp", "gravedadlo", "road_dens", "fuego", "fundacionm",
                                                                                                 "rich_anim", "N"))])


data = data %>%
  mutate_at(vars(one_of("suma_pob","cultivo_","dens_ganad", "precipitac", "div", "access","temp", "gravedadlo", "road_dens", "fuego", "fundacionm","rich_anim", "N")), scale)

data1 = data1 %>%
  mutate_if(is.numeric, scale)
summary(data)
summary(scale(data$proporcion))
#GRAFICO EXPLORATORIO
library(ggplot2)
ggplot(data =data1 , aes(x = proporcion , y = cultivo_ , color = d1)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ d1) + 
  theme(legend.position = "none")
####modelos mixto
library(lme4)
library(nlme)
#PLANTAS
#SOLO INTERCEPTO ALEATORIO
names(data1)
modelo1<-lme(proporcion ~ suma_pob + cultivo_   + access + fundacionm + 
               rich_anim ,  data = data1, random = ~ 1|d1+ , na.action=na.omit)

fit1 = lmer(proporcion ~ suma_pob + cultivo_ + PAIS+
                          dens_ganad + precipitac + ECO_ID + div + access+ temp+
                          gravedadlo + road_dens + fuego + fundacionm + 
                          rich_anim + N + (1|d1)+(1|access) , data = data1)
modelo2<-lme(proporcion ~ suma_pob + cultivo_ + 
               gravedadlo + fundacionm + 
               rich_anim , data = data1 , 
             random = ~ 0 + (suma_pob|d1) + (cultivo_|d1) + 
               (gravedadlo|d1) + (fundacionm|d1) + 
               (rich_anim|d1) ,na.action=na.omit)

#PENDIENTE E INTERCEPTO ALEATORIO
modelo3<-lmer(proporcion ~ suma_pob + cultivo_ +
               access+  fundacionm + 
               rich_anim + access|d1  , data = data1 , na.action=na.omit)
anova(modelo1, modelo3)

library(car)
Anova(fit1)
tab_model(modelo3)
summary(modelo1)
summ$tTable 
#SOLO PENDIENTE ALEATORIA
names(data1)
dat = list(d1 = pdDiag(~ suma_pob + cultivo_ + PAIS+
                         dens_ganad + precipitac + ECO_ID +div+ access + temp+
                         gravedadlo + road_dens + fuego + fundacionm + 
                         rich_anim + N))
####AVES!
modelo1_b<-lme(proporcion ~ 
               dens_ganad  +
               gravedadlo   + fundacionm + 
               rich_animm , data = data , 
             random = ~ 1|d1 + 1 ,na.action=na.omit)
tab_model(modelo1_b)

modelo2_b<-lme(proporcion ~ 
                 dens_ganad  +
                 gravedadlo   + fundacionm + 
                 rich_animm, data = data , 
               random = ~ 0 + 
                 dens_ganad  +
                 gravedadlo   + fundacionm + 
                 rich_animm|d1 ,na.action=na.omit)

      
#PENDIENTE E INTERCEPTO ALEATORIO
modelo3b<-lme(proporcion ~ 
               dens_ganad  +
               gravedadlo   + fundacionm + 
               rich_animm , data = data, random = list(d1 = pdDiag(~1+
                  dens_ganad  +
                 gravedadlo   + fundacionm + 
              rich_animm)) , na.action=na.omit)
#https://fhernanb.github.io/libro_modelos_mixtos/apli-nlme.html

fit3 = lmer(proporcion ~ suma_pob + cultivo_ + PAIS+
              dens_ganad + precipitac + ECO_ID + div + access+ temp+
              gravedadlo + road_dens + fuego + fundacionm + 
              rich_anim + N +(1|d1)+ (0 + suma_pob + cultivo_ + PAIS+
              dens_ganad + precipitac + ECO_ID + div + access+ temp+
              gravedadlo + road_dens + fuego + fundacionm + 
                 rich_anim + N|d1) , data = data1)

modelo4<-lme(proporcion ~ suma_pob +ECO_ID + 
               gravedadlo 
             + road_dens  + fundacionm , 
             data = data, random = ~ 1|tipo_ciuda, na.action=na.omit)


tab_model(modelo1_b)
summary(modelo3)
summ = summary(modelo3)
summ$tTable 

anova(modelo1_b , modelo3b)

#☺PLOTEAR MODELO
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(modelo3, show.intercept = TRUE,
          show.est = TRUE, digits = 5, p.style = "scientific_stars")

#modelo lineal simple para aves
m1 <- lm(proporcion ~ suma_pob + cultivo. +
           dens_ganad + precipitac + ECO_ID + PAIS + div+ access+ temp+ 
           gravedad + road_dens + fuego + fundacionm + 
           riqueza + remot , data = data,na.action=na.omit)
summary(m1)
step(m1)
m2 = lm(proporcion ~ suma_pob + cultivo. + dens_ganad + precipitac + 
          PAIS + div + temp + gravedad + road_dens + fuego + fundacionm + 
          riqueza + remot , data = data )
summary(m2)

#glmer
#solo intercepto
r1 <- glmer(proporcion ~ suma_pob + cultivo_ + PAIS+
              dens_ganad + precipitac + ECO_ID + div + access+ temp+
              gravedadlo + road_dens + fuego + fundacionm + 
              rich_anim + N +  (1|d1), data = data1,  na.action=na.omit, family=gaussian)

#solo pendiente
r2 <- glmer(proporcion ~ suma_pob + cultivo_ + PAIS+
              dens_ganad + precipitac + ECO_ID + div + access+ temp+
              gravedadlo + road_dens + fuego + fundacionm + 
              rich_anim + N +  (suma_pob|d1), data = data1,  na.action=na.omit, family=gaussian)
r3 <- glmer(proporcion ~ suma_pob + cultivo_ + PAIS+
              dens_ganad + precipitac + ECO_ID + div + access+ temp+
              gravedadlo + road_dens + fuego + fundacionm + 
              rich_anim + N +  (1|d1) + (0 + suma_pob|d1), data = data1,  na.action=na.omit, family=gaussian)

anova(r1,r2,r3)



####GRAFICO MODELO MIXTO
plot_model(modelo1_b ,type="est",
                 terms=c("`Riqueza Animales","Fundacion",
                         "Gravedad", "densidad ganado"))

lattice::xyplot(pred ~ access | d1, groups=d1, data= data1, type=c('p','r'), auto.key=F)

pred <- predict(modelo1 , newdata =data , na.action = na.pass)

length(which(duplicated(data$ID_1)))
data$pred <- predict(modelo4 , newdata =data, na.action = na.pass)
fit <- fitted(modelo2 , newdata =data1, na.action = na.pass)
library(ggplot2)
resid = modelo4$residuals

ggplot(data ,aes(div , pred ,col=tipo)) + 
  facet_grid(~factor(tipo))+
  geom_smooth(method = "loess" , size=0.8, colour = "black") +
  #scale_x_continuous(limits = c(0,3))+
  geom_point(alpha = 0.2) +
  xlab("Temperatura")+ ylab("Predicts modelo mixto")+
  theme_bw()+theme(legend.position = "none")


ggplot(data1 ,aes(x= suma_pob ,y=proporcion ,colour=d1 ))+  geom_point() +
  geom_point(aes(y = predict(fit1)), col = "black") +
  geom_smooth(aes(y = predict(fit1), colour = Bank2), method = "lm") + facet_wrap(~d1)

sjPlot::plot_model(modelo1_b, 
                   axis.labels=c("Riqueza animal", "Fundacion",
                                 "Gravedad", "Poblacion"
                                 ),
                   show.values=TRUE, show.p=TRUE)

library(ggplot2)
library(lme4)
library(multcomp)
tmp <- as.data.frame(confint(glht(modelo3))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()

# Second possibility
tmp <- as.data.frame(confint(glht(modelo3))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_boxplot()+
  theme_classic()

# plot estimates and confidence intervals as box plots grouped by model


                                                                                           

data$d1 <- factor(data$d1 , levels=c('Metropoli','Grandes Ciudades','Ciudad intermedia'
                                    ,'Ciudad Pequeña', 'Towns', 'NA'))
library(broom.mixed)
library(dotwhisker)

library(ggplot2)
ggplot(data, aes(dens_ganad , rich_animm , color=d1) ) + 
  geom_point() + 
  geom_smooth(method = 'lm', se=TRUE, fullrange=TRUE, size = 1)+
  labs(colour="Tipo de ciudad")+
  scale_y_continuous(limits = c(0,0.2))+
  theme_minimal() + theme(legend.position="bottom")+
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1)))+
  theme_classic()
names(data)

ggplot(data = data1 , aes(x = pred , y = access , color = d1)) +
  geom_point() +
  geom_smooth(method = 'lm', colour="black", se=TRUE, fullrange=TRUE, size = 1)+
 #scale_y_continuous(limits = c(0,2))+
  stat_smooth(method = "loess")+
  geom_smooth(method = "loess" , size=0.8, colour = "black") +
  xlab("Poblacion")+ ylab("Proporcion plantas exoticas")+
  theme_bw() +
 facet_wrap(~ d1) + 
  theme(legend.position = "bottom")

ggplot(data = data , aes(x = fundacionm , y = proporcion , color = tipo_ciuda)) +
  geom_point(aes(x = fundacionm , y = proporcion, color = tipo_ciuda)) +
  geom_smooth(colour="black", se=TRUE, fullrange=TRUE, size = 1) +
  scale_y_continuous(limits = c(0,0.))+
  theme_bw() +
  facet_wrap(~ tipo_ciuda) + 
  theme(legend.position = "none")

names(data1)
ggplot(data, aes(x= d1 , y= data$rich_animm , color= as.factor(d1))) + geom_boxplot()+
  labs(fill = "Tipo Ciudad" )+ylab('Riqueza Animales')+
  #scale_y_continuous(limits = c(0, 1))+
  xlab('Tipo de ciudad')+theme_classic()+theme(legend.position="bottom")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



graf = ggplot(aes(y = proporcion , x = fundacionm ), data = data) + geom_point()+
  scale_colour_brewer(palette = "BuPu")+ geom_smooth( se=TRUE, fullrange=TRUE, size = 1)+
  scale_y_continuous(limits = c(0, 0.25))+
  #scale_x_continuous(limits = c(0, 50000))+
  xlab("Fecha de fundacion promedio")+ ylab("Proporcion de aves exoticas")+
  theme_minimal() + theme(legend.position="bottom")+
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1)))+
  theme_classic()
graf

###MULTICOLINEARIDAD
install.packages("caTools")    # For Linear regression 
install.packages('car')        # To check multicollinearity 
install.packages("quantmod")
install.packages("MASS")
install.packages("corrplot")   # plot correlation plot

library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)
library(dplyr)

data_x <- data1 %>% select(14,15,19,22, 84,111)                                    # independent variables 
library(RColorBrewer)
var <- cor(data_x,use="complete.obs")  
corrplot(var , type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
                                       # independent variables correlation matrix 



###MAPEO PREDICTS
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = 5, name = "RdYlGn")
data$pred <- predict(modelo4 , newdata =data, na.action = na.pass)
ggplot(data = data )  +geom_sf(aes(fill= tipo ), color = tipo) +xlab("Long") + ylab("Lat")+ scale_fill_gradientn(name= "Remoticidad", 
                                                                                                                  colours= my.palette, na.value="grey" )+
  annotation_scale()  +
  annotation_north_arrow(location='tr')+theme_classic()+
  theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.))) 


writeOGR(data, ".", "plantasGBIF2", driver="ESRI Shapefile", check_exists=TRUE, 
         overwrite_layer=TRUE)
st_write(data , dsn = "C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/Analisis/GBIF.shp",layer="GBIF", driver="ESRI Shapefile" )
