library(sf)

base <- st_read("D:/Paper Biodiversidad Urbana/pred3.shp")

#escalar
library(dplyr)
data1 = data %>%
  mutate_if(is.numeric, scale)
#no escala proporcion

names(data)
data <- base %>%
  mutate_at(vars(one_of("tcmean","cultivomea","dens_ganad", "precipmean", "div","tempmean",  "fuegosum", "fundacionm", "N")), scale)


data<- data[,c("tcmean","cultivomea","dens_ganad", "precipmean", "div","tempmean",  "fuegosum", "fundacionm", "N")]  # returns a data.frame




##transformo tipos de ciudades en numeros
data$tipo2  <- ifelse(data$tipo == 'Metropoli' ,1,
                    ifelse(data$tipo  == 'Grandes Ciudades' , 2, 
                           ifelse(data$tipo  == 'Ciudad intermedia' ,3,
                                  ifelse( data$tipo  == 'Ciudad Pequeña' ,4, 
                                          ifelse(data$tipo  == 'Towns' , 5, NA )))))

data$tipo2[is.na(data$tipo)]="6"

#limpio la base de datos
length(which(is.na(data)))
data=data[!is.na(data$N),]

data$intervalom = as.factor(data$intervalom)
data$tipo = as.factor(data$tipo)
length(which(is.na(data$tipo)))
base =base[!is.na(base$N),]

###REMOVER DUPLICADOS
library(tidyverse)
length(which(duplicated(data$id)))
library(dplyr) 
# Remove duplicate rows of the dataframe using NAME variable
data = distinct(data ,ID_1 , .keep_all= TRUE)

###AQUI LE SUMO EL MINIMO A CADA VARIABLE
data$suma_pob = data$suma_pob + min(data$suma_pob, na.rm = TRUE )
data$N = data$N+ min(data$N, na.rm = TRUE )
data$cultivomea = data$cultivomea + min(data$cultivomea , na.rm = TRUE )
data$fundacionm = data$fundacionm + min(data$fundacionm, na.rm = TRUE )
data$tempmean = data$tempmean  + min(data$tempmean , na.rm = TRUE )
data$precipmean = data$precipmean  + min(data$precipmean , na.rm = TRUE )
data$div = data$div  + min(data$div , na.rm = TRUE )
data$dens_ganad = data$dens_ganad  + min(data$dens_ganad , na.rm = TRUE )
data$fuegosum = data$fuegosum  + min(data$fuegosum , na.rm = TRUE )
data$tcmean = data$tcmean + min(data$tcmean, na.rm = TRUE )


library(ggplot2)

m1 = lm (proporcion ~  gravedadlo, data)
summary(m1)

x1 = data$gravedadlo
equation1=function(x){coef(m1)[1]*x1}

library(ggplot2)
library(ggpmisc)

x1 = mean(data$tcmean)
x2 = mean(data$N, na.rm = T)
x3 = mean(data$cultivomea, na.rm = T)
x4 =  mean(data$fundacionm , na.rm = T)
x5 = mean(data$div,  na.rm = T)
x7 = data$precipmean
x8 = mean(data$tempmean, na.rm = T)
x9 = mean(data$gravedadlo, na.rm = T)



value = fixef(r4)[6]*x7+ fixef(r4)[5]*x4+ fixef(r4)[4]*x2+ fixef(r4)[3]*x5+ fixef(r4)[2]*x3+ fixef(r4)[1]
#value = coef(m1)[2]*x1 + coef(m1)[1]
data <- cbind(data, value)

v = fixef(r4)[3]*x3

data$prec_c <- base$precipmean + base$precipmean^2
data$pred <- mydf$predicted


data$d1 <- ifelse(data$tipo == 'Metropoli' ,'Metropoli',
                      ifelse(data$tipo  == 'Grandes Ciudades' , 'Big cities', 
                             ifelse(data$tipo  == 'Ciudad intermedia' ,'Intermediate cities',
                                    ifelse( data$tipo  == 'Ciudad Pequeña' ,'Small cities', 
                                            ifelse(data$tipo  == 'Towns' , 'Towns', NA )))))

data$d1[is.na(data$d1)]="Remote areas"

#re-order factor levels for region
data$d1 <- factor(data$d1 , levels=c('Metropoli', 'Big cities' , 'Intermediate cities', 'Small cities', 'Towns', 'Remote areas'))


ggplot(data ,aes( precipmean , value)) + 
  #facet_wrap(~factor(d1))+
 #stat_poly_line() +
  #stat_poly_eq() +
  geom_point() +
  geom_smooth(method = "loess" , linewidth=0.2, colour = "black", size = 5) +
  #stat_function( fun=equation1 ,geom="line", aes(color = "#E78AC3")) +
  #scale_y_continuous(limits = c(0,0.5))+
  xlab("Mean precipitation (scaled,mm)")+ ylab("Nonnative plants' proportion")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3), title="Urban Hierarchy"))+
  theme_bw()+theme(legend.position = "bottom")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 


ggplot(data,aes( proporcion ,value, color = tipo2)) + 
  facet_wrap(~factor(tipo2))+
  #stat_poly_line() +
  #stat_poly_eq() +
  geom_point() +
  geom_smooth(method = "lm" , size=0.8, colour = "black") +
  #stat_function( fun=equation1 ,geom="line", aes(color = "#E78AC3")) +
  #scale_x_continuous(limits = c(0,0.7))+
  ylab("Native plants richness")+ xlab("Nonnative plants' proportion")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
  theme_bw()+theme(legend.position = "none")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 


p + geom_text(x = 25, y = 300, label = lm_eqn(data), parse = TRUE)

ggplot(data ,aes(d1, precipmean  , color = d1)) + geom_boxplot()+ 
  xlab("Type urbanization")+ ylab("Mean precipitation(scaled,mm)")+
  theme_bw()+theme(legend.position = "none")+
  #guides(colour=guide_legend(override.aes=list(alpha=1, size=3), title="Urban Hierarchy"))+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 

v3$tem_c <- v3$tempmean*v3$tempmean

ggplot(v3 ,aes(tempmean + tem_c  , proporcion)) + 
  #facet_grid(~factor(tipo))+
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess" , size=0.8, colour = "black") +
  #stat_function(fun=equation1 ,geom="line",size =1.5,aes(color = "#E78AC3")) +
  #scale_y_continuous(limits = c(0,0.2))+
  #xlab("Mean Temperature")+ ylab("Exotic Species Proportion")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
  theme_bw()+theme(legend.position = "none")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 

ggplot(data ,aes(gravedadlo , N )) + 
  geom_point(alpha = 0.4) +
 stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  #stat_function(fun=equation1 ,geom="line",size =1.5,aes(color = "#E78AC3")) +
  #geom_smooth(method = "loess", se = T, aes(color = "#E78AC3"))+
  #xlab("Annual Rate Change")+ ylab("Foundation date")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
  theme_bw()+theme(legend.position = "none")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 



#Ploteo para ver residuos cuando saco los tipos de ciudades
library(broom)
resid <- tidy((residuals(r4,"pearson", scaled = TRUE)))

library(ggplot2)
names(data)
data$tipo <- factor(data$tipo , levels=c("1", "2", "3", "4", "5", "6"))
data$OS = as.factor(data$OS)
ggplot(data ,aes( tipo , div , color = tipo)) + 
  geom_boxplot()+
  #guides(col= guide_legend(title= "Foundation date"))+
  #scale_x_continuous(limits = c(0,3))+
  #xlab("Type urbanization")+ ylab("Nonnative plants' proportion")+
  theme_bw() +theme(legend.position = "bottom")
names(data)

x1 = data$gravedadlo
equation1=function(x){coef(m1)[1]*x1}


ggplot(data ,aes( div , pred, color = as.factor(d1))) + 
  facet_grid(~factor(tipo))+
  geom_point() +
  geom_smooth(method = "loess" , size=0.8, colour = "black") +
  #scale_y_continuous(limits = c(0,3))+
  ylab("Land use diversity")+ xlab("Exotic plants' proportion")+
  guides(color = guide_legend(title = "Type of urbanization"))+
  theme_bw()+theme(legend.position = "bottom")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 

ggplot(data ,aes( pred , fundacionm , color= as.factor(tipo))) + 
  facet_grid(~factor(tipo))+
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess" , size=0.8, colour = "black") +
  #scale_x_continuous(limits = c(0,3))+
  ylab("Foundation date")+ xlab("Gravity")+
  guides(color = guide_legend(title = "Type of urbanization"))+
  theme_bw()+theme(legend.position = "bottom")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 

library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
sjPlot::plot_model(r4,type = "est", 
                   axis.labels=c("Temperature", "Precipitation","Fire freq.", "Land use Diversity" ,"Foundation date", "Crop percentage","Native plants richness", "Anual rate change" ),vline.color = "grey",
                   show.values=TRUE, value.offset = .5, show.p=TRUE)

tab_model(m1, show.intercept = TRUE,
          show.est = TRUE, digits = 5, p.style = "scientific_stars", 
          dv.labels = c("Alien species Proportion") ,  pred.labels = c("Intercept", "Gravity")
          )
library(effects)

sjPlot::plot_model(r4,type = "pred", terms= c("N [all]", "tipo"), pred.type = "re")


ggpredict(r4, c("N", "tipo"))

####significancia RE######
pchisq(2*(logLik(fitted_model)-logLik(fitted_model_without_RE)),
       df=1,lower.tail=FALSE)/2

##mapping predicts
###MAPEO PREDICTS
###
###
library(ggeffects)



library(ggplot2)
library(ggeffects)
mydf <- ggpredict(r4, terms= c("N [all]", "tipo2") , type = "re")

ggplot(mydf, aes(x, predicted, colour = group)) +
  stat_smooth(method = "lm", se = T) +
  facet_wrap(~group) +
  labs(
    y = "Exotic plants' probabilities",
    #x = "Foundation date",
    colour = "Type Urbanization")
    #colour = get_legend_title(mydf) )






library(ggplot2)
library(ggspatial)
library(ggthemes)
library(RColorBrewer)
display.brewer.all()
colours <- brewer.pal(n = 11, name = "RdYlGn")
data$pred <- predict(r4 , type = "response" )

ggplot(data = data )  +geom_sf(aes(fill= pred ), color = NA) +xlab("Long") + ylab("Lat")+ scale_fill_gradientn(name= "Predicts", colours = colours ) +
  annotation_scale()  + 
  #annotation_north_arrow(location='tr')
  theme_classic()+
  theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.))) 

st_write(data , dsn = "D:/Paper Biodiversidad Urbana/pred_v2.shp",layer="pred_v2", driver="ESRI Shapefile", append=FALSE)

library(sf)
data <- st_read("C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/exotica_conosurptos.shp")
names(data)
m = list(levels(as.factor(data$countryCod)))
m = data.frame(m)
m
library(dplyr)
m %>% select_if(~ is.factor(data$countryCod) && any(c("UY") %in% levels(.)))
length(which(data$countryCod == "BR"))
library("writexl") 
write_xlsx(m ,"Paper Biodiversidad Urbana/Analisis/df.xlsx")
