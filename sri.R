library(sf)
v <- st_read("C:/Users/Usuario/Documents/python_scripts/SRI/sri.shp")
min <- read.csv("C:/Users/Usuario/Documents/python_scripts/SRI/min_rad.csv", header = T, sep = ";")


v$hx_pastiza = as.numeric(v$hx_pastiza)
hist(log(v$hx_ntltren), breaks = 100)
hist(v$ntl21mean , breaks = 100)
hist(v$g_flt)
plot(v$g_flt, v$hx_ntltren)
library(ggplot2)

ggplot(v ,aes( g_flt , hx_ntltren)) + 
  geom_point() +
  geom_smooth(method = "loess" , colour = "red") +
  scale_x_continuous(limits = c(0,3))+
  theme_bw()+theme(legend.position = "bottom")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 
boxplot(v$hx_ntltren~v$BIOME)

ggplot(v ,aes(fuegomean , Name, color = Name)) + geom_boxplot()+ 
  theme_bw()+theme(legend.position = "bottom")+
  #scale_x_continuous(limits = c(-25,25))+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(plotly)


base$total = base$ab_exotica + base$ab_nativas

cols <- c("SELS A1" = "red","SELS A2" = "red", "SELS A3" = "red", "SELS B1" = "blue", "SELS C1" = "darkgreen","SELS C2" = "darkgreen", "SELS D1" = "orange","SELS D2" = "orange","SELS D3" = "orange", "SELS E1" = "hotpink","SELS E2" = "hotpink","SELS E3" = "hotpink")
ggplot(v , aes(x = xcoord , y = ycoord)) +
  #geom_sf(data = v)+
  geom_point(aes(color = Name , size = trend_p), alpha = 0.5) +
  scale_color_manual( values = cols,
                      aesthetics = c("colour", "fill"))+
  scale_size_area(name = "Nighttime trends", max_size = 15) +
  theme_ipsum() +
  theme(legend.position="right")
cols <- c("SELS A1" = "red","SELS A2" = "red", "SELS A3" = "red", "SELS B1" = "blue", "SELS C1" = "darkgreen","SELS C2" = "darkgreen", "SELS D1" = "orange","SELS D2" = "orange","SELS D3" = "orange", "SELS E1" = "hotpink","SELS E2" = "hotpink","SELS E3" = "hotpink")

ggplot(v ,aes( hxtiempo , NAME, color = NAME_)) + 
  geom_boxplot()+  scale_color_manual( values = cols,
  aesthetics = c("colour", "fill"))+
  theme_bw()+theme(legend.position = "bottom")+
  #scale_x_continuous(limits = c(-25,25))+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 


nc_centers <- st_centroid(base)


#########

id1 <- read.csv("C:/Users/Usuario/Documents/python_scripts/SRI/id_1.csv", header = T)
id2 <- read.csv("C:/Users/Usuario/Documents/python_scripts/SRI/id_2.csv", header = T)

dfN <- merge(id1[,2:26], id2[,2:26], by.x = c("UNQ") , by.y = c("UNQ"))
str(dfN)
dfN<- dfN[-(46:47)]

dfN1 <- cbind(dfN[1], dfN[grepl("\\.x", names(dfN))]/dfN[grepl("\\.y", names(dfN))])
names(dfN1) <- sub("\\.x$", "", names(dfN1))

write.csv(serie2, file = "C:/Users/Usuario/Documents/python_scripts/SRI/serie_ntl2.csv", na = "")


#####calculo la tendencia

serie <- read.csv("C:/Users/Usuario/Documents/python_scripts/SRI/serie_ntl.csv", header = T, sep = ";")

library(reshape2)
serie2 <- melt(serie, id.vars=c("X2000","X2001", "X2002", "X2003", "X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010", "X2011","X2012", "X2013", "X2014", "X2015", "X2016", "X2017", "X2018", "X2019", "X2020", "X2021"))
serie2 <- melt(serie, id.vars=c("UNQ"))
names(serie2$variable) <- sub("x", "", names(serie2))

serie2 <- read.csv("C:/Users/Usuario/Documents/python_scripts/SRI/serie_ntl2.csv", header = T, sep = ";", dec = ",")
serie2[is.na(serie2) | serie2 =="NA"] = NA

library(dplyr)
serie2$UNQ <- as.factor(serie2$UNQ)
all(is.na(serie2$tiempo))
str(serie2)

fitted_models = serie2 %>% group_by(UNQ) %>% do(model = lm(value ~ tiempo , data = .))



######CONVIERTO COLUMNAS EN UNA NUEVA FILA#####

id1 <- read.csv("cattle/id_sumNTLAndes.csv", header = T, sep = ",")
serie2 <- melt(id1, id.vars=c("system.index",  "COUNTRY", "ENGTYPE_1" , "ENGTYPE_2",   
                          "GID_0"   ,     "GID_1"   ,     "GID_2"   ,     "HASC_1"    ,   "HASC_2"   ,    "ISO_1"     ,   "NAME_1"   ,    "NAME_2",       "NL_NAME_1"   , "NL_NAME_2" ,   "TYPE_1"   ,    "TYPE_2"   ,    "VARNAME_1" ,   "VARNAME_2", "fid"  ,   "layer" , "path"  ,       ".geo"))

write.csv(serie2, file = "cattle/sumNTL.csv", na = "")



######Uno conteo de luces con suma#####
serie <- read.csv("cattle/countNtl.csv", header = T, sep = ",")
serie2 <- read.csv("cattle/sumNTL.csv", header = T, sep = ",")
serie2$UNQ <- as.integer(serie2$UNQ)

library(dplyr)

M<- full_join(hex , serie , by = c("COUNTRY", "NAME_1"))
M$lu_ef <- M$sumNTL/M$countNTL#
write.csv(M, file = "cattle/count2.csv", na = "")
