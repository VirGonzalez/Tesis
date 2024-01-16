library(raster)
library(rgdal)
library(ggplot2)
library(reshape)
library(scales)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr) 
library(sf)

cor <- stack("C:/Users/pc/Dropbox/PC/Documents/Paper/corTS22.tif")
plot(cor, ylim = c(-0.01, 0.01))
hist(cor)
rbrick <- cor#asignar a la variable rbrick
hist(cor)
#spplot(rbrick, zlim=c(0,550), col.regions=colorRampPalette(c("white", "blue"))(255))
#Analisis temporal
library(raster)
names(rbrick)
## the 1 is to get a slope. 
## 1:nlyrs(s) is the independent variable  
library(terra)
time <- 1:nlayers(rbrick)
time
x = values(rbrick)
x

#regresion SIN NA
# run the regression
#fun <- function(x) { if (is.na(x[1])){ NA } else {lm(x ~ time)$coefficients[2] }} 
#x2 <- calc(sicily, fun)
#funcion CON NA
fun2 <- function(x) { 
  d <- na.omit(cbind(x, time))
  if (nrow(d) > 2) {
    lm(x ~ time, data=data.frame(d))$coefficients[2] 
  } else {
    NA
  }
} 
#rbrick[1:20] <- NA
x2 <- calc(rbrick, fun2)
plot(x2)
plot(x2,col = my.palette )

writeRaster(x2 ,'C:/Users/pc/Dropbox/PC/Documents/Paper/cordoba.tif')

cor <- raster("C:/Users/pc/Dropbox/PC/Documents/Paper/cordoba.tif")
rbrick <- cor#asignar a la variable rbrick
spplot(rbrick, zlim=c(0,550), col.regions=colorRampPalette(c("red", "green"))(255))
#rmse = sqrt( mean( (sim - obs)^2, na.rm = TRUE) )
#plot(RMSE,type="p" )
#lines(RMSE)

#veo las distintas bandas y sus caracteristicas




#ploteo la serie temporal

x.stats <- data.frame(x.mean=cellStats(cor, "mean"), time = 1:nlayers(rbrick)) 
time = c(1999, 2000,2001,2002, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2013, 2014, 2015,
         2016, 2017, 2018, 2019, 2020 )
library(ggplot2)
library(dplyr)
library(hrbrthemes)

plot(x.stats$time, x.stats$x.mean)
p <- ggplot(x.stats , aes(x=time, y=x.mean)) +
  geom_line( color="#69b3a2") + 
  geom_point() +
  xlab("Numero de años")+
  theme(axis.text.x=element_text(angle=60, hjust=1))
p
