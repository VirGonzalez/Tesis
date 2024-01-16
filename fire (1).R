setwd("C:/Users/pc/Dropbox/PC/Documents/Doctorado/Paper Biodiversidad Urbana/Analisis")
library(sf)
data <- st_read("C:/Users/pc/Documents/Fuego/hex_fuegoacc.shp")
library(ggplot2)
names(data)
ggplot(data ,aes(fuegomean ,accessmean)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "loess" , size=0.8, colour = "black") +
  scale_x_continuous(limits = c(1,3))+
  xlab("fuego")+ ylab("access")+
  theme_bw()+theme(legend.position = "none")

m1 = lm(fuegomean ~ GDP20 + accessmean + temp + prec  , data = data)
summary(m1)





































