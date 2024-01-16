cor <- raster("C:/Users/pc/Dropbox/PC/Documents/Paper/tucCampero.tif")
library(RColorBrewer)
my.palette <- brewer.pal(n = 20, name = "RdYlGn")

plot(cor,col = my.palette )


library(ggplot2)
ggplot() +
  geom_raster(data = cor , aes(x = x, y = y, fill = tucCampero)) + 
  coord_quickmap()
