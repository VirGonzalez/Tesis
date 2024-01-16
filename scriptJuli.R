library(raster)
library(rgdal)
#llamas tu archivo raster primero y despues el shape. En tu caso los radios y los LST
r.vals <- extract(salta , rsalta ) #colocas primero el nombre del shape y luego del raster
r.mean<- unlist(lapply(r.vals, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
#ahora estima esto el promedio de los valores de tu raster para cada poligono de tu shape
#creo un dataframe 
rsalta@data <- data.frame(rsalta@data, Pendiente=r.mean) #aca creas la columna que tendra esos valores
#en mi caso era la pendiente de cambio de productividad
writeOGR(rsalta, ".", "saltats1", driver="ESRI Shapefile", check_exists=TRUE, 
         overwrite_layer=TRUE) #aqui lo volves a escribir a tu shape ya modificado con esa nueva columna

#este es para rasterizar el poligono
#rasterizar un poligono
#Primero abro el raster y un shape de referencia
library(raster)
cor <- shapefile("C:/Users/LENOVO/OneDrive/Documentos/BuengeBorn/BuengeBorn/Tradios/SMTucuman 10.shp")
y = shapefile("C:/Users/LENOVO/OneDrive/Documentos/BuengeBorn/EVREDEFINIDOS/INDECRC_gsmt")
#aqui le digo que quiero que mi raster tenga la proyeccion y extension del shape
rst_template <- raster(ncols = 500, nrows = 500, 
                       crs = projection(cor), 
                       ext = extent(cor))
#rasterizo y le digo que valor quiero que tengan los pixeles
rst_germany <- rasterize(cor, rst_template, 'INSE_C10_1')
#ploteo mi raster
plot(rst_germany, col = "grey75", legend = FALSE, xlab = "lon", ylab = "lat")
plot(rasterToPolygons(rst_germany), add = TRUE)
#guardo el raster
writeRaster(rst_germany, "C:/Users/LENOVO/OneDrive/Documentos/BuengeBorn/EVREDEFINIDOS/inse.tif", overwrite = TRUE)

#este es para hacer la regresion
library(ggplot2)
library(ggthemes)
m = lm(INSEmean~ X_arb   , data= ev )
summary(m)
#hago algunos graficos de tipo scatterplot
p= ggplot(data= ev, aes(x=INSEmean, y= X_arb)) +
  geom_point() + 
  labs(x = "INSE ", y = "Porcentaje de superficie arbolada") + #le digo el nombre de los ejes
  geom_smooth(method = "lm" , fullrange=FALSE, level=0.95)+ theme_classic()
library(ggpubr)
p+stat_cor(method="pearson") #que aparezca el p value

#MAPEO
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = 5, name = "GnBU") #ESTO ES PARA ELEGIR UNA PALETA DE COLOR
#MAPEO si no pongo nada en fill solo me mapea el poligono ahora si pongo
#fill = variable puedo decirle que me coloree el mapa de acuerdo por ej al NDVI
#pongo fill = "NDVI"
ggplot(data = ev)  +
  geom_sf(aes(fill= "Espacios Verdes del Gran San Miguel de Tucuman"))+ xlab("Longitud") + ylab("Latitud")+
  annotation_scale() +annotation_north_arrow(location='tr') 

ggsave("map.pdf", plot=p, width=800, height=400, dpi=150, limitsize= FALSE)

#ASI
ggplot(data = ev)  +
  geom_sf(aes(fill= areatotalE) )+xlab("Longitud") + ylab("Latitud")+
  scale_fill_gradientn(name= "Superficie de EV por RC (km2) ", 
                       colours=brewer.pal(7,"RdYlGn") )+
  annotation_scale() +
  annotation_north_arrow(location='tr') 


library(tmap)
library(tmap)
#ev es mi shape
m= qtm( ev , fill = "green" , title="Espacios Verdes Publicos del GSMT")+tmap_options(check.and.fix = TRUE) + tm_scale_bar(position = c( "left", "bottom"))
lf5 <- tmap_leaflet(m )
lf5
#guardo mi mapa interactivo en un html
library(htmlwidgets)
saveWidget(lf5, file="EVP-TUC.html")
#este es para crear el mapa interactivo