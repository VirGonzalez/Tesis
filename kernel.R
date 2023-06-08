library(raster)
r <- raster("built_tuc.tif")
plot(r)
values(r) <- runif(ncell(r)) 
w<-matrix(1,27,27)

f1 <- focal(r, w, fun = median , filename='kernel',na.rm=T, overwrite = TRUE)

plot(f1)

rc <- function(r) {
  ifelse( r => 0.67 , 7, ifelse( r >= 0.6 , 1, 
          ifelse( r == 0.5 , 5,
                  ifelse( r >= 0.38 , 2,
                          ifelse( r >= 0.2 , 3,ifelse( r >= 0.12 , 4, 6))))))
}

rc <- function(r) {
  ifelse( r >= 0.98 , 0, ifelse( r >= 0.84 , 2,
                                 ifelse( r == 0.67 , 3,
                                         ifelse( r >= 0.6 , 4,
                                                 ifelse( r >= 0.51 , 5,
                                                         ifelse( r >= 0.38 , 6,
                                                                 ifelse( r >= 0.2 , 7,ifelse( r >= 0.12 , 8, NA) ) )))))) 
  }

myColors <- c('blue', 'green', 'yellow', 'red', 'hotpink', "brown", "black", "white") 
r.class <- overlay(f1, fun=rc)
plot(r.class, col = myColors)

plot.new() 
par(xpd=T)
legend('left',inset = 0,
       legend = c('1', '2', '3', '4', '5'), 
       fill=myColors, cex=2, horiz = F)


rf <- writeRaster(r.class, filename=file.path("/Agua/clases_swat.tif"), format="GTiff", overwrite=TRUE)


# Instalar y cargar las librerías necesarias
install.packages("raster")
library(raster)

# Cargar el raster y el polígono
raster_file <- "clases_swat2.tif"
poligono_file <- "act_prod/citrus.shp"

raster_data <- raster(raster_file)
plot(raster_data)
poligono_data <- shapefile(poligono_file)

# Definir el valor a asignar a los píxeles solapados
valor_asignado <- 10

# Convertir el polígono a un objeto raster con las mismas dimensiones que el raster original
poligono_raster <- rasterize(poligono_data, raster_data, field = 1)

# Identificar los índices de los píxeles solapados en el polígono rasterizado
indices_solapados <- which(!is.na(getValues(poligono_raster)))

# Asignar el valor especificado a los píxeles solapados en el raster original
raster_data[indices_solapados] <- valor_asignado
plot(raster_data)
# Guardar el raster modificado
nuevo_raster_file <- "clases_swat2.tif"
writeRaster(raster_data, nuevo_raster_file, format = "GTiff", overwrite = TRUE)

# Imprimir un mensaje de confirmación
cat("El proceso ha finalizado. El raster con los valores modificados ha sido guardado en:", nuevo_raster_file)
