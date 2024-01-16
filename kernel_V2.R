library(raster)
r <- raster("D:/Agua/nbi_st.tif")
plot(r)
w<-matrix(1,15,15)

f1 <- focal(r, w, fun = median , filename='kernel',na.rm=T, overwrite = TRUE)

plot(f1)


rc <- function(r) {
  ifelse(  r < 1.01 & r >= 0.5 , 0, ifelse( r < 0.5 & r >= 0.44 , 1,
                                 ifelse( r < 0.44 & r >= 0.35, 2,
                                         ifelse( r < 0.35 & r >= 0.01, 3 , NA) ) )) 
  }

myColors <- c('blue', 'green','grey', 'yellow', 'hotpink') 
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
valor_asignado <- 13

# Convertir el polígono a un objeto raster con las mismas dimensiones que el raster original
poligono_raster <- rasterize(poligono_data, raster_data, field = 1)
plot(poligono_raster)

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




install.packages("raster")
library(raster)

# Cargar el raster y el polígono
raster_file <- "D:/Agua/clases_swat.tif"
poligono_file <- "D:/Agua/bosques.shp"

raster_data <- raster(raster_file)
plot(raster_data)
poligono_data <- shapefile(poligono_file)
plot(poligono_data)

# Obtener las coordenadas del raster
raster_ext <- extent(raster_data)
x <- seq(raster_ext[1], raster_ext[2], by = res(raster_data)[1])
y <- seq(raster_ext[3], raster_ext[4], by = res(raster_data)[2])

# Crear un raster vacío con las mismas dimensiones que el raster original
poligono_raster <- raster(nrows = raster_data@nrows, ncols = raster_data@ncols, ext = raster_ext)

# Rasterizar el polígono y asignar valores
poligono_raster <- rasterize(poligono_data, poligono_raster, x, y, fun = function(x, ...) { 1 }, update = TRUE)
plot(poligono_raster)

# Definir el valor a asignar a los píxeles que no se solapan
valor_no_solapado <- 6

# Asignar valor a los píxeles no solapados del polígono raster que no se solapan con los píxeles con valor 1 del raster original
raster_modificado <- raster_data
raster_modificado[(poligono_raster == 1) & (raster_modificado != 1)] <- valor_no_solapado
plot(raster_modificado)

# Guardar el raster modificado
nuevo_raster_file <- "D:/Agua/clases_swat2.tif"
writeRaster(raster_modificado, nuevo_raster_file, format = "GTiff", overwrite = TRUE)
