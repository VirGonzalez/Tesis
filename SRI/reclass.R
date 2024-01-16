###reclasificar raster
library(raster)
r1 = raster("urb_degree1.tif")
plot(r1)

rc <- function(x) { 
  ifelse(x %in% c('30', '23', '22'), 1,
         ifelse(x %in% c('21'), 2,
                ifelse(x %in% c('13', '12', '11', '10'), 3, NA)))
}

r.class <- overlay(r1, fun=rc)
# Aplicar la reclasificación
# Aplicar la reclasificación
r1_reclasificado <- reclassify(r1, rc)

# Visualizar el raster original y el reclasificado
par(mfrow=c(1,2))
plot(r1, main="Raster Original")
plot(r1_reclasificado, main="Raster Reclasificado")

# Aplicar la reclasificación
# Definir la tabla de reclasificación
# Definir la tabla de reclasificación
reclass_table <- c(-Inf, 21, 0, 21, 21, 1, 22, Inf, 3)

# Aplicar la reclasificación
r_reclassified <- reclassify(r1, rcl = reclass_table, right = NA)

# Reemplazar los NA por 0
r_reclassified[is.na(r_reclassified)] <- 0


# Visualizar el resultado
plot(r_reclassified)

# export as tiff
# Supongamos que r1_reclassified es tu raster final
writeRaster(r_reclassified, "urdegre_i.tif", format = "GTiff",overwrite=TRUE)
