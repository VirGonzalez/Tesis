
#Esto es para indicar la carpeta donde estan los datos
setwd("C:/Users/LENOVO/Desktop/radios/clasificaciones/USAR_RASTER/prueba88_2020")


library(raster)
library(rgdal)
library(sp)
library(arm)

#cargo todo los rasters que tengo en esa carpeta, en la carpeta puse solamente los rasters que iba a usar
rasters <- list.files(path ="nuevo_analisis", pattern='.tif', full.names=TRUE)

rasters <- stack(rasters )

#creo un data frame con latitud longitud de los puntos (bovi es uno de los rasters de la carpeta)
latlong <- coordinates(rasters$bovi)
latlong <- as.data.frame(latlong)

names(datos)
#transformo en data frame todos los rastes
rasterci <- as.data.frame(rasters)
#voy a introducir elevacion como temrino cuadratico por eso creo una capa con dem^2
rasters$dem2 <- rasters$dem^2

#escalo todos los datos para estandarizar
raster1 <- scale(rasters)




names(rasters)
#los rasters scalados los hago data frame
datos <- as.data.frame(raster1)

#los raster que estan a continuacion no los necesitaba estandarizado, queria los valores originales porque son las clasificacones, entonces los reemplace por las capas sin estandarizar
datos$nativo_nuevo<- rasterci$nativo_nuevo
datos$paranat_88_nuevo_ <- rasterci$paranat_88_nuevo_
datos$bosq_0a2 <- rasterci$bosq_0a2
datos$bosq0a3_nosubset <- rasterci$bosq0a3_nosubset
datos$lat <- latlong$x
datos$long <- latlong$y
datos$fin88buf_nuevo_ <- rasterci$fin88buf_nuevo_
#esta es una mascara que estaba usando para tapar las zonas que siempre fueron bosque:
datos$mask_multi <- rasterci$mask_multi
names(datos) 
summary(datos$fin88buf_nuevo_)
summary(datos)
##########################################MODELO 

memory.limit()

#esto sirve para auumentar la memoria en r
memory.limit(size=40000)


    
library(mgcv)
#probe varios modelos, siempre introduciendo solo la lat, log como temrino suavizado, "te" es un tipo de suavizado, habia visto que este era mejor para lat long

model_lig_c3<- gam(ligustro_nuevo~ te(lat, long)+dem+dem2+norte+slope+d_lig_nuevo+ganado19+ganado_tend+d_pob+pob10+d_sec+sec91+rural+urb,
                   family = binomial(link=logit),subset = !is.na(mask_multi), data = datos )
exo<- gam(list(bosq_0a2 
               ~ te(lat, long) + dem +
                 d_lig_nuevo,
               ~te(lat, long) + dem  + 
                  d_gled_nuevo ),
                   family=multinom(K=2),data = datos)
 summary(exo)                  
mc<- gam(list(bosq0a3_nosubset 
               ~ te(lat, long) + dem +dem2+ norte + slope +
                 d_nat_nuevo  + urb + rural +pob10+
                 d_pob + d_sec + ganado19+ganado_tend,
               ~te(lat, long) + dem + dem2+ norte + slope +
                 d_lig_nuevo + urb + rural +pob10+
                 d_pob + d_sec + ganado19+ganado_tend,
               ~te(lat, long) + dem + dem2+ norte + slope +
                 d_gled_nuevo + urb + rural +pob10+
                 d_pob + d_sec + ganado19+ganado_tend),
          family=multinom(K=3),data = datos)


                    



#aqui tengo los predict del modelo, pero estan en data frame, tengo que creear un raster y llenarlo con estos datos
predict <-predict.gam(exo, type = "response")



#revisar los na de cada grupo (tambien se tienen que cargar los NA, aqui estoy buscando la ubicacion, si todas tus capas son iguales fijate donde estan los NA de una)
nadem <- which(is.na(datos$dem))





#creo nuevo vector, de la longitud de mi raster #bosq_0a2, asigno na y desp datos de prob
nuevo1 <- numeric(length(datos$bosq_0a2))
#aqui lo que digo es, en la posicion que estan los NA del dem, pone NA (EN el vector que va a ser mi raster de probabilidades depsues)
nuevo1[nadem] <- NA




#busco los no NAs del nuevo vector, es decir, donde voy a tener datos de probabilidad y los cargo
no_na<-which(!is.na(nuevo1))

#aqui estoy asignado los datos de probabilidad a las celdas que tenian datos (las que no eran NA)
nuevo1[no_na] <- predict


#armo una matrix (los numeros de filas y columnas lo sacas de tu raster original)
prelig<- matrix(nuevo1, nrow = 1598, ncol = 1794, byrow = TRUE)

#los paso a raster, los valores xmn, xmx, ymn, ymx y crs los saque de mi raster original

prel <- raster(prelig,xmn=3523260, xmx=3577080, ymn= 7011570, ymx=7059510,  
                   crs="+proj=tmerc +lat_0=-90 +lon_0=-66 +k=1 +x_0=3500000 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" )


#LISTO, DEBERIAS PODER VISUALIZARLOS
plot(prel)

#CON ESTA FUNCION GUARDAS TU RASTER CON VALORES D PROBABILIDAD
writeRaster(predictn, filename = "nuevo_analisis/predic_gam.tif", format="GTiff")

