library(sf)

base <- st_read("v2/data1.shp")

names(base)

data %>%
  group_by(Ciudad) %>%
  summarize(mean_mpg = mean(densidad01, na.rm = TRUE))

base$max_niv_ed <- as.numeric(base$max_niv_ed)


library(dplyr)
data <- base %>%
  mutate_at(vars(one_of( "nbi", "densidad" , "max_niv", "cal", "densidad01")), scale)


#probe varios modelos, siempre introduciendo solo la lat, log como temrino suavizado, "te" es un tipo de suavizado, habia visto que este era mejor para lat long
names(data)

library(ggplot2)
ggplot(aes(y = perc_ndvim , x = Ciudad, color = Ciudad), data = base) + geom_boxplot(alpha=0.2)+ xlab("Ciudad")+ ylab("Variacion de incremento porcentual de NDVI")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+ scale_color_brewer(palette="Paired")+ theme(legend.position="none")+
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.3))) 

ggsave("densidad.jpg")


library(mgcv)

data1 <- subset(data, Ciudad == "Salta")
names(data1)
summary(data1)
####saco medidas resumen######

mean(data1$perc_ndvim)
sd(data1$perc_ndvim)
mean(data1$max_niv_ed, na.rm = T)
sd(data1$perc_ndvim)
mean(data1$cal_mat, na.rm = T)
mean(data1$nbi , na.rm = T)
mean(data1$densidad , na.rm = T)
mean(data1$densidad01 , na.rm = T)
data$Ciudad <- as.factor(data$Ciudad)
model<- gam(perc_ndvim ~ te(xcoord , ycoord)+ densidad +cal + max_niv + nbi + densidad01 + densidad + s(Ciudad, bs = 're'),
                  family = gaussian(), data = data, method = 'REML')

model1<- gam(perc_ndvim ~ te(xcoord , ycoord)+ densidad +cal + max_niv + nbi + densidad01,
            family = gaussian(), data = data1)

summary(model1)
coef(model)

summary(data1$perc_ndvim)

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(model,type="est", show.values = T, show.p = TRUE, value.offset = .3, value.size = 4.5, axis.title ="Estimate value", vline.color = "grey",axis.labels=c("Initial density","UBN change" , "Mean Educational change", "Household quality change","Population Density change"))

plot_model(model_salta, type="est",transform = NULL,title = "Great Salta", terms = c("densidad", "CALIDAD" , "EDUC", "NBI", "dnsd01"),  
           axis.labels=c("Population Density change" , "Household quality change","Mean Educational change", "UBN change" ,"Initial density" , "." 
           ),          
           show.values = T, show.p = TRUE, value.offset = .3, value.size = 4.5, axis.title ="Estimate value", vline.color = "grey")+ theme_bw()

tab_model(model, transform = NULL, auto.label = FALSE, show.est = TRUE, digits = 5, p.style = "scientific_stars", 
          dv.labels = c("relative vegetation trend (Great Rosario)","relative vegetation trend (Great Rosario) (Great Cordoba)") ,  pred.labels = c("Intercept","Urbanization level change" , "Household quality change","Mean Educational change", "UBN change" ,"Initial density" , "."), file = "modelo3.doc")


library(meta)

m <- read.csv("v2/meta.csv", sep = ";")

library(ggplot2)

# Crear un gráfico de barras con barras de error
ggplot(m, aes(x = X, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=Lower,ymax=Upper),width=0)+ 
  labs(x = "Socioeconomic and demographic predictors",
       y = "Effect size") +
  theme_minimal() +theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()  # Rotar todo el gráfico

#re-order factor levels for region
m$City <- factor(m$City , levels=c("Great Salta","Great San Miguel de Tucuman", "Great Rosario","Great Cordoba" , "Great Buenos Aires", "Global effect"))



# Crea el "forest plot"
library(forestplot)
m |>
  group_by(City) |>
forestplot(
  labeltext = c("X"),
  mean = "mean",           # Nombre de la columna de estimaciones medias
  lower = "Lower",         # Nombre de la columna de límites inferiores de los intervalos
  upper = "Upper",       # Nombre de la columna de límites superiores de los intervalos
  title = "Forest Plot de Estudios",  # Título del gráfico
  boxsize = 0.18,             # Tamaño de las cajas
  line.margin = .005, # We need to add this to avoid crowdin
  xlab = "Effect size",
  txt_gp = fpTxtGp(label = gpar(fontsize = 12), xlab = gpar(fontsize = 14), title = gpar(fontsize = 16)))|>
  fp_set_style(box = c("steelblue", "darkred", "darkgreen", "hotpink", "purple") |> lapply(function(x) gpar(fill = x, col = "#555555")),
               default = gpar(vertices = TRUE))|>
  fp_set_zebra_style("#EFEFEF")


library(forestplot)
colores_estudios <- c("Great Salta" = "#E69F00", "Great San Miguel de Tucuman" = "#56B4E9", "Great Rosario" = "#009E73", "Great Cordoba" = "#D55E00","Great Buenos Aires" = "#CC79A7", "Global effect" = "black")
# Asigna colores a las filas según el estudio
m$Color <- colores_estudios[m$City]

forestplot(m,
           labeltext = "City",
           mean = "mean",           # Nombre de la columna de estimaciones medias
           lower = "Lower",         # Nombre de la columna de límites inferiores de los intervalos
           upper = "Upper",       # Nombre de la columna de límites superiores de los intervalos
           title = "Forest Plot de Estudios",  # Título del gráfico
           #xticks = c(0, 0.2, 0.4, 0.6, 0.8, 1),  # Personaliza las marcas en el eje X
           boxsize = 0.3            # Tamaño de las cajas
           )|>
fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue")|> 
  fp_set_zebra_style("#EFEFEF")  


library(ggforestplot)
#devtools::install_github("NightingaleHealth/ggforestplot")

ggforestplot::forestplot(
  df = m,
  name = X,
  estimate = mean,
  se = sd,
  xlab = "Effect size",
  colour = City,
  title = "Environmental outcomes of socioeconomic and demographic trajectories (1999-2020)"
)+
  theme_minimal() + theme(legend.position="bottom")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 



#####

# Crear el gráfico
ggplot(data = m, aes(x = mean, xmin = mean - 1.96 * sd, xmax = mean + 1.96 * sd, y = X, colour = City)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbarh(position = position_dodge(width = 0.2), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray") +  # Línea vertical en cero
  scale_color_manual(values = setNames(colores_estudios, levels(m$City))) +  # Asignar colores
  labs(x = "Effect size", y = NULL, color = "City") +
  theme_bw() +  # Cambiar a theme_bw
  theme(legend.position = "bottom", axis.title.x = element_text(face = "bold", vjust = -1.5, size = rel(1))) +
  theme(axis.title.y = element_text(face = "bold", vjust = 1.5, size = rel(1)))



library(ggplot2)
library(viridis)
base <- st_read("v2/tuc2000.shp")

data <- subset(base, Ciudad == "Cordoba")
sf::st_is_valid(data)
# Create breaks for the color scale


ggplot(data, aes(x = xcoord_2, y = ycoord_2)) +
  geom_sf(fill = "white")+
  geom_point(aes(color = perc_ndvim, size = max_niv, alpha = max_niv ),show.legend = TRUE, shape = 20) +
  scale_size_continuous(name = "Socioeconomic status change", range = c(1,8)) +
  scale_color_viridis(option="magma", name="Relative vegeatation trend", direction = -1) +
  theme_void() +coord_sf() +
  theme(legend.position = "right")


ggplot(data, aes(x = xcoord, y = ycoord)) +
  geom_sf(fill = "white",show.legend = FALSE)+
  geom_point(aes(color = perc_ndvim , size = max_niv ),show.legend = TRUE,shape=20) +
  scale_color_distiller(palette = "BuPu") +
  theme_void() +coord_sf() +
  theme(legend.position = "right")


#####valores iniciales


ggplot(base, aes(x = xcoord , y = ycoord)) +
  geom_sf(fill = "white")+
  geom_point(aes(color = ndvi2000me, size = mx_nv01 , alpha = mx_nv01 ),show.legend = TRUE, shape = 20) +
  scale_size_continuous(name = "Initial socioeconomic status", range = c(1,10)) +
  scale_color_viridis(option="magma", name="Initial vegetation", direction = -1) +
  theme_void() +coord_sf() +
  theme(legend.position = "right")

library(ggplot2)
library(ggspatial)
library(ggthemes)
library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = 5, name = "RdYlGn")

ggplot(data = data )  +geom_sf(aes(fill= perc_ndvim), color = NA) +xlab("Long") + ylab("Lat")+ scale_fill_gradientn(name= "Relative vegetation trend",                                                                     colours= my.palette, na.value="grey" )+
  annotation_scale()  +
  annotation_north_arrow(location='tr')+theme_classic()+
  theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.))) 

ggplot(data = data )  +geom_sf(aes(fill= max_niv), color = NA) +xlab("Long") + ylab("Lat")+ scale_fill_gradientn(name= "Socioeconomic status change",                                                                     colours= my.palette, na.value="grey" )+
  annotation_scale()  +
  annotation_north_arrow(location='tr')+theme_classic()+
  theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.))) 
