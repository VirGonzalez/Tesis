library(sf)

base <- st_read("v2/data.shp")

names(base)

data %>%
  group_by(Ciudad) %>%
  summarize(mean_mpg = mean(densidad01, na.rm = TRUE))

base$max_niv_ed <- as.numeric(base$max_niv_ed)
base <- read.csv("v2/base_v2.csv", sep = ";")


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

data1 <- subset(data, Ciudad == "Buenos Air")
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
model<- gam(perc_ndvim ~ te(xcoord , ycoord)+ densidad + max_niv +cal + nbi + densidad01 + densidad + s(Ciudad, bs = 're'),
                  family = gaussian(), data = data)
summary(model)

model1<- gam(perc_ndvim ~ te(xcoord , ycoord)+ densidad + max_niv +cal+ nbi + densidad01,
            family = gaussian(), data = data1)

summary(model1)
coef(model)


summary(data1$perc_ndvim)

######llevar a un word
# Instalar y cargar las bibliotecas necesarias
install.packages("officer")
library(officer)

# Obtener el resumen del modelo como texto
resumen_texto <- capture.output(summary(model))


# Crear un nuevo documento de Word
FileCon = file("modelo6.txt")
writeLines(resumen_texto, FileCon)
close(FileCon)







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
          dv.labels = c("relative vegetation trend (Great Rosario)","relative vegetation trend (Great Rosario) (Great Cordoba)") ,  pred.labels = c("Intercept","Urbanization level change" , "Household quality change","Mean Educational change", "UBN change" ,"Initial density" , "."), file = "modelo1.doc")
tab_model(model1)
tab_model(model1, transform = NULL, auto.label = FALSE, show.est = TRUE, digits = 5, p.style = "scientific_stars" ,  pred.labels = c("Intercept","Urbanization level change" , "Wealth change","Educational level change", "Structural poverty change" ,"Initial urbanization level", "."), file = "modelo1.doc")

library(meta)

m <- read.csv("v2/meta.csv", sep = ";")


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


ggplot(data, aes(x = xcoord, y = ycoord)) +
  geom_sf(fill = "white")+
  geom_point(aes(color = perc_ndvim, size = cal_mat, alpha = cal_mat),show.legend = TRUE, shape = 20) +
  scale_size_continuous(name = "Wealth change", range = c(1,6)) +
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
