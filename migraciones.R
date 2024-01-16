library(sf)
base <- st_read("D:/python_scripts/bases/paises6.shp")
puntos<- st_read("D:/Proy.gbif/gbif/ae_v2/03_space_database_e.csv")

names(base)

length(unique(puntos$Output.Taxon2))

p1 = subset(puntos, country_suggested == "Venezuela")
x = 891579

length(p1)/x*100

######## <UNIR POBLACION> ###############

POB <- read.csv("D:/Tesis/paper_migraciones/mig (2)/pob.csv", header = T, sep = ";")
merge <- merge(base, POB, by.X = c("NAME_0", "PAIS"), by.y = c("NAME_1", "DEP"))

base$proporcion = base$ab_exotica/(base$ab_exotica+base$ab_nativas)

base$inming = base$inmig_m1 + base$inming_f1

base$emig = base$emig_f1 + base$emig_m1 

base$net = base$inming - base$emig

base$migxpob = base$net/base$tp

base$inmig1 <- log(base$inmig)

library(dplyr)
base = base %>%
  mutate_if(is.numeric, scale)

base = base %>%
  mutate_at(vars(one_of("alt_std")), scale)

st_write(base , dsn = "D:/python_scripts/bases/paises6.shp",layer="migraciones", driver="ESRI Shapefile" , overwrite_layer = T)

library(dplyr)
base$inmig <- as.numeric(base$inmig)
base$emig <- as.numeric(base$emig)
names(base)

base <- base %>%
  mutate_at(vars(one_of("precipmean", "alt_std")), scale)


library(ggplot2)
ggplot(base1 ,aes(inmig , fundmean)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess",se=T, fullrange=TRUE, size = 1, aes(color = "pink" )) +
  #facet_wrap(~factor(NAME_0 ))+
  scale_x_continuous(limits = c(0,2.5))+
  xlab("Woody plants species per habitant")+ ylab("Age")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
  theme_bw()+theme(legend.position = "bottom")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 


ggplot(data ,aes(net , radsum )) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess",se=TRUE, fullrange=TRUE, size = 1, aes(color = "#E78AC3")) +
  #scale_y_continuous(limits = c(0,3))+
  xlab("Net migrations")+ ylab("NTL sum")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
  theme_bw()+theme(legend.position = "none")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 

######corplot##########

# install.packages("psych")
library(psych)

df = as.data.frame(base)
corPlot(df[,20:38],stars = TRUE, upper = FALSE)





#####BOXPLOT########

plot <- ggplot(base ,aes(NAME_0 , nativas  , color = NAME_0 )) + 
  geom_boxplot()+
 #scale_y_continuous(limits = c(0,20000))+
  xlab("Country")+ ylab("Native trees abundance")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=3)))+
  theme_bw()+theme(legend.position = "none")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1)))

plot(plot)

########### Grafico de dispersion #########

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(plotly)


base$total = base$ab_exotica + base$ab_nativas

ggplot(base , aes(x = x , y = y)) +
  geom_sf(data = base)+
  geom_point(aes(color = NAME_0 , size = total), alpha = 0.5) +scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A")+ 
  scale_size_area(name = "Total Occurrences", max_size = 10) +
  theme_ipsum() +
  theme(legend.position="right")

nc_centers <- st_centroid(base)




######### MAPA #############
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = 5, name = "RdYlGn")

ggplot(data = base )  +geom_sf(aes(fill= proporcion ), color = NA) +xlab("Long") + ylab("Lat")+ scale_fill_gradientn(name= "Exotic woody plants proportion", 
                                                                                                               colours= my.palette, na.value="grey" , trans = "reverse" )+
  annotation_scale()  +
  annotation_north_arrow(location='tr')+theme_classic()+
  theme_classic()+theme(axis.title.x = element_text(face="bold", vjust=-1, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1, size=rel(1.))) 


library(psych)

corPlot(base[,20:38])


#Fit a linear model by robust regression using an M estimator.
#
base$ntl_hab = base$ntl_count/base$TP
#
#
trama(base1$proporcion , base1$inmig)

base1$inmig1 = log(base$inmig)

m1 = lm(proporcion ~ p_inm +tempmean+ altmean +precipmean +ntl_hab
        , base)  #####MEJOR MODELO 

summary(m1)


########COMPARACION DE MODELOS###########

library(bbmle)


EMV4 = mle2(m1~dnorm(mu,sigma),start = list(mu=15,sigma=4), data=data.frame(m1))

m1 = mle(proporcion ~ p_inm +fundmean + tempmean+ preciprang+ altmean +alt_std +precipmean + ntl_hab + pob + ntltrend
         , base)  #####MEJOR MODELO 


library(olsrr)
res <- ols_step_all_possible(m1)
res

write.csv2(res,"res1.csv")




library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
sjPlot::plot_model(m1,type = "est", 
                   axis.labels=c("Mean temperature", "Precipitation range" , "Immigration(log)", "Age", "Mean altitude" ),vline.color = "grey",
                   show.values=TRUE, value.offset = .5, show.p=TRUE)


m2 = lm(ex_dens ~  ntltrendme, base1 ) ####me gusta


summary(m2)
step(m2)


step(m1)


#########    Modelos mixtos      ##########
names(base)
library(lme4)
r4 <- glmer(proporcion ~  tempmean + log_inmig + pob + fundmean + altmean + (1|selsName), data = base1 , family= binomial(link = "logit"))

summary(r4)

summary(r5)

r4 <- glmer(sp_ex ~  imn.p + ntl_hab  + agemean + tempmean + precipanua + nat_ex + (1|NAME_0),
            data = base )


########Analisis
########

model <- lm ( ex_dens ~  fundmean , data = base1 ) 





######### Hago un raster que sea un mapa de calor#########
######### 


library(MASS)

library(raster)
base = st_read("D:/Tesis/paper_migraciones/mig (2)/mig/puntos.shp")

base_f <- st_transform(base, crs = 22183)
library(spatstat)
W <- as.owin(base_f$geometry[1]) # First county of North Carolina data set in spatstat format
X <- runifpoint(100, win = W)
plot(X, "Random points")

library(ggplot2)

ggplot(data = base) +
  geom_sf() +
  theme_bw() +
  stat_density_2d(mapping = ggplot2::aes(x = purrr::map_dbl(geometry, ~.[1]),
                                         y = purrr::map_dbl(geometry, ~.[2]),
                                         fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 0.5)



#####CURVA ESPECIE ABUNDANCIA#################


sp_occ <-st_read("D:/Proy.gbif/gbif/Arboles_exoticos/data_e.shp")

length(unique(puntos$accepted_1))
a = length(sp_occ$database_i)

p1 = subset(sp_occ, country_su == "Colombia")

b = length(p1$database_i)
c = b/a*100

ibrary(dplyr)

library(tidyr)

df2 <- sp_occ %>% 
  group_by(country_su) %>% 
  count(accepted_1) 


abundance <- dplyr::count(sp_occ , accepted_1)            # Applying count function

library(data.table)
fwrite(df2, "abundancia_exoticas.csv")


library(vegan)
###abundance <- as.data.frame(t(abundance))
library("tidyverse")  

abundance <- df2[,2:3] 
nombres <- names(abundance)[-c(1,1)]

# Fit species accumulation curve
library(vegan)
# Create a matrix we can use with vegan

# Load the packages
library(vegan)
library(picante)
library(knitr)
 df2= df2  %>% select("country_su", "n", "accepted_1")

ec.matrix <- sample2matrix(df2)
curve <- specaccum(ec.matrix, method = "random", permutations = 1000)
curve


plot(curve, ci.type = "poly", col = "blue", ci.col = "lightblue", 
     lwd = 2, ci.lty = 0, xlab = "Paises", 
     ylab = "cumulative number of exotic tree species")

# Plot
# 
curve.df <- data.frame(sites = curve$sites,
                               richness = curve$richness,
                               sd = curve$sd)


ggplot(df2 , aes(x = accepted_1 , y = n , colour = country_su )) +
  # Add line
  geom_point() +
  geom_smooth(method = "lm")+
  # Add confidence interval+
  # Remove grey background
  theme_bw(base_size = 14)



RpA<- radfit(df2[,])
RpA

plot(RpA$models$Mandelbrot , xlim=c(0,250), pch=19, col="pink", cex=0.6)

###Calculo indice de diversidad
diversity(abundance, "simpson")

library(metacoder)
abundancia <- parse_tax_data(abundancia_exoticas , class_sep = ";", class_cols = "accepted_1")

abundancia %>%
  metacoder::filter_taxa(n > 0) %>% # metacoder:: needed because of phyloseq::filter_taxa
  heat_tree(node_label = accepted_1,
            node_size = n, 
            node_color = n, 
            layout = "da", initial_layout = "re", 
            title = "Taxa in leafs")


######3plot heatmap
######

