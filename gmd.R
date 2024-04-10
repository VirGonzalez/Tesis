install.packages("gdm")
library(gdm)
library(sf)

df <- read.csv("base_comp.csv", header = T, sep = ",", dec = ".")

str(df)
names(df)


# get columns with xy, site ID, and species data
sppTab <- df[,  c( "Output.Tax", "id","abundancia", "xcoord_2", "ycoord_2")]

## site-species table without coordinates
library(tidyr)
library(dplyr)
library(sf)
sppTab <- sppTab %>% 
  distinct(id, Output.Tax, abundancia) %>%   # Agregamos distinct() para obtener especies únicas por cada ID
  group_by(id, Output.Tax) %>% 
    summarize(UniqueSpeciesAbundance = sum(abundancia))

# Crear columnas para cada especie y asignar 1 o 0 según su presencia
especies_cols <- unique(sppTab$Output.Tax)

datos_grouped <-sppTab %>%
  group_by(id) %>%
  mutate(UniqueSpeciesAbundance) # Agregar una columna presente con valor 1 para cada fil

data_transformed <- datos_grouped %>% 
  pivot_wider(names_from = Output.Tax, values_from = UniqueSpeciesAbundance, values_fill = 0)

# get columns with xy, site ID, and species data
sppTab <- df[,  c( "Output.Tax", "id", "xcoord_2", "ycoord_2")]
##site-species table with coordinates
coords <- unique(sppTab[, 2:ncol(sppTab)])


testData1b <- merge(data_transformed, coords, by="id")


# get columns with site ID, env. data, and xy-coordinates
envTab <- df[, c("id", "tempmean", "precipmean","fundacionm", "acc_mean","xcoord_2", "ycoord_2")]
#envTab <- df[, c("id", "demmean", "tempmean", "precipmean", "xcoord", "ycoord")]
envTab <- unique(envTab)

str(testData1b)

testData1b$id <- as.factor(testData1b$id )




# x-y species list example
gdmTab <- formatsitepair(testData1b, 1, siteColumn="id", XColumn="xcoord_2",
                        YColumn="ycoord_2", predData=envTab)


#gdmTab %>%
#  readr::write_csv(
#    .,
#    file.path(getwd(), "GDMtab.csv")
#  )

gdm.1 <- gdm(data=gdmTab,geo= TRUE)
summary(gdm.1)

data_transformed %>%
  readr::write_csv(
    .,
    file.path(getwd(), "migraciones/data_sppAbun.csv")
  )


######plots#####

length(gdm.1$predictors) # get ideal of number of panels
#> [1] 11
plot(gdm.1, plot.layout=c(4,3))

gdm.1.splineDat <- isplineExtract(gdm.1)
str(gdm.1.splineDat)

# install.packages("Hmisc")
library(Hmisc)

x<- gdm.1.splineDat$x[,"Geographic"]
y<- gdm.1.splineDat$y[,"Geographic"]
df <- data.frame(x, y)

plot(gdm.1.splineDat$x[,"tempmean"], 
     gdm.1.splineDat$y[,"tempmean"], 
     pch = 20, cex = 2,
     col = "hotpink",
     lwd=3,
     type="p", 
     xlab="Mean temperature", 
     ylab="Partial ecological distance"
     )
lines(-4:4, -4:4, lwd = 3, col = "black")
minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)
library(ggplot2)
ggplot(df, aes(x,y)) + 
  #facet_grid(~factor(tipo))+
  geom_point() +
  geom_smooth( method = "lm" , size=0.8, colour = "hotpink") +
  #scale_y_continuous(limits = c(0,3))+
  ylab("Partial Ecological Distance")+ xlab("Geographic distance")+
  #guides(color = guide_legend(title = "Type of urbanization"))+
  theme_bw()+theme(legend.position = "bottom")+
  theme(axis.title.x = element_text(face="bold", vjust=-1.5, size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 
####predict dissimilarity####

gdm.1.pred <- predict(object=gdm.1, data=gdmTab)

head(gdm.1.pred)
#> [1] 0.4720423 0.7133571 0.8710175 0.8534788 0.9777208 0.3996694

plot(gdmTab$distance, 
     gdm.1.pred, 
     xlab="Observed dissimilarity", 
     ylab="Predicted dissimilarity", 
     xlim=c(0,1), 
     ylim=c(0,1), 
     pch=20, 
     col=rgb(0,0,1,0.5))
lines(c(-1,2), c(-1,2))
