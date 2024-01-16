require(dplyr)
r <- raster::stack("Tesis/PCA/stack_III.tif")
r2 <- raster::as.data.frame(r,xy=TRUE, na.rm = TRUE)
colnames(r2)<- c( "radiancia","inicial","final", "tiempo","tasa", "textura","distancia", "built", "height", "vol", "footprint","ev_area")
# remove rows with zeros in x, y, or z
r2 <-  r2 %>% filter (x > 0, y > 0)
# center and scale the data


head(r2)

r2 <- r2[,(1:12)]
r2 <- r2[,c(1,5,6,7,8,9,10,11,12)]

##hago matriz de covarianza
matriz_cov <- cov(r2)
matriz_cov

eigen <- eigen(matriz_cov)
eigen$values

eigen$vectors

t_eigenvectors <- t(eigen$vectors)
t_eigenvectors

t_r2 <- t(r2)
t_r2

# Producto matricial
pc_scores <- t_eigenvectors %*% t_r2
rownames(pc_scores) <- c("PC1", "PC2","PC3", "PC4", "PC5","PC6","PC7",
                         "PC8", "PC9", "PC10", "PC11", "PC12")

# Se vuelve a transponer para que los datos estén en modo tabla
t(pc_scores)


####calculo PCA
pca <- prcomp(r2, scale = TRUE, center = TRUE)
names(pca)
pca$center
pca$scale

#rotation contiene el valor de los loadings ϕ para cada componente (eigenvector). 
pca$rotation
head(pca$x)

prop_varianza <- pca$sdev^2 / sum(pca$sdev^2)
prop_varianza
library(ggplot2)
ggplot(data = data.frame(prop_varianza, pc = 1:9),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")

prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum
ggplot(data = data.frame(prop_varianza_acum, pc = 1:9),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")


####GRAFICAS#########
library(factoextra)

fviz_pca_biplot(pca, repel = F, col.var = "black", col.ind = "gray")
fviz_eig(pca,addlabels = TRUE)
fviz_pca_var(pca,axes = c(1, 2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(pca, axes = c(2, 3), repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
biplot(x = pca, scale = 0, cex = 0.7, col = c("blue4", "red"))
#CONTRIBUCION DE CADA VARIABLE
fviz_contrib(pca , choice = "var", axes = 1, top = 12)
fviz_contrib(pca, choice = "var", axes = 2, top = 12)
fviz_contrib(pca, choice = "var", axes = 3, top = 12)
fviz_contrib(pca, choice = "var", axes = 4, top = 12)
fviz_contrib(pca, choice = "var", axes = 5, top = 12)
fviz_contrib(pca, choice = "var", axes = 6, top = 12)
fviz_contrib(pca, choice = "var", axes = 7, top = 12)

###Grafica de variables

var <- get_pca_var(pca)

#contribuciones
head(var$contrib)

library("corrplot")
corrplot(var$contrib, is.corr=FALSE)


###Agrupar por coordenadas

res.km <- kmeans(var$coord, centers = 4, nstart = 25)
grp <- as.factor(res.km$cluster)
fviz_pca_var(pca,axes = c(1, 2), col.var = grp,arrowsize = 1, labelsize = 5,
             axes.linetype = "blank",
             palette = c("#00AFBB","#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

plot(r2$tiempo)


gmFstat <- PatchStat(as.matrix(grid), cellsize=30)
