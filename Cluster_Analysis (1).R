library(sf)
grid <- st_read("Tesis/PCA/ai2.shp")
grid <- grid[,c(35,36,37,38,39,40,43,57)]

cluster_vars <- grid %>%
  st_set_geometry(NULL) 
  
  
head(cluster_vars,10)
library(BBmisc)
cluster_vars  <- normalize(cluster_vars )
summary(cluster_vars )

###Hago una matrix de proximidad

proxmat <- dist(cluster_vars, method = 'euclidean')
proxmat


##Hago el cluster 

hclust_ward <- hclust(proxmat, method = 'ward.D')
plot(hclust_ward, cex = 0.6)


###Elijo el mejor metodo

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
  agnes(cluster_vars, method = x)$ac
}

library(purrr)
map_dbl(m, ac)

###Gap Statistic Method#####

#compares the total within intra-cluster variation for different values of k with their expected values under null reference distribution of the data

set.seed(12345)
library(cluster)
library(factoextra)
gap_stat <- clusGap(cluster_vars, FUN = hcut)

library(NbClust)
#The average silhouette width provides an evaluation of clustering validity, and might be used to select an ‘appropriate’ number of clusters.

fviz_nbclust(cluster_vars, FUN = hcut, method = "silhouette", k.max=10,nboot = 500, print.summary = TRUE)
#The method consists of plotting the explained variation as a function of the number of clusters and picking the elbow of the curve as the number of clusters to use.
#elbow method, which involves creating a plot with the number of clusters on the x-axis and the total within sum of squares on the y-axis and then identifying where an “elbow” or bend appears in the plot.

fviz_nbclust(cluster_vars, FUN = hcut, method = "wss", k.max=4,nboot = 100, print.summary = TRUE)
geom_vline(xintercept = 3, linetype = 2)

plot(hclust_ward, cex = 0.6)
rect.hclust(hclust_ward, k = 3, border = 2:5)

#Visually-driven hierarchical clustering analysis

shan_ict_mat <- data.matrix(cluster_vars)

library(heatmaply)
heatmaply(normalize(shan_ict_mat),
          Colv=NA,
          dist_method = "euclidean",
          hclust_method = "ward.D",
          seriate = "OLO",
          colors = Blues,
          k_row = 3,
          margins = c(NA,200,60,NA),
          fontsize_row = 4,
          fontsize_col = 5,
          main="Geographic Segmentation of Andean Cities by ICT indicators",
          xlab = "ICT Indicators",
          ylab = "Ciudades"
)

groups <- as.factor(cutree(hclust_ward, k=3))

shan_sf_cluster <- cbind(grid, as.matrix(groups)) %>%
  rename("CLUSTER"="as.matrix.groups.")
st_write(shan_sf_cluster , dsn = "C:/Users/musimundo/Documents/Tesis/PCA/cluster.shp",layer="cluster.shp", driver="ESRI Shapefile")

shan_sp <- as_Spatial(grid)
library(spdep)
shan.nb <- poly2nb(shan_sp)
summary(shan.nb)


