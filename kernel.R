library(raster)
r <- raster("/Agua/nbi_st.tif")
plot(r)
values(r) <- runif(ncell(r)) 
w<-matrix(1,11,11)

f1 <- focal(r, w, fun = median , filename='kernel',na.rm=T, overwrite = TRUE)

plot(f1)

rc <- function(r) {
  ifelse( r => 0.67 , 7, ifelse( r >= 0.6 , 1, 
          ifelse( r == 0.5 , 5,
                  ifelse( r >= 0.38 , 2,
                          ifelse( r >= 0.2 , 3,ifelse( r >= 0.12 , 4, 6))))))
}
myColors <- c('blue', 'green', 'yellow', 'red', 'hotpink', "black", "white") 
r.class <- overlay(f1, fun=rc)
plot(r.class, col = myColors)

plot.new() 
par(xpd=T)
legend('left',inset = 0,
       legend = c('1', '2', '3', '4', '5'), 
       fill=myColors, cex=2, horiz = F)


rf <- writeRaster(r.class, filename=file.path("/Agua/clases_swat.tif"), format="GTiff", overwrite=TRUE)
