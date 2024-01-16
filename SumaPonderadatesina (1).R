getwd()
setwd("C:/Users/pc/Dropbox/PC/Documents/Paper")
library(sf)
cor <- st_read("C:/Users/pc/Dropbox/PC/Documents/Paper/saltats3.shp")
cor <- read.csv("buenos_aires.csv", header = TRUE, sep = ";",
                dec = ",")
str(cor)
names(cor)

cor$dsd10 <- cor$T_P/cor$areakm2
cor$totaljefes <- rowSums (cor[, 19:26], na.rm = TRUE)

cor$mx_nv_10 <- ( cor$primario*5 + cor$inicial..j*5 + cor$secundario*20 +
                   cor$egb*20+ cor$polimodal*20 + cor$superior.n*25 
                 + cor$post.unive*50 + cor$universita*50)/ cor$totaljefes 
cor$NBI10<- cor$con_nbi/cor$T_H
cor$cld_mtt10 <- (cor$CAL_I*60 + cor$CAL_II*25 + cor$CAL_III*10 + cor$CAL_IV*5)/cor$T_H
st_write(cor, "C:/Users/pc/Dropbox/PC/Documents/Paper/BA5.shp")
library(openxlsx)
write.xlsx(cor, 'tablabaires.xlsx')
str(cor)
cor$EDUC <- (cor$mx_nv10 - cor$mx_nv01)
cor$cld_mtt <- (cor$cl_mt10 - cor$cl_mt01)
cor$nbi <- (cor$NBI10 - cor$NBI01)

cor$dens = (log(cor$dnsdd10) - log(cor$den01))/log(cor$den01)
mean(cor$cld_mtt, na.rm = T)
sd(cor$densidad01, na.rm = T)
