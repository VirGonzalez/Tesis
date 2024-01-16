#########MODELOS LINEALES##########
names(base)
##
library(dplyr)
base = base %>%
  mutate_at(vars(one_of( "cultivo_01", "min2","g_flt","temp", "prec","irrig","dst_p", "cultivo_01","Evergreen_" ,"Deciduous_", "Mixed_Fore"
                         ,"Closed_Shr", "Open_Shrub", "Woody_Sava" ,"Savannas" ,  "Grasslands", "Permanent_","Croplands" , "Urban_Buil", "Cropland_N", "Barren", "Water_Bodi")), scale)


#########MODELOS LINEALES##########
names(base)
m1 <- lm(slopbsq ~ prp_bsq + ag_urb + ag_p +  ag_r + slop_cn + slopLf0 + slpSm00 + ch_prop_u +  ch_prop_p + ch_prop_r, data = base)
m1 <- lm(hxtiempo ~ Trees + Crpln + Grssl + g_flt + temp + irrig + min2 + dst_p + X_max, data = base)
####AQUI HICE STEP####
m1<- lm(hxtiempo ~ Trees + Grssl + temp + cultivo + min2 + dst_p + cultivo_01 + 
          Woody_Sava + Savannas + Permanent_ , data = base)
summary(m1)
step(m1)



library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
sjPlot::plot_model(m1,type = "est", 
                   axis.labels=c("Max fire freq", "Port distance","Initial radiance", "Irrigation" ,"Temperature", "Flatness","Grassland cover", "Crop cover", "Tree cover" ),vline.color = "grey",
                   show.values=TRUE, value.offset = .5, show.p=TRUE)+ scale_y_continuous(limits = c(-0.1, 0.1))+theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12))

tab_model(m1, show.intercept = TRUE,show.est = TRUE, digits = 9, p.style = "scientific_stars", dv.labels = c("Nighttime lights trends") ,  pred.labels = c("Intercept","% forest (2001)","Urban aggregation","Periurban aggregation", "Rural aggregation","urban expansion", "urban densification","urban intensification", "change urban proportion",  "change periurban proportion","change rural proportion" ),file = "plot/modelosri_120.doc")
tab_model(m1, show.intercept = TRUE,show.est = TRUE, digits = 9, p.style = "scientific_stars")


###MODELOS MIXTOS

library(lme4)
base <- subset(base, base$hxtiempo >= 0)
r4 <- lmer( hxtiempo ~ cultivo_01 + min2 + (1|Name_2),
            data = base,REML = TRUE)
summary(r4)
fixef(r4)

r5 <- lm(hxtiempo ~ cultivo_01 + min2 +g_flt + temp+ prec + irrig + min2 + dst_p, data =base)
summary(r5)


library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
sjPlot::plot_model(m1,type = "est", 
                   axis.labels=c("Max fire freq", "Port distance","Initial radiance", "Irrigation" ,"Temperature", "Flatness","Grassland cover", "Crop cover", "Tree cover" ),vline.color = "grey",
                   show.values=TRUE, value.offset = .5, show.p=TRUE)+ scale_y_continuous(limits = c(-0.1, 0.1))+theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12))

tab_model(m1, show.intercept = TRUE,show.est = TRUE, digits = 9, p.style = "scientific_stars", dv.labels = c("Nighttime lights trends") ,  pred.labels = c("Intercept","Tree cover","Crop cover","Grassland cover", "Flatness","Temperature", "Irrigation","Initial radiance", "Port distance",  "Max fire freq"))


#### Regresion exponencial 

