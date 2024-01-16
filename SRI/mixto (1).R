#####abro shapefile
library(sf)
base <- st_read("Shapefile/sri.shp")


base$ch_prop_p <- base$prp_pr2  - base$prp_pr0


st_write(base , dsn = "Shapefile/sri_v2.shp", driver="ESRI Shapefile", delete_dsn = T)

#########MODELOS LINEALES##########
names(base)
##
library(dplyr)
base = base %>%
  mutate_at(vars(one_of( "prp_bsq" ,"slop_cn" , "slopLf0" , "slpSm00", "ch_prop_p","prp_pr2")), scale)
base = base %>%
  mutate_at(vars(one_of( "prop_bosqu", "ag_urb",   "ag_p"  ,   "ag_r" ,"slopecount" , "slopeLuef" , "slopeSum0", "ch_prop_u", "ch_prop_p","ch_prop_r")), scale)


names(base)
m1 <- lm(slopbsq ~ prp_bsq + slop_cn + slopLf0 + slpSm00 +  ch_prop_p + prp_pr2, data = base)




summary(m1)


###MODELOS MIXTOS

library(nlme)


r4 <- lme( slopbsq ~ prp_bsq + slop_cn + slopLf0 + slpSm00 +  ch_prop_p + prp_pr2, random = ~1|tipo,
            data = base,na.action=na.exclude)

r4 <- lme( slopbsq ~ prp_bsq + slop_cn + slopLf0 + slpSm00 +  ch_prop_p + prp_pr2, random = ~1|tipo,
           data = base,na.action=na.exclude)

anova(r4)
summary(r4)

r5 <- lme( slopeSum0 ~ cultivo_01 + slopecrop0 + slopebosqu, random = ~1|Name_2,
           data = base,na.action=na.exclude)
summary(r5)

r6 <- lme( slopeLuef0 ~ cultivo_01 + slopecrop0 + slopebosqu, random = ~1|Name_2,
            data = base,na.action=na.exclude)

summary(r6)


