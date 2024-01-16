#####abro shapefile
library(sf)
base <- st_read("Shapefile/sri.shp")

base$ch_prop_u <- base$p_urb20 - base$p_urbn1
base$ch_prop_p <- base$p_per20  - base$p_peri1
base$ch_prop_r <- base$p_rrl20  - base$p_rurl1

st_write(base , dsn = "Shapefile/sri.shp", driver="ESRI Shapefile", delete_dsn = T)

#########MODELOS LINEALES##########
names(base)
##
library(dplyr)
base = base %>%
  mutate_at(vars(one_of( "prp_bsq", "ag_urb",   "ag_p"  ,   "ag_r" ,"slop_cn" , "slopLf0" , "slpSm00", "ch_prop_u", "ch_prop_p","ch_prop_r")), scale)
base = base %>%
  mutate_at(vars(one_of( "prop_bosqu", "ag_urb",   "ag_p"  ,   "ag_r" ,"slopecount" , "slopeLuef" , "slopeSum0", "ch_prop_u", "ch_prop_p","ch_prop_r")), scale)


names(base)
m1 <- lm(slopbsq ~ prp_bsq + ag_urb + ag_p +  ag_r + slop_cn + slopLf0 + slpSm00 + ch_prop_u +  ch_prop_p + ch_prop_r, data = base)

m1 <- lm(slopebosqu ~ prop_bosqu + ag_urb + ag_p +  ag_r + slopecount + slopeLuef + slopeSum0 + ch_prop_u +  ch_prop_p + ch_prop_r, data = base)


summary(m1)


###MODELOS MIXTOS

library(nlme)

base =base[!is.na(base$slopecount),]

r4 <- lme( slopbsq ~ prp_bsq + ag_urb + ag_p +  ag_r + slop_cn + slopLf0 + slpSm00 + ch_prop_u +  ch_prop_p + ch_prop_r, random = ~1|Name_2,
            data = base,na.action=na.exclude)

anova(r4)
summary(r4)

r5 <- lme( slopeSum0 ~ cultivo_01 + slopecrop0 + slopebosqu, random = ~1|Name_2,
           data = base,na.action=na.exclude)
summary(r5)

r6 <- lme( slopeLuef0 ~ cultivo_01 + slopecrop0 + slopebosqu, random = ~1|Name_2,
            data = base,na.action=na.exclude)

summary(r6)


