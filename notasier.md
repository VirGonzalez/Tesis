# load packages
library(sf)
library(dplyr)
library(ggplot2)
library(scico)
library(rnaturalearth)
library(purrr)
library(smoothr)
library(rgbif)
# world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")
# country subset
CRpoly <- worldMap %>% filter(subregion == 'South America') 

# random points tables as a named list
sp_occ <- st_read("C:/Users/Usuario/Documents/Tesis/paper_migraciones/occurrences/tree_exotic.shp")
names(sp_occ) <- paste0("sp_", letters[1:length(sp_occ)])

CRbatsXY <- sp_occ %>%
  select(accepted_1, decimalLon, decimalLat) %>%
  na.omit()


# trim to study area
limsCR <- st_buffer(CRpoly, dist = 0.7) %>% st_bbox()
# neighboring countries
adjacentPolys <- st_touches(CRpoly, worldMap)
neighbours <- worldMap %>% slice(pluck(adjacentPolys, 1))

# countries
divpolPlot <-
  ggplot() +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly) +
  coord_sf(
    xlim = c(limsCR["xmin"], limsCR["xmax"]),
    ylim = c(limsCR["ymin"], limsCR["ymax"])
  ) +
  scale_x_continuous(breaks = c(-84)) +
  theme(
    plot.background = element_rect(fill = "#f1f2f3"),
    panel.background = element_rect(fill = "#2F4051"),
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank()
  )
divpolPlot

# plot points
spPointsPlot <-
  ggplot() +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly) +
  geom_sf(data = sp_occ, aes(fill = database_i ), pch = 21) +
  scale_fill_scico_d(palette = "davos", direction = -1, end = 0.9, guide = FALSE) +
  coord_sf(
    xlim = c(limsCR["xmin"], limsCR["xmax"]),
    ylim = c(limsCR["ymin"], limsCR["ymax"])
  ) +
  scale_x_continuous(breaks = c(-84)) +
  theme(
    plot.background = element_rect(fill = "#f1f2f3"),
    panel.background = element_rect(fill = "#2F4051"),
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank()
  )
spPointsPlot

str(CRpoly)
# grid
install.packages("cleangeo")
library(cleangeo)
clgeo_IsValid(CRpoly) # This tells you if the geometry is 


CRGrid <- CRpoly %>%
  st_make_valid() %>%
  st_make_grid(cellsize = 0.5) %>%
  st_intersection(CRpoly) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())

CRGrid
# cell richness

# to sf object, specifying variables with coordinates and projection
CRbatsXYsf <- st_as_sf(CRbatsXY, coords = c("decimalLon", "decimalLat"), crs = 4326) %>%
  group_by(accepted_1) %>%
  summarize()

# plot points
batPointsPlot <-
  ggplot() +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly) +
  geom_sf(data = CRbatsXYsf, pch = 21) +
  coord_sf(
    xlim = c(limsCR["xmin"], limsCR["xmax"]),
    ylim = c(limsCR["ymin"], limsCR["ymax"])
  ) +
  scale_x_continuous(breaks = c(-84)) +
  theme(
    plot.background = element_rect(fill = "#f1f2f3"),
    panel.background = element_rect(fill = "#2F4051"),
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank()
  )
batPointsPlot

bat_richness_grid <- CRGrid %>%
  st_join(CRbatsXYsf) %>%
  mutate(overlap = ifelse(!is.na(accepted_1), 1, 0)) %>%
  group_by(cellid) %>%
  summarize(num_species = sum(overlap))


##GUARDO EL SHAPE DE RIQUEZA


st_write(bat_richness_grid , "C:/Users/Usuario/Documents/Tesis/paper_migraciones/occurrences/grid_riqueza0.5.shp")


# plot
batRichCR <-
  ggplot(bat_richness_grid) +
  geom_sf(data = neighbours, color = "white") +
  geom_sf(data = CRpoly, fill = "grey", size = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  scale_fill_scico(palette = "davos", direction = -1, end = 0.9, name = "Bat species richness") +
  coord_sf(
    xlim = c(limsCR["xmin"], limsCR["xmax"]),
    ylim = c(limsCR["ymin"], limsCR["ymax"])
  ) +
  scale_x_continuous(breaks = c(-84)) +
  theme(
    plot.background = element_rect(fill = "#f1f2f3"),
    panel.background = element_rect(fill = "#2F4051"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    line = element_blank(),
    rect = element_blank()
  ) + labs(fill = "richness")
batRichCR

# open libraries
library(tidyverse)
library(sf)
library(stars)

# rasterize based on geometry and a column named "value". Change the name of this column if necessary
r.enn2mean<-st_rasterize(bat_richness_grid %>% dplyr::select(num_species, geometry))

plot(r.enn2mean)
# export as tiff
write_stars(r.enn2mean, "Plot/richness_grid1.tif")


