library(tidyverse)
library(terra)
library(tidyterra)
library(ggspatial)
library(RColorBrewer)

#somewhat obviously this figure will not reproduce.

# load the soils raster as the base raster.
handplant <- terra::rast("data/data-derived/soils/vepiin_hand_planting_200.tif")
empty_rast <- handplant
values(empty_rast) <- 1
remove(handplant)

# load in populations
cell_grouped <- readRDS(here::here("data/data-derived/prepAnalysis/bouts_noInterbouts_long.rds")) %>% 
  dplyr::group_by(cell) %>% 
  summarise(containsCenter = factor(max(center), 
                                    levels = c(0,1), 
                                    labels = c("No", "Yes")),
            nHouse = max(nHouse)) %>% 
  bind_cols(terra::xyFromCell(empty_rast, .$cell)) %>% 
  terra::vect(
              geom = c("x", "y"),
              crs = "EPSG:26912")


#load in DEM
study_area_dem <- terra::rast("data/data-raw/ned/study_area_NED_1.tif")

#plot

fig1 <-
  ggplot()+
  geom_spatraster(data = study_area_dem, aes(fill = layer))+
  scale_fill_gradientn(colours = terrain.colors(10),
                        guide = guide_legend(title="Elevation (m)")) +
  # geom_spatvector(data = CMV_surveys, alpha = 0.1)#+
  scale_fill_gradientn(colours = terrain.colors(10)) +
  geom_spatvector(data = cell_grouped, 
                  (aes(size = nHouse, 
                       color = containsCenter, 
                       shape = containsCenter)))+
  scale_color_manual(values = c("black", "red"))+
   ggspatial::annotation_scale()+
    theme_bw() 

fig1

png(filename = "fig1_map.png", units = "in", width = 7, height = 6, res = 300)
fig1
dev.off()
