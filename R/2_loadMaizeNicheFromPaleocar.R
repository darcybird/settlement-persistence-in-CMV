library(magrittr)
library(paleocar)
library(ggplot2)
library(sf)
library(raster)
# library(villager)

study_area <-
  robinson2020::vep_study_areas %>%
  dplyr::filter(`Study Area` == "CMV") %>%
  sf::st_transform(26912) %>%
  sf::st_buffer(4500)

# Either create them fresh
cmv_paleocar <-
  paleocar::get_bocinsky2016(as(study_area, "Spatial"),
                             raw.dir = "data/data-raw/maizeNiche/",
                             extraction.dir = "data/data-raw/maizeNiche/",
                             prcp_threshold = 250,
                             gdd_threshold = 1800, # Default. In F
                             years = 570:1300, # Default
                             force.redo = F)

# downloads but doesn't mask or calculate maize growing niche. Function needs to be updated


cmv_paleocar %<>%
  purrr::map(raster::readAll)

cmv_paleocar <-
  c(PPT = "data/data-raw/maizeNiche/PPT_niche_1-2000.tif",
    GDD = "data/data-rawa/maizeNiche/GDD_niche_1-2000.tif") %>%
  purrr::map(raster::brick) %>%
  purrr::map(raster::readAll)
