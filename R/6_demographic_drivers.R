library(tidyverse)
library(terra)
library(magrittr)
library(robinson2020)
library(fasterize)

#calcualte number of centers within 18km -----
handplant <- terra::rast("data/data-derived/soils/vepiin_hand_planting_200.tif")
empty_rast <- handplant
values(empty_rast) <- 1
remove(handplant)

centers_vect <- 
  readRDS(here::here("data/data-derived/prepAnalysis/bouts_noInterbouts_long.rds")) %>% 
  dplyr::select(cell, year, center) %>% 
  dplyr::filter(center >0) %>% 
  dplyr::arrange(year) %>% 
  group_by(year) %>%
  dplyr::select(cell, year, center) %>% 
  dplyr::mutate(xy = terra::xyFromCell(empty_rast, cell) %>%
                  as.data.frame()) %>%
  tidyr::unnest(xy) %>% 
  sf::st_as_sf(coords = c("x", "y"),
               crs = "EPSG:26912") %>% 
  terra::vect()

#load and vectorize the cell locations
sites_vect <- readRDS(here::here("data/data-derived/reese/cells_sites.rds")) %>% 
    dplyr::select(cell, year, nHouse) %>%
    dplyr::arrange(year) %>%
    group_by(year) %>%
    dplyr::mutate(xy = terra::xyFromCell(empty_rast, cell) %>%
                    as.data.frame()) %>%
    tidyr::unnest(xy) %>%
    sf::st_as_sf(coords = c("x", "y"),
                 crs = "EPSG:26912") %>%
    terra::vect()


lookingAround_vect <- function(looking_for, looking_from, dist){
  purrr::map(
    .x = 600:1280,
    .f = function(thisYear){
      
      thisYearsFrom<- terra::subset(looking_from, looking_from$year == thisYear) %>% 
        terra::buffer(dist)
      thisYearsFor <- terra::subset(looking_for, looking_for$year == thisYear)
      
      thisYearsFor$year <- NULL
      thisYearsFor$cell <- NULL
      
      found <- terra::intersect( thisYearsFrom, thisYearsFor) %>% 
        tibble::as_tibble()
      return(found)
    })%>% 
    bind_rows()
}

nearbyCenters_18km <- lookingAround_vect(looking_for = centers_vect, looking_from = sites_vect, dist = 18000) %>% 
  dplyr::group_by(cell, year, nHouse) %>% 
  dplyr::summarise(nCenters_18km = sum(center))

demo_factors <- nearbyCenters_18km %>% 
  dplyr::select(-nHouse) %>% 
  dplyr::filter(year < 1281) %>%
  dplyr::mutate(nCenters_18km = ifelse(is.na(nCenters_18km), 0, nCenters_18km))

demo_factors %>% saveRDS(here::here("data/data-derived/prepAnalysis/demo_factors.rds"))

