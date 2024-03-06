library(terra)
library(magrittr)
library(ggplot2)


# load the soils raster as the base raster.
handplant <- terra::rast("data/data-derived/soils/vepiin_hand_planting_200.tif")
empty_rast <- handplant
values(empty_rast) <- 1
remove(handplant)

# bring in the occupations

cells_occs <- readRDS(here::here("data/data-derived/prepAnalysis/bouts_noInterbouts_long.rds")) 

cells_sites_nest <- cells_occs %>%
  dplyr::select(cell, year, nHouse, center) %>% 
  # Regroup by cells, and create nested tibbles
  dplyr::group_by(cell) %>%
  tidyr::nest(.key = "occupation") %>%
  # Get the centerpoints of occupied cells, and turn back into an SF object. THESE WILL BE FALSE LOCATIONS
  dplyr::mutate(xy = terra::xyFromCell(empty_rast, cell) %>%
                  as.data.frame()) %>%
  tidyr::unnest(xy) %>% 
  sf::st_as_sf(coords = c("x", "y"),
               crs = "EPSG:26912") 

# We will focus below on the raster centers, so we can go from 200 to 3400 to capture all scales.
radii <-  
  seq(200, 4000, 200) %>%
  magrittr::add(1)# Add 1 to make sure the radii include cell centers (but just)

# make a table the combines all radii options with their cells
cells_variedRadii <- cells_sites_nest %>%
  # Repeat all for each radius
  dplyr::cross_join(tibble::tibble(radius = radii)) %>%
  # group by radii, and get buffers for each group 
  dplyr::group_by(radius) %>%
  dplyr::mutate(geometry = sf::st_buffer(geometry, radius[[1]])) %>%
  dplyr::ungroup() %>% 
  tibble::rownames_to_column(var = "ID") %>% 
  dplyr::mutate(ID = as.double(ID)) 

#identify all fields within each radius option (this script may take awhile to run)
cells_catchments <- cells_variedRadii %>% 
  terra::extract(x = empty_rast, #> 0,
                 y = .,
                 cells = TRUE) %>%
  dplyr::rename(field = cell) %>% 
  dplyr::group_by(ID) %>%
  dplyr::nest_by(.key = "catchment") %>% 
  dplyr::left_join(., 
                   cells_variedRadii %>% 
                     dplyr::select(ID, cell, radius), 
                   by = c("ID" = "ID")) %>% 
  dplyr::ungroup() %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(cell, radius, catchment)

# #export it to skip above step in the future if necessary
# cells_catchments %>% saveRDS(here::here("data/data-derived/catchments/cells_catchments.rds"))
# 
# ## past the long bit
# cells_catchments <- readRDS(here::here("data/data-derived/catchments/cells_catchments.rds"))


# produce a dataframe with each cell, year, nHouse, all radii, number of cells in catchment, list of catchment, and nFieldsNeeded
cells_catchment_interim <- 
  #produce each cell's yearly occupation list with each radius
  cells_variedRadii %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-ID) %>% 
  #join catchment information
  dplyr::left_join(., 
                   dplyr::left_join(
                     # determine number of handplantable cells in each catchment 
                     cells_catchments %>% 
                       tidyr::unnest(catchment) %>% 
                       dplyr::filter(handplant > 0 ) %>% 
                       dplyr::group_by(cell, radius) %>% 
                       dplyr::summarise(nCell = dplyr::n()) %>% 
                       dplyr::ungroup(), 
                     #keep the non-handplantable, will be used to determine catchment quality later
                     cells_catchments %>% 
                       tidyr::unnest(catchment) %>% 
                       dplyr::select(-handplant) %>% 
                       dplyr::group_by(cell, radius) %>% 
                       dplyr::summarise( catchment = list(field )), 
                     by = c("cell" = "cell", "radius" = "radius")) , 
                   by = c("cell" = "cell", "radius" = "radius")) %>% 
  #remove the extra 1 in the radius list
  dplyr::mutate(radius = radius - 1) %>% 
  #unnest the occupation list to work with it
  tidyr::unnest(occupation) %>%
  #determine number of 
  dplyr::mutate(nFieldsNeeded = nHouse) #this can be changed to alter the number of fields needed according to number of households

#make the series of weighted catchments according to the ratio described in the SI
makeWeightedCatchments <- function(data, ratio){
  purrr::map(
    .x = ratio,
    .f = function(thisratio){
      
      data %>% 
        dplyr::filter(nFieldsNeeded < floor(nCell/thisratio)) %>% 
        dplyr::group_by(cell, year) %>% 
        dplyr::filter(nCell == min(nCell))
    })  
}

#all years have this basic ratio
cells_1_4 <-   cells_catchment_interim %>% 
  makeWeightedCatchments(ratio = c(1,2,3,4 ))

#only years AD 1000-1300 have these higher ratios
cells_5_6 <- cells_catchment_interim %>% 
  dplyr::filter(year >999) %>% 
  makeWeightedCatchments(ratio = c(5,6))


#combine and unnest catchment
cells_catchmentAll <-
  cells_1_4 %>% 
  #bind the other two groups and unnest
  dplyr::bind_rows(., cells_5_6 ) %>% 
  dplyr::ungroup() %>%
  tidyr::unnest(catchment) 

#calculate preferred catchment size

maxCatch <-   # the preferred catchment
  cells_catchmentAll%>% 
  dplyr::group_by(cell, year) %>% 
  dplyr::summarize(maxRadius = max(radius))

# combine all
catchmentMaize_all <-
  cells_catchmentAll %>% 
  dplyr::select(cell, year, nHouse, nFieldsNeeded, catchment) %>% 
  dplyr::group_by(cell, year, nHouse, nFieldsNeeded) %>% 
  dplyr::summarise(
    catchment = 
      list(
        catchment %>%
          c())) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(y = maxCatch, by = c("cell" = "cell", "year" = "year")) %>% 
  #join to get center status
  dplyr::left_join(y = cells_occs %>% 
                     dplyr::select(cell, year, center), 
                   by = c("cell" = "cell", "year" = "year"))

#check how many cells have a maximum radius >2000m and for how many years
catchmentMaize_all %>% 
  dplyr::select(cell, year, maxRadius) %>% 
  dplyr::filter(maxRadius > 2000) %>% 
  dplyr::group_by(cell, maxRadius) %>% 
  dplyr::summarize(n = dplyr::n()) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(-maxRadius)

#we'll use this to calculate local catchment quality
catchmentMaize_all %>% 
  saveRDS(here::here("data/data-derived/prepAnalysis/catchmentMaize.RDS"))
