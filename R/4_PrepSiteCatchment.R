library(terra)
library(magrittr)
library(ggplot2)


# load the soils raster as the base raster.
handplant <- terra::rast("data/data-derived/soils/vepiin_hand_planting_200.tif")
empty_rast <- handplant
values(empty_rast) <- 1

# make cells catchments areas ----
reese <- readr::read_csv(
  here::here(
    "data/data-raw/Reese2021/4-occupation-by-household-public.csv"
  )) %>% 
  tidyr::pivot_longer(cols = c(X450:X1300), names_to = "year", values_to = "nHouse") %>% 
  dplyr::mutate(year = as.numeric (stringr::str_remove_all(year, "X"))) %>% 
  dplyr::filter(year %in% c(600:1300),
                nHouse > 0)  

# Join all arch sites on the same cell and consider them one settlement.
cells_sites <- 
  reese %>%
  # join with randomly generated cell locations. Note that sites on the same cell should match the same dummy cell. 
  # The distances between the cells will not be maintained, however
  dplyr::left_join(readr::read_csv(here::here("data/data-raw/Reese2021/dummyCells.csv")),
                   by = c("SITE_ID" = "SITE_ID", "SITE_NO" = "SITE_NO")) %>% 
  #rename so the rest of the script works 
  dplyr::rename(cell = dummyCell) %>% 
  # the below is the script run, but is dependent on locational information, which we cannot publicly provide
  # to run, uncomment lines  34-48 and comment out lines 27 -30
  
  # dplyr::rename(x = X_COORDS,
  #               y= Y_COORDS) %>% 
  # dplyr::select(year, SITE_NO, nHouse, CENTER, x, y) %>% 
  # sf::st_as_sf(# can turn into spatvector
  #   coords = c("x", "y"),
  #   crs = "EPSG:26912") %>%
# # First, calculate the cell that each site is within
# dplyr::mutate(
#   cell = 
#     terra::cellFromXY(
#       object = empty_rast, 
#       xy = sf::st_coordinates(geometry)
#     ) %>%
#     as.integer()
# ) %>% 
# sf::st_drop_geometry() %>%


# Aggregate nHouses by cell and year to get total houses per cell per year
  dplyr::group_by(cell, year) %>%
  dplyr::summarise(nHouse = sum(nHouse, na.rm = FALSE),
                   center = max(CENTER)) %>%
  # Regroup by cells, and create nested tibbles
  dplyr::group_by(cell) %>%
  tidyr::nest(.key = "occupation") %>%
  # Get the centerpoints of occupied cells, and turn back into an SF object. THESE WILL BE FALSE LOCATIONS
  dplyr::mutate(xy = terra::xyFromCell(handplant, cell) %>%
                  as.data.frame()) %>%
  tidyr::unnest(xy) %>% 
  sf::st_as_sf(coords = c("x", "y"),
               crs = "EPSG:26912") 


# We will focus below on the raster centers, so we can go from 200 to 3400 to capture all scales.
radii <-  
  seq(200, 4000, 200) %>%
  magrittr::add(1)# Add 1 to make sure the radii include cell centers (but just)

# make a table the combines all radii options with their cells
cells_variedRadii <- cells_sites %>%
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
  terra::extract(x = handplant, #> 0,
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

#export it to skip above step in the future if necessary
cells_catchments %>% saveRDS(here::here("data/data-derived/cells/cells_catchments.rds"))

## past the long bit
cells_catchments <- readRDS(here::here("data/data-derived/cells/cells_catchments.rds"))

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

#all years have this basic ratio
cells_1_4 <-   cells_catchment_interim %>% 
  makeWeightedCatchments(ratio = c(1,2,3,4 ))

#only years AD 1000-1300 have these higher ratios
cells_5_6 <- cells_catchment_interim %>% 
  dplyr::filter(year >999) %>% 
  makeWeightedCatchments(ratio = c(5,6))

cells_fieldhouse <-  cells_catchment_interim %>% 
  dplyr::filter(year >999) %>% 
  makeWeightedCatchments(ratio = c(13))

#combine and unnest catchment
cells_catchmentAll <-
  cells_1_4 %>% 
  #bind the other two groups and unnest
  dplyr::bind_rows(., cells_5_6 ) %>% 
  dplyr::bind_rows(., cells_fieldhouse ) %>% 
  dplyr::ungroup() %>%
  tidyr::unnest(catchment) 

#calculate two catchment sizes
maxCatch <- #the largest catchment ever considered
  cells_catchmentAll  %>% 
  dplyr::group_by(cell, year) %>% 
  dplyr::summarize(maxRadius = max(radius))

prefCatch <-   # the preferred catchment
  cells_1_4  %>% 
  dplyr::bind_rows(., cells_5_6 ) %>% 
  dplyr::ungroup() %>%
  tidyr::unnest(catchment) %>% 
  dplyr::group_by(cell, year) %>% 
  dplyr::summarize(prefRadius = max(radius))

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
  dplyr::left_join(y = prefCatch, by = c("cell" = "cell", "year" = "year")) %>% 
  #join to get center status
  dplyr::left_join(y = cells_sites %>% 
                     sf::st_drop_geometry() %>% 
                     tidyr::unnest(occupation) %>% 
                     dplyr::select(cell, year, center), 
                   by = c("cell" = "cell", "year" = "year"))

#we'll use this for after the simulation
catchmentMaize_all %>% 
  saveRDS(here::here("data/data-derived/Simulated_catchment/catchmentMaize_all.RDS"))

#we'll use this for the simulation
catchmentMaize_all %>% 
  dplyr::select(cell, year, nFieldsNeeded, catchment) %>% 
  saveRDS(here::here("data/data-derived/Simulated_catchment/catchmentMaize.RDS"))


#just close to calculate local catchment quality
catchmentClose <- 
  dplyr::bind_rows(cells_1_4 , cells_5_6 ) %>% 
  # dplyr::bind_rows(., cells_fieldhouse ) %>% 
  dplyr::ungroup() %>%
  tidyr::unnest(catchment)  %>% 
  dplyr::select(cell, year, nHouse, nFieldsNeeded, catchment) %>% 
  dplyr::group_by(cell, year, nHouse, nFieldsNeeded) %>% 
  dplyr::summarise(
    catchment = 
      list(
        catchment %>%
          c())) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(y = prefCatch, by = c("cell" = "cell", "year" = "year"))

#we'll use this to calculate local catchment quality
catchmentClose %>% 
  saveRDS(here::here("data/data-derived/Simulated_catchment/catchmentClose.RDS"))



