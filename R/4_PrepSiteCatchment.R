library(terra)
library(magrittr)
library(ggplot2)


# load the soils raster as the base raster.
handplant <- terra::rast("data/data-derived/soils/vepiin_hand_planting_200.tif")
empty_rast <- handplant
values(empty_rast) <- 1

# make reese catchments areas ----
reese <- readr::read_csv(
  here::here(
    "data/data-raw/Reese2021/4-occupation-by-household-public.csv"
  )) %>% 
  tidyr::pivot_longer(cols = c(X450:X1300), names_to = "year", values_to = "nHouse") %>% 
  dplyr::mutate(year = as.numeric (stringr::str_remove_all(year, "X"))) %>% 
  dplyr::filter(year %in% c(600:1300)) %>% 
  dplyr::filter(nHouse > 0)


# Join all arch sites on the same cell and consider them one settlement.
reese_cells <- 
  reese %>% 
  # join with randomly generated cell locations. Note that sites on the same cell should match the same dummy cell. 
  # The distances between the cells will not be maintained, however
  dplyr::left_join(readr::read_csv(here::here("data/data-raw/Reese2021/dummyCells.csv")),
                   by = c("SITE_ID" = "SITE_ID", "SITE_NO" = "SITE_NO")) %>% 
  #rename so the rest of the script works 
  dplyr::rename(cell = dummyCell) 
# the below is the script run, but is dependent on locational information, which we cannot publicly provide
# to run, uncomment lines 7-9 and 32-49 and comment out lines 27, 28, and 30
  # dplyr::rename(x = X_COORDS,
  #               y= Y_COORDS) %>% 
  # dplyr::select(year, SITE_NO, nHouse, x, y) %>%
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

cell_sites <- reese_cells %>% 
  # Aggregate nHouses by cell and year to get total houses per cell per year
  dplyr::group_by(cell, year) %>%
  dplyr::summarise(nHouse = sum(nHouse, na.rm = FALSE)) %>%
  # Regroup by cells, and create nested tibbles
  dplyr::group_by(cell) %>%
  tidyr::nest(.key = "occupation") %>%
  # Get the centerpoints of occupied cells, and turn back into an SF object
  dplyr::mutate(xy = terra::xyFromCell(handplant, cell) %>%
                  as.data.frame()) %>%
  tidyr::unnest(xy) %>% 
  sf::st_as_sf(coords = c("x", "y"),
               crs = "EPSG:26912") 


# We will focus below on the raster centers, so we can go from 200 to 4000 to capture all scales.
radii <-  
  seq(200, 4000, 200) %>%
  magrittr::add(1)# Add 1 to make sure the radii include cell centers (but just)

# make a table the combines all radii options with their cells
cell_variedRadii <- cell_sites %>%
  # Repeat all for each radius
  dplyr::cross_join(tibble::tibble(radius = radii)) %>%
  # group by radii, and get buffers for each group 
  dplyr::group_by(radius) %>%
  dplyr::mutate(geometry = sf::st_buffer(geometry, radius[[1]])) %>%
  dplyr::ungroup() %>% 
  tibble::rownames_to_column(var = "ID") %>% 
  dplyr::mutate(ID = as.double(ID)) 


#identify all fields within each radius option (this script may take awhile to run)
cell_catchments <- cell_variedRadii %>% 
  terra::extract(x = handplant, 
                 y = .,
                 cells = TRUE) %>%
  dplyr::rename(field = cell) %>% 
  dplyr::group_by(ID) %>%
  dplyr::nest_by(.key = "catchment") %>% 
  dplyr::left_join(., 
                   cell_variedRadii %>% 
                     dplyr::select(ID, cell, radius), 
                   by = c("ID" = "ID")) %>% 
  dplyr::ungroup() %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(cell, radius, catchment)

#export it to skip above steps in the future if necessary
cell_catchments %>% saveRDS(here::here("data/data-derived/cells/cell_catchments.rds"))


## past the long bit

cell_catchments <- readRDS(here::here("data/data-derived/cells/cell_catchments.rds"))

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

# produce a datafram with each cell, year, nHouse, all radii, number of cells in catchment, list of catchment, and nFieldsNeeded
cell_catchment_interim <- 
  #produce each cell's yearly occupation list with each radius
  cell_variedRadii %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-ID) %>% 
  #join catchment information
  dplyr::left_join(., 
                   dplyr::left_join(
                     # determine number of handplantable cells in each catchment 
                     cell_catchments %>% 
                       tidyr::unnest(catchment) %>% 
                       dplyr::filter(handplant > 0 ) %>% 
                       dplyr::group_by(cell, radius) %>% 
                       dplyr::summarise(nCell = dplyr::n()) %>% 
                       dplyr::ungroup(), 
                     #keep the non-handplantable, will be used to determine catchment quality later
                     cell_catchments %>% 
                       tidyr::unnest(catchment) %>% 
                       dplyr::select(-handplant) %>% 
                       dplyr::group_by(cell, radius) %>% 
                       dplyr::summarise( catchment = list(field )), 
                     by = c("cell" = "cell", "radius" = "radius")) , 
                   by = c("cell" = "cell", "radius" = "radius")) %>% 
  #remove the extra 1 in the radius list
  dplyr::mutate(radius = radius - 1) %>% 
  #unnest the occupation list so that it's more workable
  tidyr::unnest(occupation) %>%
  #determine number of 
  dplyr::mutate(nFieldsNeeded = nHouse) #this can be changed to alter the number of fields needed according to number of households

cell_999 <- cell_catchment_interim %>% 
  #only years AD 1000-1300 have this higher ratio
  dplyr::filter(year >999) %>% 
  makeWeightedCatchments(ratio = c(5,6))

cell_fieldhouse <-  cell_catchment_interim %>% 
  #only years AD 1000-1300 have this higher ratio. 
  # Note that this extremely high ratio is excluded as we don't consider this ratio as part of the local catchment quality variable
  dplyr::filter(year >999) %>% 
  makeWeightedCatchments(ratio = c(13))

catchmentAll <-
  cell_catchment_interim %>% 
  #all years include ratios 1-4
  makeWeightedCatchments(ratio = c(1,2,3,4 )) %>% 
  #bind the other two groups and unnest
  dplyr::bind_rows(., cell_999 ) %>% 
  dplyr::bind_rows(., cell_fieldhouse ) %>% 
  dplyr::ungroup() %>%
  tidyr::unnest(catchment) 

#calculate two catchment sizes
maxCatch <- 
  catchmentAll  %>% 
  dplyr::group_by(cell, year) %>% 
  dplyr::summarize(maxRadius = max(radius))

prefCatch <-   
  cell_catchment_interim %>% 
  makeWeightedCatchments(ratio = c(1,2,3,4 )) %>% 
  dplyr::bind_rows(., cell_999 ) %>% 
  dplyr::ungroup() %>%
  tidyr::unnest(catchment) %>% 
  dplyr::group_by(cell, year) %>% 
  dplyr::summarize(prefRadius = max(radius))

# combine all
catchmentMaize_alldata <-
  catchmentAll %>% 
  dplyr::select(cell, year, nHouse, nFieldsNeeded, catchment) %>% 
  dplyr::group_by(cell, year, nHouse, nFieldsNeeded) %>% 
  dplyr::summarise(
    catchment = 
      list(
        catchment %>%
          c())) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(y = maxCatch, by = c("cell" = "cell", "year" = "year"))%>% 
  dplyr::left_join(y = prefCatch, by = c("cell" = "cell", "year" = "year"))

#we'll use this for the simulation
catchmentMaize_alldata %>% 
  dplyr::select(cell, year, nFieldsNeeded, catchment) %>% 
  saveRDS(here::here("data/data-derived/Simulated_catchment/catchmentMaize.RDS"))

#just close to calculate local catchment quality
catchmentClose <- 
  cell_catchment_interim %>% 
  makeWeightedCatchments(ratio = c(1,2,3,4 )) %>% 
  dplyr::bind_rows(., cell_999 ) %>% 
  # dplyr::bind_rows(., reese_fieldhouse ) %>% 
  dplyr::ungroup() %>%
  tidyr::unnest(catchment) 

  
catchmentClose_alldata <- 
  catchmentClose %>% 
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
catchmentClose_alldata %>% 
  saveRDS(here::here("data/data-derived/Simulated_catchment/catchmentClose_0s.RDS"))



