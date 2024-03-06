library(magrittr)
library(tidyverse)
library(terra)
library(ggplot2)
library(robinson2020)

# load the soils raster as the base raster.
handplant <- terra::rast("data/data-derived/soils/vepiin_hand_planting_200.tif")
empty_rast <- handplant
values(empty_rast) <- 1
remove(handplant)

# assign cell numbers ---- 
reese <- readr::read_csv(
  here::here(
    "data/data-raw/Reese2021/4-occupation-by-household-public.csv"
  )) %>% 
  tidyr::pivot_longer(cols = c(X450:X1300), names_to = "year", values_to = "nHouse") %>% 
  dplyr::mutate(year = as.numeric (stringr::str_remove_all(year, "X"))) %>% 
  dplyr::filter(year %in% c(600:1300),
                nHouse > 0)  

# Assign fake location for public script availability
cells_sites <- 
  reese %>%
  # join with randomly generated cell locations. Note that sites on the same cell should match the same dummy cell. 
  # The distances between the cells will not be maintained, however
  dplyr::left_join(readr::read_csv(here::here("data/dummyCells.csv")),
                   by = c("SITE_ID" = "SITE_ID", "SITE_NO" = "SITE_NO")) %>%
  #rename so the rest of the script works
  dplyr::rename(cell = dummyCell)%>%
  # Aggregate nHouses by cell and year to get total houses per cell per year
  dplyr::group_by(cell, year) %>%
  dplyr::summarise(nHouse = sum(nHouse, na.rm = FALSE),
                   center = max(CENTER)) 

# the below is the script run, but is dependent on locational information, which we cannot publicly provide
# Join all arch sites on the same cell and consider them one settlement.
cells_sites <- 
  reese %>%
  # make into a spatial vector
  dplyr::rename(x = X_COORDS,
                y= Y_COORDS) %>%
  dplyr::select(year, SITE_NO, nHouse, CENTER, x, y) %>%
  sf::st_as_sf(# can turn into spatvector
    coords = c("x", "y"),
    crs = "EPSG:26912") %>%
  # First, calculate the cell that each site is within
  dplyr::mutate(
    cell =
      terra::cellFromXY(
        object = empty_rast,
        xy = sf::st_coordinates(geometry)
      ) %>%
      as.integer() )%>%
  sf::st_drop_geometry() %>%
  # Aggregate nHouses by cell and year to get total houses per cell per year
  dplyr::group_by(cell, year) %>%
  dplyr::summarise(nHouse = sum(nHouse, na.rm = FALSE),
                   center = max(CENTER)) 

#export for script 6
cells_sites %>% saveRDS(here::here("data/data-derived/reese/cells_sites.rds"))

# calculate bouts ----
#assign the number of years (since 600) each cell has been occupied
PriorOccupation <- function(data){
  occupation <- 
    purrr::map(.x = 1:nrow(data),
               .f = function(i){
                 data$occupation[[i]] %>% 
                   tibble::rownames_to_column(var = "yearsPrior") 
               }) 
  
  occupation %>% return()
}


#create occupations
reese_occs_broad <-
  cells_sites  %>% 
  dplyr::filter(year < 1281) %>% 
  tibble::as_tibble() %>%
  dplyr::group_by(cell) %>% 
  # identify each occupation bout start and end time. This is clunky but it works
  dplyr::mutate(start = ifelse(year %>% diff() %>% c(0 , .) == 1, NA, year),
                end = ifelse(year %>% diff() %>% c(. , 0) == 1,  NA, year)) %>% 
  dplyr::filter(!is.na(start) | !is.na(end) )%>% 
  dplyr::select(cell, start, end )  %>% 
  dplyr::group_by(cell) %>% 
  dplyr::reframe(start = as.numeric(na.omit(start)),
                 end = as.numeric(na.omit(end))) %>% 
  dplyr::group_by(cell) %>% 
  #calculate boutLength
  dplyr::mutate(boutLength = end - start) %>% 
  #keep only bouts longer than 7 years per Varien minimum use-life
  dplyr::filter(boutLength > 7) %>% 
  #number sequentially
  dplyr::mutate(boutNo = seq(1:dplyr::n())) 

# make long and add houses and centers back
reese_occs_long <- 
  reese_occs_broad %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>% 
  #make long
  dplyr::mutate(year = list(start:end)) %>% 
  #remove start and end dates
  dplyr::select(cell, year, boutLength, boutNo) %>% 
  tidyr::unnest(year) %>% 
  #add nHouse and center back in
  dplyr::left_join(., 
                   cells_sites,
                   by = c("cell" = "cell", "year" = "year") ) 


# remove unreasonably long bouts
reese_occs_reasonable <- reese_occs_long %>% 
  dplyr::group_by(cell, boutLength, boutNo) %>% 
  #calculate the maximum number of houses per cell
  dplyr::summarise(maxHouse = max(nHouse),
                   center = max(center)) %>% 
  #remove very long occupation lengths
  dplyr::filter(maxHouse * 80 > boutLength) %>%  # assume max boutLength is double that of Varien 2007 (40 years doubled to 80 years ) and ensure there are enough houses (80 years each) to cover the boutLength 
  dplyr::ungroup() %>% 
  dplyr::select(-maxHouse) %>%
  # bring back the start and end dates
  dplyr::left_join(reese_occs_broad,
                   by = c("cell"= "cell", "boutLength" = "boutLength", "boutNo" = "boutNo")) %>% 
  dplyr::group_by(cell) %>% 
  dplyr::arrange(start) %>% 
  dplyr::mutate(boutNo = seq(1:dplyr::n())) #renumber boutNos

#calculate time between bouts ----
interbouts <- reese_occs_reasonable %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_longer(cols = c(start:end)) %>% 
  dplyr::mutate(name = factor(name, levels = c("start", "end"))) %>% 
  dplyr::group_by(cell) %>% 
  dplyr::slice_head(n = -1) %>% 
  dplyr::slice_tail(n = -1) %>% 
  dplyr::reframe(diffBoutNo = diff(boutNo),
                 boutGap = diff(value)) %>% 
  dplyr::filter(diffBoutNo > 0) %>% 
  dplyr::group_by(cell) %>% 
  dplyr::mutate(priorBout = seq( 1:dplyr::n()),
                nextBout = priorBout + 1) %>% 
  dplyr::select(-diffBoutNo)

#there are 187 bout baps < 8 years long
shortInterbouts <- interbouts %>% 
  #keep all the too short bouts
  dplyr::filter(boutGap < 8) %>%
  #remove unnecessary steps
  dplyr::select(-nextBout) %>% 
  #join back with the reasonable bouts
  dplyr::left_join(reese_occs_reasonable,
                   by = c("cell" = "cell", "priorBout" = "boutNo")) %>% 
  dplyr::select(-boutLength, -start) %>% 
  dplyr::mutate(laterEnd = end + boutGap) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(year = list(end:laterEnd)) %>% 
  dplyr::select(-end, -laterEnd, -boutGap, -priorBout, -center) %>% 
  tidyr::unnest(year) %>% 
  dplyr::ungroup() 

# pop it on the end of the regular bouts
reese_interbouts_removed <- reese_occs_reasonable %>% 
  #make long
  dplyr::rowwise() %>% 
  dplyr::mutate(year = list(start:end)) %>% 
  dplyr::select(cell, year) %>% 
  tidyr::unnest(year) %>% 
  dplyr::ungroup() %>% 
  #add the excess
  dplyr::bind_rows(shortInterbouts) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(cell) %>% 
  dplyr::arrange(year) %>% 
  #recalculate bout number
  dplyr::group_by(cell) %>% 
  # identify each occupation bout start and end time. This is clunky but it works
  dplyr::mutate(start = ifelse(year %>% diff() %>% c(0 , .) == 1, NA, year),
                end = ifelse(year %>% diff() %>% c(. , 0) == 1,  NA, year)) %>% 
  dplyr::filter(!is.na(start) | !is.na(end) )%>% 
  dplyr::select(cell, start, end )  %>% 
  dplyr::group_by(cell) %>% 
  dplyr::reframe(start = as.numeric(na.omit(start)),
                 end = as.numeric(na.omit(end))) %>% 
  dplyr::group_by(cell) %>% 
  #calculate boutLength
  dplyr::mutate(boutLength = end - start) %>% 
  #number sequentially
  dplyr::mutate(boutNo = seq(1:dplyr::n())) 
  
#bring centers back
reese_noInterbouts_long <- reese_interbouts_removed %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>% 
  #make long
  dplyr::mutate(year = list(start:end)) %>% 
  tidyr::unnest(year) %>% 
  #bring centers back
  dplyr::left_join(., 
                   cells_sites,
                   by = c("cell" = "cell", "year" = "year") ) %>% 
  dplyr::mutate(nHouse = ifelse(is.na(nHouse), 0, nHouse),
                 center = ifelse(is.na(center), 0, center))

#export the long
reese_noInterbouts_long %>% 
  saveRDS(here::here("data/data-derived/prepAnalysis/bouts_noInterbouts_long.rds"))

# export the broad
reese_noInterbouts_broad <-
  reese_noInterbouts_long %>% 
  dplyr::group_by(cell, boutLength, boutNo) %>% 
  #calculate the maximum number of houses per cell
  dplyr::summarise(maxHouse = max(nHouse),
                   meanHouse = mean(nHouse),
                   center = max(center),
                   start = min(year),
                   end = max(year)) 

#export and save
reese_noInterbouts_broad %>% saveRDS(here::here("data/data-derived/prepAnalysis/bouts_noInterbouts_broad.rds"))
