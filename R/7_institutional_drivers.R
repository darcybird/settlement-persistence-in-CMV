library(magrittr)
library(terra)
library(tidyverse)
library(robinson2020)


handplant <- terra::rast("data/data-derived/soils/vepiin_hand_planting_200.tif")
empty_rast <- handplant
values(empty_rast) <- 1
remove(handplant)

cellNums <- read_csv(here::here("data/dummyCells.csv")) %>% 
  dplyr::rename(cell = dummyCell)

catchmentMaize <- readRDS(here::here("data/data-derived/prepAnalysis/catchmentMaize.RDS")) %>% 
  dplyr::select(cell, year, nHouse)

#create institution dataset
vepiin_output <- 
  readxl::read_xlsx(here::here("data/data-raw/VEPIIN_output/vepiin_06262014_OUTPUT-public.xlsx"), na = c("-999","")) %>% 
  dplyr::select(
    COsitenum,
    Greatkiva, Plaza,  Greathouse, Multiwalls, OSpitstrs,Towers, Encwall, Reservoir,
    ph6, ph7, ph8, ph9, ph10, ph11, ph12, ph13, ph14, ph15, ph16, ph17, ph18, ph19) %>%
  dplyr::left_join(cellNums, 
                   by = c("COsitenum" = "SITE_NO")) %>% 
  dplyr::mutate(
    Greatkiva = as.numeric(Greatkiva),
    Plaza = as.numeric(Plaza),
    Greathouse = as.numeric(Greathouse),
    Multiwalls = as.numeric(Multiwalls),
    across(Greatkiva:ph19, ~replace_na(.x, 0))
  ) %>% 
  dplyr::group_by(cell) %>%
  dplyr::summarise(Greatkiva = sum(Greatkiva),
                   Plaza = sum(Plaza),
                   OSpitstrs =sum(OSpitstrs),
                   Greathouse = sum(Greathouse),
                   Multiwalls = sum(Multiwalls),
                   Towers = sum(Towers),
                   Encwall = sum(Encwall),
                   Reservoir = sum(Reservoir),
                   ph6 = max(ph6), 
                   ph6 = max(ph6), 
                   ph7 = max(ph7),
                   ph8 = max(ph8), 
                   ph9 = max(ph9), 
                   ph10 = max(ph10), 
                   ph11 = max(ph11), 
                   ph12 = max(ph12), 
                   ph13 = max(ph13), 
                   ph14 = max(ph14), 
                   ph15 = max(ph15), 
                   ph16 = max(ph16), 
                   ph17 = max(ph17), 
                   ph18 = max(ph18), 
                   ph19 = max(ph19))

# load in the CMV periods
CMV_periods <- robinson2020::vep_demography %>% 
  dplyr::filter(`Study Area` == "CMV") %>% 
  dplyr::mutate(Period = stringr::str_c("ph", Period),
                End = End - 1) %>% 
  dplyr::select(-`Study Area`, -Population)

#create a long institutional dataset
vepiin_inst_long <- vepiin_output %>% 
  #rotate the VEP period columns so they're long
  tidyr::pivot_longer(cols = c(ph6:ph19), names_to = "Period", values_drop_na = TRUE, values_to = "occProb") %>% 
  # pivot the institutions so they're long as well
  tidyr::pivot_longer(cols = c(Greatkiva:Reservoir), names_to = "institution", values_drop_na = TRUE, values_to = "nInst") %>% 
  # limit the dataset to dummyCells with institutions >0 and VEPIIN model >0
  dplyr::filter(nInst > 0, occProb > 0) %>% 
  #get affiliated period years
  dplyr::left_join(CMV_periods, by = c("Period" = "Period")) %>% 
  dplyr::mutate(institution = as.factor(institution)) %>% 
  dplyr::rowwise() %>%
  #lengthen so that each year for which a dummyCell has an institution is a row.
  dplyr::mutate(year = list(Start:End)) %>%
  tidyr::unnest(year) %>% 
  #keep the relevant variables
  dplyr::select(
    cell,
    year, institution, nInst) 

# get number of dummy cells with institutions
vepiin_inst_long %>% dplyr::select(cell) %>% dplyr::distinct()


# some are missing from kelsey's dataset. Look at them below. 
matchingcells <- 
  vepiin_inst_long %>% 
  dplyr::inner_join(catchmentMaize,
                    by = c( "cell" = "cell", "year" = "year")) %>% 
  dplyr::select(cell) %>% dplyr::distinct() %>% as.list()

#identify which cells are and are not missing 
missingCells <- vepiin_inst_long %>% 
  dplyr::left_join(catchmentMaize %>% dplyr::select(year, cell),
                   by = c("cell" = "cell", "year" = "year")) %>% 
  dplyr::mutate(missingSites = ifelse(cell %in% c(matchingcells$cell), FALSE, TRUE)) 

#look at what's missing
missingCells %>% 
  dplyr::group_by(institution, missingSites, year) %>% 
  dplyr::summarise(sum = sum(nInst)) %>% 
  ggplot(aes(x = year, y = sum, fill = institution))+
  geom_col()+
  facet_wrap(vars(missingSites), nrow = 2, scales = "free_y")
# missing data are fairly evenly distributed over time

# bind the missing years to the bottom of the other institutions
keepMissingCells <- 
  vepiin_inst_long %>% 
  dplyr::left_join(catchmentMaize %>% dplyr::select(year, cell),
                   by = c("cell" = "cell", "year" = "year")) %>% 
  dplyr::mutate(missingSites = ifelse(cell %in% c(missingCells$cell), FALSE, TRUE)) %>% 
  dplyr::filter(missingSites == TRUE) 

#bring the missing sites back in
cells_inst <- 
  vepiin_inst_long %>% 
  #trim the institutions in both VEPIIN and Reese 2021 to match the Reese2021 annual data
  dplyr::inner_join(catchmentMaize %>% dplyr::select(year, cell),
                    by = c("cell" = "cell", "year" = "year")) %>% 
  #add back in the missing sites for all their years
  dplyr::bind_rows(keepMissingCells %>% 
                     dplyr::select(-missingSites))

cells_inst %>% 
  ggplot(aes(x = year,   fill = institution ))+
  geom_histogram(position = "stack", bins = 701)

#### Get institution distances! ------

#vectorize the institution data
inst_vect <- cells_inst %>% 
  bind_cols(.,  
            #supply xy location for each cell
            terra::xyFromCell(empty_rast,
                              .$cell) )%>%  
  dplyr::arrange(year) %>% 
  dplyr::group_by(year)%>% 
  terra::vect(geom = c("x", "y"), crs = "EPSG:26912")

inst_vect$cell <- as.integer(inst_vect$cell )

# get distances from each institution during each year ----
sites_vect <-  readRDS(here::here("data/data-derived/reese/cells_sites.rds")) %>% 
  dplyr::select(cell, year, nHouse) %>%
  dplyr::arrange(year) %>%
  group_by(year) %>%
  dplyr::mutate(xy = terra::xyFromCell(empty_rast, cell) %>%
                  as.data.frame()) %>%
  tidyr::unnest(xy) %>%
  sf::st_as_sf(coords = c("x", "y"),
               crs = "EPSG:26912") %>%
  terra::vect()

lookingAround <- function(looking_for, looking_from, dist){
  purrr::map(
    .x = 600:1279,
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

nearbyInst_18km <- lookingAround(looking_for = inst_vect, looking_from = sites_vect, dist = 18000)

inst_18km_summary <- nearbyInst_18km %>% 
  dplyr::group_by(cell, year) %>% 
  #count number of institutional structures within 18km
  dplyr::summarise(nTot_18km = sum(nInst)) %>% 
  #count number of types of institutional structures within 18km
  dplyr::left_join(
    nearbyInst_18km %>% group_by(cell, year) %>% dplyr::select(-nInst, -nHouse) %>% 
      dplyr::distinct() %>% dplyr::summarise(nType_18km = dplyr::n()),
    by = c("cell" = "cell", "year" = "year")) 

inst_summary <- inst_18km_summary %>% 
  #add back any cell and year without any institutions within 10km
  dplyr::right_join(catchmentMaize %>% dplyr::select(year, cell),
                    by = c("cell" = "cell", "year" = "year")) %>% 
  #replace Nas with zeros
  dplyr::mutate(nTot_18km = replace_na(nTot_18km, 0),
                nType_18km = replace_na(nType_18km, 0)) 

inst_summary %>% 
  ggplot(aes(y = nTot_18km, x = nType_18km))+
  geom_point()

inst_summary %>% 
  ggplot(aes(x = year, y = nType_18km))+
  geom_point()

inst_summary %>% 
  ggplot(aes(x = year, y = nTot_18km))+
  geom_point()

inst_summary %>% saveRDS(here::here("data/data-derived/prepAnalysis/inst_factors.rds"))

