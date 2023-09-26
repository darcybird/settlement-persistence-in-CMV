library(magrittr)
library(terra)
library(tidyverse)
library(robinson2020)

## combine wepiin w reese  -----
cellNums <-  readr::read_csv(here::here("data/data-raw/Reese2021/dummyCells.csv"))
catchmentMaize <- readRDS(here::here("data/data-derived/Simulated_catchment/catchmentMaize_all.RDS")) %>% 
  dplyr::rename(dummyCell = cell) %>% 
  dplyr::select(dummyCell, year) 

# get institution counts
readxl::read_xlsx(here::here("data/data-raw/VEPIIN_output/vepiin_06262014_OUTPUT-public.xlsx"), na = c("-999","")) %>% 
  dplyr::select(
    COsitenum,
    Greatkiva, Plaza, 
    OSpitstrs, Greathouse, Multiwalls,
    Towers, Encwall,
    Fieldhouse, Reservoir) %>%
  dplyr::left_join(cellNums, 
                   by = c("COsitenum" = "SITE_NO")) %>% 
  tidyr::pivot_longer(cols = c(Greatkiva:Reservoir), names_to = "institution", values_drop_na = TRUE, values_to = "nInst") %>% 
  dplyr::filter(nInst > 0) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(institution) %>% 
  dplyr::summarise(sumInst = sum(nInst)  )

#create institution dataset
vepiin_output <- 
  readxl::read_xlsx(here::here("data/data-raw/VEPIIN_output/vepiin_06262014_OUTPUT-public.xlsx"), na = c("-999","")) %>% 
  dplyr::select(
    COsitenum,
    Greatkiva, Plaza, OSpitstrs, Greathouse, Multiwalls,Towers, Encwall, Reservoir,
    ph6, ph7, ph8, ph9, ph10, ph11, ph12, ph13, ph14, ph15, ph16, ph17, ph18, ph19) %>%
  dplyr::left_join(cellNums, 
                   by = c("COsitenum" = "SITE_NO")) %>% 
  dplyr::group_by(dummyCell) %>%
  dplyr::summarise(Greatkiva = sum(Greatkiva),
                   Plaza = sum(Plaza),
                   OSpitstrs =sum(OSpitstrs),
                   Greathouse = sum(Greathouse),
                   Multiwalls = sum(Multiwalls),
                   Towers = sum(Towers),
                   Encwall = sum(Encwall),
                   Reservoir = sum(Reservoir),
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


vepiin_inst_long <- vepiin_output %>% 
  #rotate the VEP period columns so they're long
  tidyr::pivot_longer(cols = c(ph6:ph19), names_to = "Period", values_drop_na = TRUE, values_to = "occProb") %>% 
  # pivot the institutions so they're long as well
  tidyr::pivot_longer(cols = c(Greatkiva:Reservoir), names_to = "institution", values_drop_na = TRUE, values_to = "nInst") %>% 
  # limit the dataset to dummyCells with institutions >0 and VEPIIN model >0
  dplyr::filter(nInst > 0, occProb > 0)%>% 
  #get affiliated period years
  dplyr::left_join(CMV_periods, by = c("Period" = "Period")) %>% 
  dplyr::mutate(institution = as.factor(institution)) %>% 
  dplyr::rowwise() %>%
  #lengthen so that each year for which a dummyCell has an institution is a row.
  dplyr::mutate(year = list(Start:End)) %>%
  tidyr::unnest(year) %>% 
  #keep the relevant variables
  dplyr::select(dummyCell, year, institution, nInst) 

# get number of dummy cells with institutions
vepiin_inst_long %>% dplyr::select(dummyCell) %>% dplyr::distinct() #289

# Create a list of dummy cells that are missing from kelsey's dataset. 
matchingCells <- 
  vepiin_inst_long %>% 
  dplyr::inner_join(., catchmentMaize,   by = c("dummyCell" = "dummyCell", "year" = "year")) %>% 
  dplyr::select(dummyCell) %>% dplyr::distinct() %>% as.list()

#identify which cells are and are not missing according to that list
missingCells <- vepiin_inst_long %>% 
  dplyr::left_join(catchmentMaize, by = c("dummyCell" = "dummyCell", "year" = "year")) %>% 
  dplyr::mutate(cellPresence = ifelse(dummyCell %in% c(matchingCells$dummyCell), FALSE, TRUE)) 

#look at what's missing. Note the y-axis labels are obnoxious to highlight the difference in scale
missingCells %>% 
  dplyr::group_by(institution, cellPresence, year) %>% 
  dplyr::summarise(sum = sum(nInst)) %>% 
  ggplot(aes(x = year, y = sum, fill = institution))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,900,20))+
  facet_wrap(vars(cellPresence), nrow = 2, scales = "free_y")

#identify the missing Cells and their affiliated years and institutions
keepMissingCells <- 
  vepiin_inst_long %>% 
  dplyr::left_join(catchmentMaize, by = c("dummyCell" = "dummyCell", "year" = "year")) %>% 
  dplyr::mutate(missingCells = ifelse(dummyCell %in% c(matchingCells$dummyCell), FALSE, TRUE)) %>% 
  dplyr::filter(missingCells == TRUE) 

# bind the missing years to the bottom of the other institutions
cells_inst <- 
  #trim the institutions in both VEPIIN and Reese 2021 to match the Reese2021 annual data
  vepiin_inst_long %>% 
  dplyr::inner_join( catchmentMaize,by = c("dummyCell" = "dummyCell", "year" = "year")) %>% 
  # add back in the missing sites for all their years
  dplyr::bind_rows(keepMissingCells %>% 
                     dplyr::select(-missingCells))

#quick graph
cells_inst %>% 
  ggplot(aes(x = year,   fill = institution ))+
  geom_histogram(position = "stack", bins = 701)

#output for quick add later
cells_inst %>% saveRDS(here::here("data/data-derived/prepAnalysis/cells_inst.RDS"))

#### RASTERIZE the institutions! ------
cells_inst <- readRDS(here::here("data/data-derived/prepAnalysis/cells_inst.RDS"))

# the annual maize weights raster has all the appropriate dimensions. We're going to retool it here.
maizeWts <- terra::rast("data/data-derived/maizeNiche/maizeWts.tif") 

#make a dataframe to fill in empty years
longNAs <- data.frame(year = seq(600,1300,1),
                   Greatkiva = rep(NA, 701),
                   Plaza= rep(NA, 701),
                   OSpitstrs= rep(NA, 701),
                   Greathouse= rep(NA, 701),
                   Multiwalls= rep(NA, 701),
                   Towers= rep(NA, 701),
                   Encwall= rep(NA, 701),
                   Reservoir= rep(NA, 701)) %>% 
  tidyr::pivot_longer(cols = c(Greatkiva:Reservoir)) %>% 
  dplyr::select(-value)

#fill in empty years and nest a bunch
inst_nest <- cells_inst %>% 
  dplyr::group_by(year) %>% 
  #fill in empty years
  dplyr::right_join(longNAs, by = c("year" = "year", "institution" = "name")) %>%
  #there can't be institutions in cell 1 (due to 3 km buff), so put that in there as a place holder. 
  dplyr::mutate(dummyCell = tidyr::replace_na(dummyCell , 1), 
                nInst = tidyr::replace_na( nInst, 0)) %>% 
  dplyr::group_by(institution, year) %>% 
  #nest all the dummy cell locations
  dplyr::nest_by() %>%  
  dplyr::arrange(year) %>% 
  dplyr::group_by(institution) %>% 
  #then nest all the years
  dplyr::nest_by() 

# code to rasterize
RasterizeInstLoad <- function(inst, maize){
  eachInst <- 
    purrr::map(
    .x = 1:nrow(inst),
    .f = function(thisInst){
      
      this <- inst[[thisInst, 2]][[1]] 
      
      yearlyInst <-
        purrr::map(
          .x = 1:nrow(this),
          .f = function(year){
            
            thisYearsinst <- this[[year, 2]][[1]]
            #make an empty raster of the appropriate size and shape
            empty_rast <- terra::rast(maizeWts[[year]], vals = 0) 
            
            #fill raster values with number of institutions in a given year
            empty_rast[as.integer(thisYearsinst$dummyCell)] <- as.matrix(thisYearsinst$nInst) 
            return(empty_rast) # this reports the yearly output
          } ) %>% 
        terra::rast() 
      
      return(yearlyInst)
    }
  )
  return(eachInst)
}

#this will rasterize all institution types. It won't actually take very long
inst_rast_list <- RasterizeInstLoad(inst = inst_nest, maize = maizeWts)

#output to save time in the future
inst_rast_list %>% saveRDS(here::here("data/data-derived/institutions/inst_rast_list.rds"))

#calculate distance from. This could probably be function-ified, but I was having issues and it's low priority
Encwall_dist <- inst_rast_list[[1]] %>% 
  terra::classify(cbind(-Inf, 0, NA), right=TRUE) %>% 
  terra::distance() 

Greathouse_dist <- inst_rast_list[[2]] %>% 
  terra::classify(cbind(-Inf, 0, NA), right=TRUE) %>% 
  terra::distance() 

Greatkiva_dist <- inst_rast_list[[3]] %>% 
  terra::classify(cbind(-Inf, 0, NA), right=TRUE) %>% 
  terra::distance() 

Multiwalls_dist <- inst_rast_list[[4]] %>% 
  terra::classify(cbind(-Inf, 0, NA), right=TRUE) %>% 
  terra::distance() 

OSpitstrs_dist <- inst_rast_list[[5]] %>% 
  terra::classify(cbind(-Inf, 0, NA), right=TRUE) %>% 
  terra::distance() 

Plaza_dist <- inst_rast_list[[6]] %>% 
  terra::classify(cbind(-Inf, 0, NA), right=TRUE) %>% 
  terra::distance() 

Reservoir_dist <- inst_rast_list[[7]] %>% 
  terra::classify(cbind(-Inf, 0, NA), right=TRUE) %>% 
  terra::distance() 

Towers_dist <- inst_rast_list[[8]] %>% 
  terra::classify(cbind(-Inf, 0, NA), right=TRUE) %>% 
  terra::distance() 

#output. These must be output
Encwall_dist     %>% terra::writeRaster(here::here("data/data-derived/institutions/Encwall_dist.tif"))
Greathouse_dist  %>% terra::writeRaster(here::here("data/data-derived/institutions/Greathouse_dist.tif"))
Greatkiva_dist   %>% terra::writeRaster(here::here("data/data-derived/institutions/Greatkiva_dist.tif"))
Multiwalls_dist  %>% terra::writeRaster(here::here("data/data-derived/institutions/Multiwalls_dist.tif"))
OSpitstrs_dist   %>% terra::writeRaster(here::here("data/data-derived/institutions/OSpitstrs_dist.tif"))
Plaza_dist       %>% terra::writeRaster(here::here("data/data-derived/institutions/Plaza_dist.tif"))
Reservoir_dist   %>% terra::writeRaster(here::here("data/data-derived/institutions/Reservoir_dist.tif"))
Towers_dist      %>% terra::writeRaster(here::here("data/data-derived/institutions/Towers_dist.tif"))

#load in (must be loaded in)

Encwall_dist     <- terra::rast(here::here("data/data-derived/institutions/Encwall_dist.tif"))
Greathouse_dist  <- terra::rast(here::here("data/data-derived/institutions/Greathouse_dist.tif"))
Greatkiva_dist   <- terra::rast(here::here("data/data-derived/institutions/Greatkiva_dist.tif"))
Multiwalls_dist  <- terra::rast(here::here("data/data-derived/institutions/Multiwalls_dist.tif"))
OSpitstrs_dist   <- terra::rast(here::here("data/data-derived/institutions/OSpitstrs_dist.tif"))
Plaza_dist       <- terra::rast(here::here("data/data-derived/institutions/Plaza_dist.tif"))
Reservoir_dist   <- terra::rast(here::here("data/data-derived/institutions/Reservoir_dist.tif"))
Towers_dist      <- terra::rast(here::here("data/data-derived/institutions/Towers_dist.tif"))


# get distances from each institution during each year ----

# get cell centroids
cell_centroids <- catchmentMaize %>% 
  dplyr::group_by(dummyCell) %>% 
  #calculate cell centroid
  dplyr::mutate(xy = terra::xyFromCell(maizeWts[[1]], dummyCell) %>% as.data.frame())  %>% 
  tidyr::unnest(xy) %>% 
  sf::st_as_sf(coords = c("x","y"), crs = "EPSG:26912")


#create a list of the distances
inst_list <- list(
  Encwall_dist ,    
  Greathouse_dist  ,
  Greatkiva_dist   ,
  Multiwalls_dist  ,
  OSpitstrs_dist  ,
  Plaza_dist      ,
  Reservoir_dist   ,
  Towers_dist )

yearlyInst <- function(sites, inst_list){
  
  lineup <- sites[1:2]
  
  sites_nest <- sites %>% dplyr::group_by(year) %>% dplyr::nest_by()
  
  #for each institution
  purrr::map(.x = 1:length(inst_list),
             .f = function(thisInst){
               #and each year
               purrr::map(
                 .x = 1:nrow(sites_nest),
                 .f = function(year){
                   #extract the distance from that institution
                   thisYear <- sites_nest[year, 2] %>% 
                     unnest(cols = c(data)) %>% 
                     sf::st_as_sf() %>% 
                     terra::vect() %>% 
                     terra::extract(x = inst_list[[thisInst]][[year]], y= ., ID = FALSE, value = TRUE)
                   
                   #rename that distance to the institution
                   names(thisYear) <-  terra::sources(inst_list[[thisInst]]) %>% 
                     stringr::str_remove("C:/Users/Darcy/Dropbox/R/CMV-persistence/data/data-derived/institutions/") %>% 
                     stringr::str_remove(".tif")
                   
                   return(thisYear)
                 }
               ) %>% #list the cells and years. 
                 bind_rows()
             }) %>%  # then append all the inst's to the lineup
    bind_cols(lineup, .)
}

#this will probably take awhile
InstDistances <- yearlyInst(sites = cell_centroids, inst_list = inst_list)

#export if you'd like
# InstDistances %>% saveRDS(here::here("data/data-derived/institutions/InstDistances.rds"))

# calculate whether an institution is within walking distances ----
instLong <- InstDistances %>%
  tidyr::pivot_longer(Encwall_dist:Towers_dist) %>%
  dplyr::rename(institution = name,
                distance = value) %>%
  dplyr::mutate(institution = stringr::str_remove(institution, "_dist")) %>%
  dplyr::mutate(nearinst = ifelse(distance > 18000, 0, 1)) %>% 
  dplyr::distinct()

nearInst <-
  instLong %>% 
  dplyr::mutate(nearinst = replace_na(nearinst, 0)) %>% 
  dplyr::select(-distance) %>% 
  tidyr::pivot_wider(id_cols = c(year, dummyCell), names_from = institution, values_from = nearinst )

#export
nearInst %>% saveRDS(here::here("data/data-derived/institutions/nearInst.rds"))

#load in real data
nearInst <- read_csv(here::here("data/data-derived/prepAnalysis/nearInst-public.csv"))

#add to bouts
bouts_dgi <-  
  readr::read_csv(here::here("data/data-derived/prepAnalysis/bouts_dg.csv")) %>% 
  dplyr::left_join(nearInst, by = c("year" = "year",  "dummyCell" = "dummyCell")) 

bouts_dgi %>% readr::write_csv(here::here("data/data-derived/prepAnalysis/bouts_dgi.csv"))

