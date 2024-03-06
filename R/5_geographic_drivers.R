library(tidyverse)
library(terra)
library(magrittr)

## calculate local catchment quality ----

# create local quality ----
maizeWts <- terra::rast("data/data-derived/maizeNiche/maizeWts.tif") 
maizeWts <- maizeWts[[1:681]]


cell_catchments <- readRDS(here::here("data/data-derived/prepAnalysis/catchmentMaize.RDS")) %>% 
  dplyr::select(-nHouse, -nFieldsNeeded, -maxRadius) %>% 
  tidyr::unnest(catchment) %>% 
  dplyr::group_by(cell, year, catchment) %>% 
  dplyr::summarize(n = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year) %>% 
  tidyr::nest() %>% 
  dplyr::arrange(year)

extractMaize <- function(catchment, maize, startyear){
  #for each year
  purrr::map(
    .x = 1:nrow(catchment),
    .f = function(thisyear){
      # select each years assemblage of catchments
      thisYearsCatchment <- catchment[[thisyear,2]][[1]] %>% 
        dplyr::group_by(cell) %>% 
        dplyr::nest_by()
      
      maizeUnwrap <- maize %>% 
        terra::unwrap()
      
      thisYearsCatchment$meanMaize <- 
        purrr::map(
          .x = 1:nrow(thisYearsCatchment),
          .f = function(thiscell){
            
            catchQual <- 
              # take the number of times a field appears
              thisYearsCatchment$data[[thiscell]]$n *
              #and multiply it by the field's maize quality 
              maizeUnwrap[[thisyear]][thisYearsCatchment$data[[thiscell]]$catchment][[1]]  
            #then calculate the mean catchment quality
            meanCatchment <- sum(catchQual) / sum(thisYearsCatchment$data[[thiscell]]$n)
            
            return(meanCatchment)
          }
        ) %>% unlist()
      thisYearsCatchment$year <- thisyear + startyear
      return(thisYearsCatchment %>% dplyr::select(year, cell, meanMaize))
    }
  )
}
#the below script will take awhile and it's not the most graceful, but it does work. 
localCatchmentQuality <-
  dplyr::bind_rows(
    #note that this is for a computer with 16GB of RAM. Less ram will require more separations
    extractMaize(catchment =  cell_catchments[1:400,], maize = maizeWts[[1:400]] %>% terra::wrap(), startyear = 599), 
    extractMaize(catchment =  cell_catchments[401:681,], maize = maizeWts[[401:681]] %>% terra::wrap(), startyear = 999)
  )

#export
localCatchmentQuality %>% saveRDS(here::here("data/data-derived/prepAnalysis/geo_factors.RDS"))
