library(tidyverse)
library(terra)
library(magrittr)

## calculate local catchment quality ----

# create local quality ----
catchmentClose <- 
  readRDS(here::here("data/data-derived/Simulated_catchment/catchmentClose_0s.RDS"))

maizeWts <- terra::rast("data/data-derived/maizeNiche/maizeWts.tif") %>% 
  magrittr::extract2(as.character(600:1300)) %T>%
  # Read values into memory. Note the T-pipe, above.
  terra::set.values()

maizeQual <- catchmentClose %>% 
  dplyr::select(-nHouse, -nFieldsNeeded, -prefRadius) %>% 
  tidyr::unnest(catchment) %>% 
  dplyr::group_by(cell, year, catchment) %>% 
  dplyr::summarize(n = dplyr::n()) %>% 
  dplyr::distinct() %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year) %>% 
  tidyr::nest() %>% 
  dplyr::arrange(year)

extractMaize <- function(catchment, maize, startyear){
  #for each year
  purrr::map(
    .x = 1:nrow(catchment),
    .f = function(thisyear){
      # get the year's occupied cell lists
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
    extractMaize(catchment =  maizeQual[1:400,], maize = maizeWts[[1:400]] %>% terra::wrap(), startyear = 599), 
    extractMaize(catchment =  maizeQual[401:701,], maize = maizeWts[[401:701]] %>% terra::wrap(), startyear = 999)
  )

localCatchmentQuality %>% saveRDS(here::here("data/data-derived/prepAnalysis/localCatchmentQuality.RDS"))
  
rm(list = ls())
gc()

### Make background maize ----
#load the maize quality values
maizeWts <- terra::rast("data/data-derived/maizeNiche/maizeWts.tif") 
#load the simulation output. Note that I have not provided this since the output is too large to be stored on GitHub.
SimRaster <- terra::rast("data/data-derived/simulation_output/SimulatedRaster_600_1300.tif")

Rasterize <- function(d, maize){
  yearly <- 
    purrr::map(
      .x = 1:nrow(d), 
      .f = function(year){
        #extract this year's occupation values
        thisYearOcc <- d[year, 2] %>% tidyr::unnest(cols = c("data"))
        #make an empty raster of the appropriate size and shape
        empty_rast <- terra::rast(maize[[year]], vals = TRUE ) 
        # mark the occupied cells as being NOT available for new occupation
        empty_rast[thisYearOcc$cell] <- FALSE 
        return(empty_rast) # this reports the yearly output
      } ) 
  yearly %>%
    terra::rast() %>%  #make into a raster stack
    return()
}

#nest the occupation by year
unoccupied_rast <- readRDS(here::here("data/data-derived/Simulated_catchment/catchmentMaize_all.RDS")) %>% 
  dplyr::select(cell, year, nHouse) %>% 
  dplyr::group_by(year) %>% 
  dplyr::nest_by() %>% 
  Rasterize(d = ., maize = maizeWts)

#create the complement of the raster output
SimRaster_invert <- 1 - SimRaster

# multiply maize quality by whether or not it's occupied and how competitive the fields are
fieldDesireability <- maizeWts * unoccupied_rast  * SimRaster_invert
#replace NA's with zeros
fieldDesireability %<>% terra::subst(NA, 0) 

#look at all maps for one of the years with higher populations
SimRaster_invert[['1031']] %>% terra::plot()
unoccupied_rast[['1031']] %>% terra::plot()
fieldDesireability[['1031']] %>% terra::plot()

# Calculate cell deireability ----
# NOTE TO REVIEWERS: the submitted manuscript has an inaccurate description of this step 

# I actually used vonNeumann with a 1 cell radius (line 125). I wrote that I used the 2 cell radius (line 126).
# The difference is minimal. I will rerun these shortly to confirm that the results are the same. 
# When I resubmit, I intend to use the 2 cell radius.
# But for now, I'm including the script as it was run, rather than how I thought I ran it.
vonNeumann <- matrix(c(0,1,0,1,1,1,0,1,0), nrow=3)
vonNeumannR2 <- matrix(c(0,0,1,0,0,0,1,2,1,0,1,2,2,2,1,0,1,2,1,0,0,0,1,0,0), nrow = 5)

#  use the terra::focal function to calculate the quality of cells for habitation. This will take awhile
backgroundVN <- cellDesireability %>% terra::focal(w = vonNeumann, fun = "mean")

#export it to save time
backgroundVN %>% terra::writeRaster(here::here("data/data-derived/maizeNiche/backgroundVN.tif"), overwrite = TRUE)

# create background maize median ----
backgroundVN <- terra::rast(here::here("data/data-derived/maizeNiche/backgroundVN.tif"))

getMedians <-  function(data){
  out <- 
    purrr::map(
      #for each year
      .x = 1:terra::nlyr(data),
      .f = function(x){
        yearlyMedian <-
          data[[x]] %>% 
          #get the values
          values() %>% 
          #remove NAs
          na.omit() %>% 
          #calculate median
          median()
        return(yearlyMedian)
      }
    ) 
  return(out) 
} 

yearlyMedian <- data.frame(year= seq(600,1300,1), 
                           medianAvailable = getMedians(data = backgroundVN)%>%
                             unlist() )

#export to save time for future
yearlyMedian %>%  readr::write_csv(here::here("data/data-derived/prepAnalysis/yearlyMedian.csv"))


#load in the actual data
yearlyMedian <- readr::read_csv(here::here("data/data-derived/prepAnalysis/yearlyMedian.csv"))
localCatchmentQuality <- readr::read_csv(here::here("data/data-derived/prepAnalysis/localCatchmentQuality-public.csv"))

catchmentQuality <- localCatchmentQuality %>% 
  dplyr::left_join(yearlyMedian %>% 
                     dplyr::select(year, medianAvailable), 
                   by = c("year" = "year")) %>% 
  dplyr::mutate(relativeCatchmentQuality = meanMaize - medianAvailable) %>% 
  dplyr::rename(localCatchmentQuality = meanMaize) %>% 
  dplyr::select(-medianAvailable) %>% 
  dplyr::distinct()

#add to bouts
bouts_dg <-  
  readr::read_csv(here::here("data/data-derived/prepAnalysis/bouts_d.csv")) %>% 
  dplyr::left_join(catchmentQuality, by = c("year" = "year",  "dummyCell" = "dummyCell")) 

bouts_dg %>% 
  readr::write_csv(here::here("data/data-derived/prepAnalysis/bouts_dg.csv"))
