library(magrittr)
library(terra)

SimulatedResults_1_400 <- 
  readRDS(here::here("data/data-derived/simulation_output/RR_2HousePerCell/simulated_results_600_1000_RR.RDS"))
SimulatedResults_401_500 <- 
  readRDS(here::here("data/data-derived/simulation_output/RR_2HousePerCell/simulated_results_1001_1100_RR.RDS"))
SimulatedResults_501_600 <- 
  readRDS(here::here("data/data-derived/simulation_output/RR_2HousePerCell/simulated_results_1101_1200_RR.RDS"))
SimulatedResults_600_701 <- 
  readRDS(here::here("data/data-derived/simulation_output/RR_2HousePerCell/simulated_results_1201_1300_RR.RDS"))

SimulatedResults <- c( SimulatedResults_1_400, SimulatedResults_401_500, SimulatedResults_501_600 , SimulatedResults_600_701 )

#export final
SimulatedResults %>% saveRDS(here::here("data/data-derived/simulation_output/RR_2HousePerCell/SimulatedResults_600_1300.RDS"))


SimulatedResults <- readRDS(here::here("data/data-derived/simulation_output/RR_2HousePerCell/SimulatedResults_600_1300.RDS"))

maizeWts <- terra::rast("data/data-derived/maizeNiche/maizeWts.tif") 

#review
SimulatedResults[[431]] %>% 
  plot()

#rasterize the output
RasterizeSimulation <- function(sim_output, maize){
  
  yearlySimulation <- 
    
    #the below script takes each year's output and convers it into a raster, which it then returns.
    purrr::map(
      .x = 1:length(sim_output), 
      .f = function(year){
        #take this year's output
        thisYearsSimOutput <- sim_output[[year]]
        #make an empty raster of the appropriate size and shape
        empty_rast <- terra::rast(maize[[year]], vals = 0) 
        #fill raster values with the simulated output
        empty_rast[as.integer(names(thisYearsSimOutput))] <- as.matrix(thisYearsSimOutput ) 
        # this reports the yearly output
        return(empty_rast) 
      } ) 
  #join all outputs into one raster with one raster for each year 
  yearlySimulation %>%
    terra::rast() %>%  
    return()
}

SimRaster <- RasterizeSimulation(sim_output = SimulatedResults, maize = maizeWts)

SimRaster %>% terra::writeRaster("data/data-derived/simulation_output/SimulatedRaster_600_1300.tif")
