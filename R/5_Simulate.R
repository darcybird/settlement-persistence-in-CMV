library(magrittr)
library(terra)
library(furrr)

# be sure to change these options according to your computer's specifications
future::plan(multisession, workers = 23) #workers = number of cores - 1
options(future.globals.maxSize = (2400*1024^2))

#read in all cell catchments
catchmentMaize <- 
  readRDS("data/data-derived/Simulated_catchment/catchmentMaize.rds") %>% 
  dplyr::group_by(year) %>% 
  dplyr::nest_by() %>% 
  dplyr::ungroup()

#read in the weighted maize values
maizeWts <-  terra::rast("data/data-derived/maizeNiche/maizeWts.tif") %>% 
  magrittr::extract2(as.character(600:1300)) %T>%
  # Read values into memory. Note the T-pipe, above.
  terra::set.values()

#the below complicated script is the simulation
FarmingSimulation <- function(people, maize){
  
  #run one year at a time
  yearlySimulation <-
    furrr::future_map( # because each year's results don't affect next year's, we can run each year simulateneously with future map
      .options = furrr_options(seed = TRUE),
      .x = 1:nrow(people), 
      .f = function(year){
        
        thisYearsVillages <- people[[year,2]][[1]] %>% tibble::tibble()
        
        maizeUnwrap <- maize %>% unwrap()
        
        sim_output <- 
          purrr::map(
            .x = 1:nSim,
            .f = function(thisSim){
              
              # create each year's round robin according to each cell's number of field's needed
              RoundRobin <-
                rep.int(thisYearsVillages$cell, times = thisYearsVillages$nFieldsNeeded) %>%
                tibble::as_tibble_col(column_name = "cell") %>%
                dplyr::group_by(cell) %>%
                dplyr::mutate(round = 1:dplyr::n()) %>%
                dplyr::ungroup() %>%
                dplyr::arrange(round) %>%
                #organize all the rounds
                dplyr::group_by(round) %>%
                #randomize the order within each round
                dplyr::mutate(cell = sample(as.character(cell))) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(cell = as.integer(cell)) %>%
                #all we need at the end is the list of cells
                dplyr::pull(cell)
              
              # create a raster of the current year to keep track of available cells (weight >0) and unavailable cells
              availableFields <- maizeUnwrap[[year]]   
              
              drawn_cells <- 
                # insert the sampling order
                purrr::map(
                  .x = RoundRobin,
                  .f = function(x){
                    drawn_cell <-
                      thisYearsVillages %>%
                      dplyr::filter(cell == x) %$% # filter for the current cell
                      catchment[[1]] %>% # extract the current cell's catchment
                      sample(x = ., # sample the catchment one according to the available weights of the cells currently available in the catchment
                             size = 1,
                             prob = availableFields[.][[1]]) #the samples are weighted according to the maizeWts quality
                    
                    availableFields[drawn_cell] <<- 0 #set the chosen cell prob to 0 so it's not longer available
                    return(drawn_cell)
                  }) %>% 
                unlist()
            }) %>% 
          do.call(c, .) %>%
          table() %>%  #produce a table of all the fields that have been selected during the simulation and in how many simulations teach field was selected
          divide_by(nSim) # this produces the probability any given field is selected based on nSim realities
        sim_output %>% return()
        
       
      })
  
  return(yearlySimulation)
}

#practice with 2 simulations for 2 small years
nSim <- 2
thisBatch <- 600:601

start_time <- Sys.time()
simulated_results <- FarmingSimulation(people = catchmentMaize %>% dplyr::filter(year %in% c(thisBatch)) , 
                                       maize = maizeWts[[as.character(thisBatch)]] %>% terra::wrap())
end_time <- Sys.time()
end_time - start_time

# run the below to run the simulation
nSim <- 500

#600-1000
thisBatch <- 600:1000

start_time <- Sys.time()
simulated_results_600_1000 <- FarmingSimulation(people = catchmentMaize %>% dplyr::filter(year %in% c(thisBatch)) , 
                                                maize = maizeWts[[as.character(thisBatch)]]%>% terra::wrap())
end_time <- Sys.time()
runtime1<-end_time - start_time
runtime1
saveRDS(simulated_results_600_1000, file = "simulated_results_600_1000.RDS")

#1001-1100
thisBatch <- 1001:1100

start_time <- Sys.time()
simulated_results_1001_1100 <- FarmingSimulation(people = catchmentMaize %>% dplyr::filter(year %in% c(thisBatch)) , 
                                                 maize = maizeWts[[as.character(thisBatch)]]%>% terra::wrap())
end_time <- Sys.time()
runtime2<- end_time - start_time
runtime2
saveRDS(simulated_results_1001_1100, file = "simulated_results_1001_1100.RDS")

#1101-1200
thisBatch <- 1101:1200

start_time <- Sys.time()
simulated_results_1101_1200 <- FarmingSimulation(people = catchmentMaize %>% dplyr::filter(year %in% c(thisBatch)) , 
                                                 maize = maizeWts[[as.character(thisBatch)]]%>% terra::wrap())
end_time <- Sys.time()
runtime3<-end_time - start_time
runtime3
saveRDS(simulated_results_1101_1200, file = "simulated_results_1101_1200.RDS")

#1201-1300
thisBatch <- 1201:1300

start_time <- Sys.time()
simulated_results_1201_1300 <- FarmingSimulation(people = catchmentMaize %>% dplyr::filter(year %in% c(thisBatch)) , 
                                                 maize = maizeWts[[as.character(thisBatch)]]%>% terra::wrap())
end_time <- Sys.time()
runtime4 <- end_time - start_time
runtime4
saveRDS(simulated_results_1201_1300, file = "simulated_results_1201_1300.RDS")

