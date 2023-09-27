library(magrittr)
library(terra)
library(ggplot2)
library(robinson2020)


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


# calculate start and end dates for each occupation bouts
bouts_broad <-
  #read in catchment 
  readRDS(here::here("data/data-derived/Simulated_catchment/catchmentMaize_all.RDS")) %>% 
  #rename to remind the reviewers that these are not real locations
  dplyr::rename(dummyCell = cell) %>% 
  #isolate to just the relevant data
  dplyr::select(dummyCell, year, nHouse) %>% 
  tibble::as_tibble() %>%
  dplyr::group_by(dummyCell) %>% 
  # identify each occupation bout start and end time. This is clunky but it works
  dplyr::mutate(start = ifelse(year %>% diff() %>% c(0 , .) == 1, NA, year),
                end = ifelse(year %>% diff() %>% c(. , 0) == 1,  NA, year)) %>% 
  dplyr::filter(!is.na(start) | !is.na(end) ) %>% 
  dplyr::select(dummyCell, start, end )  %>% 
  dplyr::group_by(dummyCell) %>% 
  dplyr::reframe(start = as.numeric(na.omit(start)),
                 end = as.numeric(na.omit(end))) %>% 
  dplyr::group_by(dummyCell) %>% 
  dplyr::mutate(boutLength = end - start) %>% 
  dplyr::filter(boutLength > 1) %>% 
  #number the bouts sequentially as a useful way to differentiate the bouts
  dplyr::mutate(boutNo = seq(1:dplyr::n())) 

# removing incredibly long occs
bouts <- 
  bouts_broad %>% 
  dplyr::ungroup() %>% 
  #assign VEP period
  dplyr::left_join(robinson2020::vep_demography %>% 
                     dplyr::filter(`Study Area` == "CMV") %>% 
                     dplyr::select(Period, Start, End) %>% 
                     dplyr::rowwise() %>%
                     dplyr::mutate(year = list(Start:(End - 1))) %>%
                     tidyr::unnest(year) %>%
                     dplyr::rename(vep = Period) %>% 
                     dplyr::select(vep, year),
                   by = c("end" = "year")) %>% 
  #only keep VEP periods 6-19
  dplyr::filter(vep < 20) %>% 
  dplyr::rowwise() %>% 
  #make long
  dplyr::mutate(year = list(start:end)) %>% 
  #remove start and end dates
  dplyr::select(dummyCell, year, boutLength, boutNo, vep) %>% 
  tidyr::unnest(year) %>% 
  #join the annual number of contemporaneous houses
  dplyr::left_join(., 
                   readRDS(here::here("data/data-derived/Simulated_catchment/catchmentMaize_all.RDS")) %>% 
                     #rename to remind the reviewers that these are not real locations
                     dplyr::rename(dummyCell = cell) %>% 
                     #isolate to just the relevant data
                     dplyr::select(dummyCell, year, nHouse, center),
                   by = c("dummyCell" = "dummyCell", "year" = "year") ) %>% 
  dplyr::group_by(dummyCell, boutLength, boutNo, vep) %>% 
  #calculate the maximum number of houses per dummyCell
  dplyr::summarise(maxHouse = max(nHouse),
                   center = max(center)) %>% 
  #remove very long occupation lengths
  dplyr::filter(maxHouse * 80 > boutLength) %>%  # assume max boutLength is double that of Varien 2007 (40 years doubled to 80 years ) and ensure there are enough houses (80 years each) to cover the boutLength 
  dplyr::ungroup() %>% 
  dplyr::select(-maxHouse) %>%
  # bring back the start and end dates
  dplyr::left_join(bouts_broad,
                   by = c("dummyCell"= "dummyCell", "boutLength" = "boutLength", "boutNo" = "boutNo")) %>% 
  dplyr::group_by(dummyCell) %>% 
  dplyr::arrange(start) %>% 
  dplyr::mutate(boutNo = seq(1:dplyr::n())) #renumber boutNos


#export and save
bouts %>% readr::write_csv(here::here("data/data-derived/prepAnalysis/bouts.csv"))
bouts <- readr::read_csv(here::here("data/data-derived/prepAnalysis/bouts.csv"))

bouts %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(year = list(start:end)) %>%
  tidyr::unnest(year) %>% 
  readr::write_csv(here::here("data/data-derived/prepAnalysis/yearlybouts.csv"))


