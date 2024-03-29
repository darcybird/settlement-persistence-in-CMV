library(tidyverse)
library(magrittr)

# load data ----
yearlybouts <-   readRDS(here::here("data/data-derived/prepAnalysis/bouts_noInterbouts_long.rds")) 

bouts <- readRDS(here::here("data/data-derived/prepAnalysis/bouts_noInterbouts_broad.rds")) %>% 
  dplyr::select(cell, boutLength, boutNo, start, end) %>% 
  dplyr::ungroup()

# create slices
boutSlice <- bouts %>% 
  dplyr::rowwise() %>% 
  #every startslice will be every 8 years
  dplyr::mutate(startSlice = list(seq(start, end, 8))) %>% 
  tidyr::unnest(startSlice) %>% 
  #end Slices will be every 7 years. 
  #this script makes sure the end Slice isn't only the final year
  dplyr::mutate(endSlice = ifelse(startSlice + 8 < end,
                                  startSlice + 7, end),
                sliceLength = endSlice - startSlice,
                #label the event. if a bout ends, event = 1, else 0
                event = ifelse(endSlice == end, 1, 0),
                midSlice = floor((startSlice + endSlice)/2 ))%>% 
  #remove any Slicelengths that are too short (that year is included in the prior Slice)
  dplyr::filter(sliceLength > 0) %>%
  dplyr::group_by(cell, boutNo) %>% 
  #number the slices
  dplyr::mutate(sliceNo = seq(1:dplyr::n())) %>% 
  #reorganize for clarity
  dplyr::relocate(cell, boutNo, start, end, boutLength, 
                  event, sliceNo, midSlice, startSlice, endSlice
                  # , sliceLength
  ) 


#make long to join other variables
boutSlice_long <- boutSlice %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(year = list(startSlice:endSlice)) %>% 
  tidyr::unnest(year) %>% 
  #keep relevant information
  dplyr::select(cell, year, boutNo, sliceNo, midSlice, event)


#load the produced  vars
# inst <- readRDS(here::here("data/data-derived/prepAnalysis/inst_factors.rds"))
# dem <- readRDS(here::here("data/data-derived/prepAnalysis/demo_factors.rds"))
# geo <- readRDS(here::here("data/data-derived/prepAnalysis/geo_factors.RDS"))

#load the correct vars, relabel dummy cell as cell
inst <- read_csv(here::here("data/data-derived/prepAnalysis/inst_factors-public.csv")) %>% dplyr::rename(cell = dummyCell)
dem <- read_csv(here::here("data/data-derived/prepAnalysis/demo_factors-public.csv")) %>% dplyr::rename(cell = dummyCell)
geo <- read_csv(here::here("data/data-derived/prepAnalysis/geo_factors-public.csv")) %>% dplyr::rename(cell = dummyCell)

#join with the other variables by cell num and year
bout_slicemod <- yearlybouts %>% 
  dplyr::select(cell, boutNo, year, center, nHouse) %>% 
  dplyr::left_join(boutSlice_long, by = c("year" = "year", "cell" = "cell", "boutNo" = "boutNo")) %>% 
  dplyr::left_join(inst, by = c("year" = "year", "cell" = "cell")) %>% 
  dplyr::left_join(dem, by = c("year" = "year", "cell" = "cell")) %>% 
  dplyr::left_join(geo, by = c("year" = "year", "cell" = "cell")) %>% 
  dplyr::mutate(nCenters = ifelse(is.na(nCenters_18km), 0, nCenters_18km))


check <- bout_slicemod %>% dplyr::filter(year > 960, year < 1050) 

check %>% 
  ggplot(aes(x = midSlice, y = nHouse, color = factor(center)))+
  geom_jitter() +
  facet_wrap(vars(event))

cells_inst %>% dplyr::filter(year > 950, year < 1050)%>% 
  ggplot(aes(x = year, fill = institution ))+
  geom_histogram(position = "stack", bins = 126) 

#summarize these variables by SliceNo
bout_slicemod_abs <- bout_slicemod %>% 
  dplyr::mutate(nHouse = ifelse(nHouse <1, 1, nHouse)) %>% 
  dplyr::group_by(cell, boutNo, sliceNo, midSlice, event) %>% 
  dplyr::summarise(
    isCenter = max(center),
    
    cellHouse = mean(nHouse),
    nCenter_18km =  mean(nCenters_18km ),
    
    localCatchment = mean(meanMaize),
    
    nInst_18km = mean(nTot_18km),
    nType_18km = mean(nType_18km)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    event = factor(event),
    log_cellHouse = log(cellHouse),
    std_log_cellHouse = as.numeric(scale(log(cellHouse))),
    std_nCenter_18km = as.numeric(scale(nCenter_18km)),
    
    std_localCatchment = as.numeric(scale(localCatchment)),
    
    std_nInst_18km  = as.numeric(scale(nInst_18km )),
    std_nType_18km = as.numeric(scale(nType_18km))
  )


group_20 <- seq(600, 1279, 20) %>% data.frame() %>% rownames_to_column(var = "group20") %>% 
  dplyr::rename(start = ".") %>% 
  dplyr::mutate(end = ifelse(start == 1260, start + 20, start + 19)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(year = list(start:end)) %>% 
  tidyr::unnest(year) 

bout_slicemod_abs_20 <- bout_slicemod_abs %>% 
  dplyr::left_join(group_20 %>% 
                     dplyr::select(-start, -end), relationship = "many-to-one", c("midSlice" = "year"))  %>% 
  dplyr::mutate(group20 = as.integer(group20))


bout_slicemod_abs_20 %>% 
  saveRDS(here::here("data/data-derived/prepAnalysis/bout_slicemod_abs_20.rds"))
group_20 %>% saveRDS(here::here("data/data-derived/prepAnalysis/20_year_groups.rds"))

bout_slicemod_abs_20 %>% 
  tidyr::pivot_longer(cols = c(cellHouse:nType_18km)) %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
  facet_wrap(vars(name), scales = "free")  

bout_slicemod_abs_20 %>% 
  tidyr::pivot_longer(cols = c(std_log_cellHouse:std_nType_18km)) %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
  facet_wrap(vars(name), scales = "free")  

bout_slicemod_abs_20$midSlice %>% hist()
bout_slicemod_abs_20$group20 %>% hist()