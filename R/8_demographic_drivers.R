library(tidyverse)
library(robinson2020)


#make fig 1 ----
fig1 <-
  robinson2020::vep_demography %>% 
  dplyr::filter(`Study Area` == "CMV") %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(year = list(Start:(End - 1))) %>%
  tidyr::unnest(year) %>% 
  # dplyr::mutate(year = (Start + End)/2) %>%
  dplyr::rename(Schwindt2016 = Population) %>% 
  dplyr::select(year, Schwindt2016) %>% 
  dplyr::bind_rows(data.frame( year = c(599, 1301),
                               Schwindt2016 = c(0, 0))) %>%
  dplyr::right_join(readr::read_csv(here::here("data/data-raw/Reese2021/7-region-population-by-household.csv"),
                                    col_names = c("year", "Reese2021"),
                                    col_types =  list(col_integer(), col_double()))%>% 
                      dplyr::bind_rows(data.frame( year = c(1301),
                                                   Reese2021 = c(0))) %>% 
                      dplyr::mutate(Reese2021 = Reese2021 * 6),
                    by = c("year" = "year")) %>% 
  tidyr::pivot_longer(cols = c(Schwindt2016:Reese2021 ), names_to = "model", values_to = "population") %>%
  dplyr::arrange(year) %>%
  dplyr::filter(!is.na(population)) %>%
  ggplot(aes(x = year, y = population, color = factor(model)))+
  geom_line()+
  scale_color_manual(values = c("black", "purple"))+
    scale_x_continuous(breaks = seq(500,1300,100), name = "Year AD")+
    scale_y_continuous(breaks = seq(0,30000,5000), name = "Modelled Population")+
  # xlab("Year AD")+ ylab("Modelled Population")+
    theme_minimal()+
  theme(legend.position = "top",
        legend.title = element_blank())


png(filename = "fig1_models.png", units = "in", width = 6, height = 4, res = 300)
fig1
dev.off()





# field contention ----

maizeLoad <- 
  readRDS(here::here("data/data-derived/Simulated_catchment/catchmentMaize_all.RDS")) %>%
  tidyr::unnest(catchment) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(year, catchment) %>% 
  dplyr::summarise(houseLoad = sum(nHouse),
                   siteLoad = dplyr::n()) 

fieldContention <- maizeLoad %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise( minSite = min(siteLoad , na.rm = TRUE),
                    medianSite = median(siteLoad , na.rm = TRUE),
                    meanSite = mean(siteLoad , ra.rm = TRUE),
                    maxSite = max(siteLoad ),
                    minHouse = min(houseLoad),
                    medianHouse = median(houseLoad , na.rm = TRUE),
                    meanHouse = mean(houseLoad , ra.rm = TRUE),
                    maxHouse = max(houseLoad))  %>% 
  tidyr::pivot_longer(cols = c(minSite:maxHouse)) %>% 
  dplyr::mutate(Considering = factor(ifelse(name  %in% 
                                       c("minHouse", "medianHouse", "meanHouse", "maxHouse"), 
                                     "nHouse", "nCells")),
                name = stringr::str_remove(name, "Site"),
                name = stringr::str_remove(name, "House")) 

#plot Figure S2. Note that this will not match the manuscript figure as these are dummySite IDs
fieldContention %>% 
  ggplot(aes(x = year, y = value, color = name))+
  geom_line()+
  facet_wrap(~Considering, nrow = 2, scales = "free_y")+
  theme(legend.position = "top")

fieldContention %>% 
  saveRDS(here::here("data/data-derived/prepAnalysis/fieldContention.rds"))

#load in actual field contention values

fieldContention <- readr::read_csv(here::here("data/data-derived/prepAnalysis/fieldContention.csv"))



# N contemporaneous houses ----

nHouse <- readRDS(here::here("data/data-derived/Simulated_catchment/catchmentMaize_all.RDS")) %>% 
  dplyr::select(cell, year, nHouse)

# regional pop ----

reese_regionalPop <- readr::read_csv(here::here("data/data-raw/Reese2021/7-region-population-by-household.csv"),
                                     col_names = c("year", "modelledpop"),
                                     col_types =  list(col_integer(), col_integer())) %>% 
  dplyr::filter(year %in% c(600:1300))

#add to bouts
bouts_d <-  
  readr::read_csv(here::here("data/data-derived/prepAnalysis/yearlybouts.csv")) %>% 
  dplyr::left_join(fieldContention %>%
                     dplyr::filter(Considering == "nHouse", name == "max") %>%
                     dplyr::select(year, value) %>%
                     dplyr::rename(fieldContention = value),
                   by = c("year" = "year")) %>%
  dplyr::left_join(reese_regionalPop,
                   by = c("year" = "year")) %>% 
  dplyr::left_join(nHouse,
                   by = c("year" = "year",  "dummyCell" = "cell")) 

bouts_d %>% 
  readr::write_csv(here::here("data/data-derived/prepAnalysis/bouts_d.csv"))
