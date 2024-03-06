library(tidyverse)
library(magrittr)
library(lme4)
library(mgcv)



group_20 <- seq(600, 1279, 20) %>% data.frame() %>% rownames_to_column(var = "group20") %>% 
  dplyr::rename(start = ".") %>% 
  dplyr::mutate(end = ifelse(start == 1260, start + 20, start + 19)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(year = list(start:end)) %>% 
  tidyr::unnest(year) 

#join with the other variables by cell num and year
occSpans <-  readRDS(here::here("data/data-derived/prepAnalysis/bouts_noInterbouts_long.rds"))  %>% 
  dplyr::select(cell, boutNo, boutLength, year, center, nHouse, start) %>% 
  dplyr::left_join(group_20 %>% 
                     dplyr::select(group20, year), 
                   by = c("start" = "year")) %>% 
  dplyr::left_join(read_csv(here::here("data/data-derived/prepAnalysis/inst_factors-public.csv")),
                   by = c("year" = "year", "cell" = "dummyCell")) %>% 
  dplyr::left_join(read_csv(here::here("data/data-derived/prepAnalysis/demo_factors-public.csv")), 
                   by = c("year" = "year", "cell" = "dummyCell")) %>% 
  dplyr::left_join(read_csv(here::here("data/data-derived/prepAnalysis/geo_factors-public.csv")), 
                   by = c("year" = "year", "cell" = "dummyCell")) 


occSpans20 <- occSpans %>% 
  dplyr::mutate(nHouse = ifelse(nHouse <1, 1, nHouse)) %>% 
  dplyr::group_by(cell, boutNo, boutLength, group20) %>% 
  dplyr::summarise(
    year = min(year),
    isCenter = max(center),
    
    cellHouse = mean(nHouse),
    nCenter_18km =  mean(nCenters_18km ),
    
    localCatchment = mean(meanMaize),
    
    nType_18km = mean(nType_18km)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    group20 = factor(group20),
    log_occupation_span = log(boutLength),
    log_cellHouse = log(cellHouse),
    
    isCenter = factor(isCenter),
    std_log_cellHouse = as.numeric(scale(log(cellHouse))),
    std_localCatchment = as.numeric(scale(localCatchment)),
    std_nCenter_18km = as.numeric(scale(nCenter_18km)),
    std_nType_18km = as.numeric(scale(nType_18km))
  ) 


#run lmer
abs_lmer <- lmer(log_occupation_span ~ 
                       isCenter +
                       std_log_cellHouse + std_localCatchment + 
                       std_nCenter_18km + std_nType_18km  + 
                       (1|group20) ,
                     data = occSpans20)

abs_lmer %>% summary()
AIC(abs_lmer)
abs_lmer %>% MuMIn::r.squaredGLMM()

abs_confint <- data.frame(1:6)

abs_confint$var <- as.list(rownames(summary(abs_lmer)$coefficients))
abs_confint$bottom_2_5 <- as.list( round(summary(abs_lmer)$coefficients[,1] - 2*summary(abs_lmer)$coefficients[,2], 3))
abs_confint$top_97_5 <- as.list( round(summary(abs_lmer)$coefficients[,1] + 2*summary(abs_lmer)$coefficients[,2], 3))

abs_confint <- abs_confint[,2:4] %>% tibble::as_tibble() %>% tidyr::unnest()

lmer_out <-
  summary(abs_lmer)$coefficients %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "fixed") %>% 
  dplyr::select(fixed) %>% 
  bind_cols(., 
            bind_cols(summary(abs_lmer)$coefficients, abs_confint[1:6,2:3])) %>% 
  dplyr::mutate(estimate = round(Estimate, 2),
                se = round(`Std. Error`,2),
                tVal = round(`t value`, 2)) %>% 
  dplyr::select(fixed, estimate, se, tVal, bottom_2_5, top_97_5)

lmer_out %>% readr::write_csv(here::here("data/data-derived/modelResults/occspan_20years.csv"))
