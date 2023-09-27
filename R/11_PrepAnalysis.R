library(tidyverse)
library(magrittr)
library(terra)
library(ggpubr)
library(corrplot)

# load data ----
#so this is what we created in the past script
bouts_dgi <- readr::read_csv(here::here("data/data-derived/prepAnalysis/bouts_dgi.csv"))

#make figure 3
fig3 <- readr::read_csv(here::here("data/data-derived/prepAnalysis/bouts.csv")) %>% 
  dplyr::select(dummyCell, center, vep, boutLength, boutNo) %>% 
  dplyr::left_join(robinson2020::vep_demography %>% dplyr::filter(`Study Area` == "CMV") %>% 
                     dplyr::select(Period, Start),
                   by = c("vep" = "Period")) %>% 
  dplyr::mutate(vepStart = factor(vep, 
                                  levels = seq(6,19,1), 
                                  labels = c("6 AD600", "7 AD725", "8 AD800", "9 AD840", "10 AD880", 
                                             "11 AD920", "12 AD980", "13 AD1020", "14 AD1060", "15 AD1100",
                                              "16 AD1140", "17 AD1180", "18 AD1225", "19 AD1260"))) %>% 
  ggplot(aes(x = factor(center), y = boutLength, fill = factor(center)))+
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), 
                    labels=c('non-centers', 'centers'))+
  xlab("VEP period & Starting Year") +
  scale_y_continuous(breaks = c(7,  20, 32, 50, 100, 150, 207)) +
  facet_wrap(vars(vepStart), nrow = 1, strip.position ="bottom" ) +
  theme(legend.position = "top",
        legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))

png(filename = "fig3.png", units = "in", width = 10.4, height = 6, res = 300)
fig3
dev.off()

# make the absolute dataset ----
bout_absVals <- bouts_dgi %>%  
  # assign vep period according to disoccupation date
  dplyr::filter(boutLength > 6) %>% 
  # get the stats for each bout
  dplyr::group_by(dummyCell, boutNo, boutLength, vep) %>% 
  dplyr::summarise(center = max(center),
                   meanMaize = mean(meanMaize),
                   relToAll = mean(relativeToAll),
                   nHouse = mean(nHouse),
                   
                   fieldContention = max(fieldContention), #I may have written "mean" in the original doc, but it's max.
                   modelledpop = mean(modelledpop),
                   
                   greatkiva = max(Greatkiva),
                   plaza = max(Plaza),
                   oversizepit = max(OSpitstrs),
                   greathouse = max(Greathouse),
                   multiwalls = max(Multiwalls),
                   encwall = max(Encwall),
                   towers = max(Towers),
                   reservoir = max(Reservoir)) %>% 
  dplyr::ungroup() %>% 
  #standardize / take logs
  dplyr::mutate(std_nHouse = as.numeric(scale(nHouse)), 
                log10_boutLength = log10(boutLength), 
                std_fieldContention = as.numeric(scale(fieldContention)),
                std_regionalPop = as.numeric(scale(modelledpop)),
                log10_boutNo = log10(boutNo),
                std_localCatchmentQuality = as.numeric(scale(meanMaize)),
                std_catchmentRelToRegion = as.numeric(scale(relToAll))) %>% 
  #keep only the relevant variables
  dplyr::select(dummyCell, vep, log10_boutNo, log10_boutLength, center, 
                std_nHouse,std_fieldContention, std_regionalPop,
                std_localCatchmentQuality, std_catchmentRelToRegion,
                greatkiva, plaza,
                oversizepit, greathouse, multiwalls,
                encwall, towers,
                reservoir)



bout_absVals %>% write_csv(here::here("data/data-derived/prepAnalysis/bout_absVals.csv"))



# make relative dataset -----
bout_2phases <- bouts_dgi %>%  
  # assign vep period according to disoccupation date
  dplyr::filter(boutLength > 6) %>% 
  #assign early and late phases
  dplyr::mutate(lateBoutStart = ifelse(boutLength < 12, end - 2, end - 5)) %>% 
  dplyr::mutate(phase = ifelse((year <= lateBoutStart) & (year >= start), 
                                      0,
                                      1)) %>% 
  # get stats for each phase
  dplyr::group_by(dummyCell, vep, boutNo, boutLength, phase) %>% 
  dplyr::summarise(center = max(center),
                   meanMaize = mean(meanMaize),
                   relToAll = mean(relativeToAll),
                   nHouse = mean(nHouse),
                   
                   fieldContention = max(fieldContention), #I may have written "mean" in the original doc, but it's max.
                   modelledpop = mean(modelledpop),
                   
                   greatkiva = max(Greatkiva),
                   plaza = max(Plaza),
                   oversizepit = max(OSpitstrs),
                   greathouse = max(Greathouse),
                   multiwalls = max(Multiwalls),
                   encwall = max(Encwall),
                   towers = max(Towers),
                   reservoir = max(Reservoir)) %>% 
  dplyr::ungroup()

#subtract the early phase from the late phase
bout_relative <- 
  tibble::as_tibble(
  bind_cols(
    bout_2phases %>% 
      dplyr::filter(phase == 1)  %>% 
      dplyr::select(dummyCell, boutNo, boutLength, center, vep),
    (bout_2phases %>% dplyr::filter(phase == 1)%>% dplyr::select(-dummyCell, -boutNo, -boutLength, -center, -vep)) - 
      (bout_2phases %>% dplyr::filter(phase == 0)%>% dplyr::select(-dummyCell, -boutNo, -boutLength, -center, -vep))
  )) %>% 
  dplyr::select(-phase, -boutNo) %>% 
  #standardize
  dplyr::mutate(log10_boutLength = log10(boutLength), 
                Δ_std_nHouse = as.numeric(scale(nHouse)), 
                Δ_std_fieldContention = as.numeric(scale(fieldContention)),
                Δ_std_modelledpop = as.numeric(scale(modelledpop)),
                Δ_std_localCatchmentQuality = as.numeric(scale(meanMaize)),
                Δ_std_catchmentRelToRegion = as.numeric(scale(relToAll)),
                Δ_std_regionalPop = as.numeric(scale(modelledpop))) %>% 
  #rename for clarity
  dplyr::rename(
    Δ_greatkiva = greatkiva,
    Δ_plaza = plaza,
    Δ_oversizepit = oversizepit, 
    Δ_greathouse = greathouse, 
    Δ_multiwalls = multiwalls,
    Δ_encwall=  encwall, 
    Δ_towers = towers,
    Δ_reservoir =  reservoir,
  ) %>% 
  #keep only relevant variables
  dplyr::select(
    dummyCell, vep,log10_boutLength, center, 
    Δ_std_nHouse,Δ_std_fieldContention, Δ_std_regionalPop,
    Δ_std_localCatchmentQuality, Δ_std_catchmentRelToRegion,
    Δ_greatkiva, Δ_plaza,
    Δ_oversizepit, Δ_greathouse, Δ_multiwalls,
    Δ_encwall, Δ_towers,
    Δ_reservoir
  )
  
bout_2phases %>% readr::write_csv(here::here("data/data-derived/prepAnalysis/bout_2phases.csv"))
bout_relative %>% readr::write_csv(here::here("data/data-derived/prepAnalysis/bout_relative.csv"))


