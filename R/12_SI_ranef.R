library(tidyverse)
library(nlme)

bout_slicemod_abs_20 <- readRDS(here::here("data/data-derived/prepAnalysis/bout_slicemod_abs_20.rds"))
sliced_lmer <- readRDS("data/data-derived/modelResults/sliced_lmer.rds")
pecos_wide <- read_csv(here::here("data/data-derived/pecos_wide.csv"))

group20Samples <- bout_slicemod_abs_20 %>% dplyr::group_by( group20) %>% dplyr::select(group20) %>% 
  dplyr::mutate(n = dplyr::n()) %>% distinct() %>% arrange(-group20)

random_output <- 
  nlme::ranef(sliced_lmer) %>% 
  as.data.frame() %>% 
  dplyr::select(-grpvar) %>% 
  tidyr::pivot_wider(names_from = "term", values_from = c("condval", condsd))%>%
  dplyr::relocate(grp, 
                  `condval_(Intercept)`,`condsd_(Intercept)`,
                  `condval_isCenter`,`condsd_isCenter`,
                  `condval_std_log_cellHouse`,`condsd_std_log_cellHouse`,
                  `condval_std_localCatchment`,`condsd_std_localCatchment`,
                  `condval_std_nCenter_18km`,`condsd_std_nCenter_18km`,
                  `condval_std_nType_18km`,`condsd_std_nType_18km`)


# random_output %>% write_csv(here::here(
#   "data/data-derived/modelResults/randomEffects.csv"
# ))

isCenter_conf <- random_output %>% 
  dplyr::select(grp, condval_isCenter, condsd_isCenter) %>% 
  dplyr::rename(Estimate = condval_isCenter, 
                SE = condsd_isCenter)

conf_level <- 0.95  # Adjust as needed
isCenter_conf$CI_low <- isCenter_conf$Estimate - qt((1 - conf_level) / 2, df = Inf) * isCenter_conf$SE
isCenter_conf$CI_high <- isCenter_conf$Estimate + qt((1 - conf_level) / 2, df = Inf) * isCenter_conf$SE

df_centerConf <- isCenter_conf %>% 
  dplyr::mutate(grp = as.numeric(grp)) %>% 
  dplyr::left_join(group20Samples, by  = c("grp" = "group20"))%>% 
  dplyr::mutate(grp = 590 + grp * 20,
                effect = (ifelse(CI_low > 0 & CI_high > 0, "Disoccupation",
                                 ifelse( CI_high < 0 & CI_low < 0 , "Persistence", "Not Significant")))) %>% 
  dplyr::arrange(grp) 



ranf_center_fig <- df_centerConf %>% 
  ggplot(aes(x= grp, y = Estimate, ymin = CI_low, ymax = CI_high, color = effect)) +
  geom_point() +
  geom_text(aes(y = CI_low,
                label = n),
            vjust = -0.3, 
            show.legend = FALSE,
            size = 2)+
  geom_errorbar(width = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5)+
  scale_y_continuous(breaks = seq(-5,5,1))+
  scale_x_continuous(breaks = seq(600, 1280, 40))+
  scale_color_manual(values = c(  "#56B4E9", "black","#E69F00"))+
  labs(y = "Random Slope: Whether the cell contains a center", x = "20-year Temporal Bin") +
  geom_vline(
    aes(xintercept = as.numeric(EndDate) - 5), #offset so we don't overlap with any lines.
    data = pecos_wide ,
    linetype = "dashed"
  ) +
  geom_text( inherit.aes = FALSE,
             aes(x = MidDate,
                 y = 4.5,
                 label = pecos),
             vjust = -0.3,
             data = pecos_wide ,
             size = 3)+
  theme_bw()+
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(size =6),
        legend.text = element_text(size = 6),
        legend.title = element_text(size =6),
        legend.position = "top")

png(filename = "figures/fig7.png", units = "in", width = 6, height = 4, res = 300)
ranf_center_fig
dev.off()

# others

create_random_plots <- function(this_Val, this_SD, ylab){
  thisVal_conf <- bind_cols(random_output$grp, this_Val, this_SD) %>% 
    dplyr::rename(grp = ...1,
                  Estimate = ...2, 
                  SE = ...3    )
  
  conf_level <- 0.95  # Adjust as needed
  thisVal_conf$CI_low <- thisVal_conf$Estimate - qt((1 - conf_level) / 2, df = Inf) * thisVal_conf$SE
  thisVal_conf$CI_high <- thisVal_conf$Estimate + qt((1 - conf_level) / 2, df = Inf) * thisVal_conf$SE
  
  thisVal_conf <- thisVal_conf %>% 
    dplyr::mutate(grp = as.numeric(grp)) %>% 
    dplyr::left_join(group20Samples, by  = c("grp" = "group20"))%>% 
    dplyr::mutate(grp = 590 + grp * 20,
                  effect = (ifelse(CI_low > 0 & CI_high > 0, "Disoccupation",
                                   ifelse( CI_high < 0 & CI_low < 0 , "Persistence", "Not Significant")))) %>% 
    dplyr::arrange(grp) 
  
  
  
  thisVal_conf %>% 
    ggplot(aes(x= grp, y = Estimate, ymin = CI_low, ymax = CI_high, color = effect)) +
    geom_point() +
    geom_text(aes(y = CI_low,
                  label = n),
              vjust = -0.3, 
              show.legend = FALSE,
              size = 2)+
    geom_errorbar(width = 5) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5)+
    scale_y_continuous(breaks = seq(-5,5,1))+
    scale_x_continuous(breaks = seq(600, 1280, 40))+
    scale_color_manual(values = c("#E69F00", "black", "#56B4E9"))+
    labs(y = paste(ylab), x = "20-year Temporal Bin") +
    geom_vline(
      aes(xintercept = as.numeric(EndDate) - 5), #offset so we don't overlap with any lines.
      data = pecos_wide ,
      linetype = "dashed"
    ) +
    geom_text( inherit.aes = FALSE,
               aes(x = MidDate,
                   y = max(thisVal_conf$CI_low) + 0.5,
                   label = pecos),
               vjust = -0.3,
               data = pecos_wide ,
               size = 3)+
    theme_bw()+
    theme(axis.title = element_text(size = 8),
          axis.text = element_text(size =6),
          legend.text = element_text(size = 6),
          legend.title = element_text(size =6),
          legend.position = "top")
}

png(filename = "figures/SI/random_Intercepts.png", units = "in", width = 6, height = 4, res = 300)
create_random_plots(this_Val = random_output$`condval_(Intercept)`,
                    this_SD = random_output$`condsd_(Intercept)`,
                    ylab = "Random Intercepts")
dev.off()


png(filename = "figures/SI/random_cellHouse.png", units = "in", width = 6, height = 4, res = 300)
create_random_plots(this_Val = random_output$condval_std_log_cellHouse,
                    this_SD = random_output$condsd_std_log_cellHouse,
                    ylab = "Random Slope: Number of households in the home cell")
dev.off()


png(filename = "figures/SI/random_catchment.png", units = "in", width = 6, height = 4, res = 300)
create_random_plots(this_Val = random_output$condval_std_localCatchment,
                    this_SD = random_output$condsd_std_localCatchment,
                    ylab = "Random Slope: Local Catchment Quality")
dev.off()


png(filename = "figures/SI/random_centers18km.png", units = "in", width = 6, height = 4, res = 300)
create_random_plots(this_Val = random_output$condval_std_nCenter_18km,
                    this_SD = random_output$condsd_std_nCenter_18km,
                    ylab = "Random Slope: Number of centers within 18km")
dev.off()


png(filename = "figures/SI/random_Institutions.png", units = "in", width = 6, height = 4, res = 300)
create_random_plots(this_Val = random_output$condval_std_nType_18km,
                    this_SD = random_output$condsd_std_nType_18km,
                    ylab = "Random Slope: Number of institution types within 18km")
dev.off()

