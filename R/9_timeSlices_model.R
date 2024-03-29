library(tidyverse)
library(magrittr)
library(lme4)
library(mgcv)
library(ggpubr)
library(performance)
# load data
bout_slicemod_abs_20 <- readRDS(here::here("data/data-derived/prepAnalysis/bout_slicemod_abs_20.rds"))


# lmer 20 year groups ----

# view samples
bout_slicemod_abs_20 %>% dplyr::group_by( group20) %>% dplyr::select(group20) %>% 
  dplyr::mutate(n = dplyr::n()) %>% distinct() %>% arrange(-group20)

bout_slicemod_abs_20 %>% select(sliceNo) %>% 
  dplyr::group_by(sliceNo) %>% 
  dplyr::mutate( n = dplyr::n()) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(-sliceNo) 

#run model (it takes my computer ~45 minutes to run)
sliced_lmer <- glmer(event ~ factor(sliceNo) +
                     isCenter +
                       std_log_cellHouse + std_localCatchment + 
                       std_nCenter_18km + std_nType_18km  + 
                       (1 + isCenter +
                          std_log_cellHouse + std_localCatchment +
                          std_nCenter_18km + std_nType_18km | group20) ,
                     data = bout_slicemod_abs_20 ,
                     family = binomial)

#output results to 
sliced_lmer %>%  saveRDS("data/data-derived/modelResults/sliced_lmer.rds")

#output results to 
sliced_lmer <- readRDS("data/data-derived/modelResults/sliced_lmer.rds")

sliced_lmer %>% summary()

performance::icc(sliced_lmer)
MuMIn::r.squaredGLMM(sliced_lmer)

# PDPS ---
PDPout <- ggeffects::ggpredict(sliced_lmer)
A <-
  plot(PDPout$sliceNo) + 
  labs(title = NULL, 
       y = "Probability of\nDisoccupation",
       x = "Years after Initial Occupation for each Occupation Span") +
  scale_x_continuous(breaks = seq(1,33,2),labels = seq(8, 264, 16))

B <- plot(PDPout$std_log_cellHouse)+ labs(title = NULL, y = "Probability of\nDisoccupation", 
                                          x = "Number of households\nin the cell")
C <- plot(PDPout$std_nCenter_18km)+ labs(title = NULL, y = NULL,
                                         x = "Number of centers\nwithin 18km")

png(filename = "figures/fig6.png", units = "in", width = 6, height = 4, res = 300)
ggarrange(ggarrange(A, ncol = 1, labels = c("A")),
          ggarrange(B,  C, ncol = 2,  labels = c("B","C")), 
          nrow = 2)
dev.off()

#random effects ----
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


random_output %>% write_csv(here::here(
  "data/data-derived/modelResults/randomEffects.csv"
))

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

pecos_wide <- 
  tibble::tibble(
    pecos = c( "BMIII", "PI", "PII",  "PIII"), #Bocinsky 2016 exploration/Exploitation ages
    StartDate = c(600, 700, 890,  1145),
    EndDate = c(700, 890,  1145,  1285)) %>% 
  dplyr::mutate(pecos = factor(pecos,
                               levels = unique(pecos),
                               ordered = TRUE),
                MidDate = (EndDate+StartDate) / 2)

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
  scale_color_manual("Being a Center Leads to:", 
                     values = c(  "#56B4E9", "black","#E69F00"))+
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
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.position = "top")

png(filename = "figures/fig7.png", units = "in", width = 6, height = 4, res = 300)
ranf_center_fig
dev.off()


# output random effects summary table
summary(sliced_lmer)$varcor %>% 
  as.data.frame() %>% 
  write_csv(here::here("data/data-derived/modelResults/summary_randomEffects.csv"))


# fixed effects table ----

# coefficients
coef_df <- as.data.frame(summary(sliced_lmer)$coefficients)
coef_df$variable <- rownames(coef_df)
names(coef_df)[1] <- "Estimate"

# Calculate confidence intervals manually
conf_level <- 0.95  # Adjust as needed
coef_df$SE <- coef_df[, "Std. Error"]
coef_df$CI_low <- coef_df$Estimate - qt((1 - conf_level) / 2, df = Inf) * coef_df$SE
coef_df$CI_high <- coef_df$Estimate + qt((1 - conf_level) / 2, df = Inf) * coef_df$SE

fixedEffectsPlot <- coef_df[34:38,] %>% 
  dplyr::mutate(variable = factor(variable,
                                  levels = rev(c("std_log_cellHouse", "isCenter", "std_nCenter_18km", 
                                                 "std_nType_18km",  
                                                 "std_localCatchment")),
                                  labels = rev(c( "Number of households in the home cell (logged then standardized)",
                                                  "Whether the home cell contains a community center",
                                                  "Number of community centers within 18km (standardized)",
                                                  "Number of institutional types within 18km (standardized)",
                                                  "Local catchment quality for growing maize (standardized)" )),
                                  ordered = TRUE)) %>% 
  ggplot(aes(y= variable, x = Estimate, xmin = CI_low, xmax = CI_high)) +
  geom_point() +
  geom_errorbar(width = 0.3) +
  scale_x_continuous(breaks = seq(-2,1,0.5), limits = c(-2,1))+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5)+
  labs(x = "Fixed Effects Drivers", y = "Estimate")+
  theme_bw()+
  theme(axis.text = element_text(size =8))

png(filename = "figures/fig5.png", units = "in", width = 6, height = 2, res = 300)
fixedEffectsPlot
dev.off()

coef_df %>% 
  dplyr::mutate(CI_low = round(CI_low,2),
                CI_high = round(CI_high,2))%>% 
  readr::write_csv(here::here("data/data-derived/modelResults/glmm_fixedEffects.csv"))

