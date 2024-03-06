library(tidyverse)
library(magrittr)
library(lme4)
library(mgcv)
library(broom)
library(Matrix)

#load data 
bout_slicemod_abs_20 <- readRDS(here::here("data/data-derived/prepAnalysis/bout_slicemod_abs_20.rds"))

# GAM w year ----


#run the GAM 
sliced_gam <- mgcv::gam(event ~ s(midSlice, bs = "cr") + 
                          s(log_cellHouse, bs = "cr") + 
                          localCatchment + 
                          s(nCenter_18km, bs = "cr") + 
                          s(nType_18km, bs = "cr") , 
                        data = bout_slicemod_abs_20, family = binomial)
sliced_gam %>% summary()

broom::tidy(sliced_gam) %>% write_csv(here::here("data/data-derived/modelResults/gam.csv"))
AIC(sliced_gam)
summary(sliced_gam)$r.sq

plot(ggeffects::ggpredict(sliced_gam), facets = TRUE)

png(filename = "figures/fig5.png", units = "in", width = 6, height = 5, res = 300)
plot(ggeffects::ggpredict(sliced_gam), facets = TRUE)
dev.off()


# lmer 20 year groups ----

bout_slicemod_abs_20 %>% dplyr::group_by( group20) %>% dplyr::select(group20) %>% 
  dplyr::mutate(n = dplyr::n()) %>% distinct() %>% arrange(-group20)



sliced_lmer <- glmer(event ~ 
                       isCenter +
                       std_log_cellHouse + std_localCatchment + 
                       std_nCenter_18km + std_nType_18km  + 
                       (1|group20) ,
                     data = bout_slicemod_abs_20,
                     family = binomial)

sliced_lmer %>% summary()
AIC(sliced_lmer)
sliced_lmer %>% MuMIn::r.squaredGLMM()


slc_confint <- data.frame(1:6)

slc_confint$var <- as.list(rownames(summary(sliced_lmer)$coefficients))
slc_confint$bottom_2_5 <- as.list( round(summary(sliced_lmer)$coefficients[,1] - 2*summary(sliced_lmer)$coefficients[,2], 3))
slc_confint$top_97_5 <- as.list( round(summary(sliced_lmer)$coefficients[,1] + 2*summary(sliced_lmer)$coefficients[,2], 3))

slc_confint <- slc_confint[,2:4] %>% tibble::as_tibble() %>% tidyr::unnest()

lmer_out <-
  summary(sliced_lmer)$coefficients %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "fixed") %>% 
  dplyr::select(fixed) %>% 
  bind_cols(., 
            bind_cols(summary(sliced_lmer)$coefficients, slc_confint[1:6,2:3])) %>% 
  dplyr::mutate(estimate = round(Estimate, 2),
                se = round(`Std. Error`,2),
                zVal = round(`z value`, 2),
                pVal = round(`Pr(>|z|)`, 2)) %>% 
  dplyr::select(fixed, estimate, se, zVal, pVal, bottom_2_5, top_97_5)

lmer_out %>% readr::write_csv(here::here("data/data-derived/modelResults/lmerResults_20years.csv"))

#
# 2o year binwidths ----
# function for generating multiples models, one for each temporal group ----
lms_by_group <- function(d, group){
  
  ls <- unique(d[[group]]) %>% sort()
  
  out <- purrr::map(.x = ls, function(n){
    thisLM <- glm(
      event~ isCenter +
        std_log_cellHouse +
        std_localCatchment +
        # std_nHouse_catchment +
        std_nCenter_18km +
        std_nType_18km
      ,
      family = binomial(link = "logit"),
      data = d[d[[group]] == n, ]
    )
    
    carvif <- car::vif(thisLM) %>%
      tibble::as_tibble() %>%
      dplyr::rename(vif = value) %>%
      rbind(0, .)
    
    results <- summary(thisLM)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column(var = "variable") %>%
      dplyr::rename(Pvalue = `Pr(>|z|)`) %>%
      dplyr::mutate(signif = case_when(Pvalue > 0.05 ~ "N.S.",
                                       # Pvalue > 0.05 & Pvalue <= 0.1 ~ ".",
                                       Pvalue > 0.01 & Pvalue <= 0.05 ~"*",
                                       Pvalue > 0.001 & Pvalue <= 0.01 ~"**",
                                       Pvalue >= 0 & Pvalue <= 0.001 ~"***"
      )) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Estimate = ifelse(Pvalue > 0.05, NA, Estimate)) %>%
      dplyr::mutate(group = n,
                    AIC = round(summary(thisLM)$aic, digits = 4),
                    est_p =
                      # stringr::str_c(
                      round(Estimate, digits = 2)
                    # , " ", signif)
      ) %>%
      dplyr::select(variable, group, AIC, est_p) %>%
      bind_cols(. , carvif)
    
    return(results)
  })%>% bind_rows()
  
  return(out)
}

#load
bout_slicemod_abs_20 <- readRDS(here::here("data/data-derived/prepAnalysis/bout_slicemod_abs_20.rds"))
group_20 <-readRDS(here::here("data/data-derived/prepAnalysis/20_year_groups.rds"))



#run
out_20 <- lms_by_group(d = bout_slicemod_abs_20, group = "group20") %>% 
  mutate(variable = ifelse(variable == "(Intercept)", "Intercept", variable))


out_20  %>%  arrange(-vif) #check multicollinearity
out_20 %>% arrange(est_p)


# make Fig 6 ----



fig6 <- out_20 %>% 
  dplyr::select(-vif) %>% 
  dplyr::left_join(sampleSizes, by = c("group" = "group"), relationship = "many-to-one") %>% 
  ggplot(aes(x = (start+10), y = AIC)) +
  geom_point(aes(size = n))+
  geom_line()+
  # scale_y_continuous(name = "Estimate (if plotted, p<0.05)", breaks = seq(-9,6,3))+
  theme_bw()+
  scale_x_continuous(name = "Year AD", breaks = seq(600,1300,100))+
  geom_vline(
    aes(xintercept = as.numeric(EndDate)),
    data = pecos_wide 
  ) +
  geom_text( aes(x = MidDate,
                 y = 2800,
                 label = pecos),
             vjust = -0.3,
             data = pecos_wide) +
  theme(legend.position = "top")+
  guides(size=guide_legend(title="Number of events")) #+
# theme(axis.title.x = element_blank(),
#       axis.text.x = element_blank())


png(filename = "figures/fig6.png", units = "in", width = 6, height = 2.5, res = 300)
fig6
dev.off()

#make fig 7 ----
group20_years <- group_20 %>% 
  mutate(group = as.integer(group20)) %>% 
  dplyr::select(group, start, end) %>% 
  dplyr::distinct()


pecos_wide <- 
  tibble::tibble(
    pecos = c( "BMIII", "PI", "PII",  "PIII"), #Bocinsky 2016 exploration/Exploitation ages
    StartDate = c(600, 700, 890,  1145),
    EndDate = c(700, 890,  1145,  1285)) %>% 
  dplyr::mutate(pecos = factor(pecos,
                               levels = unique(pecos),
                               ordered = TRUE),
                MidDate = (EndDate+StartDate) / 2)


fig_stacked <- out_20 %>% 
  dplyr::select(-vif) %>% 
  dplyr::left_join(group20_years, by = c("group" = "group")) %>% 
  dplyr::filter(variable != "Intercept") %>% 
  dplyr::mutate(variable = factor(variable,
                                  levels = c("std_log_cellHouse", "isCenter", "std_nCenter_18km", 
                                             "std_nType_18km",  
                                             "std_localCatchment"),
                                  labels = c( "A: Number of households in the home cell (logged then standardized)",
                                              "B: Whether the home cell contains a community center",
                                              "C: Number of community centers within 18km (standardized)",
                                              "D: Number of institutional types within 18km (standardized)",
                                              "E:Local catchment quality for growing maize (standardized)"
                                             
                                             ))) %>% 
  ggplot(aes(x = (start+10), y = est_p)) +
  facet_wrap(dplyr::vars(variable), 
             ncol = 1) +
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_y_continuous(name = "Estimate (if plotted, p<0.05)", 
                     limits = c(-5, 5),
                     breaks = seq(-9,6,3))+
  scale_x_continuous(name = "Year AD", breaks = seq(600,1280, 100))+
  geom_vline(
    aes(xintercept = as.numeric(EndDate)),
    data = pecos_wide 
  ) +
  geom_text( aes(x = MidDate,
                 y = 2.75,
                 label = pecos),
             vjust = -0.3,
             data = pecos_wide %>% 
               dplyr::mutate(variable = factor("isCenter", label =  "A: Number of households in the home cell (logged then standardized)")) )+
  theme_bw()

fig_stacked

png(filename = "figures/fig7.png", units = "in", width = 6, height = 7.5, res = 300)
fig_stacked 
dev.off()

